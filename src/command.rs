use std::{
    collections::HashMap,
    fmt::{Display, self},
};

use tracing::error;

use futures::{
    pin_mut,
    future::Either,
    prelude::*,
};

use tokio::{
    sync::{mpsc, RwLock},
    net::TcpStream,
};

use tokio_util::codec::{Decoder, LengthDelimitedCodec};
use tokio_serde::{
    Framed,
    formats,
};

use serde::{Serialize, Deserialize};
use serenity::{
    prelude::*,
    model::{
        channel::{Message, ReactionType},
        id::{MessageId,EmojiId},
    }
};

use lazy_static::lazy_static;
use regex::{Regex, Captures};

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "snake_case")] 
pub enum Command {
    Query(String),
    Extend(String),
    EnumerateMore,
    ReadResponse(String),
    Halt,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "snake_case")] 
pub enum Effect {
    Ok,
    Answer(String),
    More(String),
    Halted,
    Error(String),
    Write(String),
    Read,
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum ErrorKind {
    FromDiscord(serenity::Error),
    FromPrologServer(std::io::Error),
}

impl From<std::io::Error> for ErrorKind {
    fn from(error: std::io::Error) -> Self {
        ErrorKind::FromPrologServer(error)
    }
}

impl From<serenity::Error> for ErrorKind {
    fn from(error: serenity::Error) -> Self {
        ErrorKind::FromDiscord(error)
    }
}

#[derive(Debug)]
pub struct Error {
    pub error: ErrorKind,
    pub when: String,
}

impl<T> From<Error> for Result<T> {
    fn from(error: Error) -> Self {
        Err(error)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error when {}: {:?}", self.when, self.error)
    }
}

impl Error {
    pub fn errored_when(error: impl Into<ErrorKind>, when: impl Into<String>) -> Self {
        Self {
            error: error.into(),
            when: when.into(),
        }
    }

    pub async fn relate(&self, ctx: &Context, msg: &Message) {
        error!("{}", self);
        if let Err(why) = msg.channel_id.say(ctx.http.clone(), self).await { //fixme: do not relate discord errors
            error!("Error when trying to relate error: {}", why);
        }
    }
}

pub struct Query {
    commands_tx: mpsc::Sender<Command>,
    effects: mpsc::Receiver<Result<Effect>>,
    task: tokio::task::JoinHandle<Result<()>>,
}

impl Query {
    pub async fn send_command(&self, command: Command) {
        self.commands_tx.send(command).await.unwrap()
    }

    pub fn get_command_sender(&self) -> mpsc::Sender<Command> {
        return self.commands_tx.clone()
    }
    
    pub async fn next_effect(&mut self) -> Option<Result<Effect>> {
        let effects = self.effects.recv();
        let task = &mut self.task;

        pin_mut!(effects);
        pin_mut!(task);

        match future::select(effects, task).await {
            Either::Left((e, _)) => e,
            Either::Right(_) => None,
        }
    }

    pub fn open(code: String) -> Self {
        let (effect_tx, effects) = mpsc::channel(100);
        let (commands_tx, mut commands) = mpsc::channel(100);

        let task = tokio::spawn(async move {

            let socket = match TcpStream::connect("127.0.0.1:3966").await {
                Ok(s) => s,
                Err(err) => return Error::errored_when(err, "trying to connect to pl server").into(),
            };
            
            let codec = LengthDelimitedCodec::new().framed(socket);

            let mut serialized = Framed::new(codec, formats::Json::default());

            let query = Command::Query(code);

            if let Err(err) = serialized.send(query).await {
                return Error::errored_when(err, "trying to send query to pl server").into();
            }

            loop {

                let next_effect = serialized.next();
                let next_command = commands.recv();

                pin_mut!(next_effect);
                pin_mut!(next_command);

                match future::select(next_effect, next_command).await {
                    Either::Left((Some(frame), _)) => match frame {
                        Ok(effect) => effect_tx.send( Ok(effect) ).await.unwrap(),
                        Err(err) => return Error::errored_when(err, "").into(),
                    },

                    Either::Right((Some(command), _)) => {
                        if let Err(err) = serialized.send(command).await {
                            return Error::errored_when(err, "trying to send command to pl server").into();
                        }
                    },
                    
                    Either::Left((None, _)) | Either::Right((None, _)) => return Ok(()), 
                }
            }
        });

        Self {
            commands_tx,
            effects,
            task,
        }
    }
}

#[derive(Default)]
pub struct OpenQueries {
    pub map: RwLock<HashMap<MessageId, QueryInfo>>,
}

impl TypeMapKey for OpenQueries {
    type Value = Self;
}


pub struct QueryInfo {
    pub command_sink: mpsc::Sender<Command>,
    pub current_answer: Option<Message>,
}

impl QueryInfo {
    pub fn new(tx: mpsc::Sender<Command>) -> Self {
        QueryInfo {
            command_sink: tx,
            current_answer: None,
        }
    }
}

// of the form ``` ... ``` or ` ... `
struct MarkdownCode {
    pub language: Option<String>,
    pub code: String,
}

struct ParseCodeBlockError;

impl std::str::FromStr for MarkdownCode {
    type Err = ParseCodeBlockError;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        if s.starts_with("```") && s.ends_with("```") {
            let s = &s[3 .. (s.len() - 3)];

            lazy_static! {
                static ref CODEBLOCK_LANGUAGE_REGEXP: Regex = Regex::new(r"^([\w\-+]+)\s*\n").unwrap();
            }

            let (language, code_start_offset) = match CODEBLOCK_LANGUAGE_REGEXP.captures(s) {
                None => (None, 0),
                Some(caps) => {
                    let c = caps.get(1).unwrap();
                    ( Some(c.as_str().into()), c.end() )
                },
            };

            let code = s[code_start_offset..].into();

            return Ok( Self {
                language,
                code,
            })

        } else if s.starts_with("`") && s.ends_with("`") {
            return Ok( Self {
                language: None,
                code: s[1 .. (s.len() - 1)].into(),
            });
        } else {
            return Err(ParseCodeBlockError);
        }
    }
}

impl Command {
    fn get_prolog_code_str(msg_str: &str) -> String {
        match msg_str.parse::<MarkdownCode>() {
            Ok(block) => block.code,
            Err(_) => Self::atomify_emoji(msg_str),
        }
    } 

    fn atomify_emoji(code: &str) -> String {
        lazy_static! { 
            static ref ATOMIFY_EMOJI_REGEXP: Regex = 
                // this is very cursed.
                Regex::new(r#"(<a?:[[:alpha:]]+:\d+>)|([\p{emoji}\p{emod}\p{ecomp}&&[^\d\*\#]]+)"#).unwrap(); 
        }

        ATOMIFY_EMOJI_REGEXP.replace_all(code, |captures: &Captures| {
            format!("emoji(\"{}\")", captures.get(0).unwrap().as_str()) // FIXME: unwrap
        }).into()
    }

    pub fn parse(msg: &Message) -> Option<Self> {
        let msg_str = &msg.content.as_str();
            
        if msg_str.starts_with("?-") {
            let msg_str = &msg_str[2..]
                .trim_start_matches(|c: char| c.is_whitespace() || c == '\n' );

            let code = Self::get_prolog_code_str(msg_str);

            return Some(Command::Query(code));
        }
        if msg_str.starts_with(":-") {
            let msg_str = &msg_str[2..]
                .trim_start_matches(|c: char| c.is_whitespace() || c == '\n' );

            let code = Self::get_prolog_code_str(msg_str);

            return Some(Command::Extend(code));
        }
        None
    }
}

impl Effect {
    pub async fn handle(&self, ctx: &Context, msg: &Message) -> Result<()> {
        async fn reply_to(ctx: &Context, msg: &Message, with: impl std::fmt::Display) -> Result<Message> {
            match msg.reply(ctx.http.clone(), with).await {
                Ok(message) => Ok(message),
                Err(err) => Error::errored_when(err, "replying to user").into(),
            }
        }

        match self {
            Effect::Ok => {
                let ok_emoji = ReactionType::Custom{
                    animated: false,
                    id: EmojiId(822782191225798737),
                    name: Some("swipl".into()),
                };

                if let Err(why) = msg.react(ctx, ok_emoji).await {
                    return Error::errored_when(why, "reacting to message with confirmation emoji").into();
                }
            },
            Effect::Answer(answer) => {
                let data = ctx.data.read().await;
                let open_queries = data.get::<OpenQueries>().unwrap();
                let mut open_queries_map = open_queries.map.write().await;
                let query = open_queries_map.get_mut(&msg.id).unwrap(); 

                match query.current_answer {
                        None => {  
                            reply_to(ctx, msg, answer).await?;
                        },
                        Some(ref mut prev_answer) => {
                            let prev = prev_answer.content.clone();
                            let edit_result = prev_answer.edit(ctx, |m| {
                                m.content(format!("{};\n{}", prev, answer))
                            }).await;
    
                            if let Err(why) = edit_result {
                                return Error::errored_when(why, "editing message to enumerate last answer").into();
                            }

                        } 
                    } 
                },
            Effect::More(more) => {
                let data = ctx.data.read().await;
                let open_queries = data.get::<OpenQueries>().unwrap();
                let mut open_queries_map = open_queries.map.write().await;
                let query = open_queries_map.get_mut(&msg.id).unwrap();

                match query.current_answer {
                    None => {
                        let enumerate_more_emoji = ReactionType::Unicode("➕".into());
                        let answer = reply_to(ctx, msg, more).await?;

                        if let Err(why) = msg.react(ctx, enumerate_more_emoji).await {
                            return Error::errored_when(why, "reacting to message with enumerate more emoji").into();
                        }
                        
                        query.current_answer = Some(answer);
                    },
                    Some(ref mut answer) => {
                        let prev = answer.content.clone();
                        let edit_result = answer.edit(ctx, |m| {
                            m.content(format!("{};\n{}", prev, more))
                        }).await;

                        if let Err(why) = edit_result {
                            return Error::errored_when(why, "editing message to enumerate more answers").into();
                        }
                    }
                }

            },
            Effect::Halted => unimplemented!(),
            Effect::Error(pl_err) => {
                let error_emoji = ReactionType::Unicode("❌".into());

                if let Err(why) = msg.react(ctx.http.clone(), error_emoji).await {
                    return Error::errored_when(why, "reacting to message with error emoji").into();
                }

                reply_to(ctx, msg, pl_err).await?;
            },
            Effect::Write(write) => { reply_to(ctx, msg, write).await?; },
            Effect::Read => unimplemented!(),
        };

        Ok(())
    }
}