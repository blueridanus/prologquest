use serenity::{
    async_trait,
    model::{channel::Message, gateway::Ready},
    prelude::*,
};

use tokio::{
    net::{TcpStream},
    io::{AsyncWriteExt, AsyncReadExt},
    
};

use regex::{Regex, Captures};
use lazy_static::lazy_static;

struct Handler;

#[async_trait]
impl EventHandler for Handler {
    // Set a handler for the `message` event - so that whenever a new message
    // is received - the closure (or function) passed will be called.
    //
    // Event handlers are dispatched through a threadpool, and so multiple
    // events can be dispatched simultaneously.
    async fn message(&self, ctx: Context, msg: Message) {
        lazy_static! {
            static ref CMD_PREFIX_REGEX: Regex = Regex::new(r"^\?\-[ \n]*(?:```)?(?:prolog|pl)?[ \n]*").unwrap();
            static ref BERATE_PREFIX_REGEX: Regex = Regex::new(r"^\-\?").unwrap();
            static ref BERATE_PERIOD_REGEX: Regex = Regex::new(r"^\?\-[^\.]+$").unwrap();
        }
        dbg!("Msg handle: {}", msg.content.as_str());

        if let Some(mtch) = CMD_PREFIX_REGEX.find(msg.content.as_str()){
            dbg!("Regex match");
            let mut stream = match TcpStream::connect("127.0.0.1:3966").await {
                Ok(s) => s,
                Err(serv_err) => { 
                    if let Err(msg_err) = msg.channel_id.say(
                        &ctx.http, 
                        format!("Fucky wucky connecting to prolog server: {}", serv_err)
                    ).await {
                        println!("Double fucky wucky when sending fucky wucky message (after trying to connect to pl server)");
                        println!("Server error: {}", serv_err);
                        println!("Discord error: {}", msg_err);
                    } 
                    return;
                },
            };

            lazy_static! { 
                static ref DEMOJIFY_REGEX: Regex = Regex::new(r#"(<a?:[[:alpha:]]+:\d+>)|([\p{emoji}\p{emod}\p{ecomp}&&[^\d]]+)"#).unwrap(); // this is very cursed
                static ref EMOJIFY_REGEX: Regex = Regex::new(r#"emoji\(([^\)]+)\)"#).unwrap(); 
            }

            let mut query_str = &msg.content[mtch.end()..];
            if(query_str.ends_with("```")){ //fixme this whole hack lol
                query_str = &query_str[..query_str.len()-3];
            }

            let d_str = String::from(query_str);
            println!("Checking for demojify: {}", DEMOJIFY_REGEX.is_match(&d_str));
            let d_str = DEMOJIFY_REGEX.replace_all(d_str.as_str(), |captures: &Captures| {
                println!("demojifying!");
                format!("emoji(\"{}\")", captures.get(0).unwrap().as_str()) // FIXME: unwrap
            });
            println!("{}", d_str);

            if let Err(serv_err) = stream.write_all(d_str.as_bytes()).await {
                if let Err(msg_err) = msg.channel_id.say(
                    &ctx.http, 
                    format!("Fucky wucky sending query to prolog server: {}", serv_err)
                ).await {
                    println!("Double fucky wucky when sending fucky wucky message (after trying to send query to pl server)");
                    println!("Server error: {}", serv_err);
                    println!("Discord error: {}", msg_err);
                } 
                return;
            }
            
            
            if let Err(serv_err) = stream.shutdown().await {
                if let Err(msg_err) = msg.channel_id.say(
                    &ctx.http, 
                    format!("Fucky wucky finishing write stream to prolog server: {}", serv_err)
                ).await {
                    println!("Double fucky wucky when sending fucky wucky message (after trying to finish write stream to pl server)");
                    println!("Server error: {}", serv_err);
                    println!("Discord error: {}", msg_err);
                } 
                return;
            };

            let mut response = String::new();

            if let Err(serv_err) = stream.read_to_string(&mut response).await {
                
                if let Err(msg_err) = msg.channel_id.say(
                    &ctx.http,
                    format!("Fucky wucky getting response from prolog server: {}", serv_err)
                ).await {
                    println!("Double fucky wucky when sending fucky wucky message (after trying to receive response from pl server)");
                    println!("Server error: {}", serv_err);
                    println!("Discord error: {}", msg_err);
                }
            }

            let response = EMOJIFY_REGEX.replace_all(response.as_str(), |captures: &Captures| { match captures.get(1) {
                Some(m) => String::from(m.as_str()),
                None => String::from(""), // thonk, maybe unreachable!()?
            }});

            if let Err(msg_err) = msg.channel_id.say(&ctx.http, response).await {
                println!("Received query response sucesfully, but fucky wucky sending the message to Discord: {}", msg_err)
            };

        }

        if BERATE_PREFIX_REGEX.is_match(msg.content.as_str()) {
            msg.channel_id.say(&ctx.http, "HOLY SHIT ITS ?- NOT -? COME ON").await; // ignoring Result lol
        }

        if BERATE_PERIOD_REGEX.is_match(msg.content.as_str()) {
            msg.channel_id.say(&ctx.http, "HOLY SHIT STOP FORGETTING THE PERIOD").await; // ignoring Result lol
        }

    }

    // Set a handler to be called on the `ready` event. This is called when a
    // shard is booted, and a READY payload is sent by Discord. This payload
    // contains data like the current user's guild Ids, current user data,
    // private channels, and more.
    //
    // In this case, just print what the current user's username is.
    async fn ready(&self, _: Context, ready: Ready) {
        println!("{} is connected!", ready.user.name);
    }
}

#[tokio::main]
async fn main() {
    // Configure the client with your Discord bot token in the environment.
    //let token = String::from(std::env::var("BOT_API_TOKEN").unwrap());
    let token = "ODE1MTcyMDA5MjEwMTUwOTcy.YDoiGA.nWDaEqN8r9MrFZGF6SX_S_PjUw4";

    // Create a new instance of the Client, logging in as a bot. This will
    // automatically prepend your bot token with "Bot ", which is a requirement
    // by Discord for bot users.
    let mut client = Client::builder(&token)
        .event_handler(Handler)
        .await
        .expect("Err creating client");

    // Finally, start a single shard, and start listening to events.
    //
    // Shards will automatically attempt to reconnect, and will perform
    // exponential backoff until it reconnects.
    if let Err(why) = client.start().await {
        println!("Client error: {:?}", why);
    }
}