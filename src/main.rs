use serenity::{
    async_trait,
    model::{
        channel::{Message, Reaction, ReactionType},
        gateway::Ready,
        id::UserId,
    },
    prelude::*,
};

use tracing::{info, error, Level};
use tracing_subscriber::FmtSubscriber;

mod command;

use command::{Command, Query, QueryInfo, OpenQueries};

struct Handler;

impl Handler {
    async fn handle_query(ctx: Context, msg: Message, code: String) {
        let mut query = Query::open(code);
        {
            let data = ctx.data.read().await;
            let queries = data.get::<OpenQueries>().unwrap();
            queries.map
                .write()
                .await
                .insert(msg.id, QueryInfo::new(query.get_command_sender()));
        }

        loop {
            info!("Iteration.");
            let response = query.next_effect().await;

            match response {
                Some(maybe_effect) => match maybe_effect {
                    Ok(effect) => if let Err(why) = effect.handle(&ctx, &msg).await {
                        return why.relate(&ctx, &msg).await;
                    },
                    Err(why) => return why.relate(&ctx, &msg).await,
                }
                None => return,
            }
        }

    }

    async fn handle_extend(_ctx: Context, _msg: Message, _code: String) {
        unimplemented!();
    }
}

struct BotId;

impl TypeMapKey for BotId {
    type Value = UserId;
}

#[async_trait]
impl EventHandler for Handler {
    async fn ready(&self, ctx: Context, ready: Ready) {
        let mut data = ctx.data.write().await;

        data.insert::<OpenQueries>(OpenQueries::default());
        data.insert::<BotId>(ready.user.id);
        info!("{} is connected!", ready.user.name);
    }

    async fn message(&self, ctx: Context, msg: Message) {
        info!("Got message: {}", msg.content);
        if let Some(command) = Command::parse(&msg) {
            info!("Command parsed: {:?}", command);
            use Command::*;
            match command {
                Query(code) => Self::handle_query(ctx, msg, code).await,
                Extend(code) => Self::handle_extend(ctx, msg, code).await,
                _ => unreachable!(),
            }
        } 
    }

    async fn reaction_add(&self, ctx: Context, reaction: Reaction) {
        info!("Reaction: {:?}", reaction);
        let data = ctx.data.read().await;
        let data_queries = data.get::<OpenQueries>().unwrap();
        let bot_id = data.get::<BotId>().unwrap();
        let queries = data_queries.map.read().await;

        let enumerate_more_emoji = ReactionType::Unicode("➕".into());
        if reaction.emoji == enumerate_more_emoji && &reaction.user_id.unwrap() != bot_id {
            info!("Emoji matches + emoji");
            if let Some(query) = queries.get(&reaction.message_id) {
                query.command_sink.send(Command::EnumerateMore).await.unwrap();
                info!("Asked for more");
            }
        }
        
    }
}

#[tokio::main]
async fn main() {
    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::INFO)
        .finish();

    tracing::subscriber::set_global_default(subscriber)
        .expect("Failed to start tracing");

    let token = match std::env::var("BOT_API_TOKEN") {
        Ok(v) => String::from(v),
        Err(why) => return error!("Could not read BOT_API_TOKEN: {}", why),
    };

    info!("Token is {}", token);

    let client = Client::builder(&token)
        .event_handler(Handler)
        .await;
    
    let mut client = match client {
        Ok(c) => c,
        Err(why) => return error!("Failed to create discord client: {}", why),
    };

    if let Err(why) = client.start().await {
        error!("Failed to start discord client: {}", why);
    }
}