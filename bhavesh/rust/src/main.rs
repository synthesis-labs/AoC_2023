use dotenv::dotenv;

mod days;
mod models;
pub mod utils;

#[tokio::main]
async fn main() {
    dotenv().ok();

    println!("Advent of Code 2023");
    println!("{}", days::day01::solve().await);
    println!("{}", days::day02::solve().await);
}
