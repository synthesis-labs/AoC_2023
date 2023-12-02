use std::error::Error;

use reqwest::{header::COOKIE, Client};

pub async fn get_question_data(year: i32, day: i32) -> Result<String, Box<dyn Error>> {
    let url: String = format!("https://adventofcode.com/{year}/day/{day}/input");
    let cookie: String = std::env::var("COOKIE").expect("COOKIE must be set.");

    let client: Client = reqwest::Client::new();

    let response: String = client
        .get(url)
        .header(COOKIE, cookie)
        .send()
        .await?
        .text()
        .await?;

    Ok(response)
}
