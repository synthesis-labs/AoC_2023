use reqwest::header::COOKIE;

pub async fn get_question_data(year: i32, day: i32) -> String {
    let url = format!("https://adventofcode.com/{}/day/{}/input", year, day);
    let cookie = std::env::var("COOKIE").expect("COOKIE must be set.");

    let client = reqwest::Client::new();

    let response: String = client
        .get(url)
        .header(COOKIE, cookie)
        .send()
        .await
        .expect("Could not get response")
        .text()
        .await
        .expect("Could not get text");

    response
}