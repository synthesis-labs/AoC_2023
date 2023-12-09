import os
import requests
from dotenv import load_dotenv

load_dotenv()

cookie = os.getenv("COOKIE")


def get_question_data(year, question_number):
  url = f"https://adventofcode.com/{year}/day/{question_number}/input"
  headers = {
    "Accept": "application/json",
    "Cookie": cookie,
  }

  try:
    response = requests.get(url, headers=headers)
    response.raise_for_status()
    return response.text
  except requests.exceptions.HTTPError as http_err:
    print(f"HTTP error occurred: {http_err}")
    raise http_err
  except Exception as err:
    print(f"An error occurred: {err}")
    raise err
