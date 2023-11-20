"use strict";

import axios from "axios";
import { config } from "dotenv";

config();
let cookie = process.env.COOKIE;

export async function getQuestionData(year, questionNumber) {
  let url = `https://adventofcode.com/${year}/day/${questionNumber}/input`;
  let headers = {
    Accept: "application/json",
    Cookie: cookie,
  };
  try {
    const response = await axios.get(url, { headers });
    return response.data;
  } catch (error) {
    console.log(error);
    throw error;
  }
}

export default {
  getQuestionData,
};
