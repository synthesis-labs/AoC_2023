"use strict";

import { getQuestionData } from "../common.js";

async function solve() {
  const input = await getQuestionData(2022, 1);

  return {
    part1: input.length.toString(),
    part2: "Hello part 2",
  };
}

export default {
  solve,
};
