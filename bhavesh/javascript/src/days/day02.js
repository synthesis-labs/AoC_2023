"use strict";

import { getQuestionData } from "../common.js";

async function solve() {
  const input = await getQuestionData(2022, 2);

  return {
    part1: input.length.toString(),
    part2: "Goodbye part 2",
  };
}

export default {
  solve,
};
