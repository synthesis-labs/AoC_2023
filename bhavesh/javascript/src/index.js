"use strict";

async function main() {
  const N = 25;

  for (let i = 1; i <= N; i++) {
    let answer = { part1: "", part2: "" };
    let question = i.toString().padStart(2, "0");
    try {
      const dayModule = await import(`./days/day${question}.js`);
      answer = await dayModule.default.solve();
    } catch (error) {
      // console.log(error);
      answer.part1 = "No impl found";
      answer.part2 = "No impl found";
    }

    console.log(`Day ${question} - Part 1: ${answer.part1}`);
    console.log(`Day ${question} - Part 2: ${answer.part2}`);
    console.log();
  }
}

main();
