package io.nanovc.aoc2023.day08q1;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Node;

import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static io.nanovc.aoc2023.day07q1.Day07Q1Tests.Solution1Tests.Card.*;
import static io.nanovc.aoc2023.day07q1.Day07Q1Tests.Solution1Tests.HandType.*;
import static io.nanovc.aoc2023.day08q1.Day08Q1Tests.Solution1Tests.Direction.Left;
import static io.nanovc.aoc2023.day08q1.Day08Q1Tests.Solution1Tests.Direction.Right;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * --- Day 8: Haunted Wasteland ---
 * You're still riding a camel across Desert Island when you spot a sandstorm quickly approaching. When you turn to warn the Elf, she disappears before your eyes! To be fair, she had just finished warning you about ghosts a few minutes ago.
 *
 * One of the camel's pouches is labeled "maps" - sure enough, it's full of documents (your puzzle input) about how to navigate the desert. At least, you're pretty sure that's what they are; one of the documents contains a list of left/right instructions, and the rest of the documents seem to describe some kind of network of labeled nodes.
 *
 * It seems like you're meant to use the left/right instructions to navigate the network. Perhaps if you have the camel follow the same instructions, you can escape the haunted wasteland!
 *
 * After examining the maps for a bit, two nodes stick out: AAA and ZZZ. You feel like AAA is where you are now, and you have to follow the left/right instructions until you reach ZZZ.
 *
 * This format defines each node of the network individually. For example:
 *
 * RL
 *
 * AAA = (BBB, CCC)
 * BBB = (DDD, EEE)
 * CCC = (ZZZ, GGG)
 * DDD = (DDD, DDD)
 * EEE = (EEE, EEE)
 * GGG = (GGG, GGG)
 * ZZZ = (ZZZ, ZZZ)
 * Starting with AAA, you need to look up the next element based on the next left/right instruction in your input. In this example, start with AAA and go right (R) by choosing the right element of AAA, CCC. Then, L means to choose the left element of CCC, ZZZ. By following the left/right instructions, you reach ZZZ in 2 steps.
 *
 * Of course, you might not find ZZZ right away. If you run out of left/right instructions, repeat the whole sequence of instructions as necessary: RL really means RLRLRLRLRLRLRLRL... and so on. For example, here is a situation that takes 6 steps to reach ZZZ:
 *
 * LLR
 *
 * AAA = (BBB, BBB)
 * BBB = (AAA, ZZZ)
 * ZZZ = (ZZZ, ZZZ)
 * Starting at AAA, follow the left/right instructions. How many steps are required to reach ZZZ?
 * <p>
 * Website:
 * <a href="https://adventofcode.com/2023/day/8">Challenge</a>
 */
public abstract class Day08Q1Tests extends TestBase
{

    /**
     * The day that we are solving the puzzle for.
     * eg: "Day 01"
     *
     * @return The day that we are solving the puzzle for.
     */
    @Override
    protected String getDayLabel()
    {
        return "Day 08 Q1";
    }

    /**
     * Gets the sample input that was provided by the puzzle question.
     *
     * @return The sample input that was provided by the puzzle question.
     */
    @Override
    protected String getSampleInput()
    {
        return """
                RL
                         
                AAA = (BBB, CCC)
                BBB = (DDD, EEE)
                CCC = (ZZZ, GGG)
                DDD = (DDD, DDD)
                EEE = (EEE, EEE)
                GGG = (GGG, GGG)
                ZZZ = (ZZZ, ZZZ)
                """;
    }

    /**
     * Gets the sample answer that was provided by the puzzle question.
     *
     * @return The sample answer that was provided by the puzzle question.
     */
    @Override
    protected String getSampleAnswer()
    {
        return "2";
    }

    /**
     * This tests that the {@link #solve(String) solution}
     * gets the {@link #getSampleAnswer() sample answer}
     * by using the {@link #getSampleInput() sample input}.
     */
    @Test
    @Override
    public void testSample()
    {
        super.testSample();
    }

    /**
     * This tests that the {@link #solve(String) solution}
     * gets the {@link #getActualAnswer() actual answer}
     * by using the {@link #getActualInput() actual input}.
     */
    @Test
    @Override
    public void testSolution() throws IOException
    {
        super.testSolution();
    }

    /**
     * Gets the actual answer that we compute as the solution.
     * Usually you leave this blank initially, let the test fail and then update it to what the solution produces from {@link #solve(String)}.
     *
     * @return The actual answer that we compute as the solution.
     */
    @Override
    protected String getActualAnswer()
    {
        return "17263";
    }

    public static class Solution1Tests extends Day08Q1Tests
    {

        /**
         * This tests that the {@link #solve(String) solution}
         * gets the {@link #getSampleAnswer() sample answer}
         * by using the {@link #getSampleInput() sample input}.
         */
        @Test
        @Override
        public void testSample()
        {
            super.testSample();
        }

        @Override
        public String solve(String input)
        {
            // Parse the lines:
            var lines = input.split("\\n");

            // Get the instructions:
            var instructionText = lines[0];
            var instructions = parseInstructions(instructionText);

            // Parse the network:
            var network = parseNetwork(lines);

            // Walk the network:
            int stepCount = 0;
            Node currentNode = network.startingNode;
            Instruction currentInstruction = instructions;
            while (currentNode != network.endingNode)
            {
                // We are not at the end of the network yet.

                // Follow the next instruction:
                var nextNode = currentInstruction.direction.equals(Left) ? currentNode.left : currentNode.right;

                // Detect loops:
                if (currentNode == nextNode) throw new RuntimeException("Loop on node " + currentNode + " by following instruction " + currentInstruction.index + " " + currentInstruction.direction);
                if (stepCount > 1_000_000) throw new RuntimeException("Couldn't get to the end in a million steps");

                // Move to the next step:
                currentNode = nextNode;
                currentInstruction = currentInstruction.next;
                stepCount++;
            }

            return Integer.toString(stepCount);
        }

        /**
         * Parses the network that is described.
         * @param lines The lines that describe the network. The first line contains the instructions. Ignore that. The third line is where it really starts.
         * @return The network that is described.
         */
        public Network parseNetwork(String[] lines)
        {
            // Create the network:
            var network = new Network();

            // Create pattern we need for parsing:
            var pattern = Pattern.compile("(?<Name>\\w+)\\s+=\\s+\\((?<Left>\\w+),\\s+(?<Right>\\w+)\\)");

            // First create the nodes in the network:
            // Start at line 3:
            for (int i = 2; i < lines.length; i++)
            {
                // Get line:
                var line = lines[i];

                // Parse the name of the node:
                Matcher matcher = pattern.matcher(line);
                if (matcher.find())
                {
                    // We found a match.

                    // Get the details:
                    var name = matcher.group("Name");

                    // Create the node:
                    Node node = new Node();
                    node.name = name;

                    // Check if this is the starting node:
                    if (name.equals("AAA")) network.startingNode = node;

                    // Check if this is the terminal node:
                    if (name.equals("ZZZ")) network.endingNode = node;

                    // Index the node:
                    network.nodesByName.put(name, node);
                }
            }

            // Now link up the nodes in the network:
            // Start at line 3:
            for (int i = 2; i < lines.length; i++)
            {
                // Get line:
                var line = lines[i];

                // Parse the name of the node:
                Matcher matcher = pattern.matcher(line);
                if (matcher.find())
                {
                    // We found a match.

                    // Get the details:
                    var name = matcher.group("Name");
                    var left = matcher.group("Left");
                    var right = matcher.group("Right");

                    // Find the nodes:
                    Node node = network.nodesByName.get(name);
                    Node leftNode = network.nodesByName.get(left);
                    Node rightNode = network.nodesByName.get(right);

                    // Link up the nodes:
                    node.left = leftNode;
                    node.right = rightNode;
                }
            }

            return network;
        }

        /**
         * Parses the given instruction text so that the instruction chain is looped around to the first instruction.
         * @param instructionText The instruction text to parse. LRLRLRLRLLRLLLRL
         * @return The instruction chain with the last instruction looped around to the first one.
         */
        public Instruction parseInstructions(String instructionText)
        {
            Instruction firstInstruction = null;
            Instruction currentInstruction = null;
            for (int i = 0; i < instructionText.length(); i++)
            {
                // Get the character for this instruction:
                char c = instructionText.charAt(i);

                // Parse the direction:
                Direction direction = switch (c) { case 'L' -> Left; case 'R' -> Right; default -> Left; };

                // Create the next instruction:
                var nextInstruction = new Instruction();
                nextInstruction.index = i;
                nextInstruction.direction = direction;

                // Link up the previous instruction or start a new chain:
                if (currentInstruction == null)
                {
                    // This is the first instruction.
                    // Remember it so that we can link up to it at the end:
                    firstInstruction = nextInstruction;
                    currentInstruction = nextInstruction;
                }
                else
                {
                    // We have an instruction already.

                    // Link up to it:
                    currentInstruction.next = nextInstruction;

                    // Move to the next instruction:
                    currentInstruction = nextInstruction;
                }
            }
            // Now we have parsed the instruction text.

            // Link up the last instruction to the first one to make a loop:
            if (currentInstruction != null) currentInstruction.next = firstInstruction;

            return firstInstruction;
        }


        /**
         * The direction to travel.
         */
        public enum Direction { Left, Right }


        /**
         * This encodes on instruction to follow.
         * The last instruction points back at the first instruction to make loop.
         */
        public static class Instruction
        {
            /**
             * The index of the instruction in the original sequence. Source code line number.
             */
            public int index;
            /**
             * The direction to take for this instruction.
             */
            public Direction direction;

            /**
             * A pointer to the next instruction. The last instruction points back at the first instruction to make a loop.
             */
            public Instruction next;

            @Override
            public String toString()
            {
                return index + ": " + direction;
            }
        }


        public static class Network
        {
            /**
             * The terminal node where we start.
             * AAA
             */
            public Node startingNode;

            /**
             * The terminal node where we end.
             * ZZZ
             */
            public Node endingNode;

            /**
             * The nodes of the network, indexed by name.
             */
            public Map<String, Node> nodesByName = new HashMap<>();
        }

        /**
         * A node in the network.
         */
        public static class Node
        {
            /**
             * The name of the node.
             */
            public String name;

            /**
             * The link to the left node.
             */
            public Node left;

            /**
             * The link to the right node.
             */
            public Node right;

            @Override
            public String toString()
            {
                return this.name;
            }
        }
    }

}
