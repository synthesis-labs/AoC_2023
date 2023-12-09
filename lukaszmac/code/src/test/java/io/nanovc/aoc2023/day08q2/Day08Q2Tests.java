package io.nanovc.aoc2023.day08q2;

import io.nanovc.aoc2023.TestBase;
import io.nanovc.aoc2023.day08q1.Day08Q1Tests;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static io.nanovc.aoc2023.day08q2.Day08Q2Tests.Solution1Tests.Direction.Left;
import static io.nanovc.aoc2023.day08q2.Day08Q2Tests.Solution1Tests.Direction.Right;


/**
 * --- Part Two ---
 * The sandstorm is upon you and you aren't any closer to escaping the wasteland. You had the camel follow the instructions, but you've barely left your starting position. It's going to take significantly more steps to escape!
 *
 * What if the map isn't for people - what if the map is for ghosts? Are ghosts even bound by the laws of spacetime? Only one way to find out.
 *
 * After examining the maps a bit longer, your attention is drawn to a curious fact: the number of nodes with names ending in A is equal to the number ending in Z! If you were a ghost, you'd probably just start at every node that ends with A and follow all of the paths at the same time until they all simultaneously end up at nodes that end with Z.
 *
 * For example:
 *
 * LR
 *
 * 11A = (11B, XXX)
 * 11B = (XXX, 11Z)
 * 11Z = (11B, XXX)
 * 22A = (22B, XXX)
 * 22B = (22C, 22C)
 * 22C = (22Z, 22Z)
 * 22Z = (22B, 22B)
 * XXX = (XXX, XXX)
 * Here, there are two starting nodes, 11A and 22A (because they both end with A). As you follow each left/right instruction, use that instruction to simultaneously navigate away from both nodes you're currently on. Repeat this process until all of the nodes you're currently on end with Z. (If only some of the nodes you're on end with Z, they act like any other node and you continue as normal.) In this example, you would proceed as follows:
 *
 * Step 0: You are at 11A and 22A.
 * Step 1: You choose all of the left paths, leading you to 11B and 22B.
 * Step 2: You choose all of the right paths, leading you to 11Z and 22C.
 * Step 3: You choose all of the left paths, leading you to 11B and 22Z.
 * Step 4: You choose all of the right paths, leading you to 11Z and 22B.
 * Step 5: You choose all of the left paths, leading you to 11B and 22C.
 * Step 6: You choose all of the right paths, leading you to 11Z and 22Z.
 * So, in this example, you end up entirely on nodes that end in Z after 6 steps.
 *
 * Simultaneously start on every node that ends with A. How many steps does it take before you're only on nodes that end with Z?
 * Website:
 * <a href="https://adventofcode.com/2023/day/8#part2">Challenge</a>
 */
public abstract class Day08Q2Tests extends TestBase
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
        return "Day 08 Q2";
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
                LR
                
                11A = (11B, XXX)
                11B = (XXX, 11Z)
                11Z = (11B, XXX)
                22A = (22B, XXX)
                22B = (22C, 22C)
                22C = (22Z, 22Z)
                22Z = (22B, 22B)
                XXX = (XXX, XXX)
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
        return "6";
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
        return "14631604759649";
    }

    public static class Solution1Tests extends Day08Q2Tests
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

        @Test
        @Override
        public void testSolution() throws IOException
        {
            super.testSolution();
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

            // Keep track of the maximum step:
            long maxStep = 1;

            // Figure out the full walks for each of the starting nodes:
            Map<Node, SequencedMap<Long, Node>> startingNodeToEndingNodeToStepCountMap = new LinkedHashMap<>();
            SortedSet<Long> sortedSteps = new TreeSet<>();
            for (Node startingNode : network.startingNodes)
            {
                // Calculate the full walk for this node:
                SequencedMap<Long, Node> walkedNetwork = walkNetwork(startingNode, network, instructions);

                // Save this entry for the node:
                startingNodeToEndingNodeToStepCountMap.put(startingNode, walkedNetwork);

                // Save the step size:
                Long stepSize = walkedNetwork.firstEntry().getKey();
                sortedSteps.add(stepSize);

                // Multiply this to get the maximum step for later:
                maxStep *= stepSize;
            }
            // Now we have the full walk for each of the starting nodes.

            // We know that matches are going to need to be a multiple of the longest sequence.
            // Walk through iterations of the largest sequence until we find an overlap with all other nodes.
            long largestStep = sortedSteps.last();
            outer: for (long i = largestStep; i <= maxStep; i+= largestStep)
            {
                // Go through each of the steps and make sure they evenly divide into this step:
                for (Long sortedStep : sortedSteps)
                {
                    // Check whether it divides evenly:
                    if ((i % sortedStep) != 0) continue outer;
                }
                // If we get here then it means we found and overlap of all the numbers.
                return Long.toString(i);
            }

            // If we get here then we didn't find an answer.
            return "";
        }

        /**
         * This does one full walk of the network starting at the given node, until ALL the exit nodes are traversed.
         * @param node The node to start at.
         * @param network The network to traverse.
         * @param nextInstruction The instruction chain to walk.
         * @return The map of each step and the corresponding exit node that it hits at that step.
         */
        public  SequencedMap<Long, Node> walkNetwork(Node node, Network network, Instruction nextInstruction)
        {
            // Create a map of how long this walk takes to hit each exit node:
            SequencedMap<Long, Node> exitNodeToStepCountMap = new LinkedHashMap<>();

            // Keep track of the remaining exit nodes that we need to hit:
            Set<Node> remainingExitNodes = new HashSet<>(network.endingNodes);

            // Keep track of exit nodes that have already been hit:
            Set<Node> exitNodesAlreadyHit = new HashSet<>();

            // Walk the network:
            long stepCount = 0;
            Instruction currentInstruction = nextInstruction;
            Node currentNode = node;
            do
            {
                // We are not at the end of the network yet.

                // Follow the next instruction:
                var nextNode = currentInstruction.direction.equals(Left) ? currentNode.left : currentNode.right;

                // Detect loops:
                if (currentNode == nextNode) throw new RuntimeException("Loop on node " + currentNode + " by following instruction " + currentInstruction.index + " " + currentInstruction.direction);
                //if (stepCount > 1_000) break;

                // Check if we are at one of the exit nodes:
                if (network.endingNodes.contains(currentNode))
                {
                    // We are at one of the exit nodes.

                    // Save the step count for this exit code:
                    exitNodeToStepCountMap.put(stepCount, currentNode);

                    // Check whether we have already hit this exit node before:
                    if (exitNodesAlreadyHit.contains(currentNode)) break;
                    // Now we know that we haven't hit this node before.

                    // Flag this as an exit node we have hit:
                    exitNodesAlreadyHit.add(currentNode);

                    // Remove this from the remaining exit nodes:
                    remainingExitNodes.remove(currentNode);
                }

                // Move to the next step:
                currentNode = nextNode;
                currentInstruction = currentInstruction.next;
                stepCount++;
            }
            while (remainingExitNodes.size() > 0);

            return exitNodeToStepCountMap;
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
                    if (name.endsWith("A")) network.startingNodes.add(node);

                    // Check if this is the terminal node:
                    if (name.endsWith("Z")) network.endingNodes.add(node);

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
             * The terminal nodes where we start.
             * Ending in A.
             */
            public SequencedSet<Node> startingNodes = new LinkedHashSet<>();

            /**
             * The terminal nodes where we end.
             * Ending in Z.
             */
            public SequencedSet<Node> endingNodes = new LinkedHashSet<>();

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
