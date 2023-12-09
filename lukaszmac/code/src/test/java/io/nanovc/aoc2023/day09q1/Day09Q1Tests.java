package io.nanovc.aoc2023.day09q1;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.io.IOException;

/**
 * --- Day 9: Mirage Maintenance ---
 * You ride the camel through the sandstorm and stop where the ghost's maps told you to stop. The sandstorm subsequently subsides, somehow seeing you standing at an oasis!
 *
 * The camel goes to get some water and you stretch your neck. As you look up, you discover what must be yet another giant floating island, this one made of metal! That must be where the parts to fix the sand machines come from.
 *
 * There's even a hang glider partially buried in the sand here; once the sun rises and heats up the sand, you might be able to use the glider and the hot air to get all the way up to the metal island!
 *
 * While you wait for the sun to rise, you admire the oasis hidden here in the middle of Desert Island. It must have a delicate ecosystem; you might as well take some ecological readings while you wait. Maybe you can report any environmental instabilities you find to someone so the oasis can be around for the next sandstorm-worn traveler.
 *
 * You pull out your handy Oasis And Sand Instability Sensor and analyze your surroundings. The OASIS produces a report of many values and how they are changing over time (your puzzle input). Each line in the report contains the history of a single value. For example:
 *
 * 0 3 6 9 12 15
 * 1 3 6 10 15 21
 * 10 13 16 21 30 45
 * To best protect the oasis, your environmental report should include a prediction of the next value in each history. To do this, start by making a new sequence from the difference at each step of your history. If that sequence is not all zeroes, repeat this process, using the sequence you just generated as the input sequence. Once all of the values in your latest sequence are zeroes, you can extrapolate what the next value of the original history should be.
 *
 * In the above dataset, the first history is 0 3 6 9 12 15. Because the values increase by 3 each step, the first sequence of differences that you generate will be 3 3 3 3 3. Note that this sequence has one fewer value than the input sequence because at each step it considers two numbers from the input. Since these values aren't all zero, repeat the process: the values differ by 0 at each step, so the next sequence is 0 0 0 0. This means you have enough information to extrapolate the history! Visually, these sequences can be arranged like this:
 *
 * 0   3   6   9  12  15
 *   3   3   3   3   3
 *     0   0   0   0
 * To extrapolate, start by adding a new zero to the end of your list of zeroes; because the zeroes represent differences between the two values above them, this also means there is now a placeholder in every sequence above it:
 *
 * 0   3   6   9  12  15   B
 *   3   3   3   3   3   A
 *     0   0   0   0   0
 * You can then start filling in placeholders from the bottom up. A needs to be the result of increasing 3 (the value to its left) by 0 (the value below it); this means A must be 3:
 *
 * 0   3   6   9  12  15   B
 *   3   3   3   3   3   3
 *     0   0   0   0   0
 * Finally, you can fill in B, which needs to be the result of increasing 15 (the value to its left) by 3 (the value below it), or 18:
 *
 * 0   3   6   9  12  15  18
 *   3   3   3   3   3   3
 *     0   0   0   0   0
 * So, the next value of the first history is 18.
 *
 * Finding all-zero differences for the second history requires an additional sequence:
 *
 * 1   3   6  10  15  21
 *   2   3   4   5   6
 *     1   1   1   1
 *       0   0   0
 * Then, following the same process as before, work out the next value in each sequence from the bottom up:
 *
 * 1   3   6  10  15  21  28
 *   2   3   4   5   6   7
 *     1   1   1   1   1
 *       0   0   0   0
 * So, the next value of the second history is 28.
 *
 * The third history requires even more sequences, but its next value can be found the same way:
 *
 * 10  13  16  21  30  45  68
 *    3   3   5   9  15  23
 *      0   2   4   6   8
 *        2   2   2   2
 *          0   0   0
 * So, the next value of the third history is 68.
 *
 * If you find the next value for each history in this example and add them together, you get 114.
 *
 * Analyze your OASIS report and extrapolate the next value for each history. What is the sum of these extrapolated values?
 *
 * To begin, get your puzzle input.
 * <p>
 * Website:
 * <a href="https://adventofcode.com/2023/day/9">Challenge</a>
 */
public abstract class Day09Q1Tests extends TestBase
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
        return "Day 09 Q1";
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
                0 3 6 9 12 15
                1 3 6 10 15 21
                10 13 16 21 30 45
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
        return "114";
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
        return "1725987467";
    }

    public static class Solution1Tests extends Day09Q1Tests
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
            long result = 0L;

            // Parse the lines:
            String[] lines = input.split("\\n");

            // Parse each input line:
            for (String line : lines)
            {
                // Create the starting node for this line:
                Node startingNode = null;

                // Keep track of the previous node:
                Node previousNode = null;

                // Parse the numbers:
                String[] numberSplit = line.split("\\s+");
                for (String number : numberSplit)
                {
                    // Create the node for this number:
                    Node node = new Node();

                    // Parse the number:
                    node.value = Long.parseLong(number);

                    // Check if this is the first node in this line:
                    if (startingNode == null)
                    {
                        // This is the first node.
                        // Save it as the first node:
                        startingNode = node;
                    }
                    else
                    {
                        // This is not the starting node.

                        // Save the reference to the previous node:
                        node.previous = previousNode;
                    }

                    // Update the previous node to point at this node:
                    if (previousNode != null)
                    {
                        previousNode.next = node;
                    }

                    // Work out the derivatives so that we can predict the next number:
                    node.createDerivatives();

                    // Save this as the previous node for the next step:
                    previousNode = node;
                }
                if (startingNode == null) throw new RuntimeException("Could not create a chain of nodes");
                // Now we have the input nodes for this line and all the derivatives.

                // Find the node of the tree that has zero change:
                var stableDerivativeNode = startingNode.findStableDerivative();

                // Move to the end of that level so that we can extrapolate:
                var endOfStableDerivativeNode = stableDerivativeNode.endOfChain();

                // Extrapolate the values:
                var newLeafNode = endOfStableDerivativeNode.extrapolate();

                // Sum up the extrapolated values:
                result += newLeafNode.value;
            }

            return Long.toString(result);
        }

        /**
         * A node in the derivative tree.
         * It points at the two values that it is derived from, as well as the previous and next nodes. It also points at the previous and next derivatives
         *
         *     L  R
         *      \/
         * P <- N -> X
         *     / \
         *   PD   ND
         */
        public static class Node
        {
            /**
             * The value for the node.
             */
            public long value;

            /**
             * The node to the left of this one that it was derived from.
             * Null if this is a leaf node (input).
             */
            public Node left;

            /**
             * The node to the right of this one that it was derived from.
             * Null if this is a leaf node (input).
             */
            public Node right;

            /**
             * The previous node at this level.
             */
            public Node previous;

            /**
             * The next node in this chain.
             */
            public Node next;

            /**
             * The derivative that looks at this node and the previous node.
             * If this is the start of a level then this is null.
             */
            public Node previousDerivative;

            /**
             * The derivative that looks at this node and the next node.
             */
            public Node nextDerivative;

            /**
             * This creates the derivatives as far as it can until the derivative is zero.
             */
            public void createDerivatives()
            {
                // Start at the current node:
                Node currentNode = this;
                while (currentNode != null)
                {
                    // Check whether we can create a derivative for this node:
                    if (currentNode.previous != null)
                    {
                        // We have a previous node, so we can create the derivative from the previous and next node.

                        // Create the derivative:
                        Node derivative = new Node();
                        currentNode.previousDerivative = derivative;
                        currentNode.previous.nextDerivative = derivative;

                        // Save the nodes that this derivative was created from:
                        derivative.left = currentNode.previous;
                        derivative.right = currentNode;

                        // Work out the derivative value:
                        derivative.value = derivative.right.value - derivative.left.value;

                        // Point at the previous derivative node:
                        derivative.previous = currentNode.previous.previousDerivative;

                        // Point at this derivative:
                        if (currentNode.previous.previousDerivative!= null) currentNode.previous.previousDerivative.next = derivative;

                        // Move up the derivative tree:
                        currentNode = derivative;
                    }
                    else
                    {
                        // We do not have a previous node.
                        // Flag that we are done working out derivatives:
                        currentNode = null;
                    }
                }
            }

            @Override
            public String toString()
            {
                if (left != null && right != null) return right.value + "-" + left.value + "=" + value;
                else return "" + value;
            }

            /**
             * This finds the stable derivative node by inspecting the derivatives of the current node until it finds a depth that has all zeros.
             * @return This inspects the derivatives of the current node until it finds a depth that has all zeros.
             */
            public Node findStableDerivative()
            {
                // Start at the current node and find the derivative level where we have all zeros:
                outer: for (var currentNode = this; currentNode != null; currentNode = currentNode.nextDerivative)
                {
                    // Check whether all nodes ahead are zero:
                    for (var nextNode = currentNode; nextNode!=null; nextNode = nextNode.next)
                    {
                        // Check if the value is zero:
                        if (nextNode.value != 0)
                        {
                            // This node is not equal to zero, so this can't be the level we are looking for.
                            continue outer;
                        }
                    }
                    // If we get here then we have found the level we want.
                    return currentNode;
                }
                // If we get here then there is no stable level.
                throw new RuntimeException("No stable level found");
            }

            /**
             * Gets the end of the chain of nodes.
             * @return The end of this chain of nodes.
             */
            public Node endOfChain()
            {
                Node currentNode = this;
                while (currentNode.next != null)
                {
                    // Walk forward:
                    currentNode = currentNode.next;
                }
                return currentNode;
            }

            /**
             * This should be called at the level where we have a stable derivative {@link #findStableDerivative()} and then from the end node.
             * This extrapolates down to the leaf nodes.
             * @return The extrapolated leaf node.
             */
            public Node extrapolate()
            {
                // Start at the current node:
                Node currentNode = this;

                // Create the next derivative:
                Node nextDerivative = new Node();
                nextDerivative.previous = currentNode;
                currentNode.next = nextDerivative;
                nextDerivative.value = currentNode.value;
                nextDerivative.left = currentNode.right;

                // Move to the next derivative:
                currentNode = nextDerivative;

                // Extrapolate the value until we get to the leaf level:
                while (currentNode.left != null)
                {
                    // Extrapolate the right node and value:
                    Node extrapolatedRightNode = new Node();
                    extrapolatedRightNode.previousDerivative = currentNode;
                    extrapolatedRightNode.value = currentNode.left.value + currentNode.value;
                    extrapolatedRightNode.left = currentNode.left.right;

                    // Update the current node:
                    currentNode.right = extrapolatedRightNode;

                    // Move towards the leaf level:
                    currentNode = extrapolatedRightNode;
                }

                return currentNode;
            }
        }
    }

}
