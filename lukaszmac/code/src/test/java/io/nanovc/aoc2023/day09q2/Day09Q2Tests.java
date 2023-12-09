package io.nanovc.aoc2023.day09q2;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.io.IOException;

/**
 * --- Part Two ---
 * Of course, it would be nice to have even more history included in your report. Surely it's safe to just extrapolate backwards as well, right?
 *
 * For each history, repeat the process of finding differences until the sequence of differences is entirely zero. Then, rather than adding a zero to the end and filling in the next values of each previous sequence, you should instead add a zero to the beginning of your sequence of zeroes, then fill in new first values for each previous sequence.
 *
 * In particular, here is what the third example history looks like when extrapolating back in time:
 *
 * 5  10  13  16  21  30  45
 *   5   3   3   5   9  15
 *    -2   0   2   4   6
 *       2   2   2   2
 *         0   0   0
 * Adding the new values on the left side of each sequence from bottom to top eventually reveals the new left-most history value: 5.
 *
 * Doing this for the remaining example data above results in previous values of -3 for the first history and 0 for the second history. Adding all three new values together produces 2.
 *
 * Analyze your OASIS report again, this time extrapolating the previous value for each history. What is the sum of these extrapolated values?
 * <p>
 * Website:
 * <a href="https://adventofcode.com/2023/day/9#part2">Challenge</a>
 */
public abstract class Day09Q2Tests extends TestBase
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
        return "971";
    }

    public static class Solution1Tests extends Day09Q2Tests
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

                // Extrapolate the values:
                var newLeafNode = stableDerivativeNode.extrapolateBackward();

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
             * This extrapolates values forward down to the leaf nodes.
             * This should be called at the level where we have a stable derivative {@link #findStableDerivative()} and then from the end node.
             * @return The extrapolated leaf node.
             */
            public Node extrapolateForward()
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


            /**
             * This extrapolates backwards down to the leaf nodes.
             * This should be called at the level where we have a stable derivative {@link #findStableDerivative()} and then from the end node.
             * @return The extrapolated leaf node.
             */
            public Node extrapolateBackward()
            {
                // Start at the current node:
                Node currentNode = this;

                // Create the previous derivative:
                Node previousDerivative = new Node();
                previousDerivative.next = currentNode;
                currentNode.previous = previousDerivative;
                previousDerivative.value = currentNode.value;
                previousDerivative.right = currentNode.left;

                // Move to the previous derivative:
                currentNode = previousDerivative;

                // Extrapolate the value until we get to the leaf level:
                while (currentNode.right != null)
                {
                    // Extrapolate the left node and value:
                    Node extrapolatedLeftNode = new Node();
                    extrapolatedLeftNode.nextDerivative = currentNode;
                    extrapolatedLeftNode.value = currentNode.right.value - currentNode.value;
                    extrapolatedLeftNode.right = currentNode.right.left;

                    // Update the current node:
                    currentNode.left = extrapolatedLeftNode;

                    // Move towards the leaf level:
                    currentNode = extrapolatedLeftNode;
                }

                return currentNode;
            }
        }
    }

}
