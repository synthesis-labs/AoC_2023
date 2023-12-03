package io.nanovc.aoc2023.day03q1;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.regex.Pattern;

/**
 * --- Day 3: Gear Ratios ---
 * You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you. You go inside.
 * <p>
 * It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.
 * <p>
 * "Aaah!"
 * <p>
 * You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.
 * <p>
 * The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.
 * <p>
 * The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)
 * <p>
 * Here is an example engine schematic:
 * <p>
 * 467..114..
 * ...*......
 * ..35..633.
 * ......#...
 * 617*......
 * .....+.58.
 * ..592.....
 * ......755.
 * ...$.*....
 * .664.598..
 * In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.
 * <p>
 * Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?
 * <p>
 * Website:
 * <a href="https://adventofcode.com/2023/day/3">Challenge</a>
 */
public abstract class Day03Q1Tests extends TestBase
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
        return "Day 03 Q1";
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
                467..114..
                ...*......
                ..35..633.
                ......#...
                617*......
                .....+.58.
                ..592.....
                ......755.
                ...$.*....
                .664.598..
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
        return "4361";
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
     * Gets the actual answer that we compute as the solution.
     * Usually you leave this blank initially, let the test fail and then update it to what the solution produces from {@link #solve(String)}.
     *
     * @return The actual answer that we compute as the solution.
     */
    @Override
    protected String getActualAnswer()
    {
        return "2416";
    }

    /**
     * I notice in the input data that the maximum size of a number is 3 digits.
     * <p>
     * I also see this as a 2D problem, with distances between symbols and digit runs around it.
     * <p>
     * We also only need to consider 3 rows of data in a sliding window.
     * <p>
     * There is also a certain kernel size for 7 wide and 3 high that we need to consider at a time.
     * 688..93
     * ...*...
     * .698..3
     * <p>
     * This makes it more of a machine vision problem than anything else.
     * <p>
     * When the symbol is exactly in the middle of the kernel, then we can apply the matching algorithm.
     * <p>
     * We want to create a sliding window buffer so that we can stream the data across.
     */
    public static class Solution1Tests extends Day03Q1Tests
    {
        /**
         * We notice in the window that we don't get part numbers wider than 3 digits.
         */
        public final static int MAX_DIGITS_PER_PART_NUMBER = 3;

        /**
         * The size of the kernel padding around each side of the sample point for the kernel.
         */
        public final static int KERNEL_PADDING_SIZE = MAX_DIGITS_PER_PART_NUMBER;

        /**
         * The kernel that we need to use is just the width of the maximum part on either side of the kernel which is in the middle.
         */
        public final static int MAX_KERNEL_WIDTH = KERNEL_PADDING_SIZE + 1 + KERNEL_PADDING_SIZE;

        /**
         * This is the number of lines that we need to process for a sliding kernel window.
         */
        public final static int MAX_LINES_FOR_WINDOW = 3;

        /**
         * This is a buffer that takes in the lines, one line at a time and then waits until it can process a run of the kernel sliding across the lines.
         * The buffer holds 3 lines because that is the size of the buffer.
         */
        public static class LineBuffer
        {
            /**
             * This is the maximum number of lines that this buffer holds before it starts processing the sliding kernel window.
             */
            public final int maxLines;

            /**
             * The padding size for the sliding kernel window. The actual width of the window is windowPadding + 1 + windowPadding, because the padding is the size around the sampling kernel size.
             */
            public final int windowPadding;

            /**
             * A queue of lines that we are processing.
             */
            public List<String> lines = new ArrayList<>();

            /**
             * Creates a new line buffer.
             *
             * @param maxLines      This is the maximum number of lines that this buffer holds before it starts processing the sliding kernel window.
             * @param windowPadding The padding size for the sliding kernel window. The actual width of the window is windowPadding + 1 + windowPadding, because the padding is the size around the sampling kernel size.
             */
            public LineBuffer(int maxLines, int windowPadding)
            {
                this.maxLines = maxLines;
                this.windowPadding = windowPadding;
            }

            /**
             * Adds the next line to the buffer.
             *
             * @param line The next line to add to the buffer.
             */
            public void addLine(String line)
            {
                // Check whether we are at the limit that we want:
                if (hasEnoughLinesToStartProcessing())
                {
                    // We have enough lines to process a window, meaning that our queue is full.

                    // Remove the first line to make space for the new line. We do this by bumping up each index:
                    for (int previousIndex = 0, currentIndex = 1; currentIndex < this.maxLines; previousIndex++, currentIndex++)
                    {
                        // Bump up the lines:
                        this.lines.set(previousIndex, this.lines.get(currentIndex));
                    }
                    // Now we have space for the next line in the last slot.

                    // Save the new line:
                    this.lines.set(this.maxLines - 1, line);
                } else
                {
                    // We don't have enough lines buffered yet.

                    // Just add to the buffer:
                    this.lines.add(line);
                }
            }

            /**
             * Determines whether we have enough lines to start processing sliding kernel windows.
             * It looks at the number of lines we have and the {@link #maxLines maximum that we need}.
             *
             * @return True if we have the required number of lines buffered. False if we need more lines.
             */
            public boolean hasEnoughLinesToStartProcessing()
            {
                return this.lines.size()==this.maxLines;
            }

            /**
             * This handles the sliding kernel window.
             *
             * @param kernelWindowConsumer The callback that handles the sliding kernel window.
             */
            public void handleSlidingKernelWindow(Consumer<KernelWindow> kernelWindowConsumer)
            {
                // Check whether we are ready to handle the sliding window:
                if (hasEnoughLinesToStartProcessing())
                {
                    // We are ready to slide the kernel window across the buffer.

                    // Slide across:
                    for (
                            int offset = 0,
                            max = this.lines.get(0).length() - this.windowPadding; // Size of the line in the buffer less the right side of the padding.
                            offset < max;
                            offset++
                    )
                    {
                        // Create a kernel window:
                        var kernel = new KernelWindow(this, offset, this.windowPadding, this.windowPadding, this.maxLines / 2 + 1);

                        // Allow the callback to handle the sliding kernel window:
                        kernelWindowConsumer.accept(kernel);
                    }
                }
            }
        }

        /**
         * A sliding kernel window.
         *
         * @param lineBuffer    The line buffer that we are viewing.
         * @param offset        The offset (left edge) from the start of the line buffer.
         * @param windowPadding The padding size for the sliding kernel window. The actual width of the window is windowPadding + 1 + windowPadding, because the padding is the size around the sampling kernel size.
         * @param sampleOffset  The relative offset from the left edge of the kernel window where we sample the middle of the kernel.
         * @param sampleLine    The relative offset from the top of the kernel window where we sample the middle of the kernel.
         */
        public record KernelWindow(LineBuffer lineBuffer, int offset, int windowPadding, int sampleOffset,
                                   int sampleLine)
        {
            /**
             * The width of this kernel window.
             *
             * @return The width of this kernel window.
             */
            public int width()
            {
                return 1 + (this.windowPadding << 1);
            }

            /**
             * Samples the kernel window at the given coordinates.
             *
             * @param xOffsetFromLeft The offset from the left of the kernel window.
             * @param yOffsetFromTop  The offset from the top of the kernel window.
             * @return The character at the given coordinate. It returns a blank string if the coordinate is outside the window.
             */
            public String sample(int xOffsetFromLeft, int yOffsetFromTop)
            {
                // Check the range:
                if (yOffsetFromTop < 0 ||
                    yOffsetFromTop >= this.lineBuffer().maxLines
                ) return "";

                // Get the line that we want:
                String line = this.lineBuffer().lines.get(yOffsetFromTop);

                // Get the index that we want:
                int index = this.offset + xOffsetFromLeft;

                // Make sure the index is in range:
                if (index < 0 || index >= line.length()) return "";

                // Get the character:
                return line.substring(index, index + 1);
            }

            /**
             * Samples the middle of the kernel.
             *
             * @return Gets the middle of the kernel
             */
            public String sampleMiddleOfKernel()
            {
                return sample(this.windowPadding(), this.lineBuffer().maxLines / 2);
            }

            /**
             * Samples a range of characters on a line.
             * @param xStart The start index to sample from.
             * @param xEnd The end index (inclusive) to sample to.
             * @param yOffset The offset to sample.
             * @return The sampled region.
             */
            public String sampleRange(int xStart, int xEnd, int yOffset)
            {
                StringBuilder stringBuilder = new StringBuilder();
                for (int i = xStart; i <= xEnd; i++)
                {
                    stringBuilder.append(this.sample(i, yOffset));
                }
                return stringBuilder.toString();
            }

            @Override
            public String toString()
            {
                StringBuilder stringBuilder = new StringBuilder();

                for (int yOffset = 0; yOffset < lineBuffer().maxLines; yOffset++)
                {
                    for (int xOffset = 0, max = width(); xOffset < max; xOffset++)
                    {
                        stringBuilder.append(sample(xOffset, yOffset));
                    }
                    stringBuilder.append("\n");
                }

                return stringBuilder.toString();
            }
        }

        @Override
        public String solve(String input)
        {
            // Split the input into lines:
            String[] lines = input.split("\\n");

            // Create the line buffer that we will use to process the sliding kernel window:
            var lineBuffer = new LineBuffer(MAX_LINES_FOR_WINDOW, KERNEL_PADDING_SIZE);

            // Accumulate the parts that touch a symbol:
            AtomicInteger total = new AtomicInteger();

            // Define the patterns that we want:
            var rightAlignedMatchPattern = Pattern.compile("\\d+$");
            var leftAlignedMatchPattern = Pattern.compile("^\\d+");
            var midConnectedPattern = Pattern.compile("^\\D+\\d+\\D+$");

            var topLeftPattern     = Pattern.compile("(\\d+)\\D?$");
            var midLeftPattern     = rightAlignedMatchPattern;
            var bottomLeftPattern  = rightAlignedMatchPattern;
            var topRightPattern    = Pattern.compile("^\\D?(\\d+)");
            var midRightPattern    = leftAlignedMatchPattern;
            var bottomRightPattern = leftAlignedMatchPattern;
            var topMidPattern      = midConnectedPattern;
            var bottomMidPattern   = midConnectedPattern;


            // Go through each line:
            for (int lineNumber = 0; lineNumber < lines.length; lineNumber++)
            {
                // Get the line:
                var line = lines[lineNumber];

                // Add the line to the buffer:
                lineBuffer.addLine(line);

                // Process the stream of kernel movements across the buffer:
                lineBuffer.handleSlidingKernelWindow(
                        kernel ->
                        {
                            // Sample the middle of the kernel:
                            String middleOfKernel = kernel.sampleMiddleOfKernel();

                            // Check if the kernel sees any of the special characters we want:
                            if (middleOfKernel.matches("[!@#$%^&*()_+]"))
                            {
                                // This is an interesting kernel.

                                // Look for number matches in the following regions:

                                // Top Left:
                                // XXXX...
                                // ...!...
                                // .......

                                // Top Right:
                                // ...XXXX
                                // ...!...
                                // .......

                                // Mid Left:
                                // .......
                                // XXX!...
                                // .......

                                // Mid Right:
                                // .......
                                // ...!XXX
                                // .......

                                // Bottom Left:
                                // .......
                                // ...!...
                                // XXXX...

                                // Bottom Right:
                                // .......
                                // ...!...
                                // ...XXXX

                                // Top Mid:
                                // .XXXXX.
                                // ...!...
                                // .......

                                // Bottom Mid:
                                // .......
                                // ...!...
                                // .XXXXX.

                                System.out.println("kernel = \n" + kernel);

                                // Get the top left sample:
                                var topLeftSample = kernel.sampleRange(0, KERNEL_PADDING_SIZE, 0);
                                var topLeftMatcher = topLeftPattern.matcher(topLeftSample);
                                if (topLeftMatcher.matches())
                                {
                                    int number = Integer.parseInt(topLeftMatcher.group(1));
                                    total.accumulateAndGet(number, Integer::sum);
                                }

//                                // Get the top right sample:
//                                var topRightSample = kernel.sampleRange(KERNEL_PADDING_SIZE - 1, kernel.width(), 0);
//                                var topRightMatcher = topRightPattern.matcher(topRightSample);
//                                if (topRightMatcher.matches())
//                                {
//                                    int number = Integer.parseInt(topRightMatcher.group(1));
//                                    total.accumulateAndGet(number, Integer::sum);
//                                }


                            }
                        }
                );
            }


            return null;
        }
    }

}
