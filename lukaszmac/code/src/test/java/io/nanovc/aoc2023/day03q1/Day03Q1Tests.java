package io.nanovc.aoc2023.day03q1;

import io.nanovc.aoc2023.TestBase;
import org.junit.jupiter.api.Test;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.*;

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
        return "538046";
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
         * The pattern for the symbols to match a kernel around.
         */
        public static final String SYMBOL_PATTERN = "[!@#$%^&*()_+=~\\-/\\\\]";

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
                            int xOffset = this.windowPadding,
                            max = this.lines.get(0).length() - this.windowPadding; // Size of the line in the buffer less the right side of the padding.
                            xOffset < max;
                            xOffset++
                    )
                    {
                        // Create a kernel window:
                        var kernel = new KernelWindow(this, xOffset, this.windowPadding);

                        // Allow the callback to handle the sliding kernel window:
                        kernelWindowConsumer.accept(kernel);
                    }
                }
            }
        }

        /**
         * A sliding kernel window.
         * The coordinates of the sliding kernel window are from: -windowPadding to +windowPadding
         *
         * @param lineBuffer    The line buffer that we are viewing.
         * @param offset        The offset (left edge) from the start of the line buffer.
         * @param windowPadding The padding size for the sliding kernel window. The actual width of the window is windowPadding + 1 + windowPadding, because the padding is the size around the sampling kernel size.
         */
        public record KernelWindow(LineBuffer lineBuffer, int offset, int windowPadding)
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
             * @param xOffset The offset from the middle of the kernel window. -ve means to sample to the left of the middle of the kernel. +ve means to sample to the right. 0 means to sample at the middle of the kernel.
             * @param yOffset  The offset from the middle of the kernel window. -ve means to sample above the middle of the kernel. +ve means to sample down. 0 means to sample at the middle of the kernel.
             * @return The character at the given coordinate. It returns a blank string if the coordinate is outside the window.
             */
            public String sample(int xOffset, int yOffset)
            {
                // Get the Y offset:
                int yIndex = (this.lineBuffer().lines.size() / 2) + yOffset;

                // Check the Y range:
                if (yIndex < 0 ||
                        yIndex >= this.lineBuffer().maxLines
                ) return "";

                // Get the line that we want:
                String line = this.lineBuffer().lines.get(yIndex);

                // Get the index that we want:
                int xIndex = this.offset + xOffset;

                // Make sure the index is in range:
                if (xIndex < 0 || xIndex >= line.length()) return "";

                // Get the character:
                return line.substring(xIndex, xIndex + 1);
            }

            /**
             * Samples the middle of the kernel.
             *
             * @return Gets the middle of the kernel
             */
            public String sampleMiddleOfKernel()
            {
                return sample(0, 0);
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

                for (int yOffset = -lineBuffer().maxLines / 2, maxY = lineBuffer().maxLines / 2; yOffset <= maxY; yOffset++)
                {
                    for (int xOffset = -this.windowPadding(), maxX = this.windowPadding(); xOffset <= maxX; xOffset++)
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
            var rightAlignedMatchPattern = Pattern.compile("(\\d+)$");
            var leftAlignedMatchPattern = Pattern.compile("^(\\d+)");

            var topLeftPattern     = rightAlignedMatchPattern;
            var middleLeftPattern  = rightAlignedMatchPattern;
            var bottomLeftPattern  = rightAlignedMatchPattern;
            var topRightPattern    = leftAlignedMatchPattern;
            var middleRightPattern = leftAlignedMatchPattern;
            var bottomRightPattern = leftAlignedMatchPattern;


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
                            if (middleOfKernel.matches(SYMBOL_PATTERN))
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

                                System.out.println("\n-----------\nkernel = \n" + kernel);

                                // Try to parse out the middle top:
                                Integer topMiddleNumber = parseMiddleNumber(kernel.lineBuffer().lines.get(0), kernel.offset());
                                if (topMiddleNumber != null)
                                {
                                    // We found a number at the top middle.
                                    // Accumulate the total:
                                    total.accumulateAndGet(topMiddleNumber, Integer::sum);
                                    System.out.println("Top Middle Number Found = " + topMiddleNumber);
                                }
                                else
                                {
                                    // We don't have a number touching the middle at the top.

                                    // Get the top left sample:
                                    var topLeftSample = kernel.sampleRange(-kernel.windowPadding(), -1, -1);
                                    var topLeftMatcher = topLeftPattern.matcher(topLeftSample);
                                    if (topLeftMatcher.find())
                                    {
                                        int number = Integer.parseInt(topLeftMatcher.group(1));
                                        total.accumulateAndGet(number, Integer::sum);
                                        System.out.println("Top Left Number Found = " + number);
                                    }

                                    // Get the top right sample:
                                    var topRightSample = kernel.sampleRange(1, kernel.windowPadding(), -1);
                                    var topRightMatcher = topRightPattern.matcher(topRightSample);
                                    if (topRightMatcher.find())
                                    {
                                        int number = Integer.parseInt(topRightMatcher.group(1));
                                        total.accumulateAndGet(number, Integer::sum);
                                        System.out.println("Top Right Number Found = " + number);
                                    }
                                }

                                // Try to parse out the middle bottom:
                                Integer bottomMiddleNumber = parseMiddleNumber(kernel.lineBuffer().lines.getLast(), kernel.offset());
                                if (bottomMiddleNumber != null)
                                {
                                    // We found a number at the bottom middle.
                                    // Accumulate the total:
                                    total.accumulateAndGet(bottomMiddleNumber, Integer::sum);
                                    System.out.println("Bottom Middle Number Found = " + bottomMiddleNumber);
                                }
                                else
                                {
                                    // We don't have a number touching the middle at the bottom.

                                    // Get the bottom left sample:
                                    var bottomLeftSample = kernel.sampleRange(-kernel.windowPadding(), -1, 1);
                                    var bottomLeftMatcher = bottomLeftPattern.matcher(bottomLeftSample);
                                    if (bottomLeftMatcher.find())
                                    {
                                        int number = Integer.parseInt(bottomLeftMatcher.group(1));
                                        total.accumulateAndGet(number, Integer::sum);
                                        System.out.println("Bottom Left Number Found = " + number);
                                    }

                                    // Get the bottom right sample:
                                    var bottomRightSample = kernel.sampleRange(1, kernel.windowPadding(), 1);
                                    var bottomRightMatcher = bottomRightPattern.matcher(bottomRightSample);
                                    if (bottomRightMatcher.find())
                                    {
                                        int number = Integer.parseInt(bottomRightMatcher.group(1));
                                        total.accumulateAndGet(number, Integer::sum);

                                        System.out.println("Bottom Right Number Found = " + number);
                                    }
                                }

                                // Try to parse out the middle left:
                                var middleLeftSample = kernel.sampleRange(-kernel.windowPadding(), -1, 0);
                                var middleLeftMatcher = middleLeftPattern.matcher(middleLeftSample);
                                if (middleLeftMatcher.find())
                                {
                                    int number = Integer.parseInt(middleLeftMatcher.group(1));
                                    total.accumulateAndGet(number, Integer::sum);
                                    System.out.println("Middle Left Number Found = " + number);
                                }

                                // Try to parse out the middle right:
                                var middleRightSample = kernel.sampleRange(1, kernel.windowPadding(), 0);
                                var middleRightMatcher = middleRightPattern.matcher(middleRightSample);
                                if (middleRightMatcher.find())
                                {
                                    int number = Integer.parseInt(middleRightMatcher.group(1));
                                    total.accumulateAndGet(number, Integer::sum);
                                    System.out.println("Middle Right Number Found = " + number);
                                }

                            }
                        }
                );
            }

            return Integer.toString(total.get());
        }

        public Integer parseMiddleNumber(String line, int position)
        {
            // Create a range of characters for the digits:
            int start = position;
            int end = position + 1;

            // Check whether we have a number at the given position:
            String character = line.substring(start, end);
            if (character.matches("\\d"))
            {
                // This is a digit.

                // Widen towards the left:
                do
                {
                    // Move to the left:
                    start--;

                    // Sample:
                    character = line.substring(start, start+1);
                }
                while (start >= 0 && character.matches("\\d"));
                // Now we have stepped past the start of the number.
                // Step forward to get the beginning.
                start++;


                // Widen towards the right:
                do
                {
                    // Move to the right:
                    end++;

                    // Sample:
                    character = line.substring(end - 1, end);
                }
                while (end < line.length() && character.matches("\\d"));
                // Now we have stepped past the end of the number.
                // Step backward to get the end.
                end--;

                // Parse the number:
                var numberText = line.substring(start, end);
                int number = Integer.parseInt(numberText);
                return number;
            }
            // If we get here then we didn't find the number.
            return null;
        }


        @Test
        public void testParsingKernelWithOneNumber()
        {
            LineBuffer lineBuffer = new LineBuffer(MAX_LINES_FOR_WINDOW, KERNEL_PADDING_SIZE);
            lineBuffer.addLine("....8.&");
            lineBuffer.addLine("396*...");
            lineBuffer.addLine(".......");

            Integer middleNumber = parseMiddleNumber("....8.&", 3);
            assertNull(middleNumber);

            var leftAlignedMatchPattern = Pattern.compile("^(\\d+)");

            var topRightPattern    = leftAlignedMatchPattern;

            // Create the kernel:
            var kernel = new KernelWindow(lineBuffer, KERNEL_PADDING_SIZE, KERNEL_PADDING_SIZE);

            // Get the top right sample:
            var topRightSample = kernel.sampleRange(1, kernel.windowPadding(), -1);
            var topRightMatcher = topRightPattern.matcher(topRightSample);
            if (topRightMatcher.find())
            {
                int number = Integer.parseInt(topRightMatcher.group(1));
                assertEquals(8, number);
            }
            else fail();
        }
    }

}
