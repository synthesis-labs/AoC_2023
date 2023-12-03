package AdventCode;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Advent_Day1 {

    public int Part1_CalculateCalibrationValuesSum(ArrayList<String> calibrationValues) {
        int calibrationSum = 0;
        for (String calibrationValue : calibrationValues) {
            Pattern pattern_oneDigit = Pattern.compile("^\\D*(?<onlyDigit>\\d)\\D*$");
            Pattern pattern_multipleDigits = Pattern.compile("^\\D*(?<firstDigit>\\d).*(?<lastDigit>\\d)\\D*$");
            Matcher matcher_oneDigit = pattern_oneDigit.matcher(calibrationValue);
            Matcher matcher_multipleDigits = pattern_multipleDigits.matcher(calibrationValue);

            StringBuilder sb = new StringBuilder();

            if (matcher_oneDigit.find()) {

                String onlyDigit = matcher_oneDigit.group("onlyDigit");
                int calibrationNumber = Integer.parseInt(sb.append(onlyDigit).append(onlyDigit).toString());

                calibrationSum += calibrationNumber;

            } else if (matcher_multipleDigits.find()) {
                String firstDigit = matcher_multipleDigits.group("firstDigit");
                String lastDigit = matcher_multipleDigits.group("lastDigit");

                int calibrationNumber = Integer.parseInt(sb.append(firstDigit).append(lastDigit).toString());

                calibrationSum += calibrationNumber;
            }


        }
        return calibrationSum;
    }

    public int Part2_CalculateCalibrationValuesSum(ArrayList<String> calibrationValues) {
        int calibrationSum = 0;
        String numberWords = "one|two|three|four|five|six|seven|eight|nine";
        for (String calibrationValue : calibrationValues) {


            Pattern pattern_firstDigit_int = Pattern.compile("^\\D*(?<firstDigit>\\d).*");
            Pattern pattern_firstDigit_word = Pattern.compile("^\\D*?(?<firstDigit>" + "one|two|three|four|five|six|seven|eight|nine" + ").*");

            Matcher matcher_firstDigit_int = pattern_firstDigit_int.matcher(calibrationValue);
            Matcher matcher_firstDigit_word = pattern_firstDigit_word.matcher(calibrationValue);

            String _firstDigit = "firstDigit";
            int firstDigitIndex_int = matcher_firstDigit_int.find() ? matcher_firstDigit_int.start(_firstDigit) : -1;
            int firstDigitIndex_word = matcher_firstDigit_word.find() ? matcher_firstDigit_word.start(_firstDigit) : -1;

            String firstDigit = (firstDigitIndex_int != -1 && firstDigitIndex_int < firstDigitIndex_word) || firstDigitIndex_word == -1?
                    matcher_firstDigit_int.group(_firstDigit) :
                    wordToNumber_HelperMethod(matcher_firstDigit_word.group(_firstDigit));


            Pattern pattern_lastDigit_int = Pattern.compile(".*(?<lastDigit>\\d)\\D*$");
            Pattern pattern_lastDigit_word = Pattern.compile("^.*(?<lastDigit>" + "one|two|three|four|five|six|seven|eight|nine" + ")\\D*$");
            Matcher matcher_lastDigit_int = pattern_lastDigit_int.matcher(calibrationValue);
            Matcher matcher_lastDigit_word = pattern_lastDigit_word.matcher(calibrationValue);

            String _lastDigit = "lastDigit";
            int lastDigitIndex_int = matcher_lastDigit_int.find() ? matcher_lastDigit_int.start(_lastDigit) : -1;
            int lastDigitIndex_word = matcher_lastDigit_word.find() ? matcher_lastDigit_word.start(_lastDigit) : -1;

            String lastDigit = lastDigitIndex_int > lastDigitIndex_word ?
                    matcher_lastDigit_int.group(_lastDigit) :
                    wordToNumber_HelperMethod(matcher_lastDigit_word.group(_lastDigit));

            StringBuilder sb = new StringBuilder();

            int calibrationNumber = Integer.parseInt(sb.append(firstDigit).append(lastDigit).toString());

            calibrationSum += calibrationNumber;


        }
        return calibrationSum;
    }


    public String wordToNumber_HelperMethod(String word) {
        String number = switch (word) {
            case "one" -> "1";
            case "two" -> "2";
            case "three" -> "3";
            case "four" -> "4";
            case "five" -> "5";
            case "six" -> "6";
            case "seven" -> "7";
            case "eight" -> "8";
            case "nine" -> "9";
            default -> "";
        };

        return number;
    }
}
