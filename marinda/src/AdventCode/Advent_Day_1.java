package AdventCode;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Advent_Day_1 {

    public int Day1_Solution1(ArrayList<String> calibrationValues) {
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

}
