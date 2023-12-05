package AdventCode;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Advent_Day4_Scratchcards {

    public Advent_Day4_Scratchcards() {
    }

    public int Part1(List<String> scratchcardList) {


        HashMap<String, List<Integer>> scratchCardWinningNumbers = findWinningNumbersPerCard(scratchcardList);

        List<Integer> scratchcardPoints = calculatePointsPerScratchcard(scratchCardWinningNumbers);

        int scratchcardPointsSum = scratchcardPoints.stream().mapToInt(Integer::intValue).sum();

        return scratchcardPointsSum;
    }

    public int Part2(ArrayList<String> scratchcardList) {

        HashMap<String, List<Integer>> scratchCardWinningNumbers = findWinningNumbersPerCard(scratchcardList);

        List<Integer> scratchcardPoints = calculatePointsPerScratchcard(scratchCardWinningNumbers);

        int scratchcardPointsSum = scratchcardPoints.stream().mapToInt(Integer::intValue).sum();

        return scratchcardPointsSum;
    }

    private List<Integer> calculatePointsPerScratchcard(HashMap<String, List<Integer>> scratchcardsWinningNumbers) {
        List<Integer> scratchcardPointList = new ArrayList<>();


        for (Map.Entry<String, List<Integer>> thisCard : scratchcardsWinningNumbers.entrySet()) {
            int points = 0;
            List<Integer> thisCardWinningNumbers = thisCard.getValue();

            for (int winningNumber = 0; winningNumber < thisCardWinningNumbers.size(); winningNumber++) {

                points = points == 0 ? 1 : points * 2;

            }

            scratchcardPointList.add(points);
        }
        return scratchcardPointList;
    }

    /**
     * Find the winning numbers for each card in the list.
     *
     * @param scratchcardsList
     * @return
     */
    public HashMap<String, List<Integer>> findWinningNumbersPerCard(List<String> scratchcardsList) {
        String _cardNumber = "cardNumber";
        String _num = "num";

        HashMap<String, List<Integer>> scratchcardsWinningNumbers = new HashMap<>();

        // Card row pattern:
        Pattern cardInfoPattern = Pattern.compile("Card\\s*(?<" + _cardNumber + ">\\d+)");
        Pattern numsPattern = Pattern.compile("(\\s*(?<" + _num + ">\\d+)\\s*)");

        for (String scratchcard : scratchcardsList) {
            String[] splitCardAndNumbers = scratchcard.split(":");
            String cardString = splitCardAndNumbers[0];
            String[] splitWinningAndOurNumbers = splitCardAndNumbers[1].split("\\|");
            String winningNumbersString = splitWinningAndOurNumbers[0];
            String ourNumbersString = splitWinningAndOurNumbers[1];

            Matcher cardInfoMatcher = cardInfoPattern.matcher(cardString);
            Matcher winningNumbersMatcher = numsPattern.matcher(winningNumbersString);
            Matcher ourNumbersMatcher = numsPattern.matcher(ourNumbersString);

            String cardNumber = "";
            List<Integer> winningNumbers = new ArrayList<>();
            List<Integer> ourNumbers = new ArrayList<>();

            if (cardInfoMatcher.find()) {
                // Get the card number:
                cardNumber = cardInfoMatcher.group(_cardNumber);
            }

            while (winningNumbersMatcher.find()) {
                // Get the scratchcard winning numbers:
                int winningNumber = Integer.parseInt(winningNumbersMatcher.group(_num));
                winningNumbers.add(winningNumber);
            }

            while (ourNumbersMatcher.find()) {
                // Get our numbers:
                int ourNumber = Integer.parseInt(ourNumbersMatcher.group(_num));
                ourNumbers.add(ourNumber);
            }

            // Find our winning numbers:
            List<Integer> ourWinningNumbers = findOurWinningNumbers(winningNumbers, ourNumbers);

            // Add Card with our winning numbers to list:
            if (!ourWinningNumbers.isEmpty()) {
                scratchcardsWinningNumbers.put(cardNumber, ourWinningNumbers);
            }
        }
        return scratchcardsWinningNumbers;
    }

    public List<Integer> findOurWinningNumbers(List<Integer> winningNumbers, List<Integer> ourNumbers) {
        List<Integer> ourWinningNumbers = new ArrayList<>();

        for (int ourNumber : ourNumbers) {
            if (winningNumbers.contains(ourNumber)) {
                ourWinningNumbers.add(ourNumber);
            }
        }
        return ourWinningNumbers;
    }
}
