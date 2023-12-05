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

    public int Part2(ArrayList<String> scratchcardsList) {
        int scratchCardsCount = calculateScratchcardsCount(scratchcardsList);

        return scratchCardsCount;
    }

    private int calculateScratchcardsCount(ArrayList<String> scratchcardsList) {
        HashMap<Integer, Integer> cardNumberToCountMap = new HashMap<>();
        int scratchcardsCount = 0;


        for (int cardNumber = 1, scratchcardsListSize = scratchcardsList.size(); cardNumber <= scratchcardsListSize; cardNumber++) {
            // Count this card:
            int thisCardCardCount = addToCardCount(cardNumberToCountMap, cardNumber, 1);

            String scratchcard = scratchcardsList.get(cardNumber - 1);

            HashMap<Integer, List<List<Integer>>> cardDetailsMap = extractCardDetails(scratchcard);

            List<List<Integer>> numbers = cardDetailsMap.get(cardNumber);
            List<Integer> winningNumbers = numbers.get(0);
            List<Integer> ourNumbers = numbers.get(1);

            // Get winning numbers count for this card:
            int numberOfCopiesWon = getWinningNumberCountPerCard(winningNumbers, ourNumbers);

            // get the new copies won for this card:
            List<Integer> listOfNewCopiesWonForCard = getListOfNewCopiesWonForCard(cardNumber, numberOfCopiesWon);

            // Go through list and add to card count:
            for (Integer newCopyCardNumber : listOfNewCopiesWonForCard) {
                int numberToAdd = 1 * thisCardCardCount;
                addToCardCount(cardNumberToCountMap, newCopyCardNumber, numberToAdd);
            }

        }

        for (Map.Entry<Integer, Integer> cardCountEntry : cardNumberToCountMap.entrySet()) {
            scratchcardsCount = scratchcardsCount + cardCountEntry.getValue();
        }

        return scratchcardsCount;
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

        HashMap<String, List<Integer>> scratchcardsWinningNumbers = new HashMap<>();
        for (int cardNumber = 1, scratchcardsListSize = scratchcardsList.size(); cardNumber <= scratchcardsListSize; cardNumber++) {
            String scratchcard = scratchcardsList.get(cardNumber - 1);

            HashMap<Integer, List<List<Integer>>> cardDetailsMap = extractCardDetails(scratchcard);
            List<Integer> winningNumbers = cardDetailsMap.get(cardNumber).get(0);
            List<Integer> ourNumbers = cardDetailsMap.get(cardNumber).get(1);


            // Find our winning numbers:
            List<Integer> ourWinningNumbers = findOurWinningNumbers(winningNumbers, ourNumbers);

            // Add Card with our winning numbers to list:
            if (!ourWinningNumbers.isEmpty()) {
                scratchcardsWinningNumbers.put(String.valueOf(cardNumber), ourWinningNumbers);
            }
        }
        return scratchcardsWinningNumbers;
    }

    private HashMap<Integer, List<List<Integer>>> extractCardDetails(String scratchcard) {
        String _cardNumber = "cardNumber";
        String _num = "num";

        HashMap<Integer, List<List<Integer>>> cardDetailsMap = new HashMap<>();

        // Card row pattern:
        Pattern cardInfoPattern = Pattern.compile("Card\\s*(?<" + _cardNumber + ">\\d+)");
        Pattern numsPattern = Pattern.compile("(\\s*(?<" + _num + ">\\d+)\\s*)");


        String[] splitCardAndNumbers = scratchcard.split(":");
        String cardString = splitCardAndNumbers[0];
        String[] splitWinningAndOurNumbers = splitCardAndNumbers[1].split("\\|");
        String winningNumbersString = splitWinningAndOurNumbers[0];
        String ourNumbersString = splitWinningAndOurNumbers[1];

        Matcher cardInfoMatcher = cardInfoPattern.matcher(cardString);
        Matcher winningNumbersMatcher = numsPattern.matcher(winningNumbersString);
        Matcher ourNumbersMatcher = numsPattern.matcher(ourNumbersString);

        int cardNumber = -1;
        List<Integer> winningNumbers = new ArrayList<>();
        List<Integer> ourNumbers = new ArrayList<>();

        if (cardInfoMatcher.find()) {
            // Get the card number:
            cardNumber = Integer.parseInt(cardInfoMatcher.group(_cardNumber));
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

        cardDetailsMap.put(cardNumber, List.of(winningNumbers, ourNumbers));

        return cardDetailsMap;
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

    public int getWinningNumberCountPerCard(List<Integer> winningNumbers, List<Integer> ourNumbers) {
        int winningNumberCount = 0;

        List<Integer> ourWinningNumbers = findOurWinningNumbers(winningNumbers, ourNumbers);

        winningNumberCount = ourWinningNumbers.size();

        return winningNumberCount;
    }

    public List<Integer> getListOfNewCopiesWonForCard(int cardNumber, int numberOfCopiesWon) {
        List<Integer> newCopiesWonList = new ArrayList<>();

        for (int copiedCardNumber = cardNumber + 1; copiedCardNumber <= cardNumber + numberOfCopiesWon; copiedCardNumber++) {
            newCopiesWonList.add(copiedCardNumber);
        }

        return newCopiesWonList;
    }


    private static int addToCardCount(HashMap<Integer, Integer> cardNumberToCountMap, int scratchcardNumber, int numberToAdd) {
        Integer cardNumberCount = cardNumberToCountMap.get(scratchcardNumber);
        if (cardNumberCount == null) {
            cardNumberToCountMap.put(scratchcardNumber, numberToAdd);
        } else {
            cardNumberToCountMap.put(scratchcardNumber, cardNumberCount + numberToAdd);
        }

        return cardNumberToCountMap.get(scratchcardNumber).intValue();
    }
}
