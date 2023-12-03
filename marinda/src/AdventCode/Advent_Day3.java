package AdventCode;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Advent_Day3 {

    public Advent_Day3() {
    }

    public int Part1(ArrayList<String> gameRecords) {


        int possibleGameIDsSum = 0;

        for (String game : gameRecords) {
            String _gameID = "gameID";
            String _sets = "sets";

            int gameID = -1;
            String setsString = "";
            boolean gamePossible = true;

            Pattern pattern_GameID_Sets = Pattern.compile("^Game (?<" + _gameID + ">\\d+):(?<" + _sets + ">.*)");
            Matcher matcher_GameID_Sets = pattern_GameID_Sets.matcher(game);

            if (matcher_GameID_Sets.find()) {
                gameID = Integer.parseInt(matcher_GameID_Sets.group(_gameID));
                setsString = matcher_GameID_Sets.group(_sets);

                String[] setsArray = setsString.split(";");

                for (String set : setsArray) {
                    String _red = "red";
                    String _green = "green";
                    String _blue = "blue";

                    Pattern pattern_redCubes = Pattern.compile(".*?(?<" + _red + ">\\d+)\\s" + _red);
                    Pattern pattern_greenCubes = Pattern.compile(".*?(?<" + _green + ">\\d+)\\s" + _green);
                    Pattern pattern_blueCubes = Pattern.compile(".*?(?<" + _blue + ">\\d+)\\s" + _blue);

                    Matcher matcher_redCubes = pattern_redCubes.matcher(set);
                    Matcher matcher_greenCubes = pattern_greenCubes.matcher(set);
                    Matcher matcher_blueCubes = pattern_blueCubes.matcher(set);

                    int redCubes = matcher_redCubes.find() ? Integer.parseInt(matcher_redCubes.group(_red)) : 0;
                    int greenCubes = matcher_greenCubes.find() ? Integer.parseInt(matcher_greenCubes.group(_green)) : 0;
                    int blueCubes = matcher_blueCubes.find() ? Integer.parseInt(matcher_blueCubes.group(_blue)) : 0;

                }
            }
            if (gamePossible) possibleGameIDsSum += gameID;
        }

        return possibleGameIDsSum;
    }

    public int Part2(ArrayList<String> gameRecords) {
        int powerOfSetsSum = 0;

        for (String game : gameRecords) {
            String _gameID = "gameID";
            String _sets = "sets";
            int minRedNeeded = 0;
            int minGreenNeeded = 0;
            int minBlueNeeded = 0;

            int gameID = -1;
            String setsString = "";
            boolean gamePossible = true;

            Pattern pattern_GameID_Sets = Pattern.compile("^Game (?<" + _gameID + ">\\d+):(?<" + _sets + ">.*)");
            Matcher matcher_GameID_Sets = pattern_GameID_Sets.matcher(game);

            if (matcher_GameID_Sets.find()) {
                gameID = Integer.parseInt(matcher_GameID_Sets.group(_gameID));
                setsString = matcher_GameID_Sets.group(_sets);

                String[] setsArray = setsString.split(";");

                for (String set : setsArray) {
                    String _red = "red";
                    String _green = "green";
                    String _blue = "blue";

                    Pattern pattern_redCubes = Pattern.compile(".*?(?<" + _red + ">\\d+)\\s" + _red);
                    Pattern pattern_greenCubes = Pattern.compile(".*?(?<" + _green + ">\\d+)\\s" + _green);
                    Pattern pattern_blueCubes = Pattern.compile(".*?(?<" + _blue + ">\\d+)\\s" + _blue);

                    Matcher matcher_redCubes = pattern_redCubes.matcher(set);
                    Matcher matcher_greenCubes = pattern_greenCubes.matcher(set);
                    Matcher matcher_blueCubes = pattern_blueCubes.matcher(set);

                    int redCubes = matcher_redCubes.find() ? Integer.parseInt(matcher_redCubes.group(_red)) : 0;
                    int greenCubes = matcher_greenCubes.find() ? Integer.parseInt(matcher_greenCubes.group(_green)) : 0;
                    int blueCubes = matcher_blueCubes.find() ? Integer.parseInt(matcher_blueCubes.group(_blue)) : 0;

                    minRedNeeded = Math.max(redCubes, minRedNeeded);
                    minGreenNeeded = Math.max(greenCubes, minGreenNeeded);
                    minBlueNeeded = Math.max(blueCubes, minBlueNeeded);
                }
            }
            int powerOfSet = minRedNeeded * minGreenNeeded * minBlueNeeded;

            powerOfSetsSum += powerOfSet;
        }

        return powerOfSetsSum;
    }

}
