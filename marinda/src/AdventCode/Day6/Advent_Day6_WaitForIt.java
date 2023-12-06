package AdventCode.Day6;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Advent_Day6_WaitForIt {

    public int Part1(List<String> raceSheet) {
        int waysToBeatRecordNumber = 0;

        List<Race> races = readInRaceSheet(raceSheet);

        List<Integer> numberPerRace = getWaysToBeatRecordNumberPerRace(races, waysToBeatRecordNumber);

        waysToBeatRecordNumber = numberPerRace.stream().reduce(1, (a, b) -> a * b);

        return waysToBeatRecordNumber;

    }

    private static List<Integer> getWaysToBeatRecordNumberPerRace(List<Race> races, int waysToBeatRecordNumber) {
        List<Integer> numberPerRace = new ArrayList<>();

        for (Race race : races) {
            int possibleWinNumber = 0;
            int raceTime = race.time;
            int recordDistance = race.distance;

            for (int holdTime = 1; holdTime < race.time; holdTime++) {
                int speed = holdTime;
                int remainingTime = raceTime - holdTime;
                int distance = speed * remainingTime;
                if (distance > recordDistance) possibleWinNumber++;
            }

            numberPerRace.add(possibleWinNumber);

        }
        return numberPerRace;
    }

    private List<Race> readInRaceSheet(List<String> raceSheet) {
        Pattern timePattern = Pattern.compile("(\\D*(?<time>\\d+))");
        Matcher timeMatcher = timePattern.matcher(raceSheet.get(0));

        Pattern distancePattern = Pattern.compile("(\\D*(?<distance>\\d+))");
        Matcher distanceMatcher = distancePattern.matcher(raceSheet.get(1));

        List<Race> races = new ArrayList<>();

        List<Integer> times = new ArrayList<>();
        List<Integer> distances = new ArrayList<>();

        while (timeMatcher.find()) {
            times.add(Integer.valueOf(timeMatcher.group("time")));
        }

        while (distanceMatcher.find()) {
            distances.add(Integer.valueOf(distanceMatcher.group("distance")));
        }

        for (int i = 0; i < times.size(); i++) {

            Race race = new Race();

            race.time = times.get(i);
            race.distance = distances.get(i);

            races.add(race);
        }

        return races;
    }

    public int Part2(ArrayList<String> almanacData) {
        int closestLocatoion = -1;
        return closestLocatoion;

    }


}
