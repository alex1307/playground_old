package com.ayagasha.playground.devops;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class Amazon {


    public static List<List<Integer>> nearestXsteakHouses(int totalSteakhouses,
                                            List<List<Integer>> allLocations,
                                            int numSteakhouses)
    {
        List<Integer> distances =
                allLocations
                    .stream()
                        .mapToInt((l) -> l.stream()
                                .mapToInt(x -> x*x)
                                .sum())
                        .boxed()
                        .sorted()
                        .collect(Collectors.toList());

        List<Integer> numSteakHousesDistances = distances.subList(0, numSteakhouses);
        Optional<Integer> optional = numSteakHousesDistances.stream().max(Integer::compareTo);
        Integer maxNearestDistance = optional.get();


        return allLocations.stream()
                .filter(e -> filterByMaxDistance(e, maxNearestDistance))
                .sorted((x, y) -> calculateDistance(x).compareTo(calculateDistance(y)))
                .collect(Collectors.toList()).subList(0, numSteakhouses);

        // WRITE YOUR CODE HERE
    }

    private static boolean filterByMaxDistance(List<Integer> entry, Integer maxDistance) {
        return entry.stream().mapToInt(e -> e * e).sum() <= maxDistance;
    }

    private static Integer calculateDistance(List<Integer> location) {
        return location.stream().mapToInt(x -> x * x).sum();
    }

    public static void main(String argv []){
        List<List<Integer>> steakHouses =Arrays.asList(
                Arrays.asList(1, 1),
                Arrays.asList(2, 2),
                Arrays.asList(3, 4),
                Arrays.asList(2,1),
                Arrays.asList(1,-3),
                Arrays.asList(-3,1)

        );
        List<List<Integer>> output = nearestXsteakHouses(steakHouses.size(), steakHouses, 2);
        System.out.println(output);
    }

    class Cell {
        int x;
        int y;
        int value;

        public int getX() {
            return x;
        }

        public void setX(int x) {
            this.x = x;
        }

        public int getY() {
            return y;
        }

        public void setY(int y) {
            this.y = y;
        }

        public int getValue() {
            return value;
        }

        public void setValue(int value) {
            this.value = value;
        }
    }

}
