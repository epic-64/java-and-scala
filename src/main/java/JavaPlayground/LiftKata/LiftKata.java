package JavaPlayground.LiftKata;

import java.util.*;
import java.util.stream.Collectors;

enum Direction {UP, DOWN}

record Person(int position, int destination) {
    Person {
        if (position == destination) {
            throw new IllegalArgumentException("Source and destination floor cannot be the same");
        }
    }

    public Direction desiredDirection() {
        return destination > position ? Direction.UP : Direction.DOWN;
    }

    public boolean isLowerThan(Lift lift) {
        return position < lift.position;
    }

    public boolean isHigherThan(Lift lift) {
        return position > lift.position;
    }

    @Override
    public String toString() {
        return "Person(" + position + " -> " + destination + ")";
    }
}

class Lift {
    int position;
    Queue<Person> people;
    Direction direction;
    final int capacity;

    public Lift(int position, Direction direction, int capacity) {
        this.position = position;
        this.people = new ArrayDeque<>();
        this.direction = direction;
        this.capacity = capacity;
    }

    private boolean isFull() {
        return people.size() == capacity;
    }

    public boolean hasRoom() {
        return !isFull();
    }

    public boolean hasPeople() {
        return !people.isEmpty();
    }

    public boolean isEmpty() {
        return people.isEmpty();
    }

    public boolean accepts(Person person) {
        return hasRoom() && person.desiredDirection() == direction;
    }

    public void turn() {
        direction = (direction == Direction.UP) ? Direction.DOWN : Direction.UP;
    }

    @Override
    public String toString() {
        return "Lift(Position: " + position + ", Direction: " + direction + ", People: " + people + ")";
    }
}

record Building(Map<Integer, Queue<Person>> floors) {
    public boolean isEmpty() {
        return floors.values().stream().allMatch(Queue::isEmpty);
    }
}

class State {
    final Building building;
    final Lift lift;
    final List<Integer> stops;

    public State(Building building, Lift lift) {
        this.building = building;
        this.lift = lift;
        this.stops = new ArrayList<>();
    }

    public void printState() {
        System.out.println("Building:");
        building.floors().forEach((floor, queue) -> System.out.println("Floor " + floor + ": " + queue));

        System.out.println("Lift: " + lift);
        System.out.println("Stops: " + stops);
        System.out.println();
    }
}

class ElevatorLogic {
    private static void tick(State state) {
        Lift lift = state.lift;
        Building building = state.building;
        List<Integer> stops = state.stops;

        // Off-board people
        lift.people.removeIf(p -> p.destination() == lift.position);

        // Get current floor queue
        Queue<Person> queue = building.floors().getOrDefault(lift.position, new ArrayDeque<>());

        while (lift.hasRoom() && queue.stream().anyMatch(lift::accepts)) {
            Person person = queue.stream().filter(lift::accepts).findFirst().orElse(null);
            if (person != null) {
                queue.remove(person);
                lift.people.add(person);
            }
        }

        int oldPosition = lift.position;
        int nextPosition = getNextPosition(building, lift);
        lift.position = nextPosition;

        if (oldPosition != nextPosition) {
            stops.add(nextPosition);
        }

    }

    private static List<Person> peopleGoingDown(Building building) {
        return building.floors().values().stream()
                .flatMap(Collection::stream)
                .filter(p -> p.desiredDirection() == Direction.DOWN)
                .toList();
    }

    private static List<Person> peopleGoingUp(Building building) {
        return building.floors().values().stream()
                .flatMap(Collection::stream)
                .filter(p -> p.desiredDirection() == Direction.UP)
                .toList();
    }

    private static int emptyLiftNextPosition(Building building, Lift lift) {
        if (lift.direction == Direction.UP) {
            return peopleGoingDown(building).stream()
                    .filter(p -> p.position() > lift.position)
                    .mapToInt(Person::position)
                    .max()
                    .orElse(peopleGoingUp(building).stream()
                            .filter(p -> p.isHigherThan(lift))
                            .mapToInt(Person::position)
                            .min()
                            .orElse(0));
        } else {
            return peopleGoingUp(building).stream()
                    .filter(p -> p.isLowerThan(lift))
                    .mapToInt(Person::position)
                    .min()
                    .orElse(peopleGoingDown(building).stream()
                            .filter(p -> p.isHigherThan(lift))
                            .mapToInt(Person::position)
                            .max()
                            .orElse(0));
        }
    }

    private static int getNextPosition(Building building, Lift lift) {
        if (building.isEmpty() && lift.isEmpty()) {
            return 0;
        }

        OptionalInt nearestRequestedPassenger = lift.people.stream()
                .filter(p -> p.desiredDirection() == lift.direction)
                .mapToInt(Person::destination)
                .min();

        OptionalInt nearestRequestInSameDirection = (lift.direction == Direction.UP) ?
                peopleGoingUp(building).stream()
                        .filter(p -> p.isHigherThan(lift))
                        .mapToInt(Person::position)
                        .min() :
                peopleGoingDown(building).stream()
                        .filter(p -> p.isLowerThan(lift))
                        .mapToInt(Person::position)
                        .max();

        List<Integer> combinedOptions = new ArrayList<>();
        nearestRequestedPassenger.ifPresent(combinedOptions::add);
        nearestRequestInSameDirection.ifPresent(combinedOptions::add);

        return combinedOptions.stream().min(Comparator.comparingInt(f -> Math.abs(f - lift.position)))
                .orElseGet(() -> {
                    lift.turn();
                    return emptyLiftNextPosition(building, lift);
                });
    }

    public static State simulate(State state) {
        state.stops.add(state.lift.position);
        state.printState();

        while (!state.building.isEmpty() || state.lift.hasPeople() || state.lift.position != 0) {
            tick(state);
            state.printState();
        }
        return state;
    }
}

public class LiftKata {
    public static int[] theLift(int[][] queues, int capacity) {
        Map<Integer, Queue<Person>> floors = new LinkedHashMap<>();
        for (int i = 0; i < queues.length; i++) {
            int finalI = i;
            Queue<Person> people = Arrays.stream(queues[i])
                    .mapToObj(dest -> new Person(finalI, dest))
                    .collect(Collectors.toCollection(ArrayDeque::new));
            floors.put(i, people);
        }

        Lift lift = new Lift(0, Direction.UP, capacity);
        Building building = new Building(floors);
        State initialState = new State(building, lift);

        State finalState = ElevatorLogic.simulate(initialState);
        return finalState.stops.stream().mapToInt(Integer::intValue).toArray();
    }

    public static void main(String[] args) {
        int[][] queues = {
                {1, 2, 3},
                {},
                {},
                {2, 0, 0},
        };

        int capacity = 5;
        int[] stops = theLift(queues, capacity);
        System.out.println(Arrays.toString(stops));
    }
}
