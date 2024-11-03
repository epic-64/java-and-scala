package JavaPlayground.RomanNumerals;

import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

public class JavaRomans {
    private static final List<Map.Entry<Integer, String>> romanNumerals = List.of(
        Map.entry(1000, "M"),
        Map.entry(900, "CM"),
        Map.entry(500, "D"),
        Map.entry(400, "CD"),
        Map.entry(100, "C"),
        Map.entry(90, "XC"),
        Map.entry(50, "L"),
        Map.entry(40, "XL"),
        Map.entry(10, "X"),
        Map.entry(9, "IX"),
        Map.entry(5, "V"),
        Map.entry(4, "IV"),
        Map.entry(1, "I")
    );

    public String toNumeral(int number) {
        StringBuilder sb = new StringBuilder();
        AtomicInteger remaining = new AtomicInteger(number);

        for (Map.Entry<Integer, String> entry : romanNumerals) {
            while (remaining.get() >= entry.getKey()) {
                remaining.addAndGet(-entry.getKey());
                sb.append(entry.getValue());
            }
        }

        return sb.toString();
    }
    
    public int toInt(String numeral) {
        AtomicInteger result = new AtomicInteger(0);
        AtomicInteger index = new AtomicInteger(0);

        romanNumerals.forEach(entry -> {
            while (numeral.startsWith(entry.getValue(), index.get())) {
                result.addAndGet(entry.getKey());
                index.addAndGet(entry.getValue().length());
            }
        });

        return result.get();
    }
}
