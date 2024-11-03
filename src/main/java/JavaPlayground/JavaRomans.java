package JavaPlayground;

import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

public class JavaRomans {
    private static final List<Map.Entry<Integer, String>> values = List.of(
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

    public String toNumeral(Integer number) {
        StringBuilder sb = new StringBuilder();
        AtomicInteger remaining = new AtomicInteger(number);

        values.forEach(entry -> {
            while (remaining.get() >= entry.getKey()) {
                remaining.addAndGet(-entry.getKey());
                sb.append(entry.getValue());
            }
        });

        return sb.toString();
    }
}
