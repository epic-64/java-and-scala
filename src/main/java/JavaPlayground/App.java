package JavaPlayground;

import JavaPlayground.RomanNumerals.JavaRomans;
import ScalaPlayground.RomanNumerals.ScalaRomansPipeline;

class App {
    public static void main(String[] args) {
        JavaRomans javaRomans = new JavaRomans();
        System.out.println(javaRomans.toNumeral(999));

        ScalaRomansPipeline scalaRomansPipeline = new ScalaRomansPipeline();
        System.out.println(scalaRomansPipeline.toNumeral(999));
        
        System.out.println(ScalaCalculator.add(1, 2));
    }
}

