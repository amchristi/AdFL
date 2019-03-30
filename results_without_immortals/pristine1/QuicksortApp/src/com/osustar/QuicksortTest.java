package com.osustar;

import java.util.Arrays;
import java.util.Random;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class QuicksortTest {

    int SIZE = 10;

    int MAX = 50;

    int[] numbers = new int[SIZE];

    @Before
    public void setUp() throws Exception {
        Random generator = new Random();
        for (int i = 0; i < numbers.length; i++) {
            numbers[i] = generator.nextInt(MAX);
        }
    }

    @Test
    public void testNull() {
        Quicksort sorter = new Quicksort();
        sorter.sort(null);
    }

    @Test
    public void testEmpty() {
        Quicksort sorter = new Quicksort();
        sorter.sort(new int[0]);
    }

    @Test
    public void testSimpleElement() {
        Quicksort sorter = new Quicksort();
        int[] test = new int[1];
        test[0] = 5;
        sorter.sort(test);
    }

    @Test
    public void testSpecial() {
        Quicksort sorter = new Quicksort();
        int[] test = { 5, 5, 6, 6, 4, 4, 5, 5, 4, 4, 6, 6, 5, 5 };
        sorter.sort(test);
        if (!validate(test)) {
            fail("Should not happen");
        }
        printResult(test);
    }

    @Test
    public void testQuickSort() {
        for (Integer i : numbers) {
            System.out.println(i + " ");
        }
        long startTime = System.currentTimeMillis();
        Quicksort sorter = new Quicksort();
        sorter.sort(numbers);
        long stopTime = System.currentTimeMillis();
        long elapsedTime = stopTime - startTime;
        System.out.println("Quicksort " + elapsedTime);
        if (!validate(numbers)) {
            fail("Should not happen");
        }
        assertTrue(true);
    }

    @Test
    public void testStandardSort() {
        long startTime = System.currentTimeMillis();
        Arrays.sort(numbers);
        long stopTime = System.currentTimeMillis();
        long elapsedTime = stopTime - startTime;
        System.out.println("Standard Java sort " + elapsedTime);
        if (!validate(numbers)) {
            fail("Should not happen");
        }
        assertTrue(true);
    }

    @Test
    public void testDummy1() {
        Quicksort sorter = new Quicksort();
        assertTrue(sorter.dummy(1) == 1);
    }

    @Test
    @MethodLabel(level = 1)
    public void testDummy2() {
        Quicksort sorter = new Quicksort();
        assertTrue(sorter.dummy(2) == 3);
    }

    public void testDummy10() {
        Quicksort sorter = new Quicksort();
        assertTrue(sorter.dummy(10) == 10);
    }

    @MethodLabel(level = 1)
    @Test
    public void testCombinedString() {
        Quicksort sorter = new Quicksort();
        int j = sorter.dummy(5);
        assertTrue(sorter.combinedString.equalsIgnoreCase("arpit"));
    }

    private boolean validate(int[] numbers) {
        for (int i = 0; i < numbers.length - 1; i++) {
            if (numbers[i] > numbers[i + 1]) {
                return false;
            }
        }
        return true;
    }

    private void printResult(int[] numbers) {
        for (int i = 0; i < numbers.length; i++) {
            System.out.print(numbers[i]);
        }
        System.out.println();
    }
}
