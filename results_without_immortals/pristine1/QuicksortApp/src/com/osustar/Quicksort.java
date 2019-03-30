package com.osustar;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import org.apache.log4j.*;
import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.PropertyConfigurator;

public class Quicksort {

    private List<Integer> list = new ArrayList<Integer>();

    public String combinedString = "";

    public static void main(String[] args) {
        Quicksort q = new Quicksort();
        q.numbers = new int[10];
        Random generator = new Random();
        for (int i = 0; i < q.numbers.length; i++) {
            q.numbers[i] = generator.nextInt(100);
        }
        q.sort(q.numbers);
    }

    private int[] numbers;

    private int number;

    Logger logger2 = org.apache.log4j.Logger.getLogger(Quicksort.class);

    public void sort(int[] values) {
        if (values == null || values.length == 0) {
            return;
        } else {
        }
        this.numbers = values;
        number = values.length;
        quicksort(0, number - 1);
    }

    public int dummy(int i) {
        List<String> list = new ArrayList<String>();
        list.add("arpit");
        int returnvalue = 0;
        for (String s : list) {
            combinedString += s;
        }
        switch(i) {
            case 1:
                returnvalue = 1;
                break;
            case 2:
                if (true) {
                    returnvalue++;
                }
                returnvalue = returnvalue + 2;
                break;
            case 3:
                break;
            case 10:
                break;
            default:
                break;
        }
        return returnvalue;
    }


    private int dummyMethod(int k) {
        return 5;
    }

    private void quicksort(int low, int high) {
        int i = low, j = high;
        int pivot = numbers[low + (high - low) / 2];
        while (i <= j) {
            while (numbers[i] < pivot) {
                i++;
            }
            while (numbers[j] > pivot) {
                j--;
            }
            if (i <= j) {
                exchange(i, j);
                i++;
                j--;
            }
        }
        if (low < j) quicksort(low, j);
        if (i < high) quicksort(i, high);
    }

    private void exchange(int i, int j) {
        int temp = numbers[i];
        numbers[i] = numbers[j];
        numbers[j] = temp;
    }
}
