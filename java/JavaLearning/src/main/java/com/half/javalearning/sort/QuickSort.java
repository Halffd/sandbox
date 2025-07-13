package com.half.javalearning.sort;

public class QuickSort {

    public static void main(String args[])
    {
        // quick sort = moves smaller elements to left of a pivot.
        //			   recursively divide array in 2 partitions

        //                       run-time complexity = Best case O(n log(n))
        //				   		                   Average case O(n log(n))
        //				   		                   Worst case O(n^2) if already sorted

        //                       space complexity    = O(log(n)) due to recursion

        int[] array = {8, 2, 5, 3, 9, 4, 7, 6, 1};

        quickSort(array, 0, array.length - 1);

        for(int i : array){
            System.out.print(i + " ");
        }
    }

    private static void quickSort(int[] array, int start, int end) {

        if(end <= start) {
            System.out.println("Base case " + end + " <= " + start);
            return; //base case
        }

        int pivot = partition(array, start, end);
        System.out.println("Pivot after partition: " + pivot);
        quickSort(array, start, pivot - 1);
        quickSort(array, pivot + 1, end);
    }
    private static int partition(int[] array, int start, int end) {

        int pivot = array[end];
        int i = start - 1;
        System.out.println("Pivot: " + pivot);
        System.out.println("i: " + i);
        for(int j = start; j <= end; j++) {
            if(array[j] < pivot) {
                i++;
                int temp = array[i];
                System.out.println("Swap temp: " + temp);
                System.out.println("array[j]: " + array[j]);
                array[i] = array[j];
                array[j] = temp;
            }
        }
        for(int n = 0; n < array.length; n++){
            System.out.print(array[n]+ " ");
        }
        System.out.println();
        i++;
        int temp = array[i];
        System.out.println("array[i] = " + array[i]);
        System.out.println("array[end] = " + array[end]);
        array[i] = array[end];
        array[end] = temp;
        for(int n = 0; n < array.length; n++){
            System.out.print(array[n]+ " ");
        }
        return i;
    }
}