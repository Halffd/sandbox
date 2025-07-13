package com.half.javalearning.sort;
import java.util.Arrays;

class MergeSorting {

    public static void main(String args[])
    {
        // merge sort = recursively divide array in 2, sort, re-combine
        // run-time complexity = O(n Log n)
        // space complexity    = O(n)

        int[] array = {8, 2, 5, 3, 4, 7, 6, 1};

        mergeSort(array);

        for(int i = 0; i < array.length; i++){
            System.out.print(array[i]+ " ");
        }
    }

    private static void mergeSort(int[] array) {

        int length = array.length;
        if (length <= 1) return; //base case

        int middle = length / 2;
        int[] leftArray = new int[middle];
        int[] rightArray = new int[length - middle];

        int i = 0; //left array
        int j = 0; //right array

        for(; i < length; i++) {
            if(i < middle) {
                leftArray[i] = array[i];
            }
            else {
                rightArray[j] = array[i];
                j++;
            }
        }
        mergeSort(leftArray);
        mergeSort(rightArray);
        merge(leftArray, rightArray, array);
    }

    private static void merge(int[] leftArray, int[] rightArray, int[] array) {

        int leftSize = array.length / 2;
        int rightSize = array.length - leftSize;
        int i = 0, l = 0, r = 0; //indices
        System.out.println("Left array: ");
        for(int ind = 0; ind < leftArray.length; ind++){
            System.out.print(leftArray[ind]+ " ");
        }
        System.out.println();
        System.out.println("Right array: ");
        for(int ind = 0; ind < rightArray.length; ind++){
            System.out.print(rightArray[ind]+ " ");
        }
        System.out.println();
        //check the conditions for merging
        while(l < leftSize && r < rightSize) {
            if(leftArray[l] < rightArray[r]) {
                array[i] = leftArray[l];
                i++;
                l++;
            }
            else {
                array[i] = rightArray[r];
                i++;
                r++;
            }
        }
        System.out.println("Merging array: ");
        for(int ind = 0; ind < array.length; ind++){
            System.out.print(array[ind]+ " ");
        }
        System.out.println();
        while(l < leftSize) {
            array[i] = leftArray[l];
            i++;
            l++;
        }
        while(r < rightSize) {
            array[i] = rightArray[r];
            i++;
            r++;
        }

        System.out.println("Merged array: ");
        for(int ind = 0; ind < array.length; ind++){
            System.out.print(array[ind]+ " ");
        }
        System.out.println();
    }
}

/**
 * Base MergeSort class that provides core merge sort functionality
 */
public class MergeSort {
    /**
     * Sort a one-dimensional array using merge sort
     * @param array The array to be sorted
     */
    public void sort(int[] array) {
        if (array == null || array.length <= 1) return;

        int[] temp = new int[array.length]; // Auxiliary array
        mergeSort(array, temp, 0, array.length - 1);
    }

    /**
     * Core merge sort recursive method
     */
    protected void mergeSort(int[] array, int[] temp, int left, int right) {
        if (left >= right) return;

        int mid = left + (right - left) / 2;

        // Sort left half
        mergeSort(array, temp, left, mid);
        // Sort right half
        mergeSort(array, temp, mid + 1, right);
        // Merge the sorted halves
        merge(array, temp, left, mid, right);
    }

    /**
     * Merge two sorted subarrays
     */
    protected void merge(int[] array, int[] temp, int left, int mid, int right) {
        // Copy both halves to the temp array
        for (int i = left; i <= right; i++) {
            temp[i] = array[i];
        }

        int i = left;     // Left subarray index
        int j = mid + 1;  // Right subarray index
        int k = left;     // Current position in original array

        // Copy the smallest values from either the left or right side back to the original array
        while (i <= mid && j <= right) {
            if (temp[i] <= temp[j]) { // Use <= for stability
                array[k++] = temp[i++];
            } else {
                array[k++] = temp[j++];
            }
        }

        // Copy the remaining elements from the left subarray (if any)
        while (i <= mid) {
            array[k++] = temp[i++];
        }

        // No need to copy remaining elements from right subarray
        // They're already in the right place
    }

    /**
     * Print an array (helper method)
     */
    protected void printArray(int[] array) {
        for (int element : array) {
            System.out.print(element + " ");
        }
        System.out.println();
    }

    /**
     * Print a matrix (helper method)
     */
    protected void printMatrix(int[][] matrix) {
        for (int[] row : matrix) {
            for (int element : row) {
                System.out.print(element + "\t");
            }
            System.out.println();
        }
        System.out.println();
    }
}

/**
 * MatrixSort extends MergeSort to sort an entire matrix by flattening,
 * sorting, and then restructuring it
 */
class MatrixSort extends MergeSort {

    /**
     * Sort an entire matrix (all elements)
     * @param matrix The matrix to be sorted
     */
    public void sort(int[][] matrix) {
        if (matrix == null || matrix.length == 0) return;

        int rows = matrix.length;
        int cols = matrix[0].length;

        // Flatten the matrix into a 1D array
        int[] flatArray = new int[rows * cols];
        int index = 0;

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                flatArray[index++] = matrix[i][j];
            }
        }

        // Sort the flattened array
        super.sort(flatArray);

        // Restructure the sorted array back into the matrix
        index = 0;
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                matrix[i][j] = flatArray[index++];
            }
        }
    }

    /**
     * Demo method to show the sorting process
     */
    public void demo(int[][] matrix) {
        System.out.println("===== Matrix Sort Demo =====");
        System.out.println("Original Matrix:");
        printMatrix(matrix);

        sort(matrix);

        System.out.println("Sorted Matrix (entire matrix sorted):");
        printMatrix(matrix);
    }
}

/**
 * ColumnSort extends MergeSort to sort each column of a matrix independently
 */
class ColumnSort extends MergeSort {

    /**
     * Sort each column of a matrix
     * @param matrix The matrix whose columns will be sorted
     */
    public void sort(int[][] matrix) {
        if (matrix == null || matrix.length == 0) return;

        int rows = matrix.length;
        int cols = matrix[0].length;

        // For each column
        for (int j = 0; j < cols; j++) {
            // Extract column
            int[] column = new int[rows];
            for (int i = 0; i < rows; i++) {
                column[i] = matrix[i][j];
            }

            // Sort column
            super.sort(column);

            // Put sorted column back
            for (int i = 0; i < rows; i++) {
                matrix[i][j] = column[i];
            }
        }
    }

    /**
     * Sort a specific column of a matrix
     * @param matrix The matrix
     * @param columnIndex Index of the column to sort
     */
    public void sortColumn(int[][] matrix, int columnIndex) {
        if (matrix == null || matrix.length == 0 || columnIndex < 0 || columnIndex >= matrix[0].length) {
            return;
        }

        int rows = matrix.length;

        // Extract column
        int[] column = new int[rows];
        for (int i = 0; i < rows; i++) {
            column[i] = matrix[i][columnIndex];
        }

        // Sort column
        super.sort(column);

        // Put sorted column back
        for (int i = 0; i < rows; i++) {
            matrix[i][columnIndex] = column[i];
        }
    }

    /**
     * Demo method to show the sorting process
     */
    public void demo(int[][] matrix) {
        System.out.println("===== Column Sort Demo =====");
        System.out.println("Original Matrix:");
        printMatrix(matrix);

        sort(matrix);

        System.out.println("Matrix with Sorted Columns:");
        printMatrix(matrix);
    }

    /**
     * Demo for sorting a specific column
     */
    public void demoSingleColumn(int[][] matrix, int columnIndex) {
        System.out.println("===== Single Column Sort Demo =====");
        System.out.println("Original Matrix:");
        printMatrix(matrix);

        System.out.println("Sorting column " + columnIndex + "...");
        sortColumn(matrix, columnIndex);

        System.out.println("Matrix with Column " + columnIndex + " Sorted:");
        printMatrix(matrix);
    }
}

/**
 * RowSort extends MergeSort to sort each row of a matrix independently
 */
class RowSort extends MergeSort {

    /**
     * Sort each row of a matrix
     * @param matrix The matrix whose rows will be sorted
     */
    public void sort(int[][] matrix) {
        if (matrix == null || matrix.length == 0) return;

        // For each row
        for (int i = 0; i < matrix.length; i++) {
            // Sort row
            super.sort(matrix[i]);
        }
    }

    /**
     * Sort a specific row of a matrix
     * @param matrix The matrix
     * @param rowIndex Index of the row to sort
     */
    public void sortRow(int[][] matrix, int rowIndex) {
        if (matrix == null || matrix.length == 0 || rowIndex < 0 || rowIndex >= matrix.length) {
            return;
        }

        // Sort the specified row
        super.sort(matrix[rowIndex]);
    }

    /**
     * Demo method to show the sorting process
     */
    public void demo(int[][] matrix) {
        System.out.println("===== Row Sort Demo =====");
        System.out.println("Original Matrix:");
        printMatrix(matrix);

        sort(matrix);

        System.out.println("Matrix with Sorted Rows:");
        printMatrix(matrix);
    }

    /**
     * Demo for sorting a specific row
     */
    public void demoSingleRow(int[][] matrix, int rowIndex) {
        System.out.println("===== Single Row Sort Demo =====");
        System.out.println("Original Matrix:");
        printMatrix(matrix);

        System.out.println("Sorting row " + rowIndex + "...");
        sortRow(matrix, rowIndex);

        System.out.println("Matrix with Row " + rowIndex + " Sorted:");
        printMatrix(matrix);
    }
}

/**
 * Main class to demonstrate all sorting methods
 */
class MatrixSortDemo {
    public static void main(String[] args) {
        // Create a sample matrix
        int[][] matrix = {
                {9, 3, 7, 1},
                {5, 8, 2, 6},
                {4, 0, 10, 3},
                {12, 5, 7, 11}
        };

        // Make copies for each demonstration
        int[][] matrixCopy1 = deepCopy(matrix);
        int[][] matrixCopy2 = deepCopy(matrix);
        int[][] matrixCopy3 = deepCopy(matrix);
        int[][] matrixCopy4 = deepCopy(matrix);
        int[][] matrixCopy5 = deepCopy(matrix);

        // Demonstrate MatrixSort
        MatrixSort matrixSort = new MatrixSort();
        matrixSort.demo(matrixCopy1);

        // Demonstrate ColumnSort (all columns)
        ColumnSort columnSort = new ColumnSort();
        columnSort.demo(matrixCopy2);

        // Demonstrate ColumnSort (single column)
        columnSort.demoSingleColumn(matrixCopy3, 1);

        // Demonstrate RowSort (all rows)
        RowSort rowSort = new RowSort();
        rowSort.demo(matrixCopy4);

        // Demonstrate RowSort (single row)
        rowSort.demoSingleRow(matrixCopy5, 2);
    }

    /**
     * Create a deep copy of a matrix
     */
    private static int[][] deepCopy(int[][] original) {
        if (original == null) return null;

        int[][] copy = new int[original.length][];
        for (int i = 0; i < original.length; i++) {
            copy[i] = Arrays.copyOf(original[i], original[i].length);
        }

        return copy;
    }
}
class MergeTest {
    public static void main(String args[]) {
        // merge sort = recursively divide array in 2, sort, re-combine
        // run-time complexity = O(n Log n)
        // space complexity    = O(n)

        int[] array = {8, 2, 5, 3, 4, 7, 6, 1};
        MergeSort mergeSort = new MergeSort();
        for (int i = 0; i < array.length; i++) {
            System.out.print(array[i] + " ");
        }
        System.out.println();
        System.out.println("Merge Sort:");
        mergeSort.sort(array);

        for (int i = 0; i < array.length; i++) {
            System.out.print(array[i] + " ");
        }
        System.out.println();
        System.out.println("Matrix:");
        MatrixSortDemo.main(null);
        System.out.println("Column:");
        ColumnSort columnSort = new ColumnSort();
        columnSort.demoSingleColumn(new int[][]{{7, 6, 5, 4, 3, 2, 1, 8}, {9, 10, 11, 12, 13, 14, 15, 16},{2, 3, 4, 5, 6, 7, 8, 9},{1, 2, 3, 4, 5, 6, 7, 8}}, 0);
        System.out.println("Row:");
        RowSort rowSort = new RowSort();
        rowSort.demoSingleRow(new int[][]{{7, 6, 5, 4, 3, 2, 1, 8}, {9, 10, 11, 12, 13, 14, 15, 16},{2, 3, 4, 5, 6, 7, 8, 9},{1, 2, 3, 4, 5, 6, 7, 8}}, 0);
        System.out.println("Row 2:");
        rowSort.demoSingleRow(new int[][]{{7, 6, 5, 4, 3, 2, 1, 8}, {9, 10, 11, 12, 13, 14, 15, 16},{2, 3, 4, 5, 6, 7, 8, 9},{1, 2, 3, 4, 5, 6, 7, 8}}, 1);
    }
}