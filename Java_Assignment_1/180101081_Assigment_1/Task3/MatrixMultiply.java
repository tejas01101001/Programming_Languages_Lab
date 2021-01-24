/* 

Running the Code in a linux based system:

1. Make sure Java is installed on your machine.
2. Open the terminal and change the directory to the directory of the source code.
3. Type 'javac MatrixMultiply.java' in the terminal and press enter to compile your code. 
4. Now, type 'java MatrixMultiply <number_of_threads_as_argument> ' in the terminal and press enter to run the program.
5. If the number of threads is not between 4 and 16 the output console will print an error message. 

*/

import java.lang.*;

// Creating threads by creating objects of the class Matrix
class Matrix extends Thread {

    static final int SIZE = (int) 1E3;    // Number of rows / columns in a matrix
    static int numThreads;                // Number of threads used in the computation
    static int flag = 0;                  // A flag to check whether matrices are filled or not

    int low;       // Lower bound for the first available row for a given thread
    int high;      // Upper bound for the last available row for a given thread

    static double[][] matrixA = new double[SIZE][SIZE];      // Global matrix A is declared
    static double[][] matrixB = new double[SIZE][SIZE];      // Global matrix B is declared
    static double[][] matrixC = new double[SIZE][SIZE];      // Global matrix C is declared which will store AxB

    // Constructor of the class Matrix
    public Matrix(int low, int high) {
        this.low = low;
        this.high = Math.min(high, SIZE);
    }

    // The run() function is called automatically when a thread starts.
    public void run() {

        // If flag is not set then fillMatrix function is called
        // Else multiplyMatrices is called
        if (flag == 0) {
            fillMatrix(matrixA, low, high);
            fillMatrix(matrixB, low, high);
        } else {
            multiplyMatrices(matrixA, matrixB, low, high);
        }
    }

    // Fill the matrix with random values between 0.0 and 10.0
    public static void fillMatrix(double[][] emptyMatrix, int low, int high) {

        // Each thread is assigned some set of rows
        for (int i = low; i < high; i++) {
            for (int j = 0; j < SIZE; j++) {
                emptyMatrix[i][j] = Math.random() * 10.0;
            }
        }
        return;
    }

    // Used to multiply 2 matrices
    public static void multiplyMatrices(double[][] leftMatrix, double[][] rightMatrix, int low, int high) {

         // Each thread is assigned some set of rows
        for (int i = low; i < high; i++) {
            for (int j = 0; j < SIZE; j++) {
                matrixC[i][j] = 0;
                for (int k = 0; k < SIZE; k++) {
                    // Definition of Matirx Multiplication
                    matrixC[i][j] += leftMatrix[i][k] * rightMatrix[k][j];
                }
            }
        }
        return;
    }

    public static void fillAndMuliplty() {

        // Size of a block of rows which is to be assigned to a thread is calculated
        int size = (int) Math.ceil(SIZE * 1.0 / numThreads);

         // A new array of threads is created
        Matrix[] count = new Matrix[numThreads];

        for (int i = 0; i < numThreads; i++) 
        {
            // A new thread is assigned with appropriate lower and upper bounds of rows
            count[i] = new Matrix(i * size, (i + 1) * size);
            // A thread starts its execution
            count[i].start();
        }

        try 
        {
            // Make the main thread wait till all the child threads are done with their execution
            for (int i = 0; i < numThreads; i++) {
                count[i].join();
            }
        } 
        catch (InterruptedException e) {
            // It is possible that the all the child threads are already dead
        }

        System.out.println("The two matrices are filled with random values between 0.0 and 10.0");

        flag = 1;    // When both the matrices A and B are filled with random values set the flag
        for (int i = 0; i < numThreads; i++) {
            // A new thread is created to muliply the matrices
            count[i] = new Matrix(i * size, (i + 1) * size);
            // The execution of thread starts
            count[i].start();
        }

        try 
        {
            // Make the main thread wait till all the child threads are done with their execution
            for (int i = 0; i < numThreads; i++) {
                count[i].join();
            }
        } catch (InterruptedException e) 
        {
             // It is possible that the all the child threads are already dead
        }

        return;
    }

}

public class MatrixMultiply {

    public static void main(String args[]) {

        // The number of Threads to be used is passed as an argument in the command line.
        int numThreads = Integer.parseInt(args[0]);

        // If the number of threads are not between 4 and 16 an error message is print on the console.
        if (numThreads < 4 || numThreads > 16) {
            System.out.println("Number of threads should be between 4 and 16, please run your code again");
            return;
        }

        // Start the timer
        long start = System.currentTimeMillis();

        // Static variable of the class Matrix is initialised.
        Matrix.numThreads = numThreads;

        // Static function of the class Matrix is called to fill the matrices.
        Matrix.fillAndMuliplty();

        // Static function of the class Matrix is called to calculate product of the matrices.
        System.out.println("Execution Time to fill and multiply two matrices when using " + numThreads + " Threads : "+ (System.currentTimeMillis() - start) + "ms");

        return;
    }
}
