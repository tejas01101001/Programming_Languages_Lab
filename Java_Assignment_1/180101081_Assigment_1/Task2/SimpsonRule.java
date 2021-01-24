/* 

Running the Code in a linux based system:

1. Make sure Java is installed on your machine.
2. Open the terminal and change the directory to the directory of the source code.
3. Type 'javac SimpsonRule.java' in the terminal and press enter to compile your code. 
4. Now, type 'java SimpsonRule <number_of_threads_as_argument> ' in the terminal and press enter to run the program.
5. If the number of threads is not between 4 and 16 the output console will print an error message. 

*/

import java.lang.*;

// Creating threads by creating objects of the class SimpsonIntegral
class SimpsonIntegral extends Thread {


    static final int NUM_INTERVALS = (int) 2E6;                     // A constant number of intervals are initialised
    static final double DELTA = (2.0) / (double) NUM_INTERVALS;     // DELTA is (b-a)/NUM_INTERVALS
    static int numThreads;                                          // Number of threads used in the computation

    int low;              // Lower bound for the first available point for a given thread
    int high;             // Upper bound for the last available point for a given thread
    double partialsum;    // Partial answer calculated by a given thread

    // Constructor of the class SimpsonIntegral
    public SimpsonIntegral(int low, int high) {
        this.low = low;
        this.high = Math.min(high, NUM_INTERVALS + 1);
    }

    public static double func(double x) {

        // The numerator is e^(-x^2/2)
        double numerator = Math.exp(-(Math.pow(x, 2) / 2));
        // The denominator is root(2*pi)
        double denominator = Math.pow(2 * Math.PI, 0.5);

        // Value of f(x) returned where f(x) is the function given in the question
        return numerator / denominator;
    }

     // The run() function is called automatically when a thread starts
    public void run() {
        partialsum = evaluate(low, high);
    }

    // Evaluate the Integral at data points numbered between low and high-1
    public static double evaluate(int low, int high) {

        // Stores the value of integral obtained so far
        double sum = 0;
       
        for (int i = low; i < high; i++) {

            // The multipliers in Simpson's (1/3) Rule are in order 1 4 2 4 2 4....1 (n+1) points where n is number of intervals
            double multiplier;
            if (i == 0 || i == NUM_INTERVALS)
                multiplier = 1.0;
            else if (i % 2 == 1)
                multiplier = 4.0;
            else
                multiplier = 2.0;

            // Current value of data point
            double x = -1 + i * DELTA;
            // Contribution of data point added to the sum
            sum += multiplier * func(x);
        }

        return sum;
    }

    public static double calculateIntegral() {

         // Size of a block which is to be assigned to a thread is calculated
        int size = (int) Math.ceil((NUM_INTERVALS + 1) * 1.0 / numThreads);

        // A new array of threads is created
        SimpsonIntegral[] intervals = new SimpsonIntegral[numThreads];

        for (int i = 0; i < numThreads; i++) 
        {
             // A new thread is assigned with appropriate lower and upper bounds
            intervals[i] = new SimpsonIntegral(i * size, (i + 1) * size);
            // A thread starts its execution
            intervals[i].start();
        }

        try 
        {
            // Make the main thread wait till all the child threads are done with their execution
            for (int i = 0; i < numThreads; i++) {
                intervals[i].join();
            }
        } catch (InterruptedException e) 
        {
             // It is possible that the all the child threads are already dead
        }

        // Add all the partial values of integrals caluclated by threads
        double area = 0;
        for (int i = 0; i < numThreads; i++) {
            area += intervals[i].partialsum;
        }

        // Use simpson(1/3)Rule  
        return (DELTA * area / (3.0));
    }

}

public class SimpsonRule {
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

        // Static variable of the class SimpsonIntegral is initialised.
        SimpsonIntegral.numThreads = numThreads;

        // Static function of the class SimpsonIntegral is called.
        System.out.println("Value of Integral estimated using Simpson (1/3) Rule : " + SimpsonIntegral.calculateIntegral());

        // Time required for execution is calculated
        System.out.println("Execution Time when using " + numThreads + " Threads : "+ (System.currentTimeMillis() - start) + "ms");

        return;
    }
}
