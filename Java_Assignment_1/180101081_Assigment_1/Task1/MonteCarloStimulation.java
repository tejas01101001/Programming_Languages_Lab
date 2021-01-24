/* 

Running the Code in a linux based system:

1. Make sure Java is installed on your machine.
2. Open the terminal and change the directory to the directory of the source code.
3. Type 'javac MonteCarloStimulation.java' in the terminal and press enter to compile your code. 
4. Now, type 'java MonteCarloStimulation <number_of_threads_as_argument> ' in the terminal and press enter to run the program.
5. If the number of threads is not between 4 and 16 the output console will print an error message. 

*/

import java.lang.*;

// Creating threads by creating objects of the class MonteCarloPi
class MonteCarloPi extends Thread {

    static final int NUM_POINTS = (int) 1E6;   // A constant number of points are initialised
    static int numThreads;                     // Number of threads used in the computation

    int low;            // Lower bound for the first available point for a given thread
    int high;           // Upper bound for the last available point for a given thread
    int partialCount;   // Partial answer calculated by a given thread

    // Constructor of the class MonteCarloPi 
    public MonteCarloPi(int low, int high) {
        this.low = low;
        this.high = Math.min(high, NUM_POINTS);
    }

    // The run() function is called automatically when a thread starts.
    public void run() {
        partialCount = countInside(low, high);
    }

    // Count the number of point inside the circle (x^2+y^2=1) lying in the first quadrant
    public static int countInside(int low, int high) {

        // Counter for the number of points inside the circle
        int inside = 0;   
        for (int i = low; i < high; i++) {

            // Random points are generated
            double x = Math.random();
            double y = Math.random();

            // Checking whether the points are inside the circle
            if (x * x + y * y <= 1.0)
                inside++;
        }

        return inside;
    }

    public static int insideCircle() {

        // Size of a block which is to be assigned to a thread is calculated
        int size = (int) Math.ceil(NUM_POINTS * 1.0 / numThreads);

        // A new array of threads is created
        MonteCarloPi[] count = new MonteCarloPi[numThreads];

        for (int i = 0; i < numThreads; i++) 
        {
            // A new thread is assigned with appropriate lower and upper bounds
            count[i] = new MonteCarloPi(i * size, (i + 1) * size);
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

        // Adding the counts calculated by the threads.
        int inside = 0;
        for (int i = 0; i < numThreads; i++) {
            inside += count[i].partialCount;
        }

        return inside;
    }

    public static double estimatePI() {
        // Area of circle in first quadrant is pi/4 and area of square where random point are generated is 1
        // Hence pi = 4 *(Points inside circle)/ (Total number of points)
        return (4.0 * (double) (MonteCarloPi.insideCircle()) / (double) NUM_POINTS);
    }

}

public class MonteCarloStimulation {

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

        // Static variable of the class MonteCarloPi is initialised.
        MonteCarloPi.numThreads = numThreads;

        // Static function of the class MonteCarloPi is called.
        System.out.println("Value of PI estimated using Monte-Carlo Stimulation : " + MonteCarloPi.estimatePI());

        // Time required for execution is calculated
        System.out.println("Execution Time when using " + numThreads + " Threads : " + (System.currentTimeMillis() - start) + "ms");

        return;
    }
}
