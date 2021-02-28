/*

CS-331 Assignment 2 (Java)
Tejas Khairnar 
Roll Number : 180101081

Running the Code in a linux based system:

1. Make sure Java is installed on your machine.
2. Open the terminal and change the directory to the directory of the source ( .java file) code.
3. Type ```javac <Program_Name>.java``` in the terminal and press enter to compile your code. 
4. Now, type ```java <Program_Name> <number_of_updaters_per_transaction>``` in the terminal and press enter to run the program.
5. Once the program starts running a file "Data.txt" is created wherein all transactions are present.
6. Ignore any warnings if present.

NOTE :  All the instructions are also given in the source code file.

*/

import java.io.*;
import java.math.BigInteger;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;


class Account {

    private long balance = 0; // Balance of the account
    String accountNumber;// Account number

    // Constructor for the class Account
    public Account(long amount, String accountNum) {
        this.balance = amount;
        this.accountNumber = accountNum;
    }

    // Deposit amount into the account
    public void deposit(long amount) {
        this.balance += amount;
    }

    // Withdraw amount from account
    public void withdraw(long amount) {
        this.balance -= amount;
    }

    // To get the account number of the account
    public String getAccountNumber() {
        return this.accountNumber;
    }

    // Get the balance of the account
    public long getBalance() {
        return this.balance;
    }
}

class Bank {

    // File writer
    public static PrintWriter fd;
    public static int[] userPerBranch = new int[10];
   
    @SuppressWarnings("unchecked") public static LinkedList<Account> branch[] = new LinkedList[10];

    // Generate account number for a given branch
    public static String generateAccountNumber(long branchNumber) {
        long user = userPerBranch[(int) branchNumber];
        userPerBranch[(int) branchNumber]++;

        StringBuilder acc = new StringBuilder("");
        acc.append((char) (branchNumber + '0'));

        for (int i = 0; i < 9; i++)
            acc.append('0');
        String accNum = acc.toString();

        long temp = Long.parseLong(accNum);
        temp += user;

        String tail = Long.toString(temp);
        acc = new StringBuilder("");

        for (int i = 0; i < 10 - (int) tail.length(); i++) {
            acc.append('0');
        }
        acc.append(tail);

        accNum = acc.toString();
        return accNum;
    }

}

class Modify extends Thread{
    private int branch;
    int numberTransactions;

    public Modify(int branch, int transactions) {
        this.branch = branch;
        this.numberTransactions = transactions;
    }

    // execute when a thread starts running
    @SuppressWarnings({"all"}) public void run() throws NullPointerException,IndexOutOfBoundsException{

        for (int i = 0; i < numberTransactions; i++) {

            int randomNumber = ThreadLocalRandom.current().nextInt(1, 1001);

            // Variables initialized for their corresponding operations.
            boolean deposit = false, withdraw = false, transferAmount = false, newAccount = false, delAccount = false, transferAccount = false;

            // Making the probability calculations
            if (randomNumber <= 330)
                deposit = true;
            else if (randomNumber > 330 && randomNumber <= 660)
                withdraw = true;
            else if (randomNumber > 660 && randomNumber <= 990)
                transferAmount = true;
            else if (randomNumber > 990 && randomNumber <= 993)
                newAccount = true;
            else if (randomNumber > 993 && randomNumber <= 996)
                delAccount = true;
            else
                transferAccount = true;

            if (deposit) {

                int size = Bank.branch[branch].size()/2;
                int updaterId = ThreadLocalRandom.current().nextInt(0, size);
                int randomAmount = ThreadLocalRandom.current().nextInt(0, 100001);

                synchronized (Bank.branch[branch].get(updaterId)) {

                    String accNumber = Bank.branch[branch].get(updaterId).getAccountNumber();
                    Bank.branch[branch].get(updaterId).deposit(randomAmount);
                    try {
                        Bank.fd.println("Rs " + randomAmount + " is deposited in Account Number " + accNumber);
                    } catch (Exception e) {
                    }
                }
            }

            if (withdraw) {

                int size = Bank.branch[branch].size()/2;
                int updaterId = ThreadLocalRandom.current().nextInt(0, size);
                int randomAmount = ThreadLocalRandom.current().nextInt(0, 100001);

                if (Bank.branch[branch].get(updaterId).getBalance() < randomAmount) {
                    String accNumber = Bank.branch[branch].get(updaterId).getAccountNumber();
                    try {
                        Bank.fd.println("Requested amount exceeds the available balance of Account " + accNumber);
                    } catch (Exception e) {
                    }
                } else {
                    try
                    {
                    synchronized (Bank.branch[branch].get(updaterId)) {

                        // System.out.print(updaterId + " "+ size + "\n");
                        String accNumber = Bank.branch[branch].get(updaterId).getAccountNumber();
                        Bank.branch[branch].get(updaterId).withdraw(randomAmount);
                        try {
                            Bank.fd.println("Rs " + randomAmount + " is withdrawn from Account Number " + accNumber);
                        } catch (Exception e) {
                        }
                    }
                    }catch(Exception e){
                    }
                }
            }

            if (newAccount) {
                int balance = ThreadLocalRandom.current().nextInt(10000);
                String accountNum = Bank.generateAccountNumber(branch);
                Bank.branch[branch].add(new Account(balance, accountNum));
                try {
                    Bank.fd.println("A new account " + accountNum + " added to branch number" + branch);
                } catch (Exception e) {
                }
            }

            if (delAccount) {
                int randomBranch = ThreadLocalRandom.current().nextInt(10);
                int size = Bank.branch[randomBranch].size()/2;
                int updaterId = ThreadLocalRandom.current().nextInt(0, size);
                String accNumber = Bank.branch[randomBranch].get(updaterId).getAccountNumber();

                synchronized (Bank.branch[randomBranch]) {
                    Bank.branch[randomBranch].remove(updaterId);
                    try {
                        Bank.fd.println("An account with Account Number " + accNumber + " is removed from the bank");
                    } catch (Exception e) {
                    }
                }
            }

            if(transferAmount)
            {
                int reciever=ThreadLocalRandom.current().nextInt(10);
                int recieverSize = Bank.branch[reciever].size()/2;
                int recieverIndex = ThreadLocalRandom.current().nextInt(0, recieverSize);
                
                int sender=ThreadLocalRandom.current().nextInt(10);
                int senderSize = Bank.branch[sender].size()/2;
                int senderIndex = ThreadLocalRandom.current().nextInt(0, senderSize);
                
                int randomAmount = ThreadLocalRandom.current().nextInt(0, 100001);

                if(Bank.branch[sender].get(senderIndex).getBalance()>=randomAmount)
                {
                    synchronized(Bank.branch[sender].get(senderIndex))
                    {
                        synchronized(Bank.branch[reciever].get(recieverIndex))
                        {
                            String accNumSender=Bank.branch[sender].get(senderIndex).getAccountNumber();
                            Bank.branch[sender].get(senderIndex).withdraw(randomAmount);

                            String accNumReciever=Bank.branch[reciever].get(recieverIndex).getAccountNumber();
                            Bank.branch[reciever].get(recieverIndex).deposit(randomAmount);


                            try {
                                Bank.fd.println("Rs " + randomAmount + "is tranferred from "+accNumSender+"to"+accNumReciever);
                            } catch (Exception e) {
                            }

                        }
                    }
                }
                else 
                {
                    String accNumSender=Bank.branch[sender].get(senderIndex).getAccountNumber();
                    String accNumReciever=Bank.branch[reciever].get(recieverIndex).getAccountNumber();

                    try {
                        Bank.fd.println("Transfer not possible due to insufficient balance in account " + accNumSender);
                    } catch (Exception e) {
                    }
                }
            }

            if(transferAccount)
            {
                

                int sender=ThreadLocalRandom.current().nextInt(10);
                int senderSize = Bank.branch[sender].size()/2;
                int senderIndex = ThreadLocalRandom.current().nextInt(0, senderSize);
                long balance=Bank.branch[sender].get(senderIndex).getBalance();
                String accNumSender=Bank.branch[sender].get(senderIndex).getAccountNumber();

                int reciever=ThreadLocalRandom.current().nextInt(10);
                String accNumReciever=Bank.generateAccountNumber(reciever);

                synchronized(Bank.branch[sender])
                {
                    Bank.branch[sender].remove(senderIndex);
                    Account newAcc=new Account(balance,accNumReciever);
                    Bank.branch[reciever].add(newAcc);


                    try {
                        Bank.fd.println("Account " + accNumSender + "is transferred to branch" + reciever +" at account number "+ accNumReciever);
                    } catch (Exception e) {
                    }
                }
            }

        }
    }
}

public class GNB {

    // Every branch has 10000 users initially
    public static final int USERS = (int) 1E4;

    public static void main(String[] args) throws IOException, InterruptedException {

        int perUpdaterTrans = Integer.parseInt(args[0]);

        // All transaction would be listed in Data.txt
        Bank.fd = new PrintWriter(new FileWriter("Data.txt"));
        long start = System.currentTimeMillis();
        System.out.println("Execution Started May take a while....");

        for (int i = 0; i < 10; i++) {

            Bank.userPerBranch[i] = 0;
            Bank.branch[i] = new LinkedList<Account>();

            for (int user = 1; user <= USERS; user++) {

                int balance = ThreadLocalRandom.current().nextInt(10000);
                String accountNum = Bank.generateAccountNumber(i);
            
                Bank.branch[i].add(new Account(balance, accountNum));
            }
        }

        int numThreads = 100;
        Thread[] threads = new Thread[numThreads];

        for (int threadId = 0; threadId < numThreads; threadId++) {
            threads[threadId] = new Modify((int) (threadId / 10), perUpdaterTrans);
            threads[threadId].start();
        }

        try {
            // Make the main thread wait till all the child threads are done with their
            // execution
            for (int i = 0; i < numThreads; i++) {
                threads[i].join();
            }
        } catch (InterruptedException e) {
            // It is possible that the all the child threads are already dead
        }

        System.out.println("Execution Time with " + perUpdaterTrans + " Transactions per updater : "
                + (double) ((double) (System.currentTimeMillis() - start) / (double) 1000) + "s");
        Bank.fd.close();

    }
}
