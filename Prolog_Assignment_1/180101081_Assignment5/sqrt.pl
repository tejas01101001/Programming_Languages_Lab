/*
     Author : Tejas Khairnar
     Roll Number : 180101081

     1. GNU Prolog 1.4.5 (64 bits) Compiled Feb 23 2020, 20:14:50 with gcc was used in Linux System.
     2. Type "gprolog" in the terminal and press enter to start the prolog interpreter.
     3. Load the database "sqrt.pl" by typing "consult('sqrt.pl')." in the terminal.
     4. To find the square root of given number with given accuracy :
          squareroot(number,accuracy).
          eg. squareroot(25,0.0056).

*/

% check whether the number is less than squareroot of n
check(Num, N) :- N >= (Num * Num) , !.

% check whether our answer is within the limits.
bounded(N,Val,Accuracy):- abs(N - (Val * Val)) < Accuracy.

% If valid answer is found print it to the console with accuracy upto 10 decimal places.
binSearch(L, _, N, Accuracy) :- bounded(N,L,Accuracy), format('~10f', [L]), !.

% if Mid is greater than the square root of N, continue the search on the interval [L, Mid]
binSearch(L, R, N, Accuracy) :- \+(bounded(N,L,Accuracy)), Mid is (L + R) / 2, \+(check(Mid, N)), binSearch(L, Mid, N, Accuracy), !.

% if Mid is less than or equal to the square root of N, then continue the search on the interval [Mid, R]
binSearch(L, R, N, Accuracy) :- \+(bounded(N,L,Accuracy)), Mid is (L + R) / 2, check(Mid, N), binSearch(Mid, R, N, Accuracy), !.

% If N is between 0 and 1 binarysearch for the answer between [0,1].
squareroot(N, Accuracy) :- N < 1, N >= 0, Accuracy > 0, binSearch(0, 1, N, Accuracy), !.

% If N is greater than 1 binarysearch for the answer between [0,N].
squareroot(N, Accuracy) :- N >= 1, Accuracy > 0, binSearch(0, N, N, Accuracy), !.

