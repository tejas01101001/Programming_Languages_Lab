/*
     Author : Tejas Khairnar
     Roll Number : 180101081

     1. GNU Prolog 1.4.5 (64 bits) Compiled Feb 23 2020, 20:14:50 with gcc was used in Linux System.
     2. Type "gprolog" in the terminal and press enter to start the prolog interpreter.
     3. Load the database "sublist.pl" by typing "consult('sublist.pl')." in the terminal.
     4. To find whether a list is sublist of another :
          check_sublist(List_1,List_2).
          check_sublist([a,b],[a,e,b,d,s,e]).

*/

% An empty list is a sublist of any list.
check_sublist([], _) :-  !.

% If the heads of the two lists match then consider their respective tails.
check_sublist([X | Y], [X | Z]) :- check_sublist(Y, Z), !.

% If the Head of list 1 is not equal to the head of list 2.
check_sublist(X, [_ | Z]) :- check_sublist(X, Z), !.

