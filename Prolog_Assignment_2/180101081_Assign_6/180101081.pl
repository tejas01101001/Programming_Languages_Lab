/*
   Author : Tejas Khairnar
   Roll number : 180101081

   1. This code was executed using SWI-Prolog (threaded, 64 bits, version 7.6.4) in ubuntu 20.04.
   2. Make sure you are in the same working directory as the source code i.e 180101081.pl
   3. To run the main prolog file type in terminal:    
      ` swipl 180101081.pl `
   4. Load the database 'Mazedata.pl' by typing in console:    
      ` consult('Mazedata.pl'). `

   5. To add a faulty node X :  
      `add_faulty(X).`
   6. To remove a faulty node X :    
      `remove_faulty(X).`
   7. To find the shortest path between source(src) and destination(dst) :   
      `shortest_path(src,dst,Result).`

   8. Test case embedded
     
    shortest_path(21,78,Path).
    remove_faulty(23).
    shortest_path(21,78,Path).
    add_faulty(33).
    shortest_path(21,78,Path).
    
   Observe in the test case the shortest path length decreases after we mark node
   23 as non faulty and the path changes when we mark node 33 as faulty.

   I have attached a screenshot for the same.
*/


/* To support the complete printing of the lists while writing to the console*/
:- set_prolog_flag(answer_write_options, [max_depth(0)]).

/* To support the dynamic queries*/
:- dynamic(faultynode/1).
:- dynamic(shortest_path/3).
:- dynamic(breadth_first_search/5).

/* Convert the given number to a List */
get_list(Num,Ls):- Ls = [Num]. 


/* Remove a faulty node X*/
remove_faulty(Node):-
    retract(faultynode(Node)).

/* Remove a faulty node X*/
add_faulty(Node):-
    assert(faultynode(Node)).

/* Predicate to find the shortest path*/
shortest_path(Src, Dst, Result):-

    /* Queue for breadth first search 
      Array of visited nodes
      Stores paths of nodes currently in Queue 
      are initialized with the source node */

    Queue = [Src], 
    Visited = [Src], 
    Paths = [[Src]], 

    /* After getting the first answer stop searching. */
    breadth_first_search(Queue, Dst, Visited, Paths, Result), !. 


breadth_first_search([Curr|_], Dst, _, [Path|_], Result):-

    /* Stop the search if the current node is same as the destination node*/
    Curr == Dst, 
    Result = Path,
    write('The shortest path is: ').

/* Implemented the standard breadth first search alogrithm in prolog*/
breadth_first_search([Curr|Rem], Dst, Visited, [Head|Tail], Result):- 
    
    /* Check if the current node is not the same as the destination node.*/
    \+ (Curr == Dst),

    /* Collect all the nodes in the adjaceny list of the current node which are unvisited */
    findall(X,( mazelink(Curr,X),\+ faultynode(X), \+ member(X, Visited)),Neighbours), 

    /* Add new nodes to visited list */
    append(Visited, Neighbours, NewVisited), 

    /* Convert the neighbours into from of list*/
    maplist(get_list,Neighbours ,Neighbour),

    /* append the head of the list of paths to neighbours*/
    maplist(append(Head), Neighbour, Paths),

    /* Append the neighbours to the tail of the queue*/
    append(Rem, Neighbours, NewQueue), 

    /* Append the Paths to get the new Path list */
    append(Tail, Paths, Path_list),

    breadth_first_search(NewQueue, Dst, NewVisited, Path_list, Result). 

