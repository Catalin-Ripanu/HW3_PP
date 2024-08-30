
vmpoints(Test, Points):-
        member(Test:Points,
               [
               getBoards:0.2,
               getBoard:0.5,
               getUBoard:0.3,
               getPos4:0.5,
               getPos3:0.5,
               getNextPlayer:1,
               getNextAvailableBoards:1,
               getBoardResult:1,
               validMove:2,
               makeMove:3,
               dummy_first:1,
               dummy_last:1
              ]).
