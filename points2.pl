
vmpoints(Test, Points):-
        member(Test:Points,
               [
               movePriority:2,
               bestIndividualMoves:2,
               narrowGreedy:1,
               greedy:4,
               strats:1
              ]).
