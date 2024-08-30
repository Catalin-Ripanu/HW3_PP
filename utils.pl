

% empty_board/1
% empty_board(-Board)
% Reprezentare a unei table goale.
empty_board(['', '', '', '', '', '', '', '', '']).

% player/1
% player(-Value)
% Simbolurile posibile cu care poate juca un jucător.
player(x).
player(0).

% nextPlayer/2
% nextPlayer(-Player, -NextPlayer)
% Întoarce simbolul următorului jucător în NextPlayer.
nextPlayer(x, 0).
nextPlayer(0, x).

% positions/1
% positions(+List)
% Lista de poziții posibile pe o tablă de joc.
positions([nw, n, ne, w, c, e, sw, s, se]).

% player_wins/2
% player_wins(-Player, +Board)
% Este adevărat dacă tabla Board este câștigată de jucătorul P (P poate fi x sau 0).
% Pentru remize și pentru table în care jocul este încă în derulare întoarce fals.
% Predicatul folosește predicatul getPos/3.
% Predicatul funcționează corect doar pentru tablele corecte (câștigate de cel mult un jucător).
player_wins(P, Board) :- player(P), forall(member(Pos, [nw, n, ne]), getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [w, c, e]),   getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [sw, s, se]), getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [nw, w, sw]), getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [n, c, s]),   getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [ne, e, se]), getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [nw, c, se]), getPos(Board, Pos, P)).
player_wins(P, Board) :- player(P), forall(member(Pos, [ne, c, sw]), getPos(Board, Pos, P)).


% sortMoves(+PairList, -SortedValues)
% Dacă PairList este o listă de perechi (Prioritate, Mutare), predicatul
% sortează perechile după priorități (cea mai mică prioritate este
% prima) și leagă SortedValues la mutările corespunzătoare
% priorităților, în ordinea priorităților. Sunt eliminate duplicatele
% (se păstrează cel mai din stânga / cel mai prioritar) duplicat.
% Ordinea valorilor cu aceeași prioritate nu se schimbă.
sortMoves(PriorityList, Moves) :-
	predsort(comparator, PriorityList, Sorted),
%	writeln(Sorted),
	findall(M, member((_, M), Sorted), Moves1),
	dupsout(Moves1, [], Moves).

comparator((<), (P, _), (P, _)) :- !.
comparator(Delta, (P1, _), (P2, _)) :- compare(Delta, P1, P2).
dupsout([], _, []).
dupsout([H|T], Seen, TOut) :- member(H, Seen), !, dupsout(T, Seen, TOut).
dupsout([H|T], Seen, [H|TOut]) :- dupsout(T, [H|Seen], TOut).


% apply_moves/3
% apply_moves(+Board, +Moves, -NewBoard)
% Apllies a series of moves specified as coordinates to a UTTT board.
apply_moves(State, [], State).
apply_moves(State, [Move], NewState) :-
		makeMove(State, Move, NewState), !.
apply_moves(State, [Move | T], NewState) :-
		makeMove(State, Move, IntermState), !,
		apply_moves(IntermState, T, NewState).
apply_moves(State, [Move | T], _) :- !,
		format("Apply moves failed in boards:~n"), printBoards(State),
		format("when trying to apply [~w], with moves remaining: ~w~n", [Move, T]),
		false.

apply_strategy(State, Strategy, NextState):-
    call(Strategy, State, Move),
    format("Selected move ~w~n", [Move]),
    makeMove(State, Move, NextState).

% play/4
% play(+S1, +S2, -Moves, -Winner)
% Joacă un joc complet cu strategiile S1 pentru x și S2 pentru 0 și
% leagă Moves la lista mutărilor și Winner la câștigărot sau r (dacă
% remiză).
play(S1, S2, Moves, Winner) :- initialState(S), play_strategies(S, S1, S2, [], Moves, Winner).
play_strategies(State, _, _, History, Moves, Winner) :-
		getUBoard(State, UB),
		player_wins(Winner, UB), !,
		reverse(History, Moves), printBoards(State).
play_strategies(State, _, _, History, Moves, r) :-
		getUBoard(State, UB),
		\+ member('', UB), !,
		reverse(History, Moves), printBoards(State).
play_strategies(State, S1, S2, H, Mvs, Win) :-
		getNextPlayer(State, P),
		call(S1, State, Move),
		(   validMove(State, Move) -> true
		;   format("Move invalid: ~w ~n", [Move]), false),
		makeMove(State, Move, Next), !,
		play_strategies(Next, S2, S1, [(P, Move) | H], Mvs, Win).
play_strategies(State, S1, _, H, _, _) :-
		getNextPlayer(State, P), !,
		format("Failed to get strategy ~w or to apply move for player ~w in boards:~n", [S1, P]),
		printBoards(State), reverse(H, Mvs),
		format("after moves: ~w~n", [Mvs]),
		false.
play_strategies(State, _, _, _, _, _) :-
		format("getNextPlayer failed in state: ~n"), printBoards(State),
		!, false.



% print_cell/1
% print_cell(+Cell)
% Afișează o celulă (o poziție).
print_cell('', Sep) :- format('~w~w', ['-', Sep]).
print_cell(Cell, Sep) :- format('~w~w', [Cell, Sep]).

% print_board_line/2
% print_board_line(+SubRow, +Board)
% Afișează o linie dintr-o tablă individuală.
print_board_line(SubRow, Board) :-
    IdxStart is SubRow * 3,
    IdxEnd is (SubRow + 1) * 3 - 1,
    forall(
        between(IdxStart, IdxEnd, Idx),
        (nth0(Idx, Board, Cell),
         print_cell(Cell, " "))
    ).

% print_uttt_line/2
% print_uttt_line(+Row, +UtttBoard, +UBoard)
% Afișează un rând din tabla mare de UTTT.
print_uttt_line(Row, UtttBoard, UB) :-
    IdxStart is Row * 3,
    IdxEnd is (Row + 1) * 3 - 1,
    forall(
        between(0, 2, SubRow),
        (   forall(
                between(IdxStart, IdxEnd, Idx),
                (nth0(Idx, UtttBoard, Board),
                 write("   "),
                 (SubRow == 1 ->
			(nth0(Idx, UB, R),
			print_cell(R, "| ")) ;
			write("   ")),
                 print_board_line(SubRow, Board)
                 )),
            write("\n")
        )).

% printBoards/1
% printBoards(+State)
% Afișează tabla de Ultimate Tic Tac Toe.
% Este necesar ca getBoards și getUBoard să fie deja implementate.
printBoards(State) :-
	getBoards(State, Boards),
	getUBoard(State, UB),
	getNextPlayer(State, P),
	getNextAvailableBoards(State, NB),
    forall(
        between(0, 2, Row),
        (print_uttt_line(Row, Boards, UB),
         write("\n"))
    ),
    format("Next player ~w will move in ~w~n", [P, NB]).

% printBoard/1
% printBoard(+Board)
% Afișează tabla de Ultimate Tic Tac Toe.
printBoard(Board) :-
	forall(
		between(0, 2, Row),
		(print_board_line(Row, Board), write("\n"))
		).
