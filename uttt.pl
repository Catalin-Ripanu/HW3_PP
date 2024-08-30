:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.
initialState(X) :- X = ([['', '', '', '', '', '', '', '', ''],['', '', '', '', '', '', '', '', ''],['', '', '', '', '', '', '', '', '']
              ,['', '', '', '', '', '', '', '', ''],['', '', '', '', '', '', '', '', ''],['', '', '', '', '', '', '', '', ''],
              ['', '', '', '', '', '', '', '', ''],['', '', '', '', '', '', '', '', ''],['', '', '', '', '', '', '', '', '']],-).
% getBoards/2
% getBoards(+State, -Boards)
% Este adevărat dacă în starea State, informațiile din tablele individuale sunt
% cele din variabila Boards.
% Boards este legată la o listă de 9 elemente, fiecare element reprezentând o tablă.
% Ordinea tablelor este cea din lista positions (din utils.pl).
% Fiecare element din listă este o listă de 9 elemente, reprezentând
% pozițiile de pe tablă, ca x, 0, sau ''.
% Pozițiile sunt în ordinea din lista positions (din utils.pl).

getBoards((X,_), Y) :- Y = X.

% getBoard/3
% getBoard(+State, +UPos, -Board)
% Este adebărat dacă în starea State, la poziția UPos din tabla de UTTT,
% se află tabla individuală cu reprezentarea din Board.
% Reprezentarea tablei este descrisă în predicatul getBoards/2.

getBoard((X,_),Y,Z) :- positions(Positions),nth0(Pos,Positions,Y,_),nth0(Pos,X,Z,_).

% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% Întoarce reprezentarea UBoard-ului, indicând tablele individuale câștigate,
% remizate, sau încă în desfășurare. Reprezentarea este aceeași ca a tablelor
% individuale (vezi getBoards/2).
takeout(X,[X|R],R).
takeout(X,[F |R],[F|S]) :- takeout(X,R,S).

perm([X|Y],Z) :- perm(Y,W), takeout(X,Z,W).
perm([],[]).
createList([],_).
createList([H|T],R) :- perm(H,X),append(X,R,New),createList(T,New).

sublist( [], _ ).
sublist( [X|XS], [X|XSS] ) :- sublist( XS, XSS ).
sublist( [X|XS], [_|XSS] ) :- sublist( [X|XS], XSS ).

winningLine(List) :- perm(List,L),([0,1,2] == L; [3,4,5] == L; [6,7,8] == L;
                  [0,3,6] == L; [1,4,7] == L; [2,5,8] == L;
                  [2,4,6] == L; [8,4,0] == L).
drawLine(List) :- (\+ member('',List)),(\+ verifyFunc(List,[],x,0))
                                       ,(\+ verifyFunc(List,[],0,0)).
auxFunc([],Acc,_,_) :-reverse(Acc,Acc1),sublist(L,Acc1),
    winningLine(L).
verifyFunc([],Acc,_,_) :- once(auxFunc([],Acc,_,_)).
verifyFunc([H|T],Acc,Char,N) :-  H == Char -> (append([N],Acc,New),
    N1 is N + 1,verifyFunc(T,New,Char,N1))
    ;(N1 is N + 1, verifyFunc(T,Acc,Char,N1)).


func([],Board,Res) :- reverse(Board,Board1),Res = Board1.
func([H|T],Board,Res) :- ( (verifyFunc(H,[],x,0)) -> (append([x],Board,New),func(T,New,Res))
                         ;   (verifyFunc(H,[],0,0)) -> (append([0],Board,New),func(T,New,Res))
                         ;   (drawLine(H)) -> (append([r],Board,New),func(T,New,Res))
                         ;    (append([''],Board,New),func(T,New,Res))

                             ).
getUBoard((State,_),Board) :- func(State,[],Board).

% getPos/4
% getPos(+State, +UPos, +Pos, -Cell).
% Este adevărat dacă în starea State, în tabla individuală de la poziția UPos în UBoard,
% la poziția Pos pe tablă, se află simbolul Cell (x, 0, sau '').
getPos((S,_), UPos, Pos, Cell) :- positions(P), nth0(URez, P, UPos), nth0(Rez, P, Pos), nth0(URez, S, I), nth0(Rez, I, Cell).
% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adevărat dacă în tabla individuală reprezentată în Board, la poziția Pos,
% se află simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit și pentru UBoard, caz
% în care Cell poate fi și r.
auxFunc(Board,Pos,Cell) :- positions(P),nth0(Result,P,Pos),nth0(Result,Board,Cell).
getPos(Board, Pos, Cell) :- once(auxFunc(Board,Pos,Cell)).

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adevărat dacă în starea State, jucătorul care urmează este NextPlayer
% (poate fi x sau 0)..
count1(L,P,O) :- include(=(O),L,List),length(List,P).
count([],0,_).
count([H|T],P,O) :- count(T,R1,O),count1(H,R2,O), P is R1+R2.
getNextPlayer((S,_), Next) :- count(S,P1,x),count(S,P2,0),((\+ P1 == P2) -> Next = 0; Next = x).

% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adevărat dacă în starea State, pozițiile din NextBoardsPoss sunt pozițiile
% din UBoard ale tablelor disponibile pentru următoarea mutare.
gUBoards([], Aux, [], Rez) :- reverse(Aux, Rez).
gUBoards([H|T], Aux,[HP|TP] , Rez) :- ((\+ H == '') -> gUBoards(T, Aux, TP, Rez);  gUBoards(T,[HP|Aux],TP, Rez)).
getNextAvailableBoards(State, NB) :- (_,X) = State,getUBoard(State,UB),\+ verifyFunc(UB,[],_,0),\+ drawLine(UB),\+ player_wins(_,UB),positions(P),gUBoards(UB, [],P, Aux), (member(X, Aux) -> NB = [X]; NB = Aux).

% getBoardResult/2
% getBoardResult(+Board, -Result)
% Este adevărat dacă pentru o tablă individuală (sau UBoard) cu reprezentarea
% din Board, rezultatul este Result. Result poate fi:
% x sau 0, dacă jucătorul respectiv a câștigat jocul pe tabla dată;
% r, dacă s-a ajuns la remiză (toate pozițiile au fost completate dar
% tabla nu a fost câștigată);
% '', dacă tabla nu a fost câștigată și nu s-au completat toate pozițiile.
% NOTĂ: este deja definit predicatul player_wins/2 în utils.pl.
auxFunc(B, R) :- player(R),verifyFunc(B,[],R,0).
auxFunc(B,R) :- member('',B) -> (R = ''); ( R = r).
getBoardResult(B,R) :- once(auxFunc(B,R)).

% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adevărat dacă starea State corespunde stării jocului în care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost în
% poziția PreviousPos într-o tablă individuală.
% NOTĂ: nu contează în care tablă individuală s-a realizat ultima mutare.
buildState(Boards, Prev, State) :- (Boards,Prev) = State.

% validMove/2
% validMove(+State, +Move)
% Este adevărat dacă mutarea Move este legală în starea State.
% Move este fie o poziție, în cazul în care este o singură tablă disponibilă
% pentru a următoarea mutare din starea State, fie o pereche de poziții, altfel.
myMem(X, [X|_]) :- !.
myMem(X, [_|T]) :- myMem(X, T).
validMove(S, P) :- auxFunc1(S,P).
validMove(S,(UP,P)) :- once(auxFunc1(S,(UP,P))).
auxFunc1(S,(UP,P)) :-positions(LP),myMem(UP,LP),myMem(P,LP),once(getNextAvailableBoards(S,NP)),length(NP,N),N>1,myMem(UP,NP),getPos(S,UP,P,''),!.
auxFunc1(S,P) :- positions(LP),member(P,LP),getNextAvailableBoards(S,NP),length(NP,K),K == 1,nth0(0,NP,UP),getPos(S,UP,P,''),!.

% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
%
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.
replace(List,Index,Elem,Res) :- nth0(Index,List,_,New),nth0(Index,Res,Elem,New).
makeMove(State,M, Next) :- (S,_) = State,validMove(State,M),getNextAvailableBoards(State,[U]),positions(Pos),nth0(U0,Pos,U),nth0(U1,Pos,M),getBoard(State,U,Board),getNextPlayer(State,PL),replace(Board,U1,PL,B1),replace(S,U0,B1,New),buildState(New,M,Next),!.
makeMove(State,(M1,M2), Next) :- (S,_) = State,validMove(State,(M1,M2)),replace(Board,U1,PL,B1),replace(S,U0,B1,New),positions(Pos),nth0(U0,Pos,M1),nth0(U1,Pos,M2),getBoard(State,M1,Board),getNextPlayer(State,PL),buildState(New,M2,Next),!.

% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).
dummy_first(State, Move) :- getNextAvailableBoards(State,L),length(L,Res),nth0(0,L,T),((\+ Res == 1) ->(getBoard(State,T,UBoard),getPos(UBoard,M1,''),Move = (T,M1)); getBoard(State,T,Board),getPos(Board,Move,'')).

% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă
% (ultima din lista de poziții disponibile).
cons(Var):- Var = 8.
dummy_last(State,Move) :- cons(J),getNextAvailableBoards(State,L1),reverse(L1,L),length(L,Res),nth0(0,L,T),positions(P),((\+ Res == 1) ->(getBoard(State,T,UBoard1),reverse(UBoard1,UBoard),getPos(UBoard,Aux,''),nth0(Aux1,P,Aux),I is J-Aux1,nth0(I,P,M1),Move = (T,M1)) ;getBoard(State,T,Board1),reverse(Board1,Board),getPos(Board,MoveAux,''),nth0(Aux,P,MoveAux),U is J - Aux,nth0(U,P,Move)).
















