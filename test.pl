:- dynamic(machinePen/3).
:- dynamic(tooNearTask/2).
:- dynamic(forbid/2).
:- dynamic(forced/2).
:- dynamic(tooNearPen/3).
:- dynamic(isPair/2).
:- dynamic(solutionPair/2).
:- dynamic(curPenalty/1).
:- dynamic(bestPenalty/1).
:- dynamic(comboPen/3).
:- dynamic(unavailable/1).

curPenalty(0).
bestPenalty(1000000000).


main :-
	algorithm(0,0).

storeSolutions(6):- !. 		%6 should be 8
storeSolutions(X):-
	isPair(X,B),
	solutionPair(X,B),
	A is X+1,
	storeSolutions(A).

printSol(3):- nl.
printSol(X):- 
	solutionPair(X,A),
	write(X), write(':'), write(A), write("   "),
	B is X+1,
	printCombos(B).



algorithm(0,6):-  		%6 should be 8
	% is finished
	bestPenalty(A),
	write('Best Solution Penalty: '),
	write(A),
	!.

algorithm(6,_):-					%6 should be 8
	%new solution is found
	isPair(5,A),					%5 should be 7
	retractall(solutionPairs(_,_)),
	%storeSolutions(0),
	retractall(bestPenalty(_)),
	curPenalty(B),
	asserta(bestPenalty(B)),
	retract(unavailable(A)),
	comboPen(5,A,C),				%5 should be 7
	D is B-C,
	retract(curPenalty(_)),
	asserta(curPenalty(D)),
	retract(isPair(5,_)),			%5 should be 7
	E is A+1,
	%printSol(0),
	algorithm(5,E),					%5 should be 7
	!.

algorithm(A,6):-					%4 should be 8
	%go back one machine
	B is A-1,
	isPair(B, C),
	D is C+1,
	retract(unavailable(C)),
	retract(isPair(B,C)),
	comboPen(B,C,E),
	curPenalty(F),
	G is F-E,
	retract(comboPen(B,C,_)),
	retract(curPenalty(_)),
	asserta(curPenalty(G)),
	algorithm(B,D),
	!.

algorithm(X,Y):-
	%check if valid combo
	X < 6,					%6's should be 8's
	Y < 6,
	isValid(X,Y),
	asserta(isPair(X,Y)),
	asserta(unavailable(Y)),
	calculatePenalty(X,Y,F),
	curPenalty(A),
	asserta(comboPen(X,Y,F)),
	retract(curPenalty(_)),
	C is A+F,
	asserta(curPenalty(C)),
	D is X+1,
	algorithm(D,0),!.

algorithm(X,Y):-
	% was invalid check next task 
	A is Y+1,
	algorithm(X,A).

isValid(X,Y):-
	\+ (unavailable(Y)),
	\+ (forbid(X,Y)),
	notTooNearTask(X,Y),
	notForcedAnother(X,Y),
	penaltyNotBigger(X,Y).

notTooNearTask(0,_):-!.

notTooNearTask(5,Y):-			%5 should be 7
	isPair(0,A),
	\+(tooNearTask(Y,A)),!.

notTooNearTask(X,Y):-
	A is X-1,
	isPair(A, B),
	\+ (tooNearTask(B,Y)).

notForcedAnother(X,Y):-
	(\+(forced(X,_));(forced(X,A), A =:= Y)),
	(\+(forced(_,Y));(forced(B,Y), B =:= X)).

penaltyNotBigger(X,Y):-
	calculatePenalty(X,Y,A),
	curPenalty(B),
	C is A+B,
	bestPenalty(D),
	C < D.

calculatePenalty(X,Y,C):-
	machinePen(X,Y,A),
	checkTooNearPen(X,Y,B),
	C is A+B.

checkTooNearPen(0,_,A):- A = 0, !.

checkTooNearPen(5,Y,A):-			%5 should be 7
	\+(tooNearPen(Y,_,_)),
	A = 0, !.

checkTooNearPen(5,Y,A):-
	isPair(0,B),
	tooNearPen(Y,B,C),
	A = C , !.

checkTooNearPen(X,Y,A):-
	B is X-1,
	isPair(B,C),
	\+(tooNearPen(C,Y,_)),
	A = 0, !.

checkTooNearPen(X,Y,A):-
	B is X-1,
	isPair(B,C),
	tooNearPen(C,Y,D),
	A = D.



machinePen(0,0,1).
machinePen(0,1,2).
machinePen(0,2,2).
machinePen(0,3,2).
machinePen(0,4,2).
machinePen(0,5,2).
machinePen(0,6,2).
machinePen(0,7,2).

machinePen(1,0,2).
machinePen(1,1,1).
machinePen(1,2,2).
machinePen(1,3,2).
machinePen(1,4,2).
machinePen(1,5,2).
machinePen(1,6,2).
machinePen(1,7,2).

machinePen(2,0,1).
machinePen(2,1,1).
machinePen(2,2,1).
machinePen(2,3,1).
machinePen(2,4,1).
machinePen(2,5,1).
machinePen(2,6,1).
machinePen(2,7,1).

machinePen(3,0,1).
machinePen(3,1,1).
machinePen(3,2,1).
machinePen(3,3,1).
machinePen(3,4,1).
machinePen(3,5,1).
machinePen(3,6,1).
machinePen(3,7,1).

machinePen(4,0,1).
machinePen(4,1,1).
machinePen(4,2,1).
machinePen(4,3,1).
machinePen(4,4,1).
machinePen(4,5,1).
machinePen(4,6,1).
machinePen(4,7,1).

machinePen(5,0,1).
machinePen(5,1,1).
machinePen(5,2,1).
machinePen(5,3,1).
machinePen(5,4,1).
machinePen(5,5,1).
machinePen(5,6,1).
machinePen(5,7,1).

machinePen(6,0,1).
machinePen(6,1,1).
machinePen(6,2,1).
machinePen(6,3,1).
machinePen(6,4,1).
machinePen(6,5,1).
machinePen(6,6,1).
machinePen(6,7,1).

machinePen(7,0,1).
machinePen(7,1,1).
machinePen(7,2,1).
machinePen(7,3,1).
machinePen(7,4,1).
machinePen(7,5,1).
machinePen(7,6,1).
machinePen(7,7,1).