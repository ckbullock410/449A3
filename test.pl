:- dynamic(machinePenalties/3).
:- dynamic(tooNearTasks/2).
:- dynamic(forbiddenMachines/2).
:- dynamic(forcedPartialAssignments/2).
:- dynamic(tooNearPenalties/3).
:- dynamic(isPair/2).
:- dynamic(solutionPair/2).
:- dynamic(curPenalty/1).
:- dynamic(bestValue/1).
:- dynamic(comboPen/3).
:- dynamic(unavailable/1).

curPenalty(0).
bestValue(1000000000).


main :-
	algorithm(1,1).

storeSolutions(9):- !. 		%6 should be 8
storeSolutions(X):-
	isPair(X,B),
	asserta(solutionPair(X,B)),
	A is X+1,
	storeSolutions(A).

printSol(9):- 
	bestValue(A),
	write('Quality: '), write(A), nl,!.
printSol(X):- 
	solutionPair(X,A),
	write(X), write(':'), write(A), write("   "),
	B is X+1,
	printSol(B).



algorithm(1,9):-  		%6 should be 8
	% is finished
	printSol(1),
	!.

algorithm(9,_):-					%6 should be 8
	%new solution is found
	isPair(8,A),					%5 should be 7
	retractall(solutionPairs(_,_)),
	storeSolutions(1),
	retractall(bestValue(_)),
	curPenalty(B),
	asserta(bestValue(B)),
	retract(unavailable(A)),
	comboPen(8,A,C),				%5 should be 7
	D is B-C,
	retract(curPenalty(_)),
	asserta(curPenalty(D)),
	retract(isPair(8,A)),			%5 should be 7
	E is A+1,
	printSol(1),
	algorithm(8,E),					%5 should be 7
	!.

algorithm(A,9):-					%4 should be 8
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
	X < 9,					%6's should be 8's
	Y < 9,
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
	algorithm(D,1),!.

algorithm(X,Y):-
	% was invalid check next task 
	A is Y+1,
	algorithm(X,A).



isValid(X,Y):-
	\+ (unavailable(Y)),
	\+ (forbiddenMachines(X,Y)),
	notTooNearTask(X,Y),
	notForcedAnother(X,Y),
	penaltyNotBigger(X,Y).

notTooNearTask(1,_):-!.

notTooNearTask(8,Y):-			%5 should be 7
	isPair(1,A),
	\+(tooNearTasks(Y,A)),!.

notTooNearTask(X,Y):-
	A is X-1,
	isPair(A, B),
	\+ (tooNearTasks(B,Y)).

notForcedAnother(X,Y):-
	(\+(forcedPartialAssignments(X,_));(forcedPartialAssignments(X,A), A =:= Y)),
	(\+(forcedPartialAssignments(_,Y));(forcedPartialAssignments(B,Y), B =:= X)).

penaltyNotBigger(X,Y):-
	calculatePenalty(X,Y,A),
	curPenalty(B),
	C is A+B,
	bestValue(D),
	C < D.

calculatePenalty(X,Y,C):-
	machinePenalties(X,Y,A),
	checktooNearPenalties(X,Y,B),
	C is A+B.

checktooNearPenalties(1,_,A):- A = 0, !.

checktooNearPenalties(8,Y,A):-			%5 should be 7
	\+(tooNearPenalties(Y,_,_)),
	A = 0, !.

checktooNearPenalties(8,Y,A):-			% should be 7
	isPair(1,B),
	tooNearPenalties(Y,B,C),
	A = C , !.

checktooNearPenalties(X,Y,A):-
	B is X-1,
	isPair(B,C),
	\+(tooNearPenalties(C,Y,_)),
	A = 0, !.

checktooNearPenalties(X,Y,A):-
	B is X-1,
	isPair(B,C),
	tooNearPenalties(C,Y,D),
	A = D.


forcedPartialAssignments(1,1).

machinePenalties(1,1,1).
machinePenalties(1,2,1).
machinePenalties(1,3,2).
machinePenalties(1,4,2).
machinePenalties(1,5,2).
machinePenalties(1,6,2).
machinePenalties(1,7,2).
machinePenalties(1,8,1).

machinePenalties(2,1,1).
machinePenalties(2,2,1).
machinePenalties(2,3,1).
machinePenalties(2,4,1).
machinePenalties(2,5,1).
machinePenalties(2,6,1).
machinePenalties(2,7,1).
machinePenalties(2,8,1).

machinePenalties(3,1,1).
machinePenalties(3,2,1).
machinePenalties(3,3,1).
machinePenalties(3,4,1).
machinePenalties(3,5,1).
machinePenalties(3,6,1).
machinePenalties(3,7,1).
machinePenalties(3,8,1).

machinePenalties(4,1,1).
machinePenalties(4,2,1).
machinePenalties(4,3,1).
machinePenalties(4,4,1).
machinePenalties(4,5,1).
machinePenalties(4,6,1).
machinePenalties(4,7,1).
machinePenalties(4,8,1).

machinePenalties(5,1,1).
machinePenalties(5,2,1).
machinePenalties(5,3,1).
machinePenalties(5,4,1).
machinePenalties(5,5,1).
machinePenalties(5,6,1).
machinePenalties(5,7,1).
machinePenalties(5,8,1).

machinePenalties(6,1,1).
machinePenalties(6,2,1).
machinePenalties(6,3,1).
machinePenalties(6,4,1).
machinePenalties(6,5,1).
machinePenalties(6,6,1).
machinePenalties(6,7,1).
machinePenalties(6,8,1).

machinePenalties(7,1,1).
machinePenalties(7,2,1).
machinePenalties(7,3,1).
machinePenalties(7,4,1).
machinePenalties(7,5,1).
machinePenalties(7,6,1).
machinePenalties(7,7,1).
machinePenalties(7,8,1).

machinePenalties(8,1,1).
machinePenalties(8,2,1).
machinePenalties(8,3,1).
machinePenalties(8,4,1).
machinePenalties(8,5,1).
machinePenalties(8,6,1).
machinePenalties(8,7,1).
machinePenalties(8,8,1).