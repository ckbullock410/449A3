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


convertPenalties(1,8):-
	machinePenalties(1,8,A),
	asserta(machineOnePenalties(8,A)),
	convertPenalties(2,1),!.

convertPenalties(1,A):-
	machinePenalties(1,A,B),
	asserta(machineOnePenalties(A,B)),
	C is A+1,
	convertPenalties(1,C),!.

convertPenalties(2,8):-
	machinePenalties(2,8,A),
	asserta(machineTwoPenalties(8,A)),
	convertPenalties(3,1),!.

convertPenalties(2,A):-
	machinePenalties(2,A,B),
	asserta(machineTwoPenalties(A,B)),
	C is A+1,
	convertPenalties(2,C),!.

convertPenalties(3,8):-
	machinePenalties(3,8,A),
	asserta(machineThreePenalties(8,A)),
	convertPenalties(4,1),!.

convertPenalties(3,A):-
	machinePenalties(3,A,B),
	asserta(machineThreePenalties(A,B)),
	C is A+1,
	convertPenalties(3,C),!.

convertPenalties(4,8):-
	machinePenalties(4,8,A),
	asserta(machineFourPenalties(8,A)),
	convertPenalties(5,1),!.

convertPenalties(4,A):-
	machinePenalties(4,A,B),
	asserta(machineFourPenalties(A,B)),
	C is A+1,
	convertPenalties(4,C),!.

convertPenalties(5,8):-
	machinePenalties(5,8,A),
	asserta(machineFivePenalties(8,A)),
	convertPenalties(6,1),!.

convertPenalties(5,A):-
	machinePenalties(5,A,B),
	asserta(machineFivePenalties(A,B)),
	C is A+1,
	convertPenalties(5,C),!.

convertPenalties(6,8):-
	machinePenalties(6,8,A),
	asserta(machineSixPenalties(8,A)),
	convertPenalties(7,1),!.

convertPenalties(6,A):-
	machinePenalties(6,A,B),
	asserta(machineSixPenalties(A,B)),
	C is A+1,
	convertPenalties(6,C),!.

convertPenalties(7,8):-
	machinePenalties(7,8,A),
	asserta(machineSevenPenalties(8,A)),
	convertPenalties(8,1),!.

convertPenalties(7,A):-
	machinePenalties(7,A,B),
	asserta(machineSevenPenalties(7,B)),
	C is A+1,
	convertPenalties(7,C),!.

convertPenalties(8,8):-
	machinePenalties(2,8,A),
	asserta(machineEightPenalties(8,A)),!.

convertPenalties(8,A):-
	machinePenalties(8,A,B),
	asserta(machineEightPenalties(A,B)),
	C is A+1,
	convertPenalties(8,C),!.





main :-
	convertPenalties(1,1),
	algorithm(1,1).

storeSolutions(9):- !. 		%6 should be 8
storeSolutions(X):-
	isPair(X,B),!,
	asserta(solutionPair(X,B)),!,
	A is X+1,!,
	storeSolutions(A),!.

printSol(9):-
	bestValue(A),
	write('Quality: '), write(A), nl,!.
printSol(X):-
	solutionPair(X,A),
	write(X), write(':'), write(A), write('   '),
	B is X+1,
	printSol(B).



algorithm(1,9):-  		%6 should be 8
	% is finished
	printSol(1),
	!.

algorithm(9,_):-					%6 should be 8
	%new solution is found
	isPair(8,A),!,					%5 should be 7
	retractall(solutionPairs(_,_)),!,
	storeSolutions(1),!,
	retractall(bestValue(_)),!,
	curPenalty(B),!,
	asserta(bestValue(B)),!,
	retract(unavailable(A)),!,
	comboPen(8,A,C),!,				%5 should be 7
	D is B-C,!,
	retract(curPenalty(_)),!,
	asserta(curPenalty(D)),!,
	retract(isPair(8,A)),!,			%5 should be 7
	E is A+1,!,
	printSol(1),!,
	algorithm(8,E),					%5 should be 7
	!.

algorithm(A,9):-					%4 should be 8
	%go back one machine
	B is A-1,!,
	isPair(B, C),!,
	D is C+1,!,
	retract(unavailable(C)),!,
	retract(isPair(B,C)),!,
	comboPen(B,C,E),!,
	curPenalty(F),!,
	G is F-E,!,
	retract(comboPen(B,C,_)),
	retract(curPenalty(_)),
	asserta(curPenalty(G)),
	algorithm(B,D),!,
	!.

algorithm(X,Y):-
	%check if valid combo
	X < 9,				%6's should be 8's
	Y < 9,
	isValid(X,Y),!,
  asserta(isPair(X,Y)),!,
	asserta(isPair(X,Y)),!,
	asserta(unavailable(Y)),!,
	calculatePenalty(X,Y,F),!,
	curPenalty(A),!,
	asserta(comboPen(X,Y,F)),!,
	retract(curPenalty(_)),!,
	C is A+F,!,
	asserta(curPenalty(C)),!,
	D is X+1,!,
	algorithm(D,1),!.

algorithm(X,Y):-
	% was invalid check next task
	A is Y+1,!,
	algorithm(X,A).



isValid(X,Y):-
	\+ (unavailable(Y)),!,
	\+ (forbiddenMachines(X,Y)),!,
	notTooNearTask(X,Y),!,
	notForcedAnother(X,Y),!,
	penaltyNotBigger(X,Y).

notTooNearTask(1,_):-!.

notTooNearTask(8,Y):-			%5 should be 7
	isPair(1,A),!,
	\+(tooNearTasks(Y,A)),!.

notTooNearTask(X,Y):-
	A is X-1,!,
	isPair(A, B),!,
	\+ (tooNearTasks(B,Y)).

notForcedAnother(X,Y):-
	(\+(forcedPartialAssignments(X,_));(forcedPartialAssignments(X,A), A =:= Y)),!,
	(\+(forcedPartialAssignments(_,Y));(forcedPartialAssignments(B,Y), B =:= X)).

penaltyNotBigger(X,Y):-
	calculatePenalty(X,Y,A),!,
	curPenalty(B),!,
	C is A+B,!,
	bestValue(D),!,
	C < D.



getMachinePenalties(1,A,B):-
	machineOnePenalties(A,B),!.

getMachinePenalties(2,A,B):-
	machineTwoPenalties(A,B),!.

getMachinePenalties(3,A,B):-
	machineThreePenalties(A,B),!.

getMachinePenalties(4,A,B):-
	machineFourPenalties(A,B),!.

getMachinePenalties(5,A,B):-
	machineFivePenalties(A,B),!.

getMachinePenalties(6,A,B):-
	machineSixPenalties(A,B),!.

getMachinePenalties(7,A,B):-
	machineSevenPenalties(A,B),!.

getMachinePenalties(8,A,B):-
	machineEightPenalties(A,B),!.





calculatePenalty(X,Y,C):-
	getMachinePenalties(X,Y,A),
	checktooNearPenalties(X,Y,B),!,
	C is A+B.

checktooNearPenalties(1,_,A):- A = 0, !.

checktooNearPenalties(8,Y,A):-
	isPair(1,B),
	tooNearPenalties(Y,B,C),!,
	A = C, !.

checktooNearPenalties(8,Y,A):-
	A = 0,!.

checktooNearPenalties(X,Y,A):-
	B is X-1,
	isPair(B,C),
	tooNearPenalties(C,Y,D),!,
	A = D, !.

checktooNearPenalties(X,Y,A):-
	A = 0,!.




tooNearPenalties(1,2,10).

machinePenalties(1,1,2).
machinePenalties(1,2,1).
machinePenalties(1,3,2).
machinePenalties(1,4,2).
machinePenalties(1,5,2).
machinePenalties(1,6,2).
machinePenalties(1,7,2).
machinePenalties(1,8,2).

machinePenalties(2,1,2).
machinePenalties(2,2,1).
machinePenalties(2,3,2).
machinePenalties(2,4,2).
machinePenalties(2,5,2).
machinePenalties(2,6,2).
machinePenalties(2,7,2).
machinePenalties(2,8,2).

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
machinePenalties(8,2,2).
machinePenalties(8,3,2).
machinePenalties(8,4,2).
machinePenalties(8,5,2).
machinePenalties(8,6,2).
machinePenalties(8,7,2).
machinePenalties(8,8,2).
