
unavailable(-1).
forbiddenPair(-1,-1).	


main :-
	write('TEST'),
	algorithm(0,0).
	
%X - Machine , Y - Task

algorithm(X,Y) :- 
	%if first three lines are correct, its a valid combination
	X < 8,
	Y < 8,
	isValid(X,Y),
	asserta(isPair(X,Y)),
	asserta(unavailable(Y)),
	calculatePen(X,Y,_,B),
	retract(currentPenalty(_)),
	asserta(currentPenalty(B)),
	algorithm(X+1, 0).

algorithm(X,Y) :- 	
	X < 8,
	Y < 8,
	% the pair is not valid, increase task number
	algorithm(X, (Y+1)).

algorithm(X,_) :-
	%new solution has been found, store
	X > 7,
	isPair(X-1, A),
	retractAll(solutionPairs(_,_)),
	storeSolution,
	calculatePenalty(X-1,A,C,D),
	retract(bestPenalty(_)),
	asserta(bestPenalty(D)),
	retract(unavailable(A)),
	retract(currentPenalty(_)),
	asserta(currentPenalty(B-C)),
	retract(isPair(X-1, _)),
	algorithm(X-1, A+1).

algorithm(X,Y) :-
	Y > 7,
	\+(X = 0),
	isPair(X-1, A),
	calculatePenalty(X-1, A, B, _)
	currentPenalty(E),
	retract(currentPenalty(_)),
	asserta(currentPenalty(E-B)),
	retract(unavailable(A)),
	retract(isPair(X-1, _)),
	algorithm(X-1, A+1).

algorithm(X,Y) :-
	Y > 7,
	X = 0.
	% went through every possible combination finished

storeSolution :-
	isPair(A,B),
	solutionPair(A,B),
	storeSolution.	
	

isValid(X,Y) :-
	\+ (unavailable(Y)),
	\+ (forbiddenPair(X,Y)),
	\+ (B = Y),
	notTooNearTask(X,Y),
	notForcedAnother(X,Y),
	penaltyNotBigger(X,Y).

notTooNearTask(X,_) :-
	X = 0.
	% its fine

notTooNearTask(X,Y) :-
	X = 7,
	isPair(0, A),
	\+ (tooNearTask(Y,A)).

notTooNearTask(X,Y) :-
	isPair(X-1, A),
	\+ (tooNearTask(A,Y)).

notForcedAnother(X,Y) :-
	forced(X,A),
	\+ (A = Y),
	forced(B,Y),	%if these dont exist will make it false
	\+ (B = X).

penaltyNotBigger(X,Y) :-
	calculatePenalty(X,Y,_,B),
	bestPenalty(A),
	A > B.

calculatePen(X,Y,A,B) :- %A penalty of pair, B total penalty
	machinePen(X,Y,C),
	tooNearPen(X,Y,D),	%will produce false if doesnt exist
	A = C+D,
	currentPenalty(E),
	B = E+A.

