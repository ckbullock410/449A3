/* Written by Zachariah, Chandler, and Bader
*/


/* READ

Right now to run, you need to use gprolog instead of swipl (I couldn't figure out command line arguments in swiple)

To run:
  - gprolog min2.txt output2.txt
  - [main].

You can then check the contents of the different penalties (Eg: tooNearTasks(X,Y).)

For some reason the stack size for gprolog is really small
so you might have to use 'GLOBALSZ=500000 LOCALSZ=60000 gprolog min2.txt output2.txt' instead.

*/

% Changes definitions to variable
:- dynamic(
  contents/1,
  forcedPartialAssignments/2,
  forbiddenMachines/2,
  tooNearTasks/2,
  machinePenalties/3,             % ('Mach', 'Task', 'Penalty').  Eg: (1, 'A', 3)
  tooNearPenalties/3,
  exceptions/1,
  bestValue/1,
	bestList/1,
	curPenalty/1,
	unavailable/1,
	comboPen/3,
	isPair/2).


% ALGORITHM PART --------------------------------------------------------------------------------





curPenalty(0).



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








%------------------------------------------------------------------------------------------------




% Lots of facts

bestValue(10000).
bestList([]).

taskLetter('A').
taskLetter('B').
taskLetter('C').
taskLetter('D').
taskLetter('E').
taskLetter('F').
taskLetter('G').
taskLetter('H').

streamToLetter([65|T], 'A', T).
streamToLetter([66|T], 'B', T).
streamToLetter([67|T], 'C', T).
streamToLetter([68|T], 'D', T).
streamToLetter([69|T], 'E', T).
streamToLetter([70|T], 'F', T).
streamToLetter([71|T], 'G', T).
streamToLetter([72|T], 'H', T).
streamToLetter([97|T], 'A', T).
streamToLetter([98|T], 'B', T).
streamToLetter([99|T], 'C', T).
streamToLetter([100|T], 'D', T).
streamToLetter([101|T], 'E', T).
streamToLetter([102|T], 'F', T).
streamToLetter([103|T], 'G', T).
streamToLetter([104|T], 'H', T).

numberToLetter(1, 'A').
numberToLetter(2, 'B').
numberToLetter(3, 'C').
numberToLetter(4, 'D').
numberToLetter(5, 'E').
numberToLetter(6, 'F').
numberToLetter(7, 'G').
numberToLetter(8, 'H').




:- initialization(cmdInput).
cmdInput :-
  argument_value(1, ARG_1),
  argument_value(2, ARG_2),
  runFiles(ARG_1,ARG_2),
  algorithm(1,1).






runFiles(InFile,OutFile):-

  % Removing all previous declerations
  retractall(exceptions(_)),
  retractall(contents(_)),
  retractall(forbiddenMachines(_,_)),
  retractall(forcedPartialAssignments(_,_)),
  retractall(tooNearTasks(_,_)),
  retractall(machinePenalties(_,_,_)),
  retractall(tooNearPenalties(_,_,_)),


  see(InFile),                              % Opens file and makes it the current input stream
  readInput(3,X1),                          % Y1 is the holder of character int code list
  seen,                                     % Closes the current input stream
  withoutLastElem(X1, Y1),                  % Removes last element
  append(Y1,"\n\n\n", Y2),                  % Append newlines of file to Y2
  asserta(contents(Y2)),                    %
  asserta(exceptions(nil)),                 %
  readFile,
  exceptions(Z),
  !,
  parseerrors(Z,OutFile),                   % Error checking
  retractall(exceptions(_)),                % Remove all current exceptions
  asserta(exceptions(nil)).




readInput(-1,[]).
readInput(_,Y):-
    get0(Z),
    readInput(Z,W),
    Y = [Z|W].


% Writing Errors to File
% =================================================
parseerrors(nil, _).
parseerrors(forcedPartialAssignmentException, X):-
  writeToFile(X,"partial assignment error"),
  fail.
parseerrors(machineTaskException, X):-
  writeToFile(X,"invalid machine/task"),
  fail.
parseerrors(machinePenaltyException, X):-
  writeToFile(X,"machine penalty error"),
  fail.
parseerrors(invalidTaskException, X):-
  writeToFile(X,"invalid task"),
  fail.
parseerrors(invalidPenaltyException, X):-
  writeToFile(X,"invalid penalty"),
  fail.
parseerrors(parsingException, X):-
  writeToFile(X,"Error while parsing input file"),
  fail.
parseerrors(noValidSolution, X):-
  writeToFile(X,"No valid solution possible!"),
  fail.






% Writing content to output file
writeToFile(OutputFile,OutputContent):-
  tell(OutputFile),
  outputstuff(OutputContent),
  told,!.

outputstuff(Y):-
  atom_codes(X,Y),
  write(X).



/*
    Parser:
    * Reading File
    * Forced Partial assigments
    * Forbidden machines
    * Machine penalties
    * Too near tasks
*/

readFile :-
  readFile_.
readFile :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(parsingException)).
readFile.

readFile_ :-
  contents(X),!,
  titleLine(X, R1),!,
  fpaLine(R1, R2),!,
  fmLine(R2,R3),!,
  tntLine(R3, R4),!,
  mpLine(R4, R5), !,
  tnpLine(R5, R6),!,
  endOfFile(R6).

% Title Line and Name
titleLine([], []) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(parsingException)).
titleLine(X, R) :-
  withoutPrefix("Name:", X, Q),!,
  endOfLine(Q, R1),!,
  trimmedLine(R1, _, R2),!,
  endOfLine(R2, R).

% Forced Parcial Assigments
fpaLine([], []) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(parsingException)).
fpaLine(X, R) :-
  withoutPrefix("forced partial assignment:", X, Q),!,
  endOfLine(Q, R1),!,
  getPartialAssignments(R1, R).

% Forbidden Machines
fmLine([], []) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(parsingException)).
fmLine(X, R) :-
  withoutPrefix("forbidden machine:", X, Q),!,
  endOfLine(Q, L),!,
  getForbiddenMachines(L, R).

% Too Near Tasks
tntLine([], []) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(parsingException)).
tntLine(X, R) :-
  withoutPrefix("too-near tasks:", X, Q),!,
  endOfLine(Q, L),!,
  getTooNearTasks(L, R).

% Machine Penalties
mpLine([], []) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(parsingException)).
mpLine(X, R) :-
  withoutPrefix("machine penalties:", X, Q),!,
  endOfLine(Q, L),!,
  getMachinePenalties(L, R).

% Too Near Penalties
tnpLine([], []) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(parsingException)).
tnpLine(X, R) :-
  withoutPrefix("too-near penalities", X, Q),!,
  endOfLine(Q, L),!,
  getTooNearPenalties(L, R).

% End of File
endOfFile([]).
endOfFile([10|I]) :-
  endOfFile(I).
endOfFile([9|I]) :-
  endOfFile(I).
endOfFile([13|I]) :-
  endOfFile(I).
endOfFile([32|I]) :-
  endOfFile(I).


% Forced partial assignments
%================================================
getPartialAssignments(I, R) :-
  trimmedLine(I, [], R).
getPartialAssignments(I, R) :-
  trimmedLine(I, Line, R1),!,
  getPartialAssignment(Line),!,
  getPartialAssignments(R1, R).

getPartialAssignment(I) :-
  getPartialAssignment_(I),!.
getPartialAssignment(_) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(forcedPartialAssignmentException)).

getPartialAssignment_(I) :-
  exceptions(nil),!,
  putTriple(I, M, T),!,
  \+ forcedPartialAssignments(M,X),!,
  \+ forcedPartialAssignments(Y,T),!,
  assertz(forcedPartialAssignments(M,T)), !.

putTriple(Word, M, T) :-
  putTriple_(Word, M, T).
putTriple(_, 0, 0) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(parsingException)).
putTriple(_, 0, 0).

putTriple_(Word, M, T) :-
  withoutPrefix("(", Word, R1),!,
  notSpace(R1),!,
  machineNumber(R1, M, R2),!,
  withoutPrefix(",", R2, R3),!,
  notSpace(R3),!,
  taskNumberConstraint(R3, T, R4),!,
  withoutPrefix(")", R4, []),!.


% Forbidden Machines
%================================================
getForbiddenMachines(I, R) :-
  trimmedLine(I, [], R).
getForbiddenMachines(I, R) :-
  trimmedLine(I, Line, R1),!,
  getForbiddenMachine(Line),!,
  getForbiddenMachines(R1, R).

getForbiddenMachine(I) :-
  getForbiddenMachine_(I),!.
getForbiddenMachine(_) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(invalidForbiddenMachine)).

getForbiddenMachine_(I) :-
  exceptions(nil),
  fmTriple(I, M, T),!,
  assertz(forbiddenMachines(M,T)), !.

fmTriple(Word, M, T) :-
  fmTriple_(Word, M, T).
fmTriple(_, 0, 0) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(parsingException)).

fmTriple_(Word, M, T) :-
  withoutPrefix("(", Word, R1),!,
  notSpace(R1),!,
  machineNumber(R1, M, R2),!,
  withoutPrefix(",", R2, R3),!,
  notSpace(R3),!,
  taskNumberConstraint(R3, T, R4),!,
  withoutPrefix(")", R4, []),!.

% Too-near tasks
%================================================
getTooNearTasks(I, R) :-
  trimmedLine(I, [], R).
getTooNearTasks(I, R) :-
  trimmedLine(I, Line, R1),!,
  getTooNearTask(Line),!,
  getTooNearTasks(R1, R).

getTooNearTask(I) :-
  getTooNearTask_(I),!.
getTooNearTask(_) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(invalidTooNearTask)).

getTooNearTask_(I) :-
  exceptions(nil),
  tnTriple(I, M, T),!,
  assertz(tooNearTasks(M,T)), !.

tnTriple(Word, M, T) :-
  tnTriple_(Word, M, T).
tnTriple(_, 0, 0) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(parsingException)).

tnTriple_(Word, M, T) :-
  withoutPrefix("(", Word, R1),!,
  notSpace(R1),!,
  taskNumberConstraint(R1, M, R2),!,
  withoutPrefix(",", R2, R3),!,
  notSpace(R3),!,
  taskNumberConstraint(R3, T, R4),!,
  withoutPrefix(")", R4, []),!.


% Machine penalties
%================================================
getMachinePenalties(I, R) :-
  getMachinePenalties_(I, R1, 1),
  endOfLine(R1, R).
getMachinePenalties(_, []) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(machinePenaltyException)).

getMachinePenalties_(I, R, 8) :-
  exceptions(nil),
  trimmedLine(I, Line, R),!,
  getMachinePenalty(Line, 8).
getMachinePenalties_(I, R, Num) :-
  exceptions(nil),
  trimmedLine(I, Line, R1),!,
  getMachinePenalty(Line, Num),!,
  Next is Num + 1,!,
  getMachinePenalties_(R1, R, Next).

getMachinePenalty(I, Row) :-
  getMachinePenalty_(I, 1, Row),!.
getMachinePenalty(_, _) :-
  exceptions(nil),!,
  retract(exceptions(nil)),!,
  asserta(exceptions(parsingException)).

getMachinePenalty_([], _, _) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(machinePenaltyException)).
getMachinePenalty_(I, _, _):-
  withoutPrefix(" ", I, _),!,
  exceptions(nil),!,
  retract(exceptions(nil)),!,
  asserta(exceptions(parsingException)).
getMachinePenalty_(I, 8, Row) :-
  exceptions(nil),!,
  getWords(I, Row, 8, []).
getMachinePenalty_(I, Num, Row):-
  exceptions(nil),!,
  getWords(I, Row, Num, R),!,
  Next is Num + 1,!,
  getMachinePenalty_(R, Next, Row).

getWords(Line, M, T, R) :-
  getWord(Line, Word, R),!,
  penaltyNumber(Word, P, []),!,
  numberToLetter(T, Letter),!,							
  assertz(machinePenalties(M, T, P)),!.			


% Too near penalties.
%================================================
getTooNearPenalties(I, R) :-
  trimmedLine(I, [], R).
getTooNearPenalties(I, R) :-
  trimmedLine(I, Line, R1),!,
  getTooNearPenalty(Line),!,
  getTooNearPenalties(R1, R).

getTooNearPenalty(I) :-
  getTooNearPenalty_(I),!.
getTooNearPenalty(_) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(invalidTooNearTask)).

getTooNearPenalty_(I) :-
  exceptions(nil),
  tnpQuad(I, M, T, P),!,
  assertz(tooNearPenalties(M,T,P)), !.

tnpQuad(Word, M, T, P) :-
  tnpQuad_(Word, M, T, P).
tnpQuad(_, 0, 0) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(parsingException)).

tnpQuad_(Word, M, T, P) :-
  withoutPrefix("(", Word, R1),!,
  notSpace(R1),!,
  taskNumberPenalty(R1, M, R2),!,
  withoutPrefix(",", R2, R3),!,
  notSpace(R3),!,
  taskNumberPenalty(R3, T, R4),!,
  withoutPrefix(",", R4, R5),!,
  notSpace(R5),!,
  penaltyNumber(R5, P, R6),!,
  withoutPrefix(")", R6, []),!.



% Extra methods needed
% ==============================================

% Takes unprocessed task and returns letter
taskNumberConstraint(I, P, T) :-
  exceptions(nil),
  streamToLetter(I, P, T).
taskNumberConstraint(_, _, []) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(machineTaskException)).

taskNumberPenalty(I, P, T) :-
  exceptions(nil),
  streamToLetter(I, P, T).
taskNumberPenalty(_, _, []) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(invalidTaskException)).


% Takes unprocessed number and returns actual number
penaltyNumber([], _, _) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(invalidPenaltyException)).
penaltyNumber(I, O, R) :-
  exceptions(nil),
  number(I, O, R).
penaltyNumber(_, _, []) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(invalidPenaltyException)).

% Checks for spaces
notSpace(I) :- \+ isSpace(I).
isSpace([9|I]).
isSpace([10|I]).
isSpace([13|I]).
isSpace([32|I]).

% Takes unprocessed word and returns actual word
getWord([], [], []).
getWord([9|I], [], I).
getWord([10|I], [], I).
getWord([10|I], [], I).
getWord([32|I], [], I).
getWord([C|I], [C|O], R) :-
  C \== 10,
  C \== 32,
  C \== 9,
  C \== 13,
  getWord(I, O, R).

% Trims a line
trimmedLine(I, O, R):-
  getLine(I, Line, R),!,
  trimmedLine_(Line, O).

getLine([],[],[]).
getLine([10|I], [], I).
getLine([C|I], [C|Next], R) :-
  getLine(I, Next, R).

trimmedLine_([],[]).
trimmedLine_([9], []).
trimmedLine_([10], []).
trimmedLine_([13], []).
trimmedLine_([32], []).
trimmedLine_([9|T], []) :-
  trimmedLine_(T, []).
trimmedLine_([10|T], []) :-
  trimmedLine_(T, []).
trimmedLine_([13|T], []) :-
  trimmedLine_(T, []).
trimmedLine_([32|T], []) :-
  trimmedLine_(T, []).

trimmedLine_([H|T], [H|O]) :-
  trimmedLine_(T, O).

% Returns a number
machineNumber([],_, _) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(machineTaskException)).
machineNumber(I, O, R) :-
  exceptions(nil),
  number(I, O, R),
  O < 9,
  O > 0.
machineNumber(_, _, []) :-
  exceptions(nil),
  retract(exceptions(nil)),
  asserta(exceptions(machineTaskException)).

number([H|T], O, R) :-
  exceptions(nil),
  isDigit(H),!,
  number_([H|T], [], N, R),!,
  number_codes(O, N),!.

number_([H|I], SOFAR, O, R) :-
  exceptions(nil),
  isDigit(H),
  append(SOFAR, [H], NEXT),
  number_(I, NEXT, O, R).
number_(I, SOFAR, SOFAR, I) :-
  exceptions(nil).

isDigit(N) :- N > 47,!, N < 58.

% Checks for end of line
endOfLine(X, R) :- withoutPrefix(" ", X, R1), endOfLine(R1, R).
endOfLine(X, R) :- withoutPrefix("\r",X,R1), endOfLine(R1,R).
endOfLine(X, R) :- withoutPrefix("\n", X, R).

% Converts to Atom
toAtoms([H|[]],[C]) :- atom_codes(C, [H]).
toAtoms([H|T], [C|R]) :- atom_codes(C, [H]), toAtoms(T, R).

% Converts to String
toString([H|[]], [C]) :- atom_codes(H, [C]).
toString([H|T], [C|R]) :- atom_codes(H, [C]), toString(T, R).

% Removes Last Element
withoutLastElem([_|[]], []).
withoutLastElem([H|T], [H|Q]) :- withoutLastElem(T, Q).

% Removes Prefix
withoutPrefix([], Y, Y).
withoutPrefix([H|[]], [H|Y], Y).
withoutPrefix([H|X], [H|Y], R) :- withoutPrefix(X, Y, R).
