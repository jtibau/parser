list([H|T],H,T).

%% Tokenizer code
%Created by Bruno Dufour, Fall 2005
%
% Append
append([ ],A,A).
append([A|B],C,[A|D]) :- append(B,C,D).


gather(Chars) --> [C],  {alphaNumeric(C)}, gather(Rest), {Chars=[C|Rest]}.

gather([]) --> {true}.
alphaNumeric(C):- 96<C,C<123;
                  64<C, C<91;
                  47<C, C<58.

% - Floats ---------------------------------------------------------------------
digit(D) --> [D], {47 < D, D < 58}.
nzdigit(D) --> [D], {48 < D, D < 58}.

floatlit(F) -->
        nzdigit(D0),
        digits(D1),
        ".",
        nedigits(D2),
        {append([D0|D1], [46], T), append(T, D2, D), name(F, D)}.

nedigits([D|T]) -->
        digit(D), !,
        digits(T).

digits(L) --> nedigits(L).
digits([]) --> [].
% ------------------------------------------------------------------------------

% - Strings --------------------------------------------------------------------

quote('"').

gatherString(Chars) --> [C], {C=\=34}, gatherString(Rest), {Chars=[C|Rest]}.
gatherString([]) --> {true}.

stringlit(S) --> "\"", gatherString(Chars), "\"", {string_to_list(S,Chars)}.

% ------------------------------------------------------------------------------

% Tokeinze comparison operators
tokenize(Z) --> "==", tokenize(Y), {Z = [== | Y]}.
tokenize(Z) --> ">=", tokenize(Y), {Z = [>= | Y]}.
tokenize(Z) --> "<=", tokenize(Y), {Z = [<= | Y]}.
tokenize(Z) --> "<>", tokenize(Y), {Z = [<> | Y]}.
tokenize(Z) --> ">", tokenize(Y), {Z = [> | Y]}.
tokenize(Z) --> "<", tokenize(Y), {Z = [< | Y]}.

% Tokenize float
tokenize(Result) --> floatlit(F), tokenize(Rest), {Result=[F|Rest]}.
% Tokenize string
tokenize(Result) --> stringlit(S), tokenize(Rest), {Result=[S|Rest]}.
% Tokenize id / int
tokenize(Result) --> gather(Chars),{\+ Chars =[]},tokenize(RestResult), 
                    {name(N,Chars), Result=[N|RestResult]}. 
% Discard whitespace
tokenize(R)-->[C],{C<33},tokenize(R).
% Tokenize special character
tokenize([N|R]) --> [C],{C>32},
                        {name(N,[C])},tokenize(R).
tokenize([])-->[].

%% <program> --> <type-decl-stmts> ; <stmts>   
program(TSBefore, TSAfter, (declarations(DeclarationTree),statements(StatementList))) :-
     typeDeclarationStatementList(TSBefore, TSAfterDeclaration, DeclarationTree),
     list(TSAfterDeclaration,';',T),
     statementList(T, TSAfter, StatementList).

%% <simple-type> --> int | float | string | bool
simpleType(int).
simpleType(float).
simpleType(string).
simpleType(bool).

%% <type> --> <simple-type> | <array-type>  | <struct-type>
%% Should use the space bellow the following line, for stages 2 and 3 of this project.
type(X) :- simpleType(X).

%% Assume <name> is a sequence of letters and numbers beginning with a letter. 
%%% Have to improve this.
name(X) :- atom(X).

%% <type-decl> --> <type> <name-list> 
typeDeclarationStatement( [Type | TSBefore], TSAfter,typeDeclaration(Type,Names) ):-
    type(Type),
    nameList(TSBefore, TSAfter, Names).

%% <type-decl-stmts> --> <type-decl> | <type-decl-stmts> ; <type-decl>
typeDeclarationStatementList(TSBefore, TSAfter, [RT]):-
    typeDeclarationStatement(TSBefore, TSAfter, RT).
typeDeclarationStatementList(TSBefore, TSAfter, [RT1|RTR]):-
    typeDeclarationStatement(TSBefore, TSAfter1, RT1),
    list(TSAfter1,';',T),
    typeDeclarationStatementList(T,TSAfter,RTR).

%% <name-list> --> <name> | <name>, <name-list> 
nameList([Name|TSAfter], TSAfter, [Name]).
nameList([Name,','|TSBefore], TSAfter, [Name|RT]):-
    name(Name),
    nameList(TSBefore, TSAfter, RT).

op1(+).
op1(-).
op2(*).
op2(/).

%% <comparOp> --> == | < | > | <= | >= | <> (not equal)
comparOp(==).
comparOp(>).
comparOp(<).
comparOp(>=).
comparOp(<=).
comparOp(<>).

%% <string-op> --> + (concatenation)
stringOp(+).

%% <id> --> <name> |  <id>.<name> | <id> [ <expr> ]
id(X) :- name(X).

%% <stmts> --> <stmt> | <stmts> ; <stmt>
statementList(TSBefore,TSAfter,[RT]):-
    statement(TSBefore,TSAfter,RT).
statementList(TSBefore,TSAfter,[RT1|RTR]):-
    statement(TSBefore,TSAfter1,RT1),
    list(TSAfter1,';',T),
    statementList(T,TSAfter,RTR).

%% <stmt> --> <assignStmt> | <ifStmt>
statement(TSBefore,TSAfter,RT):- assignmentStmt(TSBefore,TSAfter,RT).

statement(TSBefore,TSAfter,RT):- ifStatement(TSBefore,TSAfter,RT).

%% <assignStmt> -->  <id> = <expr>
assignmentStmt([ID,=|TSBefore],TSAfter,assign(name(ID),expression(ExpRT))):-
    id(ID),
    expression(TSBefore,TSAfter,ExpRT).


%% <expr0> --> <id> | <integer> | <numWDecimal> | <stringLiteral> | (<expr>) 
expression0([X|TSAfter],TSAfter,id(X)):- id(X).
expression0([X|TSAfter],TSAfter,int(X)):- integer(X).
expression0([X|TSAfter],TSAfter,float(X)):- float(X).
expression0([X|TSAfter],TSAfter,string(X)):- string(X).
expression0( ['('|TSBefore] , TSAfter , RT ):- 
    expression(TSBefore, [ ')' | TSAfter ], RT).

%% <expr1> --> <expr1> <op2> <expr0> | <expr0>
expression1(TSBefore,TSAfter,expr(OP,RT0,RT1)):-
    expression0(TSBefore,[OP|TSAfter1],RT0),
    op2(OP),
    expression1(TSAfter1,TSAfter,RT1).
expression1(TSBefore,TSAfter,RT):- expression0(TSBefore,TSAfter,RT).

%% <expr> --> <expr> <op1> <expr1> | <expr1>
expression(TSBefore,TSAfter,expr(OP,RT1,RTSub)):-
    expression1(TSBefore,[OP|TSAfter1],RT1),
    op1(OP),
    expression(TSAfter1,TSAfter,RTSub).
expression(TSBefore,TSAfter,RT):- expression1(TSBefore,TSAfter,RT).

%% <ifStmt> --> if <test> then <stmt> else <stmt>
ifStatement([if | TSBefore],TSAfter,if(TestRT,ThenRT,ElseRT)):-
    test(TSBefore,[then|TSAfterThen],TestRT),
    statement(TSAfterThen,[else|TSAfterElse],ThenRT),
    statement(TSAfterElse,TSAfter,ElseRT).

%% <test> --> <expr> <compareOp> <expr> | <name>
test(TSBefore,TSAfter,test(OP,FirstExpRT,SecondExpRT)):-
    expression(TSBefore,[OP|TSAfterE],FirstExpRT),
    comparOp(OP),
    expression(TSAfterE,TSAfter,SecondExpRT).
test([Name|TSAfter],TSAfter,name(Name)):- name(Name).

typecheck(FileName,ErrorReport):- 
    open(FileName, 'read', InputStream),
    read_stream_to_codes(InputStream, ProgramString),
    close(InputStream),
    phrase(tokenize(TSBefore), ProgramString),
    program(TSBefore, [], RT),
    RT = (declarations(DeclarationList),statements(StatementList)),
    createDictionary(DeclarationList,VariablesDictionary),!,
    traverse(StatementList, VariablesDictionary,ErrorReport).
    %% reportError(FirstError).

parseTree(FileName,RT):- 
    open(FileName, 'read', InputStream),
    read_stream_to_codes(InputStream, ProgramString),
    close(InputStream),
    phrase(tokenize(TSBefore), ProgramString),
    program(TSBefore, [], RT).

%% Dictionary code

%lookup(+identifier,+dictionary,-result)
lookup(Nm,dic(Nm,Val,_,_),Val).
lookup(Nm,dic(Other,_,Left,_),Val):-
       Nm @< Other, lookup(Nm,Left,Val).
lookup(Nm,dic(Other,_,_,Right),Val):-
       Other @< Nm, lookup(Nm,Right,Val).

%% /*  **creating a dictionary by using lookup!**  */
%% p(D):- lookup(salt,D,X1),
%%          lookup(mustard,D,X2),
%%          lookup(egg,D,X3).

%% %% free variables dict(Nm,**Var**,_,_) are 'waiting' to be filled
%% %% by the next lookup of Nm. Also, free vars at leafs are waiting for
%% %% data structure to be extended
%% p2(D) :- p(D),  lookup(egg,D,oef).

%% %% trying to look up a name  with an actual value, fills the **Var**!
%% p3(D):-p(D), lookup(mustard,D,dijon).
%% p4(D):-p(D), lookup(salt,D, X5). %% lookup with another var is no-op

createDictionary([],_).
createDictionary([Decl|DeclList],Dictionary):-
    registerDeclaration(Decl,Dictionary),
    createDictionary(DeclList,Dictionary).

registerDeclaration(typeDeclaration(Type,NameList),Dictionary):-
    registerVariables(Type,NameList,Dictionary).

registerVariables(_,[],_).
registerVariables(Type,[Name|Names],Dictionary):-
    lookup(Name,Dictionary,Type),
    registerVariables(Type,Names,Dictionary).


%% Using Audrey Decker's simplified list of valid operations
%% Assignments:
%% (X, X).
validAssignment(X,X).
%% (float, int).
validAssignment(float,int).
 
%% Expressions:
%% (+, string, string, string).
validExpression(+,string,string,string).
%% (_, int, int, int).
validExpression(_,int,int,int).
%% (_, int, float, float).
validExpression(_,int,float,float).
%% (_, float, int, float).
validExpression(_,float,int,float).
%% (_, float, float, float).
validExpression(_,float,float,float).

%% Comparisons:
%% (==, X, X).
validComp(==,X,X).
%% (<>, X, X).
validComp(<>,X,X).
%% (_, int, int).
validComp(_,int,int).
%% (_, float, int).
validComp(_,float,float).
%% (_, int, float).
validComp(_,int,float).
%% (_, float, float).
validComp(_,float,int).


traverse([],_,_).
traverse([Statement|Rest],Variables,ErrorReport):-
    checkStatement(Statement,Variables,ErrorReport),
    traverse(Rest,Variables,ErrorReport).


checkStatement(assign(name(ID),expression(ExpTree)),Variables,ErrorReport):-
    lookup(ID,Variables,LeftSideType),
    checkExpression(ExpTree,RightSideType,Variables),
    validAssignment(LeftSideType,RightSideType).

checkStatement(assign(name(ID),expression(ExpTree)),Variables,ErrorReport):-
    lookup(ID,Variables,LeftSideType),
    checkExpression(ExpTree,RightSideType,Variables),
    \+validAssignment(LeftSideType,RightSideType),!,
    ErrorReport = "Test".

checkStatement(if(Test,TrueStatements,FalseStatements),Variables,ErrorReport):-
    checkTest(Test,Variables,ErrorReport),
    checkStatement(TrueStatements,Variables,ErrorReport),
    checkStatement(FalseStatements,Variables,ErrorReport).

checkExpression(id(Name),Type,Dictionary):-
    lookup(Name,Dictionary,Type).
checkExpression(int(_),int,_).
checkExpression(float(_),float,_).
checkExpression(string(_),string,_).

checkExpression(expr(OP,LeftTerm,RightTerm),ResultType,Dictionary):-
    checkExpression(LeftTerm,LeftType,Dictionary),
    checkExpression(RightTerm,RightType,Dictionary),
    validExpression(OP,ResultType,LeftType,RightType).

checkExpression(expr(OP,LeftTerm,RightTerm),ResultType,Dictionary):-
    checkExpression(LeftTerm,LeftType,Dictionary),
    checkExpression(RightTerm,RightType,Dictionary),
    \+validExpression(OP,ResultType,LeftType,RightType),!,
    ErrorReport = "Test".

checkTest(name(_),_,_).
checkTest(test(OP,LeftExp,RightExp),ResultType,Dictionary):-
    checkExpression(LeftExp,LeftType,Dictionary),
    checkExpression(RightExp,RightType,Dictionary),
    validComp(OP,LeftType,RightType).

checkTest(test(OP,LeftExp,RightExp),ResultType,Dictionary):-
    checkExpression(LeftExp,LeftType,Dictionary),
    checkExpression(RightExp,RightType,Dictionary),
    \+validComp(OP,LeftType,RightType),!,
    ErrorReport = "Test".