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
tokenize(Result)-->gather(Chars),{\+ Chars =[]},tokenize(RestResult), 
                    {name(N,Chars), Result=[N|RestResult]}. 
% Discard whitespace
tokenize(R)-->[C],{C<33},tokenize(R).
% Tokenize special character
tokenize([N|R]) --> [C],{C>32},
                        {name(N,[C])},tokenize(R).
tokenize([])-->[].


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
nameList([Name,','|TSBefore], TSAfter, [Name|ResultTree]):-
    name(Name),
    nameList(TSBefore, TSAfter, ResultTree).

%% <stmts> --> <stmt> | <stmts> ; <stmt>
%% <stmt> --> <assignStmt> | <ifStmt>
%% <assignStmt> -->  <id> = <expr>

%% <expr> --> <expr> <op1> <expr1> | <expr1>
%% <expr1> --> <expr1> <op2> <expr0> | <expr0>
%% <expr0> --> <id> | <integer> | <numWDecimal> | <stringLiteral> | (<expr>) 
%% <comparOp> --> == | < | > | <= | >= | <> (not equal)

op1(+).
op1(-).
op2(*).
op2(/).

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
statement(TSBefore,TSAfter,RT):-
    assignmentStmt(TSBefore,TSAfter,RT).

%% <assignStmt> -->  <id> = <expr>
assignmentStmt([ID,=|TSBefore],TSAfter,assign(name(ID),expression(ExpressionTree))):-
    id(ID),
    expression(TSBefore,TSAfter,ExpressionTree).



%% <expr0> --> <id> | <integer> | <numWDecimal> | <stringLiteral> | (<expr>) 
expression0([X|TSAfter],TSAfter,id(X)):-
    id(X).
expression0([X|TSAfter],TSAfter,integer(X)):-
    integer(X).
expression0([X|TSAfter],TSAfter,float(X)):-
    float(X).
expression0([X|TSAfter],TSAfter,string(X)):-
    string(X).
expression0( ['('|TSBefore] , TSAfter , RT ):- 
    expression(TSBefore, [ ')' | TSAfter ], RT).

expression1(TSBefore,TSAfter,ResultTree):- expression0(TSBefore,TSAfter,ResultTree).

expression(TSBefore,TSAfter,ResultTree):- expression1(TSBefore,TSAfter,ResultTree).

%% statementList(TSBefore, TSAfter, ResultTree):-
%%     statement(TSBefore, TSAfterStatement, CurrentResult) ,
%%     restOfStatementList(TSAfterStatement, TSAfter, CurrentResult, ResultTree).

%% restOfStatementList([';'|TSBefore], TSAfter, CurrentResult, [CurrentResult, ; | [ResultTree] ]) :-
%%     statementList(TSBefore, TSAfter, ResultTree).

%% restOfStatementList(TSAfter, TSAfter, ResultTree, ResultTree).

%% statement([VariableName,:=|TSBefore], TSAfter, assign(name(VariableName), ExpressionResult)) :-
%%     atom(VariableName) ,
%%     expression(TSBefore, TSAfter, ExpressionResult).

%% statement([if|TSBefore], TSAfter, if(TestResult, ThenResult, ElseResult)) :-
%%     test(TSBefore, [then|TSAfterThen], TestResult) ,
%%     statement(TSAfterThen, [else|TSAfterElse], ThenResult) ,
%%     statement(TSAfterElse, TSAfter, ElseResult).

%% statement([while|TSBefore], TSAfter, while(TestResult, DoResult)):-
%%     test(TSBefore, [do|TSAfterDo], TestResult) ,
%%     statement(TSAfterDo, TSAfter, DoResult).

%% statement([read, VariableName|TSAfter], TSAfter, read(name(VariableName))) :-
%%     atom(VariableName).

%% statement([write|TSBefore], TSAfter, write(Expression)) :-
%%     expression(TSBefore, TSAfter, Expression).

%% statement(['('|TSBefore], TSAfter, StatementResult) :-
%%     statements(TSBefore, [ ')' | TSAfter], StatementResult).

%% statement([other | TSAfter], TSAfter, other).

%% test(TSBefore, TSAfter, test(Operation, LeftResultTree, RightResultTree)):-
%%     expression(TSBefore, [Operation | TSAfterLeft], LeftResultTree) ,
%%     comparisonOperator(Operation) ,
%%     expression(TSAfterLeft, TSAfter, RightResultTree).

%% expression(TSBefore, TSAfter, ResultTree) :-
%%     subexpression(2, TSBefore, TSAfter, ResultTree).

%% subexpression(Precedence, TSBefore, TSAfter, ResultTree) :-
%%     Precedence > 0 ,
%%     NextPrecedence is Precedence - 1 ,
%%     subexpression(NextPrecedence, TSBefore, TSAfterExpression, SubexpressionResultTree) ,
%%     restOfExpressions(Precedence, TSAfterExpression, TSAfter, SubexpressionResultTree, ResultTree).

%% subexpression(0, [ResultTree|TSAfter], TSAfter, name(ResultTree)) :-
%%     atom(ResultTree).

%% subexpression(0, [ResultTree|TSAfter], TSAfter, const(ResultTree)) :-
%%     integer(ResultTree).

%% subexpression(0, ['('|TSBefore], TSAfter, ResultTree) :-
%%     subexpression(2, TSBefore, [')'|TSAfter], ResultTree).

%% restOfExpressions(Precedence, [Operator|TSBefore], TSAfter, ExpressionResultTree, ResultTree) :-
%%     operator(Precedence, Operator),
%%     NextPrecedence is Precedence - 1,
%%     subexpression(NextPrecedence, TSBefore, TSAfterSubexpression, SubexpressionResultTree) ,
%%     restOfExpressions(Precedence, TSAfterSubexpression, TSAfter, expression(Operator, ExpressionResultTree, SubexpressionResultTree), ResultTree).

%% restOfExpressions(_AnyPrecedence, TSAfter, TSAfter, ResultTree, ResultTree).



comparisonOperator(==).
comparisonOperator(>).
comparisonOperator(<).
comparisonOperator(>=).
comparisonOperator(<=).
comparisonOperator(<>).

operator(2,*).
operator(2,/).
operator(1,+).
operator(1,-).

typecheck(FileName,TSAfter,ResultTree):- 
    open(FileName, 'read', InputStream),
    read_stream_to_codes(InputStream, ProgramString),
    close(InputStream),
    phrase(tokenize(TSBefore), ProgramString),
    program(TSBefore, TSAfter, ResultTree).
    %% traverse(ResultTree, FirstError),
    %% reportError(FirstError).
