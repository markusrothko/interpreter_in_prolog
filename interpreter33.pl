/***
Markus Bowie
Noël Hennings
***/

/*** 
Load the tokenizer (tokenize/2) and the file_writer (write_to_file/3).
***/
:- [tokenizer].
:- [filewriter].


/***
The top level predicate run/2 of the solution.
To be called like this:
?- run('program1.txt','myparsetree1.txt').
***/
run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree,Program,[]),
/*	
evaluate bortkommenterad för att den inte fungerar.
evaluate(ParseTree,[],VariablesOut),
*/
	write_to_file(OutputFile,ParseTree,VariablesOut).
	
/***
parse(-ParseTree)-->
	A grammar defining your programming language,
	and returning a parse tree.
	
***/

/* WRITE YOUR CODE FOR THE PARSER HERE */

parse(ParseTree) --> assign(ParseTree).

	
assign(assignment(Id,Assignoperator,Expr,Semi)) -->
	ident(Id),
	assign_op(Assignoperator),
	expr(Expr),
	semicolon(Semi).


	
expr(expression(Term)) -->
	term(Term).
expr(expression(Term,AddOp,Expr)) -->
	term(Term),
	add_op(AddOp),
	expr(Expr).
expr(expression(Term,SubOp,Expr)) -->
	term(Term),
	sub_op(SubOp),
	expr(Expr).
	
	
term(term(Factor)) -->
	factor(Factor).
term(term(Factor,MultOp,Term)) -->
	factor(Factor),
	mult_op(MultOp),
	term(Term).
term(term(Factor,DivOp,Term)) -->
	factor(Factor),
	div_op(DivOp),
	term(Term).
	

factor(factor(int(I))) -->
		num(I).
factor(factor(Left_paren, Expr, Right_paren)) -->
	left_paren(Left_paren),
	expr(Expr),
	right_paren(Right_paren).



ident(ident(X)) --> [X], {atom(X)}.


num(I) --> [I], {number(I)}.

	
assign_op(assign_op) --> ['='].
add_op(add_op) --> ['+'].
sub_op(sub_op) --> ['-'].
mult_op(mult_op) --> ['*'].
div_op(div_op) --> ['/'].
left_paren(left_paren) --> ['('].
right_paren(right_paren) --> [')'].
semicolon(semicolon) --> [';'].

	
/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
***/

/* WRITE YOUR CODE FOR THE EVALUATOR HERE */

evaluate(assignment(ident(Id),assign_op,Expr,semicolon),VariablesIn,VariablesOut):-
	evaluate_expr(Expr,VariablesIn,Expr2),
	add_element(Id = Expr2,VariablesIn,VariablesOut).

evaluate_expr(expression(Term),VariablesIn,Expr2):-
	evaluate_term(Term,VariablesIn,Term2),
	append(Term2,VariablesIn,Expr2).
evaluate_expr(expression(Term,add_op,Expr),VariablesIn,Expr2):-
	evaluate_term(Term,VariablesIn,Term2),
	evaluate_expr(Expr,VariablesIn,Expr3),
	add_sum(Term2,Expr3,add_op,Res),
	append(Res,VariablesIn,Expr2).
evaluate_expr(expression(Term,sub_op,Expr),VariablesIn,Expr2):-
	evaluate_term(Term,VariablesIn,Term2),
	evaluate_expr(Expr,VariablesIn,Expr3),
	sub_sum(Term2,Expr3,sub_op,Res),
	append(Res,VariablesIn,Expr2).
	
evaluate_term(term(Factor),VariablesIn,Term2):-
	evaluate_factor(Factor,VariablesIn,Factor2),
	append(Factor2,VariablesIn,Term2).
evaluate_term(term(Factor,mult_op,Term),VariablesIn,Term2):-
	evaluate_factor(Factor,VariablesIn,Factor2),
	evaluate_term(Term,VariablesIn,Term3),
	mult_sum(Term3,Expr2,mult_op,Res),
	append(Res,VariablesIn,Term2).
evaluate_term(term(Factor,div_op,Term),VariablesIn,Term2):-
	evaluate_factor(Factor,VariablesIn,Factor2),
	evaluate_term(Term,VariablesIn,Term3),
	mult_sum(Term3,Expr2,div_op,Res),
	append(Res,VariablesIn,Term2).
	
evaluate_factor(factor(int(I)),VariablesIn,Factor2):-
	append(int(I),VariablesIn,Factor2).
evaluate_factor(factor(left_paren, Expr, right_paren),VariablesIn,Factor2):-
	evaluate_expr(Expr,VariablesIn,Expr2),
	append(Expr2,VariablesIn,Factor2).
	
/***
assign = id , ‘=’ , expr , ‘;’ ;
	expr = term , [ ( ‘+’ | ‘-’ ) , expr ] ;
	term = factor , [ ( ‘*’ | ‘/’) , term] ;
	factor = int | ‘(’ , expr , ‘)’ ;
		where id is defined as a Prolog atom and int is defined as an integer.
***/

add_element(Element,ListIn,[Element|ListIn]).
	
add_sum(X,Y,add_op,X+Y).
sub_sum(X,Y,sub_op,X-Y).
mult_sum(X,Y,mult_op,X*Y).
div_sum(X,Y,div_op,X/Y).




















