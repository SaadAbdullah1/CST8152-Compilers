/*************************************************************************************************************************************
File name			: parser.c
Compiler			: MS Visual Studio 2019
Author				: Saad Abdullah, 040877175
Course				: CST 8152 - Compilers
Lab Section         : 301
Assignment			: 3 (Parser)
Date Due      		: December 05 2019
Professor			: Svillen Ranev
Purpose				: Writes a Recursive Descent Predictive Parser (RDPP) for the PLATYPUS language.
Function list		: parser(), match(), syn_eh(), syn_printe(), gen_incode(), program(), opt_statements(), statements(),
 *					  statements_p(), statement(), assignment_statement(), assignment_expression(), selection_statement(),
 *					  iteration_statement(), pre_condition(), input_statement(), variable_list(), variable_list_p(),
 *					  variable_identifier(), output_statement(), output_list(), arithmetic_expression(),
 *					  unary_arithmetic_expression(), additive_arithmetic_expression(), additive_arithmetic_expression_p(),
 *					  multiplicative_arithmetic_expression(), multiplicative_arithmetic_expression_p(),
 *					  primary_arithmetic_expression(), string_expression(), string_expression_p(), primary_string_expression(),
 *					  conditional_expression(), logical_OR_expression(), logical_OR_expression_p(), logical_AND_expression(),
 *					  logical_AND_expression_p(), relational_expression(), primary_a_relational_expression(),
 *					  primary_s_relational_expression(), relational_operator()
/************************************************************************************************************************************/

#include "parser.h"

/*
*Purpose: Begins the parsing process, when it is called on in platy.c
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: malar_next_token(), program(), match(), gen_incode()
*Parameters: void
*Return Value: n/a
*Algorithm: 
*/
void parser(void) {
	lookahead = malar_next_token();
	program(); 
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*
*Purpose: Matches two tokens: the current input token (lookahead) and the 
			token required by the parser.
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: none
*Parameters: int pr_token_code, int pr_token_attribute
*Return Value: returns to program
*Algorithm: -If the match is successful and the lookahead is SEOF_T, the function returns.
			-If the match is successful and the lookahead is not SEOF_T, the function advances to the next input token
			-If the match is unsuccessful, the function calls the error handler
*/
void match(int pr_token_code, int pr_token_attribute) {

	//check if token code match is unsuccessful
	if (lookahead.code != pr_token_code) {
		syn_eh(pr_token_code);
		return;
	}
	
	if (lookahead.code == pr_token_code) {

		//check if token code match is successful
		if (lookahead.code == SEOF_T) {
			return;
		}

		//now check for the attribute match having been a success
		switch (pr_token_code) {
		case LOG_OP_T:
		case REL_OP_T:
		case ART_OP_T:

			//for when token needed, is not matched with tokens attribute
			if (lookahead.attribute.get_int != pr_token_attribute) {
				syn_eh(pr_token_code);
				return;
			}
			break;
		case KW_T:

			if (lookahead.attribute.kwt_idx != pr_token_attribute) {
				syn_eh(pr_token_code);
				return;
			}
			break;
		}
			lookahead = malar_next_token();
			if (lookahead.code == ERR_T){
				//print the error and advance to next token
				syn_printe();
				lookahead = malar_next_token();
				//increment the error counter
				synerrno++;
				return;
			}
			}
}

/*
*Purpose: An error handling function, that implements a simple panic mode error recovery
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: syn_printe(), exit(), malar_next_token()
*Parameters: int sync_token_code
*Return Value: n/a
*Algorithm: -the function increments the error counter as soon as its called, then advances the input 
			token until it finds a matching token code, with one required by the parser
			-while advancing, the functoin can hit an SEOF and checks for it accordingly by calling exit()
*/
void syn_eh(int sync_token_code) {
	
	syn_printe();
	synerrno++;

	//advance the input token until it finds a token matching the one required by parser
	do {
		if(sync_token_code)
		lookahead = malar_next_token();
		
		//when function reaches end of the source file
		if (lookahead.code == SEOF_T) {
			
			//
			if (sync_token_code != SEOF_T) {
				exit(synerrno);
			}else{
				return;
			}
		}
	} while (lookahead.code != sync_token_code);
	lookahead = malar_next_token();
}

/*
*Purpose: Simply prints the error messages when syntax errors occur in the parsing process
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: printf(), b_mark()
*Parameters: none
*Return Value: n/a
*Algorithm: -based on the lookahead token, the function will report the line at which the semantic
			syntax erros are taking place
			-also prints the token code for which attribute the error is referring to
*/
void syn_printe() {

	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		b_mark(str_LTBL, t.attribute.str_offset);
		printf("%s\n", b_location(str_LTBL));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}

/*
*Purpose: Takes a string argument and prints it
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: none
*Parameters: char* str
*Return Value: n/a
*Algorithm:
*/
void gen_incode(char* str) {

	printf("%s\n", str);
}

/*
*Purpose: <program> -> PLATYPUS {<opt_statements>}
		  FIRST(<program>) = {KW_T(PLATYPUS)}
*/
void program(void){
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	optional_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*
*Purpose: <opt_statements> -> <statements> | ε
		  FIRST(<opt_statements>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), ε}
*/
void optional_statements(void) {

		switch (lookahead.code) {
		case AVID_T:
		case SVID_T: 
			statements();
			break;
		case KW_T:
			/* check for IF,WHILE,READ,WRITE and in statements_p()*/
			if (lookahead.attribute.get_int != PLATYPUS
				&& lookahead.attribute.get_int != ELSE
				&& lookahead.attribute.get_int != THEN
				&& lookahead.attribute.get_int != REPEAT
				&& lookahead.attribute.get_int != TRUE
				&& lookahead.attribute.get_int != FALSE)
				{
				statements();
				break;
			}
		default: /*empty string – optional statements*/;
			gen_incode("PLATY: Opt_statements parsed");
		}
	}

/*
*Purpose: <statements> -> <statement> | <statements> <statement>
		  <statements> -> <statement> <statements'>
		  FIRST(<statements>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)
*/
void statements(void) {

	//no case checks because no alternatives within production, FIRST set can't be used here
	statement();
	statements_prime();
}

/*
*Purpose: <statements'> -> <statement> <statements'> | e
		  FIRST(<statements'>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e}
*/
void statements_prime(void) {

	switch (lookahead.code) {

	case AVID_T:
	case SVID_T:
		statement();
		statements_prime();
		break;
	case KW_T:
		/* check for IF,WHILE,READ,WRITE and in statements_p()*/
		if (lookahead.attribute.get_int == IF //can use kwt_idx
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statements();
			statements_prime();
			break;
		}
	}
}

/*
*Purpose: <statement> -> <assignment statement> | <selection statement> | <iteration statement>
					| <input statement> | <output statement>
		  FIRST (<statement>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)}
*/
void statement(void) {
	
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		assignment_statement();
		break;
	case KW_T:
		//for any attribute been matched, call first of the specific statement
		switch (lookahead.attribute.get_int) {

		case IF: 
			selection_statement();
			break;
		case WHILE:
			iteration_statement();
			break;
		case READ:
			input_statement();
			break;
		case WRITE:
			output_statement();
			break;
			//incase no statement is possibly selected, that leaves no option for the codes
		}
	}
}

/*
*Purpose: <assignment statement> -> <assignment expression>;
		  FIRST(<assignment statement>) = FIRST(<assignment expression>)
*/
void assignment_statement(void) {

	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*
*Purpose: <assignment expession> -> AVID = <arithmetic expression> | SVID = <string expression>
		  FIRST(<assignment expression>) = {AVID_T, SVID_T}
*/
void assignment_expression(void) {

	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, EQ); //match with a relational operator
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		string_expression();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	}
}

/*
*Purpose: <selection statement> -> IF <pre-condition> (<conditional expression>)
					THEN {<opt_statements>} ELSE {<opt_statements>};
		  FIRST(<selection statement>) = {KW_T(IF)}
*/
void selection_statement(void) {
	
	//try to match statement individual tokens, and call appropriate production
	match(KW_T, IF);
	pre_condition();

	match(LPR_T, NO_ATTR);
	conditional_expression();

	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	optional_statements();

	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	optional_statements();

	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}

/*
*Purpose: <pre-condition> -> TRUE | FALSE
		  FIRST(<pre-condition>) = {KW_T(TRUE), KW_T(FALSE)}
*/
void pre_condition(void) {

	if (lookahead.code == KW_T) {
		
		switch (lookahead.attribute.get_int) {
		case TRUE:
		case FALSE:
			match(KW_T, lookahead.attribute.get_int);
			break;
		default:
			syn_printe();
		}
	}
	else {
		syn_printe();
	}
}

/*
*Purpose: <iteration statement> -> WHILE <pre-condition> (<conditional expression>)
					REPEAT {<statements>};
		  FIRST(<iteration statement>) = {KW_T(WHILE)}
*/
void iteration_statement(void) {

	match(KW_T, WHILE);
	pre_condition();

	match(LPR_T, NO_ATTR);
	conditional_expression();

	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();

	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);

	gen_incode("PLATY: Iteration statement parsed");
}

/*
*Purpose: <input statement> -> READ(<variable list>);
		  FIRST(<input statement>) = {KW_T(READ)}
*/
void input_statement(void) {

	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	variable_list();

	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/*
*Purpose: <variable list> -> <variable identifier> <variable list'>
		  FIRST(<variable list>) = {AVID_T, SVID_T}
*/
void variable_list(void) {

	variable_identifier();
	variable_list_prime();
	gen_incode("PLATY: Variable list parsed");
}

/*
*Purpose: <variable identifier> -> AVID_T | SVID_T
		  FIRST(<variable identifier>) = {AVID_T, SVID_T}
*/
void variable_identifier(void) {

	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
}

/*
*Purpose: <variable list'> -> ,<variable identifier> <variable list'> | e
		  FIRST(<variable list'>) = {COM_T, e}
*/
void variable_list_prime(void) {

	switch (lookahead.code) {
	case COM_T:
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_prime();
		break;
	default:
		return;
	}
}

/*
*Purpose: <output statement> -> WRITE(<opt_variable list>); | WRITE (STR_T);
	      <output statement> -> WRITE (<output_list>)
		  FIRST(<output statment>) = {KW_T(WRITE)}
*/
void output_statement(void) {

	match(KW_T, WRITE); 
	match(LPR_T, NO_ATTR);
	output_list(); 
	
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/*
*Purpose: <output_list> -> <variable list> | STR_T | e		  
		  FIRST(<output_list>) = { AVID_T, SVID_T, STR_T,  e}
*/
void output_list(void) {

	switch (lookahead.code) {

	case AVID_T: 
	case SVID_T: 
		variable_list(); 
		break;
	case STR_T: 
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed"); 
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed"); 
		break;
	}
}

/*
*Purpose: <arithmetic expression> -> <unary arithmetic expression> | <additive arithmetic expression>
		  FIRST(<arithmetic expression>) = {FIRST(unary arithmetic expression), FIRST(additive arithmetic expression)}
*/
void arithmetic_expression(void) {

	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
		case MINUS:
			unary_arithmetic_expression();
			break;
		}
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic_expression();
		break;
	default:
		syn_printe();
		break;
		}
	gen_incode("PLATY: Arithmetic expression parsed");
}

/*
*Purpose: <primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>)
		  FIRST(<primary arithmetic expression>) = {AVID_T, FPL_T, INL_T, LPR_T}
*/
void primary_arithmetic_expression(void) {

	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*
*Purpose: <unary arithmetic expression> -> -<primary arithmetic exception> 
						| +<primary arithmetic expression>
		  FIRST(<unary arithmetic expression>) = {-, +}
*/
void unary_arithmetic_expression(void) {

	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.get_int) {
		case PLUS:
		case MINUS:
			match(lookahead.code, lookahead.attribute.get_int);
			primary_arithmetic_expression();
			break;
		}
	}
	gen_incode("PLATY: Unary arithmetic expression parsed");
}

/*
*Purpose: <additive arithmetic expression> -> <additive arithmetic expression> +<multiplicative arithmetic expression>
					| <additive arithmetic expression> -<multiplicative arithmetic expression> | 
					<multiplicative arithmetic expression>
		  <additive arithmetic expression> -> <multiplicative arithmetic expression> <additive arithmetic expression'>
		  FIRST (<additive arithmetic expression>) = {AVID_T, FPL_T, INL_T, LPR_T}
*/
void additive_arithmetic_expression(void) {

	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_prime();
}

/*
*Purpose: <additive arithmetic expression’> -> + <multiplicative arithmetic expression> <additive arithmetic expression’>
 *					| - <multiplicative arithmetic expression> <additive arithmetic expression’>
 *					| e
		  FIRST(<additive arithmetic expression’>) = {+, -, e} 
*/
void additive_arithmetic_expression_prime(void) {

	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
		case MINUS:
			match(ART_OP_T, lookahead.attribute.arr_op);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_prime();
			gen_incode("PLATY: Additive arithmetic expression parsed");
		default:
			return;
		}
		break;
	}
}

/*
*Purpose: <multiplicative arithmetic expression> -> <multiplicative arithmetic expression> *
					<primary arithmetic expression> | <multiplicative arithmetic expression> /
					<primary arithmetic expression> | <primary arithmetic expression>
		  <multiplicative arithmetic expression> -> <primary expression> <multiplicative arithmetic expression'>
		  FIRST(<multiplicative arithmetic expression>) = {AVID_T, FPL_T, INL_T, LPR_T}
*/
void multiplicative_arithmetic_expression(void) {

	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_prime();
}

/*
*Purpose: <multiplicative arithmetic expression’> -> * <primary arithmetic expression> 
					<multiplicative arithmetic expression’>
  					| / <primary arithmetic expression> <multiplicative arithmetic expression’>
 					| e
		  FIRST(<multiplicative arithmetic expression’>) = {*, /, e}
*/
void multiplicative_arithmetic_expression_prime(void) {

	switch (lookahead.code ) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case MULT:
		case DIV:
			match(ART_OP_T, lookahead.attribute.arr_op);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_prime();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		}
		break;
	}
}

/*
*Purpose: <string expression> -> <primary string expression> | <string expression> << 
					<primary string expression>
		  <string expression> -> <primary string expression> <string expression'>
		  FIRST(<string expression>) = {SVID_T, STR_T}
*/
void string_expression(void) {

	primary_string_expression();
	string_expression_prime();
	gen_incode("PLATY: String expression parsed");
}

/*
*Purpose: <string expression’> -> << <primary string expression> <string expression’> | e
		  FIRST(<string expression’>) = {SCC_OP_T, e}
*/
void string_expression_prime(void) {

	switch (lookahead.code) {
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_prime();
		break;
	default:
		return;
	}
}

/*
*Purpose: <primary string expression> -> SVID_T | STR_T
		  FIRST(<primary string expression>) = {SVID_T, STR_T}
*/
void primary_string_expression(void) {

	switch (lookahead.code) {
	case SVID_T:
	case STR_T:
		match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary string expression parsed");
}

/*
*Purpose: <conditional expression> -> <logical OR expression>
		  FIRST(<conditional expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void conditional_expression(void) {

	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*
*Purpose: <logical OR expression> -> <logical AND expression> | <logical OR expression> 
					.OR. <logical AND expression>
		  <logical OR expression> -> <logical AND expression> <logical OR expression’>
		  FIRST(<conditional expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_OR_expression(void) {

	logical_AND_expression();
	logical_OR_expression_prime();
}

/*
*Purpose: <logical AND expression> -> <relational expression> | <logical AND expression> 
					.AND. <relational expression>
		  <logical AND expression> -> <logical AND expression> <logical OR expression’>
		  FIRST(<conditional expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_AND_expression(void) {

	relational_expression();
	logical_AND_expression_prime();
}

/*
*Purpose: <logical OR expression’> -> .OR. <logical AND expression> <logical OR expression’> | e
		  FIRST(<logical OR expression’>) = {.OR., e}
*/
void logical_OR_expression_prime(void) {

	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case OR:
			match(LOG_OP_T, OR);
			logical_AND_expression();
			logical_OR_expression_prime();
			gen_incode("PLATY: Logical OR expression parsed");
			break;
		}
		break;
	}
}

/*
*Purpose: <logical AND expression’> -> .AND. <relational expression> <logical AND expression’> | e
		  FIRST(<logical AND expression’>) = {.AND., e}
*/
void logical_AND_expression_prime(void) {

	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case AND:
			match(LOG_OP_T, AND);
			relational_expression();
			logical_AND_expression_prime();
			gen_incode("PLATY: Logical AND expression parsed");
			break;
		}
		break;
	}
}

/*
*Purpose: <relational expression>  -> <primary a_relational expression> <relational operator> 
					<primary a_relational expression> | <primary s_relational expression> 
					<relational operator> <primary s_relational expression>
		  FIRST(<relational expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void relational_expression(void) {

	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		primary_a_relational_expression();
		relational_operator();
		primary_a_relational_expression();
		break;
	case SVID_T:
	case STR_T:
		primary_s_relational_expression();
		relational_operator();
		primary_s_relational_expression();
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Relational expression parsed");
}

/*
*Purpose: <primary a_relational expression> -> AVID_T | FPL_T | INL_T
		  FIRST(<primary a_relational expression>) = {AVID_T, FPL_T, INL_T}
*/
void primary_a_relational_expression(void) {

	switch (lookahead.code) {
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary a_relational expression parsed");
}

/*
*Purpose:<primary s_relational expression> -> <primary string expression>
		 FIRST(<primary_s_relational expression>) = {SVID_T, STR_T}
*/
void primary_s_relational_expression(void) {

	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed");
}

/*
*Purpose: <relational operator> -> == | <> | > | <
		  FIRST(<relational operator>) = {==, <>, >, <}
*/
void relational_operator(void) {

	 // No parsing notice for relational_operator is printed in the test files
	match(REL_OP_T, lookahead.attribute.rel_op);
}
