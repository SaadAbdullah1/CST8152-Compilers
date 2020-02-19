/*************************************************************************************************************************************
*File name			: scanner.c
*Compiler			: MS Visual Studio 2019
*Author				: Saad Abdullah, 040877175
*Course				: CST 8152 - Compilers
*Lab Section        : 301
*Assignment			: 2
*Date Due      		: November 12 2019
*Professor			: Svillen Ranev
*Purpose			: To write the functions that implement the front-end of a compilers Lexical Analyzer (Scanner)
*Function list		: scanner_init(), malar_next_token(), get_next_state(), char_class(), aa_func02(), aa_func03(),
					  aa_func08(), aa_func05(), aa_func10(), aa_func11(), aa_func12(), iskeyword()
*************************************************************************************************************************************/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

 /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char); /* state machine function */
static int iskeyword(char* kw_lexeme); /*keywords lookup functuion */


/*
*Purpose: Initializes the scanner
*Author: Svillen Ranev
*Version: 1.0
*Called Functions: b_isempty(), b_rewind(), b_clear
*Parameters: pBuffer psc_buf; pointer to buffer
*Return Value: int EXIT_SUCCESS, int EXIT_FAILURE
*Algorithm: (Is exactly as written)
*/
int scanner_init(pBuffer psc_buf) {
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*
*Purpose: To return an appropriate token for the next character in the buffer
*Author: Saad Abdullah & Svillen Ranev
*Version: 1.0
*Called Functions: b_getc(), isspace(), b_retract(), b_mark(), b_getcoffset(), b_reset(), b_free(),
				   isalnum(), get_next_state(), b_allocate(), strlen(), b_addc(), b_compact(),
*Parameters: void/NA
*Return Value: Token t; the appropriate tokens for recognized patterns assigned in token.h
*Algorithm: Gets the next character and perform token recognition by:
				-checking for whitespace symbols from the input stream, and continuing incrementing
				per line to check for all the special case tokens and EOF's if there are none.
				-For each special case, if no match is found, the character is sent to the finite automaton
				(states 2,3,5,8,10,11,12) to generate a lexeme.
				-If the character read is an SEOF, the while loop reading input is broken
				-The lexeme created is processed to see if a match is possible for any state, and if so sent
				to a specific accepting function for the appropriate token association
				-the buffer is retracted based on the state of the lexeme, and reread
				-returns an error token if any token isnt legal; and while loop is fully processed
*/
Token malar_next_token(void) {
	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
	int accept = NOAS; /* type of state - initially not accepting */
	char temp; /*temporary holding place for the 'c' input symbol*/
	/* endless loop broken by token returns it will generate a warning */
	while (1) 
	{
		//GET THE NEXT SYMBOL FROM THE INPUT BUFFER
		c = b_getc(sc_buf);

		/* Part 1: Implementation of token driven scanner */
		/* every token is possessed by its own dedicated code */

			//detects for white-space characters ('', \n, \t, \v, \f, \r) from the stream
		if (isspace(c)) {
			if (c == '\n') {
				line++; //when there is a line terminator, scan the next line
			}
			continue;
		}

		//process the special case tokens and end of file
		switch (c)
		{
		case '\0':
			t.attribute.seof = SEOF_0;
			t.code = SEOF_T;
			return t;
		case SEOF:
		case SEOF_2:
			t.attribute.seof = SEOF_2;
			t.code = SEOF_T;
			return t;
		case '(':
			t.code = LPR_T;
			return t;
		case ')':
			t.code = RPR_T;
			return t;
		case '{':
			t.code = LBR_T;
			return t;
		case '}':
			t.code = RBR_T;
			return t;
		case '<':
			temp = b_getc(sc_buf);
			//check if next token is different
			if (temp == '>') {
				t.attribute.rel_op = NE; //1, not equal
				t.code = REL_OP_T;
				return t;
			}//for the case of double <, catenate
			else if (temp == '<') {
				t.code = SCC_OP_T;
				return t;
			}
			//return the next character back to buffer
			b_retract(sc_buf);
			t.code = REL_OP_T;
			t.attribute.rel_op = LT; //3, less than
			return t;
		case '>':
			t.code = REL_OP_T;
			t.attribute.rel_op = GT; //2, greater than
			return t;
		case ';':
			t.code = EOS_T;
			return t;
		case ',':
			t.code = COM_T;
			return t;
		case '-':
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		case '+':
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		case '*':
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
		case '/':
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
		case '=':
			if (b_getc(sc_buf) == '=') { //if there is another =
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ; //0, equals
				return t;
			}
			b_retract(sc_buf);
			t.code = ASS_OP_T;
			return t;
		case '!': //a comment in the stream
			//check the next char to see if its a comment '!' or error token
			temp = c = b_getc(sc_buf);

			if (temp == SEOF_2 || temp == SEOF || temp == '\0') {
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = '\0';
				return t;
			}
			else if (temp != '!') {
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';

				//this skips printing a default blank line
				if (c != '\n') {
					t.attribute.err_lex[1] = c;
				}

				t.attribute.err_lex[2] = '\0';
			}
			//as long as the input symbol does not detect a new line
			while (c != '\n')
			{
				c = b_getc(sc_buf);

				//check if char hits any of the line terminators
				if (c == SEOF_2 || c == SEOF || c == '\0')
				{
					t.code = SEOF_T;
					return t;
				}
			}
			line++;
			//if and when the next input is not a comment ""
			if (temp != '!') {
				return t; //return the token
			}
			/* continue to check next line */
			continue;
		case '.':
			/* set our mark to the first char after . incase we find an error and need to retract */
			b_mark(sc_buf, b_getcoffset(sc_buf));
			c = b_getc(sc_buf);

			//see if the first and following character is an 'AND.' or 'OR.'
			if (c == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.') {
				t.code = LOG_OP_T;
				//assing the attribute approriate to either case
				t.attribute.log_op = AND;
				return t;
			}
			else if (c == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.')
			{
				t.code = LOG_OP_T;
				t.attribute.log_op = OR;
				return t;
			}
			//if its neither case of the dot operand, set to error state
			else
				b_reset(sc_buf);
				t.code = ERR_T;
				t.attribute.err_lex[0] = '.';
				t.attribute.err_lex[1] = '\0';
				return t;

		default:

			//have to check if c is alphanumeric (illegal), or if its a string literal open quotation (") (proper token)
			if (isalnum((int)c) != 0 || c == '"' || c == '\0') {

				//set the mark at the beginning of the lexeme and save it in lexstart
				lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1);
				//get the initial state from the transition table
				state = get_next_state(state, c);

				//goes through each state to see when lexeme is not being accpeted
				while (accept == NOAS) {

					c = b_getc(sc_buf);
					//gets the next state if so, and repeats FSM1
					state = get_next_state(state, c);
					accept = as_table[state];
				}
				//see if the accepting state is retracting
				if (accept == ASWR) {

					b_retract(sc_buf); //retract the last character
				}
				//must set lexend to an offset of using this buffer function
				lexend = b_getcoffset(sc_buf);

				//create a temporary lexeme buffer in fix mode
				lex_buf = b_allocate(lexend - lexstart, 0, 'f');
				//make sure no run time error occurs due to false allocation
				if (!lex_buf) {

					t.code = ERR_T; //set token to error
					scerrnum = 1;
					//when the error occurs, define error statement for the lexeme and set token attribute to err_lex
					for (int i = 0; i < (int)strlen("RUN TIME ERROR: "); i++) {
						t.attribute.err_lex[i] = "RUN TIME ERROR: "[i];
					}
					return t;
				}
				//set the buffer offset back to lexstart
				b_reset(sc_buf);

				//copy the lexeme between lexstart and lexend from the input buffer into lex_buf
				for (int i = lexstart; i < lexend; i++) {

					b_addc(lex_buf, b_getc(sc_buf));
				}
				b_compact(lex_buf, '\0');

				//when VID recognized, call the accepting function using the aa_table with array index state
				t = aa_table[state](b_location(lex_buf));

				//free the lex_buf memory allocation
				b_free(lex_buf);
				return t;
			}
			break;
		}
			//when illegal symbol is detected, set an error to the token
			t.code = ERR_T;
			t.attribute.err_lex[0] = c;
			t.attribute.err_lex[1] = '\0';

			return t;
	}//end while(1)
}


/* Part 2: Implementation of Finite State Machine (DFA)
		   or Transition Table driven Scanner
		   Note: Part 2 must follow Part 1 to catch the illegal symbols
*/

/*
*Purpose: Selects the next state in accordance to the state table and where the current symbol is amongst the columns
*Author: Svillen Ranev
*Version: 1.0
*Called Functions: char_class(), assert()
*Parameters: int state; state of the current finite automata, char c; the symbol currently being processed
*Return Value: int next; the next state in the automation
*Algorithm: (Is exactly as written)
*/
int get_next_state(int state, char c)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);

#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	return next;
}

/*
*Purpose: Returns the column number in the transition table for the input character
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: isdigit()
*Parameters: char c; the character symbol
*Return Value: int val; the specific column of the transition table 
*Algorithm: Checks each column of the st_table array for their assigned values for the given
			specification marker of it. When a column reads alphanumeric values, its value
			is assigned accordingly as opposed to when a column check is for a SEOF or other.
*/
int char_class(char c)
{
	int val;

	//column 1, a-z 
	if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
		val = 0;
	}
	//column 2, 0
	else if (c == '0') {
		val = 1;
	}
	//column 3, 1-9
	else if (isdigit(c) && c != '0') {
		val = 2;
	}
	//column 4, .
	else if (c == '.') {
		val = 3;
	}
	//column 5, @
	else if (c == '@') {
		val = 4;
	}
	//column 7, "
	else if (c == '"') {
		val = 6;
	}
	//column 8, SEOF or \0
	else if (c == SEOF_2 || c == SEOF || c == '\0') {
		val = 7;
	}
	//column 6, other
	else {
		val = 5;
	}

	return val;
}

/*
*Purpose: To perform as an accepting function for arithmetic variable identifiers and keywords (VID- AVID/KW)
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: strlen(), strcpy()
*Parameters: char lexeme[]; the lexeme array of input symbol characters to process
*Return Value: Token t; the token generated for the lexemes
*Algorithm: Recognizes the token in play by:	
				-checking if the lexeme passed is a keyword or not, and if so then returns a token
				-for when its not a keyword from the table at all, the token is set to a variable identifier
				-stores the first lexeme characters into the VID attribute in the case that lexeme size is
				greater than variable identifier token length
				-copies the entire lexeme into VID attribute if lexeme size is shorter/equal to VID_LEN
				-returns the generated token
*/
Token aa_func02(char lexeme[]) {

	Token t = { 0 }; /*Token to be returned*/
	int i = 0;

	//check if the current lexeme is a keyword from the keyword lookup table
	if ((t.attribute.kwt_idx = iskeyword(lexeme)) != RT_FAIL_1) {
		t.code = KW_T;
		//if so, return t and 
		return t;
	}
	//if its not a keyword
	else {
		//set a variable identifier token
		t.code = AVID_T;
	}
	//if the lexeme is longer than VID_LEN characters
	if (strlen(lexeme) > VID_LEN) {

		for (i = 0; i < VID_LEN; i++) {
			//only first VID_LEN characters is stored into var attribute array
			t.attribute.vid_lex[i] = lexeme[i];
		}
		//add \0 at the end to make a c-type string
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	//check for when lexeme is shorter than or equal to VID_LEN
	else {
		//and store the entire lexeme into the attribute array
		strcpy(t.attribute.vid_lex, lexeme);
	}

	return t;
}

/*
*Purpose: To perform as an accepting function for the string variable identifiers(VID - SVID)
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: strlen(), strncpy()
*Parameters: char lexeme[]; the lexeme array of input symbol characters to process
*Return Value: Token t; the token generated for the lexemes
*Algorithm: Recognizes the tokens by:
				-intitially setting the token to a string variable identifier
				-performing a validation check for the SVID by seeing if lexeme size is greater
				or lesser than VID_LEN, and then storing the lexemes first -1 characters
				-copies the lexeme content into VID lexeme attribute of the token
				-return the generated token
*/
Token aa_func03(char lexeme[]) {

	Token t = { 0 }; /*Token to be returned*/
	int i = 0;

	//set an SVID token
	t.code = SVID_T;

	//if the lexeme is longer than VID_LEN
	if (strlen(lexeme) > VID_LEN) {

		for (i = 0; i < VID_LEN - 1; i++) {
			//only first VID_LEN -1 characters stored into var attribute array
			t.attribute.vid_lex[i] = lexeme[i];
		}
		//append the @ character to the name
		t.attribute.vid_lex[VID_LEN - 1] = '@';
		//add \0 at the end to make a c-type string
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	//check for when lexeme is shorter than or equal to VID_LEN
	else {
		//the entire lexeme is copied into the variable identifier attribute
		strncpy(t.attribute.vid_lex, lexeme, strlen(lexeme));
		t.attribute.vid_lex[strlen(lexeme)] = '\0';
	}

	return t;
}

/*
*Purpose: To perform as an accepting function for the floating-point literals (FPL)
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: atof(), strlen()
*Parameters: char lexeme[]; the lexeme array of input symbol characters to process
*Return Value: Token t; the token generated for the lexemes
*Algorithm: Recognizes the tokens by:
				-assigning a temporary float casted value to the lexeme, to handle 
				for overflow issues of large numbers
				-if the temp value is out of range, the token goes to the error state 
				of aa_table and returns itself accordingly
				-three dots are appended to the end of the err_lex after the first 3 
				characters are stored in the error lexeme
				-if there is a valid floating point literal, the numbers are attributed 
				into the flt_value lexeme  of the token
				-return the generated token
*/
Token aa_func08(char lexeme[]) {

	Token t = { 0 }; /*Token to be returned*/
	int i;
	double tempVal = atof(lexeme); //lexeme converted to double to hold floating point value

	//check if value is within same range as value of 4-byte float
	if (tempVal > FLT_MAX || (tempVal > 0 && tempVal < FLT_MIN)) {

		//when number poses overflow possiblity (too large), go to error state and return token
		t = aa_table[ES](lexeme);
		return t;
	}
	//when error lexeme is longer than ERR_LEN
	else if (strlen(lexeme) > ERR_LEN) {
		//first 3 characters are stored in err_lex
		for (i = 0; i < ERR_LEN - 3; i++) {
			t.attribute.err_lex[i] = lexeme[i];
			//three dots added to end of err_lex c-type string
			t.attribute.err_lex[ERR_LEN - 3] = '.';
			t.attribute.err_lex[ERR_LEN - 2] = '.';
			t.attribute.err_lex[ERR_LEN - 1] = '.';
		}
	}

	//set return token to floating point literal
	t.code = FPL_T;
	t.attribute.flt_value = (float)tempVal; //assign flt attribute, cast to float for catenation
	return t;
}

/*
*Purpose: To perform as an accepting function for the integer literals (IL - decimal constant(DIL))
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: atol(), aa_func12(), strlen(), strncpy(), strcpy()
*Parameters: char lexeme[]; the lexeme array of input symbol characters to process
*Return Value: Token t; the token generated for the lexemes
*Algorithm: Recognize the tokens by:
				-assigning a temporary long casted value to the lexeme, to handle 
				for overflow issues of large numbers
				-confirm that the token attribute assigned to lexeme of vid_lex is not
				valid, and assoicate an error value for it in order to then call the aa table
				for a RUN time error message.
				-make sure the lexeme conforms to value range of that of a 2 byte integer in c language
				and set the return token to INL_T, with the temporary value attributed to the lexeme accordingly
				-for the case the lexeme is out of bonunds, an error token is assigend with 
				3 dots aswell being appened individually to the end of the err_lex ctype string
				-otherwise copy the lexeme into the error token attribute
				-return the generated token
*/
Token aa_func05(char lexeme[]) {

	Token t = { 0 }; /*Token to be returned*/

	//convert lexeme to a decimal long int value
	long tempVal = atol(lexeme);

	if (!t.attribute.vid_lex) {

		scerrnum = 5;//value of run time error
		aa_func12("RUN TIME ERROR!");
	}
	//check if lexeme within range of the value 2 byte integer in c
	if (tempVal >= 0 && tempVal <= SHRT_MAX) {

		//set return token to integer literal
		t.code = INL_T;
		t.attribute.int_value = tempVal;
	}
	//when it is longer than than err_len
	else {
		t.code = ERR_T;
		if (strlen(lexeme) > ERR_LEN) {
			//first 3 characters are stored in err_lex
			strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);
			//three dots added to end of err_lex c-type string
			t.attribute.err_lex[ERR_LEN - 3] = '.';
			t.attribute.err_lex[ERR_LEN - 2] = '.';
			t.attribute.err_lex[ERR_LEN - 1] = '.';
		}
		//if the lexeme is equal to or even less than the ERR_LEN
		else {
			strcpy(t.attribute.err_lex, lexeme);
		}
	}
	return t;
}

/*
*Purpose: To perform as an accepting function for the string literal(SL)
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: b_limit(), strlen(), b_addc(),
*Parameters: char lexeme[]; the lexeme array of input symbol characters to process
*Return Value: Token t; the token generated for the lexemes
*Algorithm: Recognizes the token by:
				-setting the attribute of the string offset to the location where 
				the first char of lexeme content will be added to buffer
				-during the copying process the quotations first and second are ignored
				-stores the char lexeme into the string literal table and also checks
				for new line terminator to then increment the stream
				-adds 0 to end of the lexeme string by using the buffer addc function
				-returning the generated token after assigning STR_T to the string token
*/
Token aa_func10(char lexeme[]) {

	Token t = { 0 }; /*Token to be returned*/
	int i = 0;

	t.attribute.str_offset = b_limit(str_LTBL);

	//copy the lexeme content into str_ltbl
	for (i = 0; i < (int)strlen(lexeme); i++) {

		//makes sure to ignore " during copy
		if (lexeme[i] != '"') {
			b_addc(str_LTBL, lexeme[i]);
		}
		//if lexeme contains line terminators, increment it
		if (lexeme[i] == '\n') {
			line++;
		}
	}
	//add \0 to the end of the string
	b_addc(str_LTBL, '\0');

	t.code = STR_T;
	return t;

}

/*
*Purpose: To perform as an accepting function for the error token
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: strlen(), strcpy(), strncpy()
*Parameters: char lexeme[]; the lexeme array of input symbol characters to process
*Return Value: Token t; the token generated for the lexemes
*Algorithm: Recognizes the token by:
				-storing the erratic lexeme into the err_lex attribute of the token
				-checking the lenght of the int casted lexeme, to store the first VID_LEN - 3
				characters into err_lex[] if its longer than Err_len characters
				-for when the lexeme is too large, three dots are appended to the end of 
				err_lex as indication to the output
				-confirming there are no line terminations, however in the case they are 
				detected the line increments so long as the counter is less in size then the lexeme
				-assign an error token accordingly and return the generated token
*/
Token aa_func11(char lexeme[]) {

	Token t = { 0 }; /*Token to be returned*/
	int i = 0;

	//if error lexeme is less than or same in size to first 3 err_len chars 
	if ((int)strlen(lexeme) <= ERR_LEN - 3) {

		//atrribute of the error token content is stored in error lexeme
		strcpy(t.attribute.err_lex, lexeme);
		t.attribute.err_lex[ERR_LEN] = '\0';
	}
	//when the lexeme is longer
	else {
		strncpy(t.attribute.err_lex, lexeme, ERR_LEN - 3);
		//three dots added to end of err_lex c-type string
		t.attribute.err_lex[ERR_LEN - 3] = '.';
		t.attribute.err_lex[ERR_LEN - 2] = '.';
		t.attribute.err_lex[ERR_LEN - 1] = '.';
	}

	//for when lexeme contains line terminators, increment line counter
	for (i = 0; i < (int)strlen(lexeme); i++)
	{
		if (lexeme[i] == '\n')
		{
			line++;
		}
	}

	//set the error token
	t.code = ERR_T;
	return t;
}

/*
*Purpose: To perform as an accepting function that retracts for error tokens (state 11)
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: None
*Parameters: char lexeme[]; the lexeme array of input symbol characters to process
*Return Value: Token t; the token generated for the lexemes
*Algorithm: Recognizes tokens generated by accepting function 11 and returns them
*/
Token aa_func12(char lexeme[]) {

	Token t = { 0 }; /*Token to be returned*/

	//set the token to the aa_table to get the error token
	t = aa_table[ES](lexeme);

	return t;
}

/*
*Purpose: The keywords look up function
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: strcmp()
*Parameters: char* kw_lexeme; the lexeme being checked
*Return Value: int i; a counter for the index of the matched keyword in kwtable
*Algorithm: Recognizes the keyword by:
				-using a counter of type int, for a match check of the lexeme with a 
				keyword in the keyword table
				-will simply return the keyword index when a match is found
				-returns -1 for a failed match
*/
int iskeyword(char* kw_lexeme) {

	int i = 0;

	//checks if there is a match of the keyword lexeme and the kw_table by iterating through KWT_SIZE
	for (i; i < KWT_SIZE; i++) {

		if (strcmp(kw_table[i], kw_lexeme) == 0) {

			return i;//return the index if matched
		}
	}
	//else return fail value if no keyword matched
	return -1;
}
