#pragma once
/*************************************************************************************************************************************
*File name			: scanner.c
*Compiler			: MS Visual Studio 2019
*Author				: Saad Abdullah, 040877175
*Course				: CST 8152 - Compilers
*Lab Section        : 301
*Assignment			: 2
*Date Due      		: November 12 2019
*Professor			: Svillen Ranev
*Purpose			: To provide the structure of dataflow for our Scanner to process input.
*Function list		: aa_func02(), aa_func03(), aa_func08(), aa_func05(), aa_func10(), aa_func11(), aa_func12()
*************************************************************************************************************************************/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

 /*   Source end-of-file (SEOF) sentinel symbol
  *    '\0' or one of 255,0xFF,EOF
  */
#define SEOF 255
#define SEOF_2 EOF //-1

#define ES 11 /* Error state  with no retract */
#define ER 12 /* Error state  with retract */
#define IS -1 /* Inavalid state */

/* State transition table definition */
#define TABLE_COLUMNS 8
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/*              [L]   0  [1-9]   .     @    other    "    SEOF*/
	/* State 0 */  { 1,   6,   4,   ES,   ES,    ES,     9,    ES},		//Starting state
	/* State 1 */  { 1,   1,   1,    2,   3,      2,	 2,     2},		//Letter state
	/* State 2 */  { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},		//VID & AVID/KW finite state
	/* State 3 */  { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},		//VID/SVID accepting finite state
	/* State 4 */  { ES,  4,   4,    7,    5,     5,     5,     5},		//Non-zero digit state
	/* State 5 */  { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},		//IL/DIL accepting finite state
	/* State 6 */  { ES,  6,  ES,    7,   ES,     5,     5,     5},		//Zero digit state
	/* State 7 */  {  8,  7,   7,    8,    8,     8,     8,     8},		//Decimal state
	/* State 8 */  { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},		//FPL accepting finite state
	/* State 9 */  { 9,   9,   9,    9,    9,     9,    10,    ER},		//String literal state
	/* State 10 */ { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},		//SL accepting finite state
	/* State 11 */ { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS},		//Error accepting finite state
	/* State 12 */ { IS, IS,  IS,   IS,   IS,    IS,    IS,    IS}		//Error accepting finite state with retract
};

	/* Accepting state table definition */
	#define ASWR     14  /* accepting state with retract */
	#define ASNR     15  /* accepting state with no retract */
	#define NOAS     16  /* not accepting state */

	int as_table[] = {
	/* State 0 */ NOAS,
	/* State 1 */ NOAS,
	/* State 2 */ ASWR,
	/* State 3 */ ASNR, 
	/* State 4 */ NOAS,
	/* State 5 */ ASWR,
	/* State 6 */ NOAS,
	/* State 7 */ NOAS,
	/* State 8 */ ASWR,
	/* State 9 */ NOAS,
	/* State 10 */ ASNR,
	/* State 11 */ ASNR,
	/* State 12 */ ASWR
	};

/* Accepting action function declarations */
Token aa_func02(char* lexeme); 
Token aa_func03(char* lexeme); 
Token aa_func05(char* lexeme);
Token aa_func08(char* lexeme);
Token aa_func10(char* lexeme);
Token aa_func11(char* lexeme);
Token aa_func12(char* lexeme);

/* defining a new type: pointer to function (of one char * argument)
   returning Token
*/

typedef Token(*PTR_AAF)(char* lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[] = {

	/* state 0 */ NULL,
	/* state 1 */ NULL,
	/* state 2 */ aa_func02,
	/* state 3 */ aa_func03,
	/* state 4 */ NULL,
	/* state 5 */ aa_func05,
	/* state 6 */ NULL,
	/* state 7 */ NULL,
	/* state 8 */ aa_func08,
	/* state 9 */ NULL,
	/* state 10 */ aa_func10,
	/* state 11 */ aa_func11,
	/* state 12 */ aa_func12
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char* kw_table[] =
	{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"
	};

#endif