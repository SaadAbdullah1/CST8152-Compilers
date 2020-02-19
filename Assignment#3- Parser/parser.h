/*************************************************************************************************************************************
File name			: parser.h
Compiler			: MS Visual Studio 2019
Author				: Saad Abdullah, 040877175
Course				: CST 8152 - Compilers
Lab Section         : 301
Assignment			: 3 (Parser)
Date Due      		: December 05 2019
Professor			: Svillen Ranev
Purpose				: 
Function list		: 
/************************************************************************************************************************************/
#include <stdlib.h>

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef BUFFER_H_
#include "buffer.h"
#endif


//global variable declarations
static Token lookahead;
extern Token malar_next_token();
int synerrno;
extern int line;			/*the current line number we are parsing*/
extern pBuffer str_LTBL;	/*the string literal table*/
extern char* kw_table[];	/*the keyword table*/

//constant definitions
#define NO_ATTR -1 /* Code for no token attribute */
#define ELSE 0
#define FALSE 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

//function prototypes
void parser(void);
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe();
void gen_incode(char* str);
void program(void);
void optional_statements(void);
void statement(void);
void statements_prime(void);
void statements(void);
void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void pre_condition(void);
void iteration_statement(void);
void input_statement(void);
void variable_list(void);
void variable_identifier(void);
void variable_list(void);
void variable_list_prime(void);
void output_statement(void);
void output_list(void);
void arithmetic_expression(void);
void primary_arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_prime(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_prime(void);
void string_expression(void);
void string_expression_prime(void);
void primary_string_expression(void);
void conditional_expression(void);
void logical_OR_expression(void);
void logical_AND_expression(void);
void logical_OR_expression_prime(void);
void logical_AND_expression_prime(void);
void relational_expression(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);
void relational_operator(void);
