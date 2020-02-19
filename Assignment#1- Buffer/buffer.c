/*************************************************************************************************************************************
*File name			: buffer.c
*Compiler			: MS Visual Studio 2019
*Author				: Saad Abdullah, 040877175
*Course				: CST 8152 - Compilers
*Lab Section        : 301
*Assignment			: 1
*Date Due      		: October 02 2019
*Professor			: Svillen Ranev
*Purpose			: Programming and Using Dynamic Structures (buffers) with C
*Function list		: b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_limit(), b_capacity(), b_mark()
*					  b_mode(), b_incfactor(), b_load(), b_isempty(), b_getc(), b_eob(), b_print(), b_compact()
*					  b_rflag(), b_retract(), b_reset(), b_getcoffset(), b_rewind(), b_location()
*************************************************************************************************************************************/

#include "buffer.h"

/*
*Purpose: Creates a new buffer on the programs heap memory. Also sets the buffers operational mode, initial capacity and increment factor.
*Author:  Saad Abdullah
*Version: 1.0
*Called Functions: calloc(), malloc(), free() 
*Parameters: short init_capacity, char inc_factor, char o_mode
*Return Value: buffer (is NULL; returns if buffer was not created)
*Algorithm: -the function tries to allocate memory for one buffer structure using calloc()
			-tries to allocate memory for a dynamic char buffer using a malloc() call with the given initial capacity(init_capacity)
			-makes sure the initial_capacity is within a range of 0-(SHRTMAX-1), and if its not, then simply return NULL after releasing the memory taken up by the buffer descriptor
			-creates a char buffer with a default size of 200 using predefined DEFAULT_INIT_CAPACITY after checking wether the init_capacity is 0.
				-sets the structures increment factor based on the operative mode
				-for both mode being a or m, the inc_factor is defaulted to 15
					-otherwise, for f mode it becomes 0 as the buffer cannot be incremented
			-the malloc() pointer is returned and assigned to cb_head
			-based on what operative mode is being run and its corresponding increment factor, set the values of each in the buffer strucuture
				-for the fixed mode, check wether o_mode is f OR the inc_factor is 0 as well as init_capacity
				-for the additive mode, check wether o_mode is a, if so, make sure the inc_factor is between 1-255 bytes(unsigned char)
				-for the multiplicative mode, check wether o_mode is m, and if it is, the range of inc_factor must be between 1-100 bytes
			-copies/sets the given init_capacity into the buffer structure 'capacity' variable
			-sets the 'flags' field to its actual default hex value of FFFC
			-return a pointer to the buffer descriptor
*/
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode) {

	/*dynamically allocate memory for one buffer descriptor struct*/
	pBuffer buffer = (Buffer*)calloc(1, sizeof(Buffer));


	/*allocate memory for one dynamic char buffer/array*/
	if (0 <= init_capacity && init_capacity<= MAXIMUM_ALLOWED_POSITIVE_VALUE) {

		if (buffer) {
			buffer->cb_head = malloc(init_capacity * sizeof(char));

			if (!buffer->cb_head) {
				return NULL;
			}
		}
		/*create char buffer with default size 200*/
		if (init_capacity == 0) {
			init_capacity = (short)DEFAULT_INIT_CAPACITY;

			/*set buffer struct increment in modes*/
			if (o_mode == 'a' || o_mode == 'm') {
				if (buffer) {
					buffer->inc_factor = DEFAULT_INC_FACTOR;
				}
			}
			else if (o_mode == 'f') {
				if(buffer)
				buffer->inc_factor = 0;
			}
		}

		/*malloc pointer returned and assigned*/
		if (buffer) {
			buffer->cb_head = (char*)malloc(init_capacity * sizeof(char));
		}
	}
	else if (0 > init_capacity && init_capacity > MAXIMUM_ALLOWED_POSITIVE_VALUE) {
		free(buffer);
		return NULL;
	}

	if (buffer) { /*a reference check for the buffer pointer*/
		/*set buffer structure mode and increment*/
		if (o_mode == 'f') {
				buffer->mode = buffer->inc_factor = 0;
				if (init_capacity != 0) {
					buffer->capacity = DEFAULT_INIT_CAPACITY;
				}
			}

		if (o_mode == 'a') {
			buffer->mode = 1;
			if ((inc_factor >= 1 && inc_factor <= 255)) {
				buffer->inc_factor = inc_factor;
			}
		}

		if (o_mode == 'm') {
			buffer->mode = -1;
			if (1 <= inc_factor && inc_factor <= 100) {
				buffer->inc_factor = inc_factor;
			}
		}
		
		/*copy given init_capacity into Buffer struct capacity*/
		buffer->capacity = init_capacity;

		/*set flags to default*/
		buffer->flags = DEFAULT_FLAGS;
	}
	return buffer;
}

/*
*Purpose: Tries to add the parameter passed character 'symbol' to the buffer. Based on how full the buffer is and its operational mode, it resizes the buffer using realloc().
			Whenever the b_addc fails to reallocate the buffer capacity or faces run-time errors, it will return NULL. 
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: realloc()
*Parameters: pBuffer const pBD, char symbol
*Return Value: pBD (a pointer to the buffer structure)
*Algorithm: -resets the buffers r_flags bit to default value of 0. 
			-Makes sure the buffer has capacity to add a new character (symbol)
				-if so; adds it to the buffer head (cb_head) and increments addc_offset by 1 then returns the buffer pointer
			-for when the character buffer reaches max capacity, it must be resized based on the buffer operative mode
				-when the mode is fixed(0), can't increment anyways, so just returns NULL
				-when the mode is additive(1), tries to increase current capacity of the buffer
					-the buffer inc_factor is converted into bytes and then added to the buffers capacity to form a 'new capacity'
					-make sure the capacity is lower than the MAX_ALLOWED_POSITIVE_VALUE, but also not negative
						-if it is negative, simply return NULL
				-when the mode is multiplicative(-1), it will check the capacity to see if there is more space to increment in the buffer, and return NULL if there isnt
					-when there is space to increment; it will use a formula for checking the available space
						-a new increment value is calculated using a formula aswell, (new inc = available space *inc_factor /100)
					-the new_capacity is equal to the current buffer capacity + the new increment factor
						-if post-calculation it becomes greater than the MAX_ALLOWED_POSITIVE_VALUE, the new_capacity will become the new MAX buffer capacity
			-checks for wether capacity was incremented succesfully in a or m mode and then updates that to the buffers capacity
				-tries to expand the buffer using realloc() with the buffers newly updated capacity, and if that fails return NULL
			-makes sure to set flag bits to 1 with bitwise set operation, if the buffer was changed by the realloc
			-adds the symbol to the buffer cb_head as long as it is not at max capacity and if it is, return NULL
			-the function returns a pointer to the buffer structure
*/
pBuffer b_addc(pBuffer const pBD, char symbol) {
	short new_capacity = 0;
	short available_space, new_increment;
	char * temp_var = NULL;

	/*reset the r_flag bit to 0*/
	pBD->flags &= RESET_R_FLAG;

	/*checks if the buffer is not full*/
	if (pBD->addc_offset < pBD->capacity) {
		/*adds a new character symbol to head and offsets it*/
		pBD->cb_head [pBD->addc_offset] = symbol;
		++pBD->addc_offset;
		return pBD; 
	}

	/*when buffer is full, resize it by increasing current capacity*/
	if (pBD->addc_offset == pBD->capacity) {

		/*capacity increase based on mode*/
		if (pBD->mode == FIXED) {
			return NULL;
		}
		
		if (pBD->mode == ADDITIVE) {
			/*convert inc_factor to bytes, then add to capacity*/
			(short)((sizeof(char)) * (pBD->inc_factor));
			new_capacity = pBD->inc_factor + pBD->capacity;
	
		}
			/*checks positivity if exceeded SHRT_MAX-1 positive value, and assigns to newcapacity*/
			if ((0 < new_capacity) && (new_capacity > (MAXIMUM_ALLOWED_POSITIVE_VALUE))) {
				new_capacity = (MAXIMUM_ALLOWED_POSITIVE_VALUE);
			}
			else if (new_capacity < 0) {
				return NULL;
			}
			
		if (pBD->mode == MULTIPLICATIVE) { 
			/*if buffer is full, it cannot be incremented, thus return null*/
			if (pBD->capacity == (MAXIMUM_ALLOWED_POSITIVE_VALUE)) {
				return NULL;
			}
			/*formulae to calculate the increase in buffers capacity*/
			available_space = (MAXIMUM_ALLOWED_POSITIVE_VALUE) - pBD->capacity;
			new_increment = (available_space * pBD->inc_factor) / 100;
			new_capacity = pBD->capacity + new_increment;
			/*if capacity cannot be incremented, but current capacity smaller than buffer max, assign max to new capacity*/
			if(new_capacity > (MAXIMUM_ALLOWED_POSITIVE_VALUE)) {
				new_capacity = (MAXIMUM_ALLOWED_POSITIVE_VALUE);
			}
		}

	}

	/*if the capacity increment in Addititive or Multiplicative mode work, update capacity*/
	pBD->capacity = new_capacity;

	/*must reallocate memory for new capacity when trying to expand/resize character buffer*/
	temp_var = (char *) realloc(pBD->cb_head, pBD->capacity);
	if (!temp_var) { /*for if reallocation of the buffer fails*/
		return NULL;
	}
	/*using a temporary buffer, check if !memory has been changed and set flag bit to 1*/
	if (temp_var != pBD->cb_head) {
		pBD->flags |= SET_R_FLAG;
		pBD->cb_head = temp_var;
	}

	/*append character symbol to buffer head, if buffer not full*/
	if (pBD->addc_offset < pBD->capacity) {
		pBD->cb_head [pBD->addc_offset] = symbol;
		pBD->addc_offset++;
	}
	else {
		return NULL;
	}
	return pBD;
}

/*
*Purpose: To re-initialize (clear) specific data members of the given buffer descriptor, such that the buffer appears empty.
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: None
*Parameters: Buffer* const pBD	
*Return Value: 0 (assuming the function runs without error), -1(RT_FAIL_1) (when the function faces a run time error)
*Algorithm: (is exactly as written)
*/
int b_clear(Buffer* const pBD) {

	/*if the buffer is valid, reinitialize data member values of Buffer to 0 and flags to default*/
	if (pBD) {
		pBD->addc_offset = 0;
		pBD->getc_offset = 0;
		pBD->markc_offset = 0;

		pBD->flags &= DEFAULT_FLAGS;
	}
	else {
		return RT_FAIL_1;
	}
	return 0;
}

/*
*Purpose: de-allocates/frees the memory occupied by the character buffer and the buffer descriptor
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: free()
*Parameters: Buffer* const pBD
*Return Value: None(is type void)
*Algorithm: (is exactly as written)
*/
void b_free(Buffer* const pBD) {
	/*free memory allocated by the char buffer and structure*/
	if (pBD) {
		if (pBD->cb_head) {
			free(pBD->cb_head);
		}
		free(pBD);
	}
}

/*
*Purpose: checks and returns wether the buffer is full or not
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: None
*Parameters: Buffer* const pBD
*Return Value: int [-1 (buffer is NULL), 1(buffer is full), 0 otherwise]
*Algorithm: (is exactly as written)
*/
int b_isfull(Buffer* const pBD) {
	/*returns -1 if buffer crashes (is NULL)*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	/*check if buffer is full and return the mode associated*/
	if (pBD->addc_offset == pBD->capacity) {
		return (pBD->mode = ADDITIVE);
	}
	else {
		return (pBD->mode = FIXED);
	}
}

/*
*Purpose: checks and returns the current limit of the character buffer
			-the current limit = space (in chars) being used by all stored characters (addc_offset)
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: None
*Parameters: Buffer* const pBD
*Return Value: short [-1(run time error)]
*Algorithm: (is exactly as written)
*/
short b_limit(Buffer* const pBD) {
	/*if buffer is not valid, face a run time error and return -1*/
	if (!pBD) {
		return RT_FAIL_1;
	}
	return pBD->addc_offset;
}

/*
*Purpose: Returns the 'current' capacity of the character Buffer structure
*Author: Saad Abdullah		
*Version: 1.0
*Called Functions: None
*Parameters: Buffer* const pBD
*Return Value: short [-1 (run time error)]
*Algorithm: (is exactly as written)
*/
short b_capacity(Buffer* const pBD) {
	
	if (pBD) {
		return pBD->capacity;
	}
	else {
		return RT_FAIL_1;
	}
}

/*
*Purpose: sets markc_offset to mark; and checks if the mark parameter is within b_limit, and then returns markc_offset
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: None
*Parameters: pBuffer const pBD, short mark
*Return Value: short [-1(run time error)]
*Algorithm: (is exactly as written)
*/
short b_mark(pBuffer const pBD, short mark) {
	/*if buffer is not valid, return -1*/
	if (!pBD) {
		return RT_FAIL_1;
	} 
	/*set markc_offset to mark when it stays within current buffer limit*/
	else if (mark >= 0 && mark <= pBD->addc_offset) {
		pBD->markc_offset = mark;
	}
	return pBD->markc_offset;
}

/*
*Purpose: returns the value of the buffer operative mode
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: None
*Parameters: Buffer* const pBD
*Return Value: int [-1 (run time error) ]
*Algorithm: (is exactly as written)
*/
int b_mode(Buffer* const pBD) {
	/* if buffer is valid, the value of mode is returned to calling function, else fail*/
	if (pBD) {
		return pBD->mode;
	}

	return RT_FAIL_1;
}

/*
*Purpose: returns the non-negative value of inc_factor
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: None
*Parameters: Buffer* const pBD
*Return Value: an unsigned char casted value of buffers inc_factor, or 256(0x100)
*Algorithm: (is exactly as written)
*/
size_t b_incfactor(Buffer* const pBD) {
	/*non-negative value of incfactor returned to calling function*/
	if (pBD) {
		return (unsigned char) pBD->inc_factor;
	}
		return 0x100;
}

/*
*Purpose: reads an open input file and loads it into the character buffer
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: fgetc(), b_addc(), feof(), ungetc()
*Parameters: FILE* const fi, Buffer* const pBD
*Return Value: int [-1(run time error), -2(LOAD_FAIL), count]
*Algorithm: -reads an open input file specified by 'fi' into the buffer.
			-a while statement allows characters to be read so long as buffer end is not reached
				-uses the standard function fgetc to read characters one at a time
					-takes and stores an indvidual character into a char variable 
				-check character cannot be added, uses ungetc to return it to fi
			-keep incrementing the count variable to maintain while loop
			-return the number of characters added to the buffer
*/
int b_load(FILE* const fi, Buffer* const pBD) {

	char character;
	int count = 0; //
	/*if file stream or buffer itself is not valid, return a fail (-1)*/
	if (!fi || !pBD) {
		return RT_FAIL_1;
	}

	/*if its not the end of file, keep reading*/
	while (!feof(fi)) {
		character = (char)fgetc(fi);
		if (feof(fi)) {
			ungetc(character, fi);
			break;
		}
	/*if theres no more space in buffer return it to file stream*/
	if (!b_addc(pBD, character)) {
		ungetc(character, fi);
		return LOAD_FAIL;
	}
	count++;
}
	return count;
}

/*
*Purpose: checks and reports wether or not the buffer is empty
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: None
*Parameters: Buffer* const pBD
*Return Value: int [-1(run time error), 1(buffer is empty, 0(buffer full)]
*Algorithm: (is exactly as written)
*/
int b_isempty(Buffer* const pBD) {
	/*if buffer is valid, checks wether addc_offset is 0, else return -1*/
	if (pBD) {
		if (pBD->addc_offset == 0) {
			return 1;
		}
		else {
			return 0;
		}
	}
	return RT_FAIL_1;
}

/*
*Purpose: reads the buffer while checking for its validity in ceratain conditions
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: None
*Parameters: Buffer* const pBD
*Return Value: char[-2 (run time error), 0(eob is set to 1)]
*Algorithm: (is exactly as written)b
*/
char b_getc(Buffer* const pBD) {
	
	//holds the current value of characters at getc_offset
	char temp;

	/*reads the buffer, checking for run time error*/
	if (!pBD) {
		return RT_FAIL_2;
	}

	/*reset the default flags value to 0, also resetting EOB*/
	pBD->flags &= RESET_EOB;

	/*set flags field eob to 1 if getc_offset and addc_offset are the same*/
	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->flags |= SET_EOB;
		return 0;
	}
	
	/*reset the end of buffer flag again*/
	pBD->flags &= RESET_EOB;

	/*increment getc_offset by 1 and return character at getc_offset*/
	temp = pBD->cb_head[pBD->getc_offset];
	++pBD->getc_offset;
	
	return temp;
	}

/*
*Purpose: returns the value of flags field determined by the eob bit; using a bitwise operation
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: none
*Parameters: Buffer* const pBD
*Return Value: int[-1(runtime error)
*Algorithm: (is exactly as written)
*/
int b_eob(Buffer* const pBD) { 
	/*the flags field value is returned to calling function*/
	if (pBD) {
		return (pBD->flags & CHECK_EOB);
	}
	else {
		return RT_FAIL_1;
	}
}  

/*
*Purpose: actually prints on an individual character basis the contents of the buffer
*Author: Saad Abdullah
*Version: 1.0
*Called Functions: b_getc(), printf(), b_eob()
*Parameters: Buffer* const pBD, char nl
*Return Value: int()
*Algorithm: (is exactly as written)
*/
int b_print(Buffer* const pBD, char nl) {
	char character;
	int count = 0;

	/*check if buffer is even valid*/
	if (!pBD) {
		return RT_FAIL_1;
	}

	/*loops until the end of buffers content is hit*/
	while (!b_eob(pBD)) {
		/*storing content in buffer into char variable using b_getc*/
		character = b_getc(pBD);
		if (b_eob(pBD)) {
			break;
		}
		/*display character by character the buffer contents*/
		printf("%c", character);
		count++;
	}
	if (nl != 0) {
		printf("\n");
	}
	/*return the number of characters printed*/
	pBD->getc_offset = 0;
	return count;
}

/*
*Purpose:
*Author: Saad Abdullah
*Version: 1.0
*Called Functions:
*Parameters:
*Return Value:
*Algorithm:
*/
Buffer* b_compact(Buffer* const pBD, char symbol) {
	/*based on the operational mode, buffer resizes to a temporary new capacity*/ 
	char* tempb = NULL; 
	short new_capacity;

	/*check if buffer is not valid*/
	if (!pBD) {
		return NULL;
	}

	/*must assign flags to 0*/
	pBD->flags &= RESET_R_FLAG;
	
	/*makes sure current buffer limit is not hit*/
	if(pBD->addc_offset == MAXIMUM_ALLOWED_POSITIVE_VALUE){
		return NULL;
	}
	/*convert addc into bytes, and add 1 char space by reallocating the current limit*/
		new_capacity = (short)(pBD->addc_offset + 1) * (sizeof(char)); 
		tempb = (char*) realloc(pBD->cb_head, new_capacity);

	/*set flags to one, if the reallocation didnt change memory address*/	
	if (tempb != (pBD->cb_head)) {
		pBD->flags |= SET_R_FLAG;
	}
	pBD->cb_head = tempb;
	pBD->capacity = new_capacity;
	pBD->flags |= RESET_R_FLAG; 

	/*adds the symbol to end of char buffer using addc_offset*/ 
	pBD->cb_head [b_limit(pBD)] = symbol;
	pBD->addc_offset++;
	return pBD;
	
}

/* 
*Purpose:
*Author: Saad Abdullah
*Version: 1.0
*Called Functions:
*Parameters:
*Return Value:
*Algorithm:
*/
char b_rflag(Buffer* const pBD) {
	/*make sure buffer avoids runtime error*/
	if (!pBD) {
		return RT_FAIL_1;
	}
	/*return the value of r_flags bit(2)*/
	if ((pBD->flags | CHECK_R_FLAG) != pBD->flags) {
		return 0;
	}
	return pBD->flags & CHECK_R_FLAG;
	
}

/*
*Purpose:
*Author: Saad Abdullah
*Version: 1.0
*Called Functions:
*Parameters:
*Return Value:
*Algorithm:
*/
short b_retract(Buffer* const pBD) {
	/*decrement getc_offset*/
	pBD->getc_offset--;
	/*make sure buffer avoids runtime error*/
	if (!pBD) {
		return RT_FAIL_1;
	}
	else {
		return pBD->getc_offset;
	}
}

/*
*Purpose:
*Author: Saad Abdullah
*Version: 1.0
*Called Functions:
*Parameters:
*Return Value:
*Algorithm:
*/
short b_reset(Buffer* const pBD) {	
	/*set getc_ to value or markc_*/
	if (!pBD) {
		return RT_FAIL_1;
	}
	pBD->getc_offset = pBD->markc_offset;
	return pBD->getc_offset;
}

/*
*Purpose:
*Author: Saad Abdullah
*Version: 1.0
*Called Functions:
*Parameters:
*Return Value:
*Algorithm:
*/
short b_getcoffset(Buffer* const pBD) {
	/*just returns getc_offset to calling function*/
	if (!pBD) {
		return RT_FAIL_1;
	}

	return pBD->getc_offset;
}

/*
*Purpose:
*Author: Saad Abdullah
*Version: 1.0
*Called Functions:
*Parameters:
*Return Value:
*Algorithm:
*/
int b_rewind(Buffer* const pBD) {
	/*allows the buffer to be re-read*/
	if (!pBD) {
		return RT_FAIL_1;
	}
	/*set both getc and markc to 0*/
	pBD->flags &= RESET_EOB;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	return 0;
}

/*
*Purpose:
*Author: Saad Abdullah
*Version: 1.0
*Called Functions:
*Parameters:
*Return Value:
*Algorithm:
*/
char* b_location(Buffer* const pBD) {

	if (!pBD) {
		return NULL;
	}
	/*return pointer to location of char buffer*/
	return (pBD->cb_head + pBD->markc_offset);
	
}