#ifndef __gpp_lib_h_
#define __gpp_lib_h_
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include "y.tab.h"
int yyparse(void);
int yylex(void);
void yyerror(char *);
int isPLUS(int, int);
int isMINUS(int, int);
int isMULTI(int, int);
int isDIVIDE(int, int);
int isDBLMULT(int, int);
char* isAND(char*, char*);
char* isOR(char*, char*);
char* isNOT(char*);
char* isEQUAL_STR(char*, char*);
char* isEQUAL_INT(int, int);
char* isLESS(int,int);
void fillTheArray(int);
void printList(); 
void listAppend(int, int*);
void listConcat(int*,int*);
char* returnString(char*);
#endif
