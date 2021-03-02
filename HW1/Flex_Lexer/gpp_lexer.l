%option noyywrap
%{
   #include <stdio.h>
   #include <stdlib.h>
   #include <string.h>
%}

digit      [0-9]      
ID         [a-z][a-z0-9]*
VALUE	   [0]|[1-9]{digit}*
ERROR1	   [0-9]{digit}*
ERROR2      {digit}{ID}
ERROR3      {ID}+[*/(]+.*
ERROR4   [;][^;].*

%%
"\n\n"            { return 0; }
";;".*  		      { return 1; } 
"and"             { return 2; }
"or"              { return 3; }
"not"             { return 4; }
"equal"           { return 5; }
"less"            { return 6; }
"nil"             { return 7; }
"list"            { return 8; }
"append"          { return 9; }
"concat"          { return 10; }
"set"             { return 11; }
"deffun"          { return 12; }
"for"             { return 13; }
"if"              { return 14; }
"exit"            { return 15; }
"load"            { return 16; }
"disp"            { return 17; }
"true"            { return 18; }
"false"           { return 19; }
"("               { return 20; }
")"               { return 21; }
","               { return 22; }
"+"	            { return 23; }
"-"	            { return 24; }
"/"	            { return 25; }
"*"	            { return 26; }
"**"	            { return 27; }
""""	            { return 28; }

{ID}	   { return 29; }
{VALUE}  { return 30; }
{ERROR1}  { return 31; } 
{ERROR2}  { return 31; } 
{ERROR3}  { return 31; } 
{ERROR4}  { return 31; } 
%% 

int main(int argc, char *argv[]){
   yyin = fopen(argv[1], "r");
   FILE *fp;
   const char* names[] = { "COMMENT", "KW_AND", "KW_OR", "KW_NOT",
    "KW_EQUAL", "KW_LESS", "KW_NILL", "KW_LIST", "KW_APPEND",
    "KW_CONCAT", "KW_SET", "KW_DEFFUN", "KW_FOR",
    "KW_IF", "KW_EXIT", "KW_LOAD", "KW_DISP", "KW_TRUE",
    "KW_FALSE","OP_OP", "OP_CP","OP_COMMA", "OP_PLUS", "OP_MINUS", "OP_DIV", "OP_MULT",
    "OP_DBLMULT", "OP_OC OP_CC", "IDENTIFIER", "VALUE","SYNTAX_ERROR"};
   fp = fopen("parsed_cpp.txt", "w"); 
   int caseToken = 0;
	caseToken = yylex();
   while(caseToken != 0){
      int temp = caseToken - 1;
      
         fprintf(fp,"%s \n",names[temp]);
	   caseToken = yylex();
   }
   fclose(yyin);
   fclose(fp);
   return 0;
}
