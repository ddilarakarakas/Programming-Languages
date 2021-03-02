%option noyywrap
%{
    #include "gpp_lib.h"
%}
digit [0-9]
value -?{digit}+(\.{digit}+)?
%%
{value} {
        yylval.int_val = strtod(yytext, NULL);
        return VALUE;
}
"and"                   { return KW_AND; }
"or"                    { return KW_OR; }
"not"                   { return KW_NOT; }
"equal"                 { return KW_EQUAL; }
"less"                  { return KW_LESS; }
"nil" | "NIL"           { return KW_NIL; }
"list"                  { return KW_LIST; }
"append"                { return KW_APPEND; }
"concat"                { return KW_CONCAT; }
"set"                   { return KW_SET; }
"deffun"                { return KW_DEFFUN; }
"for"                   { return KW_FOR; }
"if"                    { return KW_IF; }
"exit"                  { return KW_EXIT; }
"load"                  { return KW_LOAD; }
"disp"                  { return KW_DISP; }
"while"                 { return KW_WHILE; }
"true"                  { yylval.string_val = (char *) malloc(strlen(yytext)*sizeof(char));
                          strcpy(yylval.string_val, yytext); return KW_TRUE; }
"false"                 { yylval.string_val = (char *) malloc(strlen(yytext)*sizeof(char));
                          strcpy(yylval.string_val, yytext); return KW_FALSE; }
"+"                     { return OP_PLUS; }
"-"                     { return OP_MINUS; }
"/"                     { return OP_DIV; }
"*"                     { return OP_MULT; }
"("                     { return OP_OP; }
")"                     { return OP_CP; }
"**"                    { return OP_DBLMULT; }
"'"                     { return OP_OC; }
","                     { return OP_COMMA; }
"\n"                    { return EOL; }
[a-zA-Z][_a-zA-Z0-9]*               { yylval.string_val = (char *) malloc(strlen(yytext)*sizeof(char)); return IDENTIFIER; }
";;"                                { return COMMENT; }
";;"+[_a-zA-Z0-9" "]*               { return COMMENT; }
";;"+[ \t\r\f][_a-zA-Z0-9" "]*      { return COMMENT; }
[ \t\r\f]+      
.                      
%%