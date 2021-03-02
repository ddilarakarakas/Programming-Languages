#include "gpp_lib.h"
#define SIZE__ARRAY 9999
int array[SIZE__ARRAY];
int current=0;
extern FILE *yyin;
FILE *outputFile;

int main(int argc, char **argv){

 if((outputFile=freopen("output.txt", "w" ,stdout))==NULL) {
  printf("Cannot open file.\n");
  exit(1);
 }
   FILE *fp;
   if(1 < argc){
      yyin = fopen(argv[1], "r");
      yyrestart(yyin);
      if(1 == yyparse()){
         fclose(yyin);
         fclose(outputFile);
         return 0;
      }
      fclose(yyin);
   }
   yyin = stdin;
   yyrestart(yyin);
   while(1) {
      if(1 == yyparse()) {
       fclose(outputFile);
         return 0;
      }
   }
   fclose(outputFile);
   return 0;
}
void yyerror(char *s) {
    fprintf(stderr, "%s\n", s);
}
int isPLUS(int op1, int op2) {
    int temp;
    temp = op1 + op2;
    return temp;
}  
int isMINUS(int op1, int op2) {
    int temp;
    temp = op1 - op2;
    return temp;
}  
int isMULTI(int op1, int op2) {
    int temp;
    temp = op1 * op2;
    return temp;
} 
int isDBLMULT(int op1, int op2) {
    int result=1;
    for(int i=0; i<op2; ++i) {
        result *= op1;
    }
    return result;
}
int isDIVIDE(int op1, int op2) {
    int temp;
    temp = (int)(op1 / op2);
    return temp;
}
char* isEQUAL_INT(int op1, int op2) {
    if (op1 == op2) 
        return "true";
    return "false";
}
char* isAND(char* op1, char* op2) {
    if ( (strcmp(op1, "true") == 0) && (strcmp(op2, "true") == 0))
        return "true";
    return "false";
}
char* isOR(char* op1, char* op2) {
    if ( (strcmp(op1, "true") == 0) || (strcmp(op2, "true") == 0))
        return "true";
    return "false";
}
char* isNOT(char* op1) {
    if ( (strcmp(op1, "true") == 0))
        return "false";
    return "true";
}
char* isEQUAL_STR(char* op1, char* op2) {
    if ( (strcmp(op1, op2) == 0))
        return "true";
    return "false";
}
char* isLESS(int a, int b) {
    if(a <= b)
        return "true";
    return "false";
}
void fillTheArray(int val) {
    array[current] = val;
    ++current;
}
void listAppend(int val, int* arr) {
    array[current] = val;
    ++current;
}
void listConcat(int* arr1, int* arr2) { }
void printList() {
    if(current == 0) {
        printf("NIL\n");
    }
    else {
        printf("( ");
        for(int i=0; i< current;++i) {
            printf("%d ",array[i]);
        }
        printf(")\n");
        current = 0;
    }   
}
char* returnString(char* str) {
    return str;
}
