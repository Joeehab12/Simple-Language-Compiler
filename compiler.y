	%{
	void yyerror (char *s);
	#include <stdio.h>
	#include <string.h>
	#include <stdlib.h>

	#include <stdarg.h>
	#include "compiler.h"
	extern int yylex();
	extern int yylineno;
	extern char* yytext;
	nodeType *opr(int oper , int nops, ...);
	nodeType *id(char* id_name);
	nodeType *con(int value);
	void freeNode(nodeType *p);
	int ex(nodeType *p);
	char* symbols[200];
	int values[200];
    FILE* fp;
    int count = 0;
    int idcount = 0;
%}

%union{int ival; char *id; char c; float fval; nodeType* nPtr;};

%token type_void
%token type_int
%token type_float
%token type_char
%token type_bool
%token left_shift
%token <val>true_case
%token <val>false_case
%token cout_command;
%token postfix_increment
%token prefix_increment
%token if_statement
%token else_statement
%token for_loop
%token while_loop
%token do_statement
%token switch_statement
%token case_statement
%token break_command
%token continue_command
%token default_command
%token <id> identifier
%token <ival>integer
%token <c>character
%token <fval>float_number
%token equals_operator
%token smaller_operator
%token greater_operator
%token smaller_or_equal_operator
%token greater_or_equal_operator
%token not_equal_operator
%token and_operator
%token or_operator
%token return_command;		
%type <nPtr> line expr term /*line_list*/
%type <nPtr> assignment declaration
%left '+' '-'
%left '*' '/'
%left greater_or_equal_operator smaller_or_equal_operator equals_operator not_equal_operator '>' '<'
%nonassoc uminus

%%
program:	function					{exit(0);}
;

function:	function line 						{ ex($2);	freeNode($2);}
|
;

line :';'								{;}
	 | declaration ';'					{;}
	 |	assignment ';' 					{;}
	 |  expr ';'						{;}
	 | 	cout_command left_shift expr ';'			{ $$ = opr(cout_command,1,$3);}
;

declaration: type_int identifier {$$ = id($2);}
			| type_char identifier{$$ = id($2);}
			| type_bool identifier{$$ = id($2);}
			| type_float identifier{$$ = id($2);}
;

assignment :  identifier '=' expr  		{$$ = opr('=',2,$1,$3);}
		   |  declaration '=' expr		{$$ = opr('=',2,$1,$3);}
;

expr :	term 	    {$$=$1;}
	|	expr '+' expr	{$$ = opr('+',2,$1,$3);}
	|	expr '-' expr	{$$ = opr('-',2,$1,$3);}
	|	expr '*' expr	{$$ = opr('*',2,$1,$3);}
	|	expr '/' expr	{$$ = opr('/',2,$1,$3);}
	|   '(' expr ')'	{$$ = $2;}
	;

term :  integer			{$$ = con($1);}
	 | identifier		{$$ = id($1);}
;


%%

void yyerror (char *s){
	fprintf(stderr,"\n%s at line number: %d \n",s,yylineno);
}
void test(){
	int i;
	for (i = 0 ;i<5;i++){
		printf("\nsymbol %d is %s \n",i,symbols[i]);
		}
}


int main(void){
	//= fopen("output.c","w");
	int i;
	for (i=0;i<200;i++){
		symbols[i]= "\0";
		values [i] = 0;
	}
	
	yyparse();
	//fflush(;
	return 0;
}

void updateSymbol(char *symbol){
	int i;
	for (i = 0; i<200;i++){
		if (!strcmp(symbols[i],"\0")){
			symbols[i]= symbol;
			printf("\nsymbol added\n");
			break;		
		}
		else if (!strcmp(symbols[i],symbol)){
			break;
		}
		else{
			printf("\nsymbol not added\n");
		}
	}
}

int symbolVal(char* symbol){
	int bucket = computeSymbolIndex(symbol);
	return values[bucket];
}

int computeSymbolIndex(char* symbol){
	int i;
	int index = 0;
	for (i = 0; i<200;i++){
		if (!strcmp(symbols[i],symbol)){
			index = i;
			break;	
		}
	}
	return index;
}

void updateSymbolVal(char *symbol,int num){
	int bucket = computeSymbolIndex(symbol);
	values[bucket] = num;
	printf("\nsymbol is %s of value = %d\n",symbol,num);
}

nodeType *con(int value) {
    nodeType *p;

    /* allocate node */
    if ((p = malloc(sizeof(nodeType))) == NULL)
        yyerror("out of memory");

    /* copy information */
    p->type = typeCon;
    p->con.value = value;
    count++;
    p->con.registerNumber = count;
    
    return p;
}

nodeType *id(char *id_name) {
    nodeType *p;

    /* allocate node */
    if ((p = malloc(sizeof(nodeType))) == NULL)
        yyerror("out of memory");

    /* copy information */
    p->type = typeId;
    p->id.id_name = id_name;
    idcount++;
    p->id.registerNumber = idcount;
    return p;
}

nodeType *opr(int oper, int nops, ...) {
    va_list ap;
    nodeType *p;
    int i;
    /* allocate node, extending op array */
    if ((p = malloc(sizeof(nodeType) + (nops-1) * sizeof(nodeType *))) == NULL)
        yyerror("out of memory");

    /* copy information */
    p->type = typeOpr;
    p->opr.oper = oper;
    p->opr.nops = nops;
    if (oper != '='){
    count++;
	}
    p->registerNumber = count;
   	   	va_start(ap, nops);
    	for (i = 0; i < nops; i++)
        	p->opr.op[i] = va_arg(ap, nodeType*);
    	va_end(ap);
   return p;
}
 
void freeNode(nodeType *p) {
    int i;

    if (!p) return;
    if (p->type == typeOpr) {
        for (i = 0; i < p->opr.nops; i++)
            freeNode(p->opr.op[i]);
    }
    free (p);
}


static int lbl;

int ex(nodeType *p) {
    int lbl1, lbl2;
    int x;	
    if (!p) return 0;
    switch(p->type) {
    case typeCon:       
        printf("mov R%d,%d\n",(p->con.registerNumber), p->con.value); 
        break;
    case typeId:        
        printf("mov R%d,%s\n",(p->id.registerNumber), p->id.id_name); 
        break;
    case typeOpr:
        switch(p->opr.oper) {      
        case cout_command:     
            printf("\tprint\n");
            break;
        case '=':
        		ex(p->opr.op[1]);
              break;
        case uminus:    
            ex(p->opr.op[0]);
            printf("\tneg\n");
            break;
        default:
        	 ex(p->opr.op[0]); ex(p->opr.op[1]);
            switch(p->opr.oper) {
            case '+': 
            if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeOpr ){
            printf ("add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeOpr ){
			printf ("add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeCon ){
			printf ("add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeCon ){
			printf ("add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeId ){
			printf ("add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeCon ){
			printf ("add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeOpr ){
			printf ("add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeId ){
			printf ("add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			else{
			printf ("add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			break;
            case '-':  
            if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeOpr ){
            printf ("sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeOpr ){
			printf ("sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeCon ){
			printf ("sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeCon ){
			printf ("sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeId ){
			printf ("sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeCon ){
			printf ("sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeOpr ){
			printf ("sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeId ){
			printf ("sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			else{
			printf ("sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
             break;
            case '*': 
            if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeOpr ){
            printf ("mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeOpr ){
			printf ("mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeCon ){
			printf ("mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeCon ){
			printf ("mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeId ){
			printf ("mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeCon ){
			printf ("mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeOpr ){
			printf ("mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeId ){
			printf ("mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			else{
			printf ("mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
             break;
            case '/':
            if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeOpr ){
            printf ("div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeOpr ){
			printf ("div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeCon ){
			printf ("div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeCon ){
			printf ("div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeId ){
			printf ("div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);

			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeCon ){
			printf ("div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeOpr ){
			printf ("div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeId ){
			printf ("div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			else{
			printf ("div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
             break;
            case '<':   printf("\tcompLT\n"); break;
            case '>':   printf("\tcompGT\n"); break;
            case greater_or_equal_operator:    printf("\tcompGE\n"); break;
            case smaller_or_equal_operator:    printf("\tcompLE\n"); break;
            case not_equal_operator:    printf("\tcompNE\n"); break;
            case equals_operator:    printf("\tcompEQ\n"); break;
            }
           
        }
    }
    return 0;
}