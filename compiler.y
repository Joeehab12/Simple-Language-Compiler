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
	nodeType *opr(int oper, int nops, ...);
	nodeType *id(char* id_name);
	nodeType *con(int value);
	void freeNode(nodeType *p);
	int ex(nodeType *p);
	char* symbols[200];
	int values[200];
    FILE* fp;
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
%type <nPtr> assignment
%left '+' '-'
%left '*' '/'
%left '^' '%'
%left greater_or_equal_operator smaller_or_equal_operator equals_operator not_equal_operator '>' '<'
%nonassoc uminus

%%
program:	function					{exit(0);}
;

function:	function line 						{ex($2),freeNode($2);}
|
;

line :';'								{;}
	 |	assignment ';' 					{;}
	 | 	cout_command left_shift expr ';'			{ $$ = opr(cout_command,1,$3);}
;
/*line_list:	line   						{$$ = $1;}
		 |	line_list line 				{$$ = opr(';',2,$1,$2);}
*/
assignment :  identifier '=' expr  		{$$ = opr('=',2,id($1),$3);}
;

expr :	term 	    {$$ = $1;}
	|	expr '+' expr	{$$ = opr('+',2,$1,$3);}
	|	expr '-' expr	{$$ = opr('-',2,$1,$3);}
	|	expr '*' expr	{$$ = opr('*',2,$1,$3);}
	|	expr '/' expr	{$$ = opr('/',2,$1,$3);}
	|	expr '^' expr		{$$ = opr('^',2,$1,$3);}	
	|	expr '%' expr	{$$ = opr('%',2,$1,$3);}
	|   '(' expr ')'	{$$ = $2;}
	;

term :  integer			{$$ = con($1);}
	 | identifier		{$$ = id($1);/*$$ = symbolVal($1); test();*/}
;


%%

void yyerror (char *s){
	fprintf(stderr,"\n%s at line number: %d \n",s,yylineno);
}
void test(){
	int i;
	for (i = 0 ;i<5;i++){
		fprintf(fp,"\nsymbol %d is %s \n",i,symbols[i]);
		}
}


int main(void){
	fp = fopen("output.c","w");
	int i;
	for (i=0;i<200;i++){
		symbols[i]= "\0";
		values [i] = 0;
	}
	yyparse();
	fflush(fp);
	return 0;
}

void updateSymbol(char *symbol){
	int i;
	for (i = 0; i<200;i++){
		if (!strcmp(symbols[i],"\0")){
			symbols[i]= symbol;
			fprintf(fp,"\nsymbol added\n");
			break;		
		}
		else if (!strcmp(symbols[i],symbol)){
			break;
		}
		else{
			fprintf(fp,"\nsymbol not added\n");
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
	fprintf(fp,"\nsymbol is %s of value = %d\n",symbol,num);
}

nodeType *con(int value) {
    nodeType *p;

    /* allocate node */
    if ((p = malloc(sizeof(nodeType))) == NULL)
        yyerror("out of memory");

    /* copy information */
    p->type = typeCon;
    p->con.value = value;

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

    if (!p) return 0;
    switch(p->type) {
    case typeCon:       
        fprintf(fp,"\tpush\t%d\n", p->con.value); 
        break;
    case typeId:        
        fprintf(fp,"\tpush\t%s\n", p->id.id_name); 
        break;
    case typeOpr:
        switch(p->opr.oper) {
        /*case WHILE:
            printf("L%03d:\n", lbl1 = lbl++);
            ex(p->opr.op[0]);
            printf("\tjz\tL%03d\n", lbl2 = lbl++);
            ex(p->opr.op[1]);
            printf("\tjmp\tL%03d\n", lbl1);
            printf("L%03d:\n", lbl2);
            break;
        case IF:
            ex(p->opr.op[0]);
            if (p->opr.nops > 2) {
                /* if else */
                /*
                printf("\tjz\tL%03d\n", lbl1 = lbl++);
                ex(p->opr.op[1]);
                printf("\tjmp\tL%03d\n", lbl2 = lbl++);
                printf("L%03d:\n", lbl1);
                ex(p->opr.op[2]);
                printf("L%03d:\n", lbl2);
            } else {
                /* if */
                /*
                printf("\tjz\tL%03d\n", lbl1 = lbl++);
                ex(p->opr.op[1]);
                printf("L%03d:\n", lbl1);
            }
            break;
            */
        
        case cout_command:     
            ex(p->opr.op[0]);
            fprintf(fp,"\tprint\n");
            break;
        case '=':       
            ex(p->opr.op[1]);
            fprintf(fp,"\tpop\t\t%s\n", p->opr.op[0]->id.id_name);
            break;
        case uminus:    
            ex(p->opr.op[0]);
            fprintf(fp,"\tneg\n");
            break;
        default:
            ex(p->opr.op[0]);
            ex(p->opr.op[1]);
            switch(p->opr.oper) {
            case '+':   fprintf(fp,"\tadd\n"); break;
            case '-':   fprintf(fp,"\tsub\n"); break; 
            case '*':   fprintf(fp,"\tmul\n"); break;
            case '/':   fprintf(fp,"\tdiv\n"); break;
            case '<':   fprintf(fp,"\tcompLT\n"); break;
            case '>':   fprintf(fp,"\tcompGT\n"); break;
            case greater_or_equal_operator:    fprintf(fp,"\tcompGE\n"); break;
            case smaller_or_equal_operator:    fprintf(fp,"\tcompLE\n"); break;
            case not_equal_operator:    fprintf(fp,"\tcompNE\n"); break;
            case equals_operator:    fprintf(fp,"\tcompEQ\n"); break;
            }
        }
    }
    return 0;
}
