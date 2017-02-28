%{
	void yyerror (char *s);
	#include <stdio.h>
	#include <string.h>
	#include <stdlib.h>
	extern int yylex();
	extern int yylineno;
	extern char* yytext;
	

	char* symbols[200];
	int values[200];

%}

%union{int val; char *id;}
%start line
%token type_int
%token print_command
%token type_float
%token type_string
%token equals_operator
%token and_operator
%token or_operator
%token not_operator
%token if_statement
%token else_statement
%token for_loop
%token while_loop
%token do_statement
%token switch_statement
%token case_statement
%token left_parenthesis
%token right_parenthesis
%token left_curly
%token right_curly
%token <val> number
%token <id> identifier
%type <val> line expr term 
%type <id> assignment


%%
line :	assignment ';' 					{;}
	 | 	print_command expr ';'			{ printf("printing %d\n",$2);}
	 | 	line print_command expr ';' 	{ printf("printing %d\n",$3);}
	 |	line assignment ';' {;} 		
;

assignment :  identifier '=' expr  {updateSymbol($1); updateSymbolVal($1,$3);}
;
expr :	term 	    {$$ = $1;}
	|	expr '+' term	{$$ = $1 + $3;}
	|	expr '-' term	{$$ = $1 - $3;}
	|	expr '*' term	{$$ = $1 * $3;}
	|	expr '/' term	{$$ = $1 / $3;}
	|   '(' expr ')'	{$$ = $2;}
;

term :  number			{$$ = $1;}
	 | identifier		{ test(); $$ = symbolVal($1);}
;

%%

void yyerror (char *s){
	fprintf(stderr,"%s at line number: %d \n",s,yylineno);
}
void test(){
	int i;
	for (i = 0 ;i<5;i++){
		printf("symbol %d is %s \n",i,symbols[i]);
	}
}


int main(void){
	int i;
	for (i=0;i<200;i++){
		symbols[i]= "\0";
		values [i] = 0;
	}
	yyparse();
	/*
	int token = yylex();
	while(token){
		switch(token){
			case identifier:
				printf("identifier\n");
			break;
			case number:
				printf("number\n");
			break;
			case if_statement:
				printf("if_statement\n");
			break;
			case left_parenthesis:
				printf("left_parenthesis\n");
				break;
			case right_parenthesis:
				printf("right_parenthesis\n");
			break;
			case equals_operator:
				printf("equals_operator\n");
				break;
		}
		//printf ("%d\n",token);
		token = yylex();
	}*/
	return 0;
}

void updateSymbol(char *symbol){
	int i;
	for (i = 0; i<200;i++){
		if (!strcmp(symbols[i],"\0")){
			symbols[i]= symbol;
			printf("symbol added\n");
			break;		
		}
		else if (!strcmp(symbols[i],symbol)){
			break;
		}
		else{
			printf("symbol not added\n");
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
	printf("symbol is %s of value = %d\n",symbol,num);
}

