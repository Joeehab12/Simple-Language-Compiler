%{
	//#include "y.tab.h"
%}

%%
"print"						{printf("print ");/*return print_command;*/}
"int"						{printf("integer ");/*return type_int;*/}
"float"						{printf("float ");/*return type_float;*/}
"string"					{printf("string ");/*return	type_string;*/}
"if"						{printf("if ");/*return if_statement;*/}
"else"						{printf("else ");/*return else_statement;*/}
"for"						{printf("for ");/*return for_loop;*/}
"while"						{printf("while ");/*return while_loop;*/}
"do"						{printf("do ");/*return do_statement;*/}
"switch"					{printf("switch ");/*return switch_statement;*/}
"case"						{printf("case ");/*return case_statement;*/}
"break"						{printf("break "); /* return break;*/}
"default"					{printf("default "); /* return default; */	}
[a-zA-Z]([a-zA-Z0-9_])*		{printf("identifier ");/*yylval.id = strdup(yytext); return identifier;*/}
[a-zA-Z]([a-zA-Z0-9_])*([+][+])	{printf("prefix_increment "); /*return prefix_increment;*/}
([+][+])([a-zA-Z]([a-zA-Z0-9_])*)	{printf("postfix_increment "); /*return prefix_increment;*/}
[0-9]+						{printf("number ");/*yylval.val = atoi(yytext); return number;*/}
[-+*/=();]					{printf("operator ");/*return yytext[0];*/}
[ \t\n]				;

"=="						{printf("equals ");/*return equals_operator;*/}
"&&"						{printf("and ");/*return and_operator;*/}
"||"						{printf("or ");/*return or_operator;*/}
"!"							{printf("not ");/*return not_operator;*/}
"{"							{printf("left_curly ");/*return left_curly;*/}
"}"							{printf("right_curly ");/*return right_curly;*/}
":"							{printf("colon "); /*return colon;*/}
.							{ECHO; /*yyerror("unexpected character");*/	}

%% 
int yywrap(void){
	return 1;
}

int main(void){
	return yylex();
}
