%{
	#include "y.tab.h"
%}

%%
"print"						{return print_command;}
"int"						{return type_int;}
"float"						{return type_float;}
"string"					{return	type_string;}
"if"						{return if_statement;}
"else"						{return else_statement;}
"for"						{return for_loop;}
"while"						{return while_loop;}
"do"						{return do_statement;}
"switch"					{return switch_statement;}
"case"						{return case_statement;}
[a-zA-Z]([a-zA-Z0-9_])*		{yylval.id = strdup(yytext); return identifier;}
[0-9]+						{yylval.val = atoi(yytext); return number;}
[-+*/=;]					{return yytext[0];}
[ \t\n]				;
"=="						{return equals_operator;}
"&&"						{return and_operator;}
"||"						{return or_operator;}
"!"							{return not_operator;}
"("							{return left_parenthesis;}
")"							{return right_parenthesis;}
"{"							{return left_curly;}
"}"							{return right_curly;}
.							{ECHO; yyerror("unexpected character");}

%% 
int yywrap(void){
	return 1;
}

