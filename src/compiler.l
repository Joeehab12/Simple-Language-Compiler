%option yylineno
%{
	#include "compiler.h"
	#include "y.tab.h"
	#include "string.h"
	void error();
%}
%%
"void"									{return type_void;}
"int"									{yylval.type = strdup(yytext); return type_int;}
"float"									{yylval.type = strdup(yytext);return type_float;}
"char"									{yylval.type = strdup(yytext);return type_char;}
"bool"									{yylval.type = strdup(yytext);return type_bool;}
"true"									{return true_case;}
"false"									{return false_case;}
"cout"									{return cout_command;}
"const"									{return constant;}
"<<"									{return left_shift;}
[-+*/%^!=<>(){}:,;]							{return *yytext;}
"++"									{return increment;}
"--"									{return decrement;}
"=="									{return equals_operator;}
"<="									{return smaller_or_equal_operator;}
">="									{return greater_or_equal_operator;}
"!="									{return not_equal_operator;}
"&&"									{return and_operator;}
"||"									{return or_operator;}
"if"									{return if_statement;}
"else"									{return else_statement;}
"while"									{return while_loop;}
"do"									{return do_statement;}
"for"									{return for_loop;}
"switch"								{return switch_statement;}
"case"									{return case_statement;}
"default"								{return default_command; 	}
"return"								{return return_command;}
"break"									{return break_command;}
[a-zA-Z][a-zA-Z0-9]*					{yylval.id = strdup(yytext); return identifier;}
([-]?[1-9][0-9]*)|(0)					{yylval.ival = atoi(yytext); return integer;}
(0|[1-9][0-9]*)[.][0-9]+							{yylval.fval = atof(yytext); return float_number;}
'[^ \t\r\n]'							{yylval.c = *(yytext+1); return character;}

[ \t\r\n]+								;
.    									{error();}

%%
void error (){
	fprintf(stderr,"\nUnexpected character at line number: %d \n",yylineno);
}
int yywrap(void){
	return 1;
}
