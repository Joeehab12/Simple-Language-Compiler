%option yylineno
%{
	//#include "compiler.tab.h"
	void yyerror();
%}
%%
"void"									{printf("void ");/*return type_void;*/}
"int"									{printf("int ");/*return type_int;*/}
"float"									{printf("float ");/*return type_float;*/}
"string"								{printf("string ");/*return	type_string;*/}
"char"									{printf("char ");/*return type_char;*/}
"bool"									{printf("boolean ");/*return type_bool;*/}
"true"									{printf("true ");/*return true_case;*/}
"false"									{printf("false ");/*return false_case;*/}
"cin"									{printf("input ");/*return cin_command;*/}
"cout"									{printf("print ");/*return cout_command;*/}
"if"									{printf("if ");/*return if_statement;*/}
"else"									{printf("else ");/*return else_statement;*/}
"for"									{printf("for ");/*return for_loop;*/}
"while"									{printf("while ");/*return while_loop;*/}
"do"									{printf("do ");/*return do_statement;*/}
"switch"								{printf("switch ");/*return switch_statement;*/}
"case"									{printf("case ");/*return case_statement;*/}
"break"									{printf("break "); /* return break;*/}
"continue"								{printf("continue "); /* return continue;*/}
"default"								{printf("default "); /* return default; */	}
"return"								{printf("return ");/*return return;*/}
"quit"									{yyterminate();}
[a-zA-Z_][a-zA-Z0-9_]*					{printf("identifier ");/*yylval.id = strdup(yytext); return identifier;*/}
[a-zA-Z_][a-zA-Z0-9_]*[+][+]			{printf("postfix_increment "); /*return postfix_increment;*/}
[+][+][a-zA-Z_][a-zA-Z0-9_]*			{printf("prefix_increment "); /*return prefix_increment;*/}
([-+]?[1-9][0-9]*)|(0)					{printf("integer ");/*yylval.val = atoi(yytext); return number;*/}
[0-9]+[.][0-9]+							{printf("float ");}
[-+*/%^=<>(){}:;,.!?'"]					{ECHO;/*return *yytext;*/}
"=="									{printf("logical_equal ");/*return equals_operator;*/}
"<="									{printf("logical_smaller_or_equal ");/*return smaller_or_equal_operator;*/}
">="									{printf("logical_greater_or_equal ");/*return greater_or_equal_operator;*/}
"!="									{printf("logical_not_equal ");/*return equals_operator;*/}
"&&"									{printf("and ");/*return and_operator;*/}
"||"									{printf("or ");/*return or_operator;*/}
[ \t\n]+								{;}
.    									{yyerror();}

%%
void yyerror (){
	fprintf(stderr,"\nUnexpected character at line number: %d \n",yylineno);
}
int yywrap(void){
	return 1;
}
int main(void){
	yylex();
	return 0;	
}