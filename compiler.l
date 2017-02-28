%{
	//#include "y.tab.h"
	void yyerror(char *s);
%}

%%
"void"						{printf("void ");}
"print"						{printf("print ");/*return print_command;*/}
"int"						{printf("int ");/*return type_int;*/}
"float"						{printf("float ");/*return type_float;*/}
"string"					{printf("string ");/*return	type_string;*/}
"bool"						{printf("boolean");}
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
[0-9]+						{printf("integer ");/*yylval.val = atoi(yytext); return number;*/}
[0-9]+[.]([0-9]+)			{printf("float");}
"+"							{printf("plus ");}
"-"							{printf("minus ");}
"*"							{printf("multiply ");}
"/"							{printf("divide ");}
"="							{printf("assignment_equal ");}
";"							{printf("semi-colon ");}
[ \t\n]				;
"'"							{printf("single_quotation ");}
"\""							{printf("double_quotation ");}			
"=="						{printf("logical_equal ");/*return equals_operator;*/}
"&&"						{printf("and ");/*return and_operator;*/}
"||"						{printf("or ");/*return or_operator;*/}
"!"							{printf("not ");/*return not_operator;*/}
"("							{printf("left_parenthesis ");}
")"							{printf("right_parenthesis ");}
"{"							{printf("left_curly ");/*return left_curly;*/}
"}"							{printf("right_curly ");/*return right_curly;*/}
":"							{printf("colon "); /*return colon;*/}
.							{yyerror("unexpected character");	}

%%
void yyerror (char *s){
	fprintf(stderr,"\n%s at line number: %d \n",s,yylineno);
}

int yywrap(void){
	return 1;
}

int main(void){
	int token = yylex();
	while(token){
		token = yylex();
	}
	return 0;	
}
