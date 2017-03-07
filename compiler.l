%option yylineno
%{
	//#include "compiler.tab.h"
	void yyerror();
%}
%%
"void"									{printf("void ");/*return type_void;*/}
"int"									{printf("type_integer ");/*return type_int;*/}
"float"									{printf("type_float ");/*return type_float;*/}
"string"								{printf("type_string ");/*return type_string;*/}
"char"									{printf("type_char ");/*return type_char;*/}
"bool"									{printf("type_boolean ");/*return type_bool;*/}
"true"									{printf("true ");/*return true_case;*/}
"false"									{printf("false ");/*return false_case;*/}
[-+*/%!=<>(){};]						{ECHO;printf(" ");/*return *yytext;*/}
[\^]								{ECHO;printf(" ");/*return *yytext;*/}
"=="									{printf("logical_equal ");/*return equals_operator;*/}
"<="									{printf("logical_smaller_or_equal ");/*return smaller_or_equal_operator;*/}
">="									{printf("logical_greater_or_equal ");/*return greater_or_equal_operator;*/}
"!="									{printf("logical_not_equal ");/*return equals_operator;*/}
"&&"									{printf("and ");/*return and_operator;*/}
"||"									{printf("or ");/*return or_operator;*/}

"if"									{printf("if_statement ");/*return if_statement;*/}
"else"									{printf("else_keyword ");/*return else_statement;*/}
"while"									{printf("while_keyword ");/*return while_loop;*/}
"do"									{printf("do_keyword ");/*return do_statement;*/}
"for"									{printf("for_keyword ");/*return for_loop;*/}
"switch"								{printf("switch_keyword ");/*return switch_statement;*/}
"case"									{printf("case_keyword ");/*return case_statement;*/}
"default"								{printf("default_keyword "); /* return default; */	}
"return"								{printf("return_keyword ");/*return ret;*/}

[a-zA-Z][a-zA-Z0-9]*					{printf("identifier ");/*yylval.id = strdup(yytext); return identifier;*/}
([-]?[1-9][0-9]*)|(0)					{printf("constant_integer ");/*yylval.val = atoi(yytext); return integer;*/}
[0-9]+[.][0-9]+							{printf("constant_float ");/*yylval.val = atof(yytext); return float;*/}
\"[^ \t\n]*\"							{printf("string ");/*yylval = strdup(yytext+1); return string;*/}
\'[^ \t\n]\'							{printf("char ");/*yylval = strdup(yytext+1); return char;*/}

[ \t\n]+								;
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
