%{
	//void yyerror (char *s);
	#include <stdio.h>
	#include <string.h>
	#include <stdlib.h>
	#include <stdarg.h>
	#include "compiler.h"
	void yyerror(const char *msg);
	extern int yylex();
	extern int yylineno;
	extern char* yytext;
	nodeType *opr(int oper , int nops, ...);
	nodeType *id(char* id_name);
	nodeType *con(int value);
	void freeNode(nodeType *p);
	int computeSymbolIndex(char* symbol);
	int symbolVal(char* symbol);
	void updateSymbol(char *symbol);
	void updateSymbolVal(char *symbol,int num);
	void symbol_init();
	void printSymbolTable();
	int ex(nodeType *p);
	char* symbols[200];
	int values[200];
	char* filename;
	int registerNumbers[200];
    extern FILE* yyin;
    extern FILE* yyout;
    int count = 0;
    int idcount = 0;
    int regcount = 0;
    int lastRegNum = 0;
    int regNum = 0;
    int decRegNum = 0;
    int id_flag = 0;
    int logex_flag = 0;
    int equal_flag = 0;
    int opr_flag = 0;
    int conditionFlag = 0;
    int lastOp1RegNum = 0;
    int lastOp2RegNum = 0;
    int op1RegNum = 0;
  	int op2RegNum = 0;
  	char *err;
    static int lastlbl = 0;
%}

%union{int ival; char *id; char* type; char c; float fval; nodeType* nPtr;};

%token type_void
%token <id>type_int
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
%type <nPtr> line expr term line_list
%type <nPtr> assignment declaration logex casestmt block case_list defaultstmt 
%left '+' '-'
%left '*' '/'
%left greater_or_equal_operator smaller_or_equal_operator equals_operator not_equal_operator '>' '<' and_operator or_operator '!'
%nonassoc uminus

%%
program:	function					{printSymbolTable(); exit(0);}
;

function:	function line 						{ ex($2);	freeNode($2);}
|
;

line :';'								{;}
	 | declaration ';'					{$$->declarationFlag = 1;}
	 |	assignment ';' 					{;}
	 |  expr ';'						{;}
	 | 	cout_command left_shift expr ';'			{ $$ = opr(cout_command,1,$3);}
	 |  if_statement '(' logex ')' '{' line_list '}' {$$ = opr(if_statement,2,$3,$6); logex_flag = 1; conditionFlag = 1; }
	 |	if_statement '(' logex ')' '{' line_list '}' else_statement '{' line_list '}' {$$ = opr(if_statement,3,$3,$6,$10); conditionFlag = 1; }
	 | 	switch_statement '(' identifier ')' '{' block '}'			{ $$ = opr(switch_statement,2,id($3),$6); $$->caseNum = 0;}
;

block:     case_list defaultstmt					{$$ = opr(';',2,$1,$2);}
;

defaultstmt: default_command ':' line_list	break_command ';'					{$$ = opr(default_command,1,$3); lastlbl++;}
;

casestmt:  case_statement integer ':' line_list break_command ';'	{ $$ = opr(case_statement,2,con($2),$4);lastlbl+=2;}
;

case_list: casestmt 								{;}
		 |case_list casestmt 						{$$ = opr(';',2,$1,$2);}
;

logex : term			{$$ = $1; conditionFlag = 1;}
	  | expr and_operator expr {$$ = opr(and_operator,2,$1,$3); conditionFlag = 1;  }
	  | expr or_operator expr {$$ = opr(or_operator,2,$1,$3); conditionFlag = 1;}
	  | expr not_equal_operator expr {$$ = opr(not_equal_operator,2,$1,$3); conditionFlag = 1;}
	  | '!' logex					 {$$ = opr('!',1,$2);  conditionFlag = 1;}
	  | expr equals_operator expr 	 {$$ = opr(equals_operator,2,$1,$3); conditionFlag = 1;}
	  | expr '>' expr	 {$$ = opr('>',2,$1,$3); conditionFlag = 1;}
	  | expr greater_or_equal_operator	expr {$$ = opr(greater_or_equal_operator,2,$1,$3); conditionFlag = 1;}
	  | expr '<' expr	 {$$ = opr('<',2,$1,$3); $$->conditionFlag = 1; conditionFlag = 1;}
	  | expr smaller_or_equal_operator expr {$$ = opr(smaller_or_equal_operator,2,$1,$3); conditionFlag = 1;}
;



line_list : line {;} 
			| line_list line {$$ = opr(';',2,$1,$2);}
;

declaration:  type_int identifier {$$ = id($2); updateSymbol($2);}
			| type_char identifier{$$ = id($2); updateSymbol($2);}
			| type_bool identifier{$$ = id($2); updateSymbol($2);}
			| type_float identifier{$$ = id($2); updateSymbol($2);}
;
assignment :  identifier '=' expr  		{$$ = opr('=',2,id($1),$3); }
		   |  declaration '=' expr		{$$ = opr('=',2,$1,$3); $1->declarationFlag = 1; }
;

expr :	term 	    {$$=$1; $$->declarationFlag = 0;}
	|	expr '+' expr	{$$ = opr('+',2,$1,$3); }
	|	expr '-' expr	{$$ = opr('-',2,$1,$3); }
	|	expr '*' expr	{$$ = opr('*',2,$1,$3); }
	|	expr '/' expr	{$$ = opr('/',2,$1,$3); }
	|   '(' expr ')'	{$$ = $2; }
	;

term :  integer			{equal_flag = 1; $$ = con($1); }
	 | identifier		{equal_flag = 1; $$ = id($1);  }
;


%%	
/*void yyerror (char *s){
	fprintf(stderr,"\n%s at line number: %d \n",s,yylineno);
}
*/

int main(void){
	symbol_init();
   filename = malloc(20*sizeof(char));
   printf("Please enter source file name: ");
   scanf("%s",filename);
   yyin = fopen(filename,"r");
   yyout = fopen("output.quad","w"); 
   yyparse(); 
   fclose(yyin);
   fclose(yyout);
   return 0;
}
void yyerror(const char *msg){
	fprintf(stderr,"%s:%d: %s",filename,yylineno,msg);
}

void printSymbolTable(){
	int i;
	printf("symbols: ");
	for (i = 0;i<10;i++){
		printf("%s ",symbols[i]);
	}
	printf("\n");
}
void symbol_init(){
	int i;
	for(i = 0;i<200;i++){
		symbols[i] = "\0";
		values[i] = 0;
		registerNumbers[i] = 0;
	}
}
void updateSymbol(char *symbol){
	int i;
	err = malloc(50*sizeof(char));
	for (i = 0; i<200;i++){
		if (!strcmp(symbols[i],"\0")){
			symbols[i]= symbol;
			regcount++;
			registerNumbers[i]=regcount;
			//printf("\nsymbol added\n");
			break;		
		}
		else if (!strcmp(symbols[i],symbol)){
			strcpy(err,"error: redeclaration of variable ");
			strcat(err,symbol);
			strcat(err,"\n");
			yyerror(err);
			break;
		}
		else{
			//printf("\nsymbol not added\n");
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
	//printf("\nsymbol is %s of value = %d\n",symbol,num);
}

nodeType *con(int value) {
    nodeType *p;

    /* allocate node */
    if ((p = malloc(sizeof(nodeType))) == NULL)
        yyerror("yyout of memory");

    /* copy information */
    p->type = typeCon;
    p->con.value = value;
    //if(equal_flag && !id_flag)count++;
    count++;
    //printf("con count = %d value = %d id_flag = %d equal_flag = %d\n",count,p->con.value,id_flag,equal_flag);
    p->registerNumber = count;
   // printf("con registerNumber = %d\n",p->registerNumber);
    return p;
}

nodeType *id(char *id_name) {
    nodeType *p;

    /* allocate node */
    if ((p = malloc(sizeof(nodeType))) == NULL)
        yyerror("yyout of memory");

    /* copy information */
    p->type = typeId;
    p->id.id_name = id_name;


    // un comment the following line if there is a bug not using idcount;
    //if(equal_flag && !id_flag)count++;
    count++;
    //printf("id count = %d id_name = %s id_flag = %d equal_flag = %d\n",count,p->id.id_name,id_flag,equal_flag);
    p->registerNumber = count;
   // printf("id registerNumber = %d\n",p->registerNumber);
    return p;
}

nodeType *opr(int oper, int nops, ...) {
    va_list ap;
    nodeType *p;
    int i;
    /* allocate node, extending op array */
    if ((p = malloc(sizeof(nodeType) + (nops-1) * sizeof(nodeType *))) == NULL)
        yyerror("yyout of memory");

    /* copy information */
    p->type = typeOpr;
    p->opr.oper = oper;
    p->opr.nops = nops;
    if (oper != '='){
    count++;
    //equal_flag = 1;
	}
	//printf("opr count = %d id_flag = %d equal_flag = %d\n",count,id_flag,equal_flag);
    p->registerNumber = count;
   // printf("opr registerNumber = %d\n",p->registerNumber);
   	   	va_start(ap, nops);
    	for (i = 0; i < nops; i++)
        	p->opr.op[i] = va_arg(ap, nodeType*);
    	va_end(ap);
    	if(p->opr.op[0]->type == typeId && p->opr.oper == '='){
    		equal_flag = 0;
    		//printf("zeroed\n");
    	}
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

  static int lbl = 0;
  int default_lbl;
  char* switch_var;
  int conditionlbl;
  char* symbol_id;
  int ex(nodeType *p) {
    // printf("ex (): type = %d condition_flag = %d opr_flag = %d\n",p->type,conditionFlag,opr_flag);
    int lbl1, lbl2;
    int x;
    int flag11;	
    if (!p) return 0;
    
    switch(p->type) {
    case typeCon:
    	//p->con.registerNumber++;
       	if (!conditionFlag){

	    	if(!p->declarationFlag && id_flag){
	    	fprintf(yyout,"mov %s,%d\n",symbol_id,p->con.value);
	    	}
	    	else{
	    			fprintf(yyout,"mov R%d,%d\n",p->registerNumber,p->con.value);
	    	}	
	    	//opr_flag = 0;
	    	/*if (!p->declarationFlag && id_flag && !logex_flag){
	    	fprintf(yyout,"mov %s,%d\n",symbol_id, p->con.value);
	    	}   
	    	else if (!p->declarationFlag && !id_flag && !logex_flag){
	        fprintf(yyout,"mov R%d,%d\n",(p->registerNumber), p->con.value);      
	    	}
	    	logex_flag = 0;
	    	id_flag = 0;
			*/
    	}
    	else{
    		if (opr_flag){
    			if(!p->declarationFlag && id_flag){
	    			fprintf(yyout,"mov %s,%d\n",symbol_id,p->con.value);
	    		}
	    		else{
	    			fprintf(yyout,"mov R%d,%d\n",p->registerNumber,p->con.value);
	    		}	
	    		
    		}
    		opr_flag = 0;
    		conditionFlag = 0;
    	}
        break;
    case typeId:   
    	
    	symbol_id = symbols[computeSymbolIndex(p->id.id_name)];    
    	if (!conditionFlag ){
	    	if (!p->declarationFlag && !id_flag){ 
	    		fprintf(yyout,"mov R%d,%s\n",(p->registerNumber), symbol_id);
	    	}
	    	//opr_flag = 0;
    	}
    	else{
    		if(opr_flag){
    			if (!p->declarationFlag && !id_flag){ 
	    			fprintf(yyout,"mov R%d,%s\n",(p->registerNumber), symbol_id);
	    		}
	    		
    		}
    		opr_flag = 0;
    		//conditionFlag = 0;
    	}
    	/*if (!p->declarationFlag && !id_flag){
    	int sym_index = computeSymbolIndex(p->id.id_name);
    	symbol_id = symbols[sym_index];
    	fprintf(yyout,"mov R%d,%s\n",(p->registerNumber), symbol_id);
    	}
    	else{

    		int sym_index = computeSymbolIndex(p->id.id_name);
    		symbol_id = symbols[sym_index];
    		decRegNum = registerNumbers[sym_index];
    		//fprintf(yyout,"mov %s,%s\n",symbol_id, p->id.id_flag);
    	}
    	*/
        break;
    case typeOpr:
    	//if (p->opr.op[1]->type == typeOpr){
    		id_flag = 0;
    	//}
        switch(p->opr.oper) {      
        case cout_command:     
            fprintf(yyout,"\tprint\n");
            break;
        case '=':
        		id_flag = 1;
        		ex(p->opr.op[0]);
        		ex(p->opr.op[1]);
        		int sym_index = computeSymbolIndex((p->opr.op[0])->id.id_name);
        		if(p->opr.op[1]->type == typeOpr){
        			fprintf(yyout,"mov %s,R%d\n",symbols[sym_index], lastRegNum/*p->id.id_name*/); 
        		}
        		/*else if (p->opr.op[1]->type == typeCon){
        			if (!id_flag){
        			fprintf(yyout,"mov %s,%d\n",symbols[sym_index], (p->opr.op[1])->con.value); 
        			
        			}
        		}*/
        		else if (p->opr.op[1]->type == typeId) {
        			
        			fprintf(yyout,"mov %s,%s\n",symbols[sym_index], (p->opr.op[1])->id.id_name); 
        			
        		}
        		
              break;

        case uminus:    
            ex(p->opr.op[0]);
            fprintf(yyout,"\tneg\n");
            break;
            case if_statement:
            ex(p->opr.op[0]);
            if (p->opr.nops > 2) {
                /* if else */
               
				/*fprintf(yyout,"cmp R%d,1\n",(p->opr.op[0])->con.registerNumber);
                fprintf(yyout,"jz\tL%03d\n", lbl1 = lbl++);
                */
                int t = conditionlbl;
                op1RegNum = ((p->opr.op[0])->opr.op[0])->registerNumber;
            	op2RegNum = ((p->opr.op[0])->opr.op[1])->registerNumber;
                ex(p->opr.op[2]);
                fprintf(yyout,"jmp\tL%03d\n", lbl2 = lbl++);
                fprintf(yyout,"L%03d:\n", t);
                ex(p->opr.op[1]);
                fprintf(yyout,"L%03d:\n", lbl2);
            } 
            else {
                /* if */
                op1RegNum = ((p->opr.op[0])->opr.op[0])->registerNumber;
            	op2RegNum = ((p->opr.op[0])->opr.op[1])->registerNumber;
                fprintf(yyout,"jmp\tL%03d\n", lbl2 = lbl++);
                fprintf(yyout,"L%03d:\n", conditionlbl);
                ex(p->opr.op[1]);
                fprintf(yyout,"L%03d:\n", lbl2);
            }
            break;
            case switch_statement:
            switch_var = p->opr.op[0]->id.id_name;
            conditionFlag = 1;
            ex(p->opr.op[0]);
            ex(p->opr.op[1]);
            fprintf(yyout,"L%03d:\n", lastlbl);
 			break;          
            case case_statement:
            conditionFlag = 1;
           // fprintf(yyout,"L%03d:\n", lbl1 = lbl++);
            ex(p->opr.op[0]);   
            fprintf(yyout,"cmp %s,%d\n",switch_var,(p->opr.op[0])->con.value);
            fprintf(yyout,"jz\tL%03d\n", lbl1 = lbl++);
            fprintf(yyout,"jmp L%03d\n", lbl1+1); default_lbl = lbl1+1;
            fprintf(yyout,"L%03d:\n", lbl1);
            ex(p->opr.op[1]);
            fprintf(yyout,"jmp L%03d\n", lastlbl);
            break;
            case default_command:
            fprintf(yyout,"L%03d:\n", default_lbl);
            ex(p->opr.op[0]);
            break;
        default:
        	
            if (p->opr.oper == '>' || p->opr.oper == '<' || p->opr.oper == equals_operator || p->opr.oper == not_equal_operator || p->opr.oper == smaller_or_equal_operator
             || p->opr.oper == greater_or_equal_operator){
            	logex_flag = 1;
            	conditionFlag = 1;
            }
            if ((p->opr.op[0]->type == typeOpr || p->opr.op[1]->type == typeOpr ) && conditionFlag){
            	opr_flag = 1;
            }
        	ex(p->opr.op[0]); ex(p->opr.op[1]);
        	lastRegNum = p->registerNumber;
            lastOp1RegNum = p->opr.op[0]->registerNumber;
            lastOp2RegNum = p->opr.op[1]->registerNumber;

            switch(p->opr.oper) {
            case '+': 
            fprintf(yyout,"add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber);
			break;
            case '-':  
            fprintf(yyout,"sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber);
			case '*':
			fprintf(yyout,"mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber);
            break;
            case '/':
            fprintf(yyout,"div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber);	
             break;

            case '<':   

            			if (p->opr.op[1]->type == typeCon){
                        fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],p->opr.op[1]->con.value);
                    	}
                    	else if (p->opr.op[1]->type == typeId){
                    	fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],p->opr.op[1]->id.id_name);
                    	}	
                    	else{
                    	fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],lastRegNum);
                    	}
            			fprintf(yyout,"jb L%03d\n",conditionlbl = lbl++);


            			break;
            case '>':   
            			if (p->opr.op[1]->type == typeCon){
                        fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],p->opr.op[1]->con.value);
                    	}
                    	else if (p->opr.op[1]->type == typeId){
                    	fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],p->opr.op[1]->id.id_name);
                    	}	
                    	else{
                    	fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],lastRegNum);
                    	}
            			fprintf(yyout,"ja L%03d\n",conditionlbl = lbl++);
            			
            			break;
            case greater_or_equal_operator:   
            			if (p->opr.op[1]->type == typeCon){
                        fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],p->opr.op[1]->con.value);
                    	}
                    	else if (p->opr.op[1]->type == typeId){
                    	fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],p->opr.op[1]->id.id_name);
                    	}	
                    	else{
                    	fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],lastRegNum);
                    	}
            			fprintf(yyout,"jae L%03d\n",conditionlbl = lbl++);
            			break;
            case smaller_or_equal_operator:   
            			if (p->opr.op[1]->type == typeCon){
                        fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],p->opr.op[1]->con.value);
                    	}
                    	else if (p->opr.op[1]->type == typeId){
                    	fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],p->opr.op[1]->id.id_name);
                    	}	
                    	else{
                    	fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],lastRegNum);
                    	}
            			fprintf(yyout,"jbe L%03d\n",conditionlbl = lbl++);
            			break;
            case not_equal_operator:    	  
            			if (p->opr.op[1]->type == typeCon){
                        fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],p->opr.op[1]->con.value);
                    	}
                    	else if (p->opr.op[1]->type == typeId){
                    	fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],p->opr.op[1]->id.id_name);
                    	}	
                    	else{
                    	fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],lastRegNum);
                    	}
            			fprintf(yyout,"jnz L%03d\n",conditionlbl = lbl++);
            			break;
            case equals_operator:    		  
            			if (p->opr.op[1]->type == typeCon){
                        fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],p->opr.op[1]->con.value);
                    	}
                    	else if (p->opr.op[1]->type == typeId){
                    	fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],p->opr.op[1]->id.id_name);
                    	}	
                    	else{
                    	fprintf(yyout,"cmp %s,%d\n",symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)],lastRegNum);
                    	}
            			fprintf(yyout,"jz L%03d\n",conditionlbl = lbl++);
            			break;
        }
        break;
    }
    return 0;
}
}