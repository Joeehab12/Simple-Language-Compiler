	%{
	void yyerror (char *s);
	#include <stdio.h>
	#include <string.h>
	#include <stdlib.h>
	#include <gtk/gtk.h>
	#include <stdarg.h>
	#include "compiler.h"
	extern int yylex();
	extern int yylineno;
	extern char* yytext;
	static void destroy(GtkWidget*,gpointer);
	nodeType *opr(int oper , int nops, ...);
	nodeType *id(char* id_name);
	nodeType *con(int value);
	void freeNode(nodeType *p);
	int ex(nodeType *p);
	char* symbols[200];
	int values[200];
    extern FILE* yyin;
    extern FILE* yyout;
    int count = 0;
    int idcount = 0;
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
%type <nPtr> assignment declaration logex line_list casestmt block case_list defaultstmt 
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
	 |  if_statement '(' logex ')' '{' line_list '}' {$$ = opr(if_statement,2,$3,$6);}
	 |	if_statement '(' logex ')' '{' line_list '}' else_statement '{' line_list '}' {$$ = opr(if_statement,3,$3,$6,$10);}
	 | 	switch_statement '(' identifier ')' '{' block '}'			{printf("identifier = %s\n",$3); $$ = opr(switch_statement,2,id($3),$6);}
;

block:     case_list defaultstmt					{$$ = opr(';',2,$1,$2);}
;

defaultstmt: default_command ':' line_list	break_command ';'					{$$ = $3; lastlbl++;}
;

casestmt:  case_statement integer ':' line_list break_command ';'	{printf("breaked!\n"); $$ = opr(case_statement,2,con($2),$4);lastlbl++;}
;

case_list: casestmt 								{;}
		 |case_list casestmt 						{$$ = opr(';',2,$1,$2);}
;

logex : integer			{$$ = con($1);}
;

line_list : line {;} 
			| line_list line {$$ = opr(';',2,$1,$2);}
			;

declaration: type_int identifier {printf("variable type = %s \n",$1); $$ = id($2);}
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
GtkWidget *window;
int parse(){
	
	int i;
	for (i=0;i<200;i++){
		symbols[i]= "\0";
		values [i] = 0;
	}
	
	return 0;
	
	//fflush(;
}

static void choose_file(GtkWidget* app, gpointer user_data,char *filename){
	GtkWidget *dialog;
GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_OPEN;
gint res;
GtkWindow *parent_window = GTK_WINDOW(window);
dialog = gtk_file_chooser_dialog_new ("Open File",
                                       parent_window,
                                      action,
                                      "Cancel",
                                      GTK_RESPONSE_CANCEL,
                                      "Open",
                                      GTK_RESPONSE_ACCEPT,
                                      NULL);

res = gtk_dialog_run (GTK_DIALOG (dialog));
if (res == GTK_RESPONSE_ACCEPT)
  {
    char *filename;
    GtkFileChooser *chooser = GTK_FILE_CHOOSER (dialog);
    filename = gtk_file_chooser_get_filename (chooser);
    printf("%s\n",filename);
    yyin = fopen(filename,"r");
    yyout = fopen("output.quad","w");
   // open (filename);
    //g_free (filename);
  }

 gtk_widget_destroy (dialog);

}
static gboolean killOffApp (gpointer userData) {
    g_application_quit (userData); // << and here  
    //gtk_widget_hide (window);
    return FALSE;
}
static void activate (){
	
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (window), "Yet Another Compiler Compiler");
  gtk_window_set_default_size (GTK_WINDOW (window), 200, 200);
  g_signal_connect (window, "destroy", G_CALLBACK (destroy), window);
  GtkWidget *button1,*button2,*button_box1,*label;


 



  button_box1 = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
  gtk_container_add (GTK_CONTAINER (window), button_box1);

  label = gtk_label_new("Browse source code file: ");
    gtk_container_add (GTK_CONTAINER (button_box1), label);

   button1 = gtk_button_new_with_label ("Browse");
  g_signal_connect (button1, "clicked", G_CALLBACK (choose_file), NULL);
  //g_signal_connect_swapped (button, "clicked", G_CALLBACK (gtk_widget_destroy), window);
  gtk_container_add (GTK_CONTAINER (button_box1), button1);


  button2 = gtk_button_new_with_label ("Compile");
  g_signal_connect_swapped(button2, "clicked", G_CALLBACK (gtk_widget_destroy), (gpointer) window);
  //g_signal_connect_swapped (button, "clicked", G_CALLBACK (gtk_widget_destroy), window);
  gtk_container_add (GTK_CONTAINER (button_box1), button2);


  gtk_widget_show_all (window);

}
static void destroy(GtkWidget* window,gpointer data){
		gtk_main_quit();
}


void yyerror (char *s){
	fprintf(stderr,"\n%s at line number: %d \n",s,yylineno);
}
void test(){
	int i;
	for (i = 0 ;i<5;i++){
		//printf("\nsymbol %d is %s \n",i,symbols[i]);
		}
}

int main(void){
	//= fopen("yyoutput.c","w");
 GtkApplication *app;
  int status;
  gtk_init(0,NULL);
  activate();
  gtk_main();
 /* app = gtk_application_new ("org.gtk.example", G_APPLICATION_FLAGS_NONE);
  g_signal_connect (app, "activate", G_CALLBACK (activate), NULL);
  status = g_application_run (G_APPLICATION (app),0,NULL);
  g_object_unref (app);

		
  fprintf(yyout,("before yyparse()\n");
  int i;

	
  */
  
   
   yyparse(); 
   fclose(yyin);
   fclose(yyout);
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
        yyerror("yyout of memory");

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
        yyerror("yyout of memory");

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
        yyerror("yyout of memory");

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

  static int lbl = 0;

  int ex(nodeType *p) {
    int lbl1, lbl2;
    int x;	
    if (!p) return 0;
    switch(p->type) {
    case typeCon:       
        fprintf(yyout,"mov R%d,%d\n",(p->con.registerNumber), p->con.value); 
        break;
    case typeId:        
        fprintf(yyout,"mov R%d,%s\n",(p->id.registerNumber), p->id.id_name); 
        break;
    case typeOpr:
        switch(p->opr.oper) {      
        case cout_command:     
            fprintf(yyout,"\tprint\n");
            break;
        case '=':
        		ex(p->opr.op[1]);
              break;
        case uminus:    
            ex(p->opr.op[0]);
            fprintf(yyout,"\tneg\n");
            break;


                    case if_statement:
            ex(p->opr.op[0]);
            if (p->opr.nops > 2) {
                /* if else */
               
				fprintf(yyout,"cmp R%d,%d",(p->opr.op[0])->con.registerNumber,(p->opr.op[0])->con.value);
				
                fprintf(yyout,"\tjz\tL%03d\n", lbl1 = lbl++);
                ex(p->opr.op[1]);
                fprintf(yyout,"\tjmp\tL%03d\n", lbl2 = lbl++);
                fprintf(yyout,"L%03d:\n", lbl1);
                ex(p->opr.op[2]);
                fprintf(yyout,"L%03d:\n", lbl2);
            } 
            else {
                /* if */
               
				fprintf(yyout,"cmp R%d,%d",(p->opr.op[0])->con.registerNumber,(p->opr.op[0])->con.value);
				
                fprintf(yyout,"\tjz\tL%03d\n", lbl1 = lbl++);
                ex(p->opr.op[1]);
                fprintf(yyout,"L%03d:\n", lbl1);
            }
            break;
            case switch_statement:
            ex(p->opr.op[0]);
            ex(p->opr.op[1]);
            fprintf(yyout,"L%03d:\n", lastlbl);
 			break;          
            case case_statement:
            ex(p->opr.op[0]);
            fprintf(yyout,"cmp R%d,R%d\n",(p->opr.op[0])->con.registerNumber,idcount);
            fprintf(yyout,"jz\tL%03d\n", lbl1 = lbl++);
       		ex(p->opr.op[1]);
            fprintf(yyout,"L%03d:\n", lbl1);
            fprintf(yyout,"jmp L%03d\n", lastlbl);
            break;
        default:
        	 ex(p->opr.op[0]); ex(p->opr.op[1]);
            switch(p->opr.oper) {
            case '+': 
            if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeOpr ){
            fprintf(yyout,"add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeOpr ){
			fprintf(yyout,"add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeCon ){
			fprintf(yyout,"add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeCon ){
			fprintf(yyout,"add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeId ){
			fprintf(yyout,"add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeCon ){
			fprintf(yyout,"add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeOpr ){
			fprintf(yyout,"add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeId ){
			fprintf(yyout,"add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			else{
			fprintf(yyout,"add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			break;
            case '-':  
            if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeOpr ){
            fprintf(yyout,"sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeOpr ){
			fprintf(yyout,"sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeCon ){
			fprintf(yyout,"sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeCon ){
			fprintf(yyout,"sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeId ){
			fprintf(yyout,"sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeCon ){
			fprintf(yyout,"sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeOpr ){
			fprintf(yyout,"sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeId ){
			fprintf(yyout,"sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			else{
			fprintf(yyout,"sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
             break;
            case '*': 
            if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeOpr ){
            fprintf(yyout,"mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeOpr ){
			fprintf(yyout,"mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeCon ){
			fprintf(yyout,"mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeCon ){
			fprintf(yyout,"mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeId ){
			fprintf(yyout,"mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeCon ){
			fprintf(yyout,"mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeOpr ){
			fprintf(yyout,"mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeId ){
			fprintf(yyout,"mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			else{
			fprintf(yyout,"mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
             break;
            case '/':
            if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeOpr ){
            fprintf(yyout,"div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeOpr ){
			fprintf(yyout,"div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeCon ){
			fprintf(yyout,"div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeCon && (p->opr.op[1])->type == typeCon ){
			fprintf(yyout,"div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeId ){
			fprintf(yyout,"div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);

			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeCon ){
			fprintf(yyout,"div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->con.registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeId && (p->opr.op[1])->type == typeOpr ){
			fprintf(yyout,"div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->id.registerNumber,(p->opr.op[1])->registerNumber/*count,count-1*/);
			}
			else if ((p->opr.op[0])->type == typeOpr && (p->opr.op[1])->type == typeId ){
			fprintf(yyout,"div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
			else{
			fprintf(yyout,"div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->con.registerNumber,(p->opr.op[1])->id.registerNumber/*count,count-1*/);
			}
             break;
            case '<':   fprintf(yyout,"\tcompLT\n"); break;
            case '>':   fprintf(yyout,"\tcompGT\n"); break;
            case greater_or_equal_operator:    fprintf(yyout,"\tcompGE\n"); break;
            case smaller_or_equal_operator:    fprintf(yyout,"\tcompLE\n"); break;
            case not_equal_operator:    fprintf(yyout,"\tcompNE\n"); break;
            case equals_operator:    fprintf(yyout,"\tcompEQ\n"); break;
        }
        break;
    }
    return 0;
}
}