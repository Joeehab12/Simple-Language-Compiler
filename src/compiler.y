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
    FILE * errors;
    FILE * symbol_file;
    void destroy();
    int progress_timeout();
    void progress();
    int syntax_flag = 0;
    nodeType *opr(int oper , int nops, ...);
    nodeType *id(char* id_name);
    nodeType *con(int value);
    nodeType *conf(float value);
    void freeNode(nodeType *p);
    //void initCheck(char* symbol);
    int stfull();
    int stempty();
    void push(int item);
    int pop();
    void display();
    int check_variable_level(char* symbol);
    char* getType(char* symbol);
    void setType(char* symbol,char *type);
    void declaredCheck(char* symbol);
    void checkUsed();
    int computeSymbolIndex(char* symbol);
    int computeFunctionIndex(char* function);
    int symbolVal(char* symbol);
    void updateSymbol(char *symbol);
    void updateSymbolVal(char *symbol,int num);
    void updateFunction(char *function);
    void symbol_init();
    void printSymbolTable();
    int ex(nodeType *p,int label);
    char* symbols[200];
    char* functions[200];
    int levels[200];
    int values[200];
    int used[200];
    int init[200];
    char* types[200];
    int cons[200];
    char* filename;
    int registerNumbers[200];
    extern FILE* yyin;
    extern FILE* yyout;
    int count = 0;
    int forcnt = 0;
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
    int post = 0;
    int pre = 0;
    char * sym_tables[200][200];
    static int lastlbl = 0;
    int fnflag = 0;
    int *lblarr;
    int lblindex = 0;
    int switchcnt = 0;
    int conditionRegister = 0;
    int scopecnt = 0;
    int cons_flag = 0;
struct stack {
   int s[200];
   int top;
} st;
%}
%error-verbose

%union{int ival; char *id; char* type; char c; float fval; nodeType* nPtr;};

%token type_void
%token <type>type_int
%token <type>type_float
%token <type>type_char
%token <type>type_bool
%token constant
%token left_shift
%token <val>true_case
%token <val>false_case
%token cout_command
%token increment
%token decrement
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
%token not_operator
%token return_command
%token repeat
%token until
%type <nPtr> line expr term line_list 
%type <nPtr> assignment declaration  casestmt case_list   arg_list_header
%type <id> return_type id_list fn_declaration 
%left '+' '-'
%left '*' '/'
%left greater_or_equal_operator smaller_or_equal_operator equals_operator not_equal_operator '>' '<' and_operator or_operator '!' ',' 
%nonassoc uminus 

%%
program:    function                    {if (!syntax_flag){checkUsed();} printSymbolTable(); exit(0);}
;

function:   function line                       {  ex($2,0);  freeNode($2);}
        |   function error   "\n"                      { syntax_flag=1; yyerrok;}
|
;

line : //';'                               {;}
        declaration ';'                  {$$->declarationFlag = 1;}
     |  assignment ';'                  {;}
     |  expr ';'                     {; conditionFlag=0;}
     |  cout_command left_shift expr ';'            { $$ = opr(cout_command,1,$3);}
     |  if_statement '(' expr ')' left_par line_list right_par {$$ = opr(if_statement,2,$3,$6); logex_flag = 1; id_flag = 0; conditionFlag = 0;  }
     |  if_statement '(' expr ')' left_par line_list right_par else_statement left_par line_list right_par {$$ = opr(if_statement,3,$3,$6,$10); conditionFlag = 0; }
     |  switch_statement '(' identifier ')' left_par case_list   default_command ':' line_list  break_command ';'right_par        {  check_variable_level($3);  switchcnt++; $$ = opr(switch_statement,3,id($3),$6,$9); used[computeSymbolIndex($3)] = 1;}
     |  while_loop '(' expr ')' left_par line_list right_par     { $$ = opr(while_loop, 2, $3, $6); logex_flag = 1; conditionFlag = 0; }
     |  repeat left_par line_list right_par until '(' expr ')' ';' { $$ = opr(repeat, 2, $3, $7); logex_flag = 1; conditionFlag = 0;  }
     |  for_loop '(' assignment ';' expr ';' expr ')' left_par line_list right_par { forcnt++; $$ = opr(for_loop, 4, $3, $5, $7, $10); logex_flag = 1; conditionFlag = 0; }
     |  fn_declaration '(' arg_list_header ')' left_par line_list right_par                    {$$=opr(return_command,3,id($1),$3,$6); fnflag = 1;conditionFlag=0; }
     |  fn_declaration '(' ')' left_par line_list right_par                    {$$=opr(return_command,2,id($1),$5); fnflag = 1;conditionFlag=0; }
     |  identifier '(' id_list ')'   ';'                  {updateFunction($1); $$ = opr(break_command,2,id($1),id($3));conditionFlag=0;}
     |  identifier '(' ')'   ';'                  {updateFunction($1); $$ = opr(break_command,1,id($1));conditionFlag=0;}
    //|  return_type identifier '('arg_list ')' '{'  line_list '}' {;}
    //  | error ';'            {yyclearin; /* discard lookahead */printf("error!\n"); yyerrok;}
    // | error '}'            {yyclearin; /* discard lookahead */ yyerrok;}
;


left_par: '{'  {scopecnt++; push(scopecnt); display();}
;
right_par: '}' {pop();}
;
arg_list_header: arg_list_header ',' declaration {$$ = $3; }
        |declaration     {$$=$1; $$->declarationFlag=1; }
;


return_type:  type_int  {; }
            | type_float { ; }
            | type_char {; } 
;

/*defaultstmt: default_command ':' line_list    break_command ';'                   {$$ = opr(default_command,1,$3); lastlbl++;}
;*/

casestmt:  case_statement integer ':' line_list break_command ';'   { $$ = opr(case_statement,2,con($2),$4);lastlbl+=2;}
;

case_list: casestmt                                 {;}
         |case_list casestmt                        {$$ = opr(';',2,$1,$2);}
;


/*condition: logex and_operator logex {$$ = opr(and_operator,2,$1,$3); conditionFlag = 1;  }
      | logex or_operator logex {$$ = opr(or_operator,2,$1,$3); conditionFlag = 1;}
      | logex not_equal_operator logex {$$ = opr(not_equal_operator,2,$1,$3); conditionFlag = 1;}
      //| '(' logex ')'           {$$=$2;}
;
*/
/*
logex : 
      condition       {;}
      | '!' logex                    {$$ = opr('!',1,$2);  conditionFlag = 1;}
      | expr equals_operator expr    {$$ = opr(equals_operator,2,$1,$3); conditionFlag = 1;}
      | expr '>' expr    {$$ = opr('>',2,$1,$3); conditionFlag = 1;}
      | expr greater_or_equal_operator  expr { $$ = opr(greater_or_equal_operator,2,$1,$3); conditionFlag = 1;}
      | expr '<' expr    {$$ = opr('<',2,$1,$3); $$->conditionFlag = 1; conditionFlag = 1;}
      | expr smaller_or_equal_operator expr {$$ = opr(smaller_or_equal_operator,2,$1,$3); conditionFlag = 1;}
     // | integer            {$$=id($1);}
    // | '(' logex ')'           {$$=$2;}
     |expr {;}
;

*/

line_list : line {$$ = $1;} 
            | line_list line {$$ = opr(';',2,$1,$2);}
;

/*declaration:  type_int identifier {if (!fnflag){$$ = id($2); updateSymbol($2);  setType($2,$1);} }
            | type_float identifier{if (!fnflag){$$ = id($2); updateSymbol($2);  setType($2,$1);}  }
            | type_char identifier{if (!fnflag){$$ = id($2); updateSymbol($2);  setType($2,$1); } }
            | type_bool identifier{if (!fnflag){$$ = id($2); updateSymbol($2);  setType($2,$1); } } 
;*/
id_list : id_list ',' identifier        {$$=$3;}
|           identifier              {$$=$1; }            
;


declaration:  return_type identifier {$$ = id($2); updateSymbol($2);  setType($2,$1); levels[computeSymbolIndex($2)] = scopecnt;}
            | constant return_type identifier {cons_flag = 1; $$ = id($3);  updateSymbol($3);  cons[computeSymbolIndex($3)] = 1; setType($3,$2); levels[computeSymbolIndex($2)] = scopecnt;}
            ;

fn_declaration: return_type identifier{$$ = $2;}
            
;

assignment :  identifier '=' expr       {if (cons[computeSymbolIndex($1)]){
	char * msg = malloc(300*sizeof(char));
	strcpy(msg,"value of constant variable '");
	strcat(msg,$1);
	strcat(msg,"' cannot be modified");
	yyerror(msg);} $$ = opr('=',2,id($1),$3);  init[computeSymbolIndex($1)] = 1; if (!syntax_flag){declaredCheck($1);}  used[computeSymbolIndex($1)] = 1; check_variable_level($1); }
           |  declaration '=' expr      { $$ = opr('=',2,$1,$3); $1->declarationFlag = 1; init[computeSymbolIndex($1->id.id_name)] = 1; used[computeSymbolIndex($1->id.id_name)] = 1; check_variable_level($1->id.id_name); }
         //  | identifier '=' logex       {$$ = opr('=',2,id($1),$3); }
;

expr :  term        {$$=$1; $$->declarationFlag = 0;}
	| '-' expr %prec uminus	 {$$ = opr(uminus,1,$2);}
    |   expr '+' expr   {$$ = opr('+',2,$1,$3); }
    |   expr '-' expr   {$$ = opr('-',2,$1,$3); }
    |   expr '*' expr   {$$ = opr('*',2,$1,$3); }
    |   expr '/' expr   {$$ = opr('/',2,$1,$3); }
    |   identifier increment {$$=opr(increment,2,id($1),con(1)); id($1)->declarationFlag = 0; used[computeSymbolIndex($1)] = 1; check_variable_level($1);}
    |   identifier decrement {$$=opr(decrement,2,id($1),con(1));id($1)->declarationFlag = 0; used[computeSymbolIndex($1)] = 1; check_variable_level($1);}
    |   increment identifier {$$=opr(increment,2,id($2),con(1));id($2)->declarationFlag = 0; used[computeSymbolIndex($2)] = 1; check_variable_level($2);}
    |   decrement identifier {$$=opr(decrement,2,id($2),con(1));id($2)->declarationFlag = 0; used[computeSymbolIndex($2)] = 1; check_variable_level($2);}
    |   '(' expr ')'    {$$ = $2; }
    | expr equals_operator expr    {$$ = opr(equals_operator,2,$1,$3); conditionFlag = 1;}
    | expr '>' expr    {$$ = opr('>',2,$1,$3); conditionFlag = 1;}
    | expr greater_or_equal_operator  expr { $$ = opr(greater_or_equal_operator,2,$1,$3); conditionFlag = 1;}
    | expr '<' expr    {$$ = opr('<',2,$1,$3); $$->conditionFlag = 1; conditionFlag = 1;}
    | expr smaller_or_equal_operator expr {$$ = opr(smaller_or_equal_operator,2,$1,$3); conditionFlag = 1;}
    | expr and_operator expr {$$ = opr(and_operator,2,$1,$3); conditionFlag = 1;  }
    | expr or_operator expr {$$ = opr(or_operator,2,$1,$3); conditionFlag = 1;}
    | expr not_equal_operator expr {$$ = opr(not_equal_operator,2,$1,$3); conditionFlag = 1;}
    ;

term :  integer         {equal_flag = 1; $$ = con($1);   $$->con.type = integer_num;  }
     | identifier       {equal_flag = 1;  $$ = id($1);   used[computeSymbolIndex($1)] = 1; if (!syntax_flag){declaredCheck($1);} check_variable_level($1); }
     | character        {equal_flag = 1; $$ = con($1); $$->con.type = integer_num; }
     | float_number     {equal_flag = 1; $$ = conf($1); $$->con.type = float_num;}
;


%%  
/*void yyerror (char *s){
    fprintf(errors,"\n%s at line number: %d \n",s,yylineno);
}
*/

void display() {
   int i;
   if (stempty())
      printf("\nStack Is Empty!");
   else {
   	printf ("Stack: ");
      for (i = st.top; i >= 0; i--)
         printf("%d ", st.s[i]);
     	 printf("\n");
   }
}

int stfull() {
   if (st.top >= 200 - 1)
      return 1;
   else
      return 0;
}
 
void push(int item) {
   st.top++;
   st.s[st.top] = item;
}
 
int stempty() {
   if (st.top == -1)
      return 1;
   else
      return 0;
}
 
int pop() {
   int item;
   item = st.s[st.top];
   st.top--;
   return (item);
}

/*
GtkWidget *window;
GtkWidget *progressbar1,*button1,*button2,*button3,*button4,*label1,*label2,*label3,*label4,*grid;
/*
void choose_file(GtkWidget* app, gpointer user_data){
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

void save_file(GtkWidget* app, gpointer user_data){
	GtkWidget *dialog;
GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_SAVE;
gint res;
GtkWindow *parent_window = GTK_WINDOW(window);
dialog = gtk_file_chooser_dialog_new ("Save Output Quadruples",
                                       parent_window,
                                      action,
                                      "Cancel",
                                      GTK_RESPONSE_CANCEL,
                                      "Save",
                                      GTK_RESPONSE_ACCEPT,
                                      NULL);

res = gtk_dialog_run (GTK_DIALOG (dialog));
if (res == GTK_RESPONSE_ACCEPT)
  {
    GtkFileChooser *chooser = GTK_FILE_CHOOSER (dialog);
    filename = gtk_file_chooser_get_filename (chooser);
    printf("%s\n",filename);
    yyout = fopen(filename,"w");
   // open (filename);
    //g_free (filename);
  }

 gtk_widget_destroy (dialog);

}
*/
/*
void choose_file(){
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
    GtkFileChooser *chooser = GTK_FILE_CHOOSER (dialog);
    filename = gtk_file_chooser_get_filename (chooser);
    printf("%s\n",filename);
    yyin = fopen(filename,"r");
   // yyout = fopen("output.quad","w");
   // open (filename);
    //g_free (filename);
  }

 gtk_widget_destroy (dialog);

}

void save_file(){
	GtkWidget *dialog;
GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_SAVE;
gint res;
GtkWindow *parent_window = GTK_WINDOW(window);
dialog = gtk_file_chooser_dialog_new ("Save Output Quadruples",
                                       parent_window,
                                      action,
                                      "Cancel",
                                      GTK_RESPONSE_CANCEL,
                                      "Save",
                                      GTK_RESPONSE_ACCEPT,
                                      NULL);

res = gtk_dialog_run (GTK_DIALOG (dialog));
if (res == GTK_RESPONSE_ACCEPT)
  {
    GtkFileChooser *chooser = GTK_FILE_CHOOSER (dialog);
    filename = gtk_file_chooser_get_filename (chooser);
    printf("%s\n",filename);
    yyout = fopen(filename,"w");
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
/*static void activate (){
	
	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (window), "Yet Another Compiler Compiler");
  gtk_window_set_default_size (GTK_WINDOW (window), 200, 200);
  g_signal_connect (window, "destroy", G_CALLBACK (destroy), window);
 

table = gtk_grid_new();
  gtk_container_add (GTK_CONTAINER (window), table);




  label = gtk_label_new("Browse source code file: ");
    gtk_grid_attach (GTK_GRID(table), label,0,0,2,1);

   button1 = gtk_button_new_with_label ("Browse");
  g_signal_connect (button1, "clicked", G_CALLBACK (choose_file), NULL);
  //g_signal_connect_swapped (button, "clicked", G_CALLBACK (gtk_widget_destroy), window);
  //gtk_container_add (GTK_CONTAINER (table), button1);
gtk_grid_attach (GTK_GRID(table), button1,6,0,1,1);


  label2 = gtk_label_new("Save output quadruples as: ");
    gtk_grid_attach (GTK_GRID (table), label2,0,4,2,1);


  button2 = gtk_button_new_with_label ("Save As");
  g_signal_connect_swapped(button2, "clicked", G_CALLBACK (save_file), (gpointer) window);
  //g_signal_connect_swapped (button, "clicked", G_CALLBACK (gtk_widget_destroy), window);
  gtk_grid_attach (GTK_GRID(table), button2,6,4,1,1);

  label3 = gtk_label_new("Click to compile: ");
    gtk_grid_attach (GTK_GRID (table), label3,0,8,2,1);

  button3 = gtk_button_new_with_label ("Compile");
  g_signal_connect_swapped(button3, "clicked", G_CALLBACK (gtk_widget_destroy), (gpointer) window);
  //g_signal_connect_swapped (button, "clicked", G_CALLBACK (gtk_widget_destroy), window);
 gtk_grid_attach (GTK_GRID (table), button3,6,8,1,1);


  gtk_widget_show_all (window);

}
*/
/*
 void destroy(){
		gtk_main_quit();
}


	GtkWidget	*widget;
*/
int main(int argc,char *argv[]){
	/*
	GtkBuilder      *builder; 
	GtkWidget *button1,*button2,*button3,*button4,*label1,*label2,*label3,*label4,*grid;

 int status;
  gtk_init(0,NULL);

	builder = gtk_builder_new();
    gtk_builder_add_from_file (builder, "/home/youssef/Desktop/CompilerGUI.glade", NULL);
    window = GTK_WIDGET(gtk_builder_get_object(builder, "window1"));
    gtk_builder_connect_signals(builder, NULL);
 	button1 = GTK_WIDGET(gtk_builder_get_object(builder, "button1"));
    button2 = GTK_WIDGET(gtk_builder_get_object(builder, "button2"));
 	button3 = GTK_WIDGET(gtk_builder_get_object(builder, "button3"));
 	button4 = GTK_WIDGET(gtk_builder_get_object(builder, "button4"));
 	label1 = GTK_WIDGET(gtk_builder_get_object(builder, "label1"));
    label2 = GTK_WIDGET(gtk_builder_get_object(builder, "label2"));
 	label3 = GTK_WIDGET(gtk_builder_get_object(builder, "label3"));
 	label4 = GTK_WIDGET(gtk_builder_get_object(builder, "label4"));
 	grid = GTK_WIDGET(gtk_builder_get_object(builder, "grid1"));
 	
    g_object_unref(G_OBJECT(builder));
*/
	/*
	printf("mono thread attach\n");
	mono_thread_attach(domain);
	printf("mono domain assembly open\n");
	assembly = mono_domain_assembly_open(domain, "/home/youssef/Documents/test/test/bin/test.exe");
	if (!assembly) printf("FAIL\n");
	printf("mono jit exec\n");
	mono_jit_exec(domain,assembly,argc-1,argv+1);
	printf("mono assembly get image\n");
	image = mono_assembly_get_image(assembly);
	if (!image) printf("FAIL\n");
	printf("mono class from name\n");
	monoclass = mono_class_from_name (image, "MonoTestSpace", "MonoTest");
	if (!monoclass) printf("FAIL\n");
	printf("get method\n");
	iter = NULL;
	while ((m = mono_class_get_methods (monoclass, &iter)))
		if (strcmp (mono_method_get_name (m), "Test") == 0)
			method = m;		
	if (!method) printf("FAIL\n");

	printf("mono object new\n");
	obj = mono_object_new(domain,monoclass);
	if (!obj) printf("FAIL\n");
	printf("mono runtime object init\n");
	mono_runtime_object_init(obj);
	gpointer args[1];
	widget = gtk_frame_new(0);
	args[0] = &widget; 	
	printf("mono runtime invoke\n");
	mono_runtime_invoke(method,obj,args,NULL);
*/


    symbol_init();
  /* filename = malloc(20*sizeof(char));
   printf("Please enter source file name: ");
   scanf("%s",filename);
   */
//   GtkApplication *app;
   //activate();
  /*  gtk_widget_show(window);
  gtk_main();
*/
		
	yyin = fopen(argv[1],"r");
	yyout = fopen(argv[2],"w"); 
  errors = fopen("errors.txt","w");
  symbol_file = fopen("symbols.txt","w");
	filename  = argv[1];
   yyparse();
    
   fclose(yyin);
   fclose(yyout);
   return 0;
}
void yyerror(const char *msg){
    fprintf(errors,"%s:%d: %s\n",filename,yylineno,msg);
}

void printSymbolTable(){
    int i;
    fprintf(symbol_file,"symbol:\ttype:\n");
    for (i = 0;i<10;i++){
        if (strcmp(symbols[i],"\0")){
        fprintf(symbol_file,"%s\t%s\n",symbols[i],types[computeSymbolIndex(symbols[i])]);
      }
    }
    printf("\n");
    printf("levels: ");
    for (i = 0;i<10;i++){
        printf("%d ",levels[i]);
    }
    printf("\n");
}
void symbol_init(){
    int i;
    for(i = 0;i<200;i++){
        symbols[i] = "\0";
        levels[i] = 0;
        values[i] = 0;
        used[i] = 0;
        types[i] = "\0";
        init[i] = 0;
        cons[i] = 0;
        functions[i]="\0";
        registerNumbers[i] = 0;
    }
}

int check_variable_level(char* symbol){
	if (levels[computeSymbolIndex(symbol)] > st.s[st.top]){
		display(); printf("top = %d\n",st.top); 	
		char * msg = malloc (300*sizeof(char));
		strcpy(msg,"variable '");
		strcat(msg,symbol);
		strcat(msg,"' ");
		strcat(msg,"was not declared in this scope.");
		yyerror(msg);
		return 0;
	}
	else{
		return 1;
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

void updateFunction(char *function){
    int i;
    err = malloc(50*sizeof(char));
    for (i = 0; i<200;i++){
        if (!strcmp(functions[i],"\0")){
            functions[i]= function;
            break;      
        }
        else if (!strcmp(functions[i],function)){
            strcpy(err,"error: redeclaration of function ");
            strcat(err,function);
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

int computeFunctionIndex(char* function){
    int i;
    int index = 0;
    for (i = 0; i<200;i++){
        if (!strcmp(functions[i],function)){
            index = i;
            break;  
        }
    }
    return index;
}


/*void initCheck(char* symbol){
    if (!init[computeSymbolIndex(symbol)]){
        char* msg = malloc(200*sizeof(char));
        strcpy(msg,"variable '");
        strcat(msg,symbol);
        strcat(msg,"' is used without being initialized");
        yyerror(msg);
    }
}*/
void declaredCheck(char* symbol){
    int found = 0;
    int i;
    for (i = 0; i<200;i++){
        if (!strcmp(symbols[i],symbol)){
            found = 1;
            break;  
        }
    }
    if (!found){
        char* msg = malloc(200*sizeof(char));
        strcpy(msg,"variable '");
        strcat(msg,symbol);
        strcat(msg,"' is used without being declared");
        yyerror(msg);
    }
    else{
        if (!init[computeSymbolIndex(symbol)]){
        char* msg = malloc(200*sizeof(char));
        strcpy(msg,"variable '");
        strcat(msg,symbol);
        strcat(msg,"' is used without being initialized");
        yyerror(msg);
    }
    }
}
char* getType(char * symbol){
    return types[computeSymbolIndex(symbol)];
}
void setType(char* symbol,char *type){
    types[computeSymbolIndex(symbol)]=type;
}
void checkUsed(){
    int i;
    for (i = 0;i<200;i++){
        if (!used[computeSymbolIndex(symbols[i])] && strcmp(symbols[i],"\0")){
            char* msg = malloc(200*sizeof(char));
            strcpy(msg,"variable '");
            strcat(msg,symbols[i]);
            strcat(msg,"' is unused");
            yyerror(msg);
        }
    }
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

nodeType *conf(float value) {
    nodeType *p;

    /* allocate node */
    if ((p = malloc(sizeof(nodeType))) == NULL)
        yyerror("yyout of memory");

    /* copy information */
    p->type = typeCon;
    p->con.fvalue = value;
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

    if (!p){return;}
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
  int ex(nodeType *p, int label ) {

   //printf("ex (): beginning  condition = %d declaration = %d id_flag = %d opr = %d\n",conditionFlag,p->declarationFlag,id_flag,opr_flag);
    int lbl1, lbl2, lbl3,whilelbl,switchlbl,forlbl,dowhilelbl;
    int x;
    int flag11; 
    if (!p){printf("no p\n");return 0;} 
    lblarr = malloc(200*sizeof(int));
    switch(p->type) {
    case typeCon:
        //p->con.registerNumber++;
        if (!conditionFlag){

            if(!p->declarationFlag && id_flag){
                if (p->con.type == integer_num){
                    fprintf(yyout,"mov %s,%d\n",symbol_id,p->con.value);
                }
                else if (p->con.type == float_num){
                    fprintf(yyout,"mov %s,%0.1f\n",symbol_id,p->con.fvalue);
                }
            
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

            /******opr_flag = 0;    -------->law bawazet el denya shelha*/   
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
            d ecRegNum = registerNumbers[sym_index];
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
                        //printf("test1.3\n");
                if (p->opr.op[1]->type == typeId){
                    if(strcmp(types[computeSymbolIndex(p->opr.op[0]->id.id_name)],types[computeSymbolIndex(p->opr.op[1]->id.id_name)])){  
                        char * msg = malloc (300*sizeof(char));
                        char * type1 = malloc (50*sizeof(char));
                        strcpy(type1,getType(p->opr.op[0]->id.id_name));
                        char * type2 = malloc (50*sizeof(char));
                        strcpy(type2,getType(p->opr.op[1]->id.id_name));
                        strcpy(msg,"warning: type conflict between variable '"); 
                        strcat(msg,p->opr.op[0]->id.id_name);
                        strcat(msg,"' of type (");
                        strcat(msg,type1);
                        strcat(msg,") and variable '");
                        strcat(msg,p->opr.op[1]->id.id_name);
                        strcat(msg,"' of type (");
                        strcat(msg,type2);
                        strcat(msg,")\n");
                        yyerror(msg);
                    }
                }
                else if (p->opr.op[1]->type == typeCon ){
                	if( p->opr.op[1]->con.type == float_num){
                		if(strcmp(types[computeSymbolIndex(p->opr.op[0]->id.id_name)],"float")){
                			char * msg = malloc (300*sizeof(char));
                        char * type1 = malloc (50*sizeof(char));
                        strcpy(type1,getType(p->opr.op[0]->id.id_name));
                        strcpy(msg,"warning: type conflict between variable '"); 
                        strcat(msg,p->opr.op[0]->id.id_name);
                        strcat(msg,"' of type (");
                        strcat(msg,type1);
                        strcat(msg,") and float value.");
                        yyerror(msg);
                		}
                	}
                	else if ( p->opr.op[1]->con.type == integer_num){
                		if(strcmp(types[computeSymbolIndex(p->opr.op[0]->id.id_name)],"int")){
                			char * msg = malloc (300*sizeof(char));
                        char * type1 = malloc (50*sizeof(char));
                        strcpy(type1,getType(p->opr.op[0]->id.id_name));
                        
                        strcpy(msg,"warning: type conflict between variable '"); 
                        strcat(msg,p->opr.op[0]->id.id_name);
                        strcat(msg,"' of type (");
                        strcat(msg,type1);
                        strcat(msg,") and integer value.");
                        yyerror(msg);
                		}
                	}
                	
                }

                ex(p->opr.op[0],label);
                
                ex(p->opr.op[1],label); 


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
            ex(p->opr.op[0],label);            fprintf(yyout,"neg R%d,R%d\n",p->registerNumber,p->opr.op[0]->registerNumber);
            break;
            case if_statement:
            ex(p->opr.op[0],label);
            if (p->opr.nops > 2) {
                /* if else */
               
                /*fprintf(yyout,"cmp R%d,1\n",(p->opr.op[0])->con.registerNumber);
                fprintf(yyout,"jz\tL%03d\n", lbl1 = lbl++);
                */
                int t = conditionlbl;
               // op1RegNum = ((p->opr.op[0])->opr.op[0])->registerNumber;
                //op2RegNum = ((p->opr.op[0])->opr.op[1])->registerNumber;
                fprintf(yyout,"cmp R%d,1\n", lastRegNum);
                fprintf(yyout,"jz L%03d\n", lbl1 = lbl++);  
              //  printf("before else\n");
                ex(p->opr.op[2],label);      
               // printf("after else\n");          
                fprintf(yyout,"jmp\tL%03d\n", lbl2 = lbl++);
                fprintf(yyout,"L%03d:\n", lbl1);
               // printf("before if\n");
                ex(p->opr.op[1],label); 
               // printf("after if\n");               
                fprintf(yyout,"L%03d:\n", lbl2);
            } 
            else {
                /* if */ 
                //op1RegNum = ((p->opr.op[0])->opr.op[0])->registerNumber;
                //op2RegNum = ((p->opr.op[0])->opr.op[1])->registerNumber;
                fprintf(yyout,"cmp R%d,1\n", lastRegNum);
                fprintf(yyout,"jz L%03d\n", lbl1 = lbl++);  
                fprintf(yyout,"jmp\tL%03d\n", lbl2 = lbl++); 
                fprintf(yyout,"L%03d:\n", lbl1);
               // printf("before single if\n");
                ex(p->opr.op[1],label);
               // printf("after single if\n");                //fprintf(yyout,"L%03d:\n", lbl2);
                fprintf(yyout,"L%03d:\n", lbl2);
                /*int x = 0;
                while(x<=lblindex){
                    fprintf(yyout,"L%03d:\n",lblarr[x],0);                  x++;
                }*/
                            
            }
            break;
            case switch_statement:
            switchlbl = switchcnt--;
            switch_var = p->opr.op[0]->id.id_name;
            conditionFlag = 1;
            ex(p->opr.op[0],label);             ex(p->opr.op[1],switchlbl); // switchlabel            printf("test1\n");
            ex(p->opr.op[2],label);
            fprintf(yyout,"switchlbl%d:\n", switchlbl); 
            break;          
            case case_statement:
            conditionFlag = 1;
            //printf("case statement no. %d\n",switchcnt);
            // fprintf(yyout,"L%03d:\n", lbl1 = lbl++);
            ex(p->opr.op[0],label);           // fprintf(yyout,"L%03d:\n", lastlbl); 
            fprintf(yyout,"cmp %s,%d\n",switch_var,(p->opr.op[0])->con.value);
            
            fprintf(yyout,"jz\tL%03d\n", lbl1 = lbl++);
            lbl2 = lbl;
            fprintf(yyout,"jmp L%03d\n", lbl++); //default_lbl = lbl1+1;
            fprintf(yyout,"L%03d:\n", lbl1);
            ex(p->opr.op[1],label);
                    
            fprintf(yyout,"jmp switchlbl%d\n", label/*switchcnt + 1*/);
            fprintf(yyout,"L%03d:\n", lbl2);
            break;
            case default_command:
            //fprintf(yyout,"L%03d:\n", default_lbl);
            conditionFlag = 0;
            id_flag = 0;
           // printf("test\n");
            ex(p->opr.op[0],label);            break;
            /*case and_operator:
            
            ex(p->opr.op[0],0);            int t = conditionlbl+1;
            fprintf(yyout,"jmp L%03d\n", t); lblarr[lblindex++] = t; 



            fprintf(yyout,"L%03d:\n", conditionlbl);
            ex(p->opr.op[1],0);            t = conditionlbl+1;
            fprintf(yyout,"jmp L%03d\n", t); lblarr[lblindex++] = t; 
            fprintf(yyout,"L%03d:\n", conditionlbl);
            break;
            */
            case while_loop:
            /*printf("L%03d:\n", lbl1 = lbl++);
            ex(p->opr.op[0],0);            printf("\tjz\tL%03d\n", lbl2 = lbl++);
            ex(p->opr.op[1],0);            printf("\tjmp\tL%03d\n", lbl1);
            printf("L%03d:\n", lbl2);*/

            /*Please check if this is right =)*/    
            lbl1 = lbl++;
            whilelbl=lbl++;
            fprintf(yyout,"whilelbl%d:\n", whilelbl);
            ex(p->opr.op[0],label);
            fprintf(yyout,"cmp R%d,1\n", lastRegNum);
            fprintf(yyout,"jz L%03d\n", lbl2 = lbl++);           
            fprintf(yyout,"jmp L%03d\n", lbl1);
            fprintf(yyout,"L%03d:\n", lbl2);
            ex(p->opr.op[1],label);           
            fprintf(yyout,"jmp whilelbl%d:\n", whilelbl);
            fprintf(yyout,"L%03d:\n", lbl1);
            //fprintf(yyout,"cmp R%03d \t , %03d\n",p->opr.op[0]->registerNumber, p->opr.op[0]->con.value);
            
            //fprintf(yyout,"\tjz\tL%03d\n", lbl2);
            break;



            case repeat:
            //fprintf(yyout,"mov R%03d \t , %03d\n",p->opr.op[1]->registerNumber, p->opr.op[1]->con.value);
           
            lbl1 = lbl++;
            dowhilelbl=lbl++;
            fprintf(yyout,"dowhilelbl%d:\n", dowhilelbl);
            //printf("before statement\n");
            ex(p->opr.op[0],label); 
            //printf("after statement\n");
           // printf("before condition\n");
            ex(p->opr.op[1],label);
           // printf("after do while condition\n");
            fprintf(yyout,"cmp R%d,0\n", lastRegNum);
            fprintf(yyout,"jz dowhilelbl%d\n", dowhilelbl);  
            break;

            case for_loop:
            //fprintf(yyout,"xor R%03d \t , R%03d\n",p->opr.op[0]->registerNumber, p->opr.op[0]->registerNumber);
            ex(p->opr.op[0],label);  
            //forlbl = lbl++; 
            lbl2 = forcnt--;        
            fprintf(yyout,"forlbl%d:\n",lbl2);
            ex(p->opr.op[1],label);
            fprintf(yyout,"cmp R%d,1\n", lastRegNum);
            fprintf(yyout,"jz L%03d\n", lbl3 = lbl++);  
            fprintf(yyout,"jmp L%03d\n", lbl1 = lbl++);
            fprintf(yyout,"L%03d:\n", lbl3);
             
            ex(p->opr.op[3],label);
            ex(p->opr.op[2],label); 
            fprintf(yyout,"jmp forlbl%d\n", lbl2);
            fprintf(yyout,"L%03d:\n", lbl1);
                        //fprintf(yyout,"inc R%03d \t\n",p->opr.op[0]->registerNumber);
                      //fprintf(yyout,"cmp %s,%d\n",p->opr.op[0]->registerNumber, p->opr.op[0]->con.value);
                        //fprintf(yyout,"\tjle\tL%03d\n", lbl1);

            /*Another logic I found:
            fprintf(yyout,"mov R%03d \t , %03d\n",p->opr.op[0]->registerNumber, p->opr.op[0]->con.value);
            ex(p->opr.op[0],0);            fprintf(yyout,"L%03d:\n", lbl1 = lbl++);
            ex(p->opr.op[3],0);            fprintf(yyout,"loop L%03d:\n", lbl1);
            ex(p->opr.op[2],0);            ex(p->opr.op[1]);*/
            break;
            case return_command:
            fprintf(yyout,"proc %s\n",(p->opr.op[0])->id.id_name);
            if (p->opr.nops>2){
            ex(p->opr.op[0],label);
            ex(p->opr.op[1],label);
            ex(p->opr.op[2],label);
            }
            else{
            ex(p->opr.op[0],label);
            ex(p->opr.op[1],label);
            }            
            fprintf(yyout,"end proc\n");
            break;
            case break_command:
            if (p->opr.nops >1){
            ex(p->opr.op[1],label);
            }
            fprintf(yyout,"call %s\n",functions[computeFunctionIndex((p->opr.op[0])->id.id_name)]);  
            break;    
        default:
            
            if (p->opr.oper == '>' || p->opr.oper == '<' || p->opr.oper == equals_operator || p->opr.oper == not_equal_operator || p->opr.oper == smaller_or_equal_operator
             || p->opr.oper == greater_or_equal_operator){
                logex_flag = 1;
                conditionFlag = 1;
                opr_flag = 1; /// law bawazet el denya shelha
            }
            if ((p->opr.op[0]->type == typeOpr || p->opr.op[1]->type == typeOpr ) && conditionFlag){
                opr_flag = 1;
            }
                

             
            ex(p->opr.op[0],label); ex(p->opr.op[1],label);         lastRegNum = p->registerNumber;
            //lastOp1RegNum = p->opr.op[0]->registerNumber;
            //lastOp2RegNum = p->opr.op[1]->registerNumber;

            switch(p->opr.oper) {
            case increment:
                //printf("var = %s\n",(p->opr.op[0])->id.id_name);
                fprintf(yyout,"mov R%d,%s\n",(p->opr.op[0])->registerNumber,(p->opr.op[0])->id.id_name);
                fprintf(yyout,"add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber);
                fprintf(yyout,"mov %s,R%d\n",(p->opr.op[0])->id.id_name, p->registerNumber /*p->id.id_name*/); 
            break;

            case decrement:
                fprintf(yyout,"mov R%d,%s\n",(p->opr.op[0])->registerNumber,(p->opr.op[0])->id.id_name);
                fprintf(yyout,"sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber);
                fprintf(yyout,"mov %s,R%d\n",(p->opr.op[0])->id.id_name, p->registerNumber /*p->id.id_name*/);
            break;
            case '+': 
            fprintf(yyout,"add R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber);
            break;
            case '-':
            fprintf(yyout,"sub R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber);
            break;
            case '*':
           
            fprintf(yyout,"mul R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber);
            break;
            case '/':
         
            fprintf(yyout,"div R%d,R%d,R%d\n",p->registerNumber,(p->opr.op[0])->registerNumber,(p->opr.op[1])->registerNumber); 
             break;

            case '<':   

                        fprintf(yyout,"cmpLT R%d,R%d,R%d\n",p->registerNumber, p->opr.op[0]->registerNumber,p->opr.op[1]->registerNumber);
                        conditionFlag = 0;
                        id_flag = 0;
                        
                        break;
            case '>':   
                        fprintf(yyout,"cmpGT R%d,R%d,R%d\n",p->registerNumber, p->opr.op[0]->registerNumber,p->opr.op[1]->registerNumber);
                        conditionFlag = 0;
                        id_flag = 0;
                        break;
            case greater_or_equal_operator:   
                       
                        fprintf(yyout,"cmpGE R%d,R%d,R%d\n",p->registerNumber, p->opr.op[0]->registerNumber,p->opr.op[1]->registerNumber);
                        conditionFlag = 0;
                        id_flag = 0;
                        break;
            case smaller_or_equal_operator:   
                        fprintf(yyout,"cmpLE R%d,R%d,R%d\n",p->registerNumber, p->opr.op[0]->registerNumber,p->opr.op[1]->registerNumber);
                        conditionFlag = 0;
                        id_flag = 0;
                        break;
            case not_equal_operator:          
                        fprintf(yyout,"cmpNE R%d,R%d,R%d\n",p->registerNumber, p->opr.op[0]->registerNumber,p->opr.op[1]->registerNumber);
                        fprintf(yyout,"jnz L%03d\n",conditionlbl = lbl++);
                        conditionFlag = 0;
                        id_flag = 0;
                        break;
            case equals_operator:             
                        fprintf(yyout,"cmpE R%d,R%d,R%d\n",p->registerNumber, p->opr.op[0]->registerNumber,p->opr.op[1]->registerNumber);
                        conditionFlag = 0;
                        id_flag = 0;
                        break;
            case and_operator:
                        fprintf(yyout,"and R%d,R%d,R%d\n",p->registerNumber, p->opr.op[0]->registerNumber,p->opr.op[1]->registerNumber/*symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)]*//*lastRegNum*/);
                        conditionFlag = 0;
                        id_flag = 0;

                        //                      fprintf(yyout,"jz L%03d\n",conditionlbl = lbl++);
                        break;
             case or_operator:
                        fprintf(yyout,"or R%d,R%d,R%d\n",p->registerNumber, p->opr.op[0]->registerNumber,p->opr.op[1]->registerNumber/*symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)]*//*lastRegNum*/);
                        conditionFlag = 0;
                        id_flag = 0;

                        //                      fprintf(yyout,"jz L%03d\n",conditionlbl = lbl++);
                        break;

             case '!':
                        fprintf(yyout,"not R%d,R%d\n",p->registerNumber, p->opr.op[0]->registerNumber/*symbols[computeSymbolIndex(p->opr.op[0]->id.id_name)]*//*lastRegNum*/);
                        conditionFlag = 0;
                        id_flag = 0;

                        //                      fprintf(yyout,"jz L%03d\n",conditionlbl = lbl++);
                        break;
                    
        }
        break;
    }
    return 0;
}
}
