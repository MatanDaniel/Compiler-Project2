%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int yyerror();
extern int yylex();
extern int yylineno;
extern char *yytext;

typedef struct node
{
	char * token;
	struct node *left;
	struct node *right;
}node;


int yyerror(char *e);
struct node* mkleaf(char* token);
struct node* mknode2(char* token,node* left,node* right);
int printtree(node* tree);

%}
%union {
  char* string; 
  struct node* node;
}


%token <string> INT CHAR STRING REAL BOOL ID VERTICAL_BAR
%token <string> REAL_TYPE INT_TYPE CHAR_TYPE STRING_TYPE REALP_TYPE CHARP_TYPE INTP_TYPE VOID_TYPE BOOL_TYPE
%token <string> RETURN NULL_ DO FUNCTION MAIN FOR WHILE IF ELSE VAR SQR_C SQR_O BAR PAREN_C PAREN_O CURLY_C CURLY_O
%token <string> COMMA SEMI PTR_VAL ADRS OP_DIV OP_MULT OP_MINUS OP_PLUS OP_PLUS_PLUS ASSIGN NOT COLUMS ARGS
%left  OR AND ROP_NE ROP_EQ ROP_GE ROP_LT ROP_LE ROP_GT
%left  OP_PLUS OP_MINUS OP_MULT OP_DIV
%right NOT ADRS PTR_VAL
%type <node> program functions function main_func code fargs body_f body_p f_args stmt_f stmt_p func_args code_ assignment expr func_call id_list var_decleration vardec_args array_id
%type <node> stmt_block_rec decleration block_stmt code_block_dec ptr_expr
%type <node> code_block stmts_oneline procedure return for_init fbody_ pbody_ end_return str_decleration str_dec_args ptr_id string_type args_Id string_Id
%type <string> id args_type f_type mid_args fargs_ literal char_or_string int_or_id
%nonassoc ELSE
%nonassoc ID
%nonassoc PAREN_O
%nonassoc CURLY_O

%start program
%%
program		    :	code  { printtree($1); }
                ;

code            :   code_ { $$ = mknode2("CODE", $1, NULL); }
                ;

code_		    :	function functions { $$ = mknode2("FUNCTION", $1, $2); }
                | 	procedure functions { $$ = mknode2("FUNCTION", $1, $2); }
                |	main_func { $$ = mknode2("FUNCTION",$1,NULL);}
                ;
    
functions	    :   code_
                ;

function : FUNCTION id PAREN_O func_args PAREN_C CURLY_O body_f CURLY_C f_type
          { $$ = mknode2($2, mknode2(strcat($9, "TYPE"), mknode2("ARGS", $4, NULL), NULL), NULL); }
          ;

procedure       :	FUNCTION id PAREN_O func_args PAREN_C CURLY_O body_p CURLY_C VOID_TYPE
                    { $$ = mknode2($2, mknode2("VOID TYPE",$9, NULL),$8);}
                ;

main_func	    :	FUNCTION MAIN PAREN_O PAREN_C CURLY_O body_f CURLY_C INT_TYPE
                    { $$ = mknode2("MAIN",mknode2("INT TYPE",mkleaf("ARGS NONE"),NULL), $7);}
                |	FUNCTION VOID_TYPE MAIN PAREN_O PAREN_C CURLY_O body_p CURLY_C
                    { $$ = mknode2("MAIN",mknode2("VOID TYPE",mkleaf("ARGS NONE"),NULL), $7);}
                ;

args_type	    :	REAL_TYPE { $$ = strdup("REAL "); }
                |  	BOOL_TYPE { $$ = strdup("BOOL "); }
                |	INT_TYPE { $$ = strdup("INT "); }
                | 	CHAR_TYPE { $$ = strdup("CHAR "); }
                | 	INTP_TYPE { $$ = strdup("INTP "); }
                | 	CHARP_TYPE { $$ = strdup("CHARP "); }
                | 	STRING_TYPE { $$ = strdup("STRING "); }
                ;

f_type		    :	BOOL_TYPE { $$ = strdup("BOOL"); }
                | 	INT_TYPE { $$ = strdup("INT"); }
                | 	CHAR_TYPE { $$ = strdup("CHAR"); }
                | 	INTP_TYPE { $$ = strdup("INTP"); }
                | 	CHARP_TYPE { $$ = strdup("CHARP"); }
                | 	STRING_TYPE { $$ = strdup("STRING"); }
                ;

string_Id       :   ID SQR_O expr SQR_C { $$ = mknode2($1, mknode2("[]", NULL,$3), NULL); }

                |   assignment SEMI { $$ = $1; }

                |   assignment COMMA string_Id { $1->right = $3;  $$ = $1; }

                |   ID SQR_O expr SQR_C  COMMA string_Id { $$ = mknode2($1, mknode2("[]", NULL,$3), $6); }

                |   ID SQR_O expr SQR_C SEMI {$$ = mknode2($1, mknode2("[]", NULL,$3), NULL); };


id		        :	ID { $$ = strdup(yytext); }
                ;

array_id	    :	id SQR_O INT SQR_C {$$=mknode2($1,mkleaf($3),NULL);}
                | 	id SQR_O id SQR_C {$$=mknode2($1,mkleaf($3),NULL);}
                ;

func_args       :   args_type args_Id func_args {$$ = mknode2($1,$2,$3);}
                |   string_type args_Id func_args {$1 ->left->right = $2; $1->right = $3; $$= $1;}
                |   ARGS func_args COLUMS args_type COLUMS func_args {$$ = mknode2($4, $2, $6);}
                |   ARGS args_Id COLUMS args_type func_args   {$$ = mknode2($4, $2, $5);}
                |   {$$ = NULL;};
                ;


string_type     :   STRING string_Id { $$ =$2; };
                


args_Id         :   ID { $$ = mknode2($1, NULL, NULL); } 

                |   ID SEMI { $$ = mknode2($1, NULL,NULL); }

                |   ID COMMA args_Id { $$ = mknode2($1, NULL, $3); }

                |   { $$ = NULL; };
                ;


f_args		    :	SEMI f_args { $$ = mknode2($1, NULL, $2); }
                |   fargs {$$=$1;}
                ;

fargs_          :   args_type mid_args { $$ = strdup(strcat($1, $2)); }
                ;

fargs		    :	args_type mid_args { $$ = mkleaf(strcat($1, $2)); }
                ;

mid_args        :   id COMMA mid_args { char* s = malloc(sizeof(char)*(strlen($1) + strlen($3)));
                                        strcpy(s, $1); strcat(s," "); strcat(s, $3);
                                        $$ = strdup(s); }
                |   id { $$ = $1; }
                ;


                

                

body_f	    	:  	fbody_ end_return { $$ = mknode2("BODY", $1, $2); }
                ;

body_p	    	:  	pbody_ { $$ = mknode2("BODY", $1, NULL); }
                ;

fbody_          :   stmt_f fbody_ { $$ = mknode2("STATEMENT", $1, $2); }
               	|   { $$ = NULL; }
                ;

pbody_          :   stmt_p pbody_ { $$ = mknode2("STATEMENT", $1, $2); }
                |   { $$ = NULL; }
                ;

assignment 	    :   id ASSIGN expr { $$ = mknode2("=", mkleaf($2), $3); }
                |	array_id ASSIGN expr { $$ = mknode2("=", $1, $3); }
                |   ptr_id ASSIGN expr { $$ = mknode2("=", $1, $3);}
                ;

ptr_id          :    OP_MULT id { $$=mknode2("PTR",mkleaf($2),NULL); }
                ;

for_init        :	assignment SEMI expr SEMI assignment {$$ = mknode2(" ",$1, mknode2(" ",$3,$5));}
                ;

stmts_oneline   : 	assignment SEMI { $$ = $1; }
                ;

stmt_p       	:   IF PAREN_O expr PAREN_C CURLY_O  pbody_ CURLY_C %prec ELSE  {$$=mknode2("IF", $3, $6);}
                |   IF PAREN_O expr PAREN_C CURLY_O pbody_ CURLY_C ELSE CURLY_O pbody_ CURLY_C
                    { $$ = mknode2("IF-ELSE", mknode2("IF", $3, $6), mknode2("ELSE", $10, NULL)); }
                |   IF PAREN_O expr PAREN_C stmts_oneline %prec ELSE { $$ = mknode2("IF", $3, $5); }
                |   IF PAREN_O expr PAREN_C stmts_oneline ELSE stmts_oneline
                    { $$ = mknode2("IF-ELSE", mknode2("IF", $3, $5), mknode2("ELSE", $7,NULL)); }
                |   IF PAREN_O expr PAREN_C stmts_oneline ELSE CURLY_O stmts_oneline CURLY_C
                    { $$ = mknode2("IF-ELSE", mknode2("IF", $3, $5), mknode2("ELSE", $8, NULL)); }
                | 	WHILE PAREN_O expr PAREN_C CURLY_O  pbody_ CURLY_C {$$ = mknode2("WHILE", $3, $6); }
                |	WHILE PAREN_O expr PAREN_C stmts_oneline SEMI {$$ = mknode2("WHILE", $3, $5); }
                |	DO CURLY_O pbody_ CURLY_C WHILE PAREN_O expr PAREN_C SEMI { $$ = mknode2("DO-WHILE", $3, $7); }
                | 	FOR PAREN_O for_init PAREN_C CURLY_O pbody_ CURLY_C {$$=mknode2("FOR",$3,$6);}
                |	FOR PAREN_O for_init PAREN_C stmts_oneline SEMI {$$=mknode2("FOR",$3,$5);}
                |	decleration { $$ = $1; }
                |	func_call SEMI { $$ = $1;}
                | 	assignment SEMI  { $$ = $1; }
                |	procedure { $$ = $1; }
                |	function { $$ = mknode2("FUNCTION", $1, NULL); }
                |	CURLY_O code_block CURLY_C { $$ = $2; }
                ;
stmt_f       	:   IF PAREN_O expr PAREN_C CURLY_O fbody_ return CURLY_C %prec ELSE {$$=mknode2("IF", $3, mknode2(" ",$6,$7));}
                |   IF PAREN_O expr PAREN_C CURLY_O fbody_ return CURLY_C ELSE CURLY_O fbody_ return CURLY_C
                    { $$ = mknode2("IF-ELSE", mknode2("IF", $3, mknode2(" ",$6,$7)), mknode2("ELSE", mknode2(" ",$11,$12), NULL)); }
                |   IF PAREN_O expr PAREN_C stmts_oneline %prec ELSE { $$ = mknode2("IF", $3, $5); }
                |   IF PAREN_O expr PAREN_C stmts_oneline ELSE stmts_oneline
                    { $$ = mknode2("IF-ELSE", mknode2("IF", $3, $5), mknode2("ELSE", $7,NULL)); }
                |   IF PAREN_O expr PAREN_C stmts_oneline ELSE CURLY_O stmts_oneline CURLY_C
                    { $$ = mknode2("IF-ELSE", mknode2("IF", $3, $5), mknode2("ELSE", $8, NULL)); }
                | 	WHILE PAREN_O expr PAREN_C CURLY_O  fbody_ return CURLY_C {$$ = mknode2("WHILE", $3, mknode2(" ",$6,$7)); }
                |	WHILE PAREN_O expr PAREN_C stmts_oneline SEMI {$$ = mknode2("WHILE", $3, $5); }
                |	DO CURLY_O fbody_ return CURLY_C WHILE PAREN_O expr PAREN_C SEMI { $$ = mknode2("DO-WHILE", mknode2(" ",$3,$4), $8); }
                | 	FOR PAREN_O for_init PAREN_C CURLY_O fbody_ return CURLY_C { $$ = mknode2("FOR",$3,mknode2(" ", $6, $7));}
                |	FOR PAREN_O for_init PAREN_C stmts_oneline SEMI {$$=mknode2("FOR",$3,$5);}
                |	decleration {  $$ = $1; }
                |	func_call SEMI { $$ = $1;}
                | 	assignment SEMI { $$ = $1; }
                |	procedure { $$ = $1; }
                |	function { $$ = mknode2("FUNCTION", $1, NULL); }
                |	CURLY_O code_block CURLY_C { $$ = $2; }
                ;

block_stmt      :   IF PAREN_O expr PAREN_C CURLY_O stmt_block_rec CURLY_C %prec ELSE  {$$=mknode2("IF", $3, $6);}
                |   IF PAREN_O expr PAREN_C CURLY_O stmt_block_rec CURLY_C ELSE CURLY_O pbody_ CURLY_C
                    { $$ = mknode2("IF-ELSE", mknode2("IF", $3, $6), mknode2("ELSE", $10, NULL)); }
                |   IF PAREN_O expr PAREN_C stmts_oneline %prec ELSE { $$ = mknode2("IF", $3, $5); }
                |   IF PAREN_O expr PAREN_C stmts_oneline ELSE stmts_oneline
                    { $$ = mknode2("IF-ELSE", mknode2("IF", $3, $5), mknode2("ELSE", $7,NULL)); }
                |   IF PAREN_O expr PAREN_C stmts_oneline ELSE CURLY_O stmts_oneline CURLY_C
                    { $$ = mknode2("IF-ELSE", mknode2("IF", $3, $5), mknode2("ELSE", $8, NULL)); }
                |   WHILE PAREN_O expr PAREN_C CURLY_O stmt_block_rec CURLY_C {$$ = mknode2("WHILE", $3, $6); }
                |   WHILE PAREN_O expr PAREN_C stmts_oneline SEMI {$$ = mknode2("WHILE", $3, $5); }
                |   DO CURLY_O stmt_block_rec CURLY_C WHILE PAREN_O expr PAREN_C SEMI { $$ = mknode2("DO-WHILE", $3, $7); }
                |   FOR PAREN_O for_init PAREN_C CURLY_O stmt_block_rec CURLY_C {$$=mknode2("FOR",$3,$6);}
                |   FOR PAREN_O for_init PAREN_C stmts_oneline SEMI {$$=mknode2("FOR",$3,$5);}
                |   func_call SEMI { $$ = $1; }
                |   assignment SEMI { $$ = $1; }
                |   CURLY_O code_block CURLY_C { $$ = $2; }
                ;

decleration     :   var_decleration { $$ = $1; } | str_decleration { $$ = $1; }
                ;

var_decleration :	VAR args_type vardec_args SEMI { $$ = mknode2($2, $3, NULL); }
                ;

vardec_args     :	id { $$ = mkleaf($1); }
                |	id COMMA vardec_args { $$ = mknode2($1,$3,NULL); }
                |	assignment COMMA vardec_args { $$ = mknode2(" ", $1, $3); }
                |	assignment { $$ = $1; }
                ;

str_decleration :   STRING_TYPE str_dec_args ASSIGN char_or_string SEMI
                    { $$ = mknode2("STRING", mknode2("=", $2, mkleaf($4)), NULL); }
                |   STRING_TYPE str_dec_args ASSIGN char_or_string COMMA str_dec_args SEMI
                    { $$ = mknode2("STRING", mknode2("=", $2, mkleaf($4)), NULL); }
                |   STRING_TYPE str_dec_args SEMI
                    { $$ = mknode2("STRING", $2, NULL); }
                ;

str_dec_args    :   id SQR_O int_or_id SQR_C COMMA str_dec_args { $$=mknode2($1,mkleaf($3),$6);}
                |   id SQR_O int_or_id SQR_C { $$=mknode2($1,mkleaf($3),NULL); }
                ;

int_or_id       :   INT {$$ = strdup(yytext); } | id
                ;

char_or_string  :   CHAR { $$ = strdup(yytext); }
                | 	STRING { $$ = strdup(yytext); }
                ;

func_call       :	id PAREN_O id_list PAREN_C {$$=mknode2("CALL",mkleaf($2),$3);}
                ;

id_list         :	id COMMA id_list {$$=mknode2($1,$3,NULL);} 
                | 	id {$$=mkleaf($1);}
                | 	literal COMMA id_list {$$=mknode2($1,$3,NULL);}
                |	literal { $$=mkleaf($1); }
                | 	{$$=NULL;}
                ;

expr     	    :   expr OP_PLUS expr { $$ = mknode2("+", $1, $3); }
                |   expr OP_MINUS expr { $$ = mknode2("-", $1, $3); }
                |   expr OP_MULT expr  { $$ = mknode2("*", $1, $3); }
                |   expr OP_DIV expr { $$ = mknode2("/", $1, $3); }
                |   expr AND expr	{ $$ = mknode2("&&", $1, $3); }
                |   expr OR expr { $$ = mknode2("||", $1, $3); }
                |   expr ROP_NE expr { $$ = mknode2("!=", $1, $3); }
                |   expr ROP_EQ expr { $$ = mknode2("==", $1, $3); }
                |   expr ROP_GT expr { $$ = mknode2(">", $1, $3); }
                |   expr ROP_GE expr { $$ = mknode2(">", $1, $3); }
                |   expr ROP_LT expr { $$ = mknode2("<", $1, $3); }
                |   expr ROP_LE expr { $$ = mknode2("<=", $1, $3); }
                |   NOT expr { $$ = mknode2($1, NULL, $2); }
                |   OP_MULT ptr_expr { $$ = mknode2("*",$2, NULL); }
                |  	ADRS ptr_expr{ $$ = mknode2("&", NULL, $2); }
                |   OP_PLUS expr { $$ = mknode2($1, NULL, $2); }
                |   OP_MINUS expr { $$ = mknode2($1, NULL, $2); }
                |   literal { $$ = mkleaf($1); }
                |   func_call { $$ = $1; }
                |   BAR id BAR { $$ = mkleaf($2); }
                |   BAR STRING BAR { $$ = mkleaf($2); }
                |   id SQR_O int_or_id SQR_C { $$ = mknode2($1, mkleaf($3), NULL); }
                |   id	{ $$ = mkleaf($1); }
                |   PAREN_O expr PAREN_C { $$ = mknode2("()", NULL, $2); }
                ;

ptr_expr	    :	PAREN_O id OP_PLUS INT PAREN_C  { $$ = mknode2("+", mkleaf($2),mkleaf($4)); }
                |	PAREN_O id OP_MINUS INT PAREN_C  { $$ = mknode2("+", mkleaf($2),mkleaf($4)); }
                |	PAREN_O id OP_MULT INT PAREN_C  { $$ = mknode2("+", mkleaf($2),mkleaf($4)); }
                |	PAREN_O id OP_DIV INT PAREN_C  { $$ = mknode2("+", mkleaf($2),mkleaf($4)); }
                | 	id  { $$ = mknode2($1, NULL,NULL); }
                | 	array_id  { $$ = mknode2(" ", $1,NULL); }
                ;

code_block      :   code_block_dec stmt_block_rec { $$ = mknode2("CODE BLOCK", $1, $2); }
                ;

stmt_block_rec  :   block_stmt stmt_block_rec { $$ = mknode2("STATEMENT", $1, $2); } | { $$ = NULL; }
                ;

code_block_dec  :   decleration code_block_dec { $$ = mknode2("STATEMENT", $1, $2); } | { $$ = NULL; }
                ;

literal    	    : 	INT { $$ = strdup(yytext); }
                | 	CHAR { $$ = strdup(yytext); }
                | 	STRING { $$ = strdup(yytext); }
                | 	REAL { $$ = strdup(yytext); }
                | 	NULL_ { $$ = strdup(yytext); }
                | 	BOOL { $$ = strdup(yytext); }
                ;

return          :	RETURN expr SEMI { $$ = mknode2("RETURN",$2,NULL); }
                |	{$$=NULL;}
                ;

end_return      :	RETURN expr SEMI { $$ = mknode2("RETURN",$2,NULL); }
                ;

%%
#include "lex.yy.c"
int main() {
	yyparse();
	return 0;
}



int tabCount1 = 0;
int tabCount2 = 0;

struct node* mkleaf(char* token){
	node* newnode=(node*)malloc(sizeof(node));
	char* newstr;

	if( token ){
		newstr=(char*)malloc(sizeof(token)+1);
		newstr[sizeof(token)]='\0';
		strcpy(newstr,token);
	}
	else{

		newstr=(char*)malloc(1);
		strcpy(newstr,"");
		
	}

	newnode->left=NULL;
	newnode->right=NULL;
	newnode->token=newstr;
	return newnode;
}


struct node* mknode2(char* token,node*left,node*right)
{

	node* newnode= mkleaf(token);
	newnode->left=left;
	newnode->right=right;

	return newnode;
}

void printTabs1() {
	for(int i=0;i<tabCount1;i++){
		printf(".  ");	
	}
}

void printTabs2(){
	int i;
	for(i=0;i<tabCount2;i++){
		printf(".  ");	
	}
}

void openTag() {
	printf("(%d)" , tabCount1);	
}

void closeTag() {
	printf("(/%d)" , tabCount1);	
}

int printtree(node* tree) {
	tabCount1++;
	if (tree->left!=NULL || tree->right!=NULL){
		if(strcmp(tree->token," ")!=0){
		printTabs1();
		printf("(");
		printf("%s\n",tree->token);
		tabCount2=tabCount1;
		}
	}
	else if(tree->left==NULL && tree->right==NULL){
		printTabs1();
		printf("(");
		printf("%s",tree->token);
		printf(")\n");
		}
	
	if(tree->left)printtree(tree->left);
	if(tree->right)printtree(tree->right);
	if(tree->left!=NULL || tree->right!=NULL){
		if(strcmp(tree->token," ")!=0){
			printTabs2();
			printf(")\n");
			tabCount2--;
		}
	}
	tabCount1--;

	return 1;
}


int yyerror(char *e)
{
	int yydebug=1; 
	fflush(stdout);
	fprintf(stderr,"Error %s at line %d\n" ,e,yylineno);
	fprintf(stderr, "does not accept '%s'\n",(yytext));
	
	return 0;
}


//|  id OP_PLUS_PLUS { $$ = mknode2("++", mkleaf($1), NULL); };
