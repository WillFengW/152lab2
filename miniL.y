    /* cs152-miniL phase2 */
%{
  #include <stdio.h>
  #include <stdlib.h>
  void yyerror(const char *msg);
  extern int currLine;
  extern int currPos;
  FILE * yyin;
%}

%union{
  /* put your types here */
  int numVal;
  char * identVal;
}

%error-verbose
%start prog_start
%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY ENUM OF IF THEN ENDIF ELSE WHILE FOR DO BEGINLOOP ENDLOOP CONTINUE READ WRITE TRUE FALSE SEMICOLON COLON COMMA L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET ASSIGN RETURN
%token <identVal> IDENT
%token <numVal> NUMBER
%right ASSIGN
%left OR
%left AND
%right NOT
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MULT DIV MOD

/* %start program */

%% 

  /* write your rules here */

prog_start:       functions { printf("prog_start -> functions\n"); }
        ;

functions:        /*empty*/ { printf("functions -> epsilon\n");}
        |         function functions { printf("functions -> function functions\n"); }
        ;

function:         FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY
                  { printf("function -> FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY\n"); }   
        ;
      
declarations:     /*empty*/ { printf("declarations -> epsilon\n"); }
        |         declaration SEMICOLON declarations {printf("declarations -> declaration SEMICOLON declarations\n"); }
        ;

declaration:      identifiers COLON INTEGER { printf("declaration -> identifiers COLON INTEGER\n"); }
        |         identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER 
                  { printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER %d R_SQUARE_BRACKET OF INTEGER\n", $5); }
        |         identifiers COLON ENUM L_PAREN identifiers R_PAREN
                  { printf("declaration -> identifiers COLON ENUM L_PAREN identifiers R_PAREN\n"); }
        ;

identifiers:      ident { printf("identifiers -> ident\n"); }
        |         ident COMMA identifiers { printf("identifiers -> ident COMMA identifiers\n"); }
        ;

ident:            IDENT { printf("ident -> IDENT %s\n", $1); }
        ;

statements:       /*empty*/ { printf("statements -> epsilon\n"); }
        |         statement SEMICOLON statements { printf("statements -> statement SEMICOLON statements\n"); }
        ;

statement:        var ASSIGN expression { printf("statement -> var ASSIGN expression\n"); }
        |         IF bool_expr THEN statements ENDIF { printf("statement -> IF bool_expr THEN statements ENDIF\n"); }
        |         IF bool_expr THEN statements ELSE statements ENDIF { printf("statement -> IF bool_expr THEN statements ELSE statements ENDIF\n"); }
        |         WHILE bool_expr BEGINLOOP statements ENDLOOP { printf("statement -> WHILE bool_expr BEGINLOOP statements ENDLOOP\n"); }
        |         DO BEGINLOOP statements ENDLOOP WHILE bool_expr { printf("statement -> DO BEGINLOOP statements ENDLOOP WHILE bool_expr\n"); }
        |         READ vars { printf("statement -> READ vars\n"); }
        |         WRITE vars { printf("statement -> WRITE vars\n"); }
        |         CONTINUE { printf("statement -> CONTINUE\n"); }
        |         RETURN expression { printf("statement -> RETURN expression\n"); }
        ;

vars:             /*empty*/ { printf("vars -> epsilon\n"); }
        |         var { printf("vars -> var\n"); }
        |         var COMMA vars { printf("vars -> var COMMA vars\n"); }
        ;

bool_expr:        relation_and_expr { printf("bool_expr -> relation_and_expr\n"); }
        |         bool_expr OR relation_and_expr { printf("bool_expr -> bool_expr OR relation_and_expr\n"); }
        ;

relation_and_expr:	  relation_expr { printf("relation_and_expr -> relation_expr\n"); }
        |             relation_and_expr AND relation_expr { printf("relation_and_expr -> relation_and_expr AND relation_expr\n"); }
        ;

relation_expr:	      expression comp expression { printf("relation_expr -> expression comp expression\n"); }
		    |             NOT expression comp expression { printf("relation_expr -> NOT expression comp expression\n"); }
	    	|             TRUE { printf("relation_expr -> TRUE\n"); }
	    	|             NOT TRUE { printf("relation_expr -> NOT TRUE\n"); }
	    	|             FALSE { printf("relation_expr -> FALSE\n"); }
	    	|             NOT FALSE { printf("relation_expr -> NOT FALSE\n"); }
	    	|             L_PAREN bool_expr R_PAREN { printf("relation_expr -> L_PAREN bool_expr R_PAREN\n"); }
        |             NOT L_PAREN bool_expr R_PAREN { printf("relation_expr -> NOT L_PAREN bool_expr R_PAREN\n"); }
	    	;

comp:		              EQ { printf("comp -> EQ\n"); }
		    |             NEQ { printf("comp -> NEQ\n"); }
		    |             LT { printf("comp -> LT\n"); }
		    |             GT { printf("comp -> GT\n"); }
		    |             LTE { printf("comp -> LTE\n"); }
		    |             GTE { printf("comp -> GTE\n"); }
	    	;

expression:           multiplicative_expr { printf("expression -> multiplicative_expr\n"); }
        |             expression ADD multiplicative_expr { printf("expression -> expression ADD multiplicative_expr\n"); }
        |             expression SUB multiplicative_expr { printf("expression -> expression SUB multiplicative_expr\n"); }
        ;

multiplicative_expr:	  term  { printf("multiplicative_expr -> term\n"); }
        |               multiplicative_expr MULT term { printf("multiplicative_expr -> multiplicative_expr MULT term\n"); }
        |               multiplicative_expr DIV term { printf("multiplicative_expr -> multiplicative_expr DIV term\n"); }
        |               multiplicative_expr MOD term { printf("multiplicative_expr -> multiplicative_expr MOD term\n"); }
        ;

term:		                var { printf("term -> var\n"); }
		    |               SUB var { printf("term -> SUB var\n"); }
		    |               NUMBER { printf("term -> NUMBER %d\n", $1); }
		    |               SUB NUMBER { printf("term -> SUB NUMBER %d\n", $2); }
		    |               L_PAREN expression R_PAREN { printf("term -> L_PAREN expression R_PAREN\n"); }
		    |               SUB L_PAREN expression R_PAREN { printf("term -> SUB L_PAREN expression R_PAREN\n"); }
	    	|               IDENT L_PAREN R_PAREN { printf("term -> IDENT %s L_PAREN R_PAREN\n", $1); }
		    |               IDENT L_PAREN expressions R_PAREN { printf("term -> IDENT %s L_PAREN expressions R_PAREN\n", $1); }
		    ;

expressions:            expression { printf("expressions -> expression"); }
        |               expressions COMMA expression { printf("expressions -> expressions COMMA expression"); }
        ;

var:		                IDENT { printf("var -> IDENT %s\n", $1); }
		    |               IDENT L_SQUARE_BRACKET expression R_SQUARE_BRACKET { printf("var -> IDENT %s L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n", $1); }
		    ;

%% 

int main(int argc, char ** argv) {
    if (argc > 1) {
        yyin = fopen(argv[1], "r");
        if (yyin == NULL) {
            printf("syntax: %s filename", argv[0]);
        }
    }
    yyparse();
    return 0;
}

void yyerror(const char *msg) {
    printf("Error: Line %d, position %d: %s \n", currLine, currPos, msg);
}