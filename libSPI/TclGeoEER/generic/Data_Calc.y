%{
#include "tclData.h"
#include "tclFSTD.h"
#include "tclGDAL.h"
#include "tclObs.h"

#include "Data_Matrix.h"
#include "Data_Calc.h"
#include "Data_Funcs.h"

extern Tcl_Interp *GInterp;

extern TData      *GField,*GFieldP;
extern TDef   *GResult;
extern TDef   *GData[1024];
extern int         GDataN;
extern int         GError;
extern int         GMode;

#define STACK_MAX 10
static TDef *Stack[STACK_MAX];
static int  StackN;

int vexpr_error(char* s);
%}

%union {
  TDef   *Val;            /*!< For returning numbers. */
  double     Num;
  TDef   *Data;           /*!< If we found a data */
  TData      *Field;          /*!< If we found a field */
  TObs       *Obs;            /*!< If we found an observation */
  TVector    *Vec;            /*!< If we found a vector */
  GDAL_Band  *Band;           /*!< If we found a band */
  OGR_Layer  *Layer;          /*!< If we found a layer */
  TFuncDef   *Func;           /*!< If we found a function */
  enum {
    OP_ADD,                   /*!< Addition operator */
    OP_SUB,                   /*!< Substraction operator */
    OP_MUL,                   /*!< Multiplication operator */
    OP_DIV,                   /*!< Division operator */
    OP_MOD,                   /*!< Modulus operator */
    OP_EXP,                   /*!< Exponentiation operator */
    OP_NOT,                   /*!< Logical NOT */
    OP_AND,                   /*!< Logical AND */
    OP_OR,                    /*!< Logical OR */
    OP_BNOT,                  /*!< Bitwise NOT */
    OP_BND,                   /*!< Bitwise AND */
    OP_BOR,                   /*!< Bitwise OR */
    OP_EQU,                   /*!< Equality conditional operator */
    OP_NEQ,                   /*!< Non-equality conditional operator */
    OP_GRQ,                   /*!< Greater-or-equal conditional operator */
    OP_GRE,                   /*!< Greater conditional operator */
    OP_SMQ,                   /*!< Smaller-or-equal conditional operator */
    OP_SMA,                   /*!< Smaller conditional operator */
    OP_COMMA,                 /*!< Hackish operator */
    OP_AROB,                  /*!< Hackish operator */
    OPEN_PAR,                 /*!< Opening parenthesis */
    CLOSE_PAR,                /*!< Closing parenthesis */
    OPEN_BRA,                 /*!< Opening indexing function */
    CLOSE_BRA,                /*!< Closing indexing function */
    OP_INTERP2,               /*!< Interpolate LINEAR */
    OP_INTERP3,               /*!< Interpolate CUBIC */
    OP_ASSIGN,                /*!< Assign operator */
    LINE_TERMINATOR           /*!< Ends a line, end-of-input symbol */
  } Operator;                 /*!< Possible infix operators */
}

%token <Operator>  T_LINE_TERMINATOR
%token <Num>       T_FLOAT T_INT      /* Simple single precision number */
%token <Func>      T_FNCT_F           /* Matrix to Float Function */
%token <Func>      T_FNCT_M           /* Matrix to Matrix Function */
%token <Func>      T_FNCT_D           /* Matrix derivative Function */
%token <Data>      T_DATA
%token <Data>      T_CHAR
%token <Num>       T_FIELD_FUNC
%token <Val>       T_ERROR            /* The lexer found something wicked */

%token T_OPEN_PAR  "("
%token T_CLOSE_PAR ")"
%token T_OPEN_BRA  "["
%token T_CLOSE_BRA "]"
%token T_COMMA     ","

%type  <Val>       exp
%type  <Operator>  T_ADD T_SUB T_MUL T_DIV T_EXP T_INTERP2 T_INTERP3 T_ASSIGN
%type  <Operator>  T_EQU T_NEQ T_GRQ T_GRE T_SMQ T_SMA
%type  <Operator>  T_BNOT T_BND T_BOR
%type  <Operator>  T_NOT T_AND T_OR T_AROB
%type  <Operator>  T_OPEN_BRA T_CLOSE_BRA
%type  <Operator>  T_OPEN_PAR T_CLOSE_PAR T_COMMA

%nonassoc          T_INTERP2 T_INTERP3 T_ASSIGN
%left              T_OR T_BOR
%left              T_AND T_BND
%nonassoc          T_NOT T_BNOT
%left              T_EQU T_NEQ T_GRQ T_GRE T_SMQ T_SMA
%left              T_ADD T_SUB
%left              T_MUL T_DIV T_MOD
%left              T_EXP                   /* Exponentiation */
%nonassoc          T_NEG                   /* Negation--unary minus */
%nonassoc          T_COMMA T_AROB
%nonassoc          T_OPEN_BRA T_CLOSE_BRA
%nonassoc          T_OPEN_PAR T_CLOSE_PAR

%start line

%%

line:
   exp T_LINE_TERMINATOR   {
      GResult = $1;
   }

   | error T_LINE_TERMINATOR {

      GError = TCL_ERROR;
   }
;

farg:
   %empty {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Empty function argument list\n");
#endif
      StackN=0;
   }
   | exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Making argument list from exp\n");
#endif
      StackN=1;
      Stack[0]=$1;
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Stack[%d]=%p\n",StackN-1,Stack[StackN-1]);
#endif
   }
   | farg "," exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Appending to argument list\n",StackN);
#endif
      if( StackN < STACK_MAX ) {
         Stack[StackN++]=$3;
      } else {
         vexpr_error("STACK_MAX reached, too many function arguments: Critical!");
         YYERROR;
      }
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Stack[%d]=%p\n",StackN-1,Stack[StackN-1]);
#endif
   }
;

exp:
   T_ERROR {
      vexpr_error("Invalid function or data id in expression");
      YYERROR;
   }

   | T_INT {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) INT (%ld)\n",(long)$1);
#endif
      $$=Calc_MatrixInt((long)$1);
      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_INT): Critical!");
         YYERROR;
      }
   }

   | T_FLOAT {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) FLOAT (%g)\n",$1);
#endif
      $$=Calc_MatrixFloat($1);
      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_FLOAT): Critical!");
         YYERROR;
      }
   }

   | exp "[" T_INT "]" {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Component Indexing\n");
#endif
      if (!$1 || $3>=$1->NC || !$1->Data[(int)$3]) {
         vexpr_error("Invalid component index");
         YYERROR;
      } else {
         $$=Calc_Index($1,(int)$3);

         if (!$$) {
            vexpr_error("Calc_Index failed: Critical!");
            YYERROR;
         }
      }
   }

   | "[" exp "]" {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Vector Length\n");
#endif
      $$=Calc_Length($2);

      if (!$$) {
         vexpr_error("Calc_Length failed: Critical!");
         YYERROR;
      }
   }

   | T_AROB exp T_AROB {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Vector Dir\n");
#endif
      $$=Calc_Dir($2);

      if (!$$) {
         vexpr_error("Calc_Length failed: Critical!");
         YYERROR;
      }
   }

   | exp "(" T_INT ")" "(" ")" "(" ")" {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Slicing over I\n");
#endif
      if (!$1 || (int)$3>$1->NI-1 || (int)$3<0) {
         vexpr_error("Invalid index");
         YYERROR;
      } else {
         $$=Calc_Slice($1,(int)$3,0);

         if (!$$) {
            vexpr_error("Calc_Slice failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp "(" T_INT ")" "(" ")" "(" ")" T_ASSIGN exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Slice setting over I\n");
#endif
      if (!$1 || (int)$3>$1->NI-1 || (int)$3<0) {
         vexpr_error("Invalid index");
         YYERROR;
      } else {
         $$=Calc_Set($1,$10,(int)$3,(int)$3,0,$1->NJ-1,0,$1->NK-1);

         if (!$$) {
            vexpr_error("Calc_Set setting failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp "(" ")" "(" T_INT ")" "(" ")" {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Slicing over J\n");
#endif
      if (!$1 || (int)$5>$1->NJ-1 || (int)$5<0) {
         vexpr_error("Invalid index");
         YYERROR;
      } else {
         $$=Calc_Slice($1,(int)$5,1);

         if (!$$) {
            vexpr_error("Calc_Slice failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp "(" ")" "(" T_INT ")" "(" ")" T_ASSIGN exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Slice setting over J\n");
#endif
      if (!$1 || (int)$5>$1->NJ-1 || (int)$5<0) {
         vexpr_error("Invalid index");
         YYERROR;
      } else {
         $$=Calc_Set($1,$10,0,$1->NI-1,(int)$5,(int)$5,0,$1->NK-1);

         if (!$$) {
            vexpr_error("Calc_Set setting failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp "(" ")" "(" ")" "(" T_INT ")" {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Slicing over K\n");
#endif
      if (!$1 || (int)$7>$1->NK-1 || (int)$7<0) {
         vexpr_error("Invalid index");
         YYERROR;
      } else {
         $$=Calc_Slice($1,(int)$7,2);

         if (!$$) {
            vexpr_error("Calc_Slice failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp "(" ")" "(" ")" "(" T_INT ")" T_ASSIGN exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Slice setting over K\n");
#endif
      if (!$1 || (int)$7>$1->NK-1 || (int)$7<0) {
         vexpr_error("Invalid index");
         YYERROR;
      } else {
         $$=Calc_Set($1,$10,0,$1->NI-1,0,$1->NJ-1,(int)$7,(int)$7);

         if (!$$) {
            vexpr_error("Calc_Set setting failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp "(" T_INT "," T_INT ")" {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Value Indexing\n");
#endif
      if (!$1 || (int)$3>$1->NI-1 || (int)$3<0 || (int)$5>$1->NJ-1 || (int)$5<0) {
         vexpr_error("Invalid grid index");
         YYERROR;
      } else {
         $$=Calc_IndexValue($1,(int)$3,(int)$5,0);

         if (!$$) {
            vexpr_error("Calc_IndexValue failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp "(" T_INT "," T_INT ")" T_ASSIGN exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Value Setting\n");
#endif
      if (!$1 || (int)$3>$1->NI-1 || (int)$3<0 || (int)$5>$1->NJ-1 || (int)$5<0) {
         vexpr_error("Invalid grid index");
         YYERROR;
      } else {
         $$=Calc_Set($1,$8,(int)$3,(int)$3,(int)$5,(int)$5,0,0);

         if (!$$) {
            vexpr_error("Calc_IndexSetting failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp "(" T_INT "," T_INT "," T_INT ")" {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Value Indexing\n");
#endif
      if (!$1 || (int)$3>$1->NI-1 || (int)$3<0 || (int)$5>$1->NJ-1 || (int)$5<0 || (int)$7>$1->NK-1 || (int)$7<0) {
         vexpr_error("Invalid grid-level index");
         YYERROR;
      } else {
         $$=Calc_IndexValue($1,(int)$3,(int)$5,(int)$7);

         if (!$$) {
            vexpr_error("Calc_IndexValue failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp "(" T_INT "," T_INT "," T_INT ")" T_ASSIGN exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Value Setting\n");
#endif
      if (!$1 || (int)$3>$1->NI-1 || (int)$3<0 || (int)$5>$1->NJ-1 || (int)$5<0 || (int)$7>$1->NK-1 || (int)$7<0) {
         vexpr_error("Invalid grid-level index");
         YYERROR;
      } else {
         $$=Calc_Set($1,$10,(int)$3,(int)$3,(int)$5,(int)$5,(int)$7,(int)$7);

         if (!$$) {
            vexpr_error("Calc_IndexSetting failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp "(" "(" T_INT "," T_INT ")" "," "(" T_INT "," T_INT ")" ")" {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Range Indexing\n");
#endif
      if (!$1 || (int)$4>$1->NI-1 || (int)$4<0 || (int)$6>$1->NI-1 || (int)$6<0 || (int)$10>$1->NJ-1 || (int)$10<0 || (int)$12>$1->NJ-1 || (int)$12<0) {
         vexpr_error("Invalid grid range");
         YYERROR;
      } else {
         $$=Calc_RangeValue($1,(int)$4,(int)$6,(int)$10,(int)$12,0,0);

         if (!$$) {
            vexpr_error("Calc_RangeValue failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp "(" "(" T_INT "," T_INT ")" "," "(" T_INT "," T_INT ")" ")" T_ASSIGN exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Range Setting\n");
#endif
      if (!$1 || (int)$4>$1->NI-1 || (int)$4<0 || (int)$6>$1->NI-1 || (int)$6<0 || (int)$10>$1->NJ-1 || (int)$10<0 || (int)$12>$1->NJ-1 || (int)$12<0) {
         vexpr_error("Invalid grid range");
         YYERROR;
      } else {
         $$=Calc_Set($1,$16,(int)$4,(int)$6,(int)$10,(int)$12,0,0);

         if (!$$) {
            vexpr_error("Calc_RangeSetting failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp "(" "(" T_INT "," T_INT ")" "," "(" T_INT "," T_INT ")" "," "(" T_INT "," T_INT ")" ")" {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Range Indexing\n");
#endif
      if (!$1 || (int)$4>$1->NI-1 || (int)$4<0 || (int)$6>$1->NI-1 || (int)$6<0 ||
          (int)$10>$1->NJ-1 || (int)$10<0 || (int)$12>$1->NJ-1 || (int)$12<0 ||
          (int)$16>$1->NK-1 || (int)$16<0 || (int)$18>$1->NK-1 || (int)$18<0) {
         vexpr_error("Invalid grid range");
         YYERROR;
      } else {
         $$=Calc_RangeValue($1,(int)$4,(int)$6,(int)$10,(int)$12,(int)$16,(int)$18);

         if (!$$) {
            vexpr_error("Calc_RangeValue failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp "(" "(" T_INT "," T_INT ")" "," "(" T_INT "," T_INT ")" "," "(" T_INT "," T_INT ")" ")" T_ASSIGN exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Range Setting\n");
#endif
      if (!$1 || (int)$4>$1->NI-1 || (int)$4<0 || (int)$6>$1->NI-1 || (int)$6<0 ||
          (int)$10>$1->NJ-1 || (int)$10<0 || (int)$12>$1->NI-1 || (int)$12<0 ||
          (int)$16>$1->NK-1 || (int)$16<0 || (int)$18>$1->NI-1 || (int)$18<0) {
         vexpr_error("Invalid grid range");
         YYERROR;
      } else {
         $$=Calc_Set($1,$22,(int)$4,(int)$6,(int)$10,(int)$12,(int)$16,(int)$18);

         if (!$$) {
            vexpr_error("Calc_RangeSetting failed: Critical!");
            YYERROR;
         }
      }
   }

   | T_FNCT_F "(" farg ")" {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Function call (F) : %s(%d args)\n",$1->Name,StackN);
#endif
      // Special case for chaed values of stats functions
      if( !StackN ) {
         while( StackN < $1->Args ) {
            Stack[StackN++] = NULL;
         }
      }

      if ($1->Args!=StackN) {
         vexpr_error("(T_FNCT_F): Invalid number of arguments");
         YYERROR;
      } else {
         $$=Calc_Matrix($1->Func,0,0,$1->Type,StackN,Stack);
         if (!$$) {
            vexpr_error("Calc_Matrix failed (T_FNCT_F): Critical!");
            YYERROR;
         }
      }
   }

   | T_FNCT_M "(" farg ")" {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Function call (M) : %s(%d args)\n",$1->Name,StackN);
#endif
      if ($1->Args!=StackN) {
         vexpr_error("(T_FNCT_M): Invalid number of arguments");
         YYERROR;
      } else {
         $$=Calc_Matrix($1->Func,1,0,$1->Type,StackN,Stack);
         if (!$$) {
            vexpr_error("Calc_Matrix failed (T_FNCT_M): Critical!");
            YYERROR;
         }
      }
   }

   | T_FNCT_D "(" farg ")" {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Function call (D) : %s(%d args)\n",$1->Name,StackN);
#endif
      if ($1->Args!=StackN) {
         vexpr_error("(T_FNCT_D): Invalid number of arguments");
         YYERROR;
      } else {
         $$=Calc_Matrix($1->Func,0,1,$1->Type,StackN,Stack);
         if (!$$) {
            vexpr_error("Calc_Matrix failed (T_FNCT_D): Critical!");
            YYERROR;
         }
      }
   }

   | T_FIELD_FUNC "(" T_INT "," T_INT ")" {
      $$ = 0;
#ifdef HAVE_RMN
      char buf[32];
      sprintf(buf,"%i",(int)$3);
      FSTD_FieldRead(GInterp,"TMPCALCXXXXXX",buf,(int)$5,-1,0,-1,-1,-1,0,0);
      Tcl_ResetResult(GInterp);

#ifdef DEBUG
      fprintf(stdout, "(DEBUG) FSTD_FieldRead finished\n");
#endif
      GFieldP=GField;
      GField=Data_Get("TMPCALCXXXXXX");
      GMode=GMode==T_VAL?T_FLD:GMode;

      $$ = GData[++GDataN]=Def_Copy(GField->Def);
#endif
      if (!$$) {
         vexpr_error("FSTD_Field failed (T_FIELD_FUNC): Critical!");
         YYERROR;
      }
   }

   | T_DATA {

      $$=$1;

      if (!$$) {
         vexpr_error("...Get failed (T_DATA): Critical!");
         YYERROR;
      }

#ifdef DEBUG
      fprintf(stdout,"(DEBUG) T_DATA:%p $$:%p\n",(void*)$1,(void*)$$);
#endif
   }

   | exp T_EQU exp {
      $$ = Calc_Matrixv(equ,1,0,TD_UByte,2,$1,$3);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_EQU): Critical!");
         YYERROR;
      }
   }

   | exp T_NEQ exp {
      $$ = Calc_Matrixv(neq,1,0,TD_UByte,2,$1,$3);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_NEQ): Critical!");
         YYERROR;
      }
   }

   | exp T_GRQ exp {
      $$ =Calc_Matrixv(grq,1,0,TD_UByte,2,$1,$3);

      if ($$ == 0) {
         vexpr_error("Calc_Matrix failed (T_GRQ): Critical!");
         YYERROR;
      }
   }

   | exp T_GRE exp {
      $$ = Calc_Matrixv(gre,1,0,TD_UByte,2,$1,$3);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_GRE): Critical!");
         YYERROR;
      }
   }

   | exp T_SMQ exp {
      $$ = Calc_Matrixv(smq,1,0,TD_UByte,2,$1,$3);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_SMQ): Critical!");
         YYERROR;
      }
   }

   | exp T_SMA exp {
      $$ = Calc_Matrixv(sma,1,0,TD_UByte,2,$1,$3);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_SMA): Critical!");
         YYERROR;
      }
   }

   | exp T_ADD exp {
      $$ = Calc_Matrixv(add,1,0,TD_Unknown,2,$1,$3);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_ADD): Critical!");
         YYERROR;
      }
   }

   | exp T_SUB exp {
      $$ = Calc_Matrixv(sub,1,0,TD_Unknown,2,$1,$3);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_SUB): Critical!");
         YYERROR;
      }
   }

   | exp T_MUL exp {
      $$ = Calc_Matrixv(mul,1,0,TD_Unknown,2,$1,$3);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_MUL): Critical!");
         YYERROR;
      }
   }

   | exp T_DIV exp {
      $$ = Calc_Matrixv(dvd,1,0,TD_Unknown,2,$1,$3);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_DIV): Critical!");
         YYERROR;
      }
   }

   | exp T_MOD exp {
      $$ = Calc_Matrixv(fmod,1,0,TD_Unknown,2,$1,$3);

      if (!$$) {
        vexpr_error("Calc_Matrix failed (T_DIV): Critical!");
        YYERROR;
      }
   }

   | T_SUB exp %prec T_NEG {
      $$ = Calc_Matrix(neg,1,0,TD_Unknown,1,&$2);

      if (!$$) {
        vexpr_error("Calc_Matrix failed (T_SUB): Critical!");
        YYERROR;
      }
   }

   | exp T_EXP exp {
      $$ = Calc_Matrixv(pow,1,0,TD_Unknown,2,$1,$3);

      if (!$$) {
        vexpr_error("Calc_Matrix failed (T_EXP): Critical!");
        YYERROR;
      }
   }

   |  T_NOT exp {
      $$ = Calc_Matrix(not,1,0,TD_UByte,1,&$2);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_NOT): Critical!");
         YYERROR;
      }
   }

   | exp T_AND exp {
      $$ = Calc_Matrixv(and,1,0,TD_UByte,2,$1,$3);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_AND): Critical!");
         YYERROR;
      }
   }

   | exp T_OR exp {
      $$ = Calc_Matrixv(or,1,0,TD_UByte,2,$1,$3);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_OR): Critical!");
         YYERROR;
      }
   }

   |  T_BNOT exp {
      $$ = Calc_Matrix(bnot,1,0,TD_Unknown,1,&$2);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_BNOT): Critical!");
         YYERROR;
      }
   }

   | exp T_BND exp {
      $$ = Calc_Matrixv(band,1,0,TD_Unknown,2,$1,$3);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_BND): Critical!");
         YYERROR;
      }
   }

   | exp T_BOR exp {
      $$ = Calc_Matrixv(bor,1,0,TD_Unknown,2,$1,$3);

      if (!$$) {
         vexpr_error("Calc_Matrix failed (T_BOR): Critical!");
         YYERROR;
      }
   }

   | exp T_INTERP2 exp {
      $$ = Calc_MatrixTo($1,$3,2);

      if (!$$) {
         vexpr_error("Calc_MatrixTo failed (T_INTERP2): Critical!");
         YYERROR;
      }
   }

   | exp T_INTERP3 exp {
      $$ = Calc_MatrixTo($1,$3,3);

      if (!$$) {
         vexpr_error("Calc_MatrixTo failed (T_INTERP3): Critical!");
         YYERROR;
      }
   }

   | "(" exp ")" {
      $$ = $2;
   }
;
