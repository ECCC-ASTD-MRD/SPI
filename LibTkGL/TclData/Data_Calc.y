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
extern TDataDef   *GResult;
extern TDataDef   *GData[256];
extern int         GDataN;
extern int         GError;
extern int         GMode;

int yyerror(char* s);
%}

%union {
  TDataDef   *Val;            /*!< For returning numbers. */
  double     Num;
  TDataDef   *Data;           /*!< If we found a data */
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

%type  <Val>       exp
%type  <Operator>  T_ADD T_SUB T_MUL T_DIV T_EXP T_INTERP2 T_INTERP3 T_ASSIGN
%type  <Operator>  T_EQU T_NEQ T_GRQ T_GRE T_SMQ T_SMA
%type  <Operator>  T_NOT T_AND T_OR T_AROB
%type  <Operator>  T_OPEN_BRA T_CLOSE_BRA
%type  <Operator>  T_OPEN_PAR T_CLOSE_PAR T_COMMA

%nonassoc          T_INTERP2 T_INTERP3 T_ASSIGN
%left              T_OR
%left              T_AND
%nonassoc          T_NOT
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

exp:
   T_ERROR {
      yyerror("Invalid function or data id in expression");
      YYERROR;
   }

   | T_INT {
      $$=Calc_MatrixInt((long)$1);
      if (!$$) {
         yyerror("Calc_Matrix failed (T_INT): Critical!");
         YYERROR;
      }
   }

   | T_FLOAT {
      $$=Calc_MatrixFloat($1);
      if (!$$) {
         yyerror("Calc_Matrix failed (T_FLOAT): Critical!");
         YYERROR;
      }
   }

   | exp T_OPEN_BRA T_INT T_CLOSE_BRA {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Component Indexing\n");
#endif
      if (!$1 || !$1->Data[(int)$3]) {
         yyerror("Invalid component index");
         YYERROR;
      } else {
         $$=Calc_Index($1,(int)$3);

         if (!$$) {
            yyerror("Calc_Index failed: Critical!");
            YYERROR;
         }
      }
   }

   | T_OPEN_BRA exp T_CLOSE_BRA {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Vector Length\n");
#endif
      $$=Calc_Length($2);

      if (!$$) {
         yyerror("Calc_Length failed: Critical!");
         YYERROR;
      }
   }

   | T_AROB exp T_AROB {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Vector Dir\n");
#endif
      $$=Calc_Dir($2);

      if (!$$) {
         yyerror("Calc_Length failed: Critical!");
         YYERROR;
      }
   }

   | exp T_OPEN_PAR T_INT T_CLOSE_PAR T_OPEN_PAR T_CLOSE_PAR T_OPEN_PAR T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Slicing over I\n");
#endif
      if (!$1 || (int)$3>$1->NI-1 || (int)$3<0) {
         yyerror("Invalid index");
         YYERROR;
      } else {
         $$=Calc_Slice($1,(int)$3,0);

         if (!$$) {
            yyerror("Calc_Slice failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp T_OPEN_PAR T_INT T_CLOSE_PAR T_OPEN_PAR T_CLOSE_PAR T_OPEN_PAR T_CLOSE_PAR T_ASSIGN exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Slice setting over I\n");
#endif
      if (!$1 || (int)$3>$1->NI-1 || (int)$3<0) {
         yyerror("Invalid index");
         YYERROR;
      } else {
         $$=Calc_Set($1,$10,(int)$3,(int)$3,0,$1->NJ-1,0,$1->NK-1);

         if (!$$) {
            yyerror("Calc_Slice setting failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp T_OPEN_PAR T_CLOSE_PAR T_OPEN_PAR T_INT T_CLOSE_PAR T_OPEN_PAR T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Slicing over J\n");
#endif
      if (!$1 || (int)$5>$1->NJ-1 || (int)$5<0) {
         yyerror("Invalid index");
         YYERROR;
      } else {
         $$=Calc_Slice($1,(int)$5,1);

         if (!$$) {
            yyerror("Calc_Slice failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp T_OPEN_PAR T_CLOSE_PAR T_OPEN_PAR T_INT T_CLOSE_PAR T_OPEN_PAR T_CLOSE_PAR T_ASSIGN exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Slice setting over J\n");
#endif
      if (!$1 || (int)$5>$1->NJ-1 || (int)$5<0) {
         yyerror("Invalid index");
         YYERROR;
      } else {
         $$=Calc_Set($1,$10,0,$1->NI-1,(int)$5,(int)$5,0,$1->NK-1);

         if (!$$) {
            yyerror("Calc_Slice setting failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp T_OPEN_PAR T_CLOSE_PAR T_OPEN_PAR T_CLOSE_PAR T_OPEN_PAR T_INT T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Slicing over K\n");
#endif
      if (!$1 || (int)$7>$1->NK-1 || (int)$7<0) {
         yyerror("Invalid index");
         YYERROR;
      } else {
         $$=Calc_Slice($1,(int)$7,2);

         if (!$$) {
            yyerror("Calc_Slice failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp T_OPEN_PAR T_CLOSE_PAR T_OPEN_PAR T_CLOSE_PAR T_OPEN_PAR T_INT T_CLOSE_PAR T_ASSIGN exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Slice setting over K\n");
#endif
      if (!$1 || (int)$7>$1->NK-1 || (int)$7<0) {
         yyerror("Invalid index");
         YYERROR;
      } else {
         $$=Calc_Set($1,$10,0,$1->NI-1,0,$1->NJ-1,(int)$7,(int)$7);

         if (!$$) {
            yyerror("Calc_Slice setting failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp T_OPEN_PAR T_INT T_COMMA T_INT T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Value Indexing\n");
#endif
      if (!$1 || (int)$3>$1->NI-1 || (int)$3<0 || (int)$5>$1->NJ-1 || (int)$5<0) {
         yyerror("Invalid grid index");
         YYERROR;
      } else {
         $$=Calc_IndexValue($1,(int)$3,(int)$5,0);

         if (!$$) {
            yyerror("Calc_IndexValue failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp T_OPEN_PAR T_INT T_COMMA T_INT T_CLOSE_PAR T_ASSIGN exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Value Setting\n");
#endif
      if (!$1 || (int)$3>$1->NI-1 || (int)$3<0 || (int)$5>$1->NJ-1 || (int)$5<0) {
         yyerror("Invalid grid index");
         YYERROR;
      } else {
         $$=Calc_Set($1,$8,(int)$3,(int)$3,(int)$5,(int)$5,0,0);

         if (!$$) {
            yyerror("Calc_IndexSetting failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp T_OPEN_PAR T_INT T_COMMA T_INT T_COMMA T_INT T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Value Indexing\n");
#endif
      if (!$1 || (int)$3>$1->NI-1 || (int)$3<0 || (int)$5>$1->NJ-1 || (int)$5<0 || (int)$7>$1->NK-1 || (int)$7<0) {
         yyerror("Invalid grid-level index");
         YYERROR;
      } else {
         $$=Calc_IndexValue($1,(int)$3,(int)$5,(int)$7);

         if (!$$) {
            yyerror("Calc_IndexValue failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp T_OPEN_PAR T_INT T_COMMA T_INT T_COMMA T_INT T_CLOSE_PAR T_ASSIGN exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Value Setting\n");
#endif
      if (!$1 || (int)$3>$1->NI-1 || (int)$3<0 || (int)$5>$1->NJ-1 || (int)$5<0 || (int)$7>$1->NK-1 || (int)$7<0) {
         yyerror("Invalid grid-level index");
         YYERROR;
      } else {
         $$=Calc_Set($1,$10,(int)$3,(int)$3,(int)$5,(int)$5,(int)$7,(int)$7);

         if (!$$) {
            yyerror("Calc_IndexSetting failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp T_OPEN_PAR T_OPEN_PAR T_INT T_COMMA T_INT T_CLOSE_PAR T_COMMA T_OPEN_PAR T_INT T_COMMA T_INT T_CLOSE_PAR T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Range Indexing\n");
#endif
      if (!$1 || (int)$4>$1->NI-1 || (int)$4<0 || (int)$6>$1->NI-1 || (int)$6<0 || (int)$10>$1->NJ-1 || (int)$10<0 || (int)$12>$1->NJ-1 || (int)$12<0) {
         yyerror("Invalid grid range");
         YYERROR;
      } else {
         $$=Calc_RangeValue($1,(int)$4,(int)$6,(int)$10,(int)$12,0,0);

         if (!$$) {
            yyerror("Calc_RangeValue failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp T_OPEN_PAR T_OPEN_PAR T_INT T_COMMA T_INT T_CLOSE_PAR T_COMMA T_OPEN_PAR T_INT T_COMMA T_INT T_CLOSE_PAR T_CLOSE_PAR T_ASSIGN exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Range Setting\n");
#endif
      if (!$1 || (int)$4>$1->NI-1 || (int)$4<0 || (int)$6>$1->NI-1 || (int)$6<0 || (int)$10>$1->NJ-1 || (int)$10<0 || (int)$12>$1->NJ-1 || (int)$12<0) {
         yyerror("Invalid grid range");
         YYERROR;
      } else {
         $$=Calc_Set($1,$16,(int)$4,(int)$6,(int)$10,(int)$12,0,0);

         if (!$$) {
            yyerror("Calc_RangeSetting failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp T_OPEN_PAR T_OPEN_PAR T_INT T_COMMA T_INT T_CLOSE_PAR T_COMMA T_OPEN_PAR T_INT T_COMMA T_INT T_CLOSE_PAR T_COMMA T_OPEN_PAR T_INT T_COMMA T_INT T_CLOSE_PAR T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Range Indexing\n");
#endif
      if (!$1 || (int)$4>$1->NI-1 || (int)$4<0 || (int)$6>$1->NI-1 || (int)$6<0 ||
          (int)$10>$1->NJ-1 || (int)$10<0 || (int)$12>$1->NJ-1 || (int)$12<0 ||
          (int)$16>$1->NK-1 || (int)$16<0 || (int)$18>$1->NK-1 || (int)$18<0) {
         yyerror("Invalid grid range");
         YYERROR;
      } else {
         $$=Calc_RangeValue($1,(int)$4,(int)$6,(int)$10,(int)$12,(int)$16,(int)$18);

         if (!$$) {
            yyerror("Calc_RangeValue failed: Critical!");
            YYERROR;
         }
      }
   }

   | exp T_OPEN_PAR T_OPEN_PAR T_INT T_COMMA T_INT T_CLOSE_PAR T_COMMA T_OPEN_PAR T_INT T_COMMA T_INT T_CLOSE_PAR T_COMMA T_OPEN_PAR T_INT T_COMMA T_INT T_CLOSE_PAR T_CLOSE_PAR T_ASSIGN exp {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Range Setting\n");
#endif
      if (!$1 || (int)$4>$1->NI-1 || (int)$4<0 || (int)$6>$1->NI-1 || (int)$6<0 ||
          (int)$10>$1->NJ-1 || (int)$10<0 || (int)$12>$1->NI-1 || (int)$12<0 ||
          (int)$16>$1->NK-1 || (int)$16<0 || (int)$18>$1->NI-1 || (int)$18<0) {
         yyerror("Invalid grid range");
         YYERROR;
      } else {
         $$=Calc_Set($1,$22,(int)$4,(int)$6,(int)$10,(int)$12,(int)$16,(int)$18);

         if (!$$) {
            yyerror("Calc_RangeSetting failed: Critical!");
            YYERROR;
         }
      }
   }

   | T_FNCT_F T_OPEN_PAR T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Function call: %s\n",$1->Name);
#endif
      $$=Calc_Matrix1(NULL,(TFunc1*)$1->Func,0,0,$1->Type);

      if (!$$) {
         yyerror("Calc_Matrix1 failed (T_FNCT_F): Critical!");
         YYERROR;
      }
   }

   | T_FNCT_F T_OPEN_PAR exp T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Function call: %s\n",$1->Name);
#endif
      if ($1->Args!=1) {
         yyerror("(T_FNCT_F): Invalid number of arguments");
         YYERROR;
      } else {
         $$=Calc_Matrix1($3,(TFunc1*)$1->Func,0,0,$1->Type);

         if (!$$) {
            yyerror("Calc_Matrix1 failed (T_FNCT_F): Critical!");
            YYERROR;
         }
      }
   }

   | T_FNCT_F T_OPEN_PAR exp T_COMMA exp T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Function call: %s\n",$1->Name);
#endif
      if ($1->Args!=2) {
         yyerror("(T_FNCT_F): Invalid number of arguments");
         YYERROR;
      } else {
         $$=Calc_Matrix2($3,$5,(TFunc2*)$1->Func,0,0,$1->Type);

         if (!$$) {
            yyerror("Calc_Matrix2 failed (T_FNCT_F): Critical!");
            YYERROR;
         }
      }
   }

   | T_FNCT_M T_OPEN_PAR exp T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Function call: %s\n",$1->Name);
#endif
      if ($1->Args!=1) {
         yyerror("(T_FNCT_M): Invalid number of arguments");
         YYERROR;
      } else {
         $$=Calc_Matrix1($3,(TFunc1*)$1->Func,1,0,$1->Type);

         if (!$$) {
            yyerror("Calc_Matrix1 failed (T_FNCT_M): Critical!");
            YYERROR;
         }
      }
   }

   | T_FNCT_M T_OPEN_PAR exp T_COMMA exp T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout, "(DEBUG) Function (2) call: %s\n", $1->Name);
#endif
      if ($1->Args!=2) {
         yyerror("(T_FNCT_M): Invalid number of arguments");
         YYERROR;
      } else {
         $$=Calc_Matrix2($3,$5,(TFunc2*)$1->Func,1,0,$1->Type);

         if (!$$) {
            yyerror("Calc_Matrix2 failed (T_FNCT_M): Critical!");
            YYERROR;
         }
      }
   }

   | T_FNCT_M T_OPEN_PAR exp T_COMMA exp T_COMMA exp T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout, "(DEBUG) Function (3) call: \n", $1->Name);
#endif
      if ($1->Args!=3) {
         yyerror("(T_FNCT_M): Invalid number of arguments");
         YYERROR;
      } else {
         $$=Calc_Matrix3($3,$5,$7,(TFunc3*)$1->Func,1,0,$1->Type);

         if (!$$) {
            yyerror("Calc_Matrix3 failed (T_FNCT_M): Critical!");
            YYERROR;
         }
      }
   }

   | T_FNCT_D T_OPEN_PAR exp T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Function call: %s\n",$1->Name);
#endif
      if ($1->Args!=1) {
         yyerror("(T_FNCT_D): Invalid number of arguments");
         YYERROR;
      } else {
         $$=Calc_Matrix1($3,(TFunc1*)$1->Func,0,1,$1->Type);

         if (!$$) {
            yyerror("Calc_Matrix1 failed (T_FNCT_D): Critical!");
            YYERROR;
         }
      }
   }

   | T_FNCT_D T_OPEN_PAR exp T_COMMA exp T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout, "(DEBUG) Function (2) call: %s\n", $1->Name);
#endif
      if ($1->Args!=2) {
         yyerror("(T_FNCT_D): Invalid number of arguments");
         YYERROR;
      } else {
         $$=Calc_Matrix2($3,$5,(TFunc2*)$1->Func,0,1,$1->Type);

         if (!$$) {
            yyerror("Calc_Matrix2 failed (T_FNCT_D): Critical!");
            YYERROR;
         }
      }
   }

   | T_FNCT_D T_OPEN_PAR exp T_COMMA exp T_COMMA exp T_CLOSE_PAR {
#ifdef DEBUG
      fprintf(stdout, "(DEBUG) Function (3) call: \n", $1->Name);
#endif
      if ($1->Args!=3) {
         yyerror("(T_FNCT_D): Invalid number of arguments");
         YYERROR;
      } else {
         $$=Calc_Matrix3($3,$5,$7,(TFunc3*)$1->Func,0,1,$1->Type);

         if (!$$) {
            yyerror("Calc_Matrix3 failed (T_FNCT_D): Critical!");
            YYERROR;
         }
      }
   }

   | T_FIELD_FUNC T_OPEN_PAR T_INT T_COMMA T_INT T_CLOSE_PAR {
      char buf[32];
      $$ = 0;
      sprintf(buf,"%i",(int)$3);
      FSTD_FieldRead(GInterp,"TMPXXXXXX",buf,(int)$5,-1,0,-1,-1,-1,0,0);
      Tcl_ResetResult(GInterp);

#ifdef DEBUG
      fprintf(stdout, "(DEBUG) FSTD_FieldRead finished\n");
#endif
      GFieldP=GField;
      GField=Data_Get("TMPXXXXXX");
      GMode=GMode==T_VAL?T_FLD:GMode;

      $$ = GData[++GDataN]=DataDef_Copy(GField->Def);

      if (!$$) {
         yyerror("FSTD_Field failed (T_FIELD_FUNC): Critical!");
         YYERROR;
      }
   }

   | T_DATA {

      $$=$1;

      if (!$$) {
         yyerror("...Get failed (T_DATA): Critical!");
         YYERROR;
      }

#ifdef DEBUG
      fprintf(stdout,"(DEBUG) T_DATA:%p $$:%p\n",(void*)$1,(void*)$$);
#endif
   }

   | exp T_EQU exp {
      $$ = Calc_Matrix2($1, $3,equ,1,0,TD_UByte);

      if (!$$) {
         yyerror("Calc_Matrix2 failed (T_EQU): Critical!");
         YYERROR;
      }
   }

   | exp T_NEQ exp {
      $$ = Calc_Matrix2($1, $3,neq,1,0,TD_UByte);

      if (!$$) {
         yyerror("Calc_Matrix2 failed (T_NEQ): Critical!");
         YYERROR;
      }
   }

   | exp T_GRQ exp {
      $$ =Calc_Matrix2($1, $3,grq,1,0,TD_UByte);

      if ($$ == 0) {
         yyerror("Calc_Matrix2 failed (T_GRQ): Critical!");
         YYERROR;
      }
   }

   | exp T_GRE exp {
      $$ = Calc_Matrix2($1, $3,gre,1,0,TD_UByte);

      if (!$$) {
         yyerror("Calc_Matrix2 failed (T_GRE): Critical!");
         YYERROR;
      }
   }

   | exp T_SMQ exp {
      $$ = Calc_Matrix2($1, $3,smq,1,0,TD_UByte);

      if (!$$) {
         yyerror("Calc_Matrix2 failed (T_SMQ): Critical!");
         YYERROR;
      }
   }

   | exp T_SMA exp {
      $$ = Calc_Matrix2($1, $3,sma,1,0,TD_UByte);

      if (!$$) {
         yyerror("Calc_Matrix2 failed (T_SMA): Critical!");
         YYERROR;
      }
   }

   | exp T_ADD exp {
      $$ = Calc_Matrix2($1, $3,add,1,0,TD_Unknown);

      if (!$$) {
         yyerror("Calc_Matrix2 failed (T_ADD): Critical!");
         YYERROR;
      }
   }

   | exp T_SUB exp {
      $$ = Calc_Matrix2($1, $3,sub,1,0,TD_Unknown);

      if (!$$) {
         yyerror("Calc_Matrix2 failed (T_SUB): Critical!");
         YYERROR;
      }
   }

   | exp T_MUL exp {
      $$ = Calc_Matrix2($1, $3,mul,1,0,TD_Unknown);

      if (!$$) {
         yyerror("Calc_Matrix2 failed (T_MUL): Critical!");
         YYERROR;
      }
   }

   | exp T_DIV exp {
      $$ = Calc_Matrix2($1, $3,dvd,1,0,TD_Unknown);

      if (!$$) {
         yyerror("Calc_Matrix2 failed (T_DIV): Critical!");
         YYERROR;
      }
   }

   | exp T_MOD exp {
      $$ = Calc_Matrix2($1, $3,fmod,1,0,TD_Unknown);

      if (!$$) {
        yyerror("Calc_Matrix2 failed (T_DIV): Critical!");
        YYERROR;
      }
   }

   | T_SUB exp %prec T_NEG {
      $$ = Calc_Matrix1($2,neg,1,0,TD_Unknown);

      if (!$$) {
        yyerror("Calc_Matrix1 failed (T_SUB): Critical!");
        YYERROR;
      }
   }

   | exp T_EXP exp {
      $$ = Calc_Matrix2($1, $3,pow,1,0,TD_Unknown);

      if (!$$) {
        yyerror("Calc_Matrix2 failed (T_EXP): Critical!");
        YYERROR;
      }
   }

   |  T_NOT exp {
      $$ = Calc_Matrix1($2,not,1,0,TD_UByte);

      if (!$$) {
         yyerror("Calc_Matrix1 failed (T_NOT): Critical!");
         YYERROR;
      }
   }

   | exp T_AND exp {
      $$ = Calc_Matrix2($1, $3,and,1,0,TD_UByte);

      if (!$$) {
         yyerror("Calc_Matrix2 failed (T_AND): Critical!");
         YYERROR;
      }
   }

   | exp T_OR exp {
      $$ = Calc_Matrix2($1, $3,or,1,0,TD_UByte);

      if (!$$) {
         yyerror("Calc_Matrix2 failed (T_OR): Critical!");
         YYERROR;
      }
   }

   | exp T_INTERP2 exp {
      $$ = Calc_MatrixTo($1,$3,2);

      if (!$$) {
         yyerror("Calc_MatrixTo failed (T_INTERP2): Critical!");
         YYERROR;
      }
   }

   | exp T_INTERP3 exp {
      $$ = Calc_MatrixTo($1,$3,3);

      if (!$$) {
         yyerror("Calc_MatrixTo failed (T_INTERP3): Critical!");
         YYERROR;
      }
   }

   | T_OPEN_PAR exp T_CLOSE_PAR {
      $$ = $2;
   }
;
