%{

#include <assert.h>
#include <math.h>        /* For math functions, cos(), sin(), etc. */
#include <stdio.h>

#include "tclFSTD.h"
#include "tclGDAL.h"
#include "tclObs.h"
#include "tclVector.h"
#include "Data_Funcs.h"
#include "Data_Matrix.h"
#include "y.tab.h"
#include "Data_Calc.h"

int vexpr_lex(void);

extern char      *curPos;
extern int        stopGuard;
extern TData     *GField,*GFieldP;
extern GDAL_Band *GBand;
extern OGR_Layer *GLayer;
extern TObs      *GObs;
extern TVector   *GVec;
extern TData_Type GType;
extern int        GMode;
extern TDataDef  *GData[1024];
extern int        GDataN;

/*
 * This macro redefines input for flex, iterating over a global
 * string given in the parse() call.
 */
#define YY_INPUT(buf,result,max_size) \
  result = 0; \
  while(*curPos != '\0' && result < max_size) \
  { buf[result] = *(curPos++); \
    result++; } \
  if(*curPos == '\0' && stopGuard != 1) \
  { buf[result] = ';'; \
    result++; \
    stopGuard = 1; }

#define _POSIX_SOURCE 1

%}

DIGIT   [0-9]
HEXA    [A-Fa-f0-9]
CHAR    [A-Za-z_]
EXTCHAR [A-Za-z0-9_.:#]

%option noyywrap

%%

"field" {
   return T_FIELD_FUNC;
}

("0x"{HEXA}+) {
   int i;

   sscanf(yytext,"%i",&i);
   vexpr_lval.Num = i;
   return T_INT;
}

{DIGIT}+ {
   vexpr_lval.Num = atof(yytext);
   return T_INT;
}

{DIGIT}+("."{DIGIT}+)?(("e"|"E")("-"|"+")?{DIGIT}+)? {
   vexpr_lval.Num = atof(yytext);
   return T_FLOAT;
}

("."{DIGIT}+)?(("e"|"E")("-"|"+")?{DIGIT}+)? {
   vexpr_lval.Num = atof(yytext);
   return T_FLOAT;
}

{CHAR}+{EXTCHAR}* {
   if((vexpr_lval.Func = FuncGet(FuncF,yytext)))  {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Found a matrix to float function token: %s\n", yytext);
#endif
      return T_FNCT_F;
   }

   if((vexpr_lval.Func = FuncGet(FuncM,yytext)))  {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Found a matrix to matrix function token: %s\n", yytext);
#endif
      return T_FNCT_M;
   }

   if((vexpr_lval.Func = FuncGet(FuncD,yytext)))  {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Found a matrix derivative function token: %s\n", yytext);
#endif
      return T_FNCT_D;
   }

   vexpr_lval.Data=NULL;

   if((vexpr_lval.Field=Data_Get(yytext))) {
      GFieldP=vexpr_lval.Field;
      GField=GField?GField:vexpr_lval.Field;
      GMode=GMode==T_VAL?T_FLD:GMode;
      if((vexpr_lval.Data=(vexpr_lval.Field->SDef?vexpr_lval.Field->SDef[0]:vexpr_lval.Field->Def))) {
#ifdef DEBUG
        fprintf(stdout,"(DEBUG) Found a T_DATA(Field) token: %s\n", yytext);
#endif
        return T_DATA;
      }
   }

   if((vexpr_lval.Band=GDAL_BandGet(yytext))) {
      GBand=GBand?GBand:vexpr_lval.Band;
      GMode=GMode==T_VAL?T_BAND:GMode;

      if((vexpr_lval.Data=vexpr_lval.Band->Def)) {
#ifdef DEBUG
        fprintf(stdout,"(DEBUG) Found a T_DATA(Band) token: %s\n", yytext);
#endif
        return T_DATA;
      }
   }

   if((vexpr_lval.Obs=Obs_Get(yytext))) {
      GObs=GObs?GObs:vexpr_lval.Obs;
      GMode=GMode==T_VAL?T_OBS:GMode;
      /*Force type to float since observations are only in float*/
      if (GMode==T_OBS) GType=TD_Float32;

      if((vexpr_lval.Data=vexpr_lval.Obs->Def)) {
#ifdef DEBUG
        fprintf(stdout,"(DEBUG) Found a T_DATA(Obs) token: %s\n", yytext);
#endif
        return T_DATA;
      }
   }

   if((vexpr_lval.Vec=Vector_Get(yytext))) {
      GVec=GVec?GVec:vexpr_lval.Vec;
      GMode=GMode==T_VAL?T_VEC:GMode;
      /*Force type to double since vectors are only in double*/
      if (GMode==T_VEC) GType=TD_Float64;

      if((vexpr_lval.Data=Vector_GetDef(vexpr_lval.Vec))) {
#ifdef DEBUG
        fprintf(stdout,"(DEBUG) Found a T_DATA(Vector) token: %s\n", yytext);
#endif
        return T_DATA;
      }
   }

   if((vexpr_lval.Layer=OGR_LayerGet(yytext))) {
      GLayer=GLayer?GLayer:vexpr_lval.Layer;
      GMode=GMode==T_VAL?T_LAYER:GMode;

      if (rindex(yytext,'.') && (vexpr_lval.Data=OGR_LayerToDef(vexpr_lval.Layer,rindex(yytext,'.')+1))) {
         /*Add it to the temp arrays to be freed*/
         GDataN++;
         GData[GDataN]=vexpr_lval.Data;
#ifdef DEBUG
        fprintf(stdout,"(DEBUG) Found a T_DATA(OGR) token: %s\n", yytext);
#endif
        return T_DATA;
      }
   }
#ifdef DEBUG
      fprintf(stdout, "(DEBUG) Found an error: %s\n", yytext);
#endif
   return T_ERROR;
}

"!=" {
   vexpr_lval.Operator = OP_NEQ;
   return T_NEQ;
}

"==" {
   vexpr_lval.Operator = OP_EQU;
   return T_EQU;
}

">=" {
   vexpr_lval.Operator = OP_GRQ;
   return T_GRQ;
}

">" {
   vexpr_lval.Operator = OP_GRE;
   return T_GRE;
}

"<=" {
   vexpr_lval.Operator = OP_SMQ;
   return T_SMQ;
}

"<" {
   vexpr_lval.Operator = OP_SMA;
   return T_SMA;
}

"+" {
   vexpr_lval.Operator = OP_ADD;
   return T_ADD;
}

"-" {
   vexpr_lval.Operator = OP_SUB;
   return T_SUB;
}

"*" {
   vexpr_lval.Operator = OP_MUL;
   return T_MUL;
}

"/" {
   vexpr_lval.Operator = OP_DIV;
   return T_DIV;
}

"%" {
   vexpr_lval.Operator = OP_MOD;
   return T_MOD;
}

"^" {
   vexpr_lval.Operator = OP_EXP;
   return T_EXP;
}

"!" {
   vexpr_lval.Operator = OP_NOT;
   return T_NOT;
}

"=" {
   vexpr_lval.Operator = OP_ASSIGN;
   return T_ASSIGN;
}

"&&" {
   vexpr_lval.Operator = OP_AND;
   return T_AND;
}

"||" {
   vexpr_lval.Operator = OP_OR;
   return T_OR;
}

"~" {
   vexpr_lval.Operator = OP_BNOT;
   return T_BNOT;
}

"&" {
   vexpr_lval.Operator = OP_BND;
   return T_BND;
}

"|" {
   vexpr_lval.Operator = OP_BOR;
   return T_BOR;
}

";" {
   vexpr_lval.Operator = LINE_TERMINATOR;
   return T_LINE_TERMINATOR;
}

"@" {
   vexpr_lval.Operator = OP_AROB;
   return T_AROB;
}
"," {
   vexpr_lval.Operator = OP_COMMA;
   return T_COMMA;
}

"(" {
   vexpr_lval.Operator = OPEN_PAR;
   return T_OPEN_PAR;
}

")" {
   vexpr_lval.Operator = CLOSE_PAR;
   return T_CLOSE_PAR;
}

"[" {
   vexpr_lval.Operator = OPEN_BRA;
   return T_OPEN_BRA;
}

"]" {
   vexpr_lval.Operator = CLOSE_BRA;
   return T_CLOSE_BRA;
}

"<<" {
   vexpr_lval.Operator = OP_INTERP2;
   return T_INTERP2;
}

"<<<" {
   vexpr_lval.Operator = OP_INTERP3;
   return T_INTERP3;
}
