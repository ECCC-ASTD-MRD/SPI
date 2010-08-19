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
#include "Data_Calc.tab.h"
#include "Data_Calc.h"

extern char      *curPos;
extern int        stopGuard;
extern TData     *GField,*GFieldP;
extern GDAL_Band *GBand;
extern OGR_Layer *GLayer;
extern TObs      *GObs;
extern TVector   *GVec;
extern TData_Type GType;
extern int        GMode;

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

%}

DIGIT   [0-9]
CHAR    [A-Za-z_]
EXTCHAR [A-Za-z0-9_.:#~]

%option noyywrap

%%

"field" {
   return T_FIELD_FUNC;
}

{DIGIT}+ {
   yylval.Num = atof(yytext);
   return T_INT;
}

{DIGIT}+("."{DIGIT}+)?(("e"|"E")("-"|"+")?{DIGIT}+)? {
   yylval.Num = atof(yytext);
   return T_FLOAT;
}

("."{DIGIT}+)?(("e"|"E")("-"|"+")?{DIGIT}+)? {
   yylval.Num = atof(yytext);
   return T_FLOAT;
}

{CHAR}+{EXTCHAR}* {
   if((yylval.Func = FuncGet(FuncF,yytext)))  {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Found a matrix to float function token: %s\n", yytext);
#endif
      return T_FNCT_F;
   }

   if((yylval.Func = FuncGet(FuncM,yytext)))  {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Found a matrix to matrix function token: %s\n", yytext);
#endif
      return T_FNCT_M;
   }

   if((yylval.Func = FuncGet(FuncD,yytext)))  {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) Found a matrix derivative function token: %s\n", yytext);
#endif
      return T_FNCT_D;
   }

   yylval.Data=NULL;

   if((yylval.Field=Data_Get(yytext))) {
      GFieldP=yylval.Field;
      GField=GField?GField:yylval.Field;
      GMode=GMode==T_VAL?T_FLD:GMode;
      if((yylval.Data=yylval.Field->Def)) {
#ifdef DEBUG
        fprintf(stdout,"(DEBUG) Found a T_DATA(Field) token: %s\n", yytext);
#endif
        return T_DATA;
      }
   }

   if((yylval.Band=GDAL_BandGet(yytext))) {
      GBand=GBand?GBand:yylval.Band;
      GMode=GMode==T_VAL?T_BAND:GMode;

      if((yylval.Data=yylval.Band->Def)) {
#ifdef DEBUG
        fprintf(stdout,"(DEBUG) Found a T_DATA(Band) token: %s\n", yytext);
#endif
        return T_DATA;
      }
   }

   if((yylval.Obs=Obs_Get(yytext))) {
      GObs=GObs?GObs:yylval.Obs;
      GMode=GMode==T_VAL?T_OBS:GMode;
      /*Force type to float since observations are only in float*/
      if (GMode==T_OBS) GType=TD_Float32;

      if((yylval.Data=yylval.Obs->Def)) {
#ifdef DEBUG
        fprintf(stdout,"(DEBUG) Found a T_DATA(Obs) token: %s\n", yytext);
#endif
        return T_DATA;
      }
   }

   if((yylval.Vec=Vector_Get(yytext))) {
      GVec=GVec?GVec:yylval.Vec;
      GMode=GMode==T_VAL?T_VEC:GMode;
      /*Force type to double since vectors are only in double*/
      if (GMode==T_VEC) GType=TD_Float64;

      if((yylval.Data=Vector_GetDef(yylval.Vec))) {
#ifdef DEBUG
        fprintf(stdout,"(DEBUG) Found a T_DATA(Vector) token: %s\n", yytext);
#endif
        return T_DATA;
      }
   }

   if((yylval.Layer=OGR_LayerGet(yytext))) {
      GLayer=GLayer?GLayer:yylval.Layer;
      GMode=GMode==T_VAL?T_LAYER:GMode;

      if ((yylval.Data=OGR_LayerToDef(yylval.Layer,rindex(yytext,'.')+1))) {
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
   yylval.Operator = OP_NEQ;
   return T_NEQ;
}

"==" {
   yylval.Operator = OP_EQU;
   return T_EQU;
}

">=" {
   yylval.Operator = OP_GRQ;
   return T_GRQ;
}

">" {
   yylval.Operator = OP_GRE;
   return T_GRE;
}

"<=" {
   yylval.Operator = OP_SMQ;
   return T_SMQ;
}

"<" {
   yylval.Operator = OP_SMA;
   return T_SMA;
}

"+" {
   yylval.Operator = OP_ADD;
   return T_ADD;
}

"-" {
   yylval.Operator = OP_SUB;
   return T_SUB;
}

"*" {
   yylval.Operator = OP_MUL;
   return T_MUL;
}

"/" {
   yylval.Operator = OP_DIV;
   return T_DIV;
}

"%" {
   yylval.Operator = OP_MOD;
   return T_MOD;
}

"^" {
   yylval.Operator = OP_EXP;
   return T_EXP;
}

"!" {
   yylval.Operator = OP_NOT;
   return T_NOT;
}

"=" {
   yylval.Operator = OP_ASSIGN;
   return T_ASSIGN;
}

"&&" {
   yylval.Operator = OP_AND;
   return T_AND;
}

"||" {
   yylval.Operator = OP_OR;
   return T_OR;
}

";" {
   yylval.Operator = LINE_TERMINATOR;
   return T_LINE_TERMINATOR;
}

"@" {
   yylval.Operator = OP_AROB;
   return T_AROB;
}
"," {
   yylval.Operator = OP_COMMA;
   return T_COMMA;
}

"(" {
   yylval.Operator = OPEN_PAR;
   return T_OPEN_PAR;
}

")" {
   yylval.Operator = CLOSE_PAR;
   return T_CLOSE_PAR;
}

"[" {
   yylval.Operator = OPEN_BRA;
   return T_OPEN_BRA;
}

"]" {
   yylval.Operator = CLOSE_BRA;
   return T_CLOSE_BRA;
}

"<<" {
   yylval.Operator = OP_INTERP2;
   return T_INTERP2;
}

"<<<" {
   yylval.Operator = OP_INTERP3;
   return T_INTERP3;
}
