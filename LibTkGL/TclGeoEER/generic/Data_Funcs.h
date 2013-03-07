/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de manipulation de type de donnees.
 * Fichier   : Data_Funcs.h
 * Creation  : Aout 2000 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Liste des fonctions de base de la calculatrice de donnees.
 *
 * Remarques :
 *
 * License      :
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation,
 *    version 2.1 of the License.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the
 *    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 *    Boston, MA 02111-1307, USA.
 *
 *==============================================================================
 */

#ifndef _DATA_FUNCS_H_
#define _DATA_FUNCS_H_

#include "tclData.h"
#include "tclGDAL.h"

#define DSLOPE100 0
#define DSLOPEDEG 1
#define DASPECT   2
#define DDX       3
#define DDY       4
#define DDXX      5
#define DDYY      6
#define DDXY      7
#define DPCURVE   8
#define DTCURVE   9

typedef double (TFunc)();
typedef double (TFunc1)(double);
typedef double (TFunc2)(double,double);
typedef double (TFunc3)(double,double,double);

typedef struct TFuncDef {
  char      *Name;     /*Function Name*/
  TFunc     *Func;     /*Function Pointer*/
  char       Args;     /*Number of Arguments*/
  TData_Type Type;     /*Result data type*/
} TFuncDef;

extern TFuncDef FuncF[];
extern TFuncDef FuncM[];
extern TFuncDef FuncD[];

TFuncDef* FuncGet(TFuncDef *Funcs,char *Symbol);

/*Derivative functions*/
double darea(TDataDef *Res,TDataDef *Def,int Mode);
double dangle(TDataDef *Res,TDataDef *Def,int Mode);
double dlat(TDataDef *Res,TDataDef *Def,int Mode);
double dlon(TDataDef *Res,TDataDef *Def,int Mode);
double ddx(TDataDef *Res,TDataDef *Def,int Mode);
double ddy(TDataDef *Res,TDataDef *Def,int Mode);
double dslopedeg(TDataDef *Res,TDataDef *Def);
double dslope100(TDataDef *Res,TDataDef *Def);
double daspect(TDataDef *Res,TDataDef *Def);
double ddxfirst(TDataDef *Res,TDataDef *Def);
double ddyfirst(TDataDef *Res,TDataDef *Def);
double ddxsecond(TDataDef *Res,TDataDef *Def);
double ddysecond(TDataDef *Res,TDataDef *Def);
double ddxysecond(TDataDef *Res,TDataDef *Def);
double dprofcurve(TDataDef *Res,TDataDef *Def);
double dtangcurve(TDataDef *Res,TDataDef *Def);
double dcore(TDataDef *Res,TDataDef *Def,int Mode);
double in(TDataDef *Res,TDataDef *MA,TDataDef *MB);
double lut(TDataDef *Res,TDataDef *MA,TDataDef *MB,TDataDef *MC);
double slut(TDataDef *Res,TDataDef *MA,TDataDef *MB,TDataDef *MC);
double fkernel(TDataDef *Res,TDataDef *MA,TDataDef *MB);
double fcentile(TDataDef *Res,TDataDef *MA,TDataDef *MB,TDataDef *MC);
double fpeel(TDataDef *Res,TDataDef *MA);

/*Table operators*/
double tcount(TDataDef *Res,TDataDef *Table,TDataDef *MB);

/*Matrix reduction operations*/
double stat_all(TDataDef *MA,TDataDef *MB);   /* All stats */
double stat_na(TDataDef *MA,TDataDef *MB);    /* Number of rejected values */
double stat_rna(TDataDef *MA,TDataDef *MB);   /* Ratio of rejected values */
double stat_mb(TDataDef *MA,TDataDef *MB);    /* Mean bias */
double stat_me(TDataDef *MA,TDataDef *MB);    /* Mean error */
double stat_nmb(TDataDef *MA,TDataDef *MB);   /* Normalized mean bias */
double stat_nme(TDataDef *MA,TDataDef *MB);   /* Normalized mean error */
double stat_mnb(TDataDef *MA,TDataDef *MB);   /* Mean normalized bias */
double stat_mne(TDataDef *MA,TDataDef *MB);   /* Mean normalized error */
double stat_lmne(TDataDef *MA,TDataDef *MB);  /* Logarithmic mean normalized error */
double stat_lmnb(TDataDef *MA,TDataDef *MB);  /* Logarithmic mean normalized bias */
double stat_mfb(TDataDef *MA,TDataDef *MB);   /* Mean fractional bias */
double stat_mfe(TDataDef *MA,TDataDef *MB);   /* Mean fractional error */
double stat_rmse(TDataDef *MA,TDataDef *MB);  /* Root mean square error */
double stat_nrmse(TDataDef *MA,TDataDef *MB); /* Normalized Root mean square error */
double stat_corr(TDataDef *MA,TDataDef *MB);  /* Correlation coeficient */
double stat_covar(TDataDef *MA,TDataDef *MB); /* Covariance */
double stat_sdev(TDataDef *MA);               /* Standard deviation */
double stat_varx(TDataDef *MA);               /* Variance */
double stat_vary(TDataDef *MA);               /* Variance */
double stat_regb(TDataDef *MA,TDataDef *MB);  /* Regression coeficient (Slope of curve fitting) */
double stat_rega(TDataDef *MA,TDataDef *MB);  /* Regression coeficient (Delta of curve fitting) */
double stat_erra(TDataDef *MA,TDataDef *MB);  /* Standard Error for a */
double stat_errb(TDataDef *MA,TDataDef *MB);  /* Standard Error for b */
double stat_ssx(TDataDef *MA,TDataDef *MB);   /* Sum of squared values */
double stat_ssxy(TDataDef *MA,TDataDef *MB);  /* Sum of squared values X-Y */
double stat_maxb(TDataDef *M,TDataDef *MB);   /* Maximum bias*/
double stat_maxe(TDataDef *M,TDataDef *MB);   /* Maximum error*/
double stat_maxre(TDataDef *MA,TDataDef *MB); /* Maximum relative error*/
double stat_mre(TDataDef *MA,TDataDef *MB);   /* Mean Relative error*/
double stat_nb(TDataDef *M);                  /* Number of sample */
double stat_median(TDataDef *MA);             /* Median */
double stat_sum(TDataDef *M);                 /* Sum */
double stat_min(TDataDef *M);                 /* Minimum */
double stat_max(TDataDef *M);                 /* Maximum */
double stat_avg(TDataDef *M);                 /* Average */
double stat_avgx(TDataDef *M);                /* Average X*/
double stat_avgy(TDataDef *M);                /* Average Y*/
double stat_minx(TDataDef *M);                /* Minimum X*/
double stat_miny(TDataDef *M);                /* Minimum Y*/
double stat_maxx(TDataDef *M);                /* Maximum X*/
double stat_maxy(TDataDef *M);                /* Maximum Y*/

/*Base operators*/
double add(double a,double b);
double sub(double a,double b);
double mul(double a,double b);
double dvd(double a,double b);
double neg(double a);
double dif(double a,double b);

/*Logical operators*/
double equ(double a,double b);
double neq(double a,double b);
double grq(double a,double b);
double gre(double a,double b);
double sma(double a,double b);
double smq(double a,double b);

double not(double a);
double and(double a,double b);
double or(double a,double b);
double bnot(double a);
double band(double a,double b);
double bor(double a,double b);

/*Conditionnal operators*/
double min(double a,double b);
double max(double a,double b);
double clamp(double a,double b,double c);
double ifelse(double a,double b,double c);

double frand(double a,double b,double c);

#endif
