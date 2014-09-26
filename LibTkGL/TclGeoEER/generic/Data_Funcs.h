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
double win(TDataDef *Res,TDataDef *MA,TDataDef *MB,TDataDef *MC);
double lut(TDataDef *Res,TDataDef *MA,TDataDef *MB,TDataDef *MC);
double slut(TDataDef *Res,TDataDef *MA,TDataDef *MB,TDataDef *MC);
double fkernel(TDataDef *Res,TDataDef *MA,TDataDef *MB);
double fcentile(TDataDef *Res,TDataDef *MA,TDataDef *MB,TDataDef *MC);
double fpeel(TDataDef *Res,TDataDef *MA);

/*Table operators*/
double tcount(TDataDef *Res,TDataDef *Table,TDataDef *MB);
double tbin(TDataDef *Res,TDataDef *Table,TDataDef *MB);
double flipy(TDataDef *Res,TDataDef *MA);

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
double stat_sdevx(TDataDef *MA,TDataDef *MB); /* Standard deviation X*/
double stat_sdevy(TDataDef *MA,TDataDef *MB); /* Standard deviation Y*/
double stat_var(TDataDef *MA);                /* Variance */
double stat_varx(TDataDef *MA,TDataDef *MB);  /* Variance */
double stat_vary(TDataDef *MA,TDataDef *MB);  /* Variance */
double stat_regb(TDataDef *MA,TDataDef *MB);  /* Regression coeficient (Slope of curve fitting) */
double stat_rega(TDataDef *MA,TDataDef *MB);  /* Regression coeficient (Delta of curve fitting) */
double stat_erra(TDataDef *MA,TDataDef *MB);  /* Standard Error for a */
double stat_errb(TDataDef *MA,TDataDef *MB);  /* Standard Error for b */
double stat_ssx(TDataDef *MA,TDataDef *MB);   /* Sum of squared X values */
double stat_ssy(TDataDef *MA,TDataDef *MB);   /* Sum of squared Y values */
double stat_ssxy(TDataDef *MA,TDataDef *MB);  /* Sum of X*Y values */
double stat_maxb(TDataDef *M,TDataDef *MB);   /* Maximum bias*/
double stat_maxe(TDataDef *M,TDataDef *MB);   /* Maximum error*/
double stat_maxre(TDataDef *MA,TDataDef *MB); /* Maximum relative error*/
double stat_mre(TDataDef *MA,TDataDef *MB);   /* Mean Relative error*/
double stat_nb(TDataDef *M);                  /* Number of sample */
double stat_med(TDataDef *MA);                /* Median */
double stat_medx(TDataDef *MA,TDataDef *MB);  /* Median X*/
double stat_medy(TDataDef *MA,TDataDef *MB);  /* Median Y*/
double stat_unique(TDataDef *MA);             /* Number of unique values */
double stat_sum(TDataDef *M);                 /* Sum */
double stat_min(TDataDef *M);                 /* Minimum */
double stat_max(TDataDef *M);                 /* Maximum */
double stat_avg(TDataDef *M);                 /* Average */
double stat_sumx(TDataDef *M,TDataDef *MB);   /* Sum X*/
double stat_sumy(TDataDef *M,TDataDef *MB);   /* Sum Y*/
double stat_avgx(TDataDef *M,TDataDef *MB);   /* Average X*/
double stat_avgy(TDataDef *M,TDataDef *MB);   /* Average Y*/
double stat_minx(TDataDef *M,TDataDef *MB);   /* Minimum X*/
double stat_miny(TDataDef *M,TDataDef *MB);   /* Minimum Y*/
double stat_maxx(TDataDef *M,TDataDef *MB);   /* Maximum X*/
double stat_maxy(TDataDef *M,TDataDef *MB);   /* Maximum Y*/
double stat_nmse(TDataDef *MA,TDataDef *MB);  /* Normalized mean square error */
double stat_gmb(TDataDef *MA,TDataDef *MB);   /* Geometric mean bias */
double stat_gmv(TDataDef *MA,TDataDef *MB);   /* Geometric mean variance */
double stat_foex(TDataDef *MA,TDataDef *MB);  /* Factor of excedance */
double stat_fa2(TDataDef *MA,TDataDef *MB);   /* Factor of 2 */
double stat_fa5(TDataDef *MA,TDataDef *MB);   /* Factor of 5 */
double stat_fa10(TDataDef *MA,TDataDef *MB);  /* Factor of 10 */
double stat_fb(TDataDef *MA,TDataDef *MB);    /* Fractionnal bias */
double stat_nad(TDataDef *MA,TDataDef *MB);   /* Normalized absolute difference */
double stat_fms(TDataDef *MA,TDataDef *MB);   /* Figure of merit in space (area) */
double stat_fmsb(TDataDef *MA,TDataDef *MB);  /* Figure of merit in space (binary) */
double stat_fmsi(TDataDef *MA,TDataDef *MB);  /* Figure of merit in space (integrated) */
double stat_osf(TDataDef *MA,TDataDef *MB);   /* Objective scoring function (area) */
double stat_osfb(TDataDef *MA,TDataDef *MB);  /* Objective scoring function (binary) */
double stat_osfi(TDataDef *MA,TDataDef *MB);  /* Objective scoring function (integrated) */
double stat_pcc(TDataDef *MA,TDataDef *MB);   /* Pearson's correlation coefficient */
double stat_ksp(TDataDef *MA,TDataDef *MB);   /* Kolmogorov-Smirnov parameter */
double stat_rank(TDataDef *MA,TDataDef *MB);  /* Rank calculated with a combination of fb, fms, fa2, foex, ksp and nad. ranges from 0 to 7 (the higher the better) */
double stat_nbeq(TDataDef *MA,TDataDef *MB);  /* Nb of equal values */
double stat_nbgt(TDataDef *MA,TDataDef *MB);  /* Nb of values of MB greater than MA */
double stat_nblt(TDataDef *MA,TDataDef *MB);  /* Nb of values of MB smaller than MA */
double stat_nbfa(TDataDef *MA,TDataDef *MB);  /* Nb of false alarms (MA=0, MB!=0)*/
double stat_nbmi(TDataDef *MA,TDataDef *MB);  /* Nb of misses (MA!=0, MB=0) */
double stat_nbnp(TDataDef *MA,TDataDef *MB);  /* Nb of null pairs (MA=0, MB=0) */
double stat_aov(TDataDef *MA,TDataDef *MB);   /* Overlap area */
double stat_afn(TDataDef *MA,TDataDef *MB);   /* False negative area */
double stat_afp(TDataDef *MA,TDataDef *MB);   /* False positive area */
double stat_ax(TDataDef *MA,TDataDef *MB);    /* Area X */
double stat_ay(TDataDef *MA,TDataDef *MB);    /* Area Y */

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
double within(double a,double b,double c);
double ifelse(double a,double b,double c);

double frand(double a,double b,double c);

#endif
