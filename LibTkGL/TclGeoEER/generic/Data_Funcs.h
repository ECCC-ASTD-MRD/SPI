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
  char      *Name;     // Function Name
  TFunc     *Func;     // Function Pointer
  char       Args;     // Number of Arguments
  TDef_Type  Type;     // Result data type
} TFuncDef;

extern TFuncDef FuncF[];
extern TFuncDef FuncM[];
extern TFuncDef FuncD[];

TFuncDef* FuncGet(TFuncDef *Funcs,char *Symbol);

// Derivative functions
double darea(TDef *Res,TDef *Def,int Mode);
double dangle(TDef *Res,TDef *Def,int Mode);
double dlat(TDef *Res,TDef *Def,int Mode);
double dlon(TDef *Res,TDef *Def,int Mode);
double ddx(TDef *Res,TDef *Def,int Mode);
double ddy(TDef *Res,TDef *Def,int Mode);
double dslopedeg(TDef *Res,TDef *Def);
double dslope100(TDef *Res,TDef *Def);
double daspect(TDef *Res,TDef *Def);
double ddxfirst(TDef *Res,TDef *Def);
double ddyfirst(TDef *Res,TDef *Def);
double ddxsecond(TDef *Res,TDef *Def);
double ddysecond(TDef *Res,TDef *Def);
double ddxysecond(TDef *Res,TDef *Def);
double dprofcurve(TDef *Res,TDef *Def);
double dtangcurve(TDef *Res,TDef *Def);
double dcore(TDef *Res,TDef *Def,int Mode);
double in(TDef *Res,TDef *MA,TDef *MB);
double win(TDef *Res,TDef *MA,TDef *MB,TDef *MC);
double lut(TDef *Res,TDef *MA,TDef *MB,TDef *MC);
double slut(TDef *Res,TDef *MA,TDef *MB,TDef *MC);
double fkernel(TDef *Res,TDef *MA,TDef *MB);
double fcentile(TDef *Res,TDef *MA,TDef *MB,TDef *MC);
double fpeel(TDef *Res,TDef *MA);

// Table operators
double tcount(TDef *Res,TDef *Table,TDef *MB);
double tbin(TDef *Res,TDef *Table,TDef *MB);
double flipy(TDef *Res,TDef *MA);

// Matrix reduction operations
double stat_all(TDef *MA,TDef *MB);   // All stats 
double stat_na(TDef *MA,TDef *MB);    // Number of rejected values 
double stat_rna(TDef *MA,TDef *MB);   // Ratio of rejected values 
double stat_mb(TDef *MA,TDef *MB);    // Mean bias 
double stat_me(TDef *MA,TDef *MB);    // Mean error 
double stat_nmb(TDef *MA,TDef *MB);   // Normalized mean bias 
double stat_nme(TDef *MA,TDef *MB);   // Normalized mean error 
double stat_mnb(TDef *MA,TDef *MB);   // Mean normalized bias 
double stat_mne(TDef *MA,TDef *MB);   // Mean normalized error 
double stat_lmne(TDef *MA,TDef *MB);  // Logarithmic mean normalized error 
double stat_lmnb(TDef *MA,TDef *MB);  // Logarithmic mean normalized bias 
double stat_mfb(TDef *MA,TDef *MB);   // Mean fractional bias 
double stat_mfe(TDef *MA,TDef *MB);   // Mean fractional error 
double stat_rmse(TDef *MA,TDef *MB);  // Root mean square error 
double stat_nrmse(TDef *MA,TDef *MB); // Normalized Root mean square error 
double stat_corr(TDef *MA,TDef *MB);  // Correlation coeficient 
double stat_covar(TDef *MA,TDef *MB); // Covariance 
double stat_sdev(TDef *MA);           // Standard deviation 
double stat_sdevx(TDef *MA,TDef *MB); // Standard deviation X
double stat_sdevy(TDef *MA,TDef *MB); // Standard deviation Y
double stat_var(TDef *MA);            // Variance 
double stat_varx(TDef *MA,TDef *MB);  // Variance 
double stat_vary(TDef *MA,TDef *MB);  // Variance 
double stat_regb(TDef *MA,TDef *MB);  // Regression coeficient (Slope of curve fitting) 
double stat_rega(TDef *MA,TDef *MB);  // Regression coeficient (Delta of curve fitting) 
double stat_erra(TDef *MA,TDef *MB);  // Standard Error for a 
double stat_errb(TDef *MA,TDef *MB);  // Standard Error for b 
double stat_ssx(TDef *MA,TDef *MB);   // Sum of squared X values 
double stat_ssy(TDef *MA,TDef *MB);   // Sum of squared Y values 
double stat_ssxy(TDef *MA,TDef *MB);  // Sum of X*Y values 
double stat_maxb(TDef *M,TDef *MB);   // Maximum bias
double stat_maxe(TDef *M,TDef *MB);   // Maximum error
double stat_maxre(TDef *MA,TDef *MB); // Maximum relative error
double stat_mre(TDef *MA,TDef *MB);   // Mean Relative error
double stat_nb(TDef *M);              // Number of sample 
double stat_med(TDef *MA);            // Median 
double stat_medx(TDef *MA,TDef *MB);  // Median X
double stat_medy(TDef *MA,TDef *MB);  // Median Y
double stat_unique(TDef *MA);         // Number of unique values 
double stat_sum(TDef *M);             // Sum 
double stat_min(TDef *M);             // Minimum 
double stat_max(TDef *M);             // Maximum 
double stat_avg(TDef *M);             // Average 
double stat_sumx(TDef *M,TDef *MB);   // Sum X
double stat_sumy(TDef *M,TDef *MB);   // Sum Y
double stat_avgx(TDef *M,TDef *MB);   // Average X
double stat_avgy(TDef *M,TDef *MB);   // Average Y
double stat_minx(TDef *M,TDef *MB);   // Minimum X
double stat_miny(TDef *M,TDef *MB);   // Minimum Y
double stat_maxx(TDef *M,TDef *MB);   // Maximum X
double stat_maxy(TDef *M,TDef *MB);   // Maximum Y
double stat_nmse(TDef *MA,TDef *MB);  // Normalized mean square error 
double stat_gmb(TDef *MA,TDef *MB);   // Geometric mean bias 
double stat_gmv(TDef *MA,TDef *MB);   // Geometric mean variance 
double stat_foex(TDef *MA,TDef *MB);  // Factor of excedance 
double stat_fa2(TDef *MA,TDef *MB);   // Factor of 2 
double stat_fa5(TDef *MA,TDef *MB);   // Factor of 5 
double stat_fa10(TDef *MA,TDef *MB);  // Factor of 10 
double stat_fb(TDef *MA,TDef *MB);    // Fractionnal bias 
double stat_nad(TDef *MA,TDef *MB);   // Normalized absolute difference 
double stat_fms(TDef *MA,TDef *MB);   // Figure of merit in space (area) 
double stat_fmsb(TDef *MA,TDef *MB);  // Figure of merit in space (binary) 
double stat_fmsi(TDef *MA,TDef *MB);  // Figure of merit in space (integrated) 
double stat_osf(TDef *MA,TDef *MB);   // Objective scoring function (area) 
double stat_osfb(TDef *MA,TDef *MB);  // Objective scoring function (binary) 
double stat_osfi(TDef *MA,TDef *MB);  // Objective scoring function (integrated) 
double stat_ksp(TDef *MA,TDef *MB);   // Kolmogorov-Smirnov parameter 
double stat_rank(TDef *MA,TDef *MB);  // Rank calculated with a combination of fb, fms, fa2, foex, ksp and nad. ranges from 0 to 7 (the higher the better) 
double stat_nbeq(TDef *MA,TDef *MB);  // Nb of equal values 
double stat_nbgt(TDef *MA,TDef *MB);  // Nb of values of MB greater than MA 
double stat_nblt(TDef *MA,TDef *MB);  // Nb of values of MB smaller than MA 
double stat_nbfa(TDef *MA,TDef *MB);  // Nb of false alarms (MA=0, MB!=0)
double stat_nbmi(TDef *MA,TDef *MB);  // Nb of misses (MA!=0, MB=0) 
double stat_nbnp(TDef *MA,TDef *MB);  // Nb of null pairs (MA=0, MB=0) 
double stat_aov(TDef *MA,TDef *MB);   // Overlap area 
double stat_afn(TDef *MA,TDef *MB);   // False negative area 
double stat_afp(TDef *MA,TDef *MB);   // False positive area 
double stat_ax(TDef *MA,TDef *MB);    // Area X 
double stat_ay(TDef *MA,TDef *MB);    // Area Y 

// Base operators
double add(double a,double b);
double sub(double a,double b);
double mul(double a,double b);
double dvd(double a,double b);
double neg(double a);
double dif(double a,double b);

// Logical operators
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

// Conditionnal operators
double min(double a,double b);
double max(double a,double b);
double clamp(double a,double b,double c);
double within(double a,double b,double c);
double ifelse(double a,double b,double c);

double frand(double a,double b,double c);

#endif
