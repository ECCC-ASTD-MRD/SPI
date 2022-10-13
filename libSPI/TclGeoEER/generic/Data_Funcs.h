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

typedef struct TFuncDef {
  char      *Name;     // Function Name
  TFunc     *Func;     // Function Pointer
  char       Args;     // Number of Arguments (total)
  char       Opts;     // Number of optional arguments
  TDef_Type  Type;     // Result data type
} TFuncDef;

extern TFuncDef FuncF[];
extern TFuncDef FuncM[];
extern TFuncDef FuncD[];
extern TFuncDef FuncC[];

TFuncDef* FuncGet(TFuncDef *Funcs,char *Symbol);

typedef struct StatCoreCtx {
   double Vnb,Vsumx,Vminx,Vmaxx,Vavgx,Vsumy,Vminy,Vmaxy,Vavgy,Vvarx,Vvary,Vssx,Vssy,Vssxy,Vrmse,Vcorr,Vcovar,
       Vregb,Vrega,Verra,Verrb,Vmb,Vnmb,Vnme,Vme,Vmnb,Vmaxb,Vmaxe,Vmre,Vmaxre,Vmedx,Vmedy,
       Vmne,Vmfb,Vmfe,Vlmnb,Vlmne,Vnrmse,Vna,Vrna,Vmse,Vnmse,Vgmb,Vgmv,Vfoex,Vfa2,Vfa5,Vfa10,Vfb,Vnad,
       Vfms,Vfmsi,Vfmsb,Vfmsn,Vosf,Vosfb,Vosfi,Vksp,Vrank,Vnbeq,Vnbgt,Vnblt,Vnbfa,Vnbmi,Vnbnp,
       Vaov,Vafn,Vafp,Vax,Vay;
   TDef   *withMA;
   TDef   *withMB;
   int     sizeMA;
} StatCore ;

#define STACK_MAX    128
#define STACKP_MAX   16

#define  CALC_CONTEXT         Calc_Ctx *ctx,
#define  CTX_STAT(s)          ctx->stat->(s)

typedef struct {
   Tcl_Interp   *GInterp;
   TDef_Type     GType;
   TData        *GField,*GFieldP;
   GDAL_Band    *GBand;
   OGR_Layer    *GLayer;
   TObs         *GObs;
   TVector      *GVec;
   TDef         *GResult;
   TDef         *GData[1024];
   int           GDataN;
   int           GMode;
   int           GForceFld;
   char         *curPos;
   char         *curTok;
   int           GError;
   int           stopGuard;
   int           GExcept;
   TDef         *STACK[STACK_MAX];
   int           STACKN;
   int           STACKP[STACKP_MAX];
   int           STACKPN;
   StatCore      *stat;
} Calc_Ctx ;

// Derivative functions
double darea(Calc_Ctx *ctx, TDef *Res,TDef *Def,int Mode);
double dcoriolis(Calc_Ctx *ctx, TDef *Res,TDef *Def,int Mode);
double dangle(Calc_Ctx *ctx, TDef *Res,TDef *Def,int Mode);
double dlat(Calc_Ctx *ctx, TDef *Res,TDef *Def,int Mode);
double dlon(Calc_Ctx *ctx, TDef *Res,TDef *Def,int Mode);
double ddx(Calc_Ctx *ctx, TDef *Res,TDef *Def,int Mode);
double ddy(Calc_Ctx *ctx, TDef *Res,TDef *Def,int Mode);
double dslopedeg(Calc_Ctx *ctx, TDef *Res,TDef *Def);
double dslope100(Calc_Ctx *ctx, TDef *Res,TDef *Def);
double daspect(Calc_Ctx *ctx, TDef *Res,TDef *Def);
double ddxfirst(Calc_Ctx *ctx, TDef *Res,TDef *Def);
double ddyfirst(Calc_Ctx *ctx, TDef *Res,TDef *Def);
double ddxsecond(Calc_Ctx *ctx, TDef *Res,TDef *Def);
double ddysecond(Calc_Ctx *ctx, TDef *Res,TDef *Def);
double ddxysecond(Calc_Ctx *ctx, TDef *Res,TDef *Def);
double dprofcurve(Calc_Ctx *ctx, TDef *Res,TDef *Def);
double dtangcurve(Calc_Ctx *ctx, TDef *Res,TDef *Def);
double dcore(Calc_Ctx *ctx, TDef *Res,TDef *Def,int Mode);
double in(Calc_Ctx *ctx, TDef *Res,TDef *MA,TDef *MB);
double win(Calc_Ctx *ctx, TDef *Res,TDef *MA,TDef *MB,TDef *MC);
double lut(Calc_Ctx *ctx, TDef *Res,TDef *MA,TDef *MB,TDef *MC);
double slut(Calc_Ctx *ctx, TDef *Res,TDef *MA,TDef *MB,TDef *MC);
double fkernel(Calc_Ctx *ctx, TDef *Res,TDef *MA,TDef *MB);
double fcentile(Calc_Ctx *ctx, TDef *Res,TDef *MA,TDef *MB,TDef *MC);
double fpeel(Calc_Ctx *ctx, TDef *Res,TDef *MA);
double dt(Calc_Ctx *ctx, TDef *Res,TDef *Fld);
double gdt(Calc_Ctx *ctx, TDef *Res,TDef *Fld,TDef *Q);

// Table operators
double tcount(Calc_Ctx *ctx, TDef *Res,TDef *Table,TDef *MB);
double tbin(Calc_Ctx *ctx, TDef *Res,TDef *Table,TDef *MB);
double flipy(Calc_Ctx *ctx, TDef *Res,TDef *MA);

// Matrix creation/manipulation functions
double seq(Calc_Ctx *ctx, TDef *Res,TDef *From,TDef *To,TDef *Step,TDef *N);
double reshape(Calc_Ctx *ctx, TDef *Res,TDef *Fld,TDef *NI,TDef *NJ,TDef *NK,TDef *NC);
double repeat(Calc_Ctx *ctx, TDef *Res,TDef *Fld,TDef *N);
double join(Calc_Ctx *ctx, TDef *Res,TDef *D,TDef *F1,TDef *F2,TDef *F3,TDef *F4,TDef *F5,TDef *F6,TDef *F7,TDef *F8);

// Matrix reduction operations
double stat_all(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // All stats 
double stat_na(Calc_Ctx *ctx, TDef *MA,TDef *MB);    // Number of rejected values 
double stat_rna(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Ratio of rejected values 
double stat_mb(Calc_Ctx *ctx, TDef *MA,TDef *MB);    // Mean bias 
double stat_me(Calc_Ctx *ctx, TDef *MA,TDef *MB);    // Mean error 
double stat_nmb(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Normalized mean bias 
double stat_nme(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Normalized mean error 
double stat_mnb(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Mean normalized bias 
double stat_mne(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Mean normalized error 
double stat_lmne(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Logarithmic mean normalized error 
double stat_lmnb(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Logarithmic mean normalized bias 
double stat_mfb(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Mean fractional bias 
double stat_mfe(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Mean fractional error 
double stat_rmse(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Root mean square error 
double stat_nrmse(Calc_Ctx *ctx, TDef *MA,TDef *MB); // Normalized Root mean square error 
double stat_corr(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Correlation coeficient 
double stat_covar(Calc_Ctx *ctx, TDef *MA,TDef *MB); // Covariance 
double stat_sdev(Calc_Ctx *ctx, TDef *MA);           // Standard deviation 
double stat_sdevx(Calc_Ctx *ctx, TDef *MA,TDef *MB); // Standard deviation X
double stat_sdevy(Calc_Ctx *ctx, TDef *MA,TDef *MB); // Standard deviation Y
double stat_var(Calc_Ctx *ctx, TDef *MA);            // Variance 
double stat_varx(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Variance 
double stat_vary(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Variance 
double stat_regb(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Regression coeficient (Slope of curve fitting) 
double stat_rega(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Regression coeficient (Delta of curve fitting) 
double stat_erra(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Standard Error for a 
double stat_errb(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Standard Error for b 
double stat_ssx(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Sum of squared X values 
double stat_ssy(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Sum of squared Y values 
double stat_ssxy(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Sum of X*Y values 
double stat_maxb(Calc_Ctx *ctx, TDef *M,TDef *MB);   // Maximum bias
double stat_maxe(Calc_Ctx *ctx, TDef *M,TDef *MB);   // Maximum error
double stat_maxre(Calc_Ctx *ctx, TDef *MA,TDef *MB); // Maximum relative error
double stat_mre(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Mean Relative error
double stat_nb(Calc_Ctx *ctx, TDef *M);              // Number of sample 
double stat_med(Calc_Ctx *ctx, TDef *MA);            // Median 
double stat_medx(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Median X
double stat_medy(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Median Y
double stat_unique(Calc_Ctx *ctx, TDef *MA);         // Number of unique values 
double stat_sum(Calc_Ctx *ctx, TDef *M);             // Sum 
double stat_min(Calc_Ctx *ctx, TDef *M);             // Minimum 
double stat_max(Calc_Ctx *ctx, TDef *M);             // Maximum 
double stat_avg(Calc_Ctx *ctx, TDef *M);             // Average 
double stat_sumx(Calc_Ctx *ctx, TDef *M,TDef *MB);   // Sum X
double stat_sumy(Calc_Ctx *ctx, TDef *M,TDef *MB);   // Sum Y
double stat_avgx(Calc_Ctx *ctx, TDef *M,TDef *MB);   // Average X
double stat_avgy(Calc_Ctx *ctx, TDef *M,TDef *MB);   // Average Y
double stat_minx(Calc_Ctx *ctx, TDef *M,TDef *MB);   // Minimum X
double stat_miny(Calc_Ctx *ctx, TDef *M,TDef *MB);   // Minimum Y
double stat_maxx(Calc_Ctx *ctx, TDef *M,TDef *MB);   // Maximum X
double stat_maxy(Calc_Ctx *ctx, TDef *M,TDef *MB);   // Maximum Y
double stat_mse(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Mean square error 
double stat_nmse(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Normalized mean square error 
double stat_gmb(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Geometric mean bias 
double stat_gmv(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Geometric mean variance 
double stat_foex(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Factor of excedance 
double stat_fa2(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Factor of 2 
double stat_fa5(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Factor of 5 
double stat_fa10(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Factor of 10 
double stat_fb(Calc_Ctx *ctx, TDef *MA,TDef *MB);    // Fractionnal bias 
double stat_nad(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Normalized absolute difference 
double stat_fms(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Figure of merit in space (area) 
double stat_fmsb(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Figure of merit in space (binary) 
double stat_fmsi(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Figure of merit in space (integrated) 
double stat_fmsn(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Figure of merit in space (integrated) 
double stat_osf(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Objective scoring function (area) 
double stat_osfb(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Objective scoring function (binary) 
double stat_osfi(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Objective scoring function (integrated) 
double stat_ksp(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Kolmogorov-Smirnov parameter 
double stat_rank(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Rank calculated with a combination of fb, fms, fa2, foex, ksp and nad. ranges from 0 to 7 (the higher the better) 
double stat_nbeq(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Nb of equal values 
double stat_nbgt(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Nb of values of MB greater than MA 
double stat_nblt(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Nb of values of MB smaller than MA 
double stat_nbfa(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Nb of false alarms (MA=0, MB!=0)
double stat_nbmi(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Nb of misses (MA!=0, MB=0) 
double stat_nbnp(Calc_Ctx *ctx, TDef *MA,TDef *MB);  // Nb of null pairs (MA=0, MB=0) 
double stat_aov(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // Overlap area 
double stat_afn(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // False negative area 
double stat_afp(Calc_Ctx *ctx, TDef *MA,TDef *MB);   // False positive area 
double stat_ax(Calc_Ctx *ctx, TDef *MA,TDef *MB);    // Area X 
double stat_ay(Calc_Ctx *ctx, TDef *MA,TDef *MB);    // Area Y 

double initrand(Calc_Ctx *ctx, TDef *M);             // Calls srand (initialize the seed for the rand function (frand here) to something else than 1)

double dmnp(Calc_Ctx *ctx, TDef *N);                                                       // Number of threads to use for DistanceMetrics calculations
double hausdorff(Calc_Ctx *ctx, TDef *A,TDef *B,TDef *Algo);                               // Hausdorff distance
double baddeley(Calc_Ctx *ctx, TDef *A,TDef *B,TDef *P);                                   // Baddeley's distance metric
double gdm(Calc_Ctx *ctx, TDef *A,TDef *B,TDef *P,TDef *Q,TDef *Algo,TDef *BS,TDef *C);    // Generalized Distance Metric

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
double min(Calc_Ctx *ctx, double a,double b);
double max(Calc_Ctx *ctx, double a,double b);
double clamp(Calc_Ctx *ctx, double a,double b,double c);
double within(Calc_Ctx *ctx, double a,double b,double c);
double ifelse(Calc_Ctx *ctx, double a,double b,double c);

double frand(Calc_Ctx *ctx, double a,double b,double c);

#endif
