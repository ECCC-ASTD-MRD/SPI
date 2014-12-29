/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de manipulation de type de donnees.
 * Fichier   : Data_Funcs.c
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

#include <math.h>

#include "Data_Funcs.h"
#include "Data_Calc.h"

/**
 * @internal Implementation detail
 * @ref func_init_table
 *
 * @author Jean-Philippe Gauthier
 * @brief Initialiation code's static function table
 * @see init_record
 */

double Vnb,Vsumx,Vminx,Vmaxx,Vavgx,Vsumy,Vminy,Vmaxy,Vavgy,Vvarx,Vvary,Vssx,Vssy,Vssxy,Vrmse,Vcorr,Vcovar,
       Vregb,Vrega,Verra,Verrb,Vssxy,Vmb,Vnmb,Vnme,Vme,Vmnb,Vmaxb,Vmaxe,Vmre,Vmaxre,Vmedx,Vmedy,
       Vmne,Vmfb,Vmfe,Vlmnb,Vlmne,Vnrmse,Vna,Vrna,Vnmse,Vgmb,Vgmv,Vfoex,Vfa2,Vfa5,Vfa10,Vfb,Vnad,
       Vfms,Vfmsi,Vfmsb,Vosf,Vosfb,Vosfi,Vksp,Vrank,Vnbeq,Vnbgt,Vnblt,Vnbfa,Vnbmi,Vnbnp,
       Vaov,Vafn,Vafp,Vax,Vay;

/*Matrix Derivative Functions*/
TFuncDef FuncD[] = {
  { "in"        , in        , 2 , TD_UByte },
  { "win"       , win       , 3 , TD_Int32 },
  { "lut"       , lut       , 3 , TD_Unknown },
  { "slut"      , slut      , 3 , TD_Unknown },
  { "fkernel"   , fkernel   , 2 , TD_Unknown },
  { "fcentile"  , fcentile  , 3 , TD_Unknown },
  { "fpeel"     , fpeel     , 1 , TD_Unknown },
  { "darea"     , darea     , 1 , TD_Float32 },
  { "dlat"      , dlat      , 1 , TD_Float32 },
  { "dlon"      , dlon      , 1 , TD_Float32 },
  { "ddx"       , ddx       , 1 , TD_Float32 },
  { "ddy"       , ddy       , 1 , TD_Float32 },
  { "dangle"    , dangle    , 1 , TD_Float32 },
  { "dslopedeg" , dslopedeg , 1 , TD_Float32 },
  { "dslope100" , dslope100 , 1 , TD_Float32 },
  { "daspect"   , daspect   , 1 , TD_Float32 },
  { "ddxfirst"  , ddxfirst  , 1 , TD_Float32 },
  { "ddyfirst"  , ddyfirst  , 1 , TD_Float32 },
  { "ddxsecond" , ddxsecond , 1 , TD_Float32 },
  { "ddysecond" , ddysecond , 1 , TD_Float32 },
  { "ddxysecond", ddxysecond, 1 , TD_Float32 },
  { "dprofcurve", dprofcurve, 1 , TD_Float32 },
  { "dtangcurve", dtangcurve, 1 , TD_Float32 },

  { "tcount"    , tcount    , 2 , TD_Int32 },
  { "flipy"     , flipy     , 1 , TD_Unknown },
  { NULL        , NULL      , 0 , TD_Unknown }
};

/*Matrix to Float functions*/
TFuncDef FuncF[] = {
  { "sall"  , stat_all    , 2 , TD_Float64 },
  { "snb"   , stat_nb     , 1 , TD_Float64 },
  { "smed"  , stat_med    , 1 , TD_Float64 },
  { "smedx" , stat_medx   , 2 , TD_Float64 },
  { "smedy" , stat_medy   , 2 , TD_Float64 },
  { "suniq" , stat_unique , 1 , TD_Int32 },
  { "ssum"  , stat_sum    , 1 , TD_Float64 },
  { "ssumx" , stat_sumx   , 2 , TD_Float64 },
  { "ssumy" , stat_sumy   , 2 , TD_Float64 },
  { "smin"  , stat_min    , 1 , TD_Float64 },
  { "sminx" , stat_minx   , 2 , TD_Float64 },
  { "sminy" , stat_miny   , 2 , TD_Float64 },
  { "smax"  , stat_max    , 1 , TD_Float64 },
  { "smaxx" , stat_maxx   , 2 , TD_Float64 },
  { "smaxy" , stat_maxy   , 2 , TD_Float64 },
  { "savg"  , stat_avg    , 1 , TD_Float64 },
  { "savgx" , stat_avgx   , 2 , TD_Float64 },
  { "savgy" , stat_avgy   , 2 , TD_Float64 },
  { "svar"  , stat_var    , 1 , TD_Float64 },
  { "svarx" , stat_varx   , 2 , TD_Float64 },
  { "svary" , stat_vary   , 2 , TD_Float64 },
  { "sssx"  , stat_ssx    , 2 , TD_Float64 },
  { "sssy"  , stat_ssy    , 2 , TD_Float64 },
  { "sssxy" , stat_ssxy   , 2 , TD_Float64 },
  { "srmse" , stat_rmse   , 2 , TD_Float64 },
  { "ssdev" , stat_sdev   , 1 , TD_Float64 },
  { "ssdevx", stat_sdevx  , 2 , TD_Float64 },
  { "ssdevy", stat_sdevy  , 2 , TD_Float64 },
  { "scor"  , stat_corr   , 2 , TD_Float64 },
  { "scov"  , stat_covar  , 2 , TD_Float64 },
  { "sregb" , stat_regb   , 2 , TD_Float64 },
  { "srega" , stat_rega   , 2 , TD_Float64 },
  { "serra" , stat_erra   , 2 , TD_Float64 },
  { "serrb" , stat_errb   , 2 , TD_Float64 },
  { "smb"   , stat_mb     , 2 , TD_Float64 },
  { "snmb"  , stat_nmb    , 2 , TD_Float64 },
  { "snme"  , stat_nme    , 2 , TD_Float64 },
  { "sme"   , stat_me     , 2 , TD_Float64 },
  { "smnb"  , stat_mnb    , 2 , TD_Float64 },
  { "smne"  , stat_mne    , 2 , TD_Float64 },
  { "smfb"  , stat_mfb    , 2 , TD_Float64 },
  { "smfe"  , stat_mfe    , 2 , TD_Float64 },
  { "slmnb" , stat_lmnb   , 2 , TD_Float64 },
  { "slmne" , stat_lmne   , 2 , TD_Float64 },
  { "smre"  , stat_mre    , 2 , TD_Float64 },
  { "smaxb" , stat_maxb   , 2 , TD_Float64 },
  { "smaxe" , stat_maxe   , 2 , TD_Float64 },
  { "smaxre", stat_maxre  , 2 , TD_Float64 },
  { "snrmse", stat_nrmse  , 2 , TD_Float64 },
  { "sna"   , stat_na     , 2 , TD_Float64 },
  { "srna"  , stat_rna    , 2 , TD_Float64 },
  { "snmse" , stat_nmse   , 2 , TD_Float64 },
  { "sgmb"  , stat_gmb    , 2 , TD_Float64 },
  { "sgmv"  , stat_gmv    , 2 , TD_Float64 },
  { "sfoex" , stat_foex   , 2 , TD_Float64 },
  { "sfa2"  , stat_fa2    , 2 , TD_Float64 },
  { "sfa5"  , stat_fa5    , 2 , TD_Float64 },
  { "sfa10" , stat_fa10   , 2 , TD_Float64 },
  { "sfb"   , stat_fb     , 2 , TD_Float64 },
  { "snad"  , stat_nad    , 2 , TD_Float64 },
  { "sfms"  , stat_fms    , 2 , TD_Float64 },
  { "sfmsb" , stat_fmsb   , 2 , TD_Float64 },
  { "sfmsi" , stat_fmsi   , 2 , TD_Float64 },
  { "sosf"  , stat_osf    , 2 , TD_Float64 },
  { "sosfb" , stat_osfb   , 2 , TD_Float64 },
  { "sosfi" , stat_osfi   , 2 , TD_Float64 },
  { "sksp"  , stat_ksp    , 2 , TD_Float64 },
  { "srank" , stat_rank   , 2 , TD_Float64 },
  { "snbeq" , stat_nbeq   , 2 , TD_Float64 },
  { "snbgt" , stat_nbgt   , 2 , TD_Float64 },
  { "snblt" , stat_nblt   , 2 , TD_Float64 },
  { "snbfa" , stat_nbfa   , 2 , TD_Float64 },
  { "snbmi" , stat_nbmi   , 2 , TD_Float64 },
  { "snbnp" , stat_nbnp   , 2 , TD_Float64 },
  { "saov"  , stat_aov    , 2 , TD_Float64 },
  { "safn"  , stat_afn    , 2 , TD_Float64 },
  { "safp"  , stat_afp    , 2 , TD_Float64 },
  { "sax"   , stat_ax     , 2 , TD_Float64 },
  { "say"   , stat_ay     , 2 , TD_Float64 },
  { NULL    , NULL        , 0 , TD_Unknown }
};

/*Matrix to Matrix functions*/
TFuncDef FuncM[] = {
  { "max"   , (TFunc*)max   , 2 , TD_Unknown },
  { "min"   , (TFunc*)min   , 2 , TD_Unknown },
  { "dif"   , (TFunc*)dif   , 2 , TD_Unknown },
  { "fmod"  , (TFunc*)fmod  , 2 , TD_Unknown },
  { "pow"   , (TFunc*)pow   , 2 , TD_Unknown },
  { "frand" , (TFunc*)frand , 3 , TD_Unknown },
  { "clamp" , (TFunc*)clamp , 3 , TD_Unknown },
  { "within", (TFunc*)within, 3 , TD_Unknown },
  { "?"     , (TFunc*)ifelse, 3 , TD_Unknown },
  { "ifelse", (TFunc*)ifelse, 3 , TD_Unknown },
  { "sin"   , (TFunc*)sin   , 1 , TD_Float32 },   /* Trigonometric functions */
  { "cos"   , (TFunc*)cos   , 1 , TD_Float32 },
  { "tan"   , (TFunc*)tan   , 1 , TD_Float32 },
  { "asin"  , (TFunc*)asin  , 1 , TD_Float32 },
  { "acos"  , (TFunc*)acos  , 1 , TD_Float32 },
  { "atan"  , (TFunc*)atan  , 1 , TD_Float32 },
  { "atan2" , (TFunc*)atan2 , 2 , TD_Float32 },
  { "sinh"  , (TFunc*)sinh  , 1 , TD_Float32 },   /* Hyperbolic functions */
  { "cosh"  , (TFunc*)cosh  , 1 , TD_Float32 },
  { "tanh"  , (TFunc*)tanh  , 1 , TD_Float32 },
  { "asinh" , (TFunc*)asinh , 1 , TD_Float32 },
  { "acosh" , (TFunc*)acosh , 1 , TD_Float32 },
  { "atanh" , (TFunc*)atanh , 1 , TD_Float32 },
  { "ln"    , (TFunc*)log   , 1 , TD_Float32 },   /* Logarithmic, exponentials and root functions */
  { "log"   , (TFunc*)log10 , 1 , TD_Float32 },
  { "exp"   , (TFunc*)exp   , 1 , TD_Float32 },
  { "sqrt"  , (TFunc*)sqrt  , 1 , TD_Float32 },
  { "cbrt"  , (TFunc*)cbrt  , 1 , TD_Float32 },
  { "abs"   , (TFunc*)fabs  , 1 , TD_Float32 }, /* Misc functions */
  { "ceil"  , (TFunc*)ceil  , 1 , TD_Unknown },
  { "floor" , (TFunc*)floor , 1 , TD_Unknown },
  { "round" , (TFunc*)rint  , 1 , TD_Unknown },
  { NULL    , (TFunc*)NULL  , 0 , TD_Unknown }
};

/**
 * @author Jean-Philippe Gauthier
 * @brief Finds a symbol in the function matrix symbol table
 * @param FuncName Name of the symbol to find
 * @return Pointer to symbol entry
 */
TFuncDef* FuncGet(TFuncDef *Funcs,char *Symbol) {

  int i=0;

  while (Funcs[i].Func) {
     if (strcmp(Funcs[i].Name,Symbol)==0)
        return(&Funcs[i]);
     i++;
  }

  return(NULL);
}

double flipy(TDataDef *Res,TDataDef *MA) {

   double        v;
   unsigned long i,j0,j1,idx0,idx1;

   v=0.0;

   /*Parse matrix*/
   for(j0=0,j1=MA->NJ-1;j0<MA->NJ>>1;j0++,j1--) {
      idx0=j0*MA->NI;
      idx1=j1*MA->NI;
      
      for(i=0;i<MA->NI;i++) {

         /*Increment result table at matrix value's index*/
         Def_Get(MA ,0,idx0+i,v);
         Def_Set(Res,0,idx1+i,v);
         Def_Get(MA ,0,idx1+i,v);
         Def_Set(Res,0,idx0+i,v);
      }
   }
   return(0.0);
}

double tcount(TDataDef *Res,TDataDef *Table,TDataDef *MB) {

   double        v,vb,va;
   unsigned long i,j,idx,idxi,k,nt;

   v=va=vb=0.0;

   /*Initialise to input table*/
   nt=FSIZE2D(Table);
   for(i=0;i<nt;i++) {
      Def_Get(Table,0,i,v);
      Def_Set(Res,0,i,v);
   }

   /*Parse matrix*/
   for(j=0;j<MB->NJ;j++) {
      idx=j*MB->NI;
      for(i=0;i<MB->NI;i++) {
         idxi=idx+i;

         /*Increment result table at matrix value's index*/
         Def_Get(MB,0,idxi,vb);
         if (vb!=MB->NoData) {
            k=vb;
            if (k>=0 && k<nt) {
               Def_Get(Res,0,k,va)
               va+=1.0;
               Def_Set(Res,0,k,va);
            }
         }
      }
   }
   return(0.0);
}

double fpeel(TDataDef *Res,TDataDef *MA) {

   double        v,va;
   unsigned long i,j,idx,idxi;
   char          t;

   v=va=0.0;

   /*Parse the matrix*/
   for(j=0;j<MA->NJ;j++) {
      idx=j*MA->NI;
      for(i=0;i<MA->NI;i++) {
         t=0;

         idxi=idx+i;
         Def_Get(MA,0,idxi,va);
         if (va!=0.0) {
            if (i>0) {
               Def_Get(MA,0,idxi-1,v);
               if (v==0.0) t++;
            }
            if (i<MA->NI-1) {
               Def_Get(MA,0,idxi+1,v);
               if (v==0.0) t++;
            }

            if (j>0) {
               idxi=idx-MA->NI+i;
               Def_Get(MA,0,idxi,v);
               if (v==0.0) t++;

               if (i>0) {
                  Def_Get(MA,0,idxi-1,v);
                  if (v==0.0) t++;
               }
               if (i<MA->NI-1) {
                  Def_Get(MA,0,idxi+1,v);
                  if (v==0.0) t++;
               }
            }

            if (j<MA->NJ-1) {
               idxi=idx+MA->NI+i;
               Def_Get(MA,0,idxi,v);
               if (v==0.0) t++;

               if (i>0) {
                  Def_Get(MA,0,idxi-1,v);
                  if (v==0.0) t++;
               }
               if (i<MA->NI-1) {
                  Def_Get(MA,0,idxi+1,v);
                  if (v==0.0) t++;
               }
            }

            /*Test for survival or death*/
            idxi=idx+i;
            if (t) {
               Def_Set(Res,0,idxi,0.0);
            } else {
               Def_Set(Res,0,idxi,va);
            }
         } else {
            Def_Set(Res,0,idxi,va);
         }
      }
   }
   return(0.0);
}

double fkernel(TDataDef *Res,TDataDef *MA,TDataDef *MB) {

   double        va,vb,s,w,dw;
   unsigned long i,j,fi,fj,idx,idxf;
   int           dmi,dmj,dj,di;
   float        *vs;

   va=vb=0.0;

   dmi=MB->NI>>1;
   dmj=MB->NJ>>1;
   w=0.0;
   for(i=0;i<FSIZE2D(MB);i++) {
      Def_Get(MB,0,i,vb);
      w+=vb;
   }

   /*Copy the matrix to a type we know, this will speedup the rest of the process*/
   if ((vs=(float*)malloc(FSIZE2D(MA)*sizeof(float)))) {
      for(j=0;j<FSIZE2D(MA);j++) {
         Def_Get(MA,0,j,vs[j]);
      }

      /*Parse the matrix*/
      for(j=0;j<MA->NJ;j++) {
         for(i=0;i<MA->NI;i++) {

            /*Apply filter*/
            dw=s=0.0;

            for(fj=0,dj=j-dmj;fj<MB->NJ;fj++,dj++) {
               idxf=fj*MB->NI;
               idx=dj*MA->NI;
               for(fi=0,di=i-dmi;fi<MB->NI;fi++,di++) {
                  Def_Get(MB,0,idxf+fi,vb);
                  if (di>=0 && di<MA->NI && dj>=0 && dj<MA->NJ) {
                     va=vs[idx+di];

                     /*Check for nodata value*/
                     if (va==MA->NoData) {
                       dw+=vb;
                     } else {
                       s+=vb*va;
                     }
                  } else {
                    dw+=vb;
                  }
               }
            }
            if ((w-dw)!=0) s/=(w-dw);
            Def_Set(Res,0,j*MA->NI+i,s);
         }
      }
      free(vs);
   }
   return(0.0);
}

double fcentile(TDataDef *Res,TDataDef *MA,TDataDef *MB,TDataDef *MC) {

   double        *vs,va,vb,vc;
   unsigned long i,j,fi,fj,idx;
   int           dm,dj,di,s;

   va=vb=vc=0.0;

   Def_Get(MB,0,0,vb);
   Def_Get(MC,0,0,vc);

   if (fmod(vb,2)==0.0)
      vb+=1.0;

   if ((vs=(double*)malloc(vb*vb*sizeof(double)))) {
      dm=vb/2;
      vc=vc<0?0:vc>1.0?1.0:vc;

      /*Parse the matrix*/
      for(j=0;j<MA->NJ;j++) {
         for(i=0;i<MA->NI;i++) {

            /*Apply filter*/
            s=0;

            for(fj=0,dj=j-dm;fj<vb;fj++,dj++) {
               idx=dj*MA->NI;
               for(fi=0,di=i-dm;fi<vb;fi++,di++) {
                  if (di>=0 && di<MA->NI && dj>=0 && dj<MA->NJ) {
                     Def_Get(MA,0,idx+di,va);

                     /*Check for nodata value*/
                     if (va!=MA->NoData) {
                       vs[s++]=va;
                     }
                  }
               }
            }

            if (s==0) {
               Def_Set(Res,0,j*MA->NI+i,MA->NoData);
            } else {
               Def_Set(Res,0,j*MA->NI+i,HCentile(vs,s,s*vc-1));
            }
         }
      }
      free(vs);
   }
   return(0.0);
}

double in(TDataDef *Res,TDataDef *MA,TDataDef *MB) {

   double        va,vb,vr;
   unsigned long i,n;

   va=vb=0.0;

   for(i=0;i<FSIZE2D(MA);i++) {
      Def_Get(MA,0,i,va);
      vr=0.0;
      for(n=0;n<FSIZE2D(MB);n++) {
         Def_Get(MB,0,n,vb);
         if (va==vb) {
            vr=1.0;
            break;
         }
      }
      Def_Set(Res,0,i,vr);
   }
   return(0.0);
}

double win(TDataDef *Res,TDataDef *MA,TDataDef *MB,TDataDef *MC) {

   double        va,vb,vc;
   unsigned long i,j,n;

   va=vb=vc=0.0;
   
   for(i=0;i<FSIZE2D(MA);i++) {
      Def_Get(MA,0,i,va);
      Def_Get(MB,0,i,vb);
      n=0.0;
      for(j=0;j<FSIZE2D(MC);j++) {
         Def_Get(MC,0,j,vc);
         if (vc>=va && vc<vb) {
            n++;
         }
      }
      Def_Set(Res,0,i,n);
   }
   return(0.0);
}

double slut(TDataDef *Res,TDataDef *MA,TDataDef *MB,TDataDef *MC) {

   double va,vb,vc;
   long   i,i0,i1,n,m;

   va=vb=vc=0.0;
   i=FSIZE2D(MB);
   n=FSIZE2D(MC);
   m=i<n?i:n;

   for(i=0;i<FSIZE2D(MA);i++) {
      Def_Get(MA,0,i,va);

      i0=0;i1=m-1;
      while(i0<i1) {
         n=(i0+i1)>>1;
         Def_Get(MB,0,n,vb);
         if (va<vb) {
            i1=n-1;
         } else if (va>vb) {
            i0=n+1;
         } else {
            Def_Get(MC,0,n,vc);
            Def_Set(Res,0,i,vc);
            break;
         }
      }
   }
   return(0.0);
}

double lut(TDataDef *Res,TDataDef *MA,TDataDef *MB,TDataDef *MC) {

   double        va,vb,vc;
   unsigned long i,n,m;

   va=vb=vc=0.0;
   i=FSIZE2D(MB);
   n=FSIZE2D(MC);
   m=i<n?i:n;

   for(i=0;i<FSIZE2D(MA);i++) {
      Def_Get(MA,0,i,va);
      for(n=0;n<m;n++) {
         Def_Get(MB,0,n,vb);
         if (va==vb) {
            Def_Get(MC,0,n,vc);
            Def_Set(Res,0,i,vc);
            break;
         }
      }
   }
   return(0.0);
}

double dslopedeg(TDataDef *Res,TDataDef *Def) {

   return(dcore(Res,Def,DSLOPEDEG));
}
double dslope100(TDataDef *Res,TDataDef *Def) {

   return(dcore(Res,Def,DSLOPE100));
}
double daspect(TDataDef *Res,TDataDef *Def) {

   return(dcore(Res,Def,DASPECT));
}
double ddxfirst(TDataDef *Res,TDataDef *Def) {

   return(dcore(Res,Def,DDX));
}
double ddyfirst(TDataDef *Res,TDataDef *Def) {

   return(dcore(Res,Def,DDY));
}
double ddxsecond(TDataDef *Res,TDataDef *Def) {

   return(dcore(Res,Def,DDXX));
}
double ddysecond(TDataDef *Res,TDataDef *Def) {

   return(dcore(Res,Def,DDYY));
}
double ddxysecond(TDataDef *Res,TDataDef *Def) {

   return(dcore(Res,Def,DDXY));
}
double dprofcurve(TDataDef *Res,TDataDef *Def) {

   return(dcore(Res,Def,DPCURVE));
}
double dtangcurve(TDataDef *Res,TDataDef *Def) {

   return(dcore(Res,Def,DTCURVE));
}

double darea(TDataDef *Res,TDataDef *Def,int Mode) {

   unsigned long i,j,idx;
   double        mx,my;
   TGeoRef      *ref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: ref=GBand->Ref; break;
      case T_FLD : ref=GField->Ref; break;
   }

   if (!ref) {
      return(0.0);
   }

   for(j=0;j<Def->NJ;j++) {
      idx=j*Def->NI;
      for(i=0;i<Def->NI;i++) {
         mx=ref->Distance(ref,i-0.5,j,i+0.5,j);
         my=ref->Distance(ref,i,j-0.5,i,j+0.5);
         Def_Set(Res,0,idx+i,mx*my);
      }
   }
   return(1.0);
}

double dangle(TDataDef *Res,TDataDef *Def,int Mode) {

   unsigned long i,j,idx;
   double        d,lat[2],lon[2];
   TGeoRef      *ref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: ref=GBand->Ref; break;
      case T_FLD : ref=GField->Ref; break;
   }

   if (!ref) {
      return(0.0);
   }

   for(j=0;j<Def->NJ;j++) {
      idx=j*Def->NI;
      for(i=0;i<Def->NI;i++) {
         ref->Project(ref,i,j-0.5,&lat[0],&lon[0],1,1);
         ref->Project(ref,i,j+0.5,&lat[1],&lon[1],1,1);
         lat[0]=DEG2RAD(lat[0]); lon[0]=DEG2RAD(lon[0]);
         lat[1]=DEG2RAD(lat[1]); lon[1]=DEG2RAD(lon[1]);

         d=COURSE(lat[0],lon[0],lat[1],lon[1]);
         d=RAD2DEG(d);
         Def_Set(Res,0,idx+i,d);
      }
   }
   return(1.0);
}

double dlat(TDataDef *Res,TDataDef *Def,int Mode) {

   unsigned long i,j,idx;
   double        lat,lon;
   TGeoRef      *ref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: ref=GBand->Ref; break;
      case T_FLD : ref=GField->Ref; break;
   }

   if (!ref) {
      return(0.0);
   }

   for(j=0;j<Def->NJ;j++) {
      idx=j*Def->NI;
      for(i=0;i<Def->NI;i++) {
         ref->Project(ref,i,j,&lat,&lon,0,1);
         Def_Set(Res,0,idx+i,lat);
      }
   }
   return(1.0);
}

double dlon(TDataDef *Res,TDataDef *Def,int Mode) {

   unsigned long i,j,idx;
   double        lat,lon;
   TGeoRef      *ref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: ref=GBand->Ref; break;
      case T_FLD : ref=GField->Ref; break;
   }

   if (!ref) {
      return(0.0);
   }

   for(j=0;j<Def->NJ;j++) {
      idx=j*Def->NI;
      for(i=0;i<Def->NI;i++) {
         ref->Project(ref,i,j,&lat,&lon,0,1);
         Def_Set(Res,0,idx+i,lon);
      }
   }
   return(1.0);
}

double ddx(TDataDef *Res,TDataDef *Def,int Mode) {

   unsigned long i,j,idx;
   double        d;
   TGeoRef      *ref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: ref=GBand->Ref; break;
      case T_FLD : ref=GField->Ref; break;
   }

   if (!ref) {
      return(0.0);
   }

   for(j=0;j<Def->NJ;j++) {
      idx=j*Def->NI;
      for(i=0;i<Def->NI;i++) {
         d=ref->Distance(ref,i-0.5,j,i+0.5,j);
         Def_Set(Res,0,idx+i,d);
      }
   }
   return(1.0);
}

double ddy(TDataDef *Res,TDataDef *Def,int Mode) {

   unsigned long i,j,idx;
   double        d;
   TGeoRef      *ref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: ref=GBand->Ref; break;
      case T_FLD : ref=GField->Ref; break;
   }

   if (!ref) {
      return(0.0);
   }

   for(j=0;j<Def->NJ;j++) {
      idx=j*Def->NI;
      for(i=0;i<Def->NI;i++) {
         d=ref->Distance(ref,i,j-0.5,i,j+0.5);
         Def_Set(Res,0,idx+i,d);
      }
   }
   return(1.0);
}

/* Derived form GRASS */
double dcore(TDataDef *Res,TDataDef *Def,int Mode) {

   unsigned long i,j,idx,d;
   double  b[9]={ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
   double  mx,my,dx,dy,dxy,dx2,dy2,dxy2,slp100,slpdeg,asp,s3,s4,s5,s6,dvx,dvy,dvxy,dvxy2,norm,pcurv,tcurv;
   TGeoRef *ref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: ref=GBand->Ref; break;
      case T_FLD : ref=GField->Ref; break;
   }

   if (!ref) {
      return(0.0);
   }

   /*check for wrap around*/
   d=ref->Type&GRID_WRAP?0:1;

   for(j=1;j<Def->NJ-1;j++) {
      for(i=d;i<Def->NI-d;i++) {
         idx=j*Def->NI+i;

         /*Get the data bloc
           b0  b1  b2
           b3  b4  b5
           b6  b7  b8
        */
         if (i!=0 && i!=1) {
            b[0]=b[1];b[1]=b[2];
            b[3]=b[4];b[4]=b[5];
            b[6]=b[7];b[7]=b[8];
         }
         if (i==0 || i==1) {
            Def_Get(Def,0,idx-Def->NI,b[1]);
            Def_Get(Def,0,idx,b[4]);
            Def_Get(Def,0,idx+Def->NI,b[7]);
         }

         if (i==0) {
            Def_Get(Def,0,idx-1,b[0]);
            Def_Get(Def,0,idx-1+Def->NI,b[3]);
            Def_Get(Def,0,idx-1+Def->NI+Def->NI,b[6]);
         }
         if (i==1) {
            Def_Get(Def,0,idx-1-Def->NI,b[0]);
            Def_Get(Def,0,idx-1,b[3]);
            Def_Get(Def,0,idx-1+Def->NI,b[6]);
         }
         if (i==Def->NI-1) {
            Def_Get(Def,0,idx-2-Def->NI-Def->NI,b[2]);
            Def_Get(Def,0,idx-2-Def->NI,b[5]);
            Def_Get(Def,0,idx-2,b[8]);
         } else {
            Def_Get(Def,0,idx+1-Def->NI,b[2]);
            Def_Get(Def,0,idx+1,b[5]);
            Def_Get(Def,0,idx+1+Def->NI,b[8]);
         }

         /*Distances*/
         mx=ref->Distance(ref,i-1,j,i+1,j)*4;
         my=ref->Distance(ref,i,j-1,i,j+1)*4;

         /*Slope*/
         dx=((b[0]+b[3]+b[3]+b[6])-(b[2]+b[5]+b[5]+b[8]))/mx;
         if (Mode==DDX) {
            Def_Set(Res,0,idx,dx);
            continue;
         }
         dy=((b[6]+b[7]+b[7]+b[8])-(b[0]+b[1]+b[1]+b[2]))/my;
         if (Mode==DDY) {
            Def_Set(Res,0,idx,dy);
            continue;
         }
         dx2=dx*dx;
         dy2=dy*dy;
         dxy2=dx2+dy2;
         dxy=sqrt(dxy2);
         slpdeg=RAD2DEG(atan(dxy));

         if (Mode==DSLOPE100) {
// Grass method = bad   slp100=100.0*dxy;
            slp100=slpdeg/0.9;
            Def_Set(Res,0,idx,slp100);
            continue;
         }

         if (Mode==DSLOPEDEG) {
            Def_Set(Res,0,idx,slpdeg);
            continue;
         }

         /*Aspect*/
         if (Mode==DASPECT) {
            asp=(dxy2==0.0?0.0:(dx==0.0?(dy>0?0.0:180.0):RAD2DEG(atan2(dy,dx))+90.0));
            asp=asp<0.0?asp+360.0:asp;
            Def_Set(Res,0,idx,asp);
            continue;
         }

         /* Derivatives*/
         s4=b[0]+b[2]+b[8]-b[4]*8.0;
         s5=(b[3]+b[5])*4.0-(b[7]+b[1])*2.0;
         s6=(b[7]+b[1])*4.0-(b[3]+b[5])*2.0;
         s3=b[6]-b[8]+b[2]-b[0];

         dvx=-(s4+s5)/(0.09375*mx*mx);    /*0.09375=3/32*/
         dvy=-(s4+s6)/(0.09375*my*my);
         dvxy=-s3/(0.0625*mx*my);         /*0.0625=1/16*/
         if (Mode==DDXX) {
            Def_Set(Res,0,idx,dvx);
            continue;
         }
         if (Mode==DDYY) {
            Def_Set(Res,0,idx,dvy);
            continue;
         }
         if (Mode==DDXY) {
            Def_Set(Res,0,idx,dvxy);
            continue;
         }

         /*Curvature*/
         norm=sqrt(dxy2+1.0);
         dvxy2=2.0*dvxy*dx*dy;
         if (Mode==DPCURVE) {
            pcurv=dxy2==0?0:(dvx*dx2+dvxy2+dvy*dy2)/(dxy2*norm*norm*norm);
            Def_Set(Res,0,idx,pcurv);
            continue;
         }
         if (Mode==DTCURVE) {
            tcurv=dxy2==0?0:(dvx*dy2-dvxy2*dvy*dx2)/(dxy2*norm);
            Def_Set(Res,0,idx,tcurv);
            continue;
         }
      }
   }
   return(0.0);
}

// For the given value, return the associated probability in the Empirically-calculated Cumulative Distribution Function
inline double ecdf(double val, double* vals, unsigned int* cums, unsigned long n) {
   unsigned long i, min=0, max=n;

   // Limit cases

   if (n<=0 || val<vals[0]) return 0.0;
   if (val>=vals[n-1]) return 1.0;

   // Normal binary search

   while (min<=max) {
      i = (max+min)/2;

      if (vals[i]==val) {
         return (double)cums[i]/(double)cums[n-1];
      }

      if (val>vals[i]) {
         min = i + 1;
      } else {
         max = i - 1;
      }
   }

   // If we get to this point, the exact number doesn't exist and we need to round to the closest inferior point

   return (double)cums[max]/(double)cums[n-1];
}

void stat_core(TDataDef *MA,TDataDef *MB) {

   double va,vb,t,nblok=0;
   double ratio,sum,dif,adif;
   unsigned long i,n,nx=0,ny=0,d=0,gi,gj;
   double *vx,*vy;
   unsigned int *cx,*cy;
   TGeoRef *ref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   Vcorr=Vnb=Vsumx=Vsumy=Vavgx=Vavgy=Vssx=Vssy=Vssxy=Vrmse=Vmb=Vnmb=Vnme=Vvarx=Vvary=Vcovar=Vme=Vmnb=Vmne=Vmfb=Vmfe=Vlmnb=Vlmne=Vnrmse=Vna=Vrna=Vmre=va=vb=0.0;
   Vnmse=Vgmb=Vgmv=Vfoex=Vfa2=Vfa5=Vfa10=Vfb=Vnad=Vfms=Vfmsb=Vfmsi=Vosf=Vosfb=Vosfi=Vksp=Vrank=Vnbeq=Vnbgt=Vnblt=Vnbfa=Vnbmi=Vnbnp=Vaov=Vafn=Vafp=Vax=Vay=0.0;
   Vminy=Vminx=HUGE_VAL;
   Vmaxy=Vmaxx=-HUGE_VAL;
   Vmaxe=Vmaxb=Vmaxre=-HUGE_VAL;
   Vmedx=Vmedy=nan("NaN");

   if ((n=FSIZE3D(MA))==0) {
      return;
   }

   switch (GMode) {
      case T_BAND: ref=GBand->Ref; break;
      case T_FLD : ref=GField->Ref; break;
   }

   // Init fields that will be used later on for ksp

   if (MA && MB) {
      vx = malloc(n*sizeof(double));
      vy = malloc(n*sizeof(double));
      cx = malloc(n*sizeof(unsigned int));
      cy = malloc(n*sizeof(unsigned int));

      if (!vx || !vy || !cx || !cy) {
         fprintf(stderr,  "[stat_core] : Could not allocate memory\n");
         if( vx ) free(vx);
         if( vy ) free(vy);
         if( cx ) free(cx);
         if( cy ) free(cy);
         return;
      }
   } else {
      vx=vy=NULL;
      cx=cy=NULL;
   }

   for(i=0;i<n;i++) {
      if (MA) {
         Def_Get(MA,0,i,va);
         if (isnan(va) || va==MA->NoData) {
            Vna++;
            continue;
         }
      }

      if (MB) {
         Def_Get(MB,0,i,vb);
         if (isnan(vb) || vb==MB->NoData) {
            Vna++;
            continue;
         }
      }

      Vminx=va<Vminx?va:Vminx;
      Vmaxx=va>Vmaxx?va:Vmaxx;

      Vsumx+=va;
      Vssx+=va*va;

      if (MB) {
         sum=vb+va;
         dif=vb-va;
         adif=fabs(dif);

         Vminy=vb<Vminy?vb:Vminy;
         Vmaxy=vb>Vmaxy?vb:Vmaxy;
         Vsumy+=vb;
         Vssy+=vb*vb;
         Vssxy+=va*vb;
         Vme+=adif;
         Vmaxb=dif>Vmaxb?dif:Vmaxb;
         Vmaxe=adif>Vmaxe?adif:Vmaxe;
         Vosfi+=va<=vb?va:vb;

         vx[d]=va;
         vy[d]=vb;
         ++d;

         if (va==vb) Vnbeq++;
         else if (vb>va) Vnbgt++;
         else Vnblt++;

         if (ref && (va!=0.0 || vb!=0.0)) {
            gi=i%MA->NI;
            gj=i/MA->NI;
            t=ref->Distance(ref,gi-0.5,gj,gi+0.5,gj) * ref->Distance(ref,gi,gj-0.5,gi,gj+0.5);
            Vfms+=t;

            if (va!=0 && vb!=0) {
                Vaov+=t;
                Vax+=t;
                Vay+=t;
            } else if (va!=0) {
                Vax+=t;
            } else {
                Vay+=t;
            }
         }

         if (va!=0.0f) {
            t=dif/va;
            Vmnb+=t;
            Vmne+=fabs(t);

            ratio=vb/va;
            if (ratio>0.0f) {
               ++nblok;
               t=log(ratio);

               Vlmnb+=t;
               Vlmne+=fabs(t);

               Vgmb+=t;
               Vgmv+=t*t;
            }

            if (0.5<=ratio && ratio<=2.0) {
               ++Vfa2; ++Vfa5; ++Vfa10;
            } else if (0.2<=ratio && ratio<=5.0) {
               ++Vfa5; ++Vfa10;
            } else if (0.1<=ratio && ratio<=10.0)
               ++Vfa10;

            t=fabs(1.0-ratio);
            Vmre+=t;
            Vmaxre=t>Vmaxre?t:Vmaxre;

            if (vb==0.0f) {
               ++Vnbmi;
            }
         } else {
            if (vb==0.0f) {
               ++Vnbnp;
            } else {
               ++Vnbfa;
            }
         }

         if (sum!=0.0f) {
            Vmfb+=dif/sum;
            Vmfe+=adif/sum;
         }
      }
      Vnb++;
   }

   Vrna=Vnb/n;

   if (Vnb==0) {
      Vcorr=Vsumx=Vsumy=Vavgx=Vavgy=Vssx=Vssy=Vssxy=Vrmse=Vmb=Vnmb=Vnme=Vvarx=Vvary=Vcovar=Vme=Vmnb=Vmne=Vmfb=Vmfe=Vlmnb=Vlmne=Vnrmse=0.0;
      Vnmse=Vgmb=Vgmv=Vfoex=Vfa2=Vfa5=Vfa10=Vfb=Vnad=Vfms=Vfmsb=Vfmsi=Vosf=Vosfb=Vosfi=Vksp=Vrank=Vnbeq=Vnbgt=Vnblt=Vnbfa=Vnbmi=Vnbnp=Vaov=Vafn=Vafp=Vax=Vay=0.0;

      if( vx ) free(vx);
      if( vy ) free(vy);
      if( cx ) free(cx);
      if( cy ) free(cy);

      return;
   }

   if (MA && MB) {
      // Calculate the Empirical Cumulative Distribution Function for x and y

      qsort(vx, d, sizeof(double), QSort_Double);
      qsort(vy, d, sizeof(double), QSort_Double);

      // We might as well use the sorted values to calculate the median
      i=d>>1;
      Vmedx=(d&1)?vx[i]:(vx[i-1]+vx[i])*0.5;
      Vmedy=(d&1)?vy[i]:(vy[i-1]+vy[i])*0.5;

      // Eliminate duplicate values, but keep the number of occurrences in the c[xy] array
      for(nx=0,ny=0,i=1;i<d;++i) {
         if (vx[i]!=vx[nx]) {
            cx[nx]=i;
            if (++nx<i) {
               vx[nx]=vx[i];
            }
         }

         if (vy[i]!=vy[ny]) {
            cy[ny]=i;
            if (++ny<i) {
               vy[ny]=vy[i];
            }
         }
      }
      cx[nx++]=cy[ny++]=d;

      // Calculate KSP = Max|D(Xk) - D(Yk)|

      for(i=0;i<nx;++i)
         Vksp = fmax(Vksp, fabs((double)cx[i]/(double)cx[nx-1] - ecdf(vx[i],vy,cy,ny)));
      for(i=0;i<ny;++i)
         Vksp = fmax(Vksp, fabs(ecdf(vy[i],vx,cx,nx) - (double)cy[i]/(double)cy[ny-1]));
   }

   Vafn  = Vax-Vaov;
   Vafp  = Vay-Vaov;

   Vmb   = Vsumy-Vsumx;
   Vnmb  = Vsumx!=0?Vmb/Vsumx*100:0;
   Vmb   /= Vnb;

   Vnad  = (Vsumx+Vsumy==0)?0:Vme/(Vsumx+Vsumy);
   Vnme  = Vsumx!=0?Vme/Vsumx*100:0;
   Vme   /= Vnb;

   Vavgx = Vsumx/Vnb;
   Vavgy = Vsumy/Vnb;

   Vvarx    = Vssx/Vnb-Vavgx*Vavgx;
   Vvary    = Vssy/Vnb-Vavgy*Vavgy;
   Vcovar   = Vssxy/Vnb-Vavgx*Vavgy;

   Vcorr=(Vvarx==0 || Vvary==0)?0:Vcovar/(sqrt(Vvarx*Vvary));

   Vrmse=(Vssy-Vssxy*2+Vssx)/Vnb;
   Vnmse=Vrmse/(Vavgx*Vavgy);
   Vrmse=sqrt(Vrmse);
   Vnrmse=Vrmse/Vavgx;

   Vregb=Vcovar/Vvarx;
   Vrega=Vavgy-Vregb*Vavgx;
   Verrb=sqrt((Vnb*Vssy-Vsumy*Vsumy-Vregb*Vregb*(Vnb*Vssx-Vsumx*Vsumx))/((Vnb-2.0)*(Vnb*Vssx-Vsumx*Vsumx)));
   Verra=Verrb*sqrt(Vssx/Vnb);

   Vmre/=Vnb;
   Vmne/=Vnb;
   Vmnb/=Vnb;
   Vmfb=2.0*Vmfb/Vnb*100;
   Vmfe=2.0*Vmfe/Vnb*100;
   Vlmnb=exp(Vlmnb/Vnb)-1;
   Vlmne=exp(Vlmne/Vnb)-1;

   t=nblok+Vnbnp;
   Vgmb=exp(Vgmb/t);
   Vgmv=exp(Vgmv/t);

   Vfoex=((Vnbgt+Vnbeq/2.0)/Vnb-0.5)*100.0;
   Vfa2=((Vfa2+Vnbnp)/Vnb)*100.0;
   Vfa5=((Vfa5+Vnbnp)/Vnb)*100.0;
   Vfa10=((Vfa10+Vnbnp)/Vnb)*100.0;
   Vfb=(Vavgy==0 || Vavgx==0)?0:2.0*(Vavgy-Vavgx)/(Vavgy+Vavgx);
   Vfms=Vfms!=0?Vaov/Vfms*100.0:0;
   Vfmsb=(Vnb-Vnbmi-Vnbfa-Vnbnp)/(Vnb-Vnbnp)*100.0;
   Vfmsi=(1.0-Vnad)/(1.0+Vnad)*100.0;
   Vosf=(Vax==0 || Vay==0)?0:sqrt(pow(Vafn/Vax,2)+pow(Vafp/Vay,2));
   Vosfb=((Vnb-Vnbnp-Vnbfa)==0 || (Vnb-Vnbnp-Vnbmi)==0)?0:sqrt(pow(Vnbmi/(Vnb-Vnbnp-Vnbfa),2)+pow(Vnbfa/(Vnb-Vnbnp-Vnbmi),2));
   Vosfi=(Vsumx==0 || Vsumy==0)?0:sqrt(pow(1-Vosfi/Vsumx,2.0)+pow(1-Vosfi/Vsumy,2.0));
   Vksp*=100.0;
   //Vrank=Vcorr*Vcorr+(1-fabs(Vfb/2.0))+(Vfms/100.0)+(Vfa2/100.0)+(1-fabs(Vfoex/50.0))+(1-Vksp/100.0)+(1-Vnad);
   Vrank=Vcorr*Vcorr+(1-fabs(Vfb/2.0))+(Vfms/100.0)+(Vfa2/100.0)+(1-Vksp/100.0)+(1-Vnad);

   if( vx ) free(vx);
   if( vy ) free(vy);
   if( cx ) free(cx);
   if( cy ) free(cy);
}

double stat_all(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vnb);
}

double stat_nmse(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnmse);
}

double stat_gmb(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vgmb);
}

double stat_gmv(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vgmv);
}

double stat_foex(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfoex);
}

double stat_fa2(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfa2);
}

double stat_fa5(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfa5);
}

double stat_fa10(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfa10);
}

double stat_fb(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfb);
}

double stat_nad(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnad);
}

double stat_fms(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfms);
}

double stat_fmsb(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfmsb);
}

double stat_fmsi(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfmsi);
}

double stat_osf(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vosf);
}

double stat_osfb(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vosfb);
}

double stat_osfi(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vosfi);
}

double stat_ksp(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vksp);
}

double stat_rank(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vrank);
}

double stat_nbeq(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnbeq);
}

double stat_nbgt(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnbgt);
}

double stat_nblt(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnblt);
}

double stat_nbfa(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnbfa);
}

double stat_nbmi(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnbmi);
}

double stat_nbnp(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnbnp);
}

double stat_aov(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vaov);
}

double stat_afn(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vafn);
}

double stat_afp(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vafp);
}

double stat_ax(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vax);
}

double stat_ay(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vay);
}

double stat_na(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vna);
}

double stat_rna(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vrna);
}

double stat_mb(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmb);
}

double stat_me(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vme);
}

double stat_nmb(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnmb);
}

double stat_nme(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnme);
}

double stat_mnb(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmnb);
}

double stat_mne(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmne);
}

double stat_lmne(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vlmne);
}

double stat_lmnb(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vlmnb);
}

double stat_mfb(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmfb);
}

double stat_mfe(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmfe);
}

double stat_mre(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmre);
}

double stat_maxb(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmaxb);
}

double stat_maxe(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmaxe);
}

double stat_maxre(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmaxre);
}

double stat_rmse(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vrmse);
}

double stat_nrmse(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnrmse);
}

double stat_corr(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vcorr);
}

double stat_covar(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vcovar);
}

double stat_varx(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vvarx);
}

double stat_vary(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vvary);
}

double stat_sumx(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vsumx);
}

double stat_sumy(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vsumy);
}

double stat_avgx(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vavgx);
}

double stat_avgy(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vavgy);
}

double stat_minx(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vminx);
}

double stat_miny(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vminy);
}

double stat_maxx(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vmaxx);
}

double stat_maxy(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmaxy);
}

double stat_regb(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vregb);
}

double stat_rega(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vrega);
}

double stat_erra(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Verra);
}

double stat_errb(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Verrb);
}

double stat_ssx(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vssx);
}

double stat_ssy(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vssy);
}

double stat_ssxy(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vssxy);
}

double stat_sdevx(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);

   return(sqrt(Vvarx));
}

double stat_sdevy(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);

   return(sqrt(Vvary));
}

double stat_medx(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmedx);
}

double stat_medy(TDataDef *MA,TDataDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmedy);
}

double stat_nb(TDataDef *MA) {
   if (MA)
      stat_core(MA,NULL);
   return(Vnb);
}

double stat_sdev(TDataDef *MA) {
   if (MA)
      stat_core(MA,NULL);

   return(sqrt(Vvarx));
}

double stat_var(TDataDef *MA) {
   if (MA)
      stat_core(MA,NULL);
   return(Vvarx);
}

double stat_med(TDataDef *M) {

   int     n,nb,t=0;
   double *v,med=0;

   nb=FSIZE3D(M);
   if ((v=(double*)malloc(nb*sizeof(double)))) {
      for(n=0;n<nb;n++) {
         Def_Get(M,0,n,v[t]);
         if( !isnan(v[t]) && v[t]!=M->NoData )
            ++t;
      }

      qsort(v,t,sizeof(double),QSort_Double);

      n=t>>1;
      med=(t&1)?v[n]:(v[n-1]+v[n])*0.5;

      free(v);
   }

   return(med);
}

double stat_unique(TDataDef *M) {

   int     n,nb,uniq=0,t=0;
   double *v;

   nb=FSIZE3D(M);
   if ((v=(double*)malloc(nb*sizeof(double)))) {
      for(n=0;n<nb;n++) {
         Def_Get(M,0,n,v[t]);
         if( !isnan(v[t]) && v[t]!=M->NoData )
            ++t;
      }

      if( t>0 ) {
         qsort(v,t,sizeof(double),QSort_Double);
         
         for(uniq=1,n=1;n<t;++n) {
            if( v[n-1]!=v[n] )
               ++uniq;
         }
      }

      free(v);
   }

   return(uniq);
}

double stat_sum(TDataDef *M) {

   double        sum=0,v=0;
   unsigned long i;

   for(i=0;i<FSIZE3D(M);i++) {
      Def_Get(M,0,i,v);
      if( !isnan(v) && v!=M->NoData )
         sum+=v;
   }
   return sum;
}

double stat_min(TDataDef *M) {

   double        min=HUGE_VAL,v=0;
   unsigned long i;

   for(i=0;i<FSIZE3D(M);i++) {
      Def_Get(M,0,i,v);
      if( !isnan(v) && v!=M->NoData && v<min )
         min=v;
   }
   return min;
}

double stat_max(TDataDef *M) {

   double        max=-HUGE_VAL,v=0;
   unsigned long i;

   for(i=0;i<FSIZE3D(M);i++) {
      Def_Get(M,0,i,v);
      if( !isnan(v) && v!=M->NoData && v>max )
         max=v;
   }
   return max;
}

double stat_avg(TDataDef *M) {

   double        sum=0,v=0;
   unsigned long i,n=0;

   for(i=0;i<FSIZE3D(M);i++) {
      Def_Get(M,0,i,v);
      if( !isnan(v) && v!=M->NoData ) {
         sum+=v;
         ++n;
      }
   }
   return sum/n;
}

double add(double a,double b) {
   return a + b;
}

double sub(double a,double b) {
   return a - b;
}

double mul(double a,double b) {
   return a * b;
}

double dvd(double a,double b) {
   return a / b;
}

double neg(double a) {
   return -a;
}

double dif(double a,double b) {
   if (a!=0.0) {
      return (fabs((a-b)*100.0/(a<b?b:a)));
   } else if (b==0.0){
      return(0.0);
   } else {
      return (100.0);
   }
}

double equ(double a,double b) {
   return(a==b);
}

double neq(double a,double b) {
   return(a!=b);
}

double grq(double a,double b) {
   return(a>=b);
}

double gre(double a,double b) {
   return(a>b);
}

double sma(double a,double b) {
   return(a<b);
}

double smq(double a,double b) {
   return(a<=b);
}

double not(double a) {
   return(a==0.0?1.0:0.0);
}

double and(double a,double b) {
   return(a!=0.0 && b!=0.0);
}

double or(double a,double b) {
   return(a!=0.0 || b!=0.0);
}

double bnot(double a) {
   return(~((int)a));
}

double band(double a,double b) {
   return(((int)a)&((int)b));
}

double bor(double a,double b) {
   return(((int)a)| ((int)b));
}

double min(double a,double b) {
   return(a<=b?a:b);
}

double max(double a,double b) {
   return(a>=b?a:b);
}

double clamp(double a,double b,double c) {
   return(a<=b?b:(a>=c?c:a));
}

double within(double a,double b,double c) {
   return(a>=b && a<=c);
}

double ifelse(double a,double b,double c) {
   return(a!=0.0?b:c);
}

double frand(double a,double b,double c) {
   return(b+((c-b)*(rand()/(RAND_MAX+1.0))));
}
