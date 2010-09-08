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

double Vnb,Vavgx,Vavgy,Vvarx,Vvary,Vssx,Vs,Vssy,Vssxy,Vrmse,Vcorr,Vcovar,
       Vregb,Vrega,Verra,Verrb,Vssxy,Vmb,Vnmb,Vnme,Vme,Vmb,Vmnb,
       Vmne,Vmfb,Vmfe,Vlmnb,Vlmne,Vnrmse,Vna,Vrna;

/*Matrix Derivative Functions*/
TFuncDef FuncD[] = {
  { "in"        , in        , 2 , TD_UByte },
  { "lut"       , lut       , 3 , TD_Unknown },
  { "slut"      , slut      , 3 , TD_Unknown },
  { "fkernel"   , fkernel   , 2 , TD_Unknown },
  { "fcentile"  , fcentile  , 3 , TD_Unknown },
  { "fpeel"     , fpeel     , 1 , TD_Unknown },
  { "darea"     , darea     , 1 , TD_Float32 },
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
  { NULL        , NULL      , 0 , TD_Unknown }
};

/*Matrix to Float functions*/
TFuncDef FuncF[] = {
  { "sall"  , stat_all    , 2 , TD_Float64 },
  { "snb"   , stat_nb     , 1 , TD_Float64 },
  { "ssum"  , stat_sum    , 1 , TD_Float64 },
  { "smin"  , stat_min    , 1 , TD_Float64 },
  { "smax"  , stat_max    , 1 , TD_Float64 },
  { "savg"  , stat_avg    , 1 , TD_Float64 },
  { "savgx" , stat_avgx   , 1 , TD_Float64 },
  { "savgy" , stat_avgy   , 1 , TD_Float64 },
  { "svar"  , stat_varx   , 1 , TD_Float64 },
  { "svarx" , stat_varx   , 1 , TD_Float64 },
  { "svary" , stat_vary   , 1 , TD_Float64 },
  { "sssx"  , stat_ssx    , 1 , TD_Float64 },
  { "srmse" , stat_rmse   , 2 , TD_Float64 },
  { "scor"  , stat_corr   , 2 , TD_Float64 },
  { "scov"  , stat_covar  , 2 , TD_Float64 },
  { "sregb" , stat_regb   , 2 , TD_Float64 },
  { "srega" , stat_rega   , 2 , TD_Float64 },
  { "serra" , stat_erra   , 2 , TD_Float64 },
  { "serrb" , stat_errb   , 2 , TD_Float64 },
  { "ssxy"  , stat_ssxy   , 2 , TD_Float64 },
  { "smb"   , stat_mb     , 2 , TD_Float64 },
  { "snmb"  , stat_nmb    , 2 , TD_Float64 },
  { "snme"  , stat_nme    , 2 , TD_Float64 },
  { "sme"   , stat_me     , 2 , TD_Float64 },
  { "smnb"  , stat_mb     , 2 , TD_Float64 },
  { "smne"  , stat_mne    , 2 , TD_Float64 },
  { "smfb"  , stat_mfb    , 2 , TD_Float64 },
  { "smfe"  , stat_mfe    , 2 , TD_Float64 },
  { "slmnb" , stat_lmnb   , 2 , TD_Float64 },
  { "slmne" , stat_lmne   , 2 , TD_Float64 },
  { "snrmse", stat_nrmse  , 2 , TD_Float64 },
  { "sna"   , stat_na     , 2 , TD_Float64 },
  { "srna"  , stat_rna    , 2 , TD_Float64 },
  { NULL    , NULL        , 0 , TD_Unknown }
};

/*Matrix to Matrix functions*/
TFuncDef FuncM[] = {
  { "max"   , (TFunc*)max   , 2 , TD_Unknown },
  { "min"   , (TFunc*)min   , 2 , TD_Unknown },
  { "dif"   , (TFunc*)dif   , 2 , TD_Unknown },
  { "fmod"  , (TFunc*)fmod  , 2 , TD_Unknown },
  { "pow"   , (TFunc*)pow   , 2 , TD_Unknown },
  { "clamp" , (TFunc*)clamp , 3 , TD_Unknown },
  { "?"     , (TFunc*)ifelse, 3 , TD_Unknown },
  { "ifelse", (TFunc*)ifelse, 3 , TD_Unknown },
  { "sin"   , (TFunc*)sin   , 1 , TD_Float32 },   /* Trigonometric functions */
  { "cos"   , (TFunc*)cos   , 1 , TD_Float32 },
  { "tan"   , (TFunc*)tan   , 1 , TD_Float32 },
  { "asin"  , (TFunc*)asin  , 1 , TD_Float32 },
  { "acos"  , (TFunc*)acos  , 1 , TD_Float32 },
  { "atan"  , (TFunc*)atan  , 1 , TD_Float32 },
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

double tcount(TDataDef *Res,TDataDef *Table,TDataDef *MB) {

   double        v,vb,va;
   unsigned long i,j,idx,idxi,k;

   /*Parse matrix*/
   for(j=0;j<MB->NJ;j++) {
      idx=j*MB->NI;
      for(i=0;i<MB->NI;i++) {
         idxi=idx+i;

         /*Increment result table at matrix value's index*/
         Def_Get(MB,0,idxi,vb);
         k=vb;

         if (k<Table->NI) {
            Def_Get(Res,0,k,va)
            va++;
            Def_Set(Res,0,k,va);
         } else {
            fprintf(stderr,"(WARNING) Index overflow (tcount) : %i\n",k);
         }
      }
   }
   return(0.0);
}

double fpeel(TDataDef *Res,TDataDef *MA) {

   double        v,va;
   unsigned long i,j,idx,idxi;
   char          t;

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

   dmi=MB->NI>>1;
   dmj=MB->NJ>>1;
   w=0.0;
   for(i=0;i<FSIZE2D(MB);i++) {
      Def_Get(MB,0,i,vb);
      w+=vb;
   }

   /*Copy the matrix to a type we know, this will speedup the rest of the process*/
   vs=(float*)malloc(FSIZE2D(MA)*sizeof(float));
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
   return(0.0);
}

double fcentile(TDataDef *Res,TDataDef *MA,TDataDef *MB,TDataDef *MC) {

   double        *vs,va,vb,vc;
   unsigned long i,j,fi,fj,idx;
   int           dm,dj,di,s;

   Def_Get(MB,0,0,vb);
   Def_Get(MC,0,0,vc);

   if (fmod(vb,2)==0.0)
      vb+=1.0;

   vs=(double*)malloc(vb*vb*sizeof(double));
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
   return(0.0);
}

double in(TDataDef *Res,TDataDef *MA,TDataDef *MB) {

   double        va,vb,vr;
   unsigned long i,n;

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

double slut(TDataDef *Res,TDataDef *MA,TDataDef *MB,TDataDef *MC) {

   double va,vb,vc;
   long   i,i0,i1,n,m;

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
   double  b[9];
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
            asp=(dxy2==0.0?0.0:(dx==0.0?(dy>0?0.0:180.0):-RAD2DEG(atan2(dy,dx))+90.0));
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

void stat_core(TDataDef *MA,TDataDef *MB) {

   double va,vb;
   double ratio,sum,dif;
   unsigned long i,n;

   Vcorr=Vnb=Vavgx=Vavgy=Vssx=Vssy=Vssxy=Vs=Vrmse=Vmb=Vnmb=Vnme=Vvarx=Vvary=Vcovar=Vme=Vmnb=Vmne=Vmfb=Vmfe=Vlmnb=Vlmne=Vnrmse=Vna=Vrna=0.0;

   if ((n=FSIZE3D(MA))==0) {
      return;
   }

   for(i=0;i<n;i++) {
      if (MA) {
         Def_Get(MA,0,i,va);
         if (va==MA->NoData) {
            Vna++;
            continue;
         }
      }

      if (MB) {
         Def_Get(MB,0,i,vb);
         if (vb==MB->NoData) {
            Vna++;
            continue;
         }
      }

      Vavgx+=va;
      Vssx+=va*va;

      if (MB) {
         sum=vb+va;
         dif=vb-va;

         Vavgy+=vb;
         Vssy+=vb*vb;
         Vssxy+=va*vb;
         Vrmse+=dif*dif;
         Vmb+=dif;
         Vme+=fabs(dif);

         if (va!=0.0f) {
            va=1.0/va;
            Vmnb+=dif*va;
            Vmne+=fabs(dif*va);

            ratio=vb*va;
            if (ratio>0.0f) {
               Vlmnb+=log(ratio);
               Vlmne+=fabs(log(ratio));
            }
         }
         if (sum!=0.0f) {
            Vmfb+=dif/sum;
            Vmfe+=fabs(dif/sum);
         }
      }
      Vnb++;
   }

   Vrna=Vnb/n;

   if (Vnb==0) {
      Vcorr=Vavgx=Vavgy=Vssx=Vssy=Vssxy=Vs=Vrmse=Vmb=Vnmb=Vnme=Vvarx=Vvary=Vcovar=Vme=Vmnb=Vmne=Vmfb=Vmfe=Vlmnb=Vlmne=Vnrmse;
      return;
   }

   if (Vavgx!=0.0f) {
      Vnmb=Vmb/Vavgx*100.0f;
      Vnme=Vme/Vavgx*100.0f;
   } else {
      Vnmb=0.0f;
      Vnme=0.0f;
   }

   Vavgx /= Vnb;
   Vavgy /= Vnb;
   Vssx  -= Vnb*Vavgx*Vavgx;
   Vssy  -= Vnb*Vavgy*Vavgy;
   Vssxy -= Vnb*Vavgx*Vavgy;
   Vs     = sqrt((Vssy-(Vssxy*Vssxy)/Vssx)/(Vnb-2.0));

   for(i=0;i<n;i++) {
      Def_Get(MA,0,i,va);
      if (MB)
         Def_Get(MB,0,i,vb);

      if (va!=MA->NoData && (MB && vb!=MB->NoData)) {
         Vvarx+=(va-Vavgx)*(va-Vavgx);
         Vvary+=(vb-Vavgy)*(vb-Vavgy);
         Vcovar+=(va-Vavgx)*(vb-Vavgy);
      }
   }

   Vvarx/=Vnb;
   Vvary/=Vnb;
   Vcovar/=Vnb;
   Vcorr=Vssy==0?0:(Vssxy*Vssxy)/(Vssx*Vssy);
   Vrmse=sqrt(Vrmse/Vnb);
   Vnrmse=Vrmse/Vavgx;
   Vregb=Vssxy/Vssx;
   Vrega=Vavgy-(Vssxy/Vssx)*Vavgx;
   Verra=Vs*sqrt(1.0/Vnb+(Vavgx*Vavgx)/Vssx);
   Verrb=Vs/sqrt(Vssx);
   Vmb/=Vnb;
   Vme/=Vnb;
   Vmne/=Vnb;
   Vmnb/=Vnb;
   Vmfb=2.0*Vmfb/Vnb;
   Vmfe=2.0*Vmfe/Vnb;
   Vlmnb=Vlmnb/Vnb-1;
   Vlmne=Vlmne/Vnb-1;
}

double stat_all(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vnb);
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
   if (MA)
      stat_core(MA,MB);
   return(Vmb);
}

double stat_me(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vme);
}

double stat_nmb(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vnmb);
}

double stat_nme(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vnme);
}

double stat_mnb(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vmnb);
}

double stat_mne(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vmne);
}

double stat_lmne(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vlmne);
}

double stat_lmnb(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vlmnb);
}

double stat_mfb(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vmfb);
}

double stat_mfe(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vmfe);
}

double stat_rmse(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vrmse);
}

double stat_nrmse(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vnrmse);
}

double stat_corr(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vcorr);
}

double stat_covar(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vcovar);
}

double stat_varx(TDataDef *MA) {
   if (MA)
      stat_core(MA,NULL);
   return(Vvarx);
}

double stat_vary(TDataDef *MA) {
   if (MA)
      stat_core(MA,NULL);
   return(Vvary);
}

double stat_avgx(TDataDef *MA) {
   if (MA)
      stat_core(MA,NULL);
   return(Vavgx);
}

double stat_avgy(TDataDef *MA) {
   if (MA)
      stat_core(MA,NULL);
   return(Vavgy);
}

double stat_regb(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vregb);
}

double stat_rega(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vrega);
}

double stat_erra(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Verra);
}

double stat_errb(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Verrb);
}

double stat_ssx(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vssx);
}

double stat_ssxy(TDataDef *MA,TDataDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vssy);
}

double stat_nb(TDataDef *MA) {
   if (MA)
      stat_core(MA,NULL);
   return(Vnb);
}

double stat_sum(TDataDef *M) {

   double        sum=0,v;
   unsigned long i;

   for(i=0;i<FSIZE3D(M);i++) {
      Def_Get(M,0,i,v);
      sum+=v;
   }
   return sum;
}

double stat_min(TDataDef *M) {

   double        min=HUGE_VAL,v;
   unsigned long i;

   for(i=0;i<FSIZE3D(M);i++) {
      Def_Get(M,0,i,v);
      min=min<v?min:v;
   }
   return min;
}

double stat_max(TDataDef *M) {

   double        max=-HUGE_VAL,v;
   unsigned long i;

   for(i=0;i<FSIZE3D(M);i++) {
      Def_Get(M,0,i,v);
      max=max>v?max:v;
   }
   return max;
}

double stat_avg(TDataDef *M) {

   double        sum=0,v;
   unsigned long i;

   for(i=0;i<FSIZE3D(M);i++) {
      Def_Get(M,0,i,v);
      sum+=v;
   }
   return sum/FSIZE3D(M);
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

double min(double a,double b) {
   if (a<=b)
      return(a);
   return(b);
}

double max(double a,double b) {
   if (a>=b)
      return(a);
   return(b);
}

double clamp(double a,double b,double c) {
   if (a<=b)
      return(b);
   if (a>=c)
      return(c);
   return(a);
}

double ifelse(double a,double b,double c) {
   if (a!=0.0) {
      return(b);
   }
   return(c);
}
