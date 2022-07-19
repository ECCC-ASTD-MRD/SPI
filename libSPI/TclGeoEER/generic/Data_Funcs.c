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

#ifdef HAVE_OPENMP
#include <omp.h>
#endif //HAVE_OPENMP

#include <math.h>
#include "App.h"
#include "Data_Funcs.h"
#include "Data_Calc.h"

#ifdef HAVE_DISTANCEMETRICS
#include "DistanceMetrics.h"
#endif //HAVE_DISTANCEMETRICS

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
       Vmne,Vmfb,Vmfe,Vlmnb,Vlmne,Vnrmse,Vna,Vrna,Vmse,Vnmse,Vgmb,Vgmv,Vfoex,Vfa2,Vfa5,Vfa10,Vfb,Vnad,
       Vfms,Vfmsi,Vfmsb,Vfmsn,Vosf,Vosfb,Vosfi,Vksp,Vrank,Vnbeq,Vnbgt,Vnblt,Vnbfa,Vnbmi,Vnbnp,
       Vaov,Vafn,Vafp,Vax,Vay;

/*Matrix Derivative Functions*/
TFuncDef FuncD[] = {
  { "in"        , in        , 2, 0, TD_UByte },
  { "win"       , win       , 3, 0, TD_Int32 },
  { "lut"       , lut       , 3, 0, TD_Unknown },
  { "slut"      , slut      , 3, 0, TD_Unknown },
  { "fkernel"   , fkernel   , 2, 0, TD_Unknown },
  { "fcentile"  , fcentile  , 3, 0, TD_Unknown },
  { "fpeel"     , fpeel     , 1, 0, TD_Unknown },
  { "darea"     , darea     , 1, 0, TD_Float32 },
  { "dcoriolis" , dcoriolis , 1, 0, TD_Float32 },
  { "dlat"      , dlat      , 1, 0, TD_Float32 },
  { "dlon"      , dlon      , 1, 0, TD_Float32 },
  { "ddx"       , ddx       , 1, 0, TD_Float32 },
  { "ddy"       , ddy       , 1, 0, TD_Float32 },
  { "dangle"    , dangle    , 1, 0, TD_Float32 },
  { "dslopedeg" , dslopedeg , 1, 0, TD_Float32 },
  { "dslope100" , dslope100 , 1, 0, TD_Float32 },
  { "daspect"   , daspect   , 1, 0, TD_Float32 },
  { "ddxfirst"  , ddxfirst  , 1, 0, TD_Float32 },
  { "ddyfirst"  , ddyfirst  , 1, 0, TD_Float32 },
  { "ddxsecond" , ddxsecond , 1, 0, TD_Float32 },
  { "ddysecond" , ddysecond , 1, 0, TD_Float32 },
  { "ddxysecond", ddxysecond, 1, 0, TD_Float32 },
  { "dprofcurve", dprofcurve, 1, 0, TD_Float32 },
  { "dtangcurve", dtangcurve, 1, 0, TD_Float32 },

  { "tcount"    , tcount    , 2, 0, TD_Int32 },
  { "flipy"     , flipy     , 1, 0, TD_Unknown },

  // Distance metrics
  { "dt"    , (TFunc*)dt    , 1, 0, TD_Float64 },
  { "gdt"   , (TFunc*)gdt   , 2, 1, TD_Float64 },

  { NULL        , NULL      , 0, 0, TD_Unknown }
};

/*Matrix to Float functions*/
TFuncDef FuncF[] = {
  { "sall"  , stat_all    , 2, 2, TD_Float64 },
  { "snb"   , stat_nb     , 1, 1, TD_Float64 },
  { "smed"  , stat_med    , 1, 0, TD_Float64 },
  { "smedx" , stat_medx   , 2, 2, TD_Float64 },
  { "smedy" , stat_medy   , 2, 2, TD_Float64 },
  { "suniq" , stat_unique , 1, 0, TD_Int32 },
  { "ssum"  , stat_sum    , 1, 0, TD_Float64 },
  { "ssumx" , stat_sumx   , 2, 2, TD_Float64 },
  { "ssumy" , stat_sumy   , 2, 2, TD_Float64 },
  { "smin"  , stat_min    , 1, 0, TD_Float64 },
  { "sminx" , stat_minx   , 2, 2, TD_Float64 },
  { "sminy" , stat_miny   , 2, 2, TD_Float64 },
  { "smax"  , stat_max    , 1, 0, TD_Float64 },
  { "smaxx" , stat_maxx   , 2, 2, TD_Float64 },
  { "smaxy" , stat_maxy   , 2, 2, TD_Float64 },
  { "savg"  , stat_avg    , 1, 0, TD_Float64 },
  { "savgx" , stat_avgx   , 2, 2, TD_Float64 },
  { "savgy" , stat_avgy   , 2, 2, TD_Float64 },
  { "svar"  , stat_var    , 1, 1, TD_Float64 },
  { "svarx" , stat_varx   , 2, 2, TD_Float64 },
  { "svary" , stat_vary   , 2, 2, TD_Float64 },
  { "sssx"  , stat_ssx    , 2, 2, TD_Float64 },
  { "sssy"  , stat_ssy    , 2, 2, TD_Float64 },
  { "sssxy" , stat_ssxy   , 2, 2, TD_Float64 },
  { "srmse" , stat_rmse   , 2, 2, TD_Float64 },
  { "ssdev" , stat_sdev   , 1, 1, TD_Float64 },
  { "ssdevx", stat_sdevx  , 2, 2, TD_Float64 },
  { "ssdevy", stat_sdevy  , 2, 2, TD_Float64 },
  { "scor"  , stat_corr   , 2, 2, TD_Float64 },
  { "scov"  , stat_covar  , 2, 2, TD_Float64 },
  { "sregb" , stat_regb   , 2, 2, TD_Float64 },
  { "srega" , stat_rega   , 2, 2, TD_Float64 },
  { "serra" , stat_erra   , 2, 2, TD_Float64 },
  { "serrb" , stat_errb   , 2, 2, TD_Float64 },
  { "smb"   , stat_mb     , 2, 2, TD_Float64 },
  { "snmb"  , stat_nmb    , 2, 2, TD_Float64 },
  { "snme"  , stat_nme    , 2, 2, TD_Float64 },
  { "sme"   , stat_me     , 2, 2, TD_Float64 },
  { "smnb"  , stat_mnb    , 2, 2, TD_Float64 },
  { "smne"  , stat_mne    , 2, 2, TD_Float64 },
  { "smfb"  , stat_mfb    , 2, 2, TD_Float64 },
  { "smfe"  , stat_mfe    , 2, 2, TD_Float64 },
  { "slmnb" , stat_lmnb   , 2, 2, TD_Float64 },
  { "slmne" , stat_lmne   , 2, 2, TD_Float64 },
  { "smre"  , stat_mre    , 2, 2, TD_Float64 },
  { "smaxb" , stat_maxb   , 2, 2, TD_Float64 },
  { "smaxe" , stat_maxe   , 2, 2, TD_Float64 },
  { "smaxre", stat_maxre  , 2, 2, TD_Float64 },
  { "snrmse", stat_nrmse  , 2, 2, TD_Float64 },
  { "sna"   , stat_na     , 2, 2, TD_Float64 },
  { "srna"  , stat_rna    , 2, 2, TD_Float64 },
  { "smse"  , stat_mse    , 2, 2, TD_Float64 },
  { "snmse" , stat_nmse   , 2, 2, TD_Float64 },
  { "sgmb"  , stat_gmb    , 2, 2, TD_Float64 },
  { "sgmv"  , stat_gmv    , 2, 2, TD_Float64 },
  { "sfoex" , stat_foex   , 2, 2, TD_Float64 },
  { "sfa2"  , stat_fa2    , 2, 2, TD_Float64 },
  { "sfa5"  , stat_fa5    , 2, 2, TD_Float64 },
  { "sfa10" , stat_fa10   , 2, 2, TD_Float64 },
  { "sfb"   , stat_fb     , 2, 2, TD_Float64 },
  { "snad"  , stat_nad    , 2, 2, TD_Float64 },
  { "sfms"  , stat_fms    , 2, 2, TD_Float64 },
  { "sfmsb" , stat_fmsb   , 2, 2, TD_Float64 },
  { "sfmsi" , stat_fmsi   , 2, 2, TD_Float64 },
  { "sfmsn" , stat_fmsn   , 2, 2, TD_Float64 },
  { "sosf"  , stat_osf    , 2, 2, TD_Float64 },
  { "sosfb" , stat_osfb   , 2, 2, TD_Float64 },
  { "sosfi" , stat_osfi   , 2, 2, TD_Float64 },
  { "sksp"  , stat_ksp    , 2, 2, TD_Float64 },
  { "srank" , stat_rank   , 2, 2, TD_Float64 },
  { "snbeq" , stat_nbeq   , 2, 2, TD_Float64 },
  { "snbgt" , stat_nbgt   , 2, 2, TD_Float64 },
  { "snblt" , stat_nblt   , 2, 2, TD_Float64 },
  { "snbfa" , stat_nbfa   , 2, 2, TD_Float64 },
  { "snbmi" , stat_nbmi   , 2, 2, TD_Float64 },
  { "snbnp" , stat_nbnp   , 2, 2, TD_Float64 },
  { "saov"  , stat_aov    , 2, 2, TD_Float64 },
  { "safn"  , stat_afn    , 2, 2, TD_Float64 },
  { "safp"  , stat_afp    , 2, 2, TD_Float64 },
  { "sax"   , stat_ax     , 2, 2, TD_Float64 },
  { "say"   , stat_ay     , 2, 2, TD_Float64 },

  { "irand" , initrand    , 1, 1, TD_Float64 },

  // Distance metrics
  { "dmnp"  , dmnp        , 1, 0, TD_Float64 },
  { "haus"  , hausdorff   , 3, 1, TD_Float64 },
  { "badd"  , baddeley    , 3, 1, TD_Float64 },
  { "gdm"   , gdm         , 7, 5, TD_Float64 },

  { NULL    , NULL        , 0, 0, TD_Unknown }
};

/*Matrix to Matrix functions*/
TFuncDef FuncM[] = {
  { "max"   , (TFunc*)max   , 2, 0, TD_Unknown },
  { "min"   , (TFunc*)min   , 2, 0, TD_Unknown },
  { "dif"   , (TFunc*)dif   , 2, 0, TD_Unknown },
  { "fmod"  , (TFunc*)fmod  , 2, 0, TD_Unknown },
  { "pow"   , (TFunc*)pow   , 2, 0, TD_Unknown },
  { "frand" , (TFunc*)frand , 3, 0, TD_Unknown },
  { "clamp" , (TFunc*)clamp , 3, 0, TD_Unknown },
  { "within", (TFunc*)within, 3, 0, TD_Unknown },
  { "?"     , (TFunc*)ifelse, 3, 0, TD_Unknown },
  { "ifelse", (TFunc*)ifelse, 3, 0, TD_Unknown },
  { "sin"   , (TFunc*)sin   , 1, 0, TD_Float32 },   /* Trigonometric functions */
  { "cos"   , (TFunc*)cos   , 1, 0, TD_Float32 },
  { "tan"   , (TFunc*)tan   , 1, 0, TD_Float32 },
  { "asin"  , (TFunc*)asin  , 1, 0, TD_Float32 },
  { "acos"  , (TFunc*)acos  , 1, 0, TD_Float32 },
  { "atan"  , (TFunc*)atan  , 1, 0, TD_Float32 },
  { "atan2" , (TFunc*)atan2 , 2, 0, TD_Float32 },
  { "sinh"  , (TFunc*)sinh  , 1, 0, TD_Float32 },   /* Hyperbolic functions */
  { "cosh"  , (TFunc*)cosh  , 1, 0, TD_Float32 },
  { "tanh"  , (TFunc*)tanh  , 1, 0, TD_Float32 },
  { "asinh" , (TFunc*)asinh , 1, 0, TD_Float32 },
  { "acosh" , (TFunc*)acosh , 1, 0, TD_Float32 },
  { "atanh" , (TFunc*)atanh , 1, 0, TD_Float32 },
  { "ln"    , (TFunc*)log   , 1, 0, TD_Float32 },   /* Logarithmic, exponentials and root functions */
  { "log"   , (TFunc*)log10 , 1, 0, TD_Float32 },
  { "exp"   , (TFunc*)exp   , 1, 0, TD_Float32 },
  { "sqrt"  , (TFunc*)sqrt  , 1, 0, TD_Float32 },
  { "cbrt"  , (TFunc*)cbrt  , 1, 0, TD_Float32 },
  { "abs"   , (TFunc*)fabs  , 1, 0, TD_Float32 }, /* Misc functions */
  { "ceil"  , (TFunc*)ceil  , 1, 0, TD_Unknown },
  { "floor" , (TFunc*)floor , 1, 0, TD_Unknown },
  { "round" , (TFunc*)rint  , 1, 0, TD_Unknown },

  { NULL    , (TFunc*)NULL  , 0, 0, TD_Unknown }
};


/*Matrix Creation/Manipulation Functions*/
TFuncDef FuncC[] = {
  { "seq"       , seq       , 4, 4, TD_Float64 },
  { "reshape"   , reshape   , 5, 3, TD_Unknown },
  { "repeat"    , repeat    , 3, 1, TD_Unknown },
  { "join"      , join      , 9, 7, TD_Unknown },

  { NULL        , NULL      , 0, 0, TD_Unknown }
};


typedef struct
{
   double b;
   double c;
} LUTentry;

static int compare_lutE ( const void *va, const void *vb );

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

double seq(TDef *Res,TDef *From,TDef *To,TDef *Step,TDef *N) {
   double from,to,step;
   int i,n;

   // Make sure we got scalars for the dimensions
   if( From && FSIZE3D(From)!=1 )   Calc_RaiseError("seq: The start value of the sequence should be a scalar\n");
   if( To && FSIZE3D(To)!=1 )       Calc_RaiseError("seq: The end value of the sequence should be a scalar\n");
   if( Step && FSIZE3D(Step)!=1 )   Calc_RaiseError("seq: The stepping value of the sequence should be a scalar\n");
   if( N && FSIZE3D(N)!=1 )         Calc_RaiseError("seq: The stepping value of the sequence should be a scalar\n");

   if( Calc_InError() )
      return(0.0);

   // Get the values from the fields
   if( From )  Def_Get(From,0,0,from);
   if( To )    Def_Get(To,0,0,to);

   if( Step ) {
      Def_Get(Step,0,0,step);
      if( step == 0.0 ) {
         Calc_RaiseError("seq: The step can't be zero, that would make an infinite amount of values\n");
         return(0.0);
      }
   }

   if( N ) {
      Def_Get(N,0,0,n);
      if( n <= 0 ) {
         Calc_RaiseError("seq: The number of values in the sequence can't be negative nor 0\n");
         return(0.0);
      }
   }

   // Calculate the missing values and make sure they are compatible
   switch( !From<<3 | !To<<2 | !Step<<1 | !N ) {
      case 0:
         // All values are given, make sure they are compatible
         if( ((to-from)<0.0) ^ (step<0.0) ) {
            Calc_RaiseError("seq: Incompatible to, from and step values : there is no way to reach the end from that starting point with that step\n");
            return(0.0);
         }
         if( (n==1) ^ (to==from) ) {
            Calc_RaiseError("seq: Incompatible From, To and N values : either N=1 but From!=To or From==To but N!=1\n");
            return(0.0);
         }
         if( n != (int)((to-from)/step)+1 ) {
            Calc_RaiseError("seq: Incompatible To, From, Step and N values : the number of values that would be created is different from the requested number of values\n");
            return(0.0);
         }
         break;
      case 1:
         // We have From, To and Step
         // Coherence check
         if( ((to-from)<0.0) ^ (step<0.0) ) {
            Calc_RaiseError("seq: Incompatible to, from and step values : there is no way to reach the end from that starting point with that step\n");
            return(0.0);
         }
         // Calculate the number of values we'll generate
         n = (int)((to-from)/step)+1;
         break;
      case 2:
         // We have From, To and N
         // Coherence check
         if( (n==1) ^ (to==from) ) {
            Calc_RaiseError("seq: Incompatible From, To and N values : either N=1 but From!=To or From==To but N!=1\n");
            return(0.0);
         }
         // Calculate the step
         step = n==1 ? 0 : (to-from)/(n-1);
         break;
      case 3:
         // We have From and To
         // Apply corresponding default stepping value
         step = from<=to ? 1.0 : -1.0;
         // Calculate the number of values we'll generate
         n = (int)((to-from)/step)+1;
         break;
      case 4:
         // We have From, Step and N
         // No need to calculate the "To" value as we won't use it anyway
         break;
      case 6:
         // We have From and N
         // Apply default stepping value
         step = 1.0;
         break;
      case 8:
         // We have To, Step and N
         // Set the from value and reverse the stepping
         from = to;
         step = -step;
         break;
      case 10:
         // We have To and N
         // Set the from value and set the reverse default stepping
         from = to;
         step = -1.0;
         break;
      case 5:  // We have From and Step
      case 7:  // We have From
      case 9:  // We have To and Step
      case 11: // We have To
      case 12: // We have Step and N
      case 13: // We have Step
      case 14: // We have N
      case 15: // We have nothing
      default:
         Calc_RaiseError("seq: Invalid combination of arguments. Valid combinations are: From+To+Step+N, From+To+Step, From+To+N, From+To, From+Step+N, From+N, To+Step+N and To+N\n");
         return(0.0);
   }

   // Resize the result field to hold the values we'll generate
   if( !Def_Resize(Res,n,1,1) ) {
      Calc_RaiseError("seq: An error occured when resizing.\n");
      return(0.0);
   }

   // Generate the sequence
   for(i=0; i<n; ++i) {
      Def_Set(Res,0,i,from+i*step);
   }

   return(n);
}

double reshape(TDef *Res,TDef *Fld,TDef *NI,TDef *NJ,TDef *NK,TDef *NC) {
   int ni=1,nj=1,nk=1,nc=1;

   if( FSIZE3D(Fld)==0 )      Calc_RaiseError("reshape: The field to resize has as zeroed dimension\n");
   if( NI && FSIZE3D(NI)!=1 ) Calc_RaiseError("reshape: The new dimension in I should be a scalar\n");
   if( NJ && FSIZE3D(NJ)!=1 ) Calc_RaiseError("reshape: The new dimension in J should be a scalar\n");
   if( NK && FSIZE3D(NK)!=1 ) Calc_RaiseError("reshape: The new dimension in K should be a scalar\n");
   if( NC && FSIZE3D(NC)!=1 ) Calc_RaiseError("reshape: The new dimension in C should be a scalar\n");

   if( !Calc_InError() ) {
      if( NI ) Def_Get(NI,0,0,ni);
      if( NJ ) Def_Get(NJ,0,0,nj);
      if( NK ) Def_Get(NK,0,0,nk);
      if( NC ) Def_Get(NC,0,0,nc);

      if( FSIZE3D(Fld)*Fld->NC == ni*nj*nk*nc ) {
         // Two things can happen: either NC was already the same in which case nothing changes,
         // or NC is different in which case NI or NJ or NK had to change (which guaranties it will be handled by Def_Resize)
         Res->NC = nc;
         if( !Def_Resize(Res,ni,nj,nk) ) {
            Calc_RaiseError("reshape: An error occured when resizing.\n");
         }
         memcpy(Res->Data[0],Fld->Data[0],TDef_Size[Res->Type]*ni*nj*nk*nc);
      } else {
         Calc_RaiseError("reshape: The new dimensions are invalid. They must match the current dimensions of the field.\n");
      }
   }

   return(0.0);
}

double repeat(TDef *Res,TDef *Fld,TDef *N,TDef *D) {
   char *datar,*dataf;
   size_t size,nloops;
   const int nd=4;
   int n,r,i,dim[4]={Fld->NI,Fld->NJ,Fld->NK,Fld->NC},d,id;

   if( FSIZE3D(Fld)==0 )      Calc_RaiseError("repeat: The field to repeat has as zeroed dimension\n");
   if( FSIZE3D(N)!=1 )        Calc_RaiseError("repeat: The repeat number should be a scalar\n");
   if( D && FSIZE3D(D)!=1 )   Calc_RaiseError("repeat: The expanding dimension should be a scalar\n");

   if( Calc_InError() )
      return(0.0);

   // Get the number of times we'll repeat the sequence
   Def_Get(N,0,0,n);
   if( n < 0 ) {
      Calc_RaiseError("repeat: Can't repeat a sequence a negative number of time\n");
      return(0.0);
   } else if( !n ) {
      Calc_RaiseError("repeat: Can't repeat a sequence a nul amount of time, the result would be empty\n");
      return(0.0);
   }

   // Check in which dimension we'll expand
   if( D ) {
      Def_Get(D,0,0,d);
      if( d<0 || d>=nd ) {
         Calc_RaiseError("repeat: The expanding dimension should be either 0 (I), 1 (J), 2 (K) or 3 (C)\n");
         return(0.0);
      }
   } else {
      // Find the first unused dimension (dim==1) or select the last one
      for(d=0; d<nd-1; ++d)
         if( dim[d] == 1 )
            break;
   }

   // Get the number of contiguous bytes we can copy per shot
   for(id=0,size=(size_t)TDef_Size[Res->Type]; id<=d; ++id)
      size *= (size_t)dim[id];

   // Get the number of times we'll need to loop over the repeat
   for(id=d+1,nloops=1; id<nd; ++id)
      nloops *= (size_t)dim[id];

   // Resize the result field to hold the values we'll generate
   dim[d] *= n;
   if( d==3 && dim[3]>4 ) {
      Calc_RaiseError("repeat: A maximum of 4 components are possible\n");
      return(0.0);
   }

   // If we need to resize NC, make sure we trigger the resize by setting NI to 0
   if( Res->NC != dim[3] ) {
      Res->NC = dim[3];
      Res->NI = 0;
   }
   if( !Def_Resize(Res,dim[0],dim[1],dim[2]) ) {
      Calc_RaiseError("repeat: An error occured when resizing.\n");
      return(0.0);
   }

   // Set the values
   for(datar=Res->Data[0],dataf=Fld->Data[0]; nloops; --nloops,dataf+=size) {
      for(r=0; r<n; ++r,datar+=size) {
         memcpy(datar,dataf,size);
      }
   }

   return(FSIZE3D(Res));
}

double join(TDef *Res,TDef *D,TDef *F1,TDef *F2,TDef *F3,TDef *F4,TDef *F5,TDef *F6,TDef *F7,TDef *F8) {
   TDef *flds[8];
   const int nd=4;
   int i,d,id,n=0,dim[nd],dimf[nd],type;

   if( FSIZE3D(D)!=1 )     Calc_RaiseError("join: The expanding dimension should be a scalar\n");

   if( Calc_InError() )
      return(0.0);

   // Check in which dimension we'll expand
   Def_Get(D,0,0,d);
   if( d<0 || d>=nd ) {
      Calc_RaiseError("join: The expanding dimension should be either 0 (I), 1 (J), 2 (K) or 3 (C)\n");
      return(0.0);
   }

   // Compile the list of fields
   if( F1 && FSIZE3D(F1)>0 ) flds[n++] = F1;
   if( F2 && FSIZE3D(F2)>0 ) flds[n++] = F2;
   if( F3 && FSIZE3D(F3)>0 ) flds[n++] = F3;
   if( F4 && FSIZE3D(F4)>0 ) flds[n++] = F4;
   if( F5 && FSIZE3D(F5)>0 ) flds[n++] = F5;
   if( F6 && FSIZE3D(F6)>0 ) flds[n++] = F6;
   if( F7 && FSIZE3D(F7)>0 ) flds[n++] = F7;
   if( F8 && FSIZE3D(F8)>0 ) flds[n++] = F8;

   if( !n ) {
      Calc_RaiseError("join: No fields to join or all fields are empty\n");
      return(0.0);
   }

   // Make sure the dimensions of the fields are compatible in the direction that is not expanding
   // and calculate the final dimension
   size_t size[n],nloops;

   dim[0]=flds[0]->NI; dim[1]=flds[0]->NJ; dim[2]=flds[0]->NK; dim[3]=flds[0]->NC; dim[d]=0;
   for(i=0; i<n; ++i) {
      dimf[0]=flds[i]->NI; dimf[1]=flds[i]->NJ; dimf[2]=flds[i]->NK; dimf[3]=flds[i]->NC;

      if( flds[0]->Type != flds[i]->Type ) {
         Calc_RaiseError("join: Incompatible field type. The type of all fields to join must be the same\n");
         return(0.0);
      }

      size[i] = (size_t)TDef_Size[flds[i]->Type];

      // Loop over dimensions
      for(id=0; id<nd; ++id) {
         if( id == d ) {
            // We expand in this direction
            dim[d] += dimf[d];
         } else if( dim[id] != dimf[id] ) {
            // We don't expand in this direction, the dimension has to match
            Calc_RaiseError("join: Incompatible field size. The size of all fields to join must match in the non-expanding dimension\n");
            return(0.0);
         }

         // Get the number of contiguous bytes we can copy per shot for that field
         if( id <= d ) {
            size[i] *= (size_t)dimf[id];
         }
      }
   }

   // Max of 4 components
   if( d==3 && dim[3]>4 ) {
      Calc_RaiseError("join: A maximum of 4 components are possible\n");
      return(0.0);
   }

   // Res has been created specifically for the result of this function, but would have been inspired by the first arg (D)
   // So it is very possible that the type of the field is not the right one, but the good news is that we can do whatever we want to the field
   // In this case, if the type is not compatible (same size), we'll reset the NI dimension to make sure the resize resets the whole thing
   if( TDef_Size[Res->Type] != TDef_Size[flds[0]->Type] )
      Res->NI=0;
   Res->Type = flds[0]->Type;

   // If we need to resize NC, make sure we trigger the resize by setting NI to 0
   if( Res->NC != dim[3] ) {
      Res->NC = dim[3];
      Res->NI = 0;
   }

   // Resize the result field to hold the values we'll generate
   if( !Def_Resize(Res,dim[0],dim[1],dim[2]) ) {
      Calc_RaiseError("join: An error occured when resizing.\n");
      return(0.0);
   }

   // Get the number of times we'll need to loop over to make the entire copy
   for(id=d+1,nloops=1; id<=3; ++id)
      nloops *= (size_t)dim[id];

   // Init the data source
   char *datar,*dataf[n];
   for(i=0; i<n; ++i)
      dataf[i] = flds[i]->Data[0];

   // Join the fields
   for(datar=Res->Data[0]; nloops; --nloops) {
      for(i=0; i<n; datar+=size[i],dataf[i]+=size[i],++i) {
         memcpy(datar,dataf[i],size[i]);
      }
   }

   return(FSIZE3D(Res));
}

double flipy(TDef *Res,TDef *MA) {

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

double tcount(TDef *Res,TDef *Table,TDef *MB) {

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
#pragma omp parallel for \
      private( j,i,idx,idxi,v,va,vb,k ) \
      shared( MB, Res,nt ) \
      schedule(static)
   for(j=0;j<MB->NJ;j++) {
      idx=j*MB->NI;
      for(i=0;i<MB->NI;i++) {
         idxi=idx+i;

         /*Increment result table at matrix value's index*/
         Def_Get(MB,0,idxi,vb);
         if (DEFVALID(MB,vb)) {
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

double fpeel(TDef *Res,TDef *MA) {

   double        v,va;
   unsigned long i,j,idx,idxi;
   char          t;

   v=va=0.0;

   /*Parse the matrix*/
#pragma omp parallel for \
      private( j,i,idx,idxi,v,va,t ) \
      shared( MA, Res ) \
      schedule(static)
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

double fkernel(TDef *Res,TDef *MA,TDef *MB) {

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
#pragma omp parallel for \
         private(j,i,idxf,idx,dj,di,fj,fi,va,dw,vb,s) \
         shared(MA,MB,Res,vs,w) \
         schedule(static)
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
                     if (!DEFVALID(MA,va)) {
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

double fcentile(TDef *Res,TDef *MA,TDef *MB,TDef *MC) {

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
#pragma omp parallel for \
         private(j,i,idx,dj,di,fj,fi,va,s) \
         shared(MA,MB,MC,Res,vs,vb,vc,dm) \
         schedule(static)
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
                     if (DEFVALID(MA,va)) {
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

double in(TDef *Res,TDef *MA,TDef *MB) {

   double        va,vb,vr;
   unsigned long i,n;

   va=vb=0.0;
#pragma omp parallel for \
      private(i,n,va,vr,vb) \
      shared(MA,MB,Res) \
      schedule(static)
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

double win(TDef *Res,TDef *MA,TDef *MB,TDef *MC) {

   double        va,vb,vc;
   unsigned long i,j,n;

   va=vb=vc=0.0;
   
#pragma omp parallel for \
      private( j,i,va,vb,vc,n ) \
      shared( MA,MB,MC,Res ) \
      schedule(static)
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

double slut(TDef *Res,TDef *MA,TDef *MB,TDef *MC) {

   double va,vb,vc;
   long   i,i0,i1,n,m;

   va=vb=vc=0.0;
   i=FSIZE2D(MB);
   n=FSIZE2D(MC);
   m=i<n?i:n;

#pragma omp parallel for \
      private(i,i0,i1,n,va,vb,vc) \
      shared(MA,MB,MC,Res,m) \
      schedule(static)
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

static int compare_lutE (const void *va,const void *vb) {
   LUTentry **ppa = (LUTentry **)va;
   LUTentry **ppb = (LUTentry **)vb;
   LUTentry  *pa  = *ppa;
   LUTentry  *pb  = *ppb;

   if (pa->b < pb->b ) return -1;
   if (pa->b > pb->b ) return 1;
   return  0;
}


double lut(TDef *Res,TDef *MA,TDef *MB,TDef *MC) {

   double        va,vb,vc;
   double        last_va;
   unsigned long i,n,m;
   LUTentry      *table, **ptrtable, lute, *ptr, **pptr;
   int            szptr;
   int            szMA;

   va=vb=vc=0.0;
   i=FSIZE2D(MB);
   n=FSIZE2D(MC);
   m=i<n?i:n;
   szMA=FSIZE2D(MA);

   // Create a sorted LUT and use bsearch to make things faster
   szptr = sizeof(LUTentry *);
   table = (LUTentry *)malloc( sizeof(LUTentry) * m );
   ptrtable = (LUTentry **)malloc( szptr * m );
   for(n=0;n<m;n++) {
      Def_Get(MB,0,n,vb);
      Def_Get(MC,0,n,vc);
      table[n].b = vb;
      table[n].c = vc;
      ptrtable[n] = &(table[n]);
   }
   qsort( ptrtable, m, szptr, compare_lutE );

#pragma omp parallel shared(Res,ptrtable,table,MA,szMA,MB,szptr,m) \
   private(i,last_va,va,lute,ptr,pptr)
   {
   ptr = NULL;
   last_va = NAN;
#pragma omp for schedule(static)
   for(i=0;i<szMA;i++) {
      Def_Get(MA,0,i,va);
#if 1
      if (last_va != va) {
         lute.b = va;
         ptr = &lute;
         pptr = (LUTentry **)bsearch( &ptr, ptrtable, m, szptr, compare_lutE );
         ptr = (pptr != NULL) ? *pptr : NULL;
         last_va = va;
      }
      if (ptr) {
         Def_Set(Res,0,i, ptr->c );
      }
#else
      for(n=0;n<m;n++) {
         Def_Get(MB,0,n,vb);
         if (va==vb) {
            Def_Get(MC,0,n,vc);
            Def_Set(Res,0,i,vc);
            break;
         }
      }
#endif
   }
   }
   free( table );
   free( ptrtable );
   return(0.0);
}

double dslopedeg(TDef *Res,TDef *Def) {

   return(dcore(Res,Def,DSLOPEDEG));
}
double dslope100(TDef *Res,TDef *Def) {

   return(dcore(Res,Def,DSLOPE100));
}
double daspect(TDef *Res,TDef *Def) {

   return(dcore(Res,Def,DASPECT));
}
double ddxfirst(TDef *Res,TDef *Def) {

   return(dcore(Res,Def,DDX));
}
double ddyfirst(TDef *Res,TDef *Def) {

   return(dcore(Res,Def,DDY));
}
double ddxsecond(TDef *Res,TDef *Def) {

   return(dcore(Res,Def,DDXX));
}
double ddysecond(TDef *Res,TDef *Def) {

   return(dcore(Res,Def,DDYY));
}
double ddxysecond(TDef *Res,TDef *Def) {

   return(dcore(Res,Def,DDXY));
}
double dprofcurve(TDef *Res,TDef *Def) {

   return(dcore(Res,Def,DPCURVE));
}
double dtangcurve(TDef *Res,TDef *Def) {

   return(dcore(Res,Def,DTCURVE));
}

double darea(TDef *Res,TDef *Def,int Mode) {

   unsigned long idx;
   unsigned int  nid;
   float        *a=NULL;
   TGeoRef      *gref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: gref=GBand->GRef; break;
      case T_FLD : gref=GField->GRef; break;
   }

   if (!gref) 
      return(0.0);

   if ((a=(float*)malloc(FSIZE2D(Def)*sizeof(float)))) {
      // Force master grid for U grids as vexpr process the whole grid
      nid=gref->NId;
      gref->NId=0;
      GeoRef_CellDims(gref,FALSE,NULL,NULL,a);
      gref->NId=nid;
      
#pragma omp parallel for \
      private( idx ) \
      shared( Def,Res,a ) \
      schedule(static)
      for(idx=0;idx<FSIZE2D(Def);idx++) {
         Def_Set(Res,0,idx,a[idx]);
      }
   } else {
      return(0.0);
   }  
   return(1.0);
}

double dcoriolis(TDef *Res,TDef *Def,int Mode) {
 
   unsigned long i,j,idx;
   double        lat,lon,cor,omega=7.292e-5;
   TGeoRef      *gref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: gref=GBand->GRef; break;
      case T_FLD : gref=GField->GRef; break;
   }

   if (!gref) 
      return(0.0);

#pragma omp parallel for \
   private( j,i,lat,lon,idx,cor ) \
   shared( gref,Def,Res ) \
   schedule(static)
   for(j=0;j<Def->NJ;j++) {
      idx=j*Def->NI;
      for(i=0;i<Def->NI;i++) {
         gref->Project(gref,i,j,&lat,&lon,0,1);
         cor = 2.0*omega*sin(DEG2RAD(lat));
         Def_Set(Res,0,idx+i,cor);
      }
   }
   
   return(1.0);
}

double dangle(TDef *Res,TDef *Def,int Mode) {

   unsigned long i,j,idx;
   double        d,lat[2],lon[2];
   TGeoRef      *gref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: gref=GBand->GRef; break;
      case T_FLD : gref=GField->GRef; break;
   }

   if (!gref) {
      return(0.0);
   }

#pragma omp parallel for \
      private( j,i,idx,lat,lon,d ) \
      shared( gref,Def,Res ) \
      schedule(static)
   for(j=0;j<Def->NJ;j++) {
      idx=j*Def->NI;
      for(i=0;i<Def->NI;i++) {
         gref->Project(gref,i,j-0.5,&lat[0],&lon[0],1,1);
         gref->Project(gref,i,j+0.5,&lat[1],&lon[1],1,1);
         lat[0]=DEG2RAD(lat[0]); lon[0]=DEG2RAD(lon[0]);
         lat[1]=DEG2RAD(lat[1]); lon[1]=DEG2RAD(lon[1]);

         d=COURSE(lat[0],lon[0],lat[1],lon[1]);
         d=RAD2DEG(d);
         Def_Set(Res,0,idx+i,d);
      }
   }
   return(1.0);
}

double dlat(TDef *Res,TDef *Def,int Mode) {

   unsigned long i,j,idx;
   double        lat,lon;
   TGeoRef      *gref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: gref=GBand->GRef; break;
      case T_FLD : gref=GField->GRef; break;
   }

   if (!gref) {
      return(0.0);
   }

#pragma omp parallel for \
      private( j,i,idx,lat,lon ) \
      shared( gref,Def,Res ) \
      schedule(static)
   for(j=0;j<Def->NJ;j++) {
      idx=j*Def->NI;
      for(i=0;i<Def->NI;i++) {
         gref->Project(gref,i,j,&lat,&lon,0,1);
         Def_Set(Res,0,idx+i,lat);
      }
   }
   return(1.0);
}

double dlon(TDef *Res,TDef *Def,int Mode) {

   unsigned long i,j,idx;
   double        lat,lon;
   TGeoRef      *gref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: gref=GBand->GRef; break;
      case T_FLD : gref=GField->GRef; break;
   }

   if (!gref) {
      return(0.0);
   }

#pragma omp parallel for \
      private( j,i,idx,lat,lon ) \
      shared( gref,Def,Res ) \
      schedule(static)
   for(j=0;j<Def->NJ;j++) {
      idx=j*Def->NI;
      for(i=0;i<Def->NI;i++) {
         gref->Project(gref,i,j,&lat,&lon,0,1);
         Def_Set(Res,0,idx+i,lon);
      }
   }
   return(1.0);
}

double ddx(TDef *Res,TDef *Def,int Mode) {

   unsigned long i,j,k,idx,sk;
   double        d;
   TGeoRef      *gref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: gref=GBand->GRef; break;
      case T_FLD : gref=GField->GRef; break;
   }

   if (!gref) {
      return(0.0);
   }

#pragma omp parallel for \
   private(j,i,idx,d,sk,k) \
   shared(gref,Def,Res) \
   schedule(static)
   for(j=0;j<Def->NJ;j++) {
      idx=j*Def->NI;
      for(i=0;i<Def->NI;i++) {
         d=gref->Distance(gref,i-0.5,j,i+0.5,j);
         for(k=0;k<Def->NK;k++) {
            sk=FSIZE2D(Def)*k;
            Def_Set(Res,0,sk+idx+i,d);
         }
      }
   }
   return(1.0);
}

double ddy(TDef *Res,TDef *Def,int Mode) {

   unsigned long i,j,k,idx,sk;
   double        d;
   TGeoRef      *gref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: gref=GBand->GRef; break;
      case T_FLD : gref=GField->GRef; break;
   }

   if (!gref) {
      return(0.0);
   }

#pragma omp parallel for \
   private( sk,j,i,idx,d ) \
   shared( gref,Def,Res ) \
   schedule(static)
   for(j=0;j<Def->NJ;j++) {
      idx=j*Def->NI;
      for(i=0;i<Def->NI;i++) {
         d=gref->Distance(gref,i,j-0.5,i,j+0.5);
         for(k=0;k<Def->NK;k++) {
            sk=FSIZE2D(Def)*k;
            Def_Set(Res,0,sk+idx+i,d);
         }
      }
   }
   return(1.0);
}

/* Derived form GRASS */
double dcore(TDef *Res,TDef *Def,int Mode) {

   unsigned long i,j,idx,d;
   double  b[9]={0.0};
   double  mx,my,dx,dy,dxy,dx2,dy2,dxy2,slp100,slpdeg,asp,s3,s4,s5,s6,dvx,dvy,dvxy,dvxy2,norm,pcurv,tcurv;
   TGeoRef *gref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   switch (GMode) {
      case T_BAND: gref=GBand->GRef; break;
      case T_FLD : gref=GField->GRef; break;
   }

   if (!gref) {
      return(0.0);
   }

   /*check for wrap around*/
   d=gref->Type&GRID_WRAP?0:1;

#pragma omp parallel for default(none) \
      firstprivate(b) \
      private( j,i,idx,mx,my,dx,dy,dxy,dx2,dy2,dxy2,slp100,slpdeg,asp,s3,s4,s5,s6,dvx,dvy,dvxy,dvxy2,norm,pcurv,tcurv ) \
      shared( gref,Def,Res,d,Mode ) \
      schedule(static)
   for(j=1;j<Def->NJ-1;j++) {
      for(i=d;i<Def->NI-d;i++) {
         idx=j*Def->NI+i;

         /*Get the data bloc
           b0  b1  b2
           b3  b4  b5
           b6  b7  b8
        */
         if (i==0 || i==1) {
            if (i==0) {
                Def_Get(Def,0,idx-1,b[0]);
                Def_Get(Def,0,idx-1+Def->NI,b[3]);
                Def_Get(Def,0,idx-1+Def->NI+Def->NI,b[6]);
            } else {
                Def_Get(Def,0,idx-1-Def->NI,b[0]);
                Def_Get(Def,0,idx-1,b[3]);
                Def_Get(Def,0,idx-1+Def->NI,b[6]);
            }
            Def_Get(Def,0,idx-Def->NI,b[1]);
            Def_Get(Def,0,idx,b[4]);
            Def_Get(Def,0,idx+Def->NI,b[7]);
         } else {
            b[0]=b[1];b[1]=b[2];
            b[3]=b[4];b[4]=b[5];
            b[6]=b[7];b[7]=b[8];
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
         mx=gref->Distance(gref,i-1,j,i+1,j)*4;
         my=gref->Distance(gref,i,j-1,i,j+1)*4;

         /*Slope*/
         dx=((b[2]+b[5]+b[5]+b[8])-(b[0]+b[3]+b[3]+b[6]))/mx;
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
            asp=(dxy2==0.0?0.0:RAD2DEG(atan2(dy,-dx))+90.0);
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
static inline double ecdf(double val, double* vals, unsigned int* cums, unsigned long n) {
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

void stat_core(TDef *MA,TDef *MB) {

   double va,vb,t,nblok=0;
   double ratio,sum,dif,adif;
   unsigned long i,n,nx=0,ny=0,d=0,gi,gj;
   double *vx,*vy;
   unsigned int *cx,*cy;
   TGeoRef *gref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   Vcorr=Vnb=Vsumx=Vsumy=Vavgx=Vavgy=Vssx=Vssy=Vssxy=Vrmse=Vmb=Vnmb=Vnme=Vvarx=Vvary=Vcovar=Vme=Vmnb=Vmne=Vmfb=Vmfe=Vlmnb=Vlmne=Vnrmse=Vna=Vrna=Vmre=va=vb=0.0;
   Vmse=Vnmse=Vgmb=Vgmv=Vfoex=Vfa2=Vfa5=Vfa10=Vfb=Vnad=Vfms=Vfmsb=Vfmsi=Vosf=Vosfb=Vosfi=Vksp=Vrank=Vnbeq=Vnbgt=Vnblt=Vnbfa=Vnbmi=Vnbnp=Vaov=Vafn=Vafp=Vax=Vay=0.0;
   Vminy=Vminx=HUGE_VAL;
   Vmaxy=Vmaxx=-HUGE_VAL;
   Vmaxe=Vmaxb=Vmaxre=-HUGE_VAL;
   Vmedx=Vmedy=nan("NaN");

   if ((n=FSIZE3D(MA))==0) {
      return;
   }

   switch (GMode) {
      case T_BAND: gref=GBand->GRef; break;
      case T_FLD : gref=GField->GRef; break;
   }

   // Init fields that will be used later on for ksp

   if (MA && MB) {
      vx = malloc(n*sizeof(double));
      vy = malloc(n*sizeof(double));
      cx = malloc(n*sizeof(unsigned int));
      cy = malloc(n*sizeof(unsigned int));

      if (!vx || !vy || !cx || !cy) {
         App_Log(ERROR,"%s: Could not allocate memory\n",__func__);
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
         if (!DEFVALID(MA,va)) {
            Vna++;
            continue;
         }
      }

      if (MB) {
         Def_Get(MB,0,i,vb);
         if (!DEFVALID(MB,vb)) {
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

         if (gref && (va!=0.0 || vb!=0.0)) {
            gi=i%MA->NI;
            gj=i/MA->NI;
            t=gref->Distance(gref,gi-0.5,gj,gi+0.5,gj) * gref->Distance(gref,gi,gj-0.5,gi,gj+0.5);
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
      Vmse=Vnmse=Vgmb=Vgmv=Vfoex=Vfa2=Vfa5=Vfa10=Vfb=Vnad=Vfms=Vfmsb=Vfmsi=Vosf=Vosfb=Vosfi=Vksp=Vrank=Vnbeq=Vnbgt=Vnblt=Vnbfa=Vnbmi=Vnbnp=Vaov=Vafn=Vafp=Vax=Vay=0.0;

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

   Vfmsn=Vsumx==0.0?0.0:100.0*0.5*(1.0+(Vsumy-Vme)/Vsumx);

   Vafn  = Vax-Vaov;
   Vafp  = Vay-Vaov;

   Vmb   = Vsumy-Vsumx;
   Vnmb  = Vsumx!=0.0?Vmb/Vsumx*100:0;
   Vmb   /= Vnb;

   Vnad  = (Vsumx+Vsumy==0.0)?0.0:Vme/(Vsumx+Vsumy);
   Vnme  = Vsumx!=0.0?Vme/Vsumx*100.0:0.0;
   Vme   /= Vnb;

   Vavgx = Vsumx/Vnb;
   Vavgy = Vsumy/Vnb;

   Vvarx    = fmax(Vssx/Vnb-Vavgx*Vavgx,0.0);
   Vvary    = fmax(Vssy/Vnb-Vavgy*Vavgy,0.0);
   Vcovar   = Vssxy/Vnb-Vavgx*Vavgy;

   Vcorr=(Vvarx==0.0 || Vvary==0.0)?0.0:Vcovar/(sqrt(Vvarx*Vvary));

   Vmse=(Vssy-Vssxy*2.0+Vssx)/Vnb;
   Vnmse=Vmse/(Vavgx*Vavgy);
   Vrmse=sqrt(Vmse);
   Vnrmse=Vrmse/Vavgx;

   Vregb=Vcovar/Vvarx;
   Vrega=Vavgy-Vregb*Vavgx;
   Verrb=sqrt((Vnb*Vssy-Vsumy*Vsumy-Vregb*Vregb*(Vnb*Vssx-Vsumx*Vsumx))/((Vnb-2.0)*(Vnb*Vssx-Vsumx*Vsumx)));
   Verra=Verrb*sqrt(Vssx/Vnb);

   Vmre/=Vnb;
   Vmne/=Vnb;
   Vmnb/=Vnb;
   Vmfb=2.0*Vmfb/Vnb*100.0;
   Vmfe=2.0*Vmfe/Vnb*100.0;
   Vlmnb=exp(Vlmnb/Vnb)-1.0;
   Vlmne=exp(Vlmne/Vnb)-1.0;

   t=nblok+Vnbnp;
   Vgmb=t?exp(Vgmb/t):0;
   Vgmv=t?exp(Vgmv/t):0;

   Vfoex=((Vnbgt+Vnbeq/2.0)/Vnb-0.5)*100.0;
   Vfa2=((Vfa2+Vnbnp)/Vnb)*100.0;
   Vfa5=((Vfa5+Vnbnp)/Vnb)*100.0;
   Vfa10=((Vfa10+Vnbnp)/Vnb)*100.0;
   Vfb=(Vavgy==0 || Vavgx==0)?0:2.0*(Vavgy-Vavgx)/(Vavgy+Vavgx);
   Vfms=Vfms!=0?Vaov/Vfms*100.0:0;
   Vfmsb=(Vnb-Vnbmi-Vnbfa-Vnbnp)/(Vnb-Vnbnp)*100.0;
   Vfmsi=(1.0-Vnad)/(1.0+Vnad)*100.0;
   Vosf=(Vax==0 || Vay==0)?0.0:sqrt(pow(Vafn/Vax,2)+pow(Vafp/Vay,2));
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

double stat_all(TDef *MA,TDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vnb);
}

double stat_mse(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmse);
}

double stat_nmse(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnmse);
}

double stat_gmb(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vgmb);
}

double stat_gmv(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vgmv);
}

double stat_foex(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfoex);
}

double stat_fa2(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfa2);
}

double stat_fa5(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfa5);
}

double stat_fa10(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfa10);
}

double stat_fb(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfb);
}

double stat_nad(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnad);
}

double stat_fms(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfms);
}

double stat_fmsb(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfmsb);
}

double stat_fmsi(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfmsi);
}

double stat_fmsn(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vfmsn);
}

double stat_osf(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vosf);
}

double stat_osfb(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vosfb);
}

double stat_osfi(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vosfi);
}

double stat_ksp(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vksp);
}

double stat_rank(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vrank);
}

double stat_nbeq(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnbeq);
}

double stat_nbgt(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnbgt);
}

double stat_nblt(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnblt);
}

double stat_nbfa(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnbfa);
}

double stat_nbmi(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnbmi);
}

double stat_nbnp(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnbnp);
}

double stat_aov(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vaov);
}

double stat_afn(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vafn);
}

double stat_afp(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vafp);
}

double stat_ax(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vax);
}

double stat_ay(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vay);
}

double stat_na(TDef *MA,TDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vna);
}

double stat_rna(TDef *MA,TDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vrna);
}

double stat_mb(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmb);
}

double stat_me(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vme);
}

double stat_nmb(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnmb);
}

double stat_nme(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnme);
}

double stat_mnb(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmnb);
}

double stat_mne(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmne);
}

double stat_lmne(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vlmne);
}

double stat_lmnb(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vlmnb);
}

double stat_mfb(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmfb);
}

double stat_mfe(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmfe);
}

double stat_mre(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmre);
}

double stat_maxb(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmaxb);
}

double stat_maxe(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmaxe);
}

double stat_maxre(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmaxre);
}

double stat_rmse(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vrmse);
}

double stat_nrmse(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vnrmse);
}

double stat_corr(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vcorr);
}

double stat_covar(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vcovar);
}

double stat_varx(TDef *MA,TDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vvarx);
}

double stat_vary(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vvary);
}

double stat_sumx(TDef *MA,TDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vsumx);
}

double stat_sumy(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vsumy);
}

double stat_avgx(TDef *MA,TDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vavgx);
}

double stat_avgy(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vavgy);
}

double stat_minx(TDef *MA,TDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vminx);
}

double stat_miny(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vminy);
}

double stat_maxx(TDef *MA,TDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vmaxx);
}

double stat_maxy(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmaxy);
}

double stat_regb(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vregb);
}

double stat_rega(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vrega);
}

double stat_erra(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Verra);
}

double stat_errb(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Verrb);
}

double stat_ssx(TDef *MA,TDef *MB) {
   if (MA)
      stat_core(MA,MB);
   return(Vssx);
}

double stat_ssy(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vssy);
}

double stat_ssxy(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vssxy);
}

double stat_sdevx(TDef *MA,TDef *MB) {
   if (MA)
      stat_core(MA,MB);

   return(sqrt(Vvarx));
}

double stat_sdevy(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);

   return(sqrt(Vvary));
}

double stat_medx(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmedx);
}

double stat_medy(TDef *MA,TDef *MB) {
   if (MA&&MB)
      stat_core(MA,MB);
   return(Vmedy);
}

double stat_nb(TDef *MA) {
   if (MA)
      stat_core(MA,NULL);
   return(Vnb);
}

double stat_sdev(TDef *MA) {
   if (MA)
      stat_core(MA,NULL);

   return(sqrt(Vvarx));
}

double stat_var(TDef *MA) {
   if (MA)
      stat_core(MA,NULL);
   return(Vvarx);
}

double stat_med(TDef *M) {

   int     n,nb,t=0;
   double *v,med=0;

   nb=FSIZE3D(M);
   if ((v=(double*)malloc(nb*sizeof(double)))) {
      for(n=0;n<nb;n++) {
         Def_Get(M,0,n,v[t]);
         if (DEFVALID(M,v[t]) )
            ++t;
      }

      qsort(v,t,sizeof(double),QSort_Double);

      n=t>>1;
      med=(t&1)?v[n]:(v[n-1]+v[n])*0.5;

      free(v);
   }

   return(med);
}

double stat_unique(TDef *M) {

   int     n,nb,uniq=0,t=0;
   double *v;

   nb=FSIZE3D(M);
   if ((v=(double*)malloc(nb*sizeof(double)))) {
      for(n=0;n<nb;n++) {
         Def_Get(M,0,n,v[t]);
         if (DEFVALID(M,v[t]))
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

double stat_sum(TDef *M) {

   double        sum=0,v=0;
   unsigned long i;

   for(i=0;i<FSIZE3D(M);i++) {
      Def_Get(M,0,i,v);
      if (DEFVALID(M,v))
         sum+=v;
   }
   return sum;
}

double stat_min(TDef *M) {

   double        min=HUGE_VAL,v=0;
   unsigned long i;

   for(i=0;i<FSIZE3D(M);i++) {
      Def_Get(M,0,i,v);
      if (DEFVALID(M,v) && v<min )
         min=v;
   }
   return min;
}

double stat_max(TDef *M) {

   double        max=-HUGE_VAL,v=0;
   unsigned long i;

   for(i=0;i<FSIZE3D(M);i++) {
      Def_Get(M,0,i,v);
      if (DEFVALID(M,v) && v>max)
         max=v;
   }
   return max;
}

double stat_avg(TDef *M) {

   double        sum=0,v=0;
   unsigned long i,n=0;

   for(i=0;i<FSIZE3D(M);i++) {
      Def_Get(M,0,i,v);
      if (DEFVALID(M,v)) {
         sum+=v;
         ++n;
      }
   }
   return sum/n;
}

double initrand(TDef *M) {
   unsigned int seed;

   if( M ) {
      Def_Get(M,0,0,seed);
   } else {
      seed = time(NULL);
   }

   srand(seed);
   return(seed);
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

static void ddxy(TDef *Fld,double **restrict DX,double **restrict DY) {
   unsigned long i,j;
   double        *dx,*dy;
   TGeoRef      *gref=NULL;

   extern TData     *GField;
   extern GDAL_Band *GBand;
   extern int        GMode;

   *DX = *DY = NULL;

   switch (GMode) {
      case T_BAND: gref=GBand->GRef; break;
      case T_FLD : gref=GField->GRef; break;
   }

   if (!gref) {
      return;
   }

   dx = malloc(Fld->NI*sizeof(*dx));
   dy = malloc(Fld->NJ*sizeof(*dy));
   if( !dx || !dy ) {
      Calc_RaiseError("ddxy: Could not allocate memory\n");
      free(dx);
      free(dy);
      return;
   }

   // Note: We don't calculate the distance between [0] and [j] directly to reduce the chance of lat/lon wrapping occuring
   dy[0] = 0.0;
   for(j=1,i=Fld->NI>>1; j<Fld->NJ; ++j) {
      dy[j] = dy[j-1] + gref->Distance(gref,i,j-1,i,j);
   }

   // Note: We don't calculate the distance between [0] and [i] directly to reduce the chance of lon wrapping occuring
   dx[0] = 0.0;
   for(i=1,j=Fld->NJ>>1; i<Fld->NI; ++i) {
      dx[i] = dx[i-1] + gref->Distance(gref,i-1,j,i,j);
   }

   *DX = dx;
   *DY = dy;
}

static int* toint(TDef *Fld) {
   int      *a;
   size_t   idx,n=FSIZE2D(Fld);
   double   v;

   // Only allocate memory if we can't coerce the field into an int field (size mismatch)
   // Note that even though IEEE754 floats agree that 0 is all zeroes, negative 0 also exists and needs to be covered
   if( Fld->Type!=TD_Int32 && Fld->Type!=TD_UInt32 ) {
      if( !(a=malloc(n*sizeof(*a))) ) {
         Calc_RaiseError("toint: Could not allocate memory\n");
         return NULL;
      }

      // Get the source field into a boolean field
      for(idx=0; idx<n; ++idx) {
         Def_Get(Fld,0,idx,v);
         a[idx] = v!=0.0;
      }

      return a;
   } else {
      return (int*)Fld->Data[0];
   }
}

double dt(TDef *Res,TDef *Fld) {
#ifdef HAVE_DISTANCEMETRICS
   double n=-1.0,*dx,*dy;
   int *a;

   if( Res->Type!=TD_Float64 )   Calc_RaiseError("dt: Resulting field has to be Float64. This might happen if the data provided is from an unsupported type.\n");
   if( Fld->NK!=1 )              Calc_RaiseError("dt: Field's NK should be 1\n");

   if( Calc_InError() ) return -1.0;

   // Get the relative distance of points w.r.t eachother
   ddxy(Fld,&dx,&dy);

   // Make sure we have int-compatible values
   a = toint(Fld);

   if( Calc_InError() ) {
      goto end;
   }

   if( !(n=DM_DT_Meijster(a,dx,dy,Fld->NI,Fld->NJ,(double*)Res->Data[0],DM_DT_EDT)) ) {
      Calc_RaiseError("dt: Could not calculate any distance as there is no non-null values in the field\n");
   }

end:
   free(dx);
   free(dy);
   if(a!=(int*)Fld->Data[0])
      free(a);

   return n;
#else // HAVE_DISTANCEMETRICS
    Calc_RaiseError("dt: vexpr was not compiled with DistanceMetrics support, this function is unavailable\n");
    return -1.0;
#endif // HAVE_DISTANCEMETRICS
}

double gdt(TDef *Res,TDef *Fld,TDef *Q) {
#ifdef HAVE_DISTANCEMETRICS
   double   *dx,*dy;
   int      *a;
   double   n=-1.0,q=-2.0;

   // Error check
   if( Res->Type!=TD_Float64 )   Calc_RaiseError("gdt: Resulting field has to be Float64. This might happen if the data provided is from an unsupported type.\n");
   if( Q && FSIZE3D(Q)!=1 )      Calc_RaiseError("gdt: Q should be a scalar\n");
   if( Fld->NK!=1 )              Calc_RaiseError("gdt: Field's NK should be 1\n");

   if( Calc_InError() ) return -1.0;

   // Get the relative distance of points w.r.t eachother
   ddxy(Fld,&dx,&dy);

   // Make sure we have int-compatible values
   a = toint(Fld);

   if( Calc_InError() ) {
      goto end;
   }

   /// Default value for Q
   if( Q ) Def_Get(Q,0,0,q);

   if( !(n=DM_GDT(a,dx,dy,Fld->NI,Fld->NJ,(double*)Res->Data[0],q)) ) {
      Calc_RaiseError("gdt: Could not calculate any distance as there is no non-null values in the field\n");
   }

end:
   free(dx);
   free(dy);
   if(a!=(int*)Fld->Data[0])
      free(a);

   return n;
#else // HAVE_DISTANCEMETRICS
    Calc_RaiseError("gdt: vexpr was not compiled with DistanceMetrics support, this function is unavailable\n");
    return -1.0;
#endif // HAVE_DISTANCEMETRICS
}

double dmnp(TDef *N) {
#ifdef HAVE_DISTANCEMETRICS
#ifdef _OPENMP
   double n;

   // Error check
   if( FSIZE3D(N)!=1 )     Calc_RaiseError("dmnp: The number of threads should be a scalar\n");

   if( Calc_InError() )
      return -1.0;

   Def_Get(N,0,0,n);
   DM_SetNumThreads((int)n);

   return n;
#else //_OPENMP
    Calc_RaiseError("dmnp: vexpr was not compiled with thread support, this function is unavailable\n");
    return -1.0;
#endif //_OPENMP
#else // HAVE_DISTANCEMETRICS
    Calc_RaiseError("dmnp: vexpr was not compiled with DistanceMetrics support, this function is unavailable\n");
    return -1.0;
#endif // HAVE_DISTANCEMETRICS
}

double hausdorff(TDef *A,TDef *B,TDef *Algo) {
#ifdef HAVE_DISTANCEMETRICS
   double   *dx=NULL,*dy=NULL;
   int      *a=NULL,*b=NULL;
   double   r=-1.0;
   int      algo=0;

   if( Algo && FSIZE3D(Algo)!=1 )      Calc_RaiseError("hausdorff: Algo should be a scalar\n");
   if( A->NK!=1 || B->NK!=1 )          Calc_RaiseError("hausdorff: NK should be 1 for both fields\n");
   if( A->NI!=B->NI || A->NJ!=B->NJ )  Calc_RaiseError("hausdorff: Both fields should have the same dimensions\n");

   if( Calc_InError() ) return -1.0;

   // Get the relative distance of points w.r.t eachother
   ddxy(A,&dx,&dy);

   // Make sure we have int-compatible values
   a = toint(A);
   b = toint(B);

   if( Calc_InError() ) goto end;

   // Apply parameters
   if( Algo ) Def_Get(Algo,0,0,algo);

   // Make the actual calculations
   switch( algo ) {
      case 0:  r=DM_Hausdorff_optimized(a,b,dx,dy,A->NI,B->NJ);  break;
      case 1:  r=DM_Hausdorff_DT(a,b,dx,dy,A->NI,B->NJ);  break;
      case 2:  r=DM_Hausdorff_naive(a,b,dx,dy,A->NI,B->NJ);  break;
      default: Calc_RaiseError("hausdorff: Invalid algorithm selected. Can be 0 for 'optimized' (default), 1 for 'DT' or 2 for 'naive'.\n"); goto end;
   }

   if( r < 0.0 ) {
      Calc_RaiseError("hausdorff: Could not calculate any distance as there is no non-null values in at least one of the fields\n");
   }

end:
   free(dx);
   free(dy);
   if(a!=(int*)A->Data[0])
      free(a);
   if(b!=(int*)B->Data[0])
      free(b);

   return r;
#else // HAVE_DISTANCEMETRICS
    Calc_RaiseError("hausdorff: vexpr was not compiled with DistanceMetrics support, this function is unavailable\n");
    return -1.0;
#endif // HAVE_DISTANCEMETRICS
}

double baddeley(TDef *A,TDef *B,TDef *P) {
#ifdef HAVE_DISTANCEMETRICS
   double   *dx=NULL,*dy=NULL;
   int      *a=NULL,*b=NULL;
   double   p=2.0,r=-1.0;

   // Error check
   if( P && FSIZE3D(P)!=1 )            Calc_RaiseError("baddeley: P should be a scalar\n");
   if( A->NK!=1 || B->NK!=1 )          Calc_RaiseError("baddeley: NK should be 1 for both fields\n");
   if( A->NI!=B->NI || A->NJ!=B->NJ )  Calc_RaiseError("baddeley: Both fields should have the same dimensions\n");

   if( Calc_InError() ) return -1.0;

   // Get the relative distance of points w.r.t eachother
   ddxy(A,&dx,&dy);

   // Make sure we have int-compatible values
   a = toint(A);
   b = toint(B);

   if( Calc_InError() ) goto end;

   // Apply parameters
   if( P ) Def_Get(P,0,0,p);

   // Make the actual calculations
   if( (r=DM_Baddeley(a,b,dx,dy,A->NI,B->NJ,p)) == -DBL_MAX ) {
      Calc_RaiseError("baddeley: Could not calculate any distance as there is no non-null values in at least one of the fields\n");
   }

end:
   free(dx);
   free(dy);
   if(a!=(int*)A->Data[0])
      free(a);
   if(b!=(int*)B->Data[0])
      free(b);

   return r;
#else // HAVE_DISTANCEMETRICS
    Calc_RaiseError("baddeley: vexpr was not compiled with DistanceMetrics support, this function is unavailable\n");
    return -1.0;
#endif // HAVE_DISTANCEMETRICS
}

double gdm(TDef *A,TDef *B,TDef *P,TDef *Q,TDef *Algo,TDef *BS,TDef *C) {
#ifdef HAVE_DISTANCEMETRICS
   double   *dx=NULL,*dy=NULL;
   int      *a=NULL,*b=NULL;
   int      algo=0,bs=100;
   double   p=2.0,q=-2.0,c=0.0,r=-1.0;

   // Error check
   if( P && FSIZE3D(P)!=1 )            Calc_RaiseError("gdm: P should be a scalar\n");
   if( Q && FSIZE3D(Q)!=1 )            Calc_RaiseError("gdm: Q should be a scalar\n");
   if( Algo && FSIZE3D(Algo)!=1 )      Calc_RaiseError("gdm: Algo should be a scalar\n");
   if( BS && FSIZE3D(BS)!=1 )          Calc_RaiseError("gdm: BS should be a scalar\n");
   if( C && FSIZE3D(C)!=1 )            Calc_RaiseError("gdm: C should be a scalar\n");
   if( A->NK!=1 || B->NK!=1 )          Calc_RaiseError("gdm: NK should be 1 for both fields\n");
   if( A->NI!=B->NI || A->NJ!=B->NJ )  Calc_RaiseError("gdm: Both fields should have the same dimensions\n");

   if( Calc_InError() ) return -1.0;

   // Get the relative distance of points w.r.t eachother
   ddxy(A,&dx,&dy);

   // Make sure we have int-compatible values
   a = toint(A);
   b = toint(B);

   if( Calc_InError() ) goto end;

   // Apply parameters
   if( P )     Def_Get(P,0,0,p);
   if( Q )     Def_Get(Q,0,0,q);
   if( Algo )  Def_Get(Algo,0,0,algo);
   if( BS )    Def_Get(BS,0,0,bs);
   if( C )     Def_Get(C,0,0,c);

   // Make the actual calculations
   switch( algo ) {
      case 0:  r=DM_GDM(a,b,dx,dy,A->NI,B->NJ,p,q);         break;
      case 1:  r=DM_GDMaf(a,b,dx,dy,A->NI,B->NJ,p,q,bs,c);  break;
      case 2:  r=DM_GDMak(a,b,dx,dy,A->NI,B->NJ,p,q,bs,c);  break;
      default: Calc_RaiseError("gdm: Invalid algorithm selected. Can be 0 for 'exact' (default), 1 for 'approx fast' or 2 for 'approx kernel'.\n"); goto end;
   }

   if( r == -DBL_MAX ) {
      Calc_RaiseError("gdm: Could not calculate any distance as there is no non-null values in at least one of the fields\n");
   }

end:
   free(dx);
   free(dy);
   if(a!=(int*)A->Data[0])
      free(a);
   if(b!=(int*)B->Data[0])
      free(b);

   return r;
#else // HAVE_DISTANCEMETRICS
    Calc_RaiseError("gdm: vexpr was not compiled with DistanceMetrics support, this function is unavailable\n");
    return -1.0;
#endif // HAVE_DISTANCEMETRICS
}
