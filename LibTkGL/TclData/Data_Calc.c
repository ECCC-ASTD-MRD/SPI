/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de manipulation de type de donnees.
 * Fichier   : Data_Calc.c
 * Creation  : Aout 2000 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Fonctions de base de la calculatrice de donnees.
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

#include <assert.h>
#include <math.h>
#include <fenv.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <tcl.h>

#include "Data_Calc.h"
#include "Data_Funcs.h"

Tcl_Interp   *GInterp;
TData_Type    GType;
TData        *GField,*GFieldP;
GDAL_Band    *GBand;
OGR_Layer    *GLayer;
TObs         *GObs;
TVector      *GVec;
TDataDef     *GResult;
TDataDef     *GData[1024];
int           GDataN;
int           GMode;
char         *curPos;
int           GError;
int           stopGuard;
int           GExcept;

/**
 * @author Jean-Philippe Gauthier
 * @brief Updates a champ symbol with the new values
 * @param Name Grammar structure for champ
 * @param Field Values from the math side
 */
void Calc_Update(Tcl_Interp* Interp,char* Name,TDataDef* Data) {

   TData     *field;
   GDAL_Band *band;
   TObs      *obs;
   double     val;
   int        n,d,needcopy=1;

   if (!Data) {
      Tcl_AppendResult(Interp,"Calc_Update: Invalid field",(char*)NULL);
      return;
   }

   n=FSIZE3D(Data);

   if (n==1) {
      if (Calc_Validate(Interp)) {
         Def_Get(Data,0,0,val);
         if (Data->Type<10) {
            Tcl_SetObjResult(Interp,Tcl_NewLongObj(val));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(val));
         }
      }
      /*Free the DataDef since it won't be passed to the script side*/
      DataDef_Free(Data);
#ifdef DEBUG
      fprintf(stderr,"(DEBUG) Calc_Update: Result is float\n");
#endif
   } else {
      /*See if we need to copy the Data (when it's not resulted from computation)*/
      for(d=0;d<=GDataN;d++) {
         if (GData[d]==Data) {
             needcopy=0;
             break;
         }
      }

      switch(GMode) {
         case T_FLD:
           if (!(field=Data_Get(Name)) || field->Def!=Data) {
               GField=Data_Copy(Interp,GField,Name,0);

               if (GField->Stat) {
                  free(GField->Stat);
                  GField->Stat=NULL;
               }
               GField->Def= needcopy ? DataDef_Copy(Data) : Data;
            }
#ifdef DEBUG
            fprintf(stderr,"(DEBUG) Calc_Update: Result is field\n");
#endif
            break;

         case T_BAND:
            if (!(band=GDAL_BandGet(Name)) || band->Def!=Data) {
               GBand=GDAL_BandCopy(Interp,GBand,Name,0);
               if (GBand->Stat) {
                  free(GBand->Stat);
                  GBand->Stat=NULL;
               }
               GBand->Def= needcopy ? DataDef_Copy(Data) : Data;
            }
#ifdef DEBUG
            fprintf(stderr,"(DEBUG) Calc_Update: Result is band\n");
#endif
            break;

         case T_LAYER:
            if ((GLayer=OGR_LayerGet(Name))) {
               GLayer=OGR_LayerFromDef(GLayer,rindex(Name,'.')+1,Data);
            }
            /*For layers, we copy back the data so we don't need the Def anymore*/
            DataDef_Free(Data);
#ifdef DEBUG
            fprintf(stderr,"(DEBUG) Calc_Update: Result is layer\n");
#endif
            break;

         case T_OBS:
            if (!(obs=Obs_Get(Name)) || obs->Def!=Data) {
               GObs=Obs_Copy(Interp,GObs,Name,0);
               GObs->Def= needcopy ? DataDef_Copy(Data) : Data;
            }
#ifdef DEBUG
            fprintf(stderr,"(DEBUG) Calc_Update: Result is observation\n");
#endif
            break;

         case T_VEC:
            GVec=Vector_Copy(Interp,GVec,Name);
            memcpy(GVec->V,Data->Data[0],n*TData_Size[GVec->Def->Type]);
            /*For vectors, we copy back the data so we don't need the Def anymore*/
            DataDef_Free(Data);
#ifdef DEBUG
            fprintf(stderr,"(DEBUG) Calc_Update: Result is vector\n");
#endif
            break;
      }

      if (Calc_Validate(Interp)) {
         Tcl_SetObjResult(Interp,Tcl_NewStringObj(Name,-1));
      }
   }
}

/**
 * @author Jean-Philipe Gauthier
 * @brief Validate result values within the field by looking for inf and nan
 * @param Interp TCL interpreter
 * @return Boolean validity code
 */
int Calc_Validate(Tcl_Interp* Interp) {

   if (GExcept) {
      if (fetestexcept(FE_DIVBYZERO)) {
         Tcl_AppendResult(Interp,"infinity",(char*)NULL);
         return 0;
      } else if (fetestexcept(FE_OVERFLOW)) {
         Tcl_AppendResult(Interp,"overflow",(char*)NULL);
         return 0;
      } else if (fetestexcept(FE_UNDERFLOW)) {
         Tcl_AppendResult(Interp,"underflow",(char*)NULL);
         return 0;
      } else if (fetestexcept(FE_INVALID)) {
         Tcl_AppendResult(Interp,"not a number",(char*)NULL);
         return 0;
      };
   }
   return 1;
}

/**
 * @author Bison example
 * @brief Called by vexpr_parse on error
 * @param s (Sub-)String that caused the error
 * @return Ignored
 */
int vexpr_error(char *Error){

   GError=TCL_ERROR;
   Tcl_AppendResult(GInterp,"(Bison) Parser error: ",Error,(char*)NULL);

   return 0;
}

/**
 * @author Jean-Philippe Gauthier
 * @brief Entry point
 * @param Tcl_Interp Current TCL interpreter, used for error messages
 * @param Champ Name of the tclFSTD champ to update
 * @param Expr Expression to parse
 * @param Mode_ Parsing mode
 * @return Error code
 */
int Calc_Parse(Tcl_Interp* Interp,int Except,char* Data,TData_Type Type,char* Expr) {

   int  i;

   /* Set per thread data */
   curPos    = Expr;
   GInterp   = Interp;
   GError    = TCL_OK;
   GMode     = T_VAL;
   GDataN    = -1;
   stopGuard = 0;
   GField    = NULL;
   GFieldP   = NULL;
   GBand     = NULL;
   GObs      = NULL;
   GVec      = NULL;
   GLayer    = NULL;
   GResult   = NULL;
   GExcept   = Except;
   GType     = Type;

   /* Interpreter initialization */
   Tcl_ResetResult(Interp);

   /* Reset exception flags */
   if (GExcept) {
      feclearexcept(FE_ALL_EXCEPT);
   }
   /* Parse, return value in GResult */
   vexpr_parse();

#ifdef DEBUG
   fprintf(stderr,"(DEBUG) Calc_Parse(Data:%p %s,GResult:%p)\n",(void*)Data,Data,(void*)GResult);
#endif

   /* Update the upstream values */
   if (GError==TCL_OK)
      Calc_Update(Interp,Data,GResult);
#ifdef DEBUG
   fprintf(stderr,"(DEBUG) Calc_Parse: freeing temp data\n");
#endif

   /* Free temporary data */
   for(i=0;i<=GDataN;i++) {
      if (GData[i] && GData[i]!=GResult)
         DataDef_Free(GData[i]);
      GData[i]=NULL;
   }

   /* Free temporary field used in fld() func*/
   Data_FreeHash(Interp,"TMPCALCXXXXXX");

   /* Error code */
   return(GError);
}
