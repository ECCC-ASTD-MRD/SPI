/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de divers fichiers de donnees
 * Fichier      : tclData.c
 * Creation     : Fevrier 2003 - J.P. Gauthier
 *
 * Description  : Fonctions generales applicables a divers types de donnees.
 *
 * Remarques    :
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
 *=========================================================
 */

#include <stdlib.h>
#include "App.h"
#include "tclData.h"
#include "tclOGR.h"
#include "Projection.h"
#include "Data_Calc.h"
#include "Data_FF.h"

TCL_DECLARE_MUTEX(MUTEX_DATACALC)

static Tcl_HashTable TData_Table;
static int           TDataInit=0;

static TDataVector DataVectorTable[256];
static int         DataVectorTableSize=0;

static int Data_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);

TDataVector *Data_VectorTableCheck(char *Var,int *Idx) {

   register int i;

   for(i=0;i<DataVectorTableSize;i++) {
      if (DataVectorTable[i].UU && strcmp(Var,DataVectorTable[i].UU)==0) {
         if (Idx) *Idx=0;
         return(&DataVectorTable[i]);
      }
      if (DataVectorTable[i].VV && strcmp(Var,DataVectorTable[i].VV)==0) {
         if (Idx) *Idx=1;
         return(&DataVectorTable[i]);
      }
      if (DataVectorTable[i].WW && strcmp(Var,DataVectorTable[i].WW)==0) {
         if (Idx) *Idx=2;
         return(&DataVectorTable[i]);
      }
   }
   return(NULL);
}

TDataVector *Data_VectorTableAdd(void) {
   return(&DataVectorTable[DataVectorTableSize++]);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Tcldata_Init>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Initialisation des commandes Tcl pour utilisation des divers types de donnees
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *
 * Retour       : Code de retour Tcl
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Tcldata_Init(Tcl_Interp *Interp) {

   if (Tcl_PkgProvide(Interp,"TclData",PACKAGE_VERSION) != TCL_OK) {
      return(TCL_ERROR);
   }

   // Initialisation du package viewport
   if (Projection_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   // Initialisation du package des cameras
   if (ProjCam_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   // Initialisation du package model
   if (Model_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   // Initialisation du package de palette de couleur
   if (TclCMap_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   // Initialisation du package de vecteur
   if (TclVector_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   // Initialisation du package de configuration des donnees
   if (TclDataSpec_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   // Initialisation du package metobs
   if (TclGeoRef_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   // Initialisation du package d'observations
   if (TclObs_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   // Initialisation du package trajectoires
   if (TclTraj_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

#ifdef HAVE_RMN
   // Initialisation du package de fichier standard
   if (TclFSTD_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);
#endif

#ifdef HAVE_ECCODES
   // Initialisation du package de fichier standard
   if (TclGRIB_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);
#endif

#ifdef HAVE_GDAL
   // Initialisation du package GDAL
   if (TclGDAL_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   // Initialisation du package OGR
   if (TclOGR_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);
#endif
   
   // Initialisation du package radar
#ifdef HAVE_URP
   if (TclRadar_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);
#endif

#ifdef HAVE_ECBUFR
   // Initialisation du package metobs
   if (TclMetObs_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   if (TclMetModel_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);
#endif

   Tcl_CreateObjCommand(Interp,"geodata",Data_FieldCmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);
   Tcl_CreateObjCommand(Interp,"vexpr",Data_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   if (!TDataInit++) {
      Tcl_InitHashTable(&TData_Table,TCL_STRING_KEYS);

      // Force UU-VV relashionship
      DataVectorTable[0].UU=strdup("UU");
      DataVectorTable[0].VV=strdup("VV");
      DataVectorTable[0].WW=NULL;
      DataVectorTableSize++;
   }

   // Check the log parameters in the environment 
   App_LogLevel(getenv("APP_VERBOSE"));

   // if OMP_NUM_THREADS not defined, set it as 0 or single thread
   if (getenv("OMP_NUM_THREADS")==NULL) {
      omp_set_num_threads(0);
   } 

   return(TCL_OK);
}

int Tclgeoeer_Init(Tcl_Interp *Interp) {

   if (Tcl_PkgProvide(Interp,"TclGeoEER",PACKAGE_VERSION) != TCL_OK) {
      return(TCL_ERROR);
   }
   return(Tcldata_Init(Interp));
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_FieldCmd>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Appel des commandes relies aux champs TData.
 *
 * Parametres     :
 *  <clientData>  : Donnees du module.
 *  <Type>        : Type de donnee de de l'appel
 *  <Interp>      : Interpreteur TCL.
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_FieldCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[],TDataType Type){

   int          idx,n,bool=0;
   TData       *field0,*field1;
   TDataSpec   *spec;
   TDataVector *uvw;
   Tcl_Obj     *obj,*lst;
   double       nd;

   static CONST char *sopt[] = { "all","vector","alias","copy","copyhead","free","configure","define","stats","sort","clear","clean","wipe","is",NULL };
   enum                opt { ALL,VECTOR,ALIAS,COPY,COPYHEAD,FREE,CONFIGURE,DEFINE,STATS,SORT,CLEAR,CLEAN,WIPE,IS };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {

      case ALL:
         TclY_HashAll(Interp,&TData_Table);
         break;
         
      case VECTOR:
         if (Objc==2) {
            lst=Tcl_NewListObj(0,NULL);
            for(n=0;n<DataVectorTableSize;n++) {
               if (DataVectorTable[n].UU && DataVectorTable[n].VV) {
                  obj=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(DataVectorTable[n].UU,-1));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(DataVectorTable[n].VV,-1));
                  if (DataVectorTable[n].WW)       { Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(DataVectorTable[n].WW,-1)); }
                  if (DataVectorTable[n].WWFactor) { Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(DataVectorTable[n].WWFactor)); }
                  Tcl_ListObjAppendElement(Interp,lst,obj);
               }
            }
            Tcl_SetObjResult(Interp,lst); 
         } else if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"{ U [V] [W] [WFactor] }");
            return(TCL_ERROR);
         } else {
            Tcl_ListObjLength(Interp,Objv[2],&n);
            if (n) {
               Tcl_ListObjIndex(Interp,Objv[2],0,&obj);
               if (!(uvw=Data_VectorTableCheck(Tcl_GetString(obj),NULL))) {
                  uvw=Data_VectorTableAdd();
               }
               if (uvw->UU) free(uvw->UU);
               if (uvw->VV) free(uvw->VV);
               if (uvw->WW) free(uvw->WW);
               uvw->UU=strdup(Tcl_GetString(obj));
               uvw->VV=uvw->WW=NULL;
               uvw->WWFactor=0.0;
               Tcl_ListObjIndex(Interp,Objv[2],1,&obj);
               if (n>1) {
                  uvw->VV=strdup(Tcl_GetString(obj));
                  if (n>2) {
                     Tcl_ListObjIndex(Interp,Objv[2],2,&obj);
                     uvw->WW=strdup(Tcl_GetString(obj));
                     if (n>3) {
                        Tcl_ListObjIndex(Interp,Objv[2],3,&obj);
                        Tcl_GetDoubleFromObj(Interp,obj,&uvw->WWFactor);
                     }
                  }
               }
            }
         }
         break;
         
      case ALIAS:
         bool=1;
         
      case COPY:
         if (Objc!=4 && Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"idto idfrom [alias]");
            return(TCL_ERROR);
         }
         if (Objc==5) {
            Tcl_GetBooleanFromObj(Interp,Objv[4],&bool);
         }
         
         if (!Data_Copy(Interp,Data_Get(Tcl_GetString(Objv[3])),Tcl_GetString(Objv[2]),1,bool)) {
            return(TCL_ERROR);
         } else {
            return(TCL_OK);
         }
         break;

      case COPYHEAD:
         if (Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"idto idfrom");
            return(TCL_ERROR);
         }
         field0=Data_Get(Tcl_GetString(Objv[2]));
         if (!field0) {
            Tcl_AppendResult(Interp,"invalid field: ",Tcl_GetString(Objv[2]),(char*)NULL);
            return(TCL_ERROR);
         }
         field1=Data_Get(Tcl_GetString(Objv[3]));
         if (!field0) {
            Tcl_AppendResult(Interp,"invalid field: ",Tcl_GetString(Objv[3]),(char*)NULL);
            return(TCL_ERROR);
         }

         field1->Set(field0);
         field1->Copy(field0->Head,field1->Head);
         return(TCL_OK);

         break;

      case FREE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            Data_FreeHash(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case CONFIGURE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld ?option?");
            return(TCL_ERROR);
         }
         field0=Data_Get(Tcl_GetString(Objv[2]));
         if (!field0) {
            Tcl_AppendResult(Interp,"invalid field",(char*)NULL);
            return(TCL_ERROR);
         }

         Data_PreInit(field0);

         if (strcmp(Tcl_GetString(Objv[3]),"-dataspec")==0) {
            if (Objc==4) {
               if (field0->Spec) {
                  DataSpec_Incr(field0->Spec);
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(field0->Spec->Name,-1));
               }
            } else {
               if ((spec=DataSpec_Get(Tcl_GetString(Objv[4])))) {
                  if (field0->Spec) {
                     DataSpec_FreeHash(Interp,field0->Spec->Name);
                  }
                  DataSpec_Incr(spec);
                  field0->Spec=spec;
               } else {
                  Tcl_AppendResult(Interp,"Data_FieldCmd: invalid configuration object",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
         } else {
            if (DataSpec_Config(Interp,field0->Spec,Objc-3,Objv+3)==TCL_OK) {
               return(TCL_OK);
            } else {
               return(TCL_ERROR);
            }
         }
         break;

      case DEFINE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld ?option?");
            return(TCL_ERROR);
         }
         field0=Data_Get(Tcl_GetString(Objv[2]));
         if (!field0) {
            Tcl_AppendResult(Interp,"invalid field",(char*)NULL);
            return(TCL_ERROR);
         }

         return(field0->Define(Interp,field0,Objc-3,Objv+3));
         break;

      case STATS:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld ?option?");
            return(TCL_ERROR);
         }
         field0=Data_Get(Tcl_GetString(Objv[2]));
         if (!field0) {
            Tcl_AppendResult(Interp,"invalid field",(char*)NULL);
            return(TCL_ERROR);
         }
         return Data_Stat(Interp,field0,Objc-3,Objv+3);
         break;

      case SORT:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fldlist");
            return(TCL_ERROR);
         }
         return(Data_Sort(Interp,Objv[2]));
         break;

      case CLEAN:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld");
            return(TCL_ERROR);
         }
         field0=Data_Get(Tcl_GetString(Objv[2]));
         if (!field0) {
            Tcl_AppendResult(Interp,"invalid field",(char*)NULL);
            return(TCL_ERROR);
         }
         Data_Clean(field0,1,1,1);
         break;

      case CLEAR:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld [value]");
            return(TCL_ERROR);
         }
         field0=Data_Get(Tcl_GetString(Objv[2]));
         if (!field0) {
            Tcl_AppendResult(Interp,"invalid field",(char*)NULL);
            return(TCL_ERROR);
         }
         nd=field0->Def->NoData;

         if (Objc>3) {
            Tcl_GetDoubleFromObj(Interp,Objv[3],&field0->Def->NoData);
         }
         Def_Clear(field0->Def);
         field0->Def->NoData=nd;
         break;

      case IS:
         if(Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld");
            return(TCL_ERROR);
         }
         if (Objc>3) {
            Tcl_GetBooleanFromObj(Interp,Objv[3],&bool);
         }
         field0=Data_Get(Tcl_GetString(Objv[2]));
         if (field0 && (bool || field0->Type==Type)) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case WIPE:
         Data_Wipe();
         break;
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_FreeHash>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Libere la hashtable et l'espace memoire associee a un champ.
 *
 * Parametres :
 *  <Name>    : Identificateur du champs.
 *
 * Retour     :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_FreeHash(Tcl_Interp *Interp,char *Name) {

   TData *data;

   if ((data=(TData*)TclY_HashDel(&TData_Table,Name))) {
      DataSpec_FreeHash(Interp,data->Spec->Name);
      Data_Free(data);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Get>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait l'adresse d'un champs de la liste des champs
 *            connu (HashTable) et en retourne la valeur.
 *
 * Parametres     :
 *  <Name>        : Nom du champ
 *
 * Retour:
 *  <Interp> : Interpreteur Tcl
 *  <TData>  : Pointeur sur la structure du champs (char)
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
TData* Data_Get(char *Name) {
   return((TData*)TclY_HashGet(&TData_Table,Name));
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_UnlinkFSTDFile>
 * Creation : Juillet 2015 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Nullify file link of field using this file.
 *
 * Parametres     :
 *  <File>        : RPN File closed
 *
 * Retour:
 *  <File>  : char)
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/

int Data_UnlinkFSTDFile(TRPNFile *File) {
   
   Tcl_HashEntry *entry=NULL;
   Tcl_HashSearch ptr;
   TData         *data;
   int            n=0;
   
   entry=Tcl_FirstHashEntry(&TData_Table,&ptr);
   while (entry) {
      data=Tcl_GetHashValue(entry);
      if (data->Type==TD_RPN && data->Head && ((TRPNHeader*)data->Head)->File && ((TRPNHeader*)data->Head)->File==File) {
         ((TRPNHeader*)data->Head)->File=NULL;
         n++;
      }
      entry=Tcl_NextHashEntry(&ptr);
   }
   return(n);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_GetShell>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait l'adresse d'un champs de la liste des champs
 *            connu (HashTable) et en retourne la valeur.
 *
 * Parametres     :
 *  <Name>        : Nom du champ
 *
 * Retour:
 *  <Interp> : Interpreteur Tcl
 *  <TData>  : Pointeur sur la structure du champs (char)
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
TData* Data_GetShell(Tcl_Interp *Interp,char *Name){

   TData          *field=NULL;
   Tcl_HashEntry  *entry;
   int             new;

   entry=TclY_CreateHashEntry(&TData_Table,Name,&new);

   if (!new) {
      Tcl_AppendResult(Interp,"Data_GetShell: Field already exist",(char *)NULL);
   } else {
      if ((field=(TData*)malloc(sizeof(TData)))) {
         Tcl_SetHashValue(entry,field);
      }
   }
   return(field);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_GetStat>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait des statistiques d'un champ.
 *            (Minimum,Maximum,Moyenne,LatMin,LatMax,LonMin,LonMax)
 *
 * Parametres :
 *  <Field>   : Champs a utiliser
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_GetStat(TData *Field){

   TDef          *def;
   double        val=0.0,mode;
   int           i,j,k,d,imin=0,jmin=0,imax=0,jmax=0,kmin=0,kmax=0;
   unsigned long idxk,idx,n=0;

   def=Field->SDef?Field->SDef[0]:Field->Def;

#ifdef HAVE_RMN
   if (Field->GRef && Field->GRef->Type&GRID_SPARSE)
      FSTD_FieldReadMesh(Field);
#endif

   // Calculate vector module if needed
   if (def->NC>1 && !def->Dir) {
      if (!def->Mode || def->Mode==def->Data[0]) {
         def->Mode=(char*)malloc(FSIZE3D(def)*TDef_Size[def->Type]);
      } else {
         def->Mode=(char*)realloc(def->Mode,FSIZE3D(def)*TDef_Size[def->Type]);
      }
      if (def->Mode) {
         for (k=0;k<def->NK;k++) {
            idxk=FSIZE2D(def)*k;
            for (j=0;j<def->NJ;j++) {
               idx=idxk+j*def->NI;
               for (i=0;i<def->NI;i++,idx++) {
                  mode=0;
                  for (d=0;d<def->NC;d++) {
                     Def_Get(def,d,idx,val);
                     mode+=val*val;
                  }
                  if (DEFVALID(Field->Def,val)) {
                     Def_SetMod(def,idx,sqrt(mode));
                  } else {
                     Def_SetMod(def,idx,val);
                  }
               }
            }
         }
      }
   } else {
      def->Mode=def->Data[0];
   }

   // For supergrids, point the subgrids mode to the right place
   if (Field->SDef) {
      for(i=1;i<=Field->GRef->NbId;i++) {
         // Point to subgrid data within global data array
         Field->SDef[i]->Mode=def->Mode+(Field->SDef[i]->Data[0]-def->Data[0]);
      }
   }

   // Initialiser la structure
   if (!Field->Stat)
      Field->Stat=(TDataStat*)malloc(sizeof(TDataStat));

   if (Field->Stat) {
      Field->Stat->Histo=NULL;
      Field->Stat->HistoBin=256;
      Field->Stat->Min=1e200;
      Field->Stat->Max=-1e200;
      Field->Stat->Avg=0.0;

      for (k=0;k<def->NK;k++) {
         idxk=FSIZE2D(def)*k;

         /*Calculer les statistiques*/
         for (j=0;j<def->NJ;j++) {
            idx=idxk+j*def->NI;
            for (i=0;i<def->NI;i++,idx++) {

               Def_GetMod(def,idx,val);

               if (DEFVALID(def,val)) {
                  n++;
                  Field->Stat->Avg+=val;

                  if (val<Field->Stat->Min && val!=0.0) {
                     Field->Stat->Min=val;
                     imin=i;
                     jmin=j;
                     kmin=k;
                  }
                  if (val>Field->Stat->Max) {
                     Field->Stat->Max=val;
                     imax=i;
                     jmax=j;
                     kmax=k;
                  }
               }
            }
         }
      }
      if (n)
         Field->Stat->Avg/=n;

      if (Field->Stat->Min==1e200 || Field->Stat->Min==Field->Stat->Max) Field->Stat->Min=0.0;

      // Recuperer les coordonnees latlon des min max
      Field->Stat->MinLoc.Lat=0;
      Field->Stat->MinLoc.Lon=0;
      Field->Stat->MinLoc.Elev=0;
      Field->Stat->MaxLoc.Lat=0;
      Field->Stat->MaxLoc.Lon=0;
      Field->Stat->MaxLoc.Elev=0;

      if (Field->GRef && Field->GRef->Grid[0]!='V') {
         if (Field->GRef->Project) {
            Field->GRef->Project(Field->GRef,imin,jmin,&Field->Stat->MinLoc.Lat,&Field->Stat->MinLoc.Lon,1,1);
            Field->GRef->Project(Field->GRef,imax,jmax,&Field->Stat->MaxLoc.Lat,&Field->Stat->MaxLoc.Lon,1,1);
         } else if (Field->GRef->AY && Field->GRef->AX) {
            Field->Stat->MinLoc.Lat=Field->GRef->AY[FIDX2D(def,imin,jmin)];
            Field->Stat->MinLoc.Lon=Field->GRef->AX[FIDX2D(def,imin,jmin)];
            Field->Stat->MaxLoc.Lat=Field->GRef->AY[FIDX2D(def,imax,jmax)];
            Field->Stat->MaxLoc.Lon=Field->GRef->AX[FIDX2D(def,imax,jmax)];
         }
         if (Field->GRef->Hgt) {
            Field->Stat->MinLoc.Elev=Field->GRef->Hgt[FIDX2D(def,imin,jmin)];
            Field->Stat->MaxLoc.Elev=Field->GRef->Hgt[FIDX2D(def,imax,jmax)];
         }  else if (Field->ZRef->Levels) {
            Field->Stat->MinLoc.Elev=ZRef_Level2Meter(Field->ZRef->Levels[kmin],Field->ZRef->Type);
            Field->Stat->MaxLoc.Elev=ZRef_Level2Meter(Field->ZRef->Levels[kmax],Field->ZRef->Type);
         }
      }
   }
}

void Data_StatFree(TDataStat *Stat) {
   if (Stat->Histo) free(Stat->Histo);
   free(Stat);
}

void Data_DefFree(TData *Field) {
   
   int i;
   
   // Free subgrids but make sure it was not freed above
   if (Field->SDef) {
      for(i=0;i<Field->GRef->NbId+1;i++) {
         Def_Free(Field->SDef[i]);
      }
      free(Field->SDef);
      Field->SDef=NULL;
   } else {
      Def_Free(Field->Def);
      Field->Def=NULL;
   }
}

int Data_Free(TData *Field) {

   if (Field) {

      // Liberer l'espace specifique au type de donnees
      Field->Free(Field);

      // Liberer l'espace de donnees
      Data_Clean(Field,1,1,1);
      Data_DefFree(Field);

      // Liberer l'espace du descriptif
      if (Field->Stat) Data_StatFree(Field->Stat);
      if (Field->GRef) GeoRef_Destroy(NULL,Field->GRef->Name);
      if (Field->ZRef) ZRef_Free(Field->ZRef);
      if (Field->Tag)  Tcl_DecrRefCount(Field->Tag);

      free(Field);
   }
   return(TCL_OK);
}

TData* Data_Copy(Tcl_Interp *Interp,TData *Field,char *Name,int Def,int Alias){

   TData *field;
   TDef  *def;
   int    i,alias;

   if (!Field || !Field->Def)
      return(NULL);

   // Verifier que le champs n'est pas lui-meme
   field=Data_Get(Name);

   if (field==Field) {
      if (!Def && field->Def) {
         Def_Free(field->SDef?field->SDef[0]:field->Def);
         field->Def=NULL;
      }
      return(field);
   }

   def=Field->SDef?Field->SDef[0]:Field->Def;
   alias=Alias?1:def->Alias;

   // Est-ce que le champs existe et si oui, verifier les dimensions 
   if (Def) {
      if (!(field=Data_Valid(Interp,Name,def->NI,def->NJ,def->NK,alias?-1:def->NC,def->Type))) {
         return(NULL);
      }
      field->Def->Alias=alias;
      field->Def->CellDim=def->CellDim;
      field->Def->NoData=def->NoData;
      field->Def->Type=def->Type;
      field->Def->Level=def->Level;
      field->Def->Sample=def->Sample;

      memcpy(field->Def->Limits,def->Limits,6*sizeof(int));
      field->Def->CoordLimits[0][0]=def->CoordLimits[0][0];
      field->Def->CoordLimits[0][1]=def->CoordLimits[0][1];
      field->Def->CoordLimits[1][0]=def->CoordLimits[1][0];
      field->Def->CoordLimits[1][1]=def->CoordLimits[1][1];
   } else {
      if (!(field=Data_Valid(Interp,Name,0,0,0,0,def->Type))) {
         return(NULL);
      }
   }

   // Copy basic information
   Field->Set(field);
   Field->Copy(field->Head,Field->Head);
   field->GRef=GeoRef_Copy(Field->GRef);
   field->ZRef=ZRef_Copy(Field->ZRef);
   field->GPos=GeoPos_Copy(Field->GPos);

   if (Field->Stat) {
      field->Stat=(TDataStat*)malloc(sizeof(TDataStat));
      memcpy(field->Stat,Field->Stat,sizeof(TDataStat));
      field->Stat->Histo=NULL;
   }

   if (field->Spec && Field->Spec) {

      if (Field->Spec->Map)  {
         field->Spec->Map=Field->Spec->Map;
         CMap_Incr(field->Spec->Map);
      }
      if (Field->Spec->Desc) field->Spec->Desc=strdup(Field->Spec->Desc);
      if (Field->Spec->Topo) field->Spec->Topo=strdup(Field->Spec->Topo);
   }

   if (Def) {
      for(i=0;i<4;i++) {
         if (def->Data[i]) {
            if (alias) {
               field->Def->Data[i]=def->Data[i];
            } else {
               memcpy(field->Def->Data[i],def->Data[i],FSIZE3D(def)*TDef_Size[def->Type]);
            }
         }
      }
      if (alias && Field->Stat)
         field->Def->Mode=def->Mode;

      if (def->Mask) {
         field->Def->Mask=(char*)malloc(field->Def->NIJ);
         memcpy(field->Def->Mask,def->Mask,field->Def->NIJ);
      }
   }

#ifdef HAVE_RMN
   if (field->GRef->Grid[0]=='U') {
      FSTD_FieldSubBuild(field);
   }
#endif

   return(field);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Cut>
 * Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue un coupe verticale aux coordonnes specifiees.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Field>   : Stream de Champs
 *  <Cut>     : Identificateur du champs de coupe
 *  <Lat>     : Stream de Latitude
 *  <Lon>     : Stream de Longitude
 *  <NbF>     : Nombre de champs
 *  <NbC>     : Nombre de coordonnees
 *  <SD>      : For vector, Get speed direction instead of reproject components along xsection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_Cut(Tcl_Interp *Interp,TData **Field,char *Cut,double *Lat,double *Lon,int NbF,int NbC,int SD) {

#ifdef HAVE_RMN
   TData *cut;
   unsigned int  n,k,f,p,g,ip1;
   unsigned long idx,idxp;
   double  i,j,i0=-1.0,j0=-1.0,theta=0.0,zeta,vi,vj,vij,p0;
   float   *fp;

   // Recuperer la grille dans l'espace des champs de base
   p=1;g=0;
   for(f=0;f<NbF;f++) {
      if (!Field[f] || Field[f]->GRef->Grid[0]=='V') {
         Tcl_AppendResult(Interp,"Data_Cut:  Invalid Field or Grid",(char*)NULL);
         return(TCL_ERROR);
      }
      /*If those are 3D fields*/
      if (Field[f]->Def->NK>1) {

         /*Decode vertical level parameters*/
         if (!FSTD_DecodeRPNLevelParams(Field[f])) {
            Tcl_AppendResult(Interp,"Data_Cut: (WARNING) Could not find level paramaters from file",(char*)NULL);
         }

         /*Check if we need to get the pressure levels*/
         if (Field[f]->ZRef->Type==LVL_ANGLE || Field[f]->ZRef->Type==LVL_GALCHEN || Field[f]->ZRef->Type==LVL_MASL || Field[f]->ZRef->Type==LVL_MBSL || Field[f]->ZRef->Type==LVL_MAGL || Field[f]->ZRef->Type==LVL_UNDEF) {
            p=0;
         }
      } else {
         p=0;
      }

      if (Field[0]->Spec && (Field[0]->Spec->ZType==LVL_MASL || Field[0]->Spec->ZType==LVL_MAGL)) {
         p=0;
         g=1;
      }
   }

   if (Field[0]->Def->NK>1) {
      cut=Data_Valid(Interp,Cut,NbF*NbC,Field[0]->Def->NK,1,Field[0]->Def->NC,Field[0]->Def->Type);
   } else {
      cut=Data_Valid(Interp,Cut,NbC,NbF,1,Field[0]->Def->NC,Field[0]->Def->Type);
   }

   Field[0]->Set(cut);
   Field[0]->Copy(cut->Head,Field[0]->Head);
   ((TRPNHeader*)cut->Head)->File=NULL;

   cut->GRef=GeoRef_Reference(Field[0]->GRef);
//TODO:EER   GeoRef_Put(Interp,NULL,cut->GRef);
   cut->GRef->Grid[0]=(Field[0]->Def->NK>1?'V':'X');
 
   cut->ZRef=ZRef_Define(Field[0]->ZRef->Type,Field[0]->ZRef->LevelNb,Field[0]->ZRef->Levels);
   cut->ZRef->ETop=Field[0]->ZRef->ETop;
   cut->ZRef->PTop=Field[0]->ZRef->PTop;
   cut->ZRef->PRef=Field[0]->ZRef->PRef;
   cut->ZRef->RCoef[0]=Field[0]->ZRef->RCoef[0];
   cut->ZRef->RCoef[1]=Field[0]->ZRef->RCoef[1];

   cut->GPos=GeoPos_Find(cut->GRef,cut->ZRef);
   
   if (Field[0]->Spec) {
      if (Field[0]->Spec->Desc) cut->Spec->Desc=strdup(Field[0]->Spec->Desc);
      if (Field[0]->Spec->Topo) cut->Spec->Topo=strdup(Field[0]->Spec->Topo);
   }

   if (Field[0]->Def->NK>1) {
      GeoRef_Size(cut->GRef,0,0,NbF*NbC-1,Field[0]->Def->NK-1,0);
   } else {
      GeoRef_Size(cut->GRef,0,0,NbC-1,NbF-1,0);
   }
   GeoRef_Qualify(cut->GRef);

   if (!NbC || !NbF)
      return(TCL_OK);

   cut->GRef->AY=(float*)malloc(NbC*sizeof(float));
   cut->GRef->AX=(float*)malloc(NbC*sizeof(float));

   if (!cut->GRef->AY || !cut->GRef->AX) {
      Tcl_AppendResult(Interp,"Data_Cut: Unable to allocate memory for coordinate caching",(char*)NULL);
      return(TCL_ERROR);
   }

   // If we are in pressure or magl coordinates, allocate height array
   if (p || g) {
      cut->GRef->Hgt=(float*)malloc(NbF*NbC*Field[0]->ZRef->LevelNb*sizeof(float));
      if (!cut->GRef->Hgt) {
         Tcl_AppendResult(Interp,"Data_Cut: Unable to allocate memory for pressure correspondance",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   // Loop on coordinates
   for(n=0;n<NbC;n++) {

      // If coordinates are valid
      if (Lat[n]!=-999.0 && Lon[n]!=-999.0) {

         // Keep coordinate for later use
         cut->GRef->AY[n]=Lat[n];
         cut->GRef->AX[n]=Lon[n];

         // Loop on fields
         for(f=0;f<NbF;f++) {

            // Read the corresponding ground pressure for level conversion, if already read, nothing will be done
            if (p && !Field[f]->Def->Pres && cut->GRef->Hgt) {
               if (FSTD_FileSet(NULL,((TRPNHeader*)Field[f]->Head)->File)>=0) {
                 if (!(FSTD_FieldReadComp(((TRPNHeader*)Field[f]->Head),&Field[f]->Def->Pres,"P0",-1,0))) {
                     /*We won't be able to calculate pressure levels*/
                     free(cut->GRef->Hgt);
                     cut->GRef->Hgt=NULL;
                     p=0;
                  }
                  FSTD_FileUnset(NULL,((TRPNHeader*)Field[f]->Head)->File);
               }
            }

            // Get the grid coordinate
            if (!Field[f]->GRef->UnProject(Field[f]->GRef,&i,&j,Lat[n],Lon[n],0,1)) {
               continue;
            }

            // Vectorial data needs to be referenced along the cut so calculate angle
            if (cut->Def->Data[1]&& NbC>1 && !SD) {
               if (i0==-1.0) {
                  Field[f]->GRef->UnProject(Field[f]->GRef,&i0,&j0,Lat[n+1],Lon[n+1],0,1);
                  theta=atan2(j0-j,i0-i);
               } else {
                  theta=atan2(j-j0,i-i0);
               }
            }

            // if we're in nearest mode
            if (Field[f]->Spec->InterpDegree[0]=='N') {
               i=ROUND(i);
               j=ROUND(j);
            }

            // Loop on vertical levels
            for(k=0;k<Field[0]->Def->NK;k++) {

               idx=(Field[0]->Def->NK>1)?(k*NbF*NbC+n*NbF+f):(f*NbC+n);

               // Convert level to pressure
               if (p && cut->GRef->Hgt && Field[f]->Def->Pres) {
                  p0=((float*)Field[f]->Def->Pres)[ROUND(j)*Field[f]->Def->NI+ROUND(i)];
                  cut->GRef->Hgt[idx]=ZRef_K2Pressure(Field[f]->ZRef,p0,k);
               }

               // Read the corresponding ground pressure for level conversion, if already read, nothing will be done
               if (g && cut->GRef->Hgt) {
                  if (!Field[f]->Def->Height) {
                     if ((Field[f]->Def->Height=(float*)malloc(FSIZE3D(Field[f]->Def)*sizeof(float)))) {
                        for(idxp=0;idxp<Field[f]->Def->NK;idxp++) {
                           Field[f]->Def->Height[FSIZE2D(Field[f]->Def)*idxp]=-999;
                        }
                     }
                  }
                  idxp=FSIZE2D(Field[f]->Def)*k;
                  if (Field[f]->Def->Height && Field[f]->Def->Height[idxp]==-999) {
                     if (FSTD_FileSet(NULL,((TRPNHeader*)Field[f]->Head)->File)>=0) {
                        ip1=((TRPNHeader*)Field[f]->Head)->IP1;
                        fp=&Field[f]->Def->Height[idxp];
                        ((TRPNHeader*)Field[f]->Head)->IP1=ZRef_Level2IP(Field[f]->ZRef->Levels[k],Field[f]->ZRef->Type,DEFAULT);
                        if (!(FSTD_FieldReadComp(((TRPNHeader*)Field[f]->Head),&fp,"GZ",0,1))) {
                           /*We won't be able to calculate pressure levels*/
                           free(cut->GRef->Hgt);
                           cut->GRef->Hgt=NULL;
                           p=0;
                        }
                        ((TRPNHeader*)Field[f]->Head)->IP1=ip1;
                        FSTD_FileUnset(NULL,((TRPNHeader*)Field[f]->Head)->File);
                     }
                  }
                  if (cut->GRef->Hgt) {
                     cut->GRef->Hgt[idx]=Field[f]->Def->Height[idxp+ROUND(j)*Field[f]->Def->NI+ROUND(i)]*10.0;
                  }
               }

               // If it is vectors and components are asked
               if (cut->Def->Data[1] && !SD) {

                  // Get vector component
                  vi=VertexVal(Field[f]->Def,0,i,j,k);
                  vj=VertexVal(Field[f]->Def,1,i,j,k);

                  // If it is xsection, reproject along xsection axis
                  if (NbC>1) {
                     
                     // Get reprojection angle
                    zeta=atan2(vj,vi)-theta;

                     // Reproject along xsection axis
                     vij=hypot(vi,vj);
                     Def_Set(cut->Def,0,idx,vij*cos(zeta));
                     Def_Set(cut->Def,1,idx,vij*sin(zeta));
                     
                     App_Log(DEBUG,"%s: Reprojected angle=%f ll=(%f,%f) ij=[%f,%f] orig=%f %f (%f) project=%f %f (%f)\n",__func__,theta*57.295779,Lat[n],Lon[n],i,j,vi,vj,vij,
                        vij*cos(zeta),vij*sin(zeta),hypot(vij*cos(zeta),vij*sin(zeta)));
                  } else {
                     // This is a profile
                     Def_Set(cut->Def,0,idx,vi);
                     Def_Set(cut->Def,1,idx,vj);
                  }                    
               } else {
                  Field[f]->GRef->Value(Field[f]->GRef,Field[f]->Def,Field[f]->Spec->InterpDegree[0],0,i,j,k,&vi,&vj);
                  Def_Set(cut->Def,0,idx,vi);
                  if (cut->Def->Data[1]) {
                     Def_Set(cut->Def,1,idx,vj);
                     cut->Def->Dir=cut->Def->Data[1];
                  }
               }
               
               // If we have vertical component
               if (cut->Def->Data[2]) {
                  vi=VertexVal(Field[f]->Def,2,i,j,k);
                  Def_Set(cut->Def,2,idx,vi);
               }
            }
            i0=i;
            j0=j;
         }
      }
   }
   
   App_Log(DEBUG,"%s: Vertical grid size (%i,%i)\n",__func__,cut->Def->NI,cut->Def->NJ);

#endif
  return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_PreInit>
 * Creation : Juillet 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la pre-initialisation des parametres avant le rendue.
 *
 * Parametres :
 *  <Data>    : Champs
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_PreInit(TData *Data) {

   if (!Data->Stat)
      Data_GetStat(Data);

   if (!(Data->Spec->MinMax&DATASPEC_MINSET)) Data->Spec->Min=Data->Spec->InterNb?Data->Spec->Inter[0]:Data->Stat->Min;
   if (!(Data->Spec->MinMax&DATASPEC_MAXSET)) Data->Spec->Max=Data->Spec->InterNb?Data->Spec->Inter[Data->Spec->InterNb-1]:Data->Stat->Max;
   if (!(Data->Spec->MinMax&DATASPEC_MINSET)) Data->Spec->Min=Data->Spec->Max<Data->Spec->Min?Data->Spec->Max:Data->Spec->Min;

   DataSpec_Define(Data->Spec);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Valid>
 * Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Determiner la validite d'un champ de par ses dimension ou en
 *            creer un nouveau si necessaire.
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Name>        : Nom du champ
 *  <NI>          : Dimension en I
 *  <NJ>          : Dimension en J
 *  <NK>          : Dimension en K
 *  <Dim>         : Champs vectoriel (<0 = alias field)
 *  <Type>        : Type de donnees
 *
 * Retour:
 *  <Field>       : Champs valide pour les parametres demandees
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
TData *Data_Valid(Tcl_Interp *Interp,char *Name,int NI,int NJ,int NK,int Dim,TDef_Type Type){

   TData          *field=NULL;
   Tcl_HashEntry  *entry;
   int             new;

   if (!(entry=TclY_CreateHashEntry(&TData_Table,Name,&new))) {
      Tcl_AppendResult(Interp,"Data_Valid: Unable to create hash entry for field ",Name,(char *)NULL);
      return(NULL);
   }

   if (!new) {
      field=(TData*)Tcl_GetHashValue(entry);

      if (NI*NJ*NK==0) {
         Data_DefFree(field);
      } else {
         // Si les dimensions sont correctes et les composantes sont ls memes
         if (NI!=field->Def->NI || NJ!=field->Def->NJ || NK!=field->Def->NK || abs(Dim)!=field->Def->NC || TDef_Size[field->Def->Type]!=TDef_Size[Type]) {
            Data_DefFree(field);
            if (!(field->Def=Def_New(NI,NJ,NK,Dim,Type))) {
               Tcl_AppendResult(Interp,"Data_Valid: Not enough memory to allocate data for field ",Name,(char *)NULL);
               return(NULL);
            }
         }
         field->Def->Type=Type;
      }

      // Liberer les donnees secondaires
      Data_Clean(field,1,1,1);
      field->Free(field);                                      field->Head=NULL;
      if (field->Tag)  Tcl_DecrRefCount(field->Tag);           field->Tag=NULL;
      if (field->Stat) Data_StatFree(field->Stat);             field->Stat=NULL;
      if (field->ZRef) ZRef_Free(field->ZRef);                 field->ZRef=NULL;
//TODO: Check if ok not to free ->  if (field->GRef) GeoRef_Destroy(NULL,field->GRef->Name); field->GRef=NULL;
   }

   // Allouer la memoire pour les structures
   if (!field) {
       if (!(field=(TData*)malloc(sizeof(TData)))) {
         Tcl_AppendResult(Interp,"Data_Valid: Not enough memory to allocate field ",Name,(char *)NULL);
         return(NULL);
      }
      if (!(field->Spec=DataSpec_Create(Interp,NULL))) {
         Tcl_AppendResult(Interp,"Data_Valid: Not enough memory to allocate field ",Name,(char *)NULL);
         return(NULL);
      }

      field->Def=NULL;
      field->SDef=NULL;
      field->Stat=NULL;
      field->GPos=NULL;
      field->GRef=NULL;
      field->ZRef=NULL;
      field->Head=NULL;
      field->Free=NULL;
      field->ReadCube=NULL;
      field->Define=NULL;
      field->Tag=NULL;
      
      field->Map=NULL;
      field->MapIdx=NULL;
      field->MapIdxNb=0;
      field->GLId=0;

      if (NI*NJ*NK) {
         if (!(field->Def=Def_New(NI,NJ,NK,Dim,Type))) {
            Tcl_AppendResult(Interp,"Data_Valid: Not enough memory to allocate data for field ",Name,(char *)NULL);
            return(NULL);
         }
      }
      Tcl_SetHashValue(entry,field);
   }
   return(field);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Wipe>
 * Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Liberer toutes la memoire allouee par ce package.
 *
 * Parametres     :
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_Wipe() {
   TclY_HashWipe(&TData_Table,(TclY_HashFreeEntryDataFunc*)Data_Free);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Data_Cmd>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Execution de la commande d'expression
 *
 * Parametres   :
 *  <clientData>  : Donnees du module.
 *  <Interp>      : Interpreteur TCL.
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour       : Code de retour Tcl
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int Data_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   char      expr[PARSE_LEN],*fld;
   int       i,len,tlen=0;
   TDef_Type type=TD_Unknown;

   /*Clear the expression*/
   memset(expr,0,PARSE_LEN);
   expr[0]='\0';

   fld=Tcl_GetString(Objv[1]);
   if (strncmp(fld,"(Binary)",8)==0) {
      fld=&fld[8];type=TD_Binary;
   } else if (strncmp(fld,"(UByte)",7)==0) {
      fld=&fld[7];type=TD_UByte;
   } else if (strncmp(fld,"(Byte)",6)==0) {
      fld=&fld[6];type=TD_Byte;
   } else if (strncmp(fld,"(UInt16)",8)==0) {
      fld=&fld[8];type=TD_UInt16;
   } else if (strncmp(fld,"(Int16)",7)==0) {
      fld=&fld[7];type=TD_Int16;
   } else if (strncmp(fld,"(UInt32)",8)==0) {
      fld=&fld[8];type=TD_UInt32;
   } else if (strncmp(fld,"(Int32)",7)==0) {
      fld=&fld[7];type=TD_Int32;
   } else if (strncmp(fld,"(UInt64)",8)==0) {
      fld=&fld[8];type=TD_UInt64;
   } else if (strncmp(fld,"(Int64)",7)==0) {
      fld=&fld[7];type=TD_Int64;
   } else if (strncmp(fld,"(Float32)",9)==0) {
      fld=&fld[9];type=TD_Float32;
   } else if (strncmp(fld,"(Float64)",9)==0) {
      fld=&fld[9];type=TD_Float64;
   }

   /*Build the expression by concatenation of the args*/
   for(i=2;i<Objc;i++) {
      strcat(expr,Tcl_GetStringFromObj(Objv[i],&len));
      if ((len+=tlen)>=PARSE_LEN) {
         Tcl_AppendResult(Interp,"Data_Cmd: expression string too long",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   Tcl_MutexLock(&MUTEX_DATACALC);
   len=Calc_Parse(Interp,0,fld,type,expr);  /*Lexing and Parsing yeyeye*/
   Tcl_MutexUnlock(&MUTEX_DATACALC);

   return(len);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Clean>
 * Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Reinitialiser les structures du champ.
 *
 * Parametres :
 *  <Field>   : Reference du champs
 *  <Map>     : Clean de la Texture
 *  <Pos>     : Clean des Positions
 *  <Seg>     : Clean des Contours
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_Clean(TData *Data,int Map,int Pos,int Seg){

   int n=0;

   if (Data) {
      if (Pos) {
         if (Data->GPos) {
            GeoPos_Free(Data->GPos);
            Data->GPos=NULL;
         }

         if (Data->Def) {
            if (Data->Def->Pres)  {
               free(Data->Def->Pres);
               Data->Def->Pres=NULL;
            }
            if (Data->Def->Height) {
               free(Data->Def->Height);
               Data->Def->Height=NULL;
            }
         }
      }

      if (Map) {
         if (Data->Map) {
            free(Data->Map);
            Data->Map=NULL;
         }
         if (Data->MapIdx) {
            free(Data->MapIdx);
            Data->MapIdx=NULL;
            Data->MapIdxNb=0;
         }
         if (Data->GLId) {
            glDeleteLists(Data->GLId,1);
            Data->GLId=0;
         }
      }
      
      if (Seg) {
         if (Data->SDef) {
            // Loop on subgrids (U grids)
            for(n=1;n<=Data->GRef->NbId;n++) {
               TList_Clear(Data->SDef[n]->Segments,(TList_FreeProc*)T3DArray_Free);
               Data->SDef[n]->Segments=NULL;
            }
         }
         if (Data->Def) {
            TList_Clear(Data->Def->Segments,(TList_FreeProc*)T3DArray_Free);
            Data->Def->Segments=NULL;
         }
      }
   }
}

void Data_CleanAll(TDataSpec *Spec,int Map,int Pos,int Seg) {

   TData          *data;
   Tcl_HashSearch  ptr;
   Tcl_HashEntry  *entry=NULL;

   TclY_LockHash();
   entry=Tcl_FirstHashEntry(&TData_Table,&ptr);
   while (entry) {
      data=Tcl_GetHashValue(entry);
      if (data && data->Spec && (data->Spec==Spec || data->Spec->Id==Spec->Id)) {
         Data_Clean(data,Map,Pos,Seg);
      }
      entry=Tcl_NextHashEntry(&ptr);
   }
   TclY_UnlockHash();
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Sort>
 * Creation : Octobre 2005- J.P. Gauthier - CMC/CMOE
 *
 * But      : Trier une liste de TDef pour diverses stats.
 *
 * Parametres :
 *  <Interp>  : Dimension du champs a allouer
 *  <List>    : Liste de Champs/Obs
 *
 * Retour:
 *  <Status>  :   Code d'erreur Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_Sort(Tcl_Interp *Interp,Tcl_Obj *List){

   Tcl_Obj      *obj;
   TDef    **def=NULL;
   TData        *fld;
   TObs         *obs;
   int           n,nobj;
   unsigned long i=0;
   double       *v=NULL;

   if (!List) {
      Tcl_AppendResult(Interp,"\n   Data_Sort: Empty list",(char*)NULL);
      return(TCL_ERROR);
   }
   Tcl_ListObjLength(Interp,List,&nobj);

   v=(double*)alloca(nobj*sizeof(double));
   def=(TDef**)alloca(nobj*sizeof(TDef*));

   if (v && def) {
      for(n=0;n<nobj;n++) {
         Tcl_ListObjIndex(Interp,List,n,&obj);
         if ((fld=Data_Get(Tcl_GetString(obj)))) {
            def[n]=fld->Def;
         } else if ((obs=Obs_Get(Tcl_GetString(obj)))) {
            def[n]=obs->Def;
         } else {
            Tcl_AppendResult(Interp,"\n   Data_Sort: Invalid field of observation",(char*)NULL);
            return(TCL_ERROR);
         }
         if (i!=0 && i!=FSIZE2D(def[n])) {
            Tcl_AppendResult(Interp,"\n   Data_Sort: Invalid dimensions",(char*)NULL);
            return(TCL_ERROR);
         }
         i=FSIZE2D(def[n]);
      }

      for(i=0;i<FSIZE2D(def[0]);i++) {
         for(n=0;n<nobj;n++) {
            Def_Get(def[n],0,i,v[n]);
         }
         qsort(v,nobj,sizeof(double),QSort_Double);
         for(n=0;n<nobj;n++) {
            Def_Set(def[n],0,i,v[n]);
         }
      }
   } else {
      Tcl_AppendResult(Interp,"\n   Data_Sort: Unable to allocate temporary buffer",(char*)NULL);
      return(TCL_ERROR);
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_GetImage>
 * Creation : Janvier 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer une image Tk d'un champs (snapshot).
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Field>       : Champs
 *  <Img>         : Image
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_GetImage(Tcl_Interp *Interp,TData *Field,char* Img){

   double incri,incrj,val=0.0;
   int idx,cidx,x,y,i,j,nidx;

   Tk_PhotoImageBlock data;
   Tk_PhotoHandle     handle;

   if (!Field || !Field->Spec || !Field->Spec->Map) {
      Tcl_AppendResult(Interp,"Data_GetImage: Invalid field or missing colormap",(char*)NULL);
      return(TCL_ERROR);
   }

   Data_PreInit(Field);

   /*Recuperer le handle de l'image specifie*/
   handle=Tk_FindPhoto(Interp,Img);

   /*Definire les parametres du bock de donnees*/
   Tk_PhotoGetSize(handle,&data.width,&data.height);

   data.pitch=data.width*4;
   data.pixelSize=4;
   data.offset[0]=0;
   data.offset[1]=1;
   data.offset[2]=2;
   data.offset[3]=3;

   if ((data.pixelPtr=(unsigned char*)calloc(data.width*data.height*4,sizeof(unsigned char)))) {

      /*Creer l'image de la palette*/
      incrj=(double)(Field->Def->NJ)/(data.height+1);
      incri=(double)(Field->Def->NI)/(data.width+1);

      for(y=0,idx=0;y<data.height;y++) {
         for(x=0;x<data.width;x++) {
            i=(double)x*incri;
            j=Field->Def->NJ-(double)y*incrj;
            nidx=j*Field->Def->NI+i;
            Def_GetMod(Field->Def,nidx,val);
            if (DEFVALID(Field->Def,val)) {
               VAL2COL(cidx,Field->Spec,val);

               data.pixelPtr[idx++]=Field->Spec->Map->Color[cidx][0];
               data.pixelPtr[idx++]=Field->Spec->Map->Color[cidx][1];
               data.pixelPtr[idx++]=Field->Spec->Map->Color[cidx][2];
               data.pixelPtr[idx++]=Field->Spec->Map->Color[cidx][3];
            }
         }
      }

      /*Envoyer le data dans l'image Tk*/
      if (Tk_PhotoPutBlock(Interp,handle,&data,0,0,data.width,data.height,TK_PHOTO_COMPOSITE_SET)==TCL_ERROR) {
         return(TCL_ERROR);
      }

      free(data.pixelPtr);
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_HighLow>
 * Creation : Octobre 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer les Highs ou Lows.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Field>   : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *  <High>    : Recherche de (1:high -1:Low)
 *  <Tile>    : Dimension des tuiles d'evaluation
 *
 * Retour:
 *
 * Remarques :
 *     -base sur l'algorithme de Michel Grenier
 *
 *----------------------------------------------------------------------------
*/
Tcl_Obj* Data_HighLow(Tcl_Interp *Interp,TData *Field,int High,int Tile){

   Tcl_Obj *obj,*sub;
   double zm,zmm,zn,zv;
   int    high;
   int    ichk,jchk;
   int    is,js,jt,it;
   int    ip,jp,ik,jk;
   int    ni,nj;

   ni=Field->Def->NI;
   nj=Field->Def->NJ;
   zm=zmm=zn=zv=0.0;

   /* ichk and jchk indicates the covering area under which the extrema is to be evaluated */
   ichk=ni/Tile;ichk=(2>=ichk?2:(2*Tile-1<=ichk?2*Tile-1:ichk));
   jchk=nj/Tile;jchk=(2>=jchk?2:(2*Tile-1<=jchk?2*Tile-1:jchk));

   obj=Tcl_NewListObj(0,NULL);

   /* loop in all the lines */
   for (ip=0;ip<ni;ip++) {

      /* as long as the function is increasing or decreasing move along the y axe */
      Def_GetMod(Field->Def,ip,zm);
      Def_GetMod(Field->Def,ip+1,zmm);
      high=(zm<=zmm);
      for (jp=0;jp<nj-1;jp++) {
         if (jp!=nj-1) {
            Def_GetMod(Field->Def,ip+(jp+1)*ni,zn);
            if (zm==zn || ((zm<zn) && high) || ((zm>zn) && !high)) {
               zm=zn;
               continue;
            }
         }

         /* check if it is really a max or a min over the area */
         js=(0>=jp-jchk?0:jp-jchk);
         jt=(nj-1<=jp+jchk?nj-1:jp+jchk);
         is=(0>=ip-ichk?0:ip-ichk);
         it=(ni-1<=ip+ichk?ni-1:ip+ichk);

         for (jk=js;jk<=jt;jk++) {
            for (ik=is;ik<=it;ik++) {
               if (ik!=ip || jk!=jp) {
                  Def_GetMod(Field->Def,ik+jk*ni,zv);
                  if (((zv>=zm) && high) || ((zv<=zm) && !high)) goto nexty;
               }
            }
         }

         /* an extrema was found */
         if (high==High && DEF2DIN(Field->Def,ip,jp)) {
            sub=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,zm)));
            Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(ip));
            Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(jp));
            Tcl_ListObjAppendElement(Interp,obj,sub);
         }

         /* continues the search */
         nexty: high=!high;
         zm=zn;
      }
   }
   return(obj);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Stat>
 * Creation : Avril 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la configuration des statistiques et le retour des valeurs
 *            si il n'y a pas de valeur specifie (seulement le token).
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Field>       : Pointeur sur le champs
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/

int Data_Stat(Tcl_Interp *Interp,TData *Field,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj    *obj,*sub;
   TData       *fld;
   TZRef       *zref;
   TList       *list;
   T3DArray    *array;
   Vect3d      *vbuf;
   int          n,i,j,ni,nj,index,idx,b,f,tr=1,ex,c1,c2,ci,cj;
   int          nb,len,nobj,mode;
   long         npt;
   double       dlat,dlon,dlat0,dlon0,dlat1,dlon1,dx,dy,dx0,dy0,dx1,dy1,val,val1,dl,dv,tmpd,min,max;
   float       *levels;
   char         buf[32];
   const char **lvls;

   extern int FFStreamLine(TGeoPos *GPos,TDef *Def,ViewportItem *VP,Vect3d *Stream,float *Map,double X,double Y,double Z,int MaxIter,double Step,double Min,double Res,int Mode,int ZDim);

   static CONST char *sfrmt[] = { "LIST","KML","GML","JSON","JAVASCRIPT",NULL };
   enum  frmt { LIST,KML,GML,JSON,JAVASCRIPT };
   
   static CONST char *sopt[] = { "-tag","-size","-component","-image","-nodata","-max","-min","-avg","-high","-low","-boundary","-grid","-gridcell","-gridlat","-gridlon","-gridpoint","-gridbox","-coordpoint","-project","-unproject","-gridvalue","-coordvalue",
      "-gridstream","-coordstream","-gridcontour","-coordcontour","-within","-withinvalue","-height","-levelindex","-level","-levels","-leveltype","-pressurelevels","-meterlevels","-limits","-coordlimits","-sample","-matrix","-mask","-celldim","-top","-ref","-coef","-poff","-datacopy", NULL };
   enum        opt {  TAG,SIZE,COMPONENT,IMAGE,NODATA,MAX,MIN,AVG,HIGH,LOW,BOUNDARY,GRID,GRIDCELL,GRIDLAT,GRIDLON,GRIDPOINT,GRIDBOX,COORDPOINT,PROJECT,UNPROJECT,GRIDVALUE,COORDVALUE,
      GRIDSTREAM,COORDSTREAM,GRIDCONTOUR,COORDCONTOUR,WITHIN,WITHINVALUE,HEIGHT,LEVELINDEX,LEVEL,LEVELS,LEVELTYPE,PRESSURELEVELS,METERLEVELS,LIMITS,COORDLIMITS,SAMPLE,MATRIX,MASK,CELLDIM,TOP,REF,COEF,POFF,DATACOPY };

   if (!Field ) {
      return(TCL_OK);
   }

   ex=Field->Spec->ExtrapDegree[0]!='N'?1:0;

#ifdef HAVE_RMN
   if (Field->GRef && Field->GRef->Type&GRID_SPARSE)
      FSTD_FieldReadMesh(Field);
#endif

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {

         case TAG:
            if (Objc==1) {
               if (Field->Tag) {
                  Tcl_SetObjResult(Interp,Field->Tag);
               }
            } else {
               if (Field->Tag) {
                  Tcl_DecrRefCount(Field->Tag);
                  Field->Tag=NULL;
               }
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  Field->Tag=Tcl_DuplicateObj(Objv[i]);
                  Tcl_IncrRefCount(Field->Tag);
               }
            }
            break;

          case SIZE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Field->Def->NI*Field->Def->NJ*Field->Def->NK*Field->Def->NC*TDef_Size[Field->Def->Type]));
            }
            break;
            
        case NODATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Field->Def->NoData));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Field->Def->NoData);
               if (Field->GRef && Field->GRef->NId==0 && Field->GRef->NbId>1) {
                  for(nb=1;nb<=Field->GRef->NbId;nb++)
                     Field->SDef[nb]->NoData=Field->Def->NoData;
               }

            }
            break;

         case COMPONENT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Field->Def->NC));
            }
            break;

         case IMAGE:
            if(Objc!=2) {
               Tcl_WrongNumArgs(Interp,2,Objv,"image");
               return(TCL_ERROR);
            }
            if (!Field->Stat)
               Data_GetStat(Field);
            return(Data_GetImage(Interp,Field,Tcl_GetString(Objv[++i])));
            break;

         case MAX:
            if (Objc==1) {
               if (!Field->Stat)
                  Data_GetStat(Field);

               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,Field->Stat->Max)));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Stat->MaxLoc.Lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Stat->MaxLoc.Lon));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Stat->MaxLoc.Elev));
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Data_GetAreaValue(Interp,3,Field,Objc-1,Objv+1)==TCL_ERROR) {
                  return(TCL_ERROR);
               }
               i++;
            }
            break;

         case MIN:
            if (Objc==1) {
               if (!Field->Stat)
                  Data_GetStat(Field);

               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,Field->Stat->Min)));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Stat->MinLoc.Lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Stat->MinLoc.Lon));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Stat->MinLoc.Elev));
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Data_GetAreaValue(Interp,2,Field,Objc-1,Objv+1)==TCL_ERROR) {
                  return(TCL_ERROR);
               }
               i++;
            }
            break;

         case AVG:
            if (Objc==1) {
               if (!Field->Stat)
                  Data_GetStat(Field);

               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,Field->Stat->Avg)));
            } else {
               if (Data_GetAreaValue(Interp,0,Field,Objc-1,Objv+1)==TCL_ERROR) {
                  return(TCL_ERROR);
               }
               i++;
            }
            break;

         case HIGH:
            if (!Field->Stat)
               Data_GetStat(Field);
            Tcl_GetIntFromObj(Interp,Objv[++i],&len);
            Tcl_SetObjResult(Interp,Data_HighLow(Interp,Field,1,len));
            break;

         case LOW:
            if (!Field->Stat)
               Data_GetStat(Field);
            Tcl_GetIntFromObj(Interp,Objv[++i],&len);
            Tcl_SetObjResult(Interp,Data_HighLow(Interp,Field,0,len));
            break;

         case GRID:
            if (!Field->GRef) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            npt=1e9;
            nobj=0;
            if (Objc>1) {
               if (Tcl_GetLongFromObj(Interp,Objv[++i],&npt)==TCL_ERROR) {
                  Tcl_ListObjLength(Interp,Objv[i],&nobj);
               }
            }

            if (!nobj) {
               obj=Tcl_NewListObj(0,NULL);
               for(nj=Field->Def->Limits[1][0];nj<=Field->Def->Limits[1][1];nj+=Field->Def->Sample) {
                  for(ni=Field->Def->Limits[0][0];ni<=Field->Def->Limits[0][1];ni+=Field->Def->Sample) {
                     if (Field->GRef->Grid[0]!='V') {
                        Field->GRef->Project(Field->GRef,ni,nj,&dlat,&dlon,0,1);
                        if (dlat>=Field->Def->CoordLimits[1][0] && dlat<=Field->Def->CoordLimits[1][1] &&
                           dlon>=Field->Def->CoordLimits[0][0] && dlon<=Field->Def->CoordLimits[0][1]) {

                           Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
                           Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
                        }
                     } else {
                        if (Field->GRef->AY && Field->GRef->AX) {
                           index=FIDX2D(Field->Def,ni,nj);
                           Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->GRef->AY[index]));
                           Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->GRef->AX[index]));
                         }
                     }
                     if (!(--npt)) break;
                  }
                  if (Field->GRef->Grid[0]=='V' || !npt) break;
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Field->GRef->Grid[0]=='V') {
                  if ((nobj>>1)!=Field->Def->NI) {
                     Tcl_AppendResult(Interp,"Data_Stat: Invalid number of coordinates",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  if (!Field->GRef->AY) {
                     Field->GRef->AY=(float*)malloc(Field->Def->NI*sizeof(float));
                     Field->GRef->AX=(float*)malloc(Field->Def->NI*sizeof(float));
                  }

                  if (Field->GRef->AY && Field->GRef->AX) {
                     for (index=0,n=0;index<nobj;index+=2,n++) {
                        Tcl_ListObjIndex(Interp,Objv[i],index,&obj);
                        Tcl_GetDoubleFromObj(Interp,obj,&dlat);
                        Tcl_ListObjIndex(Interp,Objv[i],index+1,&obj);
                        Tcl_GetDoubleFromObj(Interp,obj,&dlon);
                        Field->GRef->AY[n]=dlat;
                        Field->GRef->AX[n]=dlon;
                     }
                  } else {
                     Tcl_AppendResult(Interp,"Data_Stat: Unable to allocate memory for coordinate buffer",(char*)NULL);
                     return(TCL_ERROR);
                  }
               }
            }
            break;

         case GRIDCELL:
            if (!Field->GRef) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            npt=1e9;
            if (Objc==2)
               Tcl_GetLongFromObj(Interp,Objv[++i],&npt);

            obj=Tcl_NewListObj(0,NULL);
            for(nj=Field->Def->Limits[1][0];nj<=Field->Def->Limits[1][1];nj+=Field->Def->Sample) {
               for(ni=Field->Def->Limits[0][0];ni<=Field->Def->Limits[0][1];ni+=Field->Def->Sample) {
                  if (Field->GRef->Grid[0]!='V') {
                     Field->GRef->Project(Field->GRef,ni-0.5,nj-0.5,&dlat,&dlon,0,1);
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
                     Field->GRef->Project(Field->GRef,ni+0.5,nj-0.5,&dlat,&dlon,0,1);
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
                     Field->GRef->Project(Field->GRef,ni+0.5,nj+0.5,&dlat,&dlon,0,1);
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
                     Field->GRef->Project(Field->GRef,ni-0.5,nj+0.5,&dlat,&dlon,0,1);
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
                  }
                  if (!(--npt)) break;
               }
               if (!npt) break;
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case GRIDLAT:
            if (!Field->GRef) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }

            /*Check for maximum number of elements*/
            npt=1e9;
            if (Objc==2)
               Tcl_GetLongFromObj(Interp,Objv[++i],&npt);

            obj=Tcl_NewListObj(0,NULL);
            for(nj=Field->Def->Limits[1][0];nj<=Field->Def->Limits[1][1];nj+=Field->Def->Sample) {
               for(ni=Field->Def->Limits[0][0];ni<=Field->Def->Limits[0][1];ni+=Field->Def->Sample) {
                  if (Field->GRef->Grid[0]!='V') {
                     Field->GRef->Project(Field->GRef,ni,nj,&dlat,&dlon,0,1);
                     if (dlat>=Field->Def->CoordLimits[1][0] && dlat<=Field->Def->CoordLimits[1][1] &&
                         dlon>=Field->Def->CoordLimits[0][0] && dlon<=Field->Def->CoordLimits[0][1]) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
                     }
                  } else {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->GRef->AY[FIDX2D(Field->Def,ni,nj)]));
                  }
                  if (!(--npt)) break;
               }
               if (Field->GRef->Grid[0]=='V' || !npt) break;
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case GRIDLON:
            if (!Field->GRef) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            /*Check for maximum number of elements*/
            npt=1e9;
            if (Objc==2)
               Tcl_GetLongFromObj(Interp,Objv[++i],&npt);

            obj=Tcl_NewListObj(0,NULL);
            for(nj=Field->Def->Limits[1][0];nj<=Field->Def->Limits[1][1];nj+=Field->Def->Sample) {
               for(ni=Field->Def->Limits[0][0];ni<=Field->Def->Limits[0][1];ni+=Field->Def->Sample) {
                  if (Field->GRef->Grid[0]!='V') {
                     Field->GRef->Project(Field->GRef,ni,nj,&dlat,&dlon,0,1);
                     if (dlat>=Field->Def->CoordLimits[1][0] && dlat<=Field->Def->CoordLimits[1][1] &&
                         dlon>=Field->Def->CoordLimits[0][0] && dlon<=Field->Def->CoordLimits[0][1]) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
                     }
                  } else {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->GRef->AX[FIDX2D(Field->Def,ni,nj)]));
                  }
                  if (!(--npt)) break;
               }
               if (Field->GRef->Grid[0]=='V' || !npt) break;
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case PROJECT:
            tr=0;
            ex=1;
         case GRIDPOINT:
            if (!Field->GRef) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dx);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dy);
            if (Field->GRef->Grid[0]=='Y') {
               if (!Field->GRef->AY || !Field->GRef->AX) {
                  Tcl_AppendResult(Interp,"Data_Stat: No positional information",(char*)NULL);
                  return(TCL_ERROR);
               }
               index=FIDX2D(Field->Def,lrint(dx),lrint(dy));
               dlat=Field->GRef->AY[index];
               dlon=Field->GRef->AX[index];
            } else {
               Field->GRef->Project(Field->GRef,dx,dy,&dlat,&dlon,ex,tr);
            }
            obj=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
            Tcl_SetObjResult(Interp,obj);
            break;

         case GRIDBOX:
            if (!Field->GRef) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dl);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlat0);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlon0);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlat1);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlon1);

            if (!Field->GRef->UnProject(Field->GRef,&dx0,&dy0,dlat0,dlon0,1,1)) {
               break;
            }
            if (!Field->GRef->UnProject(Field->GRef,&dx1,&dy1,dlat1,dlon1,1,1)) {
               break;
            }

            obj=Tcl_NewListObj(0,NULL);

            for(dx=dx0;dx<dx1-dl;dx+=dl) {
               Field->GRef->Project(Field->GRef,dx,dy0,&dlat,&dlon,0,1);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
            }
            Field->GRef->Project(Field->GRef,dx1,dy0,&dlat,&dlon,0,1);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));

            for(dy=dy0;dy<dy1-dl;dy+=dl) {
               Field->GRef->Project(Field->GRef,dx1,dy,&dlat,&dlon,0,1);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
            }
            Field->GRef->Project(Field->GRef,dx1,dy1,&dlat,&dlon,0,1);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));

            for(dx=dx1;dx>dx0+dl;dx-=dl) {
               Field->GRef->Project(Field->GRef,dx,dy1,&dlat,&dlon,0,1);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
            }
            Field->GRef->Project(Field->GRef,dx0,dy1,&dlat,&dlon,0,1);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));

            for(dy=dy1;dy>dy0+dl;dy-=dl) {
               Field->GRef->Project(Field->GRef,dx0,dy,&dlat,&dlon,0,1);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
            }
            Field->GRef->Project(Field->GRef,dx0,dy0,&dlat,&dlon,0,1);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));

            Tcl_SetObjResult(Interp,obj);
            break;

         case WITHIN:
            if (!Field->GRef) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }

            if (Data_GetAreaValue(Interp,4,Field,Objc-1,Objv+1)==TCL_ERROR) {
               return(TCL_ERROR);
            }
            i++;
            break;

         case WITHINVALUE:
            if (!Field->GRef) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }

            if (Data_GetAreaValue(Interp,5,Field,Objc-1,Objv+1)==TCL_ERROR) {
               return(TCL_ERROR);
            }
            i+=2;
            break;

         case UNPROJECT:
           tr=0;
           ex=1;
         case COORDPOINT:
            if (!Field->GRef) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlat);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlon);
            Field->GRef->UnProject(Field->GRef,&dx,&dy,dlat,dlon,ex,tr);
            obj=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dx));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dy));
            Tcl_SetObjResult(Interp,obj);
            break;

         case GRIDVALUE:
            if (!Field->Stat && Objc!=4)
               Data_GetStat(Field);

            if (Objc==1 || Objc==2) {

               /*Check for maximum number of elements*/
               npt=1e9;
               if (Objc==2)
                  Tcl_GetLongFromObj(Interp,Objv[++i],&npt);

               obj=Tcl_NewListObj(0,NULL);
               for(nj=Field->Def->Limits[1][0];nj<=Field->Def->Limits[1][1];nj+=Field->Def->Sample) {
                  for(ni=Field->Def->Limits[0][0];ni<=Field->Def->Limits[0][1];ni+=Field->Def->Sample) {
                     if (Field->GRef->Grid[0]!='V') {
                        Field->GRef->Project(Field->GRef,ni,nj,&dlat,&dlon,0,1);
                        if (dlat>=Field->Def->CoordLimits[1][0] && dlat<=Field->Def->CoordLimits[1][1] &&
                           dlon>=Field->Def->CoordLimits[0][0] && dlon<=Field->Def->CoordLimits[0][1]) {
                           if (Field->Def->NC==1) {
                              Def_GetMod(Field->Def,FIDX3D(Field->Def,ni,nj,Field->Def->Level),val);
                              Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,val)));
                           } else if (Field->Def->NC==2) {
                              sub=Tcl_NewListObj(0,NULL);
                              Field->GRef->Value(Field->GRef,Field->Def,'N',0,ni,nj,Field->Def->Level,&val,&val1);
                              Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,val)));
                              Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(val1));
                              Tcl_ListObjAppendElement(Interp,obj,sub);
                           } else {
                              sub=Tcl_NewListObj(0,NULL);
                              for(n=0;n<Field->Def->NC;n++) {
                                 Field->GRef->Value(Field->GRef,Field->Def,'N',n,ni,nj,Field->Def->Level,&val,&val1);
                                 Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,val)));
                              }
                              Tcl_ListObjAppendElement(Interp,obj,sub);
                           }
                         }
                     } else {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,val)));
                     }
                     if (!(--npt)) break;
                  }
                  if (!npt) break;
               }
               Tcl_SetObjResult(Interp,obj);
            } else {

               Tcl_GetDoubleFromObj(Interp,Objv[++i],&dx);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&dy);

               if (Objc==4) {
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&val);
                  Data_ValSet(Field,dx,dy,val);
               } else {
#ifdef HAVE_RMN
                  c_ezsetopt("INTERP_DEGREE",(char*)Field->Spec->InterpDegree);
#endif
                  obj=Tcl_NewListObj(0,NULL);
                  if (Field->Def->NC==2) {
                     if (Field->GRef->Value(Field->GRef,Field->Def,Field->Spec->InterpDegree[0],0,dx,dy,Field->Def->Level,&val,&val1)) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,val)));
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(val1));
                     } else {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
                     }
                  } else {
                     for(n=0;n<Field->Def->NC;n++) {
                        if (Field->GRef->Value(Field->GRef,Field->Def,Field->Spec->InterpDegree[0],n,dx,dy,Field->Def->Level,&val,&val1)) {
                           Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,val)));
                        } else {
                           Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
                        }
                     }
                  }
                  Tcl_SetObjResult(Interp,obj);
               }
            }
            break;

         case COORDVALUE:
            if (!Field->GRef) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlat);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlon);

            Field->GRef->UnProject(Field->GRef,&dx,&dy,dlat,dlon,1,1);

            if (!Field->Stat && Objc!=4)
               Data_GetStat(Field);

            if (Objc==4) {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&val);
               Data_ValSet(Field,dx,dy,val);
            } else {
#ifdef HAVE_RMN
               c_ezsetopt("INTERP_DEGREE",(char*)Field->Spec->InterpDegree);
#endif
               obj=Tcl_NewListObj(0,NULL);
               if (Field->Def->NC==2) {
                 if (Field->GRef->Value(Field->GRef,Field->Def,Field->Spec->InterpDegree[0],0,dx,dy,Field->Def->Level,&val,&val1)) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,val)));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(val1));
                  } else {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
                  }
               } else {
                  for(n=0;n<Field->Def->NC;n++) {
                     if (Field->GRef->Value(Field->GRef,Field->Def,Field->Spec->InterpDegree[0],n,dx,dy,Field->Def->Level,&val,&val1)) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,val)));
                     } else {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
                     }
                  }
               }
               Tcl_SetObjResult(Interp,obj);
            }
            break;

         case GRIDSTREAM:
            if(Objc!=7) {
               Tcl_WrongNumArgs(Interp,1,Objv,"dx dy len min dv dl");
               return(TCL_ERROR);
            }
            
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dx);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dy);
            Tcl_GetIntFromObj(Interp,Objv[++i],&len);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&val);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dv);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dl);

            if (!Field->Def->Data[1]) {
               Tcl_AppendResult(Interp,"Data_Stat: Field is not vectorial",(char*)NULL);
               return(TCL_ERROR);
            }
            
            if (!Field->Stat)
               Data_GetStat(Field);

            vbuf=VBuffer_Alloc(len*2+1);

            b=FFStreamLine(Field->GPos,Field->Def,NULL,vbuf,NULL,dx,dy,0,len,-val,dv,dl,REF_GRID,0);
            f=FFStreamLine(Field->GPos,Field->Def,NULL,&vbuf[len],NULL,dx,0,dy,len,val,dv,dl,REF_GRID,0);
            obj=Tcl_NewListObj(0,NULL);
            ex=0;

            /*Loop on all streamline points*/
            for (nb=0;nb<b+f-1;nb++) {
               /*Clip to extent limits*/
               if ((ex=LiangBarsky_LineClip2D(vbuf[len-b+nb],vbuf[len-b+nb+1],&c1,&c2,
                  Field->Def->Limits[0][0],Field->Def->Limits[1][0],Field->Def->Limits[0][1],Field->Def->Limits[1][1]))) {

                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vbuf[len-b+nb][0]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vbuf[len-b+nb][1]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vbuf[len-b+nb][2]));
               }
            }
            /*If last segment was visible, add its end point*/
            if (ex){
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vbuf[len-b+nb][0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vbuf[len-b+nb][1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vbuf[len-b+nb][2]));
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case COORDSTREAM:
            if(Objc!=7) {
               Tcl_WrongNumArgs(Interp,1,Objv,"dx dy len min dv dl");
               return(TCL_ERROR);
            }
            
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dx);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dy);
            Tcl_GetIntFromObj(Interp,Objv[++i],&len);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&val);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dv);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dl);

            if (!Field->Def->Data[1]) {
               Tcl_AppendResult(Interp,"Data_Stat: Field is not vectorial",(char*)NULL);
               return(TCL_ERROR);
            }
            
            if (!Field->GRef) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            
            if (!Field->Stat)
               Data_GetStat(Field);

            vbuf=VBuffer_Alloc(len*2+1);

            b=FFStreamLine(Field->GPos,Field->Def,NULL,vbuf,NULL,dx,dy,0,len,-val,dv,dl,REF_COOR,0);
            f=FFStreamLine(Field->GPos,Field->Def,NULL,&vbuf[len],NULL,dx,dy,0,len,val,dv,dl,REF_COOR,0);
            obj=Tcl_NewListObj(0,NULL);
            ex=0;

            /*Loop on all streamline points*/
            for (nb=0;nb<b+f-1;nb++) {
               /*Clip to extent limits*/
               if ((ex=LiangBarsky_LineClip2D(vbuf[len-b+nb],vbuf[len-b+nb+1],&c1,&c2,Field->Def->CoordLimits[1][0],Field->Def->CoordLimits[0][0],Field->Def->CoordLimits[1][1],Field->Def->CoordLimits[0][1]))) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vbuf[len-b+nb][0]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vbuf[len-b+nb][1]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vbuf[len-b+nb][2]));
               }
            }
            /*If last segment was visible, add its end point*/
            if (ex) {
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vbuf[len-b+nb][0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vbuf[len-b+nb][1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(vbuf[len-b+nb][2]));
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case GRIDCONTOUR:
            if (Objc>3) {
               Tcl_WrongNumArgs(Interp,2,Objv,"[Resolution] [Polygon]");
               return(TCL_ERROR);
            }
            len=1;
            if (Objc>1) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&len);
            }

            ex=0;
            if (Objc>2) {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&ex);
            }
            
            if (!Field->Stat)
               Data_GetStat(Field);

            if (Field->Spec->InterNb) {
               Data_Clean(Field,0,0,1);
               FFContour(REF_GRID,Field->GPos,Field->Def,Field->Stat,NULL,Field->Spec->InterNb,Field->Spec->Inter,len,ex);

               list=Field->Def->Segments;
               obj=Tcl_NewListObj(0,NULL);

               /*Loop on all contours*/
               while(list) {
                  array=(T3DArray*)list->Data;
                  sub=Tcl_NewListObj(0,NULL);
                  ex=tr=0;

                  /*Loop on the contour points*/
                  for (n=0;n<array->Size-1;n++) {
                     /*Clip to extent limits*/
                     if ((ex=LiangBarsky_LineClip2D(array->Data[n],array->Data[n+1],&c1,&c2,
                        Field->Def->Limits[0][0],Field->Def->Limits[1][0],Field->Def->Limits[0][1],Field->Def->Limits[1][1]))) {

                        Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(array->Data[n][0]));
                        Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(array->Data[n][1]));
                        tr=1;
                     }
                  }
                  /*If last segment was visible, add its end point*/
                  if (ex){
                     Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(array->Data[n][0]));
                     Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(array->Data[n][1]));
                  }
                  if (tr) {
                     Tcl_ListObjAppendElement(Interp,obj,sub);
                  }
                  list=list->Next;
               }
               Tcl_SetObjResult(Interp,obj);
            }
            break;

         case COORDCONTOUR:
            if (Objc>4) {
               Tcl_WrongNumArgs(Interp,2,Objv,"[Resolution] [Polygon] [LIST|GML|KML|JSON|JAVASCRIPT]");
               return(TCL_ERROR);
            }
            len=1;
            if (Objc>1) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&len);
            }
            
            ex=0;
            if (Objc>2) {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&ex);
            }

            mode=LIST;
            if (Objc>3) {
               if (Tcl_GetIndexFromObj(Interp,Objv[++i],sfrmt,"type",TCL_EXACT,&mode)!=TCL_OK) {
                  return(TCL_ERROR);
               }
            }

            if (!Field->Stat)
               Data_GetStat(Field);

            if (Field->Spec->InterNb) {
               Data_Clean(Field,0,0,1);
               FFContour(REF_COOR,Field->GPos,Field->Def,Field->Stat,NULL,Field->Spec->InterNb,Field->Spec->Inter,len,ex);

               list=Field->Def->Segments;

               obj=Tcl_NewListObj(0,NULL);

               while(list) {
                  array=(T3DArray*)list->Data;
                  sub=Tcl_NewListObj(0,NULL);
                  ex=tr=0;

                  f=0;
                  for (n=0;n<array->Size-1;n++) {
                     // Clip to extent limits
                     if ((ex=LiangBarsky_LineClip2D(array->Data[n],array->Data[n+1],&c1,&c2,Field->Def->CoordLimits[0][0],Field->Def->CoordLimits[1][0],Field->Def->CoordLimits[0][1],Field->Def->CoordLimits[1][1]))) {
                        if (mode!=LIST) {
                           if (!f) {
                              switch(mode) {
                                 case KML:       Tcl_AppendResult(Interp,"<LineString><coordinates>",(char*)NULL);break;
                                 case GML:       Tcl_AppendResult(Interp," <gml:lineStringMember><gml:LineString><gml:coordinates>",(char*)NULL);break;
                                 case JSON:      Tcl_AppendResult(Interp,"{\n\t\"type\": \"LineString\",\n\t\"coordinates\": [",(char*)NULL);break;
                                 case JAVASCRIPT:Tcl_AppendResult(Interp,"[",(char*)NULL);break;
                              }
                              f=1;
                           }
                           switch(mode) {
                              case KML:
                              case GML:        snprintf(buf,32,"%.5f,%.5f ",array->Data[n][0],array->Data[n][1]); break;
                              case JSON:
                              case JAVASCRIPT: if (n) snprintf(buf,1,","); snprintf(buf,32,"[%.5f,%.5f]",array->Data[n][0],array->Data[n][1]); break;
                           }
                           
                           Tcl_AppendResult(Interp,buf,(char*)NULL);
                        } else {
                           Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(array->Data[n][0]));
                           Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(array->Data[n][1]));
                        }
                        tr=1;
                     }
                  }
                  // If last segment was visible, add its end point
                  if (ex){
                     switch(mode) {
                        case KML:
                        case GML:        snprintf(buf,32,"%.5f,%.5f ",array->Data[n][0],array->Data[n][1]);
                                         Tcl_AppendResult(Interp,buf,(char*)NULL);
                                         break;
                        case JSON:
                        case JAVASCRIPT: snprintf(buf,32,",[%.5f,%.5f]",array->Data[n][0],array->Data[n][1]);
                                         Tcl_AppendResult(Interp,buf,(char*)NULL);
                                         break;
                        case LIST:       Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(array->Data[n][0]));
                                         Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(array->Data[n][1]));
                     }
                  }
                  if (f) {
                     switch(mode) {
                        case KML:        Tcl_AppendResult(Interp,"</coordinates></LineString>",(char*)NULL);break;
                        case GML:        Tcl_AppendResult(Interp,"</gml:coordinates></gml:LineString></gml:lineStringMember>",(char*)NULL);break;
                        case JSON:       Tcl_AppendResult(Interp,"\n\t]\n}",(char*)NULL);break;
                        case JAVASCRIPT: Tcl_AppendResult(Interp,"]",(char*)NULL);break;
                     }
                  }
                  if (mode==LIST && tr) {
                     Tcl_ListObjAppendElement(Interp,obj,sub);
                  }
                  list=list->Next;
               }

               if (mode==LIST) {
                  Tcl_SetObjResult(Interp,obj);
               }
            }
            break;

         case BOUNDARY:          
            if (Objc>2) {
               Tcl_WrongNumArgs(Interp,2,Objv,"[LIST|GML|KML|JSON|JAVASCRIPT]");
               return(TCL_ERROR);
            }
            if (Tcl_GetIndexFromObj(Interp,Objv[++i],sfrmt,"type",TCL_EXACT,&mode)!=TCL_OK) {
               return(TCL_ERROR);
            }
                                   
            obj=Tcl_NewListObj(0,NULL);
            switch(mode) {
               case KML:        Tcl_AppendResult(Interp,"<Polygon><outerBoundaryIs><LinearRing><coordinates>",(char*)NULL);break;
               case GML:        Tcl_AppendResult(Interp,"<gml:Polygon><gml:Exterior><gml:LinearRing><gml:coordinates>",(char*)NULL);break;
               case JSON:       Tcl_AppendResult(Interp,"{\n\t\"type\": \"Polygon\",\n\t\"coordinates\": [",(char*)NULL);break;
               case JAVASCRIPT: Tcl_AppendResult(Interp,"[",(char*)NULL);break;
            }
            
            i=0;j=0;
            ci=1;cj=0;
            while(1) {
 
               Field->GRef->Project(Field->GRef,i,j,&dlat,&dlon,1,1);
               switch(mode) {
                  case KML:
                  case GML:        snprintf(buf,32,"%.5f,%.5f ",dlat,dlon); 
                                   Tcl_AppendResult(Interp,buf,(char*)NULL); break;
                  case JSON:
                  case JAVASCRIPT: if (n) snprintf(buf,1,","); snprintf(buf,32,"[%.5f,%.5f],",dlat,dlon); 
                                   Tcl_AppendResult(Interp,buf,(char*)NULL); break;
                  case LIST:       Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
                                   Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon)); break;
               }
               // We loop on the gridpoints by going around the grid limits 
               if (i==Field->Def->NI-1 && ci>0) { ci=0;  cj=1; }  // Check lower right corner 
               if (j==Field->Def->NJ-1 && cj>0) { ci=-1; cj=0; }  // Check upper right corner 
               if (i==0 && ci<0)                { ci=0;  cj=-1;}  // Check upper left corner
               i+=ci;
               j+=cj;
               if (j==-1) break;
            }
            switch(mode) {
               case KML:        Tcl_AppendResult(Interp,"</coordinates></LinearRing></outerBoundaryIs></Polygon>",(char*)NULL);break;
               case GML:        Tcl_AppendResult(Interp,"</gml:coordinates></gml:LinearRing></gml:Exterior></gml:Polygon>",(char*)NULL);break;
               case JSON:       Tcl_AppendResult(Interp,"\n\t]\n}",(char*)NULL);break;
               case JAVASCRIPT: Tcl_AppendResult(Interp,"]",(char*)NULL);break;
               case LIST:       Tcl_SetObjResult(Interp,obj);
            }
            break;
               
         case LIMITS:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Field->Def->Limits[0][0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Field->Def->Limits[1][0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Field->Def->Limits[2][0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Field->Def->Limits[0][1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Field->Def->Limits[1][1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Field->Def->Limits[2][1]));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_ListObjLength(Interp,Objv[++i],&nobj);

               if (nobj==0) {
                  Field->Def->Limits[0][0]=0;
                  Field->Def->Limits[1][0]=0;
                  Field->Def->Limits[2][0]=0;
                  Field->Def->Limits[0][1]=Field->Def->NI-1;
                  Field->Def->Limits[1][1]=Field->Def->NJ-1;
                  Field->Def->Limits[2][1]=Field->Def->NK-1;
               } else {
                  if (nobj!=6) {
                     Tcl_AppendResult(Interp,"Data_Stat: Invalid number of coordinates must be { i0 j0 k0 i1 j1 k1 }",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  Tcl_ListObjIndex(Interp,Objv[i],0,&obj); Tcl_GetIntFromObj(Interp,obj,&Field->Def->Limits[0][0]);
                  Tcl_ListObjIndex(Interp,Objv[i],1,&obj); Tcl_GetIntFromObj(Interp,obj,&Field->Def->Limits[1][0]);
                  Tcl_ListObjIndex(Interp,Objv[i],2,&obj); Tcl_GetIntFromObj(Interp,obj,&Field->Def->Limits[2][0]);
                  Tcl_ListObjIndex(Interp,Objv[i],3,&obj); Tcl_GetIntFromObj(Interp,obj,&Field->Def->Limits[0][1]);
                  Tcl_ListObjIndex(Interp,Objv[i],4,&obj); Tcl_GetIntFromObj(Interp,obj,&Field->Def->Limits[1][1]);
                  Tcl_ListObjIndex(Interp,Objv[i],5,&obj); Tcl_GetIntFromObj(Interp,obj,&Field->Def->Limits[2][1]);

                  /*Check limits order*/
                  if (Field->Def->Limits[0][0]>Field->Def->Limits[0][1]) { tmpd=Field->Def->Limits[0][0];Field->Def->Limits[0][0]=Field->Def->Limits[0][1];Field->Def->Limits[0][1]=tmpd; }
                  if (Field->Def->Limits[1][0]>Field->Def->Limits[1][1]) { tmpd=Field->Def->Limits[1][0];Field->Def->Limits[1][0]=Field->Def->Limits[1][1];Field->Def->Limits[1][1]=tmpd; }
                  if (Field->Def->Limits[2][0]>Field->Def->Limits[2][1]) { tmpd=Field->Def->Limits[2][0];Field->Def->Limits[2][0]=Field->Def->Limits[2][1];Field->Def->Limits[2][1]=tmpd; }

                  /*Check limit limits*/
                  if (Field->Def->Limits[0][0]<0) Field->Def->Limits[0][0]=0.0;
                  if (Field->Def->Limits[1][0]<0) Field->Def->Limits[1][0]=0.0;
                  if (Field->Def->Limits[2][0]<0) Field->Def->Limits[2][0]=0.0;
                  if (Field->Def->Limits[0][1]>Field->Def->NI-1) Field->Def->Limits[0][1]=Field->Def->NI-1;
                  if (Field->Def->Limits[1][1]>Field->Def->NJ-1) Field->Def->Limits[1][1]=Field->Def->NJ-1;
                  if (Field->Def->Limits[2][1]>Field->Def->NK-1) Field->Def->Limits[2][1]=Field->Def->NK-1;
               }
               Data_Clean(Field,1,0,1);
            }
            break;

         case COORDLIMITS:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Def->CoordLimits[1][0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Def->CoordLimits[0][0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Def->CoordLimits[1][1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Def->CoordLimits[0][1]));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_ListObjLength(Interp,Objv[++i],&nobj);

               if (nobj==0) {
                  Field->Def->CoordLimits[0][0]=-180;
                  Field->Def->CoordLimits[1][0]=-90;
                  Field->Def->CoordLimits[0][1]=180;
                  Field->Def->CoordLimits[1][1]=90;
               } else {
                  if (nobj!=4) {
                     Tcl_AppendResult(Interp,"Data_Stat: Invalid number of coordinates must be { lat0 lon0 lat1 lon1 }",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  Tcl_ListObjIndex(Interp,Objv[i],0,&obj); Tcl_GetDoubleFromObj(Interp,obj,&Field->Def->CoordLimits[1][0]);
                  Tcl_ListObjIndex(Interp,Objv[i],1,&obj); Tcl_GetDoubleFromObj(Interp,obj,&Field->Def->CoordLimits[0][0]);
                  Tcl_ListObjIndex(Interp,Objv[i],2,&obj); Tcl_GetDoubleFromObj(Interp,obj,&Field->Def->CoordLimits[1][1]);
                  Tcl_ListObjIndex(Interp,Objv[i],3,&obj); Tcl_GetDoubleFromObj(Interp,obj,&Field->Def->CoordLimits[0][1]);

                  if (Field->Def->CoordLimits[1][0]<-90.0) Field->Def->CoordLimits[1][0]=-90.0;
                  if (Field->Def->CoordLimits[0][0]<-180.0) Field->Def->CoordLimits[0][0]=-180.0;
                  if (Field->Def->CoordLimits[1][1]>90.0) Field->Def->CoordLimits[1][1]=90.0;
                  if (Field->Def->CoordLimits[0][1]>180.0) Field->Def->CoordLimits[0][1]=180.0;
               }
               Data_Clean(Field,0,0,1);
            }
            break;

         case SAMPLE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Field->Def->Sample));\
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Field->Def->Sample);
               Field->Def->Sample=Field->Def->Sample<1?1:Field->Def->Sample;
            }
            break;

         case HEIGHT:
            if (Objc<4) {
               Tcl_AppendResult(Interp,"Data_Stat: Invalid number of coordinates must be I J K",(char*)NULL);
               return(TCL_ERROR);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&dx);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&dy);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&dv);

               val=0.0;
               if (Field->GRef->Height) {
                  val=Field->GRef->Height(Field->GRef,Field->ZRef,dx,dy,dv);
               }
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(val));
            }
            break;

         case LEVELS:
            if (Objc==1) {
                if (Field->GRef) {
                 // If xsection/profile, reload vertical descriptor (COLUMN model)
#ifdef HAVE_RMN
                  if (Field->GRef->Grid[0]=='V' && Field->Spec->Extrude && strlen(Field->Spec->Extrude)) {
                     if (FSTD_FileSet(NULL,((TRPNHeader*)Field->Head)->File)>=0) {
                        FSTD_FieldReadVLevels(Field);
                        FSTD_FileUnset(NULL,((TRPNHeader*)Field->Head)->File);
                     }
                  }
#endif
                  // Use NJ for xsection since NK and ZRefs levelnb are not set
                  nb=(Field->GRef && Field->GRef->Grid[0]=='V')?Field->Def->NJ:Field->Def->NK;

                  obj=Tcl_NewListObj(0,NULL);
                  for (index=0;index<nb;index++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->ZRef->Levels?Field->ZRef->Levels[index]:index));
                  }
                  Tcl_SetObjResult(Interp,obj);
               }
            } else {
               nb=(Field->GRef && Field->GRef->Grid[0]=='V')?Field->Def->NJ:Field->Def->NK;

               Tcl_ListObjLength(Interp,Objv[++i],&nobj);

               if (nobj>nb) {
                  Tcl_AppendResult(Interp,"Data_Stat: Too many levels",(char*)NULL);
                  return(TCL_ERROR);
               }
               if (!(levels=(float*)malloc(nobj*sizeof(float)))) {
                  Tcl_AppendResult(Interp,"Data_Stat: Unable to allocate level buffer",(char*)NULL);
                  return(TCL_ERROR);
               }
               for(index=0;index<nobj;index++) {
                  Tcl_ListObjIndex(Interp,Objv[i],index,&obj);
                  Tcl_GetDoubleFromObj(Interp,obj,&dv);
                  levels[index]=dv;
               }
#ifdef HAVE_RMN
               ((TRPNHeader*)Field->Head)->IP1=-1;
#endif
               zref=Field->ZRef;
               Field->ZRef=ZRef_Define(Field->ZRef->Type,Field->Def->NK,levels);
               ZRef_Free(zref);
               free(levels);
            }
            break;

         case LEVEL:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Field->ZRef->Levels[Field->Def->Level]));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&dv);
               Field->ZRef->Levels[Field->Def->Level]=dv;
#ifdef HAVE_RMN
               ((TRPNHeader*)Field->Head)->IP1=-1;
#endif
            }
            break;

         case LEVELINDEX:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Field->Def->Level));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&n);
               if (n<0 || n>=Field->ZRef->LevelNb) {
                  Tcl_AppendResult(Interp,"Data_Stat: Invalid level index",(char*)NULL);
                  return(TCL_ERROR);
               }
               if (n!=Field->Def->Level) {
                  if (Field->ZRef->Type==LVL_ANGLE) {
                     Field->GRef->CTH=cos(DEG2RAD(Field->ZRef->Levels[n]));
                     Field->GRef->STH=sin(DEG2RAD(Field->ZRef->Levels[n]));
                  }
                  Field->Def->Level=n;

                  /*For contours, we'll have to recalculate them*/
                  if (Field->Spec->RenderContour)
                     Data_Clean(Field,0,0,1);
               }
            }
            break;

         case LEVELTYPE:
            lvls=ZRef_LevelNames();

            if (Objc==1) {
               if (Field->GRef)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(lvls[Field->ZRef->Type],-1));
            } else {
               if (Tcl_GetIndexFromObj(Interp,Objv[++i],lvls,"type",0,&index)!=TCL_OK) {
                  return(TCL_ERROR);
               }
               zref=Field->ZRef;
               Field->ZRef=ZRef_Define(index,Field->ZRef->LevelNb,Field->ZRef->Levels);
               ZRef_Free(zref);
            }
            break;

         case PRESSURELEVELS:
            if (Objc==1) {
               if (Field->GRef && Field->GRef->Hgt) {
                  obj=Tcl_NewListObj(0,NULL);
                  if (Field->GRef->Grid[0]=='V') {
                     min=1e32;
                     max=-1e32;
                     for (index=0;index<Field->Def->NI*Field->Def->NJ;index++) {
                        min=min<Field->GRef->Hgt[index]?min:Field->GRef->Hgt[index];
                        max=max>Field->GRef->Hgt[index]?max:Field->GRef->Hgt[index];
                     }
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(max));
                     for (index=Field->Def->NI;index<Field->Def->NI*(Field->Def->NJ-1);index+=Field->Def->NI) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->GRef->Hgt[index]));
                     }
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(min));
                  } else {
                     for (index=0;index<Field->Def->NK;index++) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->GRef->Hgt[index]));
                     }
                  }
                  Tcl_SetObjResult(Interp,obj);
               }
            }
            break;

         case METERLEVELS:
            if (Objc==1) {
               if (Field->GRef && Field->GRef->Hgt) {
                  obj=Tcl_NewListObj(0,NULL);
                  if (Field->GRef->Grid[0]=='V') {
                     min=1e32;
                     max=-1e32;
                     for (index=0;index<Field->Def->NI*Field->Def->NJ;index++) {
                        min=min<Field->GRef->Hgt[index]?min:Field->GRef->Hgt[index];
                        max=max>Field->GRef->Hgt[index]?max:Field->GRef->Hgt[index];
                     }
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(min));
                     for (index=Field->Def->NI;index<Field->Def->NI*(Field->Def->NJ-1);index+=Field->Def->NI) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->GRef->Hgt[index]));
                     }
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(max));
                  } else {
                     for (index=0;index<Field->Def->NK;index++) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->GRef->Hgt[index]));
                     }
                  }
                  Tcl_SetObjResult(Interp,obj);
               }
            }
            break;

         case MATRIX:
            if (Objc!=2) {
               Tcl_WrongNumArgs(Interp,2,Objv,"matrix var");
               return(TCL_ERROR);
            } else {
               i++;
               Tcl_UnsetVar2(Interp,Tcl_GetString(Objv[i]),NULL,0x0);
               for(ni=Field->Def->Limits[0][0];ni<=Field->Def->Limits[0][1];ni++) {
                  for(nj=Field->Def->Limits[1][0];nj<=Field->Def->Limits[1][1];nj++) {
                     sprintf(buf,"%i,%i",nj+1,ni+1);
                     Def_Get(Field->Def,0,FIDX2D(Field->Def,ni,nj),val);
                     switch(Field->Def->Type) {
                        case TD_Unknown:
                        case TD_Binary: break;
                        case TD_UInt64:
                        case TD_Int64: Tcl_SetVar2Ex(Interp,Tcl_GetString(Objv[i]),buf,Tcl_NewLongObj(val),0x0); break;
                        case TD_UByte:
                        case TD_Byte:
                        case TD_UInt16:
                        case TD_Int16:
                        case TD_UInt32:
                        case TD_Int32:   Tcl_SetVar2Ex(Interp,Tcl_GetString(Objv[i]),buf,Tcl_NewIntObj(val),0x0);break;
                        case TD_Float32:
                        case TD_Float64: Tcl_SetVar2Ex(Interp,Tcl_GetString(Objv[i]),buf,Tcl_NewDoubleObj(val),0x0);
                     }
                  }
               }
            }
            break;

         case MASK:
            if (Objc==1) {
               Tcl_WrongNumArgs(Interp,2,Objv,"fld");
               return(TCL_ERROR);
            } else {
               if ((fld=Data_Get(Tcl_GetString(Objv[++i])))) {
                  if (fld->Def->NI==Field->Def->NI && fld->Def->NJ==Field->Def->NJ) {
                     if (!Field->Def->Mask) {
                        if (!(Field->Def->Mask=(char*)malloc(FSIZE2D(Field->Def)))) {
                           Tcl_AppendResult(Interp,"Data_Stat: Unable to allocate mask",(char*)NULL);
                           return(TCL_ERROR);
                        }
                     }
                     for(ni=0;ni<FSIZE2D(fld->Def);ni++) {
                        Def_Get(fld->Def,0,ni,val);
                        Field->Def->Mask[ni]=(val!=0.0);
                     }
                  } else {
                     Tcl_AppendResult(Interp,"Data_Stat: Mask and data dimensions don't fit",(char*)NULL);
                     return(TCL_ERROR);
                  }
               } else {
                  if (strlen(Tcl_GetString(Objv[i]))==0) {
                     if (Field->Def->Mask) {
                        free(Field->Def->Mask);
                     }
                     Field->Def->Mask=NULL;
                  } else {
                     Tcl_AppendResult(Interp,"Data_Stat: Invalid field",(char*)NULL);
                     return(TCL_ERROR);
                  }
               }
            }
            break;

         case CELLDIM:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Field->Def->CellDim));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Field->Def->CellDim);
            }
            break;

         case DATACOPY :
            if (Objc<2) {
               Tcl_WrongNumArgs(Interp,1,Objv,"fldfrom");
               return TCL_ERROR;
            } else {
               int  delta=0;
               TData *field0=Data_Get(Tcl_GetString(Objv[++i]));
               if (!field0) {
                  Tcl_AppendResult(Interp,"invalid field: ",Tcl_GetString(Objv[i]),(char*)NULL);
                  return(TCL_ERROR);
               }
               if (Objc==3) {
                  Tcl_GetIntFromObj(Interp,Objv[++i],&delta);
               }
               return FSTD_FieldDataCopy( Interp, Field, field0, delta );
            }
         break;
#ifdef HAVE_RMN
         case POFF:
            if (Objc==1) {
               if (Field->ZRef->Version==-1) {
                  FSTD_DecodeRPNLevelParams(Field);
               }
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Field->ZRef->POff));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmpd);
               Field->ZRef->POff=tmpd;
            }
            break;
            
         case TOP:
            if (Objc==1) {
               if (Field->ZRef->Version==-1) {
                  FSTD_DecodeRPNLevelParams(Field);
               }
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Field->ZRef->PTop));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmpd);
               Field->ZRef->PTop=tmpd;

               // If 0, force ZRef refresh
               Field->ZRef->Version=(tmpd==0.0)?-1:0;
            }
            break;

         case REF:
            if (Objc==1) {
               if (Field->ZRef->Version==-1) {
                  FSTD_DecodeRPNLevelParams(Field);
               }
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Field->ZRef->PRef));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmpd);
               Field->ZRef->PRef=tmpd;

               // If 0, force ZRef refresh
               Field->ZRef->Version=(tmpd==0.0)?-1:0;
            }
            break;

         case COEF:
            if (Objc==1) {
               if (Field->ZRef->Version==-1) {
                  FSTD_DecodeRPNLevelParams(Field);
               }
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->ZRef->RCoef[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->ZRef->RCoef[1]));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_ListObjLength(Interp,Objv[++i],&nobj);
               if (nobj>1) {
                  Tcl_ListObjIndex(Interp,Objv[i],0,&obj);
                  Tcl_GetDoubleFromObj(Interp,obj,&tmpd);
                  Field->ZRef->RCoef[0]=tmpd;
                  Tcl_ListObjIndex(Interp,Objv[i],1,&obj);
                  Tcl_GetDoubleFromObj(Interp,obj,&tmpd);
                  Field->ZRef->RCoef[1]=tmpd;
               } else {
                  Tcl_GetDoubleFromObj(Interp,Objv[i],&tmpd);
                  Field->ZRef->RCoef[0]=tmpd;
               }
            }
            break;
#endif
      }
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_GetAreaValue>
 * Creation : Juin 2012 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait des valeurs pour une region latlon ou un polygone latlon
 *
 * Parametres  :
 *  <Interp>   : Interpreteur TCL
 *  <Field>    : Pointeur sur le champs
 *  <Mode>     : Type d'extraction (0:Average,1:Somme,2:Minimum,3:Maximum,4:Points de grilles,5:Values)
 *  <Objc>     : Nombre d'arguments
 *  <Objv>     : Liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_GetAreaValue(Tcl_Interp *Interp,int Mode,TData *Field,int Objc,Tcl_Obj *CONST Objv[]) {

   Tcl_Obj *obj=NULL,*sub=NULL;
   int      f,nt,n=0,ni,nj,nc,vnb=0,vn0,vn1,nid,pnid;
   double   v,dlat,dlon=0.0,dlat0,dlat1,dlon0,dlon1,tot=0.0,i0,j0,i1,j1;
   Vect3d   vp,*vn=NULL;
   TVector *vec=NULL;

   if (Objc!=1 && Objc!=2) {
      Tcl_WrongNumArgs(Interp,2,Objv,"{ lat0 lon0 lat1 lon1 } | { coords } [vector]");
      return(TCL_ERROR);
   }

   // Get result vector if passed in (for use in values mode)
   if (Objc==2 && Mode==5) {
      vec=Vector_Get(Tcl_GetString(Objv[1]));
      if (!vec) {
         Tcl_AppendResult(Interp,"Invalid vector",(char*)NULL);
         return(TCL_ERROR);
      }
      Vector_Clear(Interp,vec);
   }
   
   // Get coordinate list
   Tcl_ListObjLength(Interp,Objv[0],&nc);
   
   // Initialise counter
   nt=0;
   switch(Mode) {
      case 0:
      case 1: tot=0.0; break;
      case 2: tot=1e38; break;
      case 3: tot=-1e38; break;
      case 4:
      case 5: if (!vec) obj=Tcl_NewListObj(0,NULL); break;
   }

   pnid=Field->GRef->NId;

   for(nid=(pnid?pnid:(Field->GRef->NbId>1?1:0));nid<=(pnid?pnid:(Field->GRef->NbId>1?Field->GRef->NbId:0));nid++) {
#ifdef HAVE_RMN
      FSTD_FieldSubSelect(Field,nid);
#endif   
      if (nc==4) {
         // This is a latlon bounding box defined by 2 corners
         Tcl_ListObjIndex(Interp,Objv[0],0,&sub);
         Tcl_GetDoubleFromObj(Interp,sub,&dlat0);
         Tcl_ListObjIndex(Interp,Objv[0],1,&sub);
         Tcl_GetDoubleFromObj(Interp,sub,&dlon0);
         Tcl_ListObjIndex(Interp,Objv[0],2,&sub);
         Tcl_GetDoubleFromObj(Interp,sub,&dlat1);
         Tcl_ListObjIndex(Interp,Objv[0],3,&sub);
         Tcl_GetDoubleFromObj(Interp,sub,&dlon1);

         if (dlon0==dlon1 || dlat0==dlat1) {
            Tcl_AppendResult(Interp,"Data_GetAreaValue: Bad range coordinates",(char*)NULL);
            return(TCL_ERROR);            
         }
         
         // Order values in latitude only since longitude order gives the selection orientation
         if (dlat0>dlat1) { v=dlat0; dlat0=dlat1; dlat1=v; };

         // Get ij bounding box
         GeoRef_BoundingBox(Field->GRef,dlat0,dlon0,dlat1,dlon1,&i0,&j0,&i1,&j1);

         // If Wrapover 180/-180, need to check all gridpoint along longitude
         if (Field->GRef->Type&GRID_WRAP) {
            i0=Field->GRef->X0;
            i1=Field->GRef->X1;
         }
         // TODO:Test without bbox pre-selection
         i0=Field->GRef->X0;
         i1=Field->GRef->X1;
         j0=Field->GRef->Y0;
         j1=Field->GRef->Y1;
      } else {
         vnb=nc>>1;
         if (!vnb || nc%2) {
            Tcl_AppendResult(Interp,"Data_GetAreaValue: Invalid number of coordinates",(char*)NULL);
            return(TCL_ERROR);
         }
         i0=j0=1000000;
         i1=j1=-1000000;

         if (!vn && !(vn=(Vect3d*)malloc(vnb*sizeof(Vect3d)))) {
            Tcl_AppendResult(Interp,"Data_GetAreaValue: Unable to allocate memory for temporary buffer",(char*)NULL);
            return(TCL_ERROR);
         }

         for(n=0,ni=0;n<vnb;n++) {
            Tcl_ListObjIndex(Interp,Objv[0],ni++,&sub);
            Tcl_GetDoubleFromObj(Interp,sub,&dlat);
            Tcl_ListObjIndex(Interp,Objv[0],ni++,&sub);
            Tcl_GetDoubleFromObj(Interp,sub,&dlon);

            Field->GRef->UnProject(Field->GRef,&vn[n][0],&vn[n][1],dlat,dlon,1,1);
            vn[n][2]=0.0;
            i0=i0<vn[n][0]?i0:vn[n][0];
            j0=j0<vn[n][1]?j0:vn[n][1];
            i1=i1>vn[n][0]?i1:vn[n][0];
            j1=j1>vn[n][1]?j1:vn[n][1];
         }
      }

      if (Field->GRef->Grid[0]!='V') {

         // Loop on ij bounding box
         for (ni=floor(i0);ni<=ceil(i1);ni++) {
            for (nj=floor(j0);nj<=ceil(j1);nj++) {
               if (nc==4) {
                  // Range case
                  Field->GRef->Project(Field->GRef,ni,nj,&dlat,&dlon,0,1);
                  
                  // Adjust for 0-360 if needed
                  if (dlon0>180 || dlon1>180) dlon=dlon<0?dlon+360.0:dlon;

                  f=0;
                  if (dlat>=dlat0 && dlat<=dlat1) {
                     if (dlon0<dlon1) {
                        if (dlon>=dlon0 && dlon<=dlon1) {
                           f=1;
                        }
                     } else {
                        if ((dlon>=dlon0 && dlon<=180) || (dlon<=dlon1 && dlon>=-180)) {
                           f=1;
                        }
                     }
                  }
               } else {
                  // Polygon case
                  Vect_Init(vp,ni,nj,0.0);

                  f=0;
                  for(vn0=0,vn1=vnb-1;vn0<vnb;vn1=vn0++) {
                     /*Check for point insidness*/
                     if (OGR_PointInside(vp,vn[vn0],vn[vn1])) {
                        f=!f;
                     }
                  }
               }

               // Point is inside
               if (f) {
                  f=FIDX2D(Field->Def,ni,nj);
                  if (Field->Def->Mask && !Field->Def->Mask[f]) {
                     continue;
                  }
                  
                  Def_GetMod(Field->Def,f,v);
                  nt++;
                  switch(Mode) {
                     case 0:
                     case 1: tot+=v; break;
                     case 2: tot=tot>v?v:tot; break;
                     case 3: tot=tot<v?v:tot; break;
                     case 4:
                        sub=Tcl_NewListObj(0,NULL);
                        Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(ni));
                        Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(nj));
                        Tcl_ListObjAppendElement(Interp,obj,sub);
                        break;
                     case 5:
                        if (vec) {
                           Vector_AppendData(Interp,vec,NULL,v);
                        } else {
                           Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(v));
                        }
                        break;
                  }
               }
            }
         }
      } else {
         for (ni=0;ni<Field->Def->NI;ni++) {
            f=0;
            if (nc==4) {
               //Range case
               if (Field->GRef->AY[ni]>=dlat0 && Field->GRef->AY[ni]<=dlat1) {
                  if (dlon0<dlon1) {
                     if (Field->GRef->AX[ni]>=dlon0 && Field->GRef->AX[ni]<=dlon1) {
                        f=1;
                     }
                  } else {
                     if ((Field->GRef->AX[ni]>=dlon0 && Field->GRef->AX[ni]<=180) || (Field->GRef->AX[ni]<=dlon1 && Field->GRef->AX[ni]>=-180)) {
                        f=1;
                     }
                  }
               }
            } else {
               //Polygon case
            }

            // Point is inside
            if (f) {
               Def_GetMod(Field->Def,ni,v);
               nt++;
               switch(Mode) {
                  case 0:
                  case 1: tot+=v; break;
                  case 2: tot=tot>v?v:tot; break;
                  case 3: tot=tot<v?v:tot; break;
                  case 4:
                     sub=Tcl_NewListObj(0,NULL);
                     Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(ni));
                     Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(0));
                     Tcl_ListObjAppendElement(Interp,obj,sub);
                     break;
                  case 5:
                     if (vec) {
                        Vector_AppendData(Interp,vec,NULL,v);
                     } else {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(v));
                     }
                     break;
               }
            }
         }
      }
   }
   #ifdef HAVE_RMN
   FSTD_FieldSubSelect(Field,pnid);
   #endif
   
   // Finalize calculations
   switch(Mode) {
      case 0: tot/=nt;
      case 1:
      case 2:
      case 3: obj=Tcl_NewDoubleObj(tot);
   }
   if (!vec)
      Tcl_SetObjResult(Interp,obj);

   if (vn) free(vn);

   return(TCL_OK);
}

void Data_FromString(char *String,TDef *Def,int Comp,int Idx) {

   switch(Def->Type) {
      case TD_Unknown:break;
      case TD_Binary: break;
      case TD_Byte:   ((char*)Def->Data[Comp])[Idx]=atoi(String); break;
      case TD_UByte:  ((unsigned char*)Def->Data[Comp])[Idx]=atoi(String);break;
      case TD_Int16:  ((short*)Def->Data[Comp])[Idx]=atoi(String);break;
      case TD_UInt16: ((unsigned short*)Def->Data[Comp])[Idx]=atoi(String);break;
      case TD_Int32:  ((int*)Def->Data[Comp])[Idx]=atoi(String);break;
      case TD_UInt32: ((unsigned int*)Def->Data[Comp])[Idx]=atoi(String);break;
      case TD_Int64:  ((long*)Def->Data[Comp])[Idx]=atol(String);break;
      case TD_UInt64: ((unsigned long*)Def->Data[Comp])[Idx]=atol(String);break;
      case TD_Float32:((float*)Def->Data[Comp])[Idx]=atof(String);break;
      case TD_Float64:((double*)Def->Data[Comp])[Idx]=atof(String);break;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_ValGetMatrix>
 * Creation : Juillet 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait les donnees du champs et les retourne a Tcl en liste
 *            de ligne (NJ ligne de NI donnees)
 *
 * Parametres  :
 *  <Interp>   : Interpreteur TCL
 *  <Field>    : Pointeur sur le champs
 *  <Component>: Composante
 *  <Flip>     : Flip x/y axis
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_ValGetMatrix(Tcl_Interp *Interp,TData *Field,int Component,int Flip){

   int      i,j;
   double   val=0.0;
   Tcl_Obj *objj,*obji;

   if (Component<0) {
      objj=Tcl_NewByteArrayObj((unsigned char*)Field->Def->Data[0],FSIZE3D(Field->Def)*TDef_Size[Field->Def->Type]);
   } else {
      objj=Tcl_NewListObj(0,NULL);
      if (Component<Field->Def->NC) {
         if (Flip) {
            for(i=0;i<Field->Def->NI;i++){
               obji=Tcl_NewListObj(0,NULL);
               for(j=0;j<Field->Def->NJ;j++){
                  Def_Get(Field->Def,Component,j*Field->Def->NI+i,val);
                  Tcl_ListObjAppendElement(Interp,obji,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,val)));
               }
               Tcl_ListObjAppendElement(Interp,objj,obji);
            }
         } else {
            for(j=0;j<Field->Def->NJ;j++){
               obji=Tcl_NewListObj(0,NULL);
               for(i=0;i<Field->Def->NI;i++){
                  Def_Get(Field->Def,Component,j*Field->Def->NI+i,val);
                  Tcl_ListObjAppendElement(Interp,obji,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,val)));
               }
               Tcl_ListObjAppendElement(Interp,objj,obji);
            }
         }
      }
   }
   Tcl_SetObjResult(Interp,objj);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_ValPutMatrix>
 * Creation : Septembre 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Inserer une liste de valeur dans un champs.
 *
 * Parametres  :
 *  <Interp>   : Interpreteur TCL
 *  <Field>    : Pointeur sur les donnees du champs
 *  <Component>: Composante
 *  <Data>     : Matrice de valeurs (Liste ou binaire)
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *   La matrice en question peut etre:
 *      - une serie de listes imbriquees comprenant NJ listes de NI elements (2D).
 *      - un objet bytearray (2D ou 3D)
 *----------------------------------------------------------------------------
*/
int Data_ValPutMatrix(Tcl_Interp *Interp,TData *Field,int Component,Tcl_Obj *Data){

   Tcl_Obj *objj,*obji;
   int      i,j,nobjj,nobji;
   double   value;
   unsigned char *data;

   if (Component>=Field->Def->NC) {
      Tcl_AppendResult(Interp,"Data_ValPutMatrix: Invalid component index",(char*)NULL);
      return(TCL_ERROR);
   }
   if (Component<0) Component=0;
   
   if (strcmp(Data->typePtr->name,"bytearray")==0) {
      data=Tcl_GetByteArrayFromObj(Data,&nobjj);
      if (nobjj>(FSIZE3D(Field->Def)*TDef_Size[Field->Def->Type])) {
         Tcl_AppendResult(Interp,"Data_ValPutMatrix: Array too big",(char*)NULL);
         return(TCL_ERROR);             
      }
      memcpy(Field->Def->Data[Component],data,nobjj);
   } else {
      // Extraire les nj lignes de donnees de la liste bidimensionnelle
      Tcl_ListObjLength(Interp,Data,&nobjj);

      for (j=0;j<nobjj;j++){

         // Extraire les ni points de la nj ieme ligne
         Tcl_ListObjIndex(Interp,Data,j,&objj);
         Tcl_ListObjLength(Interp,objj,&nobji);

         if (!j) {
            if (nobji*nobjj>Field->Def->NIJ) {
               Tcl_AppendResult(Interp,"Data_ValPutMatrix: Too many values",(char*)NULL);
               return(TCL_ERROR);             
            }
         }
         
         // Assigner les valeurs ni de la nj ieme ligne
         for (i=0;i<nobji;i++){
            Tcl_ListObjIndex(Interp,objj,i,&obji);
            
            Tcl_GetDoubleFromObj(Interp,obji,&value);
            value=SPEC2VAL(Field->Spec,value);
            Def_Set(Field->Def,Component,j*nobji+i,value);
         }
      }
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_ValSet>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Modifier la valeur du champs a une position I,J.
 *
 * Parametres :
 *  <Field>   : Pointeur sur une structure de champ.
 *  <I>       : Coordonnee en I dans le champs
 *  <J>       : Coordonnee en J dans le champs
 *  <Val>     : Valeur a assigner
 *
 * Retour     :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *   -La valeur sera interpolee selon les parametres du champs et peut
 *    donc affecter plus d'un point
 *
 *----------------------------------------------------------------------------
*/
int Data_ValSet(TData *Field,double I,double J,double Val) {

   float         dx0,dx1,dy0,dy1;
   unsigned long x0,y0,idx;
   int           i,j;

   if (I<0 || I>Field->Def->NI-1 || J<0 || J>Field->Def->NJ-1)
      return 0;

   if (Field->Stat) {
      Data_StatFree(Field->Stat);
      Field->Stat=NULL;
   }

   i=lrint(I);
   j=lrint(J);
   idx=FIDX3D(Field->Def,i,j,Field->Def->Level);
   Val=SPEC2VAL(Field->Spec,Val);
   Def_Set(Field->Def,0,idx,Val);
   return 1;

   if (Field->Spec->InterpDegree[0]=='N') {
      Field->Def->Data[0][j*Field->Def->NI+i]=Val;
   } else {
      x0=floor(I);
      y0=floor(J);

      if (x0<0 || x0>Field->Def->NI-2 || y0<0 || y0>Field->Def->NJ-2) {
         return 0;
      } else {
         dx1=I-floor(I);
         dx0=1.0-dx1;
         dy1=J-floor(J);
         dy0=1.0-dy1;
         Field->Def->Data[0][y0    *Field->Def->NI+x0]    =dy0*dx0*Val;
         Field->Def->Data[0][(y0+1)*Field->Def->NI+x0]    =dy1*dx0*Val;
         Field->Def->Data[0][y0    *Field->Def->NI+(x0+1)]=dy0*dx1*Val;
         Field->Def->Data[0][(y0+1)*Field->Def->NI+(x0+1)]=dy1*dx1*Val;
         return 1;
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Data_AppendValueObj>
 * Creation     : Novembre 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Recuperer la valeur d'une position dans une bance.
 *
 * Parametres  :
 *   <Band>     : Bande a afficher
 *   <X>        : Position en X
 *   <Y>        : Position en Y
 *   <Z>        : Position en Z
 *   <Val>      : Valeur de retour
 *
 * Retour       :
 *   <int>      : Interieur ou exterieur de la bande
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
Tcl_Obj* Data_Val2Obj(TDef *Def,double Val) {

   Tcl_Obj *obj=NULL;

   switch(Def->Type) {
      case TD_Unknown:
      case TD_Binary:break;
      case TD_UByte:
      case TD_Byte:
      case TD_UInt16:
      case TD_Int16:
      case TD_UInt32:
      case TD_Int32:   obj=Tcl_NewIntObj(Val);break;
      case TD_UInt64:
      case TD_Int64:   obj=Tcl_NewLongObj(Val);break;
      case TD_Float32:
      case TD_Float64: obj=Tcl_NewDoubleObj(Val);
   }
   return(obj);
}

Tcl_Obj* Data_AppendValueObj(Tcl_Interp *Interp,TDef *Def,int X,int Y) {

   Tcl_Obj *obj;
   int      i;
   double   val=0.0;

   obj=Tcl_NewListObj(0,NULL);
   for(i=0;i<Def->NC;i++) {
      if (X>=0 && X<Def->NI && Y>=0 && Y<Def->NJ) {
         Def_Get(Def,i,FIDX2D(Def,X,Y),val);
         Tcl_ListObjAppendElement(Interp,obj,Data_Val2Obj(Def,val));
      } else {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
      }
   }
   return(obj);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_IndexInit>
 * Creation : Octobre 2016 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cr??er ou r??cup??rer un index d'une variable Tcl
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Obj>     : Pointeur sur objet TCL contenant le nom de la variable
 *  <Size>    : Dimension de l'index
 *
 * Retour     :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
float* Data_IndexInit(Tcl_Interp *Interp,Tcl_Obj **Obj,unsigned long Size) {
   
   Tcl_Obj *item=NULL;
   float   *index=NULL;
   int      len=0;
   
   // Check for index array
   if (*Obj) {
      item=Tcl_ObjGetVar2(Interp,*Obj,NULL,0x0);
      if (!item) {
         // Got an empty variable, will fill it with index (Tcl's max is 2Gb so clamp to this value and pray it's enough)
         if ((item=Tcl_NewByteArrayObj(NULL,FMIN(Size*sizeof(float),INT_MAX>>1)))) {
            index=(float*)Tcl_GetByteArrayFromObj(item,&len);
            if (!index || !len) {
               *Obj=NULL;
               index=NULL;
               App_Log(WARNING,"%s: Unable to allocate index array, will not produce and index",__func__);
            } else {
               index[0]=DEF_INDEX_EMPTY;
               *Obj=Tcl_ObjSetVar2(Interp,*Obj,NULL,item,0x0);
                Tcl_IncrRefCount(*Obj);
            }
         } else {
            *Obj=NULL;
            App_Log(WARNING,"%s: Unable to allocate index array, will not produce and index",__func__);
         }
      } else {
         // Got a filled variable, will use it's index
         *Obj=NULL;
         index=(float*)Tcl_GetByteArrayFromObj(item,NULL);
      }
   }
   return(index);
}

char** Data_IndexInitPtr(Tcl_Interp *Interp,Tcl_Obj **Obj) {
   
   Tcl_Obj *item=NULL;
   char   **index=NULL;
   
   // Check for index array
   if (*Obj) {
      item=Tcl_ObjGetVar2(Interp,*Obj,NULL,0x0);
      if (!item) {
         // Got an empty variable, will fill it with index
         if ((item=Tcl_NewByteArrayObj(NULL,sizeof(char*)))) {
            index=(char**)Tcl_GetByteArrayFromObj(item,NULL);
            index[0]=(char*)0x1;
            *Obj=Tcl_ObjSetVar2(Interp,*Obj,NULL,item,0x0);
             Tcl_IncrRefCount(*Obj);
         } else {
            *Obj=NULL;
            App_Log(WARNING,"%s: Unable to allocate index array, will not produce and index",__func__);
         }
      } else {
         // Got a filled variable, will use it's index
         *Obj=NULL;
         index=(char**)Tcl_GetByteArrayFromObj(item,NULL);
      }
   }
   return(index);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_IndexResize>
 * Creation : Octobre 2016 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Redimensionner un index d'une variable Tcl
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Obj>     : Pointeur sur objet TCL contenant le nom de la variable
 *  <Size>    : Dimension de l'index
 *
 * Retour     :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
unsigned long Data_IndexResize(Tcl_Interp *Interp,Tcl_Obj **Obj,unsigned long Size) {

   Tcl_Obj *item=NULL;

   if (*Obj) {
      if ((item=Tcl_ObjGetVar2(Interp,*Obj,NULL,0x0))) {
         Tcl_SetByteArrayLength(item,Size*sizeof(float));
         return(Size);
      }
   }
   return(0);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Within>
 * Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Determiner si une valeur est a l'interieur des limites d'affichage du champs.
 *
 * Parametres :
 *  <Field>   : Pointeur sur une structure de champ.
 *  <Val>     : Valeur
 *
 * Retour     :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_Within(TData *Field,float Val) {

   if (Field->Spec->InterNb) {
      if (Val>=Field->Spec->Inter[0])
         return(1);
   } else {
      if ((Val>=Field->Spec->Min || Field->Spec->MapBellow) && (Val<=Field->Spec->Max || Field->Spec->MapAbove))
         return(1);
   }
   return(0);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_WithinNb>
 * Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Determiner le nombre de valeur qui sont a l'interieur des limites d'affichage du champs.
 *
 * Parametres :
 *  <Field>   : Pointeur sur une structure de champ.
 *  <Val>     : Valeur
 *
 * Retour     :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_WithinNb(TData *Field) {

   int   n,t=0;
   float val=0.0f;

   for(n=0;n<FSIZE2D(Field->Def);n++) {
      Def_GetMod(Field->Def,n,val);
      if (Field->Spec->InterNb) {
         if (val>=Field->Spec->Inter[0])
            t++;
      } else {
         if ((val>=Field->Spec->Min || Field->Spec->MapBellow) && (val<=Field->Spec->Max || Field->Spec->MapAbove))
            t++;
      }
   }
   return(t);
}
