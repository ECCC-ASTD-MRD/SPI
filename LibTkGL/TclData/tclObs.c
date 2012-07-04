/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers d'observations
 * Fichier      : tclObs.c
 * Creation     : Fevrier 2003 - J.P. Gauthier
 *
 * Description  : Fichier d'entete du module Obs.
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
 *
 *=========================================================
 */

#include "tclObs.h"
#include "Projection.h"

#define fgetskip(BYTES,LEN,STREAM)   BYTES[0]='\0';while (fgets(BYTES,LEN,STREAM) && BYTES[0]=='#')

/* HashTable Tcl pour les observations */
static Tcl_HashTable ObsTable;
static int ObsInit=0;
static int ObsNo=0;

static int Obs_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int Obs_Create(Tcl_Interp *Interp,char* Name);
static int Obs_Config(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int Obs_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int Obs_Stat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int Obs_FreeHash(Tcl_Interp *Interp,char *Name);

int  Obs_RenderIcon(Tcl_Interp *Interp,TObs *Obs,ViewportItem *VP,Projection *Proj);
void Obs_RenderInfo(Tcl_Interp *Interp,TObs *Obs,ViewportItem *VP,Projection *Proj);
void Obs_RenderPath(Tcl_Interp *Interp,TObs *Obs,ViewportItem *VP,Projection *Proj);
void Obs_RenderVector(Tcl_Interp *Interp,TObs *Obs,ViewportItem *VP,Projection *Proj);

extern TIcon IconList[];

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclObs_Init>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Initialisation des commandes Tcl pour utilisation des obseravtions
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
int TclObs_Init(Tcl_Interp *Interp) {

   if (!ObsInit++) {
      Tcl_InitHashTable(&ObsTable,TCL_STRING_KEYS);
   }
   Tcl_CreateObjCommand(Interp,"observation",Obs_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Obs_Cmd>
 * Creation      : Fevrier 2003 J.P. Gauthier
 *
 * But          : Effectuer les commandes du package
 *
 * Parametres    :
 *   <clientData>: Nom de l'observation
 *   <interp>    : Interpreteur Tcl
 *   <Objc>      : Nombre d'arguments
 *   <Objv>      : Pointeur sur la liste des arguments (word)
 *
 * Retour        : Code de retour standard TCL
 *
 * Remarques     :
 *
 *---------------------------------------------------------------------------------------------------------------
*/

static int Obs_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   TObs       *obs;
   TDataSpec  *spec;

   int         idx,n;
   static CONST char *sopt[] = { "create","load","write","free","configure","define","stats","extract","copy","intersection","union","sort","is","all","wipe",NULL };
   enum               opt { CREATE,LOAD,WRITE,FREE,CONFIGURE,DEFINE,STATS,EXTRACT,COPY,INTERSECTION,UNION,SORT,IS,ALL,WIPE };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case CREATE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"obsid");
            return(TCL_ERROR);
         }
         return(Obs_Create(Interp,Tcl_GetString(Objv[2])));
         break;

      case LOAD:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"file");
            return(TCL_ERROR);
         }
         return(Obs_LoadASCII(Interp,Tcl_GetString(Objv[2]),NULL));
         break;

      case WRITE:
         if(Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"file { obs } title");
            return(TCL_ERROR);
         }
         return(Obs_WriteASCII(Interp,Tcl_GetString(Objv[2]),Objv[3],Tcl_GetString(Objv[4])));
         break;

      case FREE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"obsid");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            Obs_FreeHash(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case CONFIGURE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"obsid ?option?");
            return(TCL_ERROR);
         }
         obs=Obs_Get(Tcl_GetString(Objv[2]));
         if (!obs) {
            Tcl_AppendResult(Interp,"invalid observation",(char*)NULL);
            return(TCL_ERROR);
         }

         if (strcmp(Tcl_GetString(Objv[3]),"-dataspec")==0) {
            if (Objc==4) {
               if (obs->Spec) {
                  obs->Spec->NRef++;
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(obs->Spec->Name,-1));
               }
            } else {
               if ((spec=DataSpec_Get(Tcl_GetString(Objv[4])))) {
                  if (obs->Spec) {
                     DataSpec_FreeHash(Interp,obs->Spec->Name);
                  }
                  obs->Spec=spec;
                  spec->NRef++;
               } else {
                  Tcl_AppendResult(Interp,"Obs_Cmd: invalid configuration object",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
         } else {
            if (DataSpec_Config(Interp,obs->Spec,Objc-3,Objv+3)==TCL_OK) {
               return(TCL_OK);
            } else {
               return(TCL_ERROR);
            }
         }
         break;

      case DEFINE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"obsid ?option?");
            return(TCL_ERROR);
         }
         return Obs_Define(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3);
         break;

      case STATS:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"obsid");
            return(TCL_ERROR);
         }
         return Obs_Stat(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3);
         break;

      case EXTRACT:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"obsid field");
            return(TCL_ERROR);
         }
         return Obs_Extract(Interp,Obs_Get(Tcl_GetString(Objv[2])),Data_Get(Tcl_GetString(Objv[3])));
         break;

      case INTERSECTION:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"{ obs ...} header");
            return(TCL_ERROR);
         }
         return Obs_Intersection(Interp,Objv[2],Tcl_GetString(Objv[3]));
         break;

      case UNION:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"{ obs ...} header");
            return(TCL_ERROR);
         }
         return Obs_Union(Interp,Objv[2],Tcl_GetString(Objv[3]));
         break;

      case SORT:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"obslist");
            return(TCL_ERROR);
         }
         return DataDef_Sort(Interp,Objv[2]);
         break;

      case COPY:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"obsidto obsidfrom");
            return(TCL_ERROR);
         }
         if (!Obs_Copy(Interp,Obs_Get(Tcl_GetString(Objv[3])),Tcl_GetString(Objv[2]),1)) {
            return(TCL_ERROR);
         } else {
            return(TCL_OK);
         }
         break;

      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"obsid");
            return(TCL_ERROR);
         }
         if (Obs_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case ALL:
         TclY_HashAll(Interp,&ObsTable);
         break;

      case WIPE:
         TclY_HashWipe(&ObsTable,(TclY_HashFreeEntryDataFunc*)Obs_Free);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Obs_Define>
 * Creation : Fevrier 2003 J.P. Gauthier
 *
 * But      : Definition des donnees'observations
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Name>    : Nom de la projection
 *  <Objc>    : Nombre d'arguments
 *  <Objv>    : Liste des arguments
 *
 * Retour     :
 *  <TCL_...> : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int Obs_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj *obj,*sub;
   TObs    *obs;
   int      i,j,k,idx,type;
   double   val;
   time_t   sec;

   static CONST char *sopt[] = { "-INFO","-COORD","-ID","-NO","-IDX","-DATA","-NB","-DATE",NULL };
   enum                opt { INFO,COORD,ID,NO,IDX,DATA,NB,DATE };

   obs=Obs_Get(Name);
   if (!obs) {
      Tcl_AppendResult(Interp,"\n   Obs_Define: Observation id unknown: \"",Name,"\"",(char*)NULL);
      return TCL_ERROR;
   }

   for (i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case INFO:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for(j=0;j<obs->Loc->NbInfo;j++)
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(obs->Loc->Head[j],-1));
               Tcl_SetObjResult(Interp,obj);
            } else if (Objc==2) {
               i++;
               for(j=0;j<obs->Loc->NbInfo;j++) {
                  if (strcmp(obs->Loc->Head[j],Tcl_GetString(Objv[i]))==0)
                     break;
               }
               if (j==obs->Loc->NbInfo) {
                  Tcl_AppendResult(Interp,"\n   Obs_Define: Unknown information tag",(char*)NULL);
                  return(TCL_ERROR);
               }
               obj=Tcl_NewListObj(0,NULL);
               for(k=0;k<obs->Loc->Nb;k++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(obs->Loc->Info[k][j],-1));
               }
               Tcl_SetObjResult(Interp,obj);
            } else if (Objc==3 || Objc==4 || Objc==5) {
               i++;
               for(j=0;j<obs->Loc->NbInfo;j++) {
                  if (strcmp(obs->Loc->Head[j],Tcl_GetString(Objv[i]))==0)
                     break;
               }
               if (j==obs->Loc->NbInfo) {
                  if (Objc==5) {
                     obs->Loc->Head=realloc(obs->Loc->Head,(j+1)*sizeof(char*));
                     obs->Loc->Head[j]=strdup(Tcl_GetString(Objv[i]));

                     for(k=0;k<obs->Loc->Nb;k++) {
                        obs->Loc->Info[k]=realloc(obs->Loc->Info[k],(j+1)*sizeof(char*));
                     }
                  } else {
                     Tcl_AppendResult(Interp,"\n   Obs_Define: Unknown information tag",(char*)NULL);
                     return(TCL_ERROR);
                  }
               }

               Tcl_GetIntFromObj(Interp,Objv[++i],&k);
               if (k<0 || k>obs->Loc->Nb) {
                  Tcl_AppendResult(Interp,"\n   Obs_Define: Index out of range",(char*)NULL);
                  return TCL_ERROR;
               }

               if (Objc==4) {
                  if (obs->Loc->Info[k][j]) free(obs->Loc->Info[k][j]);
                  obs->Loc->Info[k][j]=strdup(Tcl_GetString(Objv[++i]));
               } else {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(obs->Loc->Info[k][j],-1));
               }
            } else {
               Tcl_AppendResult(Interp,"\n   Obs_Define: Wrong number of arguments, must be \"observation define -INFO [token] [index] [value] [force]\"",(char*)NULL);
               return TCL_ERROR;
            }
            break;

         case COORD:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for (j=0;j<obs->Loc->Nb;j++) {
                  sub=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(obs->Loc->Coord[j].Lat));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(obs->Loc->Coord[j].Lon));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(obs->Loc->Coord[j].Elev));
                  Tcl_ListObjAppendElement(Interp,obj,sub);
               }
               Tcl_SetObjResult(Interp,obj);
            } else if (Objc==2 || Objc==4 || Objc==5) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&j);
               if (j<0 || j>=obs->Loc->Nb) {
                  Tcl_AppendResult(Interp,"\n   Obs_Define: Index out of range",(char*)NULL);
                  return TCL_ERROR;
               }
               if (Objc==4) {
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&obs->Loc->Coord[j].Lat);
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&obs->Loc->Coord[j].Lon);
                  obs->Loc->Coord[j].Elev=0.0;
               } else if (Objc==5) {
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&obs->Loc->Coord[j].Lat);
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&obs->Loc->Coord[j].Lon);
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&obs->Loc->Coord[j].Elev);
               } else {
                  sub=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(obs->Loc->Coord[j].Lat));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(obs->Loc->Coord[j].Lon));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(obs->Loc->Coord[j].Elev));
                  Tcl_SetObjResult(Interp,sub);
               }
            } else {
               Tcl_AppendResult(Interp,"\n   Obs_Define: Wrong number of arguments, must be \"observation define -COORD [index] [lat] [lon] [elev]\"",(char*)NULL);
               return TCL_ERROR;
            }
            break;

         case ID:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for (j=0;j<obs->Loc->Nb;j++)
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(obs->Loc->Id[j],-1));
               Tcl_SetObjResult(Interp,obj);
            } else if (Objc==2 || Objc==3) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&j);
               if (j<0 || j>obs->Loc->Nb) {
                  Tcl_AppendResult(Interp,"\n   Obs_Define: Index out of range",(char*)NULL);
                  return TCL_ERROR;
               }
               if (Objc==3) {
                  if (obs->Loc->Id[j])
                     free(obs->Loc->Id[j]);
                 obs->Loc->Id[j]=strdup(Tcl_GetString(Objv[++i]));
               } else {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(obs->Loc->Id[j],-1));
               }
            } else {
               Tcl_AppendResult(Interp,"\n   Obs_Define: Wrong number of arguments, must be \"observation define -ID [index] [value]\"",(char*)NULL);
               return TCL_ERROR;
            }
            break;

         case NO:
            if (!obs->Loc->No) {
               break;
            }
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for (j=0;j<obs->Loc->Nb;j++)
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(obs->Loc->No[j],-1));
               Tcl_SetObjResult(Interp,obj);
            } else if (Objc==2 || Objc==3) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&j);
               if (j<0 || j>obs->Loc->Nb) {
                  Tcl_AppendResult(Interp,"\n   Obs_Define: Index out of range",(char*)NULL);
                  return TCL_ERROR;
               }
               if (Objc==3) {
                  if (!obs->Loc->No) {
                     obs->Loc->No=calloc(obs->Loc->Nb,sizeof(char*));
                  }
                  if (obs->Loc->No[j]) {
                     free(obs->Loc->No[j]);
                  }
                  obs->Loc->No[j]=strdup(Tcl_GetString(Objv[++i]));
               } else {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(obs->Loc->No[j],-1));
               }
            } else {
               Tcl_AppendResult(Interp,"\n   Obs_Define: Wrong number of arguments, must be \"observation define -NO [index] [value]\"",(char*)NULL);
               return TCL_ERROR;
            }
            break;

         case IDX:
            if (Objc!=2 && Objc!=3) {
               Tcl_AppendResult(Interp,"\n   Obs_Define: Wrong number of arguments, must be \"observation define -IDX [ID|NO] id\"",(char*)NULL);
               return TCL_ERROR;
            } else {
               type=1;
               if (Objc==3) {
                  type=strcmp("NO",Tcl_GetString(Objv[++i]));
               }
               i++;
               obj=Tcl_NewListObj(0,NULL);
               for (j=0;j<obs->Loc->Nb;j++) {
                  if (type) {
                     if (strcmp(obs->Loc->Id[j],Tcl_GetString(Objv[i]))==0) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(j));
                     }
                  } else {
                     if (strcmp(obs->Loc->No[j],Tcl_GetString(Objv[i]))==0) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(j));
                     }
                  }
               }
               Tcl_SetObjResult(Interp,obj);
            }
            break;

         case DATA:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for (j=0;j<obs->Loc->Nb;j++) {
                  sub=Tcl_NewListObj(0,NULL);
                  for(k=0;k<3 && obs->Def->Data[k];k++) {
                     if (OBSVALID(((float*)obs->Def->Data[k])[j])) {
                        Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(((float*)obs->Def->Data[k])[j]));
                     } else {
                        Tcl_ListObjAppendElement(Interp,sub,Tcl_NewStringObj("-",-1));
                     }
                  }
                  Tcl_ListObjAppendElement(Interp,obj,sub);
               }
               Tcl_SetObjResult(Interp,obj);
            } else if (Objc==2) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&j);
               if (j<0 || j>obs->Loc->Nb) {
                  Tcl_AppendResult(Interp,"\n   Obs_Define: Index out of range",(char*)NULL);
                  return TCL_ERROR;
               } else {
                  sub=Tcl_NewListObj(0,NULL);
                  for(k=0;k<3 && obs->Def->Data[k];k++) {
                     if (OBSVALID(((float*)obs->Def->Data[k])[j])) {
                        Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(((float*)obs->Def->Data[k])[j]));
                     } else {
                        Tcl_ListObjAppendElement(Interp,sub,Tcl_NewStringObj("-",-1));
                    }
                  }
                  Tcl_SetObjResult(Interp,sub);
               }
            } else if (Objc==3 || Objc==4 || Objc==5) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&j);
               if (j<0 || j>obs->Loc->Nb) {
                  Tcl_AppendResult(Interp,"\n   Obs_Define: Index out of range",(char*)NULL);
                  return TCL_ERROR;
               } else {
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&val);
                  ((float*)obs->Def->Data[0])[j]=val;
                  if (Objc>=4 && obs->Def->Data[1]) {
                     Tcl_GetDoubleFromObj(Interp,Objv[++i],&val);
                     ((float*)obs->Def->Data[1])[j]=val;
                  }
                  if (Objc==5 && obs->Def->Data[2]) {
                     Tcl_GetDoubleFromObj(Interp,Objv[++i],&val);
                     ((float*)obs->Def->Data[2])[j]=val;
                  }
               }
            } else {
               Tcl_AppendResult(Interp,"\n   Obs_Define: Wrong number of arguments, must be \"observation define -DATA [index] [value]\"",(char*)NULL);
               return TCL_ERROR;
            }
            break;

         case NB:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(obs->Loc->Nb));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&idx);

               if (!obs->Loc) {
                  obs->Loc=(TLoc*)malloc(sizeof(TLoc));
                  obs->Loc->Ref=0;
                  obs->Loc->Nb=0;
                  obs->Loc->NbInfo=0;
                  obs->Loc->No=NULL;
                  obs->Loc->Id=NULL;
                  obs->Loc->Head=NULL;
                  obs->Loc->Info=NULL;
                  obs->Loc->Coord=NULL;
               }
               k=obs->Loc->Nb;
               obs->Loc->Nb=idx;

               if (!obs->Def) {
                  obs->Def=DataDef_New(obs->Loc->Nb,1,1,1,TD_Float32);
               } else {
                  obs->Def=DataDef_Resize(obs->Def,obs->Loc->Nb,1,1);
               }
               obs->Def->NoData=-999.0;

               if (obs->Loc->No)
                  obs->Loc->No=realloc(obs->Loc->No,obs->Loc->Nb*sizeof(char*));

               obs->Loc->Id=realloc(obs->Loc->Id,obs->Loc->Nb*sizeof(char*));
               obs->Loc->Head=realloc(obs->Loc->Head,obs->Loc->Nb*sizeof(char*));
               obs->Loc->Info=realloc(obs->Loc->Info,obs->Loc->Nb*sizeof(char*));
               obs->Loc->Coord=realloc(obs->Loc->Coord,obs->Loc->Nb*sizeof(Coord));
               for(;k<obs->Loc->Nb;k++) {
                  obs->Loc->Id[k]=NULL;
                  obs->Loc->Head[k]=NULL;
                  obs->Loc->Info[k]=NULL;
               }
            }
            break;

         case DATE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewLongObj(System_DateTime2Seconds(obs->Date,obs->Time,1)));
            } else {
               Tcl_GetLongFromObj(Interp,Objv[++i],&sec);
               System_Seconds2DateTime(sec,&obs->Date,&obs->Time,1);
            }
            break;
      }
   }

   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Obs_Copy>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Copy d'un objet observations.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Obs>      : Observation a copier
 *   <Name>     : Nom de l'observation a creer
 *   <Def>      : Copie de la definition de donnee
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
TObs *Obs_Copy(Tcl_Interp *Interp,TObs *Obs,char *Name,int Def) {

   TObs *new;

   if (new=Obs_Get(Name)) {
      if (new!=Obs) {
         Obs_FreeHash(Interp,Name);
      } else {
         if (!Def && new->Def) {
            DataDef_Free(new->Def);
            new->Def=NULL;
         }
         return(new);
      }
   }

   if (new) {
      Obs_FreeHash(Interp,Name);
   }

   if (Obs_Create(Interp,Name)!=TCL_OK) {
       return(NULL);
   }

   new=Obs_Get(Name);
   new->Date=Obs->Date;
   new->Time=Obs->Time;
   new->Min=Obs->Min;
   new->Max=Obs->Max;

   new->Loc=Obs->Loc;
   new->Loc->Ref++;

   new->Def=NULL;

   new->Tag=NULL;

   if (Def) {
      new->Def=DataDef_Copy(Obs->Def);
      if (!new->Def) {
         Tcl_AppendResult(Interp,"\n   Obs_Copy : Unable to allocate data definition",(char*)NULL);
         return(NULL);
      }
   }
   Obs_GetStat(Obs);

   return(new);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Obs_Extract>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Extraire les valeurs d'un champs aux positions de l'observation.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Obs>      : Observation
 *   <Field>    : Champs
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Obs_Extract(Tcl_Interp *Interp,TObs *Obs,TData *Field) {

   int    i;
   double x,y,tmp,val;

   if (!Obs) {
      Tcl_AppendResult(Interp,"\n   Obs_Extract: Invalid observation",(char*)NULL);
      return TCL_ERROR;
   }

   if (!Field) {
      Tcl_AppendResult(Interp,"\n   Obs_Extract: Invalid field",(char*)NULL);
      return TCL_ERROR;
   }

   if (!Field->Ref) {
      Tcl_AppendResult(Interp,"\n   Obs_Extract: Invalid field geographic reference",(char*)NULL);
      return TCL_ERROR;
   }

#ifdef LNK_FSTD
   c_ezsetopt("INTERP_DEGREE",Field->Spec->InterpDegree);
#endif
   for(i=0;i<Obs->Loc->Nb;i++) {

      if (!Field->Ref->UnProject(Field->Ref,&x,&y,Obs->Loc->Coord[i].Lat,Obs->Loc->Coord[i].Lon,0,1)) {
         ((float*)Obs->Def->Data[0])[i]=Obs->Def->NoData;
      } else {
         Field->Ref->Value(Field->Ref,Field->Def,Field->Spec->InterpDegree[0],0,x,y,Field->Def->Level,&val,&tmp);
         ((float*)Obs->Def->Data[0])[i]=val;
      }
   }

   i=-3;
   f77name(newdate)(&((FSTD_Head*)(Field->Head))->DATEV,&Obs->Date,&Obs->Time,&i);
   Obs_GetStat(Obs);

   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Obs_Grid>
 * Creation     : Fevrier 2007 J.P. Gauthier
 *
 * But          : Reprojecter les obs sur une grille (Ref).
 *
 * Parametres   :
 *   <Ref>      : Georeference de reprojection
 *   <Obs>      : Observation
 *   <NObs>     : Nombre d'obs reprojectee.
 *   <Extrap>   : Reprojeter en dehors du domaine
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/

Vect3d *Obs_Grid(TGeoRef *Ref,TObs *Obs,int *NObs,int Extrap) {

   int     i,j,k,skip,idx,n;
   double  dk,d;
   Vect3d *pos=(Vect3d*)malloc(Obs->Loc->Nb*sizeof(Vect3d));
   Coord   c0,c1,c2;

   *NObs=0;
   if (pos) {
      for(i=0;i<Obs->Loc->Nb;i++) {

         if (Ref->Grid[0]=='V') {
            j=1;

            /* Get the right level*/
            for(k=Ref->ZRef.LevelNb-1;k>=0;k--) {
              if (Obs->Loc->Coord[i].Elev>Ref->ZRef.Levels[k])
                  break;
            }
            if (k==Ref->ZRef.LevelNb-1) {
               dk=Ref->ZRef.Levels[k]-Ref->ZRef.Levels[k-1];
               pos[*NObs][1]=ILIN(k-1,k,(Obs->Loc->Coord[i].Elev-Ref->ZRef.Levels[k])/dk);
               j=0;
            } else {
               dk=Ref->ZRef.Levels[k+1]-Ref->ZRef.Levels[k];
               pos[*NObs][1]=ILIN(k,k+1,(Obs->Loc->Coord[i].Elev-Ref->ZRef.Levels[k])/dk);
            }

            /*Get the horizontal position*/
            d=1e32;
            idx=0;

            /*Find closest point*/
            c2.Lat=DEG2RAD(Obs->Loc->Coord[i].Lat);c2.Lon=DEG2RAD(Obs->Loc->Coord[i].Lon);
            n=Ref->X1-Ref->X0;
            for(k=0;k<=n;k++) {
               c0.Lat=DEG2RAD(Ref->Lat[k]);c0.Lon=DEG2RAD(Ref->Lon[k]);
               dk=DIST(0,c2.Lat,c2.Lon,c0.Lat,c0.Lon);
               if (d>dk) {
                  d=dk;
                  idx=k;
               }
            }

            /*Figure out right angle crossing point*/
            c1.Lat=DEG2RAD(Ref->Lat[idx==n?idx-1:idx+1]);c1.Lon=DEG2RAD(Ref->Lon[idx==n?idx-1:idx+1]);
            pos[*NObs][0]=idx+GeoFunc_RadialPointRatio(c0,c1,c2);
            if (pos[*NObs][0]<0 || pos[*NObs][0]>n) {
               j=0;
            }
         } else {
            j=Ref->UnProject(Ref,&pos[*NObs][0],&pos[*NObs][1],Obs->Loc->Coord[i].Lat,Obs->Loc->Coord[i].Lon,Extrap,1);
         }

         skip=0;
         for(k=0;k<*NObs;k++) {
            if (pos[*NObs][0]==pos[k][0] && pos[*NObs][1]==pos[k][1]) {
               skip=1;
               break;
            }
         }

         if (!skip && (Extrap || j)) {
            Def_Get(Obs->Def,0,i,pos[*NObs][2]);

            if (OBSVALID(pos[*NObs][2])) {
               (*NObs)++;
            }
        }
      }
   }
   return(pos);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Obs_Intersection>
 * Creation     : Mars 2005 J.P. Gauthier
 *
 * But          : Effectuer l'intersection des obs donc, pour chaque obs, on remplace son contenue par
 *                celui des loc communes.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <List>     : Observations
 *   <Token>    : Token de selection
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Obs_Intersection(Tcl_Interp *Interp,Tcl_Obj *List,char *Token) {

   int       nobs,nb,i,j,n,k0;
   char     **array0,**array1,**array;
   TObs     *obs0,*obs1;
   TLoc     *loc=NULL;
   TDataDef *def=NULL;
   Tcl_Obj  *obj;

   if (!List) {
      Tcl_AppendResult(Interp,"\n   Obs_Intersection: Empty list",(char*)NULL);
      return TCL_ERROR;
   }

   Tcl_ListObjLength(Interp,List,&nobs);

   /*For each observation*/
   nb=0;
   for(i=0;i<nobs;i++) {
      Tcl_ListObjIndex(Interp,List,i,&obj);
      obs0=Obs_Get(Tcl_GetString(obj));
      if (!obs0->Loc) {
         Tcl_AppendResult(Interp,"\n   Obs_Intersection: Invalid observation (Empty)",(char*)NULL);
         return(TCL_ERROR);
      }
      array0=Token[0]=='I'?obs0->Loc->Id:obs0->Loc->No;

      /*Allocate positions structure*/
      if (!loc) {
         loc=(TLoc*)malloc(sizeof(TLoc));
         loc->Id=(char**)calloc(obs0->Loc->Nb,sizeof(char*));
         if (obs0->Loc->No)
            loc->No=(char**)calloc(obs0->Loc->Nb,sizeof(char*));
         loc->Coord=(Coord*)malloc(obs0->Loc->Nb*sizeof(Coord));
         array=Token[0]=='I'?loc->Id:loc->No;
      }
// mem leak nb <-> Loc->Nb

      /*Parse the Ids of fisrt obs*/
      for(k0=0;k0<obs0->Loc->Nb;k0++) {

         /*Check if already included*/
         if (Obs_LocFind(loc,array0[k0],array)!=-1) {
            break;
         }

         /*Compare to each other observation not already compared*/
         n=1;
         for(j=0;j<nobs;j++) {
            Tcl_ListObjIndex(Interp,List,j,&obj);
            obs1=Obs_Get(Tcl_GetString(obj));
            array1=Token[0]=='I'?obs1->Loc->Id:obs1->Loc->No;

            if (j!=i && (Obs_LocFind(obs1->Loc,array0[k0],array1))!=-1) {
               n++;
            }
         }

         /*If everywhere, include it*/
         if (n==nobs) {
            loc->Id[nb]=strdup(obs0->Loc->Id[k0]);
            if (loc->No)
               loc->No[nb]=strdup(obs0->Loc->No[k0]);
            memcpy(&loc->Coord[nb],&obs0->Loc->Coord[k0],sizeof(Coord));
            nb++;
         }
      }
   }
   loc->Nb=nb;

   /*Copy other informations*/
   loc->Head=(char**)calloc(obs0->Loc->NbInfo,sizeof(char**));
   loc->NbInfo=obs0->Loc->NbInfo;
   for(n=0;n<loc->NbInfo;n++) {
      loc->Head[n]=strdup(obs0->Loc->Head[n]);
   }
   loc->Info=(char***)malloc(loc->Nb*sizeof(char**));
   for(n=0;n<loc->Nb;n++) {
      loc->Info[n]=(char**)malloc(obs0->Loc->NbInfo*sizeof(char*));
   }

   /*Reparse the obs to include only intersection previously found*/
   for(i=0;i<nobs;i++) {
      Tcl_ListObjIndex(Interp,List,i,&obj);
      obs0=Obs_Get(Tcl_GetString(obj));
      array0=Token[0]=='I'?obs0->Loc->Id:obs0->Loc->No;
      def=DataDef_New(loc->Nb,1,1,DSIZE(obs0->Def->Data),TD_Float32);
      def->NoData=-999.0;

      /*Parse the locations*/
      nb=0;
      for(k0=0;k0<obs0->Loc->Nb;k0++) {
         /*if Loc is in our intersection*/
         if ((Obs_LocFind(loc,array0[k0],array))!=-1) {
           /*Copy data*/
            if (nb>=loc->Nb) {
               Tcl_AppendResult(Interp,"\n   Obs_Intersection: list id's are not unique",(char*)NULL);
               return TCL_ERROR;
            }
            if (obs0->Def->Data[0]) ((float*)def->Data[0])[nb]=((float*)obs0->Def->Data[0])[k0];
            if (obs0->Def->Data[1]) ((float*)def->Data[1])[nb]=((float*)obs0->Def->Data[1])[k0];
            if (obs0->Def->Data[2]) ((float*)def->Data[2])[nb]=((float*)obs0->Def->Data[2])[k0];

            /*Copy other informations*/
            for(j=0;j<loc->NbInfo;j++) {
               loc->Info[nb][j]=strdup(obs0->Loc->Info[k0][j]);
            }
            nb++;
         }
      }

      /*Replace structures with intersection ones*/
      if (Obs_LocFree(obs0->Loc)==0)
         free(obs0->Loc);

      obs0->Loc=loc;
      loc->Ref++;
      DataDef_Free(obs0->Def);
      obs0->Def=def;
   }
   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Obs_Union>
 * Creation     : Mars 2005 J.P. Gauthier
 *
 * But          : Effectuer l'union des obs donc, pour chaque obs, on remplace son contenue par
 *                celui des loc de toutes.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <List>     : Observations
 *   <Token>    : Token de selection
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Obs_Union(Tcl_Interp *Interp,Tcl_Obj *List,char *Token) {

   int       nobs,nb,i,j,n,k0,idx,k;
   char     **array0,**array;
   TObs     *obs0;
   TLoc     *loc=NULL;
   TDataDef *def=NULL;
   Tcl_Obj  *obj;

   if (!List) {
      Tcl_AppendResult(Interp,"\n   Obs_Union: Empty list",(char*)NULL);
      return(TCL_ERROR);
   }

   Tcl_ListObjLength(Interp,List,&nobs);

   nb=0;
   for(i=0;i<nobs;i++) {
      Tcl_ListObjIndex(Interp,List,i,&obj);
      obs0=Obs_Get(Tcl_GetString(obj));
      if (!obs0 || !obs0->Loc) {
         Tcl_AppendResult(Interp,"\n   Obs_Union: Invalid observation (Empty)",(char*)NULL);
         return(TCL_ERROR);
      }
      nb+=obs0->Loc->Nb;
   }

   /*Allocate positions structure*/
   loc=(TLoc*)malloc(sizeof(TLoc));
   loc->Id=(char**)calloc(nb,sizeof(char*));
   loc->No=NULL;
   if (Token[0]!='I')
      loc->No=(char**)calloc(nb,sizeof(char*));
   loc->Coord=(Coord*)malloc(nb*sizeof(Coord));
   array=Token[0]=='I'?loc->Id:loc->No;
// mem leak nb <-> Loc->Nb

   /*For each observation*/
   loc->Nb=0;
   for(i=0;i<nobs;i++) {
      Tcl_ListObjIndex(Interp,List,i,&obj);
      obs0=Obs_Get(Tcl_GetString(obj));
      array0=Token[0]=='I'?obs0->Loc->Id:obs0->Loc->No;

      /*Compare to each other observation not already compared*/
      for(k0=0;k0<obs0->Loc->Nb;k0++) {
         /*Check if already included*/
        if (Obs_LocFind(loc,array0[k0],array)==-1) {
            loc->Id[loc->Nb]=strdup(obs0->Loc->Id[k0]);
            if (loc->No) {
               if (obs0->Loc->No) {
                  loc->No[loc->Nb]=strdup(obs0->Loc->No[k0]);
               }
            }
            memcpy(&loc->Coord[loc->Nb],&obs0->Loc->Coord[k0],sizeof(Coord));
            loc->Nb++;
         }
      }
   }

   /*Copy other informations*/
   loc->NbInfo=obs0->Loc->NbInfo;
   loc->Head=(char**)calloc(loc->NbInfo,sizeof(char**));
   for(n=0;n<loc->NbInfo;n++) {
      loc->Head[n]=strdup(obs0->Loc->Head[n]);
   }
   loc->Info=(char***)malloc(loc->Nb*sizeof(char**));
   for(n=0;n<loc->Nb;n++) {
      loc->Info[n]=(char**)calloc(loc->NbInfo,sizeof(char*));
   }

   /*Reparse the obs to include only union previously found*/
   for(i=0;i<nobs;i++) {
      Tcl_ListObjIndex(Interp,List,i,&obj);
      obs0=Obs_Get(Tcl_GetString(obj));
      array0=Token[0]=='I'?obs0->Loc->Id:obs0->Loc->No;
      def=DataDef_New(loc->Nb,1,1,DSIZE(obs0->Def->Data),TD_Float32);
      def->NoData=-999.0;

      /*Parse the locations*/
      for(k0=0;k0<loc->Nb;k0++) {
         /*if Loc is in our union*/
         if ((idx=Obs_LocFind(obs0->Loc,array[k0],array0))!=-1) {
           /*Copy data*/
            if (obs0->Def->Data[0]) ((float*)def->Data[0])[k0]=((float*)obs0->Def->Data[0])[idx];
            if (obs0->Def->Data[1]) ((float*)def->Data[1])[k0]=((float*)obs0->Def->Data[1])[idx];
            if (obs0->Def->Data[2]) ((float*)def->Data[2])[k0]=((float*)obs0->Def->Data[2])[idx];

            /*Copy other informations*/
            for(j=0;j<loc->NbInfo;j++) {
              for(k=0;k<obs0->Loc->NbInfo;k++) {
                  if (strcmp(loc->Head[j],obs0->Loc->Head[k])==0) {
                     if (!loc->Info[k0][j]) {
                        loc->Info[k0][j]=strdup(obs0->Loc->Info[idx][k]);
                     }
                     break;
                  }
               }
            }
         } else {
            if (obs0->Def->Data[0]) ((float*)def->Data[0])[k0]=def->NoData;
            if (obs0->Def->Data[1]) ((float*)def->Data[1])[k0]=def->NoData;
            if (obs0->Def->Data[2]) ((float*)def->Data[2])[k0]=def->NoData;
         }
      }

      /*Replace structures with union ones*/
      if (Obs_LocFree(obs0->Loc)==0)
         free(obs0->Loc);

      obs0->Loc=loc;
      loc->Ref++;
      DataDef_Free(obs0->Def);
      obs0->Def=def;
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Obs_Create>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Creation d'un objet observations et insertion d'un nouveau nom dans la table.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de l'observation a creer
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/

static int Obs_Create(Tcl_Interp *Interp,char *Name) {

   TObs *obs=NULL;
   char *buf=NULL;

   if (!(obs=(TObs*)TclY_HashPut(Interp,&ObsTable,Name,sizeof(TObs)))) {
      return(TCL_ERROR);
   }

   /*Initialisation de la structure obs*/
   buf=strdup(Name);
   strrep(buf,'.','\0');
   if (!(obs->Spec=DataSpec_Create(Interp,buf))) {
      return(TCL_ERROR);
   }
   if (buf) free(buf);

   obs->Def  = NULL;
   obs->Loc  = NULL;
   obs->Tag  = NULL;

   obs->Date = 0;
   obs->Time = 0;
   obs->Min  = 0;
   obs->Max  = 0;

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Obs_FreeHash>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Destruction d'une observation a partir de son nom.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de l'observation a detruire
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int Obs_FreeHash(Tcl_Interp *Interp,char *Name) {

   TObs *obs=NULL;

   if ((obs=(TObs*)TclY_HashDel(&ObsTable,Name))) {
      DataSpec_FreeHash(Interp,obs->Spec->Name);
      Obs_Free(obs);
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Obs_Free>
 * Creation     : Juin 2003 J.P. Gauthier
 *
 * But          : Liberation de la memoire associe a une observation.
 *
 * Parametres   :
 *   <Obs>      : Observation
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Obs_Free(TObs *Obs) {

   DataDef_Free(Obs->Def);

   if (Obs->Tag)                 Tcl_DecrRefCount(Obs->Tag);
   if (Obs_LocFree(Obs->Loc)==0) free(Obs->Loc);

   free(Obs);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Obs_Get>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Obtenir un objet observation en fonction de son nom dans la table de Tcl ObsTable
 *
 * Parametres   :
 *   <Name>     : Nom de l'objet observation a obtenir.
 *
 * Retour       : Une structure TObs ou un pointeur NULL si rien trouve.
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
TObs* Obs_Get(char *Name) {
   return((TObs*)TclY_HashGet(&ObsTable,Name));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Obs_WriteASCII>
 * Creation     : Decembre 2004 J.P. Gauthier
 *
 * But          : Ecriture d'un fichier d'observations.
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl (IN)
 *   <File>     : Le nom du fichier (IN)
 *   <List>     : Liset des observations
 *   <Title>    : Tire du set de donnees
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Obs_WriteASCII(Tcl_Interp *Interp,char *File,Tcl_Obj *List,char *Title) {

   int       n,i,k,nobs;
   char      buf[256];
   FILE     *stream;
   TObs     *obs=NULL;
   TLoc     *loc=NULL;
   Tcl_Obj  *obj;

   stream=fopen(File,"w+");

   if (!stream) {
      Tcl_AppendResult(Interp,"\n   Obs_WriteASCII :  Could not open observation file ",File,(char*)NULL);
      return TCL_ERROR;
   }

   Tcl_ListObjLength(Interp,List,&nobs);
   Tcl_ListObjIndex(Interp,List,0,&obj);
   obs=Obs_Get(Tcl_GetString(obj));

   if (!obs) {
      Tcl_AppendResult(Interp,"\n   Obs_WriteASCII: Observation id unknown: \"",Tcl_GetString(obj),"\"",(char*)NULL);
      return TCL_ERROR;
   }
   loc=obs->Loc;

   fputs("Obs 3.1\n",stream);
   fputs(Title,stream);
   fputs("\nID",stream);
   if (obs->Loc->No) fputs(" NO",stream);
   fputs(" LAT LON ELEV ELEVTYPE",stream);

   for(n=0;n<obs->Loc->NbInfo;n++){
      fputc(' ',stream);
      fputs(obs->Loc->Head[n],stream);
   }


   for(n=0;n<nobs;n++) {
      Tcl_ListObjIndex(Interp,List,n,&obj);
      obs=Obs_Get(Tcl_GetString(obj));
      if (!obs) {
         Tcl_AppendResult(Interp,"\n   Obs_WriteASCII: Observation id unknown: \"",Tcl_GetString(obj),"\"",(char*)NULL);
         return TCL_ERROR;
      }
      if (loc->Nb!=obs->Loc->Nb || loc->NbInfo!=obs->Loc->NbInfo) {
         Tcl_AppendResult(Interp,"\n   Obs_WriteASCII: Observations are incompatible",(char*)NULL);
         return TCL_ERROR;
      }

      fputs(" DATA.",stream);
      i=0;
      while (!(Tcl_GetString(obj)[i]=='.' && Tcl_GetString(obj)[i+1]=='#') && Tcl_GetString(obj)[i]!='\0') {
         fputc(Tcl_GetString(obj)[i],stream);
         i++;
      }
   }

   for(n=0;n<loc->Nb;n++) {
      fputs("\n\"",stream);
      fputs(loc->Id[n],stream);
      fputs("\"",stream);
      if (obs->Loc->No) {
        fputs(" ",stream);
        fputs(loc->No[n],stream);
      }
      sprintf(buf," %.15f",loc->Coord[n].Lat);
      fputs(buf,stream);
      sprintf(buf," %.15f",loc->Coord[n].Lon);
      fputs(buf,stream);
      sprintf(buf," %.5f",loc->Coord[n].Elev);
      fputs(buf,stream);
      sprintf(buf," %i",obs->LevelType);
      fputs(buf,stream);

      for(i=0;i<loc->NbInfo;i++) {
         fputs(" \"",stream);
         fputs(loc->Info[n][i],stream);
         fputs("\"",stream);
      }

      for(i=0;i<nobs;i++) {
         Tcl_ListObjIndex(Interp,List,i,&obj);
         obs=Obs_Get(Tcl_GetString(obj));
         if (obs->Def->Data[1]) {
            fputs(" {",stream);
         }
         for(k=0;k<3;k++) {
            if (obs->Def->Data[k]) {
               sprintf(buf," %.5e",((float*)obs->Def->Data[k])[n]);
               fputs(buf,stream);
            }
         }
         if (obs->Def->Data[1]) {
            fputs(" }",stream);
         }
      }
   }

   fclose(stream);

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Obs_LoadASCII>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Chargement d'un fichier d'observations.
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <Name>     : Nom de l'observation dans la table
 *   <File>     : Le nom du fichier
 *   <Token>    : Token de concatenation de l'ID
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Obs_LoadASCII(Tcl_Interp *Interp,char *File,char *Token) {

   Tcl_Obj *obj;
   TObs    *obs=NULL;
   TLoc    *loc;
   FILE    *stream;
   char    title[256],name[256];
   char    *bytes=NULL,*head=NULL;
   int     sz,sk,nb,n,hd,k,sec;
   int     ntok,gntok,nltok;
   char    **tok,**gtok,**ltok;
   int     err=TCL_OK;
   int    levtyp=0;
   static CONST char *type[] = { "MASL","SIGMA","PRESSURE","UNDEFINED","MAGL","HYBRID","THETA","ETA","GALCHEN" };

   stream=fopen(File,"r");

   if (!stream) {
      Tcl_AppendResult(Interp,"\n   Obs_LoadASCII :  Could not open observation file ",File,(char*)NULL);
      return TCL_ERROR;
   }

   /*Read the version*/
   fgetskip(title,256,stream);

   if (!strstr(title,"Obs 3.1")) {
      Tcl_AppendResult(Interp,"\n   Obs_LoadASCII :  Wrong file version while reading ",File,(char*)NULL);
      fclose(stream);
      return TCL_ERROR;
   }

   /*Read the title*/
   fgetskip(title,256,stream);

   /*Read the header*/
   nb=255;
   while(nb==255) {
      fgetskip(title,256,stream);
      head=strcatalloc(head,title);
      nb=strlen(title);
   }

   sz=strlen(head)*4;
   bytes=(char*)malloc(sz);

   Tcl_SplitList(Interp,head,&gntok,&gtok);
   free(head);

   /*Get the number of data*/
   sk=ftell(stream);
   nb=0;
   while (!feof(stream)) {
      fgetskip(bytes,sz,stream);
      if (strlen(bytes)>10) nb++;
   }
   fseek(stream,sk,SEEK_SET);

   /*Is there any observation in there*/
   if (!nb) {
      Tcl_AppendResult(Interp,"\n   Obs_LoadCMC :  No observation found ",(char*)NULL);
      fclose(stream);
      return TCL_ERROR;
   }

   /*Allocate positions structure*/
   loc=(TLoc*)malloc(sizeof(TLoc));
   loc->Id=(char**)calloc(nb,sizeof(char*));
   loc->Coord=(Coord*)calloc(nb,sizeof(Coord));
   loc->Info=(char***)malloc(nb*sizeof(char**));
   loc->Nb=nb;
   loc->No=NULL;
   loc->Ref=0;
   loc->NbInfo=0;

   obj=Tcl_NewListObj(0,NULL);
  /*Allocate space for structures*/
   for(n=0;n<gntok;n++) {
      if (strncmp(gtok[n],"DATA",4)==0) {
         sprintf(name,"%s.#%i",&gtok[n][5],ObsNo+n);
         if (Obs_Create(Interp,name)==TCL_OK) {
            obs=Obs_Get(name);
            obs->Def=DataDef_New(nb,1,1,1,TD_Float32);
            obs->Def->NoData=-999.0;
            obs->Loc=loc;
            loc->Ref++;
            obs->Spec->Desc=strdup(&gtok[n][5]);
            strrep(obs->Spec->Desc,'.','\0');

            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(name,-1));

            /*Extract date and time*/
            hd=0;sec=0;
            sscanf((char*)(strrchr(gtok[n],(int)'.'))+1,"%8d%02d%02d%02d",&obs->Date,&obs->Time,&hd,&sec);
            obs->Time=obs->Time*10000+hd*100+sec;
        } else {
            Tcl_AppendResult(Interp,"\n   Obs_LoadCMC :  Could not create observation object link ",&gtok[n][5],(char*)NULL);
            fclose(stream);
            return TCL_ERROR;
         }
      } else if (strcmp(gtok[n],"NO")==0) {
         loc->No=(char**)malloc(nb*sizeof(char*));
      } else if (strcmp(gtok[n],"LAT")!=0 && strcmp(gtok[n],"LON")!=0 && strcmp(gtok[n],"ELEV")!=0 && strcmp(gtok[n],"ELEVTYPE")!=0 && strcmp(gtok[n],"ID")!=0) {
         loc->NbInfo++;
      }
   }
   Tcl_SetObjResult(Interp,obj);

   /*Allocate variable information array*/
   loc->Head=(char**)calloc(loc->NbInfo,sizeof(char**));
   for(n=0;n<nb;n++) {
      loc->Info[n]=(char**)malloc(loc->NbInfo*sizeof(char*));
   }

   nb=0;
   while (!feof(stream)) {
      fgetskip(bytes,sz,stream);

      if (strlen(bytes)>10) {
         Tcl_SplitList(Interp,bytes,&ntok,&tok);

         if (ntok!=gntok) {
            Tcl_AppendResult(Interp,"\n   Obs_LoadASCII :  Invalid number of item on following line\n",bytes,(char*)NULL);
            fclose(stream);
            Tcl_Free((char*)tok);
            return(TCL_ERROR);
         }

         /*Parse the tokens*/
         hd=0;
         for(n=0;n<ntok;n++) {
            if (strcmp(gtok[n],"LAT")==0) {                  /*Latitude information*/
               loc->Coord[nb].Lat=atof(tok[n]);
            } else if (strcmp(gtok[n],"LON")==0) {           /*Longitude information*/
               loc->Coord[nb].Lon=atof(tok[n]);
            } else if (strcmp(gtok[n],"ELEVTYPE")==0) {      /*Elevation type information*/
               if (isdigit(tok[n][0])) {
                  levtyp=atoi(tok[n]);
               } else {
                  if (strcmp(tok[n],"MASL")==0) {
                     levtyp=0;
                  } else if (strcmp(tok[n],"SIGMA")==0) {
                     levtyp=1;
                  } else if (strcmp(tok[n],"PRESSURE")==0) {
                     levtyp=2;
                  } else if (strcmp(tok[n],"UNDEFINED")==0) {
                     levtyp=3;
                  } else if (strcmp(tok[n],"MAGL")==0) {
                     levtyp=4;
                  } else if (strcmp(tok[n],"HYBRID")==0) {
                     levtyp=5;
                  } else if (strcmp(tok[n],"THETA")==0) {
                     levtyp=6;
                  } else if (strcmp(tok[n],"ETA")==0) {
                     levtyp=7;
                  } else if (strcmp(tok[n],"GALCHEN")==0) {
                     levtyp=8;
                  } else {
                     Tcl_AppendResult(Interp,"\n   Obs_LoadASCII : Invalid level type, must be [ MASL SIGMA PRESSURE UNDEFINED MAGL HYBRID THETA ETA GALCHEN ]",(char*)NULL);
                     err=TCL_ERROR;
                  }
               }
            } else if (strcmp(gtok[n],"ELEV")==0) {          /*Elevation information*/
               loc->Coord[nb].Elev=atof(tok[n]);
            } else if (strcmp(gtok[n],"NO")==0) {            /*Number information*/
               loc->No[nb]=strdup(tok[n]);
               /*Patch temporaire car le postscript niaise avec les paranthese non uniforme*/
               strrep(loc->No[nb],'(',' ');
               strrep(loc->No[nb],')',' ');
            } else if (strcmp(gtok[n],"ID")==0) {            /*Identificateur*/
               loc->Id[nb]=strdup(tok[n]);
               /*Patch temporaire car le postscript niaise avec les paranthese non uniforme*/
               strrep(loc->Id[nb],'(',' ');
               strrep(loc->Id[nb],')',' ');
            } else if (strncmp(gtok[n],"DATA",4)==0) {       /*Values*/
               sprintf(name,"%s.#%i",&gtok[n][5],ObsNo+n);
               obs=Obs_Get(name);
               obs->LevelType=levtyp;

               if (tok[n][0]=='-' && tok[n][1]=='\0') {
                  ((float*)obs->Def->Data[0])[nb]=obs->Def->NoData;
               } else {
                  /* Split the list of values in case of vectorial data*/
                  if ((err=Tcl_SplitList(Interp,tok[n],&nltok,&ltok))==TCL_OK) {
                     for(k=0;k<nltok;k++) {
                        if (!obs->Def->Data[k]) {
                           if (!(obs->Def->Data[k]=(char*)calloc(FSIZE3D(obs->Def),TData_Size[TD_Float32]))) {
                              Tcl_AppendResult(Interp,"\n   Obs_LoadASCII : Unable to allocate memory for vectorial components",(char*)NULL);
                              err=TCL_ERROR;
                              break;
                           }
                           obs->Def->NC=k;
                        }
                        Data_FromString(ltok[k],obs->Def,k,nb);
                     }
                     Tcl_Free((char*)ltok);
                  } else {
                     Tcl_AppendResult(Interp,"\n   Obs_LoadASCII : Problems while reading data components",(char*)NULL);
                  }
               }
            } else {                                         /*Information*/
               if (!loc->Head[hd])
                  loc->Head[hd]=strdup(gtok[n]);
               obs->Loc->Info[nb][hd]=strdup(tok[n]);
               hd++;
            }
         }

         nb++;
         Tcl_Free((char*)tok);
      }
   }

   fclose(stream);

   /* Figure out the stats*/
   for(n=0;n<ntok;n++) {
      sprintf(name,"%s.%i",&gtok[n][5],ObsNo+n);
      if ((obs=Obs_Get(name))) {
         Obs_GetStat(obs);
      }
   }

   Tcl_Free((char*)gtok);
   free(bytes);

   ObsNo+=gntok;

   return(err);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Obs_LocFind>
 * Creation     : Mars 2005 J.P. Gauthier
 *
 * But          : Recherche de l'index d'un ID dans une structure TLOC
 *
 * Parametres   :
 *   <Locp>     : Structure TLoc
 *   <Value>    : Valeur a rechercher
 *   <Array>    : Tableau de recherche
 *
 * Retour       : Index (-1 si pas trouve)
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Obs_LocFind(TLoc *Loc,char *Value,char **Array) {

   int idx;

   for(idx=0;idx<Loc->Nb;idx++) {
       if (Array[idx]==NULL) {
          idx=-1;
          break;
       }
       if (strcmp(Array[idx],Value)==0) {
          break;
       }
   }

   return(idx=idx==Loc->Nb?-1:idx);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Obs_LocFree>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Liberer la liste de locaisation et information partagee.
 *
 * Parametres   :
 *   <Loc>      : Structure partagee
 *
 * Retour       : Compteur de reference a la structure partagee
 *
 * Remarques :
 *    -On libere seulement quand le compteur de reference est a zero.
 *
 *---------------------------------------------------------------------------------------------------------------
*/

int Obs_LocFree(TLoc *Loc){

   int i,n;

   if (!Loc)
      return(-1);

   if (!(--Loc->Ref)) {

      for (i=0;i<Loc->Nb;i++) {
         for(n=0;n<Loc->NbInfo;n++) {
            free(Loc->Info[i][n]);
         }

         free(Loc->Id[i]);
         if (Loc->No) free(Loc->No[i]);
         free(Loc->Info[i]);
      }

      for(n=0;n<Loc->NbInfo;n++) {
         free(Loc->Head[n]);
      }

      free(Loc->Head);
      free(Loc->Info);

      if (Loc->Coord) free(Loc->Coord);
      if (Loc->Id)    free(Loc->Id);
      if (Loc->No)    free(Loc->No);
   }

   return(Loc->Ref);
}

/*----------------------------------------------------------------------------
 * Nom      : <Obs_GetStat>
 * Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait des statistiques d'un champ.
 *            (Minimum,Maximum,Moyenne,LatMin,LatMax,LonMin,LonMax)
 *
 * Parametres :
 *  <Obs>     : Observations
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Obs_GetStat(TObs *Obs){

   int i;
   double min,max,val;

   /*Initialiser la structure*/

   min=1e32;
   max=-1e32;

   Obs->Min=Obs->Max=0;

   for (i=0;i<Obs->Loc->Nb;i++) {
       Def_Get(Obs->Def,0,i,val);
       if (OBSVALID(val)) {
         if (val<min) {
            Obs->Min=i;
            min=val;
         }
         if (val>max) {
            Obs->Max=i;
            max=val;
         }
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Obs_PreInit>
 * Creation : Juillet 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la pre-initialisation des parametres avant le rendue.
 *
 * Parametres :
 *   <Obs>    : Observations
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Obs_PreInit(TObs *Obs) {

   /*Assigner les limites d'affichage*/
   if (!(Obs->Spec->MinMax&DATASPEC_MINSET)) Def_Get(Obs->Def,0,Obs->Min,Obs->Spec->Min);
   if (!(Obs->Spec->MinMax&DATASPEC_MAXSET)) Def_Get(Obs->Def,0,Obs->Max,Obs->Spec->Max);
   if (!(Obs->Spec->MinMax&DATASPEC_MINSET)) Obs->Spec->Min=Obs->Spec->Max<Obs->Spec->Min?Obs->Spec->Max:Obs->Spec->Min;

   if (Obs->Spec->InterMode) {
      DataSpec_Intervals(Obs->Spec,Obs->Spec->Min,Obs->Spec->Max);
   }
   DataSpec_Define(Obs->Spec);
}

/*----------------------------------------------------------------------------
 * Nom      : <Obs_Render>
 * Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectuer le rendu
 *
 * Parametres :
 *   <Interp> : L'interpreteur Tcl
 *   <Obs>    : Observations
 *   <VP>     : Le viewport ou le rendu doit etre fait
 *   <Proj>   : La projection courante
 *   <GLMode> : Mode de rendue
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Obs_Render(Tcl_Interp *Interp,TObs *Obs,ViewportItem *VP,Projection *Proj,GLuint GLMode) {

   if (Interp)
      Tcl_AppendResult(Interp,"%% Postscript des observations\n",(char*)NULL);

   Obs_PreInit(Obs);

   if (Obs->Spec->Style)
      Obs_RenderPath(Interp,Obs,VP,Proj);

   if (Obs->Spec->Icon)
      Obs_RenderIcon(Interp,Obs,VP,Proj);

   if (Obs->Spec->RenderVector)
      Obs_RenderVector(Interp,Obs,VP,Proj);

   if (Obs->Spec->Font && (Obs->Spec->RenderValue || Obs->Spec->RenderLabel || Obs->Spec->RenderCoord))
      Obs_RenderInfo(Interp,Obs,VP,Proj);

   return(1);
}

void Obs_RenderPath(Tcl_Interp *Interp,TObs *Obs,ViewportItem *VP,Projection *Proj) {

   Vect3d pix[2];
   Coord  co;
   double val;
   int    i,idx;

   glEnable(GL_DEPTH_TEST);
   glLineWidth(Obs->Spec->Width+1);
   if (Obs->Spec->Map && Obs->Spec->Map->Alpha) {
      glEnable(GL_BLEND);
   }

   /*Height markers*/
   glColor3us(0x00,0x00,0x00);
   if (Obs->Spec->Style==2 || Obs->Spec->Style==4) {
      glBegin(GL_LINES);
      for (i=0;i<Obs->Loc->Nb;i++) {
         co.Lat=Obs->Loc->Coord[i].Lat;
         co.Lon=Obs->Loc->Coord[i].Lon;
         co.Elev=0.0;

         Proj->Type->Project(Proj,&Obs->Loc->Coord[i],&pix[0],1);
         Proj->Type->Project(Proj,&co,&pix[1],1);
         glVertex3dv(pix[0]);
         glVertex3dv(pix[1]);
      }
      glEnd();
   }

   /*Shadow (Ground zero)*/
   if (Obs->Spec->Style==3 || Obs->Spec->Style==4) {
      glBegin(GL_LINE_STRIP);
      for (i=0;i<Obs->Loc->Nb;i++) {
         co.Lat=Obs->Loc->Coord[i].Lat;
         co.Lon=Obs->Loc->Coord[i].Lon;
         co.Elev=0.0;

         Proj->Type->Project(Proj,&co,&pix[1],1);
         glVertex3dv(pix[1]);
      }
      glEnd();
   }

   /*Ribbon*/
   if (Obs->Spec->Style==5) {
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glDisable(GL_CULL_FACE);

      glBegin(GL_QUAD_STRIP);
      for (i=0;i<Obs->Loc->Nb;i++) {
         val=((float*)Obs->Def->Data[0])[i];
         VAL2COL(idx,Obs->Spec,val);
         co.Lat=Obs->Loc->Coord[i].Lat;
         co.Lon=Obs->Loc->Coord[i].Lon;
         co.Elev=0.0;

         Proj->Type->Project(Proj,&Obs->Loc->Coord[i],&pix[0],1);
         Proj->Type->Project(Proj,&co,&pix[1],1);
         glColor4ubv(Obs->Spec->Map->Color[idx]);
         glVertex3dv(pix[0]);
         glVertex3dv(pix[1]);
      }
      glEnd();
   }

   /*3D Line*/
   glBegin(GL_LINE_STRIP);
   for (i=0;i<Obs->Loc->Nb;i++) {
      val=((float*)Obs->Def->Data[0])[i];
      VAL2COL(idx,Obs->Spec,val);

      Proj->Type->Project(Proj,&Obs->Loc->Coord[i],&pix[0],1);
      glColor4ubv(Obs->Spec->Map->Color[idx]);
      glVertex3dv(pix[0]);
   }
   glEnd();

   glDisable(GL_DEPTH_TEST);
   glDisable(GL_BLEND);
}

int Obs_RenderIcon(Tcl_Interp *Interp,TObs *Obs,ViewportItem *VP,Projection *Proj) {

   int    i,idx=0,n;
   float  sz,z;
   float  val;

   if (Interp) {
      Tk_CanvasPsColor(Interp,VP->canvas,Obs->Spec->Outline);
      Tcl_AppendResult(Interp,"1.0 setlinewidth 1 setlinecap 1 setlinejoin\n",(char*)NULL);
   } else {
      if (Obs->Spec->Outline) {
         glColor3us(Obs->Spec->Outline->red,Obs->Spec->Outline->green,Obs->Spec->Outline->blue);
      } else {
         glColor3us(0,0,0);
      }
   }

   glMatrixMode(GL_MODELVIEW);
   glLineWidth(2.0);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   glDisable(GL_CULL_FACE);
   glPushName(PICK_OBS);

   if (Obs->Spec->Icon) {
      glEnableClientState(GL_VERTEX_ARRAY);
      glVertexPointer(2,GL_DOUBLE,0,IconList[Obs->Spec->Icon].Co);
   }

   if (Obs->Spec->Map && Obs->Spec->Map->Alpha) {
      glEnable(GL_BLEND);
   }

   /*Outline mode*/
   if (Obs->Spec->Width && Obs->Spec->Icon && Obs->Spec->RenderTexture) {
      for (i=0;i<Obs->Loc->Nb;i++) {
         val=((float*)Obs->Def->Data[0])[i];
         VAL2COL(idx,Obs->Spec,val);

         if (idx>=0) {
            glPushName(i);
            glPushMatrix();
            Proj->Type->Locate(Proj,Obs->Loc->Coord[i].Lat,Obs->Loc->Coord[i].Lon,1);
            z=ZM(Proj,ZRef_Level2Meter(Obs->Loc->Coord[i].Elev,Obs->LevelType));
            glTranslated(0.0,0.0,z);

            sz=VP->Ratio*(Obs->Spec->Size*0.5+Obs->Spec->Width);

            if (Interp) {
               glFeedbackInit(IconList[Obs->Spec->Icon].Nb*8,GL_2D);
            }

            glScalef(sz,sz,1.0);
            glDrawArrays(IconList[Obs->Spec->Icon].Type,0,IconList[Obs->Spec->Icon].Nb);
            glPopMatrix();
            glPopName();

            if (Interp) {
               glFeedbackProcess(Interp,GL_2D);
            }
         }
      }
   }

   if (Obs->Spec->RenderVol) {
      glEnable(GL_DEPTH_TEST);
   }

   /*Display icons*/
   for (i=0;i<Obs->Loc->Nb;i++) {
      idx=-1;
      val=((float*)Obs->Def->Data[0])[i];
      if (Obs->Spec->RenderTexture && Obs->Spec->Map) {
         VAL2COL(idx,Obs->Spec,val);
      } else {
         if (Obs->Spec->InterNb) {
            if (val>=Obs->Spec->Inter[0]) {
               idx=0;
            }
         } else {
            if (val>=Obs->Spec->Min && val<=Obs->Spec->Max) {
                idx=0;
            }
         }
      }

      if (idx>=0 || (!OBSVALID(val) && (!Obs->Spec->InterNb && Obs->Spec->Max==((float*)Obs->Def->Data[0])[Obs->Max] && Obs->Spec->Min==((float*)Obs->Def->Data[0])[Obs->Min]))) {

         sz=Obs->Spec->Size*0.5*VP->Ratio;

         if (OBSVALID(val)) {
            glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
            if (Obs->Spec->RenderTexture && Obs->Spec->Map) {
               if (Interp) {
                  CMap_PostscriptColor(Interp,Obs->Spec->Map,idx);
               } else {
                  glColor4ubv(Obs->Spec->Map->Color[idx]);
               }
            }
         } else {
            glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
            if (Interp) {
               Tk_CanvasPsColor(Interp,VP->canvas,Obs->Spec->Outline);
            } else {
               glColor3us(Obs->Spec->Outline->red,Obs->Spec->Outline->green,Obs->Spec->Outline->blue);
            }
         }

         if (Obs->Spec->Icon) {

            glPushName(i);
            glPushMatrix();
            Proj->Type->Locate(Proj,Obs->Loc->Coord[i].Lat,Obs->Loc->Coord[i].Lon,1);
            z=ZM(Proj,ZRef_Level2Meter(Obs->Loc->Coord[i].Elev,Obs->LevelType));
            glTranslated(0.0,0.0,z);

            if (Interp) {
               glFeedbackInit(IconList[Obs->Spec->Icon].Nb*40,GL_2D);
            }
            glScalef(sz,sz,1.0);
            glDrawArrays(IconList[Obs->Spec->Icon].Type,0,IconList[Obs->Spec->Icon].Nb);

            if (Obs->Spec->RenderVol && OBSVALID(val)) {
               if (Obs->Spec->RenderVol==-1) {
                  z=Proj->ZFactor*Proj->Scale*sz*fabs(val)*10/Obs->Spec->Max;
               } else {
                  z=Proj->ZFactor*Proj->Scale*sz*val*10/Obs->Spec->Max;
               }
               glDisable(GL_STENCIL_TEST);
               glBegin(GL_QUAD_STRIP);
               for (n=0;n<IconList[Obs->Spec->Icon].Nb*2;n+=2) {
                  glVertex3f(IconList[Obs->Spec->Icon].Co[n],IconList[Obs->Spec->Icon].Co[n+1],0);
                  glVertex3f(IconList[Obs->Spec->Icon].Co[n],IconList[Obs->Spec->Icon].Co[n+1],z);
               }
               glVertex3f(IconList[Obs->Spec->Icon].Co[0],IconList[Obs->Spec->Icon].Co[1],0);
               glVertex3f(IconList[Obs->Spec->Icon].Co[0],IconList[Obs->Spec->Icon].Co[1],z);
               glEnd();
               glTranslated(0.0,0.0,z);
               glDrawArrays(IconList[Obs->Spec->Icon].Type,0,IconList[Obs->Spec->Icon].Nb);
               glEnable(GL_STENCIL_TEST);
            }

            if (Interp) {
               glFeedbackProcess(Interp,GL_2D);
            }

            glPopMatrix();
            glPopName();
         }
      }
   }

   glPopName();

   glEnable(GL_CULL_FACE);
   glDisable(GL_BLEND);
   glDisable(GL_DEPTH_TEST);
   glDisableClientState(GL_VERTEX_ARRAY);

   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <Obs_RenderInfo>
 * Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectuer le rendu
 *
 * Parametres :
 *   <Interp> : L'interpreteur Tcl
 *   <Obs>    : Observations
 *   <VP>     : Le viewport ou le rendu doit etre fait
 *   <Proj>   : La projection courante
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Obs_RenderInfo(Tcl_Interp *Interp,TObs *Obs,ViewportItem *VP,Projection *Proj) {

   int    i,idx=0,y=0,len,dx=0,dy=0;
   float  sz;
   char   buf[128];
   Vect3d vr;
   Coord  co;
   float val;

   if (!Obs->Spec->Font) {
      return;
   }

   Projection_UnClip(Proj);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glLoadIdentity();

   glMatrixMode(GL_PROJECTION);
   glPushMatrix();
   glLoadIdentity();
   gluOrtho2D(0,VP->Width,0,VP->Height);

   glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),Obs->Spec->Font);

   /*Display icons*/
   for (i=0;i<Obs->Loc->Nb;i++) {
      idx=-1;
      val=((float*)Obs->Def->Data[0])[i];
      if (Obs->Spec->InterNb) {
         if (val>=Obs->Spec->Inter[0]) {
            idx=0;
         }
      } else {
         if (val>=Obs->Spec->Min && val<=Obs->Spec->Max) {
            idx=0;
         }
      }

      if (idx>=0 || (!OBSVALID(val) && (!Obs->Spec->InterNb && Obs->Spec->Max==((float*)Obs->Def->Data[0])[Obs->Max] && Obs->Spec->Min==((float*)Obs->Def->Data[0])[Obs->Min]))) {

         /*Project coordinates to 2D screen space*/
         co.Elev=ZRef_Level2Meter(Obs->Loc->Coord[i].Elev,Obs->LevelType);
         co.Lat=Obs->Loc->Coord[i].Lat;
         co.Lon=Obs->Loc->Coord[i].Lon;

         /*If visible, draw text*/
         if (Projection_Pixel(Proj,VP,co,vr)) {

            sz=Obs->Spec->Size*0.5+Obs->Spec->Width*2;
            y=0;

            if (Interp) {
               Tk_CanvasPsColor(Interp,VP->canvas,Obs->Spec->Outline);
            } else {
               glColor3us(Obs->Spec->Outline->red,Obs->Spec->Outline->green,Obs->Spec->Outline->blue);
            }

            if (Obs->Spec->RenderValue && OBSVALID(val)) {
               DataSpec_Format(Obs->Spec,VAL2SPEC(Obs->Spec,val),buf);

               if (Obs->Def->Data[1] && OBSVALID(((float*)Obs->Def->Data[1])[i])) {
                  len=strlen(buf);
                  buf[len-1]='@';
                  DataSpec_Format(Obs->Spec,((float*)Obs->Def->Data[1])[i],&buf[len]);
               }

               if (!sz) {
                  dx=-Tk_TextWidth(Obs->Spec->Font,buf,strlen(buf))/2;
                  dy=-Obs->Spec->TKM.linespace/2;
               }
               glPrint(Interp,VP->canvas,buf,vr[0]+sz+dx,vr[1]+sz+dy,0);
               y-=Obs->Spec->TKM.linespace;
            }
            if (Obs->Spec->RenderLabel) {
               if (Obs->Loc->No)
                  sprintf(buf,"%s (%s)",Obs->Loc->Id[i],Obs->Loc->No[i]);
               else
                  sprintf(buf,"%s",Obs->Loc->Id[i]);

               if (!sz) {
                  dx=-Tk_TextWidth(Obs->Spec->Font,buf,strlen(buf))/2;
                  dy=-Obs->Spec->TKM.linespace/2;
               }
               glPrint(Interp,VP->canvas,buf,vr[0]+sz+dx,vr[1]+sz+dy+y,0);
               y-=Obs->Spec->TKM.linespace;
            }

            if (Obs->Spec->RenderCoord) {
               sprintf(buf,"(%.4f, %.4f)",Obs->Loc->Coord[i].Lat,Obs->Loc->Coord[i].Lon);
               if (!sz) {
                  dx=-Tk_TextWidth(Obs->Spec->Font,buf,strlen(buf))/2;
                  dy=-Obs->Spec->TKM.linespace/2;
               }
               glPrint(Interp,VP->canvas,buf,vr[0]+sz+dx,vr[1]+sz+dy+y,0);
            }
         }
      }
   }

   glPopMatrix();
   glMatrixMode(GL_MODELVIEW);
   glPopMatrix();

   Projection_Clip(Proj);
}

/*----------------------------------------------------------------------------
 * Nom      : <Obs_RenderVector>
 * Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectuer le rendu vectoriel
 *
 * Parametres :
 *   <Interp> : L'interpreteur Tcl
 *   <Obs>    : Observations
 *   <VP>     : Le viewport ou le rendu doit etre fait
 *   <Proj>   : La projection courante
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Obs_RenderVector(Tcl_Interp *Interp,TObs *Obs,ViewportItem *VP,Projection *Proj) {

   int i;

   extern void Data_RenderBarbule(int Type,int Flip,float Axis,float Lat,float Lon,float Elev,float Speed,float Dir,float Size,Projection *Proj);

   if (!Obs->Def->Data[1])
      return;

   /*Afficher toutes les barbules*/
   glMatrixMode(GL_MODELVIEW);
   glPolygonMode(GL_FRONT,GL_FILL);
   glLineWidth(1.0);
   glPushName(PICK_OBS);

   if (Obs->Spec->Outline) {
      glColor3us(Obs->Spec->Outline->red,Obs->Spec->Outline->green,Obs->Spec->Outline->blue);
   } else {
      glColor3us(0,0,0);
   }

   if (Interp) {
      glFeedbackInit(Obs->Loc->Nb*200,GL_2D);
      Tcl_AppendResult(Interp,"%% Postscript des donnees vectorielles\n0 setlinewidth 0 setlinecap 0 setlinejoin\n",(char*)NULL);
      Tk_CanvasPsColor(Interp,VP->canvas,Obs->Spec->Outline);
   }

   for(i=0;i<Obs->Loc->Nb;i++) {
      glPushName(i);
      Data_RenderBarbule(Obs->Spec->RenderVector,0,0.0,Obs->Loc->Coord[i].Lat,Obs->Loc->Coord[i].Lon,ZRef_Level2Meter(Obs->Loc->Coord[i].Elev,Obs->LevelType),((float*)Obs->Def->Data[0])[i],((float*)Obs->Def->Data[1])[i],VP->Ratio*VECTORSIZE(Obs->Spec,((float*)Obs->Def->Data[0])[i]),Proj);
      glPopName();
   }

   if (Interp) {
      i=glFeedbackProcess(Interp,GL_2D);
      Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);
   }
   glPopName();
}

/*----------------------------------------------------------------------------
 * Nom      : <Obs_Stat>
 * Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
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
static int Obs_Stat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   TObs    *obs;
   Tcl_Obj *obj;
   int      i,idx,n,f;
   double   val,dlat0,dlon0,dlat1,dlon1,dl;

   static CONST char *sopt[] = { "-tag","-max","-min","-within","-leveltype",NULL };
   enum                opt { TAG,MAX,MIN,WITHIN,LEVELTYPE };

   obs=Obs_Get(Name);
   if (!obs) {
      Tcl_AppendResult(Interp,"\n   Obs_Stats: Observation Id unknown: \"",Name,"\"",(char*)NULL);
      return TCL_ERROR;
   }
   Obs_GetStat(obs);

   for(i=0;i<Objc;i++) {
      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case TAG:
            if (Objc==1) {
               if (obs->Tag) {
                  Tcl_SetObjResult(Interp,obs->Tag);
               }
            } else {
               if (obs->Tag) {
                  Tcl_DecrRefCount(obs->Tag);
               }
               obs->Tag=Objv[++i];
               Tcl_IncrRefCount(obs->Tag);
            }
            break;

         case WITHIN:
            Tcl_ListObjLength(Interp,Objv[++i],&n);
            if (n==4) {
               Tcl_ListObjIndex(Interp,Objv[i],0,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&dlat0);
               Tcl_ListObjIndex(Interp,Objv[i],1,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&dlon0);
               Tcl_ListObjIndex(Interp,Objv[i],2,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&dlat1);
               Tcl_ListObjIndex(Interp,Objv[i],3,&obj);
               Tcl_GetDoubleFromObj(Interp,obj,&dlon1);
            }

            if (dlon0*dlon1<0) {
               dl=dlon1-dlon0;
            } else {
               dl=0;
            }
            obj=Tcl_NewListObj(0,NULL);
            for(n=0;n<obs->Loc->Nb;n++) {
               if (dlat0==0 && dlat1==0 && dlon0==0 && dlon1==0) {
                  f=1;
               } else {
                  f=0;
                  if (obs->Loc->Coord[n].Lat>=dlat0 && obs->Loc->Coord[n].Lat<=dlat1) {
                     if (dl<=180) {
                        if (obs->Loc->Coord[n].Lon>=dlon0 && obs->Loc->Coord[n].Lon<=dlon1) {
                           f=1;
                        }
                     } else {
                        if ((obs->Loc->Coord[n].Lon<=dlon0 && obs->Loc->Coord[n].Lon>-180) || (obs->Loc->Coord[n].Lon>=dlon1 && obs->Loc->Coord[n].Lon<180)) {
                           f=1;
                        }
                     }
                  }
               }
               if (f) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(n));
               }
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case MAX:
            if (Objc==1) {
               Def_Get(obs->Def,0,obs->Max,val);
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(obs->Spec,val)));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(obs->Loc->Coord[obs->Max].Lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(obs->Loc->Coord[obs->Max].Lon));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(obs->Loc->Coord[obs->Max].Elev));
               Tcl_SetObjResult(Interp,obj);
            }
            break;

         case MIN:
            if (Objc==1) {
               Def_Get(obs->Def,0,obs->Min,val);
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(obs->Spec,val)));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(obs->Loc->Coord[obs->Min].Lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(obs->Loc->Coord[obs->Min].Lon));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(obs->Loc->Coord[obs->Min].Elev));
               Tcl_SetObjResult(Interp,obj);
            }
            break;

         case LEVELTYPE:
            if (Objc==1) {
               switch(obs->LevelType) {
                  case LVL_MASL   : Tcl_SetObjResult(Interp,Tcl_NewStringObj("MASL",-1)); break;
                  case LVL_SIGMA  : Tcl_SetObjResult(Interp,Tcl_NewStringObj("SIGMA",-1)); break;
                  case LVL_PRES   : Tcl_SetObjResult(Interp,Tcl_NewStringObj("PRESSURE",-1)); break;
                  case LVL_UNDEF  : Tcl_SetObjResult(Interp,Tcl_NewStringObj("UNDEFINED",-1)); break;
                  case LVL_MAGL   : Tcl_SetObjResult(Interp,Tcl_NewStringObj("MAGL",-1)); break;
                  case LVL_HYBRID : Tcl_SetObjResult(Interp,Tcl_NewStringObj("HYBRID",-1)); break;
                  case LVL_THETA  : Tcl_SetObjResult(Interp,Tcl_NewStringObj("THETA",-1)); break;
                  case LVL_ETA    : Tcl_SetObjResult(Interp,Tcl_NewStringObj("ETA",-1)); break;
                  case LVL_GALCHEN: Tcl_SetObjResult(Interp,Tcl_NewStringObj("GALCHEN",-1)); break;
               }
            } else {
               i++;
               if (strcmp(Tcl_GetString(Objv[i]),"MASL")==0) {
                  obs->LevelType=LVL_MASL;
               } else if (strcmp(Tcl_GetString(Objv[i]),"SIGMA")==0) {
                  obs->LevelType=LVL_SIGMA;
               } else if (strcmp(Tcl_GetString(Objv[i]),"PRESSURE")==0) {
                  obs->LevelType=LVL_PRES;
               } else if (strcmp(Tcl_GetString(Objv[i]),"UNDEFINED")==0) {
                  obs->LevelType=LVL_UNDEF;
               } else if (strcmp(Tcl_GetString(Objv[i]),"MAGL")==0) {
                  obs->LevelType=LVL_MAGL;
               } else if (strcmp(Tcl_GetString(Objv[i]),"HYBRID")==0) {
                  obs->LevelType=LVL_HYBRID;
               } else if (strcmp(Tcl_GetString(Objv[i]),"THETA")==0) {
                  obs->LevelType=LVL_THETA;
               } else if (strcmp(Tcl_GetString(Objv[i]),"ETA")==0) {
                  obs->LevelType=LVL_ETA;
               } else if (strcmp(Tcl_GetString(Objv[i]),"GALCHEN")==0) {
                  obs->LevelType=LVL_GALCHEN;
               } else{
                  Tcl_AppendResult(Interp,"Obs_Stat: Invalid level type, must be [ GALCHEN ETA HYBRID MAGL MASL PRESSURE SIGMA THETA UNDEFINED ]",(char*)NULL);
                  return TCL_ERROR;
               }
            }
      }
   }

   return TCL_OK;
}
