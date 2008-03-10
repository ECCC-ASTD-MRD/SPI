/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers d'observations
 * Fichier      : tclMetObs.c
 * Creation     : Avril 2006 - J.P. Gauthier
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
 * Modification :
 *
 *   Nom        :
 *   Date       :
 *   Description:
 *
 *=========================================================
 */

#include "tclMetObs.h"
#include "Projection.h"
#include <math.h>

static TBUFRTable *BUFRTable=NULL;
static int         BUFRTableSize=0;
TCL_DECLARE_MUTEX(MUTEX_BURPFILE)

#define fgetskip(BYTES,LEN,STREAM)   BYTES[0]='\0';while (fgets(BYTES,LEN,STREAM) && BYTES[0]=='#')

/* HashTable Tcl pour les observations et rapport*/
static Tcl_HashTable MetObsTable;
static Tcl_HashTable MetRepTable;
static long          MetRepNo=0;
static int           MetObsInit=0;

static CONST char *LVLTYPE[] = { "MASL","SIGMA","PRESSURE","UNDEFINED","MAGL","HYBRID","THETA","ETA","GALCHEN" };

static int MetObs_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int MetObs_Create(Tcl_Interp *Interp,char* Name);
static int MetObs_Destroy(Tcl_Interp *Interp,char *Name);
static int MetObs_Config(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int MetObs_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int MetObs_Stat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int MetObs_FreeHash(Tcl_Interp *Interp,char *Name);
static int MetObs_Table(Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);

static int MetReport_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int MetReport_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);

int  MetObs_Render(Tcl_Interp *Interp,TMetObs *Obs,ViewportItem *VP,Projection *Proj,GLuint GLMode);
int  MetObs_RenderIcon(Tcl_Interp *Interp,TDataSpec *Spec,double Alpha,double Value,ViewportItem *VP,Projection *Proj);
void MetObs_RenderInfo(Tcl_Interp *Interp,TDataSpec *Spec,char *String,ViewportItem *VP,Projection *Proj,int Line,int DX,int DY);

extern TIcon IconList[];

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclMetObs_Init>
 * Creation     : Avril 2006 J.P. Gauthier
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
 */
int TclMetObs_Init(Tcl_Interp *Interp) {

   if (!MetObsInit++) {
      Tcl_InitHashTable(&MetObsTable,TCL_STRING_KEYS);
      Tcl_InitHashTable(&MetRepTable,TCL_STRING_KEYS);
   }
   Tcl_CreateObjCommand(Interp,"metobs",MetObs_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);
   Tcl_CreateObjCommand(Interp,"metreport",MetReport_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_Cmd>
 * Creation      : Avril 2006 J.P. Gauthier
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
 * Modification    :
 *   Nom         :
 *   Date        :
 *   Description :
 *---------------------------------------------------------------------------------------------------------------
*/

static int MetObs_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   TMetObs    *obs;

   int         idx,c,n;
   static CONST char *sopt[] = { "create","read","write","free","define","stats","is","all","wipe","table",NULL };
   enum               opt { CREATE,READ,WRITE,FREE,DEFINE,STATS,IS,ALL,WIPE,TABLE };

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
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id [files]");
            return(TCL_ERROR);
         }
         if (MetObs_Create(Interp,Tcl_GetString(Objv[2]))==TCL_ERROR) {
            return(TCL_ERROR);
         }
         obs=MetObs_Get(Tcl_GetString(Objv[2]));
         for(c=3;c<Objc;c++) {
            if (MetObs_LoadBURP(Interp,Tcl_GetString(Objv[c]),obs)==TCL_ERROR) {
               return(TCL_ERROR);
            }
         }
         return(TCL_OK);
         break;

      case READ:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id file");
            return(TCL_ERROR);
         }
         obs=MetObs_Get(Tcl_GetString(Objv[2]));
         if (!obs) {
            Tcl_AppendResult(Interp,"invalid observation",(char*)NULL);
            return(TCL_ERROR);
         }
         return(MetObs_LoadBURP(Interp,Tcl_GetString(Objv[3]),obs));

         break;

      case WRITE:
         if(Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"file { ids } title");
            return(TCL_ERROR);
         }
         return(MetObs_WriteASCII(Interp,Tcl_GetString(Objv[2]),Objv[3],Tcl_GetString(Objv[4])));
         break;

      case FREE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            MetObs_FreeHash(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case DEFINE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id ?option?");
            return(TCL_ERROR);
         }
         return(MetObs_Define(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case STATS:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         return(MetObs_Stat(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case TABLE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id ?option?");
            return(TCL_ERROR);
         }
         return(MetObs_Table(Interp,Objc-2,Objv+2));
         break;

      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         if (MetObs_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case ALL:
         Tcl_HashAll(Interp,&MetObsTable);
         break;

      case WIPE:
         MetObs_Wipe();
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <MetObs_Table>
 * Creation : Avril 2006 J.P. Gauthier
 *
 * But      : Definition des tables de corresponcances des donnees'observations
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
static int MetObs_Table(Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj   *obj;
   int        i,idx,no;
   long       code;
   FILE      *fid;
   char      buf[256];
   static CONST char *sopt[] = { "-read","-code","-desc","-insert",NULL };
   enum                opt { READ,CODE,DESC,INSERT };

   for (i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case READ:
            if (Objc==1) {
            } else {
               if (!(fid=fopen(Tcl_GetString(Objv[++i]),"r"))) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Table: Invalid table file",(char*)NULL);
                  return(TCL_ERROR);
               }

               /*How big is the table*/
               fgets(buf,256,fid);
               while(!feof(fid)) {
                  fgets(buf,256,fid);
                  if (isdigit(buf[0])) BUFRTableSize++;
               }
               if (BUFRTable) {
                  free(BUFRTable);
               }
               BUFRTableSize++;
               BUFRTable=(TBUFRTable*)malloc(BUFRTableSize*sizeof(TBUFRTable));

               /*Read in the table*/
               rewind(fid);
               fgets(buf,256,fid);
               BUFRTableSize=0;
               while(!feof(fid)) {
                  fgets(buf,256,fid);
                  if (isdigit(buf[0])) {
                     BUFRTable[BUFRTableSize].No=atoi(buf);
                     strncpy(BUFRTable[BUFRTableSize].Desc,&buf[8],43);BUFRTable[BUFRTableSize].Desc[43]='\0';
                     strncpy(BUFRTable[BUFRTableSize].Unit,&buf[52],12);BUFRTable[BUFRTableSize].Unit[12]='\0';
                     strtrim(BUFRTable[BUFRTableSize].Desc,' ');
                     strtrim(BUFRTable[BUFRTableSize].Unit,' ');
                     BUFRTableSize++;
                  }
               }
               BUFRTable[BUFRTableSize].No=0;
               strcpy(BUFRTable[BUFRTableSize].Desc,"UNKNOWN");
               strcpy(BUFRTable[BUFRTableSize].Unit,"UNKNOWN");
            }
            break;

         case CODE:
            if(Objc<2) {
               Tcl_WrongNumArgs(Interp,1,Objv,"code [value] [units]");
               return(TCL_ERROR);
            }
            Tcl_GetLongFromObj(Interp,Objv[++i],&code);
            if ((code=MetObs_BURPFindTableCode(code))==-1 || code==BUFRTableSize) {
               Tcl_AppendResult(Interp,"\n   MetObs_Table: Could not find code",(char*)NULL);
               return(TCL_ERROR);
            }
            if (Objc>2) {
               strcpy(BUFRTable[code].Desc,Tcl_GetString(Objv[++i]));
               if (Objc>3)
                  strcpy(BUFRTable[code].Unit,Tcl_GetString(Objv[++i]));
            } else {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(MetObs_BURPGetTableDesc(code),-1));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(MetObs_BURPGetTableUnit(code),-1));
               Tcl_SetObjResult(Interp,obj);
            }
            break;

         case DESC:
            if(Objc<2) {
               Tcl_WrongNumArgs(Interp,1,Objv,"desc [value]");
               return(TCL_ERROR);
            }
            if ((code=MetObs_BURPFindTableDesc(Tcl_GetString(Objv[++i])))==-1) {
               Tcl_AppendResult(Interp,"\n   MetObs_Table: Could not find description",(char*)NULL);
               return(TCL_ERROR);
            }

            if (Objc>2) {
               Tcl_GetLongFromObj(Interp,Objv[++i],&code);
               BUFRTable[code].No=code;
            } else {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(BUFRTable[code].No));
            }
            break;

         case INSERT:
            if(Objc<4) {
               Tcl_WrongNumArgs(Interp,1,Objv,"code value units");
               return(TCL_ERROR);
            }
            Tcl_GetLongFromObj(Interp,Objv[++i],&code);
            if ((no=MetObs_BURPFindTableCode(code))!=-1 && no!=BUFRTableSize) {
               Tcl_AppendResult(Interp,"\n   MetObs_Table: code already exists",(char*)NULL);
               return(TCL_ERROR);
            }

            BUFRTableSize++;
            BUFRTable=(TBUFRTable*)realloc(BUFRTable,(BUFRTableSize+1)*sizeof(TBUFRTable));
            BUFRTable[BUFRTableSize-1].No=code;
            strcpy(BUFRTable[BUFRTableSize-1].Desc,Tcl_GetString(Objv[++i]));
            strcpy(BUFRTable[BUFRTableSize-1].Unit,Tcl_GetString(Objv[++i]));
            BUFRTable[BUFRTableSize].No=0;
            strcpy(BUFRTable[BUFRTableSize].Desc,"UNKNOWN");
            strcpy(BUFRTable[BUFRTableSize].Unit,"UNKNOWN");
            break;

      }
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <MetObs_Define>
 * Creation : Avril 2006 J.P. Gauthier
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
static int MetObs_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj      *obj,*sub,*subsub;
   TMetObs      *obs;
   TMetLoc      *loc;
   TMetElem     *elem;
   TMetElemData *data;
   TMetModel    *mdl;
   long          time;
   int           e,t,i,j,d,v,idx,code,nv;
   float        *valf;
   double        val;

   static CONST char *sopt[] = { "-INFO","-ADDINFO","-COORD","-ID","-NO","-ELEMENT","-REPORT","-NB","-DATE","-DATE0","-DATE1","-VALID","-MODEL","-PERSISTANCE","-CACHE","-PIXEL",NULL };
   enum                opt { INFO,ADDINFO,COORD,ID,NO,ELEMENT,REPORT,NB,DATE,DATE0,DATE1,VALID,MODEL,PERSISTANCE,CACHE,PIXEL };

   obs=MetObs_Get(Name);
   if (!obs) {
      Tcl_AppendResult(Interp,"\n   MetObs_Define: Observation id unknown: \"",Name,"\"",(char*)NULL);
      return(TCL_ERROR);
   }

   for (i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case INFO:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for(j=0;j<obs->NbInfo;j++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(obs->Info[j],-1));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               loc=TMetLoc_Find(obs,Tcl_GetString(Objv[++i]),MET_TYPEID);
               if (!loc) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Invalid observation id",(char*)NULL);
                  return(TCL_ERROR);
               }

               if (Objc==2) {
                  obj=Tcl_NewListObj(0,NULL);
                  for(j=0;j<obs->NbInfo;j++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(loc->Info[j],-1));
                  }
                  Tcl_SetObjResult(Interp,obj);
               } else {
                  i++;
                  for(j=0;j<obs->NbInfo;j++) {
                     if (strcmp(obs->Info[j],Tcl_GetString(Objv[i]))==0)
                        break;
                  }
                  if (j==obs->NbInfo) {
                     Tcl_AppendResult(Interp,"\n   MetObs_Define: Unknown information tag",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  if (Objc==3) {
                     Tcl_SetObjResult(Interp,Tcl_NewStringObj(loc->Info[j],-1));
                  } else if (Objc==4) {
                     if (loc->Info[j]) free(loc->Info[j]);
                     loc->Info[j]=strdup(Tcl_GetString(Objv[i++]));
                  } else {
                     Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -INFO [id] [info] [value]\"",(char*)NULL);
                     return(TCL_ERROR);
                  }
               }
            }
            break;

         case ADDINFO:
            if (Objc<2) {
               Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -ADDINFO info\"",(char*)NULL);
               return(TCL_ERROR);
            } else {

               i++;
               for(j=0;j<obs->NbInfo;j++) {
                  if (strcmp(obs->Info[j],Tcl_GetString(Objv[i]))==0)
                     break;
               }
               if (j<obs->NbInfo) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Information tag already exist",(char*)NULL);
                  return(TCL_ERROR);
               }

               obs->Info=realloc(obs->Info,(j+1)*sizeof(char*));
               obs->Info[j]=strdup(Tcl_GetString(Objv[i]));

               loc=obs->Loc;
               while(loc) {
                  loc->Info=realloc(loc->Info,(j+1)*sizeof(char*));
                  loc->Info[j]=NULL;
                  loc=loc->Next;
               }
            }
            break;

        case COORD:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               loc=obs->Loc;
               while(loc) {
                  sub=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(loc->Coord.lat));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(loc->Coord.lon));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(loc->Coord.elev));
                  Tcl_ListObjAppendElement(Interp,obj,sub);
                  loc=loc->Next;
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               loc=TMetLoc_Find(obs,Tcl_GetString(Objv[++i]),MET_TYPEID);
               if (!loc) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Invalid observation id",(char*)NULL);
                  return(TCL_ERROR);
               }
               if (Objc==2) {
                  obj=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc->Coord.lat));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc->Coord.lon));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc->Coord.elev));
                  Tcl_SetObjResult(Interp,obj);
              } else if (Objc==4) {
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc->Coord.lat);
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc->Coord.lon);
                  loc->Coord.elev=0.0;
               } else if (Objc==5) {
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc->Coord.lat);
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc->Coord.lon);
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc->Coord.elev);
               } else {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -COORD [id] [lat - lon] [elev]\"",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case ID:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               loc=obs->Loc;
               while(loc) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(loc->Id,-1));
                  loc=loc->Next;
               }
               Tcl_SetObjResult(Interp,obj);
            } else if (Objc==2) {
               loc=TMetLoc_Find(obs,Tcl_GetString(Objv[++i]),MET_TYPEID);
               if (loc) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Observation id already exist",(char*)NULL);
                  return(TCL_ERROR);
               }
               TMetLoc_New(obs,Tcl_GetString(Objv[i]),NULL,0.0,0.0,0.0);
           } else {
               Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -ID [id]\"",(char*)NULL);
               return(TCL_ERROR);
            }
            break;

         case PIXEL:
            if (Objc!=2 && Objc!=4) {
               Tcl_WrongNumArgs(Interp,4,Objv,"id [x y]");
               return(TCL_ERROR);
            }
            loc=TMetLoc_Find(obs,Tcl_GetString(Objv[++i]),MET_TYPEID);
            if (!loc) {
               Tcl_AppendResult(Interp,"\n   MetObs_Define: Observation id does not exist",(char*)NULL);
               return(TCL_ERROR);
            }
            if (Objc==2) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc->Pix[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc->Pix[1]));
               Tcl_SetObjResult(Interp,obj);
               return(TCL_OK);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc->Pix[0]);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc->Pix[1]);
            }
            break;

         case NO:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               loc=obs->Loc;
               while(loc) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(loc->No,-1));
                  loc=loc->Next;
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               loc=TMetLoc_Find(obs,Tcl_GetString(Objv[++i]),MET_TYPEID);
               if (!loc) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Invalid observation id",(char*)NULL);
                  return(TCL_ERROR);
               }
               if (Objc==2) {
                  obj=Tcl_NewStringObj(loc->No,-1);
                  Tcl_SetObjResult(Interp,obj);
               } else if (Objc==3) {
                  if (loc->No) free(loc->No);
                  loc->No=strdup(Tcl_GetString(Objv[++i]));
               } else {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -NO [id] [no]\"",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case ELEMENT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_DuplicateObj(obs->Elems));
            } else {
               loc=TMetLoc_Find(obs,Tcl_GetString(Objv[++i]),MET_TYPEID);
               if (!loc) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Invalid observation id",(char*)NULL);
                  return(TCL_ERROR);
               }

               if (Objc==2) {
                  sub=Tcl_NewObj();
                  obj=Tcl_NewListObj(0,NULL);
                  elem=loc->Elems;
                  while(elem) {
                     for(d=0;d<elem->NData;d++) {
                        for(e=0;e<elem->EData[d]->Ne;e++) {
                           Tcl_SetStringObj(sub,BUFRTable[elem->EData[d]->Code[e]].Desc,-1);
                           if (Tcl_ListObjFind(Interp,obj,sub)==-1) {
                              Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(BUFRTable[elem->EData[d]->Code[e]].Desc,-1));
                           }
                        }
                     }
                     elem=elem->Next;
                  }
                  Tcl_SetObjResult(Interp,obj);
               } else {
                  if ((code=MetObs_BURPFindTableCodeOrDesc(Interp,Objv[2]))==-1) {
                     Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong element",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  ++i;

                  if (Objc==3) {
                     obj=Tcl_NewListObj(0,NULL);
                     elem=loc->Elems;

                     while(elem) {
                        sub=Tcl_NewListObj(0,NULL);
                        subsub=Tcl_NewListObj(0,NULL);
                        Tcl_ListObjAppendElement(Interp,sub,Tcl_NewLongObj(elem->Time));
                        Tcl_ListObjAppendElement(Interp,sub,subsub);
                        Tcl_ListObjAppendElement(Interp,obj,sub);
                        for(d=0;d<elem->NData;d++) {
                           data=elem->EData[d];
                           for(e=0;e<data->Ne;e++) {
                              if (data->Code[e]==code) {
                                 for(v=0;v<data->Nv;v++) {
                                    for(t=0;t<data->Nt;t++) {
                                       Tcl_ListObjAppendElement(Interp,subsub,Tcl_NewDoubleObj(MetObs_GetData(data,e,v,t)));
                                    }
                                 }
                              }
                           }
                        }
                        elem=elem->Next;
                     }
                     Tcl_SetObjResult(Interp,obj);
                  } else if (Objc==4) {
                     obj=Tcl_NewListObj(0,NULL);
                     Tcl_GetLongFromObj(Interp,Objv[++i],&time);
                     if ((elem=TMetElem_Find(loc,time,obs->Strict))) {
                        for(d=0;d<elem->NData;d++) {
                           data=elem->EData[d];
                           for(e=0;e<data->Ne;e++) {
                              if (data->Code[e]==code) {
                                 for(v=0;v<data->Nv;v++) {
                                    for(t=0;t<data->Nt;t++) {
                                       Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(MetObs_GetData(data,e,v,t)));
                                    }
                                 }
                              }
                           }
                        }
                     }
                     ++i;
                     Tcl_SetObjResult(Interp,obj);
                  } else if (Objc==5) {
                     Tcl_GetLongFromObj(Interp,Objv[++i],&time);
                     Tcl_ListObjLength(Interp,Objv[++i],&nv);
                     valf=(float*)malloc(nv*sizeof(float));
                     for(v=0;v<nv;v++) {
                        Tcl_ListObjIndex(Interp,Objv[i],v,&obj);
                        Tcl_GetDoubleFromObj(Interp,obj,&val);
                        valf[v]=val;
                     }

                     if (!(data=TMetElem_Insert(loc,0,time,1,nv,1,valf,&code))) {
                        Tcl_AppendResult(Interp,"\n   MetObs_Define: Unable to add element",(char*)NULL);
                        free(valf);
                        return(TCL_ERROR);
                     }
                     obj=Tcl_NewStringObj(MetObs_BURPGetTableDesc(code),-1);
                     if (Tcl_ListObjFind(Interp,obs->Elems,obj)==-1) {
                        Tcl_ListObjAppendElement(Interp,obs->Elems,obj);
                     }
                     free(valf);
                  } else {
                     Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -ELEMENT [id] [element] [time] [value]\"",(char*)NULL);
                     return(TCL_ERROR);
                  }
               }
            }
            break;

         case REPORT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_DuplicateObj(obs->Elems));
            } else {
               loc=TMetLoc_Find(obs,Tcl_GetString(Objv[++i]),MET_TYPEID);
               if (!loc) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Invalid observation id",(char*)NULL);
                  return(TCL_ERROR);
               }

               if (Objc==2) {
                  obj=Tcl_NewListObj(0,NULL);
                  sub=Tcl_NewObj();
                  elem=loc->Elems;
                  while(elem) {
                     for(d=0;d<elem->NData;d++) {
                        Tcl_ListObjAppendElement(Interp,obj,MetReport_Put(Interp,NULL,elem->EData[d]));
                     }
                     elem=elem->Next;
                  }
                  Tcl_SetObjResult(Interp,obj);
               } else if (Objc==3) {
                  Tcl_GetLongFromObj(Interp,Objv[++i],&time);
                  obj=Tcl_NewListObj(0,NULL);
                  elem=loc->Elems;
                  while(elem) {
                     if (elem->Time==time) {
                        for(d=0;d<elem->NData;d++) {
                           Tcl_ListObjAppendElement(Interp,obj,MetReport_Put(Interp,NULL,elem->EData[d]));
                        }
                     }
                     elem=elem->Next;
                  }
                  Tcl_SetObjResult(Interp,obj);
               } else if (Objc==4) {
                  Tcl_GetLongFromObj(Interp,Objv[++i],&time);
                  data=MetReport_Get(Tcl_GetString(Objv[++i]));
                  TMetElem_InsertCopy(loc,0,time,data);
                }
            }
            break;

         case NB:
            if (Objc==1) {
               j=0;
               loc=obs->Loc;
               while(loc) { j++; loc=loc->Next; }
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(j));
            }
            break;

         case DATE0:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewLongObj(obs->Time0));
            }
            break;

         case DATE1:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewLongObj(obs->Time1));
            }
            break;

         case DATE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               loc=obs->Loc;
               sub=Tcl_NewObj();
               while(loc) {
                  elem=loc->Elems;
                  while(elem) {
                     Tcl_SetLongObj(sub,elem->Time);
                     if (Tcl_ListObjFind(Interp,obj,sub)==-1) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewLongObj(elem->Time));
                     }
                     elem=elem->Next;
                  }
                  loc=loc->Next;
               }
               /*TODO Sort the list*/
               Tcl_SetObjResult(Interp,obj);
            } else {
               loc=TMetLoc_Find(obs,Tcl_GetString(Objv[++i]),MET_TYPEID);
               if (!loc) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Invalid observation id",(char*)NULL);
                  return(TCL_ERROR);
               }

               if (Objc==2) {
                  elem=loc->Elems;
                  obj=Tcl_NewListObj(0,NULL);
                  while(elem) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewLongObj(elem->Time));
                     elem=elem->Next;
                  }
                  Tcl_SetObjResult(Interp,obj);
               } else {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -DATE [id]\"",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case VALID:
            if (Objc==1) {
                Tcl_SetObjResult(Interp,Tcl_NewLongObj(obs->Time));
            } else {
               if (Objc<3) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -VALID [time] [strict]\"",(char*)NULL);
                  return(TCL_ERROR);
               } else {
                  Tcl_GetLongFromObj(Interp,Objv[++i],&obs->Time);
                  Tcl_GetBooleanFromObj(Interp,Objv[++i],&obs->Strict);
               }
             }
            break;

         case MODEL:
            if (Objc==1) {
               obs->Model->NRef++;
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(obs->Model->Name,-1));
            } else {
               if ((mdl=MetModel_Get(Tcl_GetString(Objv[++i])))) {
                  if (obs->Model) {
                     MetModel_FreeHash(Interp,obs->Model->Name);
                  }
                  obs->Model=mdl;
                  obs->Model->NRef++;
               } else {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Invalid model ","\"",Tcl_GetString(Objv[i]),"\"",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case CACHE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewLongObj(obs->Cache));
            } else {
               Tcl_GetLongFromObj(Interp,Objv[++i],&obs->Cache);
            }
            break;

         case PERSISTANCE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewLongObj(obs->Persistance));
            } else {
               Tcl_GetLongFromObj(Interp,Objv[++i],&obs->Persistance);
            }
            break;
      }
   }

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_Create>
 * Creation     : Avril 2006 J.P. Gauthier
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
static int MetObs_Create(Tcl_Interp *Interp,char *Name) {

   TMetObs *obs;

   if (!(obs=(TMetObs*)Tcl_HashPut(Interp,&MetObsTable,Name,sizeof(TMetObs)))) {
      return(TCL_ERROR);
   }

   obs->Tag    = NULL;
   obs->FId    = -1;
   obs->Elems  = Tcl_NewListObj(0,NULL);
   obs->Time0  = 0;
   obs->Time1  = 0;
   obs->Loc    = NULL;
   obs->Time   = 0;
   obs->Cache  = 0;
   obs->Persistance = 0;
   obs->Strict = 0;
   obs->Info   = NULL;
   obs->NbInfo = 0;
   obs->NoData = -999.0f;

   MetModel_Create(Interp,Name);
   obs->Model = MetModel_Get(Name);

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_FreeHash>
 * Creation     : Avril 2006 J.P. Gauthier
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
static int MetObs_FreeHash(Tcl_Interp *Interp,char *Name) {

   TMetObs *obs=NULL;

   if ((obs=(TMetObs*)Tcl_HashDel(&MetObsTable,Name))) {
      MetObs_Free(obs);
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_Free>
 * Creation     : Juin 2006 J.P. Gauthier
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
void MetObs_Free(TMetObs *Obs) {

   int n;

   MetObs_LocFree(Obs->Loc);

   if (Obs->Info) {
      for (n=0;n<Obs->NbInfo;n++){
         free(Obs->Info[n]);
      }
      free(Obs->Info);
   }

   if (Obs->Tag)   Tcl_DecrRefCount(Obs->Tag);
   if (Obs->Elems) Tcl_DecrRefCount(Obs->Elems);

   free(Obs);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_Get>
 * Creation     : Avril 2006 J.P. Gauthier
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
TMetObs* MetObs_Get(char *Name) {
   return((TMetObs*)Tcl_HashGet(&MetObsTable,Name));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_Grid>
 * Creation     : Fevrier 2007 J.P. Gauthier
 *
 * But          : Reprojecter les metobs sur une grille (Ref).
 *
 * Parametres   :
 *   <Ref>      : Georeference de reprojection
 *   <Obs>      : Observation
 *   <Time>     : Temps en secondes
 *   <Elem>     : Element
 *   <NObs>     : Nombre d'obs reprojectee.
 *   <Extrap>   : Reprojeter en dehors du domaine
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
Vect3d *MetObs_Grid(TGeoRef *Ref,TMetObs *Obs,long Time,char *Elem,int *NObs,int Extrap) {

   int           n,j=0,k,skip,code;
   TMetLoc      *loc;
   TMetElem     *elem;
   TMetElemData *data;
   Vect3d       *pos=NULL;

   *NObs=0;

   if ((code=MetObs_BURPFindTableDesc(Elem))!=-1 && code!=BUFRTableSize) {

      loc=Obs->Loc;
      while(loc) { j++; loc=loc->Next; }

      if ((pos=(Vect3d*)malloc(j*sizeof(Vect3d)))) {
         loc=Obs->Loc;
         while(loc) {

            j=Ref->UnProject(Ref,&pos[*NObs][0],&pos[*NObs][1],loc->Coord.lat,loc->Coord.lon,Extrap,1);

            skip=0;
            for(k=0;k<*NObs;k++) {
               if (pos[*NObs][0]==pos[k][0] && pos[*NObs][1]==pos[k][1]) {
                  skip=1;
                  break;
               }
            }

            if (!skip && (Extrap || j)) {
               /*Get the element for the specific time*/
               if ((elem=TMetElem_Find(loc,Time,Obs->Strict))) {

                  /*Get the specific data*/
                  for(n=0;n<elem->NData;n++) {
                     data=elem->EData[n];
                     pos[*NObs][2]=TMetElem_Value(data,code,0,0);
                     if (MET_VALID(pos[*NObs][2],Obs->NoData)) {
                        (*NObs)++;
                        break;
                     }
                  }
               }
            }
            loc=loc->Next;
         }
      }
   }
   return(pos);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_WriteASCII>
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int MetObs_WriteASCII(Tcl_Interp *Interp,char *File,Tcl_Obj *List,char *Title) {

   return(TCL_OK);
}

int MetObs_BURPFindTableCodeOrDesc(Tcl_Interp *Interp,Tcl_Obj *Code) {
   int code;

   if (Tcl_GetIntFromObj(Interp,Code,&code)==TCL_OK) {
      code=MetObs_BURPFindTableCode(code);
   } else {
      code=MetObs_BURPFindTableDesc(Tcl_GetString(Code));
   }
   return(BUFRTable[code].No==0?-1:code);
}

int MetObs_BURPFindTableCode(unsigned int Code) {

   int i=0;

   if (!BUFRTable) {
      return(-1);
   }

   while(BUFRTable[i].No) {
      if (BUFRTable[i].No==Code)
         break;
      i++;
   }

   return(i);
}

int MetObs_BURPFindTableDesc(char *Desc) {

   int i=0;

   if (!BUFRTable || !Desc) {
      return(-1);
   }

   while(BUFRTable[i].No) {
      if (strcmp(BUFRTable[i].Desc,Desc)==0)
         break;
      i++;
   }
   return(i);
}

char* MetObs_BURPGetTableDesc(int Idx) {

   if (BUFRTable && Idx>0) {
      return(BUFRTable[Idx].Desc);
   } else {
      return(NULL);
   }
}

char* MetObs_BURPGetTableUnit(int Idx) {

   if (BUFRTable && Idx>0) {
      return(BUFRTable[Idx].Unit);
   } else {
      return(NULL);
   }
}

TMetLoc *TMetLoc_Find(TMetObs *Obs,char *Id,int Type) {
   TMetLoc *loc=Obs->Loc;

   while(loc) {
      if (Type) {
         if (strcmp(loc->No,Id)==0) {
            break;
         }
      } else {
         if (strcmp(loc->Id,Id)==0) {
            break;
         }
      }
      loc=loc->Next;
   }
   return(loc);
}

TMetLoc *TMetLoc_New(TMetObs *Obs,char *Id,char *No,double Lat,double Lon,double Elev) {

   TMetLoc *loc;

   loc=(TMetLoc*)malloc(sizeof(TMetLoc));
   loc->Id=Id?strdup(Id):NULL;
   loc->No=No?strdup(No):NULL;
   loc->Info=NULL;
   loc->Coord.lat=Lat;
   loc->Coord.lon=Lon;
   loc->Coord.elev=Elev;
   loc->Pix[0]=0.0;
   loc->Pix[1]=0.0;
   loc->Grid[0]=loc->Grid[1]=loc->Grid[2]=0;
   loc->Level=0;
   loc->Elems=NULL;

   /*Patch temporaire car le postscript m'aime pas les ()*/
   strrep(loc->Id,'(',' ');
   strrep(loc->Id,')',' ');

   if (Obs) {
      loc->Next=Obs->Loc;
      Obs->Loc=loc;
   } else {
      loc->Next=NULL;
   }

   return(loc);
}

float TMetElem_Height(TMetElemData *Data,int Code,int Nv,int Nt) {

   int   e;
   float v;

   if (BUFRTable) {
      for(e=0;e<Data->Ne;e++) {
         if (Data->Code[e]==Code) {
            v=MetObs_GetData(Data,e,Nv,Nt);
            if (v!=-999.0f) {
               if (strcmp(BUFRTable[Data->Code[e]].Unit,"PA")==0) {
                  return(PRESS2METER(v/100.0f));
               } else {
                  return(v);
               }
            } else {
               return(v);
            }
         }
      }
   }
   return(-999.0f);
}

float TMetElem_Value(TMetElemData *Data,int Code,int Nv,int Nt) {

   int e;

   for(e=0;e<Data->Ne;e++) {
      if (Code==Data->Code[e]) {
         return(MetObs_GetData(Data,e,Nv,Nt));
      }
   }
   return(-999.0f);
}

TMetElem *TMetElem_Find(TMetLoc *Loc,long Time,int Exact) {

   TMetElem *elem=Loc->Elems;

   while(elem && Time && Time<elem->Time) {
      elem=elem->Next;
   }

   if (elem && elem->Time!=Time && Exact) {
      elem=NULL;
   }
   return(elem);
}

void TMetElem_Free(TMetElem *Elem) {

   int n;

   for(n=0;n<Elem->NData;n++) {
      TMetElemData_Free(Elem->EData[n]);
   }
   free(Elem->EData);
}

void TMetElemData_Free(TMetElemData *Data) {

   if (Data) {
      if (Data->Code) free(Data->Code);
      if (Data->Data) free(Data->Data);
   }
}

TMetElemData **TMetElem_Add(TMetLoc *Loc,time_t Time) {

   TMetElem     *new,*pre,*elem=Loc->Elems;

   /* Look for a spot in the ordered list*/
   pre=NULL;
   while(elem && Time<elem->Time) {
      pre=elem;
      elem=elem->Next;
   }

   /*If we already have this time*/
   if (elem && Time==elem->Time) {
      new=elem;
      new->NData++;
   } else {
      new=(TMetElem*)malloc(sizeof(TMetElem));
      new->Time=Time;
      new->EData=NULL;
      new->NData=1;

      if (pre) {
         new->Next=elem;
         pre->Next=new;
      } else {
         new->Next=elem;
         Loc->Elems=new;
      }
   }

   /*Create a new data bloc*/
   new->EData=(TMetElemData**)realloc(new->EData,new->NData*sizeof(TMetElemData*));
   return(&new->EData[new->NData-1]);
}

void TMetElem_Clean(TMetLoc *Loc,time_t Time) {

   TMetElem *next,*pre,*elem;

   /*Check for maximum cache time*/
   pre=Loc->Elems;
   elem=Loc->Elems->Next;
   while(elem) {
      next=elem->Next;

      if (elem->Time<Time) {
         TMetElem_Free(elem);
         free(elem);
         pre->Next=NULL;
      } else {
         pre=elem;
      }
      elem=next;
   }
}

TMetElemData *TMetElem_Insert(TMetLoc *Loc,time_t Min,time_t Time,int Ne,int Nv,int Nt,float *Data,int *Codes) {

   TMetElemData **ptr,*data=NULL;

   ptr=TMetElem_Add(Loc,Time);
   *ptr=data=(TMetElemData*)malloc(sizeof(TMetElemData));

   data->Ne=Ne;
   data->Nv=Nv;
   data->Nt=Nt;
   data->St=0x0;
   data->Data=(float*)malloc(data->Ne*data->Nv*data->Nt*sizeof(float));
   if (Data) memcpy(data->Data,Data,data->Ne*data->Nv*data->Nt*sizeof(float));
   data->Code=(int*)malloc(data->Ne*sizeof(int));
   if (Codes) memcpy(data->Code,Codes,data->Ne*sizeof(int));

   TMetElem_Clean(Loc,Min);
   return(data);
}

TMetElemData *TMetElem_InsertCopy(TMetLoc *Loc,time_t Min,time_t Time,TMetElemData *Data) {

   TMetElemData **ptr=NULL;

   ptr=TMetElem_Add(Loc,Time);
   *ptr=Data;
   TMetElem_Clean(Loc,Min);
   return(Data);
}

int MetObs_Load(Tcl_Interp *Interp,char *File,TMetObs *Obs) {

   int type,res;

   switch (type=f77name(wkoffit)(File)) {
      case 6 : res=MetObs_LoadBURP(Interp,File,Obs); break;
      case 8 : res=MetObs_LoadBUFR(Interp,File,Obs); break;
      case 31: res=MetObs_LoadASCII(Interp,File,Obs); break;
      default: Tcl_AppendResult(Interp,"\n   MetObs_Load : Invalid file type ",File,(char*)NULL);
               res=TCL_ERROR;
   }
   return(res);
}

int MetObs_LoadBUFR(Tcl_Interp *Interp,char *File,TMetObs *Obs) {

}

int MetObs_LoadBURP(Tcl_Interp *Interp,char *File,TMetObs *Obs) {

   int       err,sz,handle,code=TCL_OK;
   int      *buf=NULL;
   time_t    time=0,dt;

   Tcl_Obj      *obj;
   TMetLoc      *loc;
   TMetElem     *elem;
   TMetElemData *data;

   int      e,sz1=0,sz2=0;

   int      hhmm,flag,codtyp,blat,blon,hgt,dx,dy,dlay,yymmdd,oars,runn,nblk,sup=0,nsup=0,xaux=0,nxaux=0;
   int      blkno,nelem,nval,nt,bfam,bdesc,btyp,nbit,bit0,datyp,bknat,bktyp,bkstp,n,k;
   int      c;
   char     stnid[10];
   int     *elems=NULL,*types=NULL,*tblval=NULL;
   float   *tblvalf=NULL;

   Tcl_MutexLock(&MUTEX_BURPFILE);

   if (Obs->FId==-1)
      Obs->FId=FSTD_FileGetId();

   dt=Obs->Time-Obs->Cache;
   dt=Obs->Time-60;
   dt=0;

   /*Setup the API*/
   c_mrfopc("MSGLVL","FATAL");
   c_mrfopr("MISSING",Obs->NoData);

   /*Open the file*/
   err=c_fnom(Obs->FId,File,"RND",0);
   if (err<0) {
      Tcl_AppendResult(Interp,"\n   MetObs_LoadBURP :  Unable to link filename ",File,(char*)NULL);
      Tcl_MutexUnlock(&MUTEX_BURPFILE);
      return(TCL_ERROR);
   }
   err=c_mrfopn(Obs->FId,"READ");
   if (err<0) {
      Tcl_AppendResult(Interp,"\n   MetObs_LoadBURP :  Could not open observation file ",File,(char*)NULL);
      c_fclos(Obs->FId);
      Tcl_MutexUnlock(&MUTEX_BURPFILE);
      return(TCL_ERROR);
   }

   /*Allocate enough words*/
   sz=c_mrfmxl(Obs->FId)*4+1024000;
   buf=(int*)malloc(sz*4);
   if (!buf) {
      Tcl_AppendResult(Interp,"\n   MetObs_LoadBURP :  Could not allocate memory",(char*)NULL);
      c_mrfcls(Obs->FId);
      c_fclos(Obs->FId);
      Tcl_MutexUnlock(&MUTEX_BURPFILE);
      return(TCL_ERROR);
   }

   obj=Tcl_NewStringObj("",0);

  /*Start reading reports*/
   handle=0;
   while((handle=c_mrfloc(Obs->FId,handle,"*********",-1,-1,-1,-1,-1,-1,0))>0) {
     *buf=sz;
      err=c_mrfget(handle,buf);
      if (err<0) {
         Tcl_AppendResult(Interp,"\n   MetObs_LoadBURP :  Allocated buffer too small",(char*)NULL);
         code=TCL_ERROR;
         break;
      }

      strcpy(stnid,"         ");
      err=c_mrbhdr(buf,&hhmm,&flag,stnid,&codtyp,&blat,&blon,&dx,&dy,&hgt,&dlay,&yymmdd,&oars,&runn,&nblk,sup,nsup,xaux,nxaux);
      if (err<0) {
         Tcl_AppendResult(Interp,"\n   MetObs_LoadBURP :  Unable to read message header",(char*)NULL);
         code=TCL_ERROR;
         break;
      }

      /*Skip if it is a resume or a header*/
      if (stnid[0]=='>')
         continue;

      /*Insert station in list if not already done*/
      strtrim(stnid,' ');
      loc=TMetLoc_Find(Obs,stnid,MET_TYPEID);
      if (!loc) {
         loc=TMetLoc_New(Obs,stnid,NULL,(blat-9000.0)/100.0,blon/100.0,hgt-400);
         loc->Grid[0]=dx;
         loc->Grid[1]=dy;
      }
      if ((time=System_DateTime2Seconds(yymmdd,hhmm*100))<0)
         continue;

      Obs->Time0=(Obs->Time0<time && Obs->Time0!=0)?Obs->Time0:time;
      Obs->Time1=Obs->Time1>time?Obs->Time1:time;

      /*Start reading the information blocs*/
      blkno=0;
      while ((blkno=c_mrbloc(buf,-1,-1,-1,blkno))>0) {
         err=c_mrbprm(buf,blkno,&nelem,&nval,&nt,&bfam,&bdesc,&btyp,&nbit,&bit0,&datyp);
         if (err<0) {
            Tcl_AppendResult(Interp,"\n   MetObs_LoadBURP :  Invalid block",(char*)NULL);
            code=TCL_ERROR;
            break;
         }

         // 0x0=New, 0x8=Correction, 0x10=Repeat, 0X18=Human Correction
//         if (bfam!=0) { continue; };
//         if (bfam&0x8) { continue; };
         if (bfam&0x10) { continue; };
//         if (bfam&0x18) { continue; };

         bknat=(btyp>>11)%16;
         bktyp=(btyp>>4)%128;
         bkstp=(btyp)%16;

         /*Skip if this is a bloc marker*/
         if ((bknat%4)!=0)
            continue;

         /*Skip if empty*/
         if ((nelem*nval*nt)==0)
            continue;

         /*Resize temporary buffers if needed*/
         if (nelem>sz1) {
            sz1=nelem;
            elems=(int*)realloc(elems,sz1*sizeof(int));
            types=(int*)realloc(types,sz1*sizeof(int));
         }
         if (nelem*nval*nt>sz2) {
            sz2=nelem*nval*nt;
            tblval=(int*)realloc(tblval,sz2*sizeof(int));
            tblvalf=(float*)realloc(tblvalf,sz2*sizeof(float));
         }

         /*Extract info*/
         err=c_mrbxtr(buf,blkno,elems,tblval);
         err=c_mrbcvt(elems,tblval,tblvalf,nelem,nval,nt,0);
         err=c_mrbdcl(elems,types,nelem);

         /*Test for superobs ..... ta daaaaaaa*/
         if (stnid[0]=='^') {
            if (stnid[1]=='^') {
               fprintf(stderr,"(DEBUG) ---- super duper\n");
            } else {

            }
            fprintf(stderr,"DEBUG) ---- dx=%f dy=%f, %i %i %i\n",dx*10.0,dy*10.0,nelem,nval,nt);
         }

         /*Get the elements code list and cache it within obs object*/
         for(e=0;e<nelem;e++) {
            /*Patch the Geopotential height since there is multiple definition for it, use the first one*/
            if (types[e]==10194 || types[e]==10009 || types[e]==7194)
               types[e]=7009;

            types[e]=MetObs_BURPFindTableCode(types[e]);

            Tcl_SetStringObj(obj,MetObs_BURPGetTableDesc(types[e]),-1);
            if (Tcl_ListObjFind(Interp,Obs->Elems,obj)==-1) {
               Tcl_ListObjAppendElement(Interp,Obs->Elems,Tcl_NewStringObj(MetObs_BURPGetTableDesc(types[e]),-1));
            }
         }

         data=TMetElem_Insert(loc,dt,time,nelem,nval,nt,tblvalf,types);
         data->St=bfam==0?MET_STATENEW:(bfam&0x8?MET_STATESCO:MET_STATEHCO);
      }
   }

   if (!Obs->Time)
      Obs->Time=Obs->Time1;

   Tcl_DecrRefCount(obj);
   if (elems)   free(elems);
   if (types)   free(types);
   if (tblval)  free(tblval);
   if (tblvalf) free(tblvalf);
   free(buf);

   /*Close the file*/
   c_mrfcls(Obs->FId);
   c_fclos(Obs->FId);

   Tcl_MutexUnlock(&MUTEX_BURPFILE);
   return(code);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_LoadASCII>
 * Creation     : Avril 2006 J.P. Gauthier
 *
 * But          : Chargement d'un fichier d'observations.
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <Name>     : Nom de l'observation dans la table
 *   <File>     : Le nom du fichier
 *   <Obs>      : Observation
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int MetObs_LoadASCII(Tcl_Interp *Interp,char *File,TMetObs *Obs) {

   TMetLoc  *loc;
   TMetElem *elem;

   FILE    *stream;
   char    buf[256];
   char    *bytes=NULL,**elems;
   int     sz,sk,nb,n,hd,k,sec;
   int     ntok,gntok,nltok;
   char    **tok,**gtok,**ltok;
   int     err=TCL_OK,date,time;
   time_t  *gtime;

   stream=fopen(File,"r");

   if (!stream) {
      Tcl_AppendResult(Interp,"\n   MetObs_LoadASCII :  Could not open observation file ",File,(char*)NULL);
      return(TCL_ERROR);
   }

   /*Read the version*/
   fgetskip(buf,256,stream);

   if (!strstr(buf,"Obs 3.1")) {
      Tcl_AppendResult(Interp,"\n   MetObs_LoadASCII :  Wrong file version while reading ",File,(char*)NULL);
      fclose(stream);
      return(TCL_ERROR);
   }

   /*Read the title*/
   fgetskip(buf,256,stream);
   if (Obs->Desc) free(Obs->Desc);
   Obs->Desc=strdup(buf);

   /*Read the header*/
   nb=255;
   while(nb==255) {
      fgetskip(buf,256,stream);
      bytes=strcatalloc(bytes,buf);
      nb=strlen(buf);
   }

   /*Split the header tokens into list*/
   Tcl_SplitList(Interp,bytes,&gntok,&gtok);

   /*Allocate big enough buffer to read data*/
   sz=strlen(bytes)*4;
   free(bytes);
   bytes=(char*)malloc(sz);

   gtime=(time_t*)calloc(gntok,sizeof(time_t));

   Obs->NbInfo=0;
   /*Parse header tokens*/
   for(n=0;n<gntok;n++) {
      if (strncmp(gtok[n],"DATA",4)==0) {
         /*Extract date and time*/
         hd=0;sec=0;
         sscanf((char*)(strrchr(gtok[n],(int)'.'))+1,"%8d%02d%02d%02d",&date,&time,&hd,&sec);
         time=time*10000+hd*100+sec;
         gtime[n]=System_DateTime2Seconds(date,time);


      } else if (strcmp(gtok[n],"ID")!=0 && strcmp(gtok[n],"NO")!=0 && strcmp(gtok[n],"LAT")!=0 && strcmp(gtok[n],"LON")!=0 && strcmp(gtok[n],"ELEV")!=0 && strcmp(gtok[n],"ELEVTYPE")!=0 && strcmp(gtok[n],"SIZE")!=0) {
         Obs->NbInfo++;
      }
   }
   Obs->Info=(char**)calloc(Obs->NbInfo,sizeof(char**));

   nb=0;
   while (!feof(stream)) {
      fgetskip(bytes,sz,stream);

      if (strlen(bytes)>10) {
         Tcl_SplitList(Interp,bytes,&ntok,&tok);

         if (ntok!=gntok) {
            Tcl_AppendResult(Interp,"\n   MetObs_LoadASCII :  Invalid number of item on following line\n",bytes,(char*)NULL);
            fclose(stream);
            return(TCL_ERROR);
         }

         /*Parse the tokens*/
         hd=0;
         for(n=0;n<ntok;n++) {
            if (strcmp(gtok[n],"LAT")==0) {                  /*Latitude information*/
               loc->Coord.lat=atof(tok[n]);
            } else if (strcmp(gtok[n],"LON")==0) {           /*Longitude information*/
               loc->Coord.lon=atof(tok[n]);
            } else if (strcmp(gtok[n],"ELEVTYPE")==0) {      /*Elevation type information*/
               if (isdigit(tok[n][0])) {
                  loc->Level=atoi(tok[n]);
               } else {
                  if (strcmp(tok[n],"MASL")==0) {
                     loc->Level=0;
                  } else if (strcmp(tok[n],"SIGMA")==0) {
                     loc->Level=1;
                  } else if (strcmp(tok[n],"PRESSURE")==0) {
                     loc->Level=2;
                  } else if (strcmp(tok[n],"UNDEFINED")==0) {
                     loc->Level=3;
                  } else if (strcmp(tok[n],"MAGL")==0) {
                     loc->Level=4;
                  } else if (strcmp(tok[n],"HYBRID")==0) {
                     loc->Level=5;
                  } else if (strcmp(tok[n],"THETA")==0) {
                     loc->Level=6;
                  } else if (strcmp(tok[n],"ETA")==0) {
                     loc->Level=7;
                  } else if (strcmp(tok[n],"GALCHEN")==0) {
                     loc->Level=8;
                  } else {
                     Tcl_AppendResult(Interp,"\n   MetObs_LoadASCII : Invalid level type, must be [ MASL SIGMA PRESSURE UNDEFINED MAGL HYBRID THETA ETA GALCHEN ]",(char*)NULL);
                     err=TCL_ERROR;
                  }
               }
            } else if (strcmp(gtok[n],"ELEV")==0) {          /*Elevation information*/
               loc->Coord.elev=atof(tok[n]);
            } else if (strcmp(gtok[n],"NO")==0) {            /*Number information*/
               loc->No=strdup(tok[n]);
               strrep(loc->No,'(',' ');
               strrep(loc->No,')',' ');
            } else if (strcmp(gtok[n],"ID")==0) {            /*Identificateur*/
               /*Insert station in list if not already done*/
               loc=TMetLoc_Find(Obs,tok[n],MET_TYPEID);
               if (!loc) {
                  loc=TMetLoc_New(Obs,tok[n],NULL,0.0,0.0,0.0);
               }
            } else if (strncmp(gtok[n],"DATA",4)==0) {       /*Values*/
/*TODO
               data=TMetElem_Insert(loc,0,gtime[n],nelem,nt*nval);

               if (tok[n][0]=='-' && tok[n][1]=='\0') {
                  ((float*)obs->Def->Data[0])[nb]=-999.0f;
               } else {
                  Tcl_SplitList(Interp,tok[n],&nltok,&ltok);
                  for(k=0;k<nltok;k++) {
                     if (!obs->Def->Data[k]) {
                        obs->Def->Data[k]=(char*)calloc(FSIZE3D(obs->Def),TData_Size[TD_Float32]);
                     }
                     Data_FromString(ltok[k],obs->Def,k,nb);
                  }
                  Tcl_Free((char*)ltok);
               }
*/
            } else {                                         /*Information*/
               if (!Obs->Info[hd])
                  Obs->Info[hd]=strdup(gtok[n]);
               if (!loc->Info)
                  loc->Info=(char**)malloc(Obs->NbInfo*sizeof(char*));
               loc->Info[hd]=strdup(tok[n]);
               hd++;
            }
         }

         nb++;
         Tcl_Free((char*)tok);
      }
   }

   /*Is there any observation in there*/
   if (!nb) {
      Tcl_AppendResult(Interp,"\n   MetObs_LoadASCII :  No observation found ",(char*)NULL);
      fclose(stream);
      return(TCL_ERROR);
   }
   fclose(stream);

   Tcl_Free((char*)gtok);
   free(bytes);

   return(err);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_LocFree>
 * Creation     : Avril 2006 J.P. Gauthier
 *
 * But          : Liberer la liste des observations et information partagee.
 *
 * Parametres   :
 *   <Loc>      : Liste de localisation
 *
 * Retour       :
 *
 * Remarques :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
void MetObs_LocFree(TMetLoc *Loc){

   TMetLoc  *lnext,*loc=Loc;
   TMetElem *enext,*elem;

   while(loc) {
      if (loc->Id) free(loc->Id);
      if (loc->No) free(loc->No);

      elem=loc->Elems;
      while(elem) {
         enext=elem->Next;
         TMetElem_Free(elem);
         free(elem);
         elem=enext;
      }

      lnext=loc->Next;
      free(loc);
      loc=lnext;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <MetObs_GetStat>
 * Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
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
 * Modifications :
 *
 *    Nom         : J.P. Gauthier
 *    Date        : Aout 2000
 *    Description : Ajout du calcul des statistiques pour des champs vectoriels
 *----------------------------------------------------------------------------
*/
void MetObs_GetStat(TMetObs *Obs,TMetModelItem *Item){

   TMetLoc      *loc;
   TMetElem     *elem;
   TMetElemData *data;
   int           n,e,v,t=0;
   double        min,max;
   float         val;

   /*Initialiser la structure*/

   min=1e32;
   max=-1e32;
   loc=Obs->Loc;

   while(loc) {
      elem=loc->Elems;
      while(elem) {
         for(n=0;n<elem->NData;n++) {
            data=elem->EData[n];
            for(e=0;e<data->Ne;e++) {
               if (data->Code[e]==Item->Code[0]) {
                  for(v=0;v<data->Nv;v++) {
//                     for(t=0;t<data->Nt;t++) {
                        val=MetObs_GetData(data,e,v,t);
                        if (MET_VALID(val,Obs->NoData)) {
                           min=min<val?min:val;
                           max=max>val?max:val;
                        }
//                     }
                  }
                  break;
               }
            }
         }
         elem=elem->Next;
      }
      loc=loc->Next;
   }

   if (!(Item->Spec->MinMax&DATASPEC_MINSET)) Item->Spec->Min=min;
   if (!(Item->Spec->MinMax&DATASPEC_MAXSET)) Item->Spec->Max=max;
   if (!(Item->Spec->MinMax&DATASPEC_MINSET)) Item->Spec->Min=Item->Spec->Max<Item->Spec->Min?Item->Spec->Max:Item->Spec->Min;
}

/*----------------------------------------------------------------------------
 * Nom      : <MetObs_Render>
 * Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
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
 * Modifications :
 *
 *    Nom         : J.P. Gauthier
 *    Date        : Aout 2000
 *    Description : Ajout du calcul des statistiques pour des champs vectoriels
 *----------------------------------------------------------------------------
*/

int MetObs_Render(Tcl_Interp *Interp,TMetObs *Obs,ViewportItem *VP,Projection *Proj,GLuint GLMode) {

   TMetLoc      *loc;
   TMetElem     *elem;
   TMetElemData *data;
   TDataSpec    *spec;
   Vect3d        pix;
   char          buf[128];
   double        z,val,dir,dx,dy,k;
   int           d,e,i,n,v,iy,idx,code,line,id;
   double        alpha=1.0;

   extern void Data_RenderBarbule(int Type,int Flip,float Axis,float Lat,float Lon,float Elev,float Speed,float Dir,float Size,Projection *Proj);

   if (!Obs->Model) {
      return(0);
   }

   if (Obs->Model->Flat) {
      glMatrixMode(GL_PROJECTION);
      glPushMatrix();
      if (GLMode==GL_SELECT) {
         glLoadMatrixd(VP->GLPick);
      } else {
         glLoadIdentity();
      }
      gluOrtho2D(0,VP->Width,0,VP->Height);

      glMatrixMode(GL_MODELVIEW);
      glPushMatrix();
      glLoadIdentity();
   } else {
      glMatrixMode(GL_MODELVIEW);
   }

   glLineWidth(1.0);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   glDisable(GL_CULL_FACE);
   glPushName(PICK_METOBS);

   if (Interp)
      Tcl_AppendResult(Interp,"%% Postscript des observations meteorologiques\n",(char*)NULL);

   if (Obs->Model->Topo && strlen(Obs->Model->Topo)) {
      glEnable(GL_DEPTH_TEST);
      code=MetObs_BURPFindTableDesc(Obs->Model->Topo);
   } else {
      code=-1;
   }

   /*For all of the sations*/
   loc=Obs->Loc;
   n=0;
   while(loc) {

      line=0;
      if (GLMode==GL_SELECT)
         glPushName(n);

      /*Check if visible*/
      if (Projection_Pixel(Proj,VP,loc->Coord,pix)) {

         /*Get the element for the specific time*/
         if ((elem=TMetElem_Find(loc,Obs->Time,Obs->Strict))) {

            /*Fix transparency on validity time persistance*/
            if (Obs->Persistance) {
               alpha=1.0-((double)(Obs->Time-elem->Time)/Obs->Persistance);
               alpha=alpha<0.0?0:alpha;
            }

            /*Loop on the model items*/
            for(i=0;i<Obs->Model->NItem;i++) {

#ifdef AQBUG
fprintf(stderr,"(AQ_DEBUG) Looping on model Items (%i)\n",i);
#endif
               if (!(spec=Obs->Model->Items[i].Spec)) {
                  continue;
               }
               if (Obs->Persistance || (spec->Map && spec->Map->Alpha)) {
                  glEnable(GL_BLEND);
               } else {
                  glDisable(GL_BLEND);
               }

               /*Assigner les limites d'affichage*/
               if (isnan(spec->Min) || isnan(spec->Max))
                   MetObs_GetStat(Obs,&Obs->Model->Items[i]);

               DataSpec_Intervals(spec,spec->Min,spec->Max);
               DataSpec_Define(spec);

               if (GLMode==GL_SELECT)
                  glPushName(i);

               /*Loop on the data*/
               for(d=0;d<elem->NData;d++) {
                  data=elem->EData[d];

                 for(e=0;e<data->Ne;e++) {
                     if (data->Code[e]==Obs->Model->Items[i].Code[0]) {
                        id=0;
                        for(v=0;v<data->Nv;v++) {

                           /*Check for validity*/
                           val=MetObs_GetData(data,e,v,0);
                           if (MET_VALID(val,Obs->NoData)) {
                              if (val<spec->Min || val>spec->Max)
                                 continue;
                              if (spec->InterNb && val<spec->Inter[0])
                                 continue;
                           }

                           if (Interp) {
                              Tk_CanvasPsColor(Interp,VP->canvas,spec->Outline);
                              Tcl_AppendResult(Interp,"1.0 setlinewidth 1 setlinecap 1 setlinejoin\n",(char*)NULL);
                           } else {
                              if (spec->Outline) {
                                 glColor4us(spec->Outline->red,spec->Outline->green,spec->Outline->blue,alpha*65535);
                              } else {
                                 glColor4us(0,0,0,alpha*65535);
                              }
                           }

                           /*Get height*/
                           if (code>-1 && (k=TMetElem_Height(data,code,v,0))!=-999.0) {
                              z=k;
                           } else {
                              z=Data_Level2Meter(loc->Level,loc->Coord.elev);
                           }

                          /*Set position within projection*/
                           glPushMatrix();

                           if (Obs->Model->Flat) {
                              if (loc->Pix[0]!=0.0 && loc->Pix[1]!=0.0) {
                                 glTranslated(loc->Pix[0],VPY(VP,loc->Pix[1]),0.0);
                              } else {
                                 glTranslated(pix[0],pix[1],0.0);
                              }
                           } else {
                              Proj->Type->Locate(Proj,loc->Coord.lat,loc->Coord.lon,1);
                              glTranslated(0.0,0.0,ZM(Proj,z));
                              glScalef(VP->Ratio,VP->Ratio,1.0);
                           }

                           dx=Obs->Model->Items[i].X*Obs->Model->Space+(Obs->Model->Flat?0:loc->Pix[0]);
                           dy=-Obs->Model->Items[i].Y*Obs->Model->Space+(Obs->Model->Flat?0:-loc->Pix[1]);

                           /*Draw the position line if needed*/
                           if ((loc->Pix[0]!=0.0 || loc->Pix[1]!=0.0) && !line) {
                              if (Interp)
                                 glFeedbackInit(20,GL_2D);
                              glLineWidth(spec->Width);
                              glBegin(GL_LINES);
                                 glVertex3d(0,0,0);
                                 if (Obs->Model->Flat) {
                                    glVertex3d(pix[0]-loc->Pix[0],pix[1]-VPY(VP,loc->Pix[1]),0.0);
                                 } else {
                                    glVertex3d(dx,dy,0.0);
                                 }
                              glEnd();
                              if (Interp) {
                                 glFeedbackProcess(Interp,GL_2D);
                                 Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);
                                 glFeedbackInit(4000,GL_2D);
                              }

                              v=spec->Width<<1;
                              glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
                              glPushMatrix();
                              if (Obs->Model->Flat)
                                 glTranslated(pix[0]-loc->Pix[0],pix[1]-VPY(VP,loc->Pix[1]),0.0);
                              glScalef(v,v,1.0);
                              glDrawCircle(64,GL_POLYGON);
                              line=1;
                              glPopMatrix();
                              if (Interp) {
                                 glFeedbackProcess(Interp,GL_2D);
                              }
                           }

                           /*Positionner dans le modele*/
                           glTranslated(dx,dy,0.0);

                           iy=spec->RenderLabel+spec->RenderCoord+spec->RenderValue-2;

                           if (id && spec->RenderLabel) iy--;
                           if (id && spec->RenderCoord) iy--;

                           if (!id) {
                              if (spec->RenderLabel && GLRender->Resolution<=1 && GLMode!=GL_SELECT) {
                                 if (loc->No) {
                                    sprintf(buf,"%s (%s)",loc->Id,loc->No);
                                 } else {
                                    sprintf(buf,"%s",loc->Id);
#ifdef AQBUG
fprintf(stderr,"(AQ_DEBUG) Drawing info ---\n");
fprintf(stderr,"(AQ_DEBUG) Drawing info (%s)\n",buf);
fprintf(stderr,"(AQ_DEBUG) Drawing info ---\n");
#endif
                                 }
                                 MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dx);
                              }

                              if (spec->RenderCoord && GLRender->Resolution<=1 && GLMode!=GL_SELECT) {
                                 sprintf(buf,"(%.4f,%.4f)",loc->Coord.lat,loc->Coord.lon);
                                 MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dx);
                              }
                              id=1;
                           }

                           if (spec->RenderVector && MET_VALID(val,Obs->NoData)) {
                              dir=TMetElem_Value(data,Obs->Model->Items[i].Code[1],v,0);
                              if (MET_VALID(dir,Obs->NoData)) {
                                 if (spec->Map && spec->MapAll) {
                                    VAL2COL(idx,spec,val);

                                    if (Interp) {
                                       CMap_PostscriptColor(Interp,spec->Map,idx);
                                    } else {
                                       glColor4ub(spec->Map->Color[idx][0],spec->Map->Color[idx][1],spec->Map->Color[idx][2],spec->Map->Color[idx][3]*alpha);
                                    }
                                 }

                                 if (Interp)
                                    glFeedbackInit(40,GL_2D);
                                 Data_RenderBarbule(spec->RenderVector,1,0.0,0.0,0.0,0.0,val,dir,VECTORSIZE(spec,val),NULL);
                                 if (Interp) {
                                    glFeedbackProcess(Interp,GL_2D);
                                    Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);
                                 }
                              }
                           }

                          if (spec->RenderValue && MET_VALID(val,Obs->NoData) && GLRender->Resolution<=1 && GLMode!=GL_SELECT) {
                              if (spec->Map && spec->MapAll) {
                                 VAL2COL(idx,spec,val);

                                 if (Interp) {
                                    CMap_PostscriptColor(Interp,spec->Map,idx);
                                 } else {
                                    glColor4ub(spec->Map->Color[idx][0],spec->Map->Color[idx][1],spec->Map->Color[idx][2],spec->Map->Color[idx][3]*alpha);
                                 }
                              }
                              DataSpec_Format(spec,VAL2SPEC(spec,val),buf);
fprintf(stderr,"(AQ_DEBUG) Drawing value ---\n");
fprintf(stderr,"(AQ_DEBUG) Drawing value (%s)\n",buf);
fprintf(stderr,"(AQ_DEBUG) Drawing value ---\n");
                              MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                           }

                           if (spec->Icon) {
                              MetObs_RenderIcon(Interp,spec,alpha,val,VP,Proj);
                           }

                           glPopMatrix();
                           if (GLMode==GL_SELECT && MET_VALID(val,Obs->NoData))
                              break;
                        }
                        break;
                     }
                  }
               }
               if (GLMode==GL_SELECT)
                  glPopName();
            }
         }
      }
      if (GLMode==GL_SELECT)
         glPopName();
      loc=loc->Next;
      n++;
   }

   glPopName();

   if (Obs->Model->Flat) {
      glPopMatrix();
      glMatrixMode(GL_PROJECTION);
      glPopMatrix();
   }
   glEnable(GL_CULL_FACE);
   glDisable(GL_BLEND);
   glDisable(GL_DEPTH_TEST);
   glDisableClientState(GL_VERTEX_ARRAY);
   glDisable(GL_DEPTH_TEST);
#ifdef AQBUG
fprintf(stderr,"(AQ_DEBUG) Done rendering\n");
#endif
   return(n);
}

int MetObs_RenderIcon(Tcl_Interp *Interp,TDataSpec *Spec,double Alpha,double Value,ViewportItem *VP,Projection *Proj) {

   int    i,idx=-1;
   double sz;
   char   buf[256];

   if (!Spec->Icon) {
      return(0);
   }

   glEnableClientState(GL_VERTEX_ARRAY);
   glVertexPointer(2,GL_DOUBLE,0,IconList[Spec->Icon].Co);
   glScalef(Spec->Size*0.5,Spec->Size*0.5,1.0);

   /*Display icons*/
   if (MET_VALID(Value,-999.0f)) {
      if (Spec->RenderTexture) {
         if (Spec->Map) {
            VAL2COL(idx,Spec,Value);
            if (idx>=0) {
               if (Interp) {
                  CMap_PostscriptColor(Interp,Spec->Map,idx);
               } else {
                 glColor4ub(Spec->Map->Color[idx][0],Spec->Map->Color[idx][1],Spec->Map->Color[idx][2],Spec->Map->Color[idx][3]*Alpha);
               }
            } else {
               return;
            }
         } else {
            if (Spec->Outline) {
               if (Interp) {
                  Tk_CanvasPsColor(Interp,VP->canvas,Spec->Outline);
                  Tcl_AppendResult(Interp,"1.0 setlinewidth 1 setlinecap 1 setlinejoin\n",(char*)NULL);
               } else {
                  glColor4us(Spec->Outline->red,Spec->Outline->green,Spec->Outline->blue,Alpha*65535);
               }
            } else {
               return;
            }
         }
      }

      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      if (Interp) glFeedbackInit(IconList[Spec->Icon].Nb*40,GL_2D);
      glDrawArrays(IconList[Spec->Icon].Type,0,IconList[Spec->Icon].Nb);
      if (Interp) glFeedbackProcess(Interp,GL_2D);
   }

   if (Spec->RenderContour && Spec->Outline) {
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
      if (Interp) {
         Tk_CanvasPsColor(Interp,VP->canvas,Spec->Outline);
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",Spec->RenderContour);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
      } else {
         glColor4us(Spec->Outline->red,Spec->Outline->green,Spec->Outline->blue,Alpha*65535);
         glLineWidth(Spec->RenderContour);
      }
      if (Interp) glFeedbackInit(IconList[Spec->Icon].Nb*40,GL_2D);
      glDrawArrays(IconList[Spec->Icon].Type,0,IconList[Spec->Icon].Nb);
      if (Interp) glFeedbackProcess(Interp,GL_2D);
   }
   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <MetObs_RenderInfo>
 * Creation : Avril 2006 2006 - J.P. Gauthier - CMC/CMOE
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
 * Modifications :
 *
 *    Nom         : J.P. Gauthier
 *    Date        : Aout 2000
 *    Description : Ajout du calcul des statistiques pour des champs vectoriels
 *----------------------------------------------------------------------------
*/
void MetObs_RenderInfo(Tcl_Interp *Interp,TDataSpec *Spec,char *String,ViewportItem *VP,Projection *Proj,int Line,int DX,int DY) {

   Tk_FontMetrics tkm;
   double dx=0,dy=0,sz=0;

   if (!Spec->Font || !String) {
      return;
   }

   if (Interp) {
      dx=DX;
      dy=DY;
   }

   Tk_GetFontMetrics(Spec->Font,&tkm);
   glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),Spec->Font);

   if (Spec->Icon) {
      sz=(Spec->Size*0.5+Spec->RenderContour);
      dx+=sz+2;
   } else {
      dx-=Tk_TextWidth(Spec->Font,String,strlen(String))/2;
   }

   dy+=sz+(Line+1)*2*(tkm.linespace/2);

   glPrint(Interp,VP->canvas,String,dx,dy,0);
}

/*----------------------------------------------------------------------------
 * Nom      : <MetObs_Stat>
 * Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
static int MetObs_Stat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   TMetObs *met;
   int      i,idx,n;
   double   val;

   static CONST char *sopt[] = { "-tag","-nodata",NULL };
   enum                opt { TAG,NODATA };

   met=MetObs_Get(Name);
   if (!met) {
      Tcl_AppendResult(Interp,"\n   MetObs_Stats: Observation Id unknown: \"",Name,"\"",(char*)NULL);
      return(TCL_ERROR);
   }

   for(i=0;i<Objc;i++) {
      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case TAG:
            if (Objc==1) {
               if (met->Tag) {
                  Tcl_SetObjResult(Interp,met->Tag);
               }
            } else {
               if (met->Tag) {
                  Tcl_DecrRefCount(met->Tag);
               }
               met->Tag=Objv[++i];
               Tcl_IncrRefCount(met->Tag);
            }
            break;

         case NODATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(met->NoData));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&val);
               met->NoData=val;
            }
            break;
      }
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <MetObs_Wipe>
 * Creation : Juin 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Liberer toutes la memoire allouee par ce package.
 *
 * Parametres     :
 *
 * Retour:
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
void MetObs_Wipe() {

   Tcl_HashSearch ptr;
   Tcl_HashEntry  *entry=NULL;

   printf("(INFO) MetObs_Wipe: Wiping allocated memory\n");

   entry=Tcl_FirstHashEntry(&MetObsTable,&ptr);

   while (entry) {
      MetObs_Free((TMetObs*)Tcl_GetHashValue(entry));
      Tcl_DeleteHashEntry(entry);
      entry=Tcl_FirstHashEntry(&MetObsTable,&ptr);
   }

   entry=Tcl_FirstHashEntry(&MetRepTable,&ptr);

   while (entry) {
      TMetElemData_Free((TMetElemData*)Tcl_GetHashValue(entry));
      Tcl_DeleteHashEntry(entry);
      entry=Tcl_FirstHashEntry(&MetRepTable,&ptr);
   }

   Tcl_DeleteHashTable(&MetRepTable);
   Tcl_DeleteHashTable(&MetObsTable);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetReport_Cmd>
 * Creation     : Juin 2007 J.P. Gauthier
 *
 * But          : Effectuer les commandes metreport du package
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
 * Modification    :
 *   Nom         :
 *   Date        :
 *   Description :
 *---------------------------------------------------------------------------------------------------------------
*/

static int MetReport_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   TMetObs      *obs;
   TMetElemData *data;

   int         idx,c,n;
   static CONST char *sopt[] = { "create","free","define","stats","is","all",NULL };
   enum               opt { CREATE,FREE,DEFINE,STATS,IS,ALL };

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
         if (!(data=(TMetElemData*)malloc(sizeof(TMetElemData)))) {
            Tcl_AppendResult(Interp,"\n   MetReport_Cmd: Unable to allocate memory for report",(char*)NULL);
            return(TCL_ERROR);
         }
         data->Ne=data->Nv=data->Nt=0;
         data->Code=data->Data=NULL;

         if (!MetReport_Put(Interp,Tcl_GetString(Objv[2]),data)) {
            return(TCL_ERROR);
         }
         break;

      case FREE:
         for(n=2;n<Objc;n++) {
            MetReport_Destroy(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case DEFINE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id ?option?");
            return(TCL_ERROR);
         }
         return(MetReport_Define(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         if (MetReport_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case ALL:
         Tcl_HashAll(Interp,&MetRepTable);
         break;
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <MetReport_Define>
 * Creation : Juin 2007 J.P. Gauthier
 *
 * But      : Definition des donnees rapport d'observations
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
static int MetReport_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj      *obj,*sub,*subsub;
   TMetElemData *data;
   int           ne,e,v,t,nv,nt,i,j,idx,code;
   float        *valf;
   double        val;

   static CONST char *sopt[] = { "-ELEMENT",NULL };
   enum                opt { ELEMENT };

   data=MetReport_Get(Name);
   if (!data) {
      Tcl_AppendResult(Interp,"\n   MetReport_Define: Report id unknown: \"",Name,"\"",(char*)NULL);
      return(TCL_ERROR);
   }

   for (i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case ELEMENT:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for(e=0;e<data->Ne;e++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(BUFRTable[data->Code[e]].Desc,-1));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               if ((code=MetObs_BURPFindTableCodeOrDesc(Interp,Objv[++i]))==-1) {
                  Tcl_AppendResult(Interp,"\n   MetReport_Define: Wrong element",(char*)NULL);
                  return(TCL_ERROR);
               }

               if (Objc==2) {
                  obj=Tcl_NewListObj(0,NULL);
                  for(e=0;e<data->Ne;e++) {
                     if (data->Code[e]==code) {
                        for(v=0;v<data->Nv;v++) {
                           for(t=0;t<data->Nt;t++) {
                              Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(MetObs_GetData(data,e,v,t)));
                           }
                        }
                     }
                  }
                  Tcl_SetObjResult(Interp,obj);
               } else if (Objc==3) {
                  Tcl_ListObjLength(Interp,Objv[++i],&nv);
                  Tcl_ListObjIndex(Interp,Objv[i],0,&obj);
                  Tcl_ListObjLength(Interp,obj,&nt);

                  if (!data->Nv) {
                     data->Nv=nv;
                  } else if (data->Nv!=nv) {
                     Tcl_AppendResult(Interp,"\n   MetReport_Define: Wrong number of values (NV)",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  if (!data->Nt) {
                     data->Nt=nt;
                  } else if (data->Nt!=nt) {
                     Tcl_AppendResult(Interp,"\n   MetReport_Define: Wrong number of values (NT)",(char*)NULL);
                     return(TCL_ERROR);
                  }

                  j=1;
                  for(ne=0;ne<data->Ne;ne++) {
                     if (data->Code[ne]==code) {
                        j=0;
                        break;
                     }
                  }
                  if (j) {
                     data->Ne++;
                     data->Code=(int*)realloc(data->Code,data->Ne*sizeof(int));
                     data->Code[ne]=code;

                     /*Copy data to new array*/
                     valf=(float*)malloc(data->Ne*data->Nv*data->Nt*sizeof(float));
                     for(e=0;e<ne;e++) {
                        for(v=0;v<data->Nv;v++) {
                           for(t=0;t<data->Nt;t++) {
                              valf[(t*data->Nv+v)*data->Ne+e]=data->Data[(t*data->Nv+v)*ne+e];
                           }
                        }
                     }
                  } else {
                     valf=(float*)malloc(data->Ne*data->Nv*data->Nt*sizeof(float));
                 }

                  for(v=0;v<data->Nv;v++) {
                     Tcl_ListObjIndex(Interp,Objv[i],v,&obj);
                     Tcl_ListObjLength(Interp,obj,&nt);
                     if (data->Nt!=nt) {
                        Tcl_AppendResult(Interp,"\n   MetReport_Define: Wrong number of values (NT)",(char*)NULL);
                        return(TCL_ERROR);
                     }
                     for(t=0;t<data->Nt;t++) {
                        Tcl_ListObjIndex(Interp,obj,t,&sub);
                        if (Tcl_GetDoubleFromObj(Interp,sub,&val)==TCL_OK) {
                           valf[(t*data->Nv+v)*data->Ne+ne]=val;
                        } else {
                           valf[(t*data->Nv+v)*data->Ne+ne]=-999.0;
                        }
                     }
                  }
                  if (data->Data) free(data->Data);
                  data->Data=valf;
               } else {
                  Tcl_AppendResult(Interp,"\n   MetReport_Define: Wrong number of arguments, must be \" -ELEMENT [element] [value]\"",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;
      }
   }

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetReport_Get>
 * Creation     : Juin 2007 J.P. Gauthier
 *
 * But          : Obtenir un objet metreport en fonction de son nom dans la table de Tcl ObsTable
 *
 * Parametres   :
 *   <Name>     : Nom de l'objet observation a obtenir.
 *
 * Retour       : Une structure TObs ou un pointeur NULL si rien trouve.
 *
 * Remarques    :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
TMetElemData* MetReport_Get(char *Name) {
   return((TMetElemData*)Tcl_HashGet(&MetRepTable,Name));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetReport_Put>
 * Creation     : Juin 2007 J.P. Gauthier
 *
 * But          : Creation d'un objet rapport de metobsd et insertion d'un nouveau nom dans la table.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de l'observation a creer
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
Tcl_Obj* MetReport_Put(Tcl_Interp *Interp,char *Name,TMetElemData *Report) {

   char buf[64];

   if (Report) {
      if (!Name) {
         sprintf(buf,"METREPORT_____%li",MetRepNo++);
         Name=buf;
      }
      if (Tcl_HashSet(Interp,&MetRepTable,Name,Report)==TCL_ERROR) {
         return(NULL);
      }
//      Ref->Name=strdup(Name);

      return(Tcl_NewStringObj(Name,-1));
   } else {
      if (Interp)
         Tcl_AppendResult(Interp,"\n   MetReport_Put: Report invalid: \"",Name, "\"",(char *)NULL);
      return(NULL);
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetReport_Destroy>
 * Creation     : Juillet 2005 J.P. Gauthier
 *
 * But          : Destruction d'un georef a partir de son nom.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom du layer
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int MetReport_Destroy(Tcl_Interp *Interp,char *Name) {

   TMetElemData  *ref=NULL;

   if ((ref=(TMetElemData*)Tcl_HashDel(&MetRepTable,Name))) {
      TMetElemData_Free(ref);
   }
   return(TCL_OK);
}
