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
 * Description  : Fichier d'implémentation du module Obs.
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
#ifdef HAVE_ECBUFR

#include "App.h"
#include "tclMetObs.h"
#include "Projection.h"
#include "tclMetObs_Test.h"
#include <math.h>
// #define PHIL_DEBUG
#include "debug.h"
#include "tclMetObs_SQLite.h"

static BUFR_Tables *BUFRTable=NULL;

/* HashTable Tcl pour les observations et rapport*/
static Tcl_HashTable MetObsTable;
static Tcl_HashTable MetRepTable;
static long          MetRepNo=0;
static unsigned int  MetLocNo=0;
static int           MetObsInit=0;

static Tk_Font WMO_Symbol1=NULL;
static Tk_Font WMO_Symbol2=NULL;

static int MetObs_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int MetObs_Create(Tcl_Interp *Interp,char* Name);
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
 *---------------------------------------------------------------------------------------------------------------
 */
int TclMetObs_Init(Tcl_Interp *Interp) {

   if (!MetObsInit++) {
      Tcl_InitHashTable(&MetObsTable,TCL_STRING_KEYS);
      Tcl_InitHashTable(&MetRepTable,TCL_STRING_KEYS);

      /*Load CMC Table B and D, includes local descriptors*/
      BUFRTable=bufr_create_tables();
/*
      if (bufr_load_cmc_tables(BUFRTable)<=0) {
         printf("(WARNING) TclMetObs_Init: Unable to load default CMC BUFR tables\n");
      }
*/
   }
   Tcl_CreateObjCommand(Interp,"metobs",MetObs_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);
   Tcl_CreateObjCommand(Interp,"metreport",MetReport_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   TclMetDataset_Init(Interp);
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
 *---------------------------------------------------------------------------------------------------------------
*/
//static int MetObs_Test(Tcl_Interp *Interp, int Objc, Tcl_Obj *CONST Objv[]);
static int MetObs_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   FUNCBEGIN
   (void) clientData;  // Squelch warning about unused parameter.
   TMetObs    *obs;

   int         idx,c,n;
   static CONST char *sopt[] = { "create","read","write","free","define","stats","is","all","wipe","table", "test",NULL };
   enum               opt { CREATE,READ,WRITE,FREE,DEFINE,STATS,IS,ALL,WIPE,TABLE,TEST };

   Tcl_ResetResult(Interp);

   for(int j = 0; j < Objc; ++j){
      SVAL(Tcl_GetString(Objv[j]));
   }

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",TCL_EXACT,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case TEST:
//         return MetObs_Test(Interp, Objc-2, Objv+2);
         break;
      case CREATE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id [files]");
            return(TCL_ERROR);
         }
         if (MetObs_Create(Interp,Tcl_GetString(Objv[2]))==TCL_ERROR) {
            return(TCL_ERROR);
         }
         obs=MetObs_Get(Tcl_GetString(Objv[2]));
         for(c=3;c<Objc;c++) {
            if (MetObs_Load(Interp,Tcl_GetString(Objv[c]),obs)==TCL_ERROR) {
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
         return(MetObs_Load(Interp,Tcl_GetString(Objv[3]),obs));

         break;

      case WRITE:
         if(Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"file { obs } [format]");
            return(TCL_ERROR);
         }
/*
         if (strcmp(Tcl_GetString(Objv[4]),"BUFR")==0) {
            return(MetObs_WriteBUFR(Interp,Tcl_GetString(Objv[2]),Objv[3]));
         } else if { strcmp(Tcl_GetString(Objv[4]),"OBS")==0 } {
            return(MetObs_WriteBUFRASCII(Interp,Tcl_GetString(Objv[2]),Objv[3],Tcl_GetString(Objv[4])));
         }
*/
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
         TclY_HashAll(Interp,&MetObsTable);
         break;

      case WIPE:
         TclY_HashWipe(&MetObsTable,(TclY_HashFreeEntryDataFunc*)MetObs_Free);
         TclY_HashWipe(&MetRepTable,(TclY_HashFreeEntryDataFunc*)TMetElemData_Free);
         break;

      default:
         fprintf(stderr, "%s(): Default case reached\n",__func__);
         return TCL_ERROR;
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
 *----------------------------------------------------------------------------
*/
static int MetObs_Table(Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   EntryTableB *eb;

   Tcl_Obj   *obj;
   int        i,idx,no=1,res=0;
   long       code;
   char       table='\0';

   static CONST char *sopt[] = { "-readcmc","-readmaster","-readlocal","-code","-desc","-unit","-format","-insert",NULL };
   enum                opt { READCMC,READMASTER,READLOCAL,CODE,DESC,UNIT,FORMAT,INSERT };

   /*Figure out which table we are talking about*/
   if (Objc>1) {
      table=Tcl_GetString(Objv[1])[0];
      if (table!='B' && table!='C' && table!='D') {
         table='B';
         no=0;
      }
   }

   for (i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case READCMC:
            bufr_load_cmc_tables(BUFRTable);
            break;

         case READMASTER:
            if (Objc==1) {
            } else {
               i+=no+1;
               switch(table) {
                  case 'B': res=bufr_load_m_tableB(BUFRTable,Tcl_GetString(Objv[i]));break;
                  case 'D': res=bufr_load_m_tableD(BUFRTable,Tcl_GetString(Objv[i]));break;
               }
               if (res<0) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Table: Unable to load master table ",Tcl_GetString(Objv[i]),(char*)NULL);
                 return(TCL_ERROR);
               }
            }
            break;

         case READLOCAL:
            if (Objc==1) {
            } else {
               i+=no+1;
               switch(table) {
                  case 'B': res=bufr_load_l_tableB(BUFRTable,Tcl_GetString(Objv[i]));break;
                  case 'D': res=bufr_load_l_tableD(BUFRTable,Tcl_GetString(Objv[i]));break;
               }
               if (res<0) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Table: Unable to load local table ",Tcl_GetString(Objv[i]),(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case DESC:
            if(Objc<2 || Objc>4) {
               Tcl_WrongNumArgs(Interp,1,Objv,"code [desc] [units]");
               return(TCL_ERROR);
            }
            Tcl_GetLongFromObj(Interp,Objv[++i],&code);
            if (!(eb=MetObs_BUFRFindTableCode(code))) {
               Tcl_AppendResult(Interp,"\n   MetObs_Table: Could not find element",(char*)NULL);
               return(TCL_ERROR);
            }
            if (Objc>2) {
               if (eb->description) free(eb->description);
               eb->description=strdup(Tcl_GetString(Objv[++i]));
               if (Objc>3)
                  if (eb->unit) free(eb->unit);
                  eb->unit=strdup(Tcl_GetString(Objv[++i]));
            } else {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(eb->description,-1));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(eb->unit,-1));
               Tcl_SetObjResult(Interp,obj);
            }
            break;

         case CODE:
            if(Objc<2 || Objc>4) {
               Tcl_WrongNumArgs(Interp,1,Objv,"desc [code] [unit]");
               return(TCL_ERROR);
            }
            if (!(eb=MetObs_BUFRFindTableDesc(Tcl_GetString(Objv[++i])))) {
               Tcl_AppendResult(Interp,"\n   MetObs_Table: Could not find element",(char*)NULL);
               return(TCL_ERROR);
            }

            if (Objc>2) {
               if (eb->description) free(eb->description);
               eb->description=strdup(Tcl_GetString(Objv[++i]));
               if (Objc>3)
                  if (eb->unit) free(eb->unit);
                  eb->unit=strdup(Tcl_GetString(Objv[++i]));
            } else {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(eb->descriptor));
            }
            break;

         case UNIT:
            if(Objc<2 || Objc>3) {
               Tcl_WrongNumArgs(Interp,1,Objv,"code|desc [unit]");
               return(TCL_ERROR);
            }
            if (!(eb=MetObs_BUFRFindTableCodeOrDesc(Interp,Objv[++i]))) {
               Tcl_AppendResult(Interp,"\n   MetObs_Table: Could not find element",(char*)NULL);
               return(TCL_ERROR);
            }

            if (Objc>2) {
               if (eb->unit) free(eb->unit);
               eb->unit=strdup(Tcl_GetString(Objv[++i]));
            } else {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(eb->unit,-1));
            }
            break;

         case FORMAT:
            if(Objc!=2) {
               Tcl_WrongNumArgs(Interp,1,Objv,"code|desc");
               return(TCL_ERROR);
            }
            if (!(eb=MetObs_BUFRFindTableCodeOrDesc(Interp,Objv[++i]))) {
               Tcl_AppendResult(Interp,"\n   MetObs_Table: Could not find element",(char*)NULL);
               return(TCL_ERROR);
            }

            if (eb->encoding.scale) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj("%g",-1));
            } else {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj("%.0f",-1));
            }
            break;

         case INSERT:
            if(Objc<4) {
               Tcl_WrongNumArgs(Interp,1,Objv,"code desc unit");
               return(TCL_ERROR);
            }
            Tcl_GetLongFromObj(Interp,Objv[++i],&code);
            if ((eb=bufr_tableb_fetch_entry(BUFRTable->master.tableB,code))) {
               Tcl_AppendResult(Interp,"\n   MetObs_Table: Element already exists",(char*)NULL);
               return(TCL_ERROR);
            }

            eb=bufr_new_EntryTableB();
            eb->descriptor=code;
            eb->description=strdup(Tcl_GetString(Objv[++i]));
            eb->unit=strdup(Tcl_GetString(Objv[++i]));
            arr_add(BUFRTable->master.tableB,(char*)&eb);
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
 *----------------------------------------------------------------------------
*/
static int MetObs_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   EntryTableB  *eb;

   Tcl_Obj      *obj,*sub,*subsub,*subsubsub;
   TMetObs      *obs;
   TMetLoc      *loc;
   TMetElem     *elem;
   TMetElemData *data;
   TMetModel    *mdl;
   TDataSpec    *spec;
   long          time=-1;
   int           e,t,i,j,d,v,idx,nv,mk,flag;
   float        *valf;
   double        val;
   char          search;

   static CONST char *sopt[] = { "-INFO","-ADDINFO","-COORD","-ID","-STATION","-TAG","-NO","-ELEMENT","-REPORT","-NB","-DATE","-DATE0","-DATE1","-LAG","-VALID","-LEVELS","-FLAG","-CODETYPE","-FAMILY","-TYPE","-STYPE","-MARKER","-MARKEROP","-NVAL","-MODEL","-PERSISTANCE","-CACHE","-PIXEL",NULL };
   enum                opt { INFO,ADDINFO,COORD,ID,STATION,TAG,NO,ELEMENT,REPORT,NB,DATE,DATE0,DATE1,LAG,VALID,LEVELS,FLAG,CODETYPE,FAMILY,TYPE,STYPE,MARKER,MARKEROP,NVAL,MODEL,PERSISTANCE,CACHE,PIXEL };

   obs=MetObs_Get(Name);
   if (!obs) {
      Tcl_AppendResult(Interp,"\n   MetObs_Define: Observation id unknown: \"",Name,"\"",(char*)NULL);
      return(TCL_ERROR);
   }

   for (i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
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
               search=Tcl_GetString(Objv[++i])[0]=='|'?MET_TYPETG:MET_TYPEID;
               loc=TMetLoc_Find(obs,NULL,Tcl_GetString(Objv[i]),search);
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
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(loc->Coord.Lat));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(loc->Coord.Lon));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(loc->Coord.Elev));
                  Tcl_ListObjAppendElement(Interp,obj,sub);
                  loc=loc->Next;
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               search=Tcl_GetString(Objv[++i])[0]=='|'?MET_TYPETG:MET_TYPEID;
               loc=TMetLoc_Find(obs,NULL,Tcl_GetString(Objv[i]),search);
               if (!loc) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Invalid observation id",(char*)NULL);
                  return(TCL_ERROR);
               }
               if (Objc==2) {
                  obj=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc->Coord.Lat));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc->Coord.Lon));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(loc->Coord.Elev));
                  Tcl_SetObjResult(Interp,obj);
              } else if (Objc==4) {
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc->Coord.Lat);
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc->Coord.Lon);
                  loc->Coord.Elev=0.0;
               } else if (Objc==5) {
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc->Coord.Lat);
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc->Coord.Lon);
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc->Coord.Elev);
               } else {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -COORD [id] [lat - lon] [elev]\"",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case STATION:
         case ID:
            obj=Tcl_NewListObj(0,NULL);

            if (Objc==1) {
               // List all stations
               loc=obs->Loc;
               while(loc) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(loc->Id,-1));
                  loc=loc->Next;
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               // Add station
               search=Tcl_GetString(Objv[++i])[0]=='|'?MET_TYPETG:MET_TYPEID;
               loc=TMetLoc_Find(obs,NULL,Tcl_GetString(Objv[i]),search);

               if (Objc==2) {
                  if (loc) {
                     if (search==MET_TYPETG) {
                        Tcl_SetObjResult(Interp,Tcl_NewStringObj(loc->Id,-1));
                        return(TCL_OK);
                     } else {
                        Tcl_AppendResult(Interp,"\n   MetObs_Define: Observation id already exist",(char*)NULL);
                        return(TCL_ERROR);
                     }
                  }
                  TMetLoc_New(obs,Tcl_GetString(Objv[i]),NULL,0.0,0.0,0.0);

               } else if (Objc==3) {
                  // Find stations for specified date
                  Tcl_GetLongFromObj(Interp,Objv[++i],&time);
                  sub=Tcl_NewObj();
                  if (!strlen(Tcl_GetString(Objv[1]))) {
                     loc=obs->Loc;
                     while(loc) {
                       if (!obs->CodeType || obs->CodeType==loc->CodeType) {
                          if ((elem=TMetElem_Find(loc,time,obs->Lag))) {
                              for(d=0;d<elem->NData;d++) {
                                 data=elem->EData[d];
                                 // Check for selected family
                                 flag=data->Family&0x38;
                                 if (MET_FLAG(obs,flag)) {
                                    // Check for data bktyp matching
                                    if (obs->Type==-1 || (data->Type>>6&0x1)==obs->Type) {
                                    // Add station if not already there
                                       Tcl_SetStringObj(sub,loc->Id,-1);
                                       if (TclY_ListObjFind(Interp,obj,sub)==-1) {
                                          Tcl_ListObjAppendElement(Interp,obj,Tcl_DuplicateObj(sub));
                                       }
                                    }
                                 }
                              }
                           }
                        }
                        loc=loc->Next;
                     }
                  } else {
                     Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -ID [id] [date]\"",(char*)NULL);
                     return(TCL_ERROR);
                  }
               }
               Tcl_SetObjResult(Interp,obj);
            }
            break;

         case TAG:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               loc=obs->Loc;
               while(loc) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(loc->Tag,-1));
                  loc=loc->Next;
               }
               Tcl_SetObjResult(Interp,obj);
           } else {
               Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -TAG \"",(char*)NULL);
               return(TCL_ERROR);
            }
            break;

         case PIXEL:
            if (Objc!=2 && Objc!=4) {
               Tcl_WrongNumArgs(Interp,4,Objv,"id [x y]");
               return(TCL_ERROR);
            }
            search=Tcl_GetString(Objv[++i])[0]=='|'?MET_TYPETG:MET_TYPEID;
            loc=TMetLoc_Find(obs,NULL,Tcl_GetString(Objv[i]),search);
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
               search=Tcl_GetString(Objv[++i])[0]=='|'?MET_TYPETG:MET_TYPEID;
               loc=TMetLoc_Find(obs,NULL,Tcl_GetString(Objv[i]),search);
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
               search=Tcl_GetString(Objv[++i])[0]=='|'?MET_TYPETG:MET_TYPEID;
               loc=TMetLoc_Find(obs,NULL,Tcl_GetString(Objv[i]),search);
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
                           Tcl_SetIntObj(sub,elem->EData[d]->Code[e]->descriptor);
                           if (TclY_ListObjFind(Interp,obj,sub)==-1) {
                              Tcl_ListObjAppendElement(Interp,obj,Tcl_DuplicateObj(sub));
                           }
                        }
                     }
                     elem=elem->Next;
                  }
                  Tcl_SetObjResult(Interp,obj);
               } else {
                  if (!(eb=MetObs_BUFRFindTableCodeOrDesc(Interp,Objv[2]))) {
                     Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong element",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  ++i;

                  spec=NULL;
                  if (obs->Model) {
                     for(d=0;d<obs->Model->NItem;d++) {
                        if (obs->Model->Items[d].Code[0]==eb->descriptor) {
                           spec=obs->Model->Items[d].Spec;
                           break;
                        }
                     }
                  }
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
                           /*Check for selected family*/
                           flag=data->Family&0x38;
                           if (MET_FLAG(obs,flag)) {
                              /*Check for data bktyp matching*/
                              if (obs->Type==-1 || (data->Type>>6&0x1)==obs->Type) {
                                 for(e=0;e<data->Ne;e++) {
                                    if (data->Code[e]->descriptor==eb->descriptor) {
                                       for(v=(obs->NVal<=-1?0:obs->NVal);v<(obs->NVal<=-1?data->Nv:((obs->NVal+1)>data->Nv?data->Nv:(obs->NVal+1)));v++) {
                                          subsubsub=Tcl_NewListObj(0,NULL);
                                          for(t=0;t<data->Nt;t++) {
                                             /*Check for selected marker*/
                                             mk=MetObs_GetMarker(data,e,v,t);
                                             if (!obs->Marker || (obs->MarkerOp=='O' && (mk&obs->Marker)) || (obs->MarkerOp=='A' && (mk==obs->Marker))) {
                                                Tcl_ListObjAppendElement(Interp,subsubsub,Tcl_NewDoubleObj(VAL2SPEC(spec,MetObs_GetData(data,e,v,t))));
                                             }
                                          }
                                          Tcl_ListObjAppendElement(Interp,subsub,subsubsub);
                                       }
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
                     if ((elem=TMetElem_Find(loc,time,obs->Lag))) {
                        for(d=0;d<elem->NData;d++) {
                           data=elem->EData[d];
                           /*Check for selected state*/
                           flag=data->Family&0x38;
                           if (MET_FLAG(obs,flag)) {
                              if (obs->Type==-1 || (data->Type>>6&0x1)==obs->Type) {
                                 for(e=0;e<data->Ne;e++) {
                                    if (data->Code[e]->descriptor==eb->descriptor) {
                                       for(v=(obs->NVal<=-1?0:obs->NVal);v<(obs->NVal<=-1?data->Nv:((obs->NVal+1)>data->Nv?data->Nv:(obs->NVal+1)));v++) {
                                          subsub=Tcl_NewListObj(0,NULL);
                                          for(t=0;t<data->Nt;t++) {
                                             /*Check for selected marker*/
                                             mk=MetObs_GetMarker(data,e,v,t);
                                             if (!obs->Marker || (obs->MarkerOp=='O' && (mk&obs->Marker)) || (obs->MarkerOp=='A' && (mk==obs->Marker))) {
                                                Tcl_ListObjAppendElement(Interp,subsub,Tcl_NewDoubleObj(VAL2SPEC(spec,MetObs_GetData(data,e,v,t))));
                                             }
                                          }
                                          Tcl_ListObjAppendElement(Interp,obj,subsub);
                                       }
                                    }
                                 }
                              }
                           }
                        }
                     }
                     ++i;
                     Tcl_SetObjResult(Interp,obj);
                  } else if (Objc==5) {
                     // Add new element

                     Tcl_GetLongFromObj(Interp,Objv[++i],&time);
                     Tcl_ListObjLength(Interp,Objv[++i],&nv);
                     if (nv==0) {
                        nv=1;
                        valf=(float*)malloc(nv*sizeof(float));
                        valf[0]=-999.0;
                     } else {
                        valf=(float*)malloc(nv*sizeof(float));
                        for(v=0;v<nv;v++) {
                           Tcl_ListObjIndex(Interp,Objv[i],v,&obj);
                           if (Tcl_GetDoubleFromObj(Interp,obj,&val)==TCL_ERROR) {
                              Tcl_AppendResult(Interp,"\n   MetObs_Define: Invalid value for element: ",Tcl_GetString(obj),(char*)NULL);
                              free(valf);
                              return(TCL_ERROR);
                           }
                           valf[v]=val;
                        }
                     }

                     if (!(data=TMetElem_Merge(loc,0,time,0x0,0x0,0x0,1,nv,1,valf,NULL,&eb))) {
                        Tcl_AppendResult(Interp,"\n   MetObs_Define: Unable to add element",(char*)NULL);
                        free(valf);
                        return(TCL_ERROR);
                     }
                     obj=Tcl_NewIntObj(eb->descriptor);
                     if (TclY_ListObjFind(Interp,obs->Elems,obj)==-1) {
                        Tcl_ListObjAppendElement(Interp,obs->Elems,obj);
                     }
                     free(valf);

                     // Adjust time limits
                     obs->Time0=(obs->Time0<time && obs->Time0!=0)?obs->Time0:time;
                     obs->Time1=obs->Time1>time?obs->Time1:time;
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
               search=Tcl_GetString(Objv[++i])[0]=='|'?MET_TYPETG:MET_TYPEID;
               if (Objc==2 || Objc==3) {
                  if (Objc==3) {
                     Tcl_GetLongFromObj(Interp,Objv[++i],&time);
                  }
                  obj=Tcl_NewListObj(0,NULL);

                  if (!strlen(Tcl_GetString(Objv[1])) && Objc==3) {
                     // No station specified, look for all report for all stations for this date
                     loc=obs->Loc;
                     while(loc) {
                        if (!obs->CodeType || obs->CodeType==loc->CodeType) {
                           if ((elem=TMetElem_Find(loc,time,obs->Lag))) {
                              for(d=0;d<elem->NData;d++) {
                                 data=elem->EData[d];
                                 /*Check for selected family*/
                                 flag=data->Family&0x38;
                                 if (MET_FLAG(obs,flag)) {
                                    /*Check for data bktyp matching*/
                                    if (obs->Type==-1 || (data->Type>>6&0x1)==obs->Type) {
                                       /*Link the ElementData to its Obs and location*/
                                       elem->EData[d]->Obs=obs;
                                       elem->EData[d]->Loc=loc;
                                       Tcl_ListObjAppendElement(Interp,obj,MetReport_Put(Interp,NULL,elem->EData[d]));
                                    }
                                 }
                              }
                           }
                        }
                        loc=loc->Next;
                     }
                  } else {
                     // Station specificed, look for all report for this stations (and date if specified
                     loc=NULL;
                     while ((loc=TMetLoc_Find(obs,loc,Tcl_GetString(Objv[1]),search))) {
                        if ((elem=TMetElem_Find(loc,time,obs->Lag))) {
                           for(d=0;d<elem->NData;d++) {
                              data=elem->EData[d];
                              /*Check for selected family*/
                              flag=data->Family&0x38;
                              if (MET_FLAG(obs,flag)) {
                                 /*Check for data bktyp matching*/
                                 if (obs->Type==-1 || (data->Type>>6&0x1)==obs->Type) {
                                    /*Link the ElementData to its Obs and location*/
                                    elem->EData[d]->Obs=obs;
                                    elem->EData[d]->Loc=loc;
                                    Tcl_ListObjAppendElement(Interp,obj,MetReport_Put(Interp,NULL,elem->EData[d]));
                                 }
                              }
                           }
                        }
                     }
                  }
                  Tcl_SetObjResult(Interp,obj);
               } else if (Objc==4) {
                  loc=TMetLoc_Find(obs,NULL,Tcl_GetString(Objv[1]),search);
                  if (!loc) {
                     Tcl_AppendResult(Interp,"\n   MetObs_Define: Invalid observation id",(char*)NULL);
                     return(TCL_ERROR);
                  }
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
            if (Objc!=1 && Objc!=2) {
               Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -DATE [id]\"",(char*)NULL);
               return(TCL_ERROR);
            }
            obj=Tcl_NewListObj(0,NULL);
            sub=Tcl_NewObj();
            if (Objc==1) {
               loc=obs->Loc;
               while(loc) {
                  elem=loc->Elems;
                  while(elem) {
                     Tcl_SetLongObj(sub,elem->Time);
                     if (TclY_ListObjFind(Interp,obj,sub)==-1) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_DuplicateObj(sub));
                     }
                     elem=elem->Next;
                  }
                  loc=loc->Next;
               }
            } else {
               loc=NULL;
               search=Tcl_GetString(Objv[++i])[0]=='|'?MET_TYPETG:MET_TYPEID;
               while ((loc=TMetLoc_Find(obs,loc,Tcl_GetString(Objv[1]),search))) {
                  elem=loc->Elems;
                  while(elem) {
                     Tcl_SetLongObj(sub,elem->Time);
                     if (TclY_ListObjFind(Interp,obj,sub)==-1) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_DuplicateObj(sub));
                     }
                     elem=elem->Next;
                  }
               }
            }
            /*TODO Sort the list*/
            Tcl_SetObjResult(Interp,obj);
            break;

         case VALID:
            if (Objc==1) {
                Tcl_SetObjResult(Interp,Tcl_NewLongObj(obs->Time));
            } else {
               if (Objc<3) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -VALID [time] [lag]\"",(char*)NULL);
                  return(TCL_ERROR);
               } else {
                  Tcl_GetLongFromObj(Interp,Objv[++i],&obs->Time);
                  Tcl_GetLongFromObj(Interp,Objv[++i],&obs->Lag);
               }
             }
            break;

         case LAG:
            if (Objc==1) {
                Tcl_SetObjResult(Interp,Tcl_NewLongObj(obs->Lag));
            } else {
               if (Objc<2) {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -LAG [lag]\"",(char*)NULL);
                  return(TCL_ERROR);
               } else {
                  Tcl_GetLongFromObj(Interp,Objv[++i],&obs->Lag);
               }
             }
            break;

         case CODETYPE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(obs->CodeType));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&e);
               obs->CodeType=e;
            }
            break;

         case FLAG:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(obs->Flag));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&e);
               obs->Flag=e;
            }
            break;
            
         case FAMILY:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(obs->Family));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&obs->Family);
            }
            break;


         case TYPE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(obs->Type));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&obs->Type);
            }
            break;

         case STYPE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(obs->SType));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&obs->SType);
            }
            break;

         case NVAL:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(obs->NVal));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&obs->NVal);
            }
            break;

         case MARKER:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(obs->Marker));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&obs->Marker);
            }
            break;

         case MARKEROP:
            if (Objc==1) {
               if (obs->MarkerOp=='A') {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj("AND",-1));
               } else {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj("OR",-1));
               }
            } else {
               ++i;
               if (Tcl_GetString(Objv[i])[0]!='A' && Tcl_GetString(Objv[i])[0]!='O') {
                  Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong operator, must be \"AND or OR\"",(char*)NULL);
                  return(TCL_ERROR);
               }
               obs->MarkerOp=Tcl_GetString(Objv[i])[0];
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
            
         case LEVELS:
            if (Objc==1) {
                Tcl_SetObjResult(Interp,obs->Levels);
            } else {
               obs->Levels=Tcl_DuplicateObj(Objv[++i]);
               Tcl_IncrRefCount(obs->Levels);
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
 *---------------------------------------------------------------------------------------------------------------
*/
int MetObs_Create(Tcl_Interp *Interp,char *Name) {

   TMetObs *obs;

   if (!(obs=(TMetObs*)TclY_HashPut(Interp,&MetObsTable,Name,sizeof(TMetObs)))) {
      return(TCL_ERROR);
   }

   obs->Format   = MET_OTHER;
   obs->Tag      = NULL;
   obs->Levels   = NULL;
   obs->FId      = -1;
   obs->Elems    = Tcl_NewListObj(0,NULL);
   obs->Time0    = 0;
   obs->Time1    = 0;
   obs->Loc      = NULL;
   obs->Time     = -1;
   obs->Cache    = 0;
   obs->Persistance = 0;
   obs->Lag      = 0;
   obs->NVal     = -1;
   obs->Type     = -1;
   obs->SType    = -1;
   obs->Family   = -1;
   obs->Marker   = 0x0;
   obs->MarkerOp = 'O';
   obs->CodeType = 0x0;
   obs->Flag     = 0x0;
   obs->Info     = NULL;
   obs->NbInfo   = 0;
   obs->NoData   = -999.0f;

   MetModel_Create(Interp,Name);
   obs->Model = MetModel_Get(Name);

   Tcl_InitHashTable(&obs->LocationCoordIndex, TCL_STRING_KEYS);

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
 *---------------------------------------------------------------------------------------------------------------
*/
static int MetObs_FreeHash(Tcl_Interp *Interp,char *Name) {

   TMetObs *obs=NULL;

   if ((obs=(TMetObs*)TclY_HashDel(&MetObsTable,Name))) {
      if (obs->Model)
            MetModel_FreeHash(Interp,obs->Model->Name);
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

   if (Obs->Tag)    Tcl_DecrRefCount(Obs->Tag);
   if (Obs->Levels) Tcl_DecrRefCount(Obs->Levels);
   if (Obs->Elems)  Tcl_DecrRefCount(Obs->Elems);

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
 *---------------------------------------------------------------------------------------------------------------
*/
TMetObs* MetObs_Get(char *Name) {
   return((TMetObs*)TclY_HashGet(&MetObsTable,Name));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_GetTag>
 * Creation     : Avril 2008 J.P. Gauthier
 *
 * But          : Obtenir un le tag unique d'une localisation d'observation
 *
 * Parametres   :
 *   <Obs>      : Observation
 *   <Idx>       : Index .
 *
 * Retour       : Pointeur sur le tag.
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
char* MetObs_GetTag(TMetObs *Obs,int Idx) {

   TMetLoc *loc;

   loc=Obs->Loc;
   while(Idx--) {
      if (!loc)
         return(NULL);

      loc=loc->Next;
   }
   return(loc->Tag);
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
 *   <Desc>     : Element
 *   <NObs>     : Nombre d'obs reprojectee.
 *   <Extrap>   : Reprojeter en dehors du domaine
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
Vect3d *MetObs_Grid(Tcl_Interp *Interp,TGeoRef *Ref,TMetObs *Obs,long Time,Tcl_Obj *Desc,int *NObs,int Extrap) {

   EntryTableB  *eb=NULL;

   int           n,j=0,k,skip;
   TMetLoc      *loc;
   TMetElem     *elem;
   TMetElemData *data;
   Vect3d       *pos=NULL;

   *NObs=0;

  if ((eb=MetObs_BUFRFindTableCodeOrDesc(Interp,Desc))) {
      loc=Obs->Loc;
      while(loc) { j++; loc=loc->Next; }

      if ((pos=(Vect3d*)malloc(j*sizeof(Vect3d)))) {
         loc=Obs->Loc;
         while(loc) {

            j=Ref->UnProject(Ref,&pos[*NObs][0],&pos[*NObs][1],loc->Coord.Lat,loc->Coord.Lon,Extrap,1);

            skip=0;
            for(k=0;k<*NObs;k++) {
               if (pos[*NObs][0]==pos[k][0] && pos[*NObs][1]==pos[k][1]) {
                  skip=1;
                  break;
               }
            }

            if (!skip && (Extrap || j)) {
               /*Get the element for the specific time*/
               if ((elem=TMetElem_Find(loc,Time,Obs->Lag))) {

                  /*Get the specific data*/
                  for(n=0;n<elem->NData;n++) {
                     data=elem->EData[n];
                     pos[*NObs][2]=TMetElem_Value(data,eb->descriptor,0,0,0);
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

BUFR_Tables *MetObs_GetTables(void) {
   return(BUFRTable);
}

EntryTableB *MetObs_BUFRFindTableCodeOrDesc(Tcl_Interp *Interp,Tcl_Obj *Code) {
   int code;

   EntryTableB *eb=NULL;

   if (BUFRTable) {
      if (BUFRTable->master.tableB) {
         if (TclY_Get0IntFromObj(Interp,Code,&code)==TCL_OK) {
            eb=bufr_tableb_fetch_entry(BUFRTable->master.tableB,code);
         } else {
            eb=bufr_tableb_fetch_entry_desc(BUFRTable->master.tableB,Tcl_GetString(Code));
         }
      }
      if (!eb && BUFRTable->local.tableB) {
         if (TclY_Get0IntFromObj(Interp,Code,&code)==TCL_OK) {
            eb=bufr_tableb_fetch_entry(BUFRTable->local.tableB,code);
         } else {
            eb=bufr_tableb_fetch_entry_desc(BUFRTable->local.tableB,Tcl_GetString(Code));
         }
      }
   }

   Tcl_ResetResult(Interp);
   return(eb);
}

EntryTableB *MetObs_BUFRFindTableCode(unsigned int Code) {

   EntryTableB *eb=NULL;

   if (BUFRTable) {
      if (BUFRTable->master.tableB)
         eb=bufr_tableb_fetch_entry(BUFRTable->master.tableB,Code);

      if (!eb && BUFRTable->local.tableB)
         eb=bufr_tableb_fetch_entry(BUFRTable->local.tableB,Code);
   }

   return(eb);
}

EntryTableB *MetObs_BUFRFindTableDesc(char *Desc) {

   EntryTableB *eb=NULL;

   if (BUFRTable) {
      if (BUFRTable->master.tableB)
         eb=bufr_tableb_fetch_entry_desc(BUFRTable->master.tableB,Desc);

      if (!eb && BUFRTable->local.tableB)
         eb=bufr_tableb_fetch_entry_desc(BUFRTable->local.tableB,Desc);
   }

   return(eb);
}

TMetLoc *TMetLoc_Find(TMetObs *Obs,TMetLoc *From,char *Id,int Type) {
   TMetLoc *loc;

   loc=From?From->Next:Obs->Loc;

   while(loc) {
      if (Type==MET_TYPEID) {
         if (strcmp(loc->Id,Id)==0) break;
      } else if (Type==MET_TYPENO) {
         if (strcmp(loc->No,Id)==0) break;
      } else {
         if (strcmp(loc->Tag,Id)==0) break;
      }
      loc=loc->Next;
   }
   return(loc);
}

static inline int hash_loc_coord(double Lat, double Lon, double Elev, char *Key) {
   snprintf(Key, 64, "%.6f,%.6f,%.6f",Lat,Lon,Elev);
   return(TCL_OK);
}

TMetLoc *TMetLoc_FindWithCoordIndex(TMetObs *Obs,TMetLoc *From,char *Id,double Lat,double Lon,double Elev,int Type,char *Multi) {
   (void) From;
   (void) Id;
   (void) Type;
   (void) Multi;

   TMetLoc *loc = NULL;

   char loc_key[64];
   hash_loc_coord(Lat, Lon, Elev, loc_key);

   Tcl_HashEntry *entryPtr;
   if((entryPtr = Tcl_FindHashEntry(&Obs->LocationCoordIndex, loc_key)) != NULL)
      loc = (TMetLoc *) Tcl_GetHashValue(entryPtr);

   /*
    * TODO See with JP about what else to check for to emulate behavior of
    * TMetLoc_FindWithCoord, maybe put the Id in the hash string.
    */

   return loc;
}


TMetLoc *TMetLoc_FindWithCoord(TMetObs *Obs,TMetLoc *From,char *Id,double Lat,double Lon,double Elev,int Type,char *Multi) {

   TMetLoc *start=From?From->Next:Obs->Loc;
   for(TMetLoc *loc = start; loc; loc = loc->Next){
      char *cmp;
      switch(Type){
         case MET_TYPENO: cmp = loc->No; break;
         case MET_TYPEID: cmp = loc->Id; break;
         default: cmp = loc->Tag; break;
      }

      if(strcmp(cmp, Id) != 0)
         continue;

      /*
       * Id matches, now check coordinates
       */
      if (     (Lat==-999.0  || loc->Coord.Lat==Lat)
            && (Lon==-999.0  || loc->Coord.Lon==Lon)
            && (Elev==-999.0 || loc->Coord.Elev==Elev)) {
         *Multi=0; // We found a loc with the same Id and same Coordinates
         return loc;
      } else {
         *Multi=1; // There is a loc with the same Id but different coordinates
      }
   }
   return NULL;
}

TMetLoc *TMetLoc_New(TMetObs *Obs,char *Id,char *No,double Lat,double Lon,double Elev) {

   TMetLoc *loc;

   loc=(TMetLoc*)malloc(sizeof(TMetLoc));
   loc->Id=Id?strdup(Id):NULL;
   loc->No=No?strdup(No):NULL;
   loc->Info=NULL;
   loc->Coord.Lat=Lat;
   loc->Coord.Lon=Lon;
   loc->Coord.Elev=Elev;
   loc->Pix[0]=0.0;
   loc->Pix[1]=0.0;
   loc->Grid[0]=loc->Grid[1]=loc->Grid[2]=0;
   loc->Level=0;
   loc->CodeType=0;
   loc->Flag=0x0;
   loc->Elems=NULL;

   sprintf(loc->Tag,"|%u",MetLocNo++);

   if (Obs) {
      loc->Next=Obs->Loc;
      Obs->Loc=loc;
   } else {
      loc->Next=NULL;
   }

   char loc_key[64];
   hash_loc_coord(Lat, Lon, Elev, loc_key);
   int new;
   Tcl_HashEntry *entryPtr = Tcl_CreateHashEntry(&Obs->LocationCoordIndex, loc_key, &new);
   Tcl_SetHashValue(entryPtr, loc);

   return(loc);
}

int TMetElem_Index(const TMetElemData* restrict const Data,const int Code,const int Ne) {

   int   e,ne;

   ne=Ne;
   for(e=0;e<Data->Ne;e++) {
      if (Code==Data->Code[e]->descriptor) {
         if ((ne--)==0) {
            return(e);
         }
      }
   }
   return(-1);
}

float TMetElem_Value(const TMetElemData* restrict const Data,const int Code,int Ne,const int Nv,const int Nt) {

   int e,ne;

   ne=Ne;
   for(e=0;e<Data->Ne;e++) {
      if (Code==Data->Code[e]->descriptor) {
         if ((ne--)==0) {
            return(MetObs_GetData(Data,e,Nv,Nt));
         }
      }
   }
   return(-999.0f);
}

TMetElem *TMetElem_Find(const TMetLoc* restrict const Loc,const long Time,const long Lag) {

   TMetElem *elem=Loc->Elems;

   while(elem && (Time>-1 && Time<elem->Time)) {
      elem=elem->Next;
   }

   if (elem && Lag && (Time>-1 && (Time-elem->Time)>Lag)) {
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
   Elem->EData=NULL;
}

void TMetElemData_Free(TMetElemData *Data) {

   if (Data) {
      if (Data->Code)   free(Data->Code);   Data->Code=NULL;
      if (Data->Data)   free(Data->Data);   Data->Data=NULL;
      if (Data->Marker) free(Data->Marker); Data->Marker=NULL;
   }
}

int TMetElemData_Same(const TMetElemData* restrict Data0,const TMetElemData* restrict Data1) {

   if (Data0->Ne!=Data1->Ne || Data0->Nv!=Data1->Nv || Data0->Nt!=Data1->Nt || Data0->Family!=Data1->Family) {
      return(0);
   }

   if (Data0->Code && Data1->Code && memcmp(Data0->Code,Data1->Code,Data0->Ne*sizeof(int))!=0) {
      return(0);
   }

   if (Data0->Data && Data1->Data && memcmp(Data0->Data,Data1->Data,Data0->Ne*Data0->Nv*Data0->Nt*sizeof(float))!=0) {
      return(0);
   }

   if (Data0->Marker && Data1->Marker && memcmp(Data0->Marker,Data1->Marker,Data0->Ne*Data0->Nv*Data0->Nt*sizeof(int))!=0) {
      return(0);
   }
   return(1);
}

TMetElemData *TMetElem_Add(TMetLoc *Loc,TMetElemData *Data,time_t Time) {

   TMetElem    *new,*pre,*elem=Loc->Elems;
   int          n;

   // Look for a spot in the ordered list
   pre=NULL;
   while(elem && Time<elem->Time) {
      pre=elem;
      elem=elem->Next;
   }

   // If we already have this time
   if (elem && Time==elem->Time) {
      new=elem;
      for(n=elem->NData-1;n>=0;n--) {
         if (TMetElemData_Same(Data,elem->EData[n])) {
            // Check for markers/data
            if (elem->EData[n]->Data) {
               if (!elem->EData[n]->Marker && Data->Marker) {
                  elem->EData[n]->Marker=Data->Marker;
                  Data->Marker=NULL;
               }
            }
            if (elem->EData[n]->Marker) {
               if (!elem->EData[n]->Data && Data->Data) {
                  elem->EData[n]->Data=Data->Data;
                  Data->Data=NULL;
               }
            }
            return(NULL);
         }
      }
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

   // Create a new data bloc
   new->EData=(TMetElemData**)realloc(new->EData,new->NData*sizeof(TMetElemData*));
   new->EData[new->NData-1]=Data;

   return(new->EData[new->NData-1]);
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

TMetElemData *TMetElem_Merge(TMetLoc *Loc,time_t Min,time_t Time,int Fam,int Type,int SType,int Ne,int Nv,int Nt,float *Data,int *Marker,EntryTableB **Codes) {

   TMetElem     *elem;
   TMetElemData *data=NULL;
   int           nb=0,d,e,vt;

   // Check if an element exist at this time
   if (!(elem=TMetElem_Find(Loc,Time,0)) || elem->Time!=Time) {
       data=TMetElem_Insert(Loc,Min,Time,Fam,Type,SType,Ne,Nv,Nt,Data,Marker,Codes);    
    } else {
      // If so,append to the first element data
      for(d=0;d<elem->NData;d++) {
         // If the dimemsion of the packet is the same
         if (elem->EData[d]->Nv==Nv && elem->EData[d]->Nt==Nt) {
            data=elem->EData[d];
            break;
         }
      }
      
      if (!data) {
         // Create a new data bloc
         data=TMetElem_Insert(Loc,Min,Time,Fam,Type,SType,Ne,Nv,Nt,Data,Marker,Codes);
         return(data);
      }
      
       // Expand arrays for new items
      if (!Marker || !(data->Ne*data->Nv*data->Nt)) {
         nb=(data->Ne+Ne)*data->Nv*data->Nt;      
         if (!(data->Code=(EntryTableB**)realloc(data->Code,(data->Ne+Ne)*sizeof(EntryTableB*)))) {
            return(NULL);
         }
         memcpy(&data->Code[data->Ne],Codes,Ne*sizeof(EntryTableB*));
      }
      
      // Add the new values
      if (Data) {
         float *dptr,*dold,*dnew,*new;
         
         if (!(new=(float*)malloc(nb*sizeof(float)))) {
            return(NULL);            
         }
         dold=data->Data;
         dnew=Data;
         dptr=new;
     
         for(vt=0;vt<data->Nv*data->Nt;vt++) {
            if (dold) {
               for(e=0;e<data->Ne;e++,dptr++,dold++) {
                  *dptr=*dold;
               }
            }
            for(e=0;e<Ne;e++,dptr++,dnew++) {
               *dptr=*dnew;
            }
         }
         free(data->Data);
         data->Data=new;
         data->Ne+=Ne;
      }
      
      if (Marker) {
         int *dptr,*dold,*dnew,*new;
         
         if (!(new=(int*)malloc(data->Ne*data->Nv*data->Nt*sizeof(int)))) {
            return(NULL);            
         }
         dold=data->Marker;
         dnew=Marker;
         dptr=new;

         for(vt=0;vt<data->Nv*data->Nt;vt++) {
            if (dold) {
               for(e=0;e<data->Ne-Ne;e++,dptr++,dold++) {
                  *dptr=*dold;
               }
            }
            for(e=0;e<Ne;e++,dptr++,dnew++) {
               *dptr=*dnew;
            }
         }
         free(data->Marker);
         data->Marker=new;
      }
      
      data->Family=Fam;
      data->Type=Type;
      data->SType=SType;
      data->Time=Time;
   }

   return(data);
}


/*
 * Is the parameter time_t Min like the minimum timestep or something?
 * It is called dt in MetObs_LoadBURP() so maybe it's some kind of precision or
 * something.
 */
TMetElemData *TMetElem_Insert(TMetLoc *Loc,
      time_t Min, time_t Time,
      int Fam, int Type, int SType,
      int Ne, int Nv, int Nt,
      float *Data, int *Marker,
      EntryTableB **Codes
){

   TMetElemData *ptr,*data=NULL;

   data = (TMetElemData*) malloc(sizeof(*data));

   data->Ne=Ne;
   data->Nv=Nv;
   data->Nt=Nt;
   data->Family=Fam;
   data->Type=Type;
   data->SType=SType;
   data->Time=Time;

   if (Data) {
      const size_t nb_data = Ne * Nv * Nt;
      const size_t nb_bytes = nb_data * sizeof(float);
      data->Data=(float*) malloc(nb_bytes);
      memcpy(data->Data, Data, nb_bytes);
   } else {
      data->Data=NULL;
   }

   if (Marker) {
      const size_t nb_marker = Ne * Nv * Nt;
      const size_t nb_bytes = nb_marker * sizeof(int);
      data->Marker=(int*) malloc(nb_bytes);
      memcpy(data->Marker, Marker, nb_bytes);
   } else {
      data->Marker=NULL;
   }

   if (Codes) {
      const size_t nb_codes = data->Ne;
      const size_t nb_bytes = nb_codes * sizeof(EntryTableB*);
      data->Code = (EntryTableB**) malloc(nb_bytes);
      memcpy(data->Code,Codes,nb_bytes);
   } else {
      data->Code=NULL;
   }

   if (!(ptr = TMetElem_Add(Loc,data,Time))) {
      TMetElemData_Free(data);
      free(data);
   } else {
      TMetElem_Clean(Loc,Min);
   }
   return(ptr);
}

TMetElemData *TMetElem_InsertCopy(TMetLoc *Loc,time_t Min,time_t Time,TMetElemData *Data) {

   if (TMetElem_Add(Loc,Data,Time)) {
      TMetElem_Clean(Loc,Min);
   }
   return(Data);
}

TMetElemData *TMetElemData_New(int Ne,int Nv,int Nt) {

   TMetElemData *data;

   data=(TMetElemData*)malloc(sizeof(TMetElemData));
   data->Obs=NULL;
   data->Loc=NULL;
   data->Ne=Ne;
   data->Nv=Nv;
   data->Nt=Nt;
   data->Family=0x0;
   data->Type=0x0;
   data->SType=0x0;
   data->Time=0;
   data->Data=(float*)malloc(Ne*Nv*Nt*sizeof(float));
   data->Code=(EntryTableB**)malloc(Ne*Nv*Nt*sizeof(EntryTableB*));
   data->Marker=NULL;

   return(data);
}

TMetElemData *TMetElemData_Resize(TMetElemData *Data,int Ne,int Nv,int Nt) {

   Data->Data=(float*)realloc(Data->Data,Ne*Nv*Nt*sizeof(float));
   Data->Code=(EntryTableB**)realloc(Data->Code,Ne*Nv*Nt*sizeof(EntryTableB*));

   return(Data);
}
/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_Load>
 * Creation     : Mars 2008 J.P. Gauthier
 *
 * But          : Chargement d'un fichier d'observations.
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <File>     : Le nom du fichier
 *   <Obs>      : Observation
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *   - Cette fonction essaie tout les formats connus possibles
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int MetObs_Load(Tcl_Interp *Interp,char *File,TMetObs *Obs) {

   int res=0,type=31;

#ifdef HAVE_RMN
   type=f77name(wkoffit)(File,strlen(File));
#endif
   if(strstr(File,".sqlite") != NULL){
      type = MET_SQLITE;
   } else if (strstr(File, ".other") != NULL){
      type = MET_SQLITE;
   }

   App_Log(DEBUG,"%s: File type is %i\n",__func__,type);

   switch (type) {
      case MET_BURP: res=MetObs_LoadBURP(Interp,File,Obs);  break;
      case MET_BUFR: res=MetObs_LoadBUFR(Interp,File,Obs); break;
//Not recognized      case 8 : res=MetObs_LoadBUFR(Interp,File,Obs);  break;
#ifdef HAVE_SQLITE3
      case MET_SQLITE: res=MetObs_LoadSQLite(Interp,File,Obs); break;
#endif
      case MET_OTHER: if ((res=MetObs_LoadSWOB(Interp,File,Obs))==TCL_ERROR) {
                  res=MetObs_LoadASCII(Interp,File,Obs);
               }
               break;
   }

   if (!Obs->Time)
      Obs->Time=Obs->Time1;

   return(res);
}

int TMetElem_BUFRAdd(TMetObs *Obs,TMetElemData *Data,float Lat,float Lon,float Hgt,time_t Time,char *Id,char *Multi) {

   TMetLoc *loc=NULL;

   // Insert station in list if not already done
   if (Data->Ne && Lat!=-999.0 && Lon!=-999.0) {

      // Check if station already exists, unless this is a satobs file with multiple location for same id and station name is same as before
 //     if (!Multi || strcmp(PrevId,Id)!=0)
      loc=TMetLoc_FindWithCoord(Obs,NULL,Id,Lat,Lon,-999.0,MET_TYPEID,Multi);

      if (!loc) {
         loc=TMetLoc_New(Obs,Id,NULL,Lat,Lon,Hgt);
      }
      Obs->Time0=(Obs->Time0<Time && Obs->Time0!=0)?Obs->Time0:Time;
      Obs->Time1=Obs->Time1>Time?Obs->Time1:Time;
      TMetElem_InsertCopy(loc,0,Time,Data);
      return(1);
   }
   return(0);
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
 *---------------------------------------------------------------------------------------------------------------
*/
void MetObs_LocFree(TMetLoc *Loc){

   TMetLoc  *lnext,*loc=Loc;
   TMetElem *enext,*elem;

   while(loc) {
      if (loc->Id)  free(loc->Id);
      if (loc->No)  free(loc->No);

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
 *----------------------------------------------------------------------------
*/
void MetObs_GetStat(TMetObs *Obs,TMetModelItem *Item){

   TMetLoc      *loc;
   TMetElem     *elem;
   TMetElemData *data;
   int           n,e,v,t=0,s=0;
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
               if (data->Code[e]->descriptor==Item->Code[0]) {
                  for(v=0;v<data->Nv;v++) {
//                     for(t=0;t<data->Nt;t++) {
                        val=MetObs_GetData(data,e,v,t);
                        if (MET_VALID(val,Obs->NoData)) {
                           min=min<val?min:val;
                           max=max>val?max:val;
                           s=1;
                        }
//                     }
                  }
               }
            }
         }
         elem=elem->Next;
      }
      loc=loc->Next;
   }

   if (s) {
      if (!(Item->Spec->MinMax&DATASPEC_MINSET)) Item->Spec->Min=min;
      if (!(Item->Spec->MinMax&DATASPEC_MAXSET)) Item->Spec->Max=max;
      if (!(Item->Spec->MinMax&DATASPEC_MINSET)) Item->Spec->Min=Item->Spec->Max<Item->Spec->Min?Item->Spec->Max:Item->Spec->Min;
   }
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
 *----------------------------------------------------------------------------
*/

int MetObs_Render(Tcl_Interp *Interp,TMetObs *Obs,ViewportItem *VP,Projection *Proj,GLuint GLMode) {

   TMetLoc      *loc;
   TMetElem     *elem;
   TMetElemData *data;
   TDataSpec    *spec;
   Tcl_Obj      *obj;
   int           ib,ia,io;
   Vect3d        pix;
   Coord         co;
   char          buf[128];
   double        dlat,dlon,z,val,valid,dir,dx,dy,k,dk;
   int           d,e,i,n,v,t,iy,idx,line,id,ne,mk,l,nl;
   double        alpha=1.0;
   int           clat,clon,cdir,nobs,min[2],max[2],skip,flag,nv;

   extern void Data_RenderBarbule(int Type,int Flip,float Axis,float Lat,float Lon,float Elev,float Speed,float Dir,float Size,Projection *Proj);

   if (GLRender->Resolution>2) {
      return(0);
   }

   if (!Obs || !Obs->Model) {
      return(0);
   }

   if (!Obs->Model->Flat) {
      Projection_UnClip(Proj);
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

   if (Interp)
      Tcl_AppendResult(Interp,"%% Postscript des observations meteorologiques\n",(char*)NULL);

   // Get height code definition pointer
   if (Obs->Model->Elev) {
      glEnable(GL_DEPTH_TEST);
   }

   // Initialize rendering parameters per model items
   min[0]=min[1]=max[0]=max[1]=0;n=0;
   for(i=0;i<Obs->Model->NItem;i++) {
      if ((spec=Obs->Model->Items[i].Spec)) {
	
	if (!spec->Active) {
	  return 0;
	}

         // Get value limits
         if (isnan(spec->Min) || isnan(spec->Max) || spec->Min==spec->Max)
            MetObs_GetStat(Obs,&Obs->Model->Items[i]);

         // Keep model limits
         min[0]=fmin(min[0],Obs->Model->Items[i].X);
         min[1]=fmin(min[1],Obs->Model->Items[i].Y);
         max[0]=fmax(max[0],Obs->Model->Items[i].X);
         max[1]=fmax(max[1],Obs->Model->Items[i].Y);
         n=fmax(n,Obs->Model->Items[i].Spec->Size);

         // Define rendering parameters
         DataSpec_Intervals(spec,spec->Min,spec->Max);
         DataSpec_Define(spec);
      }
   }

   // Add spacing for crowd coverage
   min[0]*=Obs->Model->Space;
   min[1]*=Obs->Model->Space;
   max[0]*=Obs->Model->Space;
   max[1]*=Obs->Model->Space;
   n>>=1;
   min[0]-=n;
   min[1]-=n;
   max[0]+=n;
   max[1]+=n;

   // for all of the sations
   loc=Obs->Loc;
   n=-1;
   nobs=0;

   glPushName(PICK_METOBS);
   
   while(loc) {

      line=0;
      n++;

      // Check for data bktyp matching
      if (Obs->CodeType && Obs->CodeType!=loc->CodeType) {
         loc=loc->Next;
         continue;
      }
      
      // Check for data flag matching
      if (Obs->Flag && !(Obs->Flag&loc->Flag)) {
         loc=loc->Next;
         continue;
      }

      if (GLMode==GL_SELECT) {
         if (!VP->ForcePick && (n && !(n%10)) && Tcl_DoOneEvent(TCL_WINDOW_EVENTS|TCL_DONT_WAIT)) {
            break;
         }
      }

      // Get the elements group for the specific time
      if ((elem=TMetElem_Find(loc,Obs->Time,Obs->Lag))) {

         // Fix transparency on validity time persistance
         if (Obs->Persistance) {
            alpha=1.0-((double)(Obs->Time-elem->Time)/Obs->Persistance);

            // Continue with next location if this data is too old to be seen
            if (alpha<=0.0) {
               loc=loc->Next;
               continue;
            }
         }

         Projection_Pixel(Proj,VP,loc->Coord,pix);
         if (!loc->Grid[0] && Obs->Model->Overspace && !ViewportCrowdPush(VP,pix[0]+min[0],pix[1]+min[1],pix[0]+max[0],pix[1]+max[1],Obs->Model->Overspace)) {
            loc=loc->Next;
            continue;
         }

         glPushName(n);

         // Get station height
         z=ZRef_Level2Meter(loc->Coord.Elev,loc->Level);

         // Loop on the model items
         skip=1;
         for(i=0;i<Obs->Model->NItem;i++) {
            if (!(spec=Obs->Model->Items[i].Spec)) {
               continue;
            }

            if (Obs->Persistance || (spec->Map && spec->Map->Alpha)) {
               glEnable(GL_BLEND);
            } else {
               glDisable(GL_BLEND);
            }
            glPushName(i);

            // Loop on the data elements
            nv=-1;
            for(d=0;d<elem->NData;d++) {
               data=elem->EData[d];

               // Get displacement, elevation, component indexes
               clat=clon=-1;
               io=ia=ib=-1;
               cdir=-1;
               for(e=0;e<data->Ne;e++) {
                  if (spec->RenderVector && data->Code[e]->descriptor==Obs->Model->Items[i].Code[1]) {
                     cdir=e;
                  }
                  if (data->Code[e]->descriptor==5002 || data->Code[e]->descriptor==5001) {        // Latitude coordinate
                     clat=e;
                  } else if (data->Code[e]->descriptor==6002 || data->Code[e]->descriptor==6001) { // Longitude coordinate
                     clon=e;
                  } else if (data->Code[e]->descriptor==5015) {                                    // Latitude displacement
                     ia=e;
                  } else if (data->Code[e]->descriptor==6015) {                                    // Longitude displacement
                     io=e;
                  } else if (data->Code[e]->descriptor==Obs->Model->Elev) {                        // Height
                     ib=e;
                  }
               }

               // Check for data family matching (bit 3-5, 000=new,001=corrected,010=repeat,011=human corrected,100=reserved
               flag=data->Family&0x38;
               if (Obs->Family>-1 && Obs->Family!=flag) {
                  continue;
               }

               // Check for data bktyp matching
               if (Obs->Type>-1 && !((data->Type>>6&0x1)==Obs->Type)) {
                  continue;
               }

               ne=-1;
               for(e=0;e<data->Ne;e++) {
                  if (data->Code[e]->descriptor==Obs->Model->Items[i].Code[0]) {
                     ne++;
                     
                     id=0;
                     for(v=(Obs->NVal<=-1?0:Obs->NVal);v<(Obs->NVal<=-1?data->Nv:((Obs->NVal+1)>data->Nv?data->Nv:(Obs->NVal+1)));v++) {
                        for(t=0;t<data->Nt;t++) {
                           nv++;

                           // Check markers
                           if (Obs->Marker) {
                              mk=MetObs_GetMarker(data,e,v,t);
                              if ((Obs->MarkerOp=='O' && !(mk&Obs->Marker)) || (Obs->MarkerOp=='A' && !(mk==Obs->Marker))) {
                                 continue;
                              }
                           }

                           // Check for validity
                           val=MetObs_GetData(data,e,v,t);
                           valid=MET_VALID(val,Obs->NoData);

                           if (valid) {
                              if (val<spec->Min || val>spec->Max) {
                                 continue;
                              }
                              if (spec->InterNb && val<spec->Inter[0])
                                 continue;
                           }

                           // Station coordinates
                           co.Lat=loc->Coord.Lat;
                           co.Lon=loc->Coord.Lon;
                           co.Elev=z;

                           // Get height if specified
                           if (ib!=-1) {
                              k=MetObs_GetData(data,ib,v,0);
                              if (MET_VALID(k,Obs->NoData)) {
 
                                 // Compare with specified list
                                 if (Obs->Levels) {
                                    Tcl_ListObjLength(Interp,Obs->Levels,&nl);
                                    if (nl) {
                                       for(l=0;l<nl;l++) {
                                          Tcl_ListObjIndex(Interp,Obs->Levels,l,&obj);
                                          Tcl_GetDoubleFromObj(Interp,obj,&dk);
                                          if (k==dk) {
                                             break;   
                                          }
                                       }
                                       if (l==nl) {
                                          continue;
                                       }
                                    }
                                 }
                                 // Convert pressure level to meters
                                 if (data->Code[ib]->unit[0]=='P' && data->Code[ib]->unit[1]=='A') {         
                                    k=ZRef_Level2Meter(k/100.0f,LVL_PRES);
                                 }
                                 co.Elev=k;
                              }
                           }
                          
                           // Get latlon displacement
                           dlat=dlon=0.0;
                           if (ia!=-1 && io!=-1) {
                              dlat=MetObs_GetData(data,ia,v,0);
                              dlon=MetObs_GetData(data,io,v,0);
                              if (!MET_VALID(dlat,Obs->NoData)) dlat=0.0;
                              if (!MET_VALID(dlon,Obs->NoData)) dlon=0.0;
                           }
                           
                           // Get coordinates if per sample
                           if (clat!=-1 && clon!=-1) {
                              co.Lat=MetObs_GetData(data,clat,(loc->Grid[0]?0:v),t);
                              co.Lon=MetObs_GetData(data,clon,(loc->Grid[0]?0:v),t);
                           }
                           
                           // Add displacement
                           co.Lat+=dlat;
                           co.Lon+=dlon;
                           
                           if (!Projection_Pixel(Proj,VP,co,pix)) {
                              continue;
                           }
                           
//                           if (Obs->Model->Overspace && !ViewportCrowdPush(VP,pix[0]+min[0],pix[1]+min[1],pix[0]+max[0],pix[1]+max[1],Obs->Model->Overspace)) {
//                              continue;
//                           }

                           skip=0;

                           if (Interp) {
                              Tk_CanvasPsColor(Interp,VP->canvas,spec->Outline);
                              sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",spec->Width);
                              Tcl_AppendResult(Interp,buf,(char*)NULL);
                           } else {
                              if (spec->Outline) {
                                 glColor4us(spec->Outline->red,spec->Outline->green,spec->Outline->blue,alpha*65535);
                              } else {
                                 glColor4us(0,0,0,alpha*65535);
                              }
                              glLineWidth(spec->Width);
                           }

                           // Set position within projection
                           glPushMatrix();
                           glPushName(nv);

                           if (Obs->Model->Flat) {
                              Proj->Type->Locate(Proj,co.Lat,co.Lon,1);
                              glTranslated(0.0,0.0,ZM(Proj,co.Elev));
                              glScalef(VP->Ratio,VP->Ratio,1.0);
                           } else {
                              if (loc->Pix[0]!=0.0 && loc->Pix[1]!=0.0) {
                                 glTranslated(loc->Pix[0],ViewportY(VP)+VP->Height-loc->Pix[1],0.0);
                              } else {
                                 glTranslated(pix[0],pix[1],0.0);
                              }
                           }

                           dx=Obs->Model->Items[i].X*Obs->Model->Space+(Obs->Model->Flat?loc->Pix[0]:0);
                           dy=-Obs->Model->Items[i].Y*Obs->Model->Space+(Obs->Model->Flat?-loc->Pix[1]:0);

                           // Draw the position line if needed
                           if ((loc->Pix[0]!=0.0 || loc->Pix[1]!=0.0) && !line) {
                              if (Interp)
                                 glFeedbackInit(20,GL_2D);

                              glBegin(GL_LINES);
                                 glVertex3d(0,0,0);
                                 if (Obs->Model->Flat) {
                                    glVertex3d(dx,dy,0.0);
                                 } else {
                                    glVertex3d(pix[0]-loc->Pix[0],pix[1]-(ViewportY(VP)+VP->Height-loc->Pix[1]),0.0);
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
                              if (!Obs->Model->Flat)
                                 glTranslated(pix[0]-loc->Pix[0],pix[1]-(ViewportY(VP)+VP->Height-loc->Pix[1]),0.0);
                              glScalef(v,v,1.0);
                              glDrawCircle(64,GL_POLYGON);
                              line=1;
                              glPopMatrix();
                              if (Interp) {
                                 glFeedbackProcess(Interp,GL_2D);
                              }
                           }

                           // Positionner dans le modele
                           glTranslated(dx,dy,0.0);

                           iy=spec->RenderLabel+spec->RenderCoord+spec->RenderValue+(spec->WMO?1:0);

                           if (id && spec->RenderLabel) iy--;
                           if (id && spec->RenderCoord) iy--;

                           if (!id) {
                              if (spec->RenderLabel && GLRender->Resolution<=1 && GLMode!=GL_SELECT) {
                                 if (loc->No) {
                                    snprintf(buf,128,"%s (%s)",loc->Id,loc->No);
                                 } else {
                                    snprintf(buf,128,"%s",loc->Id);
                                 }
                                 MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                              }

                              if (spec->RenderCoord && GLRender->Resolution<=1 && GLMode!=GL_SELECT) {
                                 snprintf(buf,128,"(%.4f,%.4f)",co.Lat,co.Lon);
                                 MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                              }
                              id=1;
                           }

                           if (spec->RenderVector && valid && !(spec->WMO && VAL2SPEC(spec,val)<5.0)) {
                              dir=MetObs_GetData(data,cdir,v,t);
//                                 dir=TMetElem_Value(data,Obs->Model->Items[i].Code[1],e,v,t);
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
                                 Data_RenderBarbule(spec->RenderVector,1,0.0,0.0,0.0,0.0,VAL2SPEC(spec,val),dir,VECTORSIZE(spec,val),NULL);
                                 if (Interp) {
                                    glFeedbackProcess(Interp,GL_2D);
                                    Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);
                                 }
                              }
                           }

                           if (spec->RenderValue && valid && GLRender->Resolution<=1 && GLMode!=GL_SELECT) {
                              if (spec->Map && spec->MapAll) {
                                 VAL2COL(idx,spec,val);

                                 if (Interp) {
                                    CMap_PostscriptColor(Interp,spec->Map,idx);
                                 } else {
                                    glColor4ub(spec->Map->Color[idx][0],spec->Map->Color[idx][1],spec->Map->Color[idx][2],spec->Map->Color[idx][3]*alpha);
                                 }
                              }
                              DataSpec_Format(spec,VAL2SPEC(spec,val),buf);
                              MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                           }

                           if (spec->WMO && GLRender->Resolution<=1 && GLMode!=GL_SELECT) {
                              // Load up the symbols if not done yet
                              if (!WMO_Symbol1) {
                                 WMO_Symbol1=Tk_GetFont(Interp,GLRender->TkWin,"-misc-weathersymbols-medium-r-normal--14-0-0-0-p-0-microsoft-symbol");
                              }
                                 if (!WMO_Symbol2) {
                                 WMO_Symbol2=Tk_GetFont(Interp,GLRender->TkWin,"-macromedia-meteo_b-medium-r-normal--14-0-0-0-p-0-microsoft-symbol");
                              }
                              if (spec->WMO==1) { // AUTO
                                 if (val==0) {
                                    glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
                                    glEnableClientState(GL_VERTEX_ARRAY);
                                    glVertexPointer(2,GL_DOUBLE,0,IconList[1].Co);
                                    glScalef(9,9,1.0);
                                    glTranslated(0.0,0.4,0.0);

                                    if (Interp) glFeedbackInit(IconList[1].Nb*40,GL_2D);
                                    glDrawArrays(IconList[1].Type,0,IconList[1].Nb);
                                    if (Interp) glFeedbackProcess(Interp,GL_2D);
                                 }
                              } else if (spec->WMO==2) { // N
                                 if (val>=0 && val<=9) {
                                    spec->Font=WMO_Symbol1;
                                    Tk_GetFontMetrics(spec->Font,&spec->TKM);
                                    sprintf(buf,"%c",(int)val+40);
                                    MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                                 }
                              } else if (spec->WMO==3) { // WW
                                 if (val>=4 && val<=99) {
                                    spec->Font=WMO_Symbol1;
                                    Tk_GetFontMetrics(spec->Font,&spec->TKM);
                                    sprintf(buf,"%c",(int)val+100);
                                    MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                                 }
                              } else if (spec->WMO==4) { // CL
                                 if (val>=1 && val<=9) {
                                    spec->Font=WMO_Symbol2;
                                    Tk_GetFontMetrics(spec->Font,&spec->TKM);
                                    sprintf(buf,"%c",(int)val+34);
                                    MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                                 }
                              } else if (spec->WMO==5) { // CM
                                 if (val>=1 && val<=9) {
                                    spec->Font=WMO_Symbol2;
                                    Tk_GetFontMetrics(spec->Font,&spec->TKM);
                                    sprintf(buf,"%c",(int)val+44);
                                    MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                                 }
                              } else if (spec->WMO==6) { // CH
                                 if (val>=1 && val<=9) {
                                    spec->Font=WMO_Symbol2;
                                    Tk_GetFontMetrics(spec->Font,&spec->TKM);
                                    sprintf(buf,"%c",(int)val+55);
                                    MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                                 }
                              } else if (spec->WMO==7) { // A
                                 if (val>=0 && val<=8) {
                                    spec->Font=WMO_Symbol2;
                                    Tk_GetFontMetrics(spec->Font,&spec->TKM);
                                    sprintf(buf,"%c",(int)val+93);
                                    MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                                 }
                              }
                           }

                           if (spec->Icon) {
                              MetObs_RenderIcon(Interp,spec,alpha,val,VP,Proj);
                           }
                           nobs++;

                           glPopName();
                           glPopMatrix();

                           // If we're dynamic, only show first of grouped data
                           if (clat && GLRender->Resolution!=1) {
                              break;
                           }
                        }

                        // If this is no grouped data, skip the rest if no height is specified, they'll all be overlapped anyway
                        if (clat==-1 && ib==-1) {
                           break;
                        }
                     }
                     // TODO break if grouped data, until we can select the variables (ex:Per channel)
                     if (data->Nt>1)
                        break;
                  }
                  // Skip the rest if no height is specified, and one has been drawn, they'll all be overlapped anyway
                  if (ne>=0 && ((ib==-1 && clat==-1 && ia==-1) || Proj->Scale==1.0)) {
                     break;
                  }
               }
            }
            glPopName();
         }

         // It is possible nothing was drawn so remove from crowd list if so
         if (Obs->Model->Overspace && skip) {
            ViewportCrowdPop(VP);
         };

         glPopName();
      }
      
      loc=loc->Next;
   }


   if (!Obs->Model->Flat) {
      glPopMatrix();
      glMatrixMode(GL_PROJECTION);
      glPopMatrix();
      Projection_Clip(Proj);
   }
   glPopName();
   glEnable(GL_CULL_FACE);
   glDisable(GL_BLEND);
   glDisable(GL_DEPTH_TEST);
   glDisableClientState(GL_VERTEX_ARRAY);
   glDisable(GL_DEPTH_TEST);

   if (GLRender->GLDebug)
      App_Log(DEBUG,"%s: Nb Loc=%i NbObs=%i\n",__func__,n,nobs);

   return(n);
}

int MetObs_RenderIcon(Tcl_Interp *Interp,TDataSpec *Spec,double Alpha,double Value,ViewportItem *VP,Projection *Proj) {

   int    idx=-1;
   char   buf[128];

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
               return(1);
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
               return(1);
            }
         }
      }

      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      if (Interp) glFeedbackInit(IconList[Spec->Icon].Nb*40,GL_2D);
      glDrawArrays(IconList[Spec->Icon].Type,0,IconList[Spec->Icon].Nb);
      if (Interp) glFeedbackProcess(Interp,GL_2D);
   }

   if (Spec->Outline && Spec->Width) {
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
      if (Interp) {
         Tk_CanvasPsColor(Interp,VP->canvas,Spec->Outline);
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",Spec->Width);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
      } else {
         glColor4us(Spec->Outline->red,Spec->Outline->green,Spec->Outline->blue,Alpha*65535);
         glLineWidth(Spec->Width);
      }
      if (Interp) glFeedbackInit(IconList[Spec->Icon].Nb*40,GL_2D);
      glDrawArrays(IconList[Spec->Icon].Type,0,IconList[Spec->Icon].Nb);
      if (Interp) glFeedbackProcess(Interp,GL_2D);
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
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
 *----------------------------------------------------------------------------
*/
void MetObs_RenderInfo(Tcl_Interp *Interp,TDataSpec *Spec,char *String,ViewportItem *VP,Projection *Proj,int Line,int DX,int DY) {

   double dx=0,dy=0,sz=0;

   if (!Spec->Font || !String) {
      return;
   }

   if (Interp) {
      dx=DX;
      dy=DY;
   }

   glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),Spec->Font);

   if (Spec->Icon) {
      sz=(Spec->Size*0.5+Spec->Width);
      dx+=sz+2;
   } else {
      dx-=Tk_TextWidth(Spec->Font,String,strlen(String))/2;
      dy-=(Spec->TKM.linespace/2);
   }

   dy+=sz+(Line-1)*2*(Spec->TKM.linespace/2);

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
 *----------------------------------------------------------------------------
*/
static int MetObs_Stat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   TMetObs *met;
   int      i,idx;
   double   val;

   static CONST char *sopt[] = { "-tag","-nodata",NULL };
   enum                opt { TAG,NODATA };

   met=MetObs_Get(Name);
   if (!met) {
      Tcl_AppendResult(Interp,"\n   MetObs_Stats: Observation Id unknown: \"",Name,"\"",(char*)NULL);
      return(TCL_ERROR);
   }

   for(i=0;i<Objc;i++) {
      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
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
 *---------------------------------------------------------------------------------------------------------------
*/

static int MetReport_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   TMetElemData *data;

   int         idx,n;
   static CONST char *sopt[] = { "create","free","define","stats","is","all",NULL };
   enum               opt { CREATE,FREE,DEFINE,STATS,IS,ALL };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",TCL_EXACT,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case CREATE:
         if (!(data=(TMetElemData*)malloc(sizeof(TMetElemData)))) {
            Tcl_AppendResult(Interp,"\n   MetReport_Cmd: Unable to allocate memory for report",(char*)NULL);
            return(TCL_ERROR);
         }
         data->Ne=data->Nv=data->Nt=0;
         data->Code=NULL;
         data->Data=NULL;
         data->Marker=NULL;

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

      case STATS:
         break;

      case ALL:
         TclY_HashAll(Interp,&MetRepTable);
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
 *----------------------------------------------------------------------------
*/
static int MetReport_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj      *obj,*sub,*subsub;
   TMetElemData *data;
   EntryTableB  *eb;
   int           ne,e,v,t,nv,nt,i,j,idx,n,nl,mk;
   float        *valf;
   double        val;

   static CONST char *sopt[] = { "-DATE","-FLAG","-CODETYPE","-FAMILY","-TYPE","-STYPE","-ELEMENT","-DESC","-UNIT","-CODE","-FORMAT","-VALUE",NULL };
   enum                opt { DATE,FLAG,CODETYPE,FAMILY,TYPE,STYPE,ELEMENT,DESC,UNIT,CODE,FORMAT,VALUE };

   data=MetReport_Get(Name);
   if (!data) {
      Tcl_AppendResult(Interp,"\n   MetReport_Define: Report id unknown: \"",Name,"\"",(char*)NULL);
      return(TCL_ERROR);
   }

   for (i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case DATE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewLongObj(data->Time));
            } else {
               Tcl_GetLongFromObj(Interp,Objv[++i],&data->Time);
            }
            break;

         case FLAG:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(data->Loc->Flag));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&mk);
               data->Loc->Flag=mk;
            }
            break;

         case CODETYPE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(data->Loc->CodeType));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&mk);
               data->Loc->CodeType=mk;
            }
            break;

         case FAMILY:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(data->Family));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&data->Family);
            }
            break;

         case TYPE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(data->Type));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&data->Type);
            }
            break;

         case STYPE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(data->SType));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&data->SType);
            }
            break;

         case DESC:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for(e=0;e<data->Ne;e++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(data->Code[e]->description,-1));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ne);
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(data->Code[ne]->description,-1));
            }
            break;

         case UNIT:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for(e=0;e<data->Ne;e++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(data->Code[e]->unit,-1));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ne);
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(data->Code[ne]->unit,-1));
            }
            break;

         case FORMAT:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for(e=0;e<data->Ne;e++) {
                  sub=Tcl_NewListObj(0,NULL);
                  for(v=(data->Obs->NVal<=-1?0:data->Obs->NVal);v<(data->Obs->NVal<=-1?data->Nv:((data->Obs->NVal+1)>data->Nv?data->Nv:(data->Obs->NVal+1)));v++) {
                     subsub=Tcl_NewListObj(0,NULL);
                     for(t=0;t<data->Nt;t++) {
                        mk=MetObs_GetMarker(data,e,v,t);
                        if (!data->Obs || !data->Obs->Marker || (data->Obs->MarkerOp=='O' && (mk&data->Obs->Marker)) || (data->Obs->MarkerOp=='A' && (mk==data->Obs->Marker))) {
                           if (data->Code[e]->encoding.scale) {
                              Tcl_ListObjAppendElement(Interp,subsub,Tcl_NewStringObj("%g",-1));
                           } else {
                              Tcl_ListObjAppendElement(Interp,subsub,Tcl_NewStringObj("%.0f",-1));
                           }
                        }
                     }
                     Tcl_ListObjAppendElement(Interp,sub,subsub);
                  }
                  Tcl_ListObjAppendElement(Interp,obj,sub);
               }
               Tcl_SetObjResult(Interp,obj);
            }
            break;

         case CODE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for(e=0;e<data->Ne;e++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(data->Code[e]->descriptor));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ne);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(data->Code[ne]->descriptor));
            }
            break;
            
         case VALUE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for(e=0;e<data->Ne;e++) {
                  sub=Tcl_NewListObj(0,NULL);
                  for(v=(data->Obs->NVal<=-1?0:data->Obs->NVal);v<(data->Obs->NVal<=-1?data->Nv:((data->Obs->NVal+1)>data->Nv?data->Nv:(data->Obs->NVal+1)));v++) {
                     subsub=Tcl_NewListObj(0,NULL);
                     for(t=0;t<data->Nt;t++) {
                        mk=MetObs_GetMarker(data,e,v,t);
                        if (!data->Obs || !data->Obs->Marker || (data->Obs->MarkerOp=='O' && (mk&data->Obs->Marker)) || (data->Obs->MarkerOp=='A' && (mk==data->Obs->Marker))) {
                           Tcl_ListObjAppendElement(Interp,subsub,Tcl_NewDoubleObj(MetObs_GetData(data,e,v,t)));
                        }
                     }
                     Tcl_ListObjAppendElement(Interp,sub,subsub);
                  }
                  Tcl_ListObjAppendElement(Interp,obj,sub);
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&ne);
               sub=Tcl_NewListObj(0,NULL);
               for(v=(data->Obs->NVal<=-1?0:data->Obs->NVal);v<(data->Obs->NVal<=-1?data->Nv:((data->Obs->NVal+1)>data->Nv?data->Nv:(data->Obs->NVal+1)));v++) {
                  subsub=Tcl_NewListObj(0,NULL);
                  for(t=0;t<data->Nt;t++) {
                     mk=MetObs_GetMarker(data,ne,v,t);
                     if (!data->Obs || !data->Obs->Marker || (data->Obs->MarkerOp=='O' && (mk&data->Obs->Marker)) || (data->Obs->MarkerOp=='A' && (mk==data->Obs->Marker))) {
                        Tcl_ListObjAppendElement(Interp,subsub,Tcl_NewDoubleObj(MetObs_GetData(data,ne,v,t)));
                     }
                  }
                  Tcl_ListObjAppendElement(Interp,sub,subsub);
               }
               Tcl_SetObjResult(Interp,sub);
            }
            break;

         case ELEMENT:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for(e=0;e<data->Ne;e++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(data->Code[e]->descriptor));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Objc==2) {
                  Tcl_ListObjLength(Interp,Objv[++i],&nl);
                  nv=0;
                  for(n=0;n<nl;n++) {
                     Tcl_ListObjIndex(Interp,Objv[i],n,&sub);
                     if ((eb=MetObs_BUFRFindTableCodeOrDesc(Interp,sub))) {
                        obj=Tcl_NewListObj(0,NULL);
                        for(e=0;e<data->Ne;e++) {
                           if (data->Code[e]->descriptor==eb->descriptor) {
                              nv=1;
                              for(v=(data->Obs->NVal<=-1?0:data->Obs->NVal);v<(data->Obs->NVal<=-1?data->Nv:((data->Obs->NVal+1)>data->Nv?data->Nv:(data->Obs->NVal+1)));v++) {
                                 sub=Tcl_NewListObj(0,NULL);
                                 for(t=0;t<data->Nt;t++) {
                                    /*Check for selected marker*/
                                    mk=MetObs_GetMarker(data,e,v,t);
                                    if (!data->Obs || !data->Obs->Marker || (data->Obs->MarkerOp=='O' && (mk&data->Obs->Marker)) || (data->Obs->MarkerOp=='A' && (mk==data->Obs->Marker))) {
                                       Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(MetObs_GetData(data,e,v,t)));
                                    }
                                 }
                                 Tcl_ListObjAppendElement(Interp,obj,sub);
                              }
                           }
                        }
                        if (nv) {
                           Tcl_SetObjResult(Interp,obj);
                           return(TCL_OK);
                           break;
                        }
                     } else {
                        Tcl_AppendResult(Interp,"\n   MetReport_Define: Wrong element(s)",(char*)NULL);
                        return(TCL_ERROR);
                     }
                  }
               } else if (Objc==3) {
                  if (!(eb=MetObs_BUFRFindTableCodeOrDesc(Interp,Objv[++i]))) {
                     Tcl_AppendResult(Interp,"\n   MetReport_Define: Wrong element",(char*)NULL);
                     return(TCL_ERROR);
                  }
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
                     if (data->Code[ne]->descriptor==eb->descriptor) {
                        j=0;
                        break;
                     }
                  }
                  if (j) {
                     data->Ne++;
                     data->Code=(EntryTableB**)realloc(data->Code,data->Ne*sizeof(EntryTableB*));
                     data->Code[ne]=eb;

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
 *---------------------------------------------------------------------------------------------------------------
*/
TMetElemData* MetReport_Get(char *Name) {
   return((TMetElemData*)TclY_HashGet(&MetRepTable,Name));
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
 *---------------------------------------------------------------------------------------------------------------
*/
Tcl_Obj* MetReport_Put(Tcl_Interp *Interp,char *Name,TMetElemData *Report) {

   char buf[64];

   if (Report) {
      if (!Name) {
         sprintf(buf,"METREPORT_____%li",MetRepNo++);
         Name=buf;
      }
      if (TclY_HashSet(Interp,&MetRepTable,Name,Report)==TCL_ERROR) {
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
 *---------------------------------------------------------------------------------------------------------------
*/
int MetReport_Destroy(Tcl_Interp *Interp,char *Name) {

   TMetElemData  *ref=NULL;

   if ((ref=(TMetElemData*)TclY_HashDel(&MetRepTable,Name))) {
      TMetElemData_Free(ref);
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_Test>
 * Creation     : Mai 2018 Philippe Carphin
 *
 * But          : Permettre des tests par TCL
 *
 * Parametres    :
 *   <interp>    : Interpreteur Tcl
 *   <Objc>      : Nombre d'arguments
 *   <Objv>      : Pointeur sur la liste des arguments (word)
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques : This test command is invoked in TCL the following way:
 *    metobs test arg1 arg2 ... if we are in this function, it means that
 *    Objv[0] ~ metobs
 *    Objv[1] ~ test
 *
 *    Furthermore,
 *    Objv[2] should contain a test subcommand and
 *    Objv[3] should contain a name to be used as a key in the TCL hashmaps when
 *    creating or looking up a TMetObs instance.
 *
 *    I wanted to put this in a separate file but it uses functions that are
 *    static in this file.
 *---------------------------------------------------------------------------------------------------------------
*/
#if 0
static int MetObs_Test(Tcl_Interp *Interp, int Objc, Tcl_Obj *const Objv[])
{
   /*
    * Read the arguments of the command and determine the subcommand
    */
   const char *test_subcommands[] = {"create_test_obs", "show", "sql", NULL};
   enum TestSubcommand              { CREATE_TEST_OBS,    SHOW,   SQL      };

   enum TestSubcommand test_subcommand;
   int rc = Tcl_GetIndexFromObj(
         Interp,
         Objv[0],
         test_subcommands,
         "metobs test subcommand",
         TCL_EXACT,
         (int *)&test_subcommand
   ); if (rc != TCL_OK) return rc;

   char * name = NULL;
   if( Objc > 1 ){
      name = Tcl_GetString(Objv[1]);
   }

   /*
    * Dispatch to handler functions
    */
   TMetObs *obs = NULL;
   switch(test_subcommand){
      case CREATE_TEST_OBS:
         MetObs_Create(Interp, strdup(name));
         obs = MetObs_Get(name);
         MetObsTest_FillTestObs(obs);
         break;
      case SHOW:
         obs = MetObs_Get(name);
         if(obs == NULL){
            printf("No obs with name=%s found\n", name);
         } else {
//            MetObs_ShowObs(obs);
         }
         break;
      case SQL:
         if(name == NULL){
            name = "acars.sqlite";
         }
         MetObs_Create(Interp, "sql_test");
         obs = MetObs_Get("sql_test");
         MetObs_LoadSQLite(Interp, name, obs);

         break;
   }

   FUNCEND
   return TCL_OK;
}
#endif

#endif
