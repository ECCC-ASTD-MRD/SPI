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
 *=========================================================
 */

#include "tclMetObs.h"
#include "Projection.h"
#include <math.h>

static BUFR_Tables *BUFRTable=NULL;
TCL_DECLARE_MUTEX(MUTEX_BURPFILE)

#define fgetskip(BYTES,LEN,STREAM)   BYTES[0]='\0';while (fgets(BYTES,LEN,STREAM) && BYTES[0]=='#')

/* HashTable Tcl pour les observations et rapport*/
static Tcl_HashTable MetObsTable;
static Tcl_HashTable MetRepTable;
static long          MetRepNo=0;
static unsigned int  MetLocNo=0;
static int           MetObsInit=0;

static Tk_Font WMO_Symbol1=NULL;
static Tk_Font WMO_Symbol2=NULL;

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
   int        i,idx,no=1,res;
   long       code;
   char       table;

   static CONST char *sopt[] = { "-readcmc","-readmaster","-readlocal","-code","-desc","-unit","-insert",NULL };
   enum                opt { READCMC,READMASTER,READLOCAL,CODE,DESC,UNIT,INSERT };

   /*Figure out which table we are talking about*/
   if (Objc>1) {
      table=Tcl_GetString(Objv[1])[0];
      if (table!='B' && table!='C' && table!='D') {
         table='B';
         no=0;
      }
   }

   for (i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
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
            if (!(eb=bufr_tableb_fetch_entry(BUFRTable->master.tableB,code))) {
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
   long          time=0;
   int           e,t,i,j,d,v,idx,nv,mk,flag;
   float        *valf;
   double        val;
   char          search;

   static CONST char *sopt[] = { "-INFO","-ADDINFO","-COORD","-ID","-TAG","-NO","-ELEMENT","-REPORT","-NB","-DATE","-DATE0","-DATE1","-LAG","-VALID","-CODETYPE","-FAMILY","-FAMILYOP","-TYPE","-STYPE","-MARKER","-MARKEROP","-NVAL","-MODEL","-PERSISTANCE","-CACHE","-PIXEL",NULL };
   enum                opt { INFO,ADDINFO,COORD,ID,TAG,NO,ELEMENT,REPORT,NB,DATE,DATE0,DATE1,LAG,VALID,CODETYPE,FAMILY,FAMILYOP,TYPE,STYPE,MARKER,MARKEROP,NVAL,MODEL,PERSISTANCE,CACHE,PIXEL };

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
               search=Tcl_GetString(Objv[++i])[0]=='|'?MET_TYPETG:MET_TYPEID;
               loc=TMetLoc_Find(obs,NULL,Tcl_GetString(Objv[i]),search);
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
           } else {
               Tcl_AppendResult(Interp,"\n   MetObs_Define: Wrong number of arguments, must be \"observation define -ID [id]\"",(char*)NULL);
               return(TCL_ERROR);
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
                           flag=(data->Family&0x7)==0?data->Family|0x20:data->Family;
                           if (!obs->Family || ((obs->FamilyOp=='O' && (obs->Family&flag)) || (obs->FamilyOp=='A' && (obs->Family==flag)))) {
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
                           flag=(data->Family&0x7)==0?data->Family|0x20:data->Family;
                           if (!obs->Family || ((obs->FamilyOp=='O' && (obs->Family&flag)) || (obs->FamilyOp=='A' && (obs->Family==flag)))) {
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

                     if (!(data=TMetElem_Insert(loc,0,time,0x0,0x0,0x0,1,nv,1,valf,NULL,&eb))) {
                        Tcl_AppendResult(Interp,"\n   MetObs_Define: Unable to add element",(char*)NULL);
                        free(valf);
                        return(TCL_ERROR);
                     }
                     obj=Tcl_NewIntObj(eb->descriptor);
                     if (TclY_ListObjFind(Interp,obs->Elems,obj)==-1) {
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
               search=Tcl_GetString(Objv[++i])[0]=='|'?MET_TYPETG:MET_TYPEID;
               if (Objc==2 || Objc==3) {
                  if (Objc==3) {
                     Tcl_GetLongFromObj(Interp,Objv[++i],&time);
                  }

                  loc=NULL;
                  obj=Tcl_NewListObj(0,NULL);
                  while (loc=TMetLoc_Find(obs,loc,Tcl_GetString(Objv[1]),search)) {
                     if ((elem=TMetElem_Find(loc,time,obs->Lag))) {
                        for(d=0;d<elem->NData;d++) {
                           data=elem->EData[d];
                           /*Check for selected family*/
                           flag=(data->Family&0x7)==0?data->Family|0x20:data->Family;
                           if (!obs->Family || ((obs->FamilyOp=='O' && (obs->Family&flag)) || (obs->FamilyOp=='A' && (obs->Family==flag)))) {
                              /*Check for data bktyp matching*/
                              if (obs->Type==-1 || (data->Type>>6&0x1)==obs->Type) {
                                 /*Link the ElementData to its Obs*/
                                 elem->EData[d]->Obs=obs;
                                 Tcl_ListObjAppendElement(Interp,obj,MetReport_Put(Interp,NULL,elem->EData[d]));
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
               while (loc=TMetLoc_Find(obs,loc,Tcl_GetString(Objv[1]),search)) {
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

         case FAMILY:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(obs->Family));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&obs->Family);
            }
            break;

         case FAMILYOP:
            if (Objc==1) {
               if (obs->FamilyOp=='A') {
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
               obs->FamilyOp=Tcl_GetString(Objv[i])[0];
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
static int MetObs_Create(Tcl_Interp *Interp,char *Name) {

   TMetObs *obs;

   if (!(obs=(TMetObs*)TclY_HashPut(Interp,&MetObsTable,Name,sizeof(TMetObs)))) {
      return(TCL_ERROR);
   }

   obs->Tag      = NULL;
   obs->FId      = -1;
   obs->Elems    = Tcl_NewListObj(0,NULL);
   obs->Time0    = 0;
   obs->Time1    = 0;
   obs->Loc      = NULL;
   obs->Time     = 0;
   obs->Cache    = 0;
   obs->Persistance = 0;
   obs->Lag      = 0;
   obs->NVal     = -1;
   obs->Type     = -1;
   obs->SType    = -1;
   obs->CodeType = 0;
   obs->Family   = 0x0;
   obs->FamilyOp = 'O';
   obs->Marker   = 0x0;
   obs->MarkerOp = 'O';
   obs->Info     = NULL;
   obs->NbInfo   = 0;
   obs->NoData   = -999.0f;

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
 *---------------------------------------------------------------------------------------------------------------
*/
static int MetObs_FreeHash(Tcl_Interp *Interp,char *Name) {

   TMetObs *obs=NULL;

   if ((obs=(TMetObs*)TclY_HashDel(&MetObsTable,Name))) {
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
 *---------------------------------------------------------------------------------------------------------------
*/
int MetObs_WriteASCII(Tcl_Interp *Interp,char *File,Tcl_Obj *List,char *Title) {

   return(TCL_OK);
}

int MetObs_WriteBUFR(Tcl_Interp *Interp,char *File,char *Template,Tcl_Obj *List,int Compress) {

   return(TCL_OK);
}

BUFR_Tables *MetObs_GetTables(void) {
   return(BUFRTable);
}

EntryTableB *MetObs_BUFRFindTableCodeOrDesc(Tcl_Interp *Interp,Tcl_Obj *Code) {
   int code;

   EntryTableB *eb=NULL;

   if (BUFRTable && BUFRTable->master.tableB) {
      if (TclY_Get0IntFromObj(Interp,Code,&code)==TCL_OK) {
         eb=bufr_tableb_fetch_entry(BUFRTable->master.tableB,code);
      } else {
         eb=bufr_tableb_fetch_entry_desc(BUFRTable->master.tableB,Tcl_GetString(Code));
      }
   }
   Tcl_ResetResult(Interp);
   return(eb);
}

EntryTableB *MetObs_BUFRFindTableCode(unsigned int Code) {

   if (!BUFRTable || !BUFRTable->master.tableB) {
      return(NULL);
   }
   return(bufr_tableb_fetch_entry(BUFRTable->master.tableB,Code));
}

EntryTableB *MetObs_BUFRFindTableDesc(char *Desc) {

   if (!BUFRTable || !BUFRTable->master.tableB) {
      return(NULL);
   }
   return(bufr_tableb_fetch_entry_desc(BUFRTable->master.tableB,Desc));
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

TMetLoc *TMetLoc_FindWithCoord(TMetObs *Obs,TMetLoc *From,char *Id,double Lat,double Lon,double Elev,int Type,char *Multi) {
   TMetLoc *loc;

   loc=From?From->Next:Obs->Loc;

   while(loc) {
      if (Type==MET_TYPENO) {
         if (strcmp(loc->No,Id)==0) {
            if ((Lat==-999.0 || loc->Coord.Lat==Lat) && (Lon==-999.0 || loc->Coord.Lon==Lon) && (Elev==-999.0 || loc->Coord.Elev==Elev)) {
               *Multi=0;
               break;
            } else {
               *Multi=1;
            }
         }
      } else if (Type==MET_TYPEID) {
         if (strcmp(loc->Id,Id)==0) {
            if ((Lat==-999.0 || loc->Coord.Lat==Lat) && (Lon==-999.0 || loc->Coord.Lon==Lon) && (Elev==-999.0 || loc->Coord.Elev==Elev)) {
               *Multi=0;
               break;
            } else {
               *Multi=1;
            }
         }
      } else {
         if (strcmp(loc->Tag,Id)==0) {
            if ((Lat==-999.0 || loc->Coord.Lat==Lat) && (Lon==-999.0 || loc->Coord.Lon==Lon) && (Elev==-999.0 || loc->Coord.Elev==Elev)) {
               *Multi=0;
               break;
            } else {
               *Multi=1;
            }
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
   loc->Coord.Lat=Lat;
   loc->Coord.Lon=Lon;
   loc->Coord.Elev=Elev;
   loc->Pix[0]=0.0;
   loc->Pix[1]=0.0;
   loc->Grid[0]=loc->Grid[1]=loc->Grid[2]=0;
   loc->Level=0;
   loc->Elems=NULL;

   sprintf(loc->Tag,"|%u",MetLocNo++);

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

float TMetElem_Height(const TMetElemData* restrict const Data,const int Code,const int Ne,const int Nv,const int Nt) {

   int   e,ne;
   float v;

   ne=Ne;
   for(e=0;e<Data->Ne;e++) {
      if (Data->Code[e]->descriptor==Code) {
         if ((ne--)==0) {
            v=MetObs_GetData(Data,e,Nv,Nt);
            if (v!=-999.0f) {
               if (strcmp(Data->Code[e]->unit,"PA")==0) {
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

   while(elem && Time && Time<elem->Time) {
      elem=elem->Next;
   }

   if (elem && Lag && (Time-elem->Time)>Lag) {
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
      if (Data->Code)   free(Data->Code);
      if (Data->Data)   free(Data->Data);
      if (Data->Marker) free(Data->Marker);
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

   /*Look for a spot in the ordered list*/
   pre=NULL;
   while(elem && Time<elem->Time) {
      pre=elem;
      elem=elem->Next;
   }

   /*If we already have this time*/
   if (elem && Time==elem->Time) {
      new=elem;
      for(n=elem->NData-1;n>=0;n--) {
         if (TMetElemData_Same(Data,elem->EData[n])) {
            /*Check for markers/data*/
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

   /*Create a new data bloc*/
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

TMetElemData *TMetElem_Insert(TMetLoc *Loc,time_t Min,time_t Time,int Fam,int Type,int SType,int Ne,int Nv,int Nt,float *Data,int *Marker,EntryTableB **Codes) {

   TMetElemData *ptr,*data=NULL;

   data=(TMetElemData*)malloc(sizeof(TMetElemData));

   data->Ne=Ne;
   data->Nv=Nv;
   data->Nt=Nt;
   data->Family=Fam;
   data->Type=Type;
   data->SType=SType;

   if (Data) {
      data->Data=(float*)malloc(data->Ne*data->Nv*data->Nt*sizeof(float));
      memcpy(data->Data,Data,data->Ne*data->Nv*data->Nt*sizeof(float));
   } else {
      data->Data=NULL;
   }

   if (Marker) {
      data->Marker=(int*)malloc(data->Ne*data->Nv*data->Nt*sizeof(int));
      memcpy(data->Marker,Marker,data->Ne*data->Nv*data->Nt*sizeof(int));
   } else {
      data->Marker=NULL;
   }

   if (Codes) {
      data->Code=(EntryTableB**)malloc(data->Ne*sizeof(EntryTableB*));
      memcpy(data->Code,Codes,data->Ne*sizeof(EntryTableB*));
   } else {
      data->Code=NULL;
   }

   if (!(ptr=TMetElem_Add(Loc,data,Time))) {
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
   data->Ne=Ne;
   data->Nv=Nv;
   data->Nt=Nt;
   data->Family=0x0;
   data->Type=0x0;
   data->SType=0x0;
   data->Data=(float*)malloc(Ne*Nv*Nt*sizeof(float));
   data->Code=(EntryTableB**)malloc(Ne*Nv*Nt*sizeof(EntryTableB*));
   data->Marker=NULL;

   return(data);
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

   int res;

   switch ((f77name(wkoffit)(File,strlen(File)))) {
      case 6 : res=MetObs_LoadBURP(Interp,File,Obs); break;
//      case 8 : res=MetObs_LoadBUFR(Interp,File,Obs); break;
      case 31: res=MetObs_LoadASCII(Interp,File,Obs); break;
      default: res=MetObs_LoadBUFR(Interp,File,Obs); break;
/*      default: Tcl_AppendResult(Interp,"\n   MetObs_Load : Invalid file type ",File,(char*)NULL);
               res=TCL_ERROR;
*/
   }

   if (!Obs->Time)
      Obs->Time=Obs->Time1;

   return(res);
}

int TMetElem_BUFRAdd(TMetObs *Obs,TMetElemData *Data,float Lat,float Lon,float Hgt,time_t Time,char *Id,char *PrevId,char *Multi) {

   TMetLoc *loc=NULL;
   time_t   time=0;

   /*Insert station in list if not already done*/
   if (Data->Ne && Lat!=-999.0 && Lon!=-999.0) {

      /*Check if station already exists, unless this is a satobs file with multiple location for same id and station name is same as before*/
 //     if (!Multi || strcmp(PrevId,Id)!=0)
      loc=TMetLoc_FindWithCoord(Obs,NULL,Id,Lat,Lon,-999.0,MET_TYPEID,Multi);

      strcpy(PrevId,Id);

      if (!loc) {
         loc=TMetLoc_New(Obs,Id,NULL,Lat,Lon,Hgt);
      }
      Obs->Time0=(Obs->Time0<time && Obs->Time0!=0)?Obs->Time0:Time;
      Obs->Time1=Obs->Time1>time?Obs->Time1:Time;
      TMetElem_InsertCopy(loc,0,Time,Data);
      return(1);
   }
   return(0);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_LoadBUFR>
 * Creation     : Mars 2008 J.P. Gauthier
 *
 * But          : Chargement d'un fichier d'observations en format BUFR.
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <File>     : Le nom du fichier
 *   <Obs>      : Observation
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int MetObs_LoadBUFR(Tcl_Interp *Interp,char *File,TMetObs *Obs) {

   Tcl_Obj        *obj=NULL;
   TMetElemData   *data=NULL;

   FILE           *fpBufr;
   BUFR_Tables    *tbls;
   BUFR_Message   *msg;
   BUFR_Dataset   *dts;
   DataSubset     *subset;
   BufrDescriptor *bcv;
   EntryTableB    *eb;
   int             i,j,ne,len,strid=0;
   char            stnid[256],previd[256],multi=0;
   double          value,lat,lon,hgt=0.0;
   int             yyyy,mm,dd,hh,mn,ss;

   obj=Tcl_NewStringObj("",0);

   if (!(fpBufr=fopen(File,"r"))) {
      Tcl_AppendResult(Interp,"\n   MetObs_LoadBUFR :  Unable to open file ",File,(char*)NULL);
      return(TCL_ERROR);
   }

   while ((bufr_read_message(fpBufr,&msg))>0) {

      /*Decode message*/
      dts=bufr_decode_message(msg,BUFRTable);
      bufr_free_message(msg);
      if (!dts) {
         Tcl_AppendResult(Interp,"\n   MetObs_LoadBUFR :  Unable to decode message",(char*)NULL);
         return(TCL_ERROR);
      }

      /*Check for local lable update*/
      if (bufr_contains_tables(dts)) {
         if ((tbls=bufr_extract_tables(dts))) {
            bufr_merge_tables(BUFRTable,tbls);
            bufr_free_tables(tbls);
         }
      }

      for (i=0;i<bufr_count_datasubset(dts);i++) {
         if (!(subset=bufr_get_datasubset(dts,i))) {
            fprintf(stdout,"(WARNING) MetObs_LoadBUFR: Invalid subset");
            continue;
         }

         ne=bufr_datasubset_count_descriptor(subset);
         data=TMetElemData_New(ne,1,1);
         data->Ne=0;

         stnid[0]='-';stnid[1]='\0';
         lat=lon=-999.0;
         yyyy=1970;mm=1;dd=1;hh=mn=ss=0;

         for (j=0;j<bufr_datasubset_count_descriptor(subset);j++) {
            if (!(bcv=bufr_datasubset_get_descriptor(subset,j))) {
               fprintf(stdout,"(WARNING) MetObs_LoadBUFR: Invalid subset code");
               continue;
            }

            /*Skip Table D && Table C operators*/
            if (bcv->descriptor>=300000 || bcv->descriptor>=100000)
               continue;

            if (bcv->meta) {
//               bufr_print_metadata(buf,bcv->meta);
//               bufr_print_output(buf);
            }

            if (bcv->flags & FLAG_SKIPPED) {
//               printf("#  %.6d ",bcv->descriptor);
            } else {

               /*If this code has a value*/
               if (bcv->value) {
                  if (!(eb=MetObs_BUFRFindTableCode(bcv->descriptor))) {
                     fprintf(stdout,"(WARNING) MetObs_LoadBUFR: Could not find element code (%i) int tables",bcv->descriptor);
                  } else {
                     Tcl_SetIntObj(obj,eb->descriptor);
                     if (TclY_ListObjFind(Interp,Obs->Elems,obj)==-1) {
                        Tcl_ListObjAppendElement(Interp,Obs->Elems,Tcl_DuplicateObj(obj));
                     }
                  }

                  /*Look for needed specific descriptor*/
                  switch(bcv->descriptor) {
                     /*Date related*/
                     case 4001:  yyyy=bufr_descriptor_get_ivalue(bcv); break;
                     case 4002:  mm=bufr_descriptor_get_ivalue(bcv);   break;
                     case 4003:  dd=bufr_descriptor_get_ivalue(bcv);   break;
                     case 4004:  hh=bufr_descriptor_get_ivalue(bcv);   break;
                     case 4005:  mn=bufr_descriptor_get_ivalue(bcv);   break;
                     case 4006:  ss=bufr_descriptor_get_ivalue(bcv);   break;
                     case 5001:  lat=bufr_descriptor_get_dvalue(bcv);  break;

                     /*Location related*/
                     case 6001:
                     case 6002:
                     case 28001:
                     case 28002:
                     case 28003:
                     case 28004:
                     case 6192:
                     case 6193:  lon=bufr_descriptor_get_dvalue(bcv);  break;
                     case 5002:
                     case 27001:
                     case 27002:
                     case 27003:
                     case 27004:
                     case 5192:
                     case 5193:  lat=bufr_descriptor_get_dvalue(bcv);  break;

                     /*Height related*/
                     case 7001:
                     case 7002:  hgt=bufr_descriptor_get_dvalue(bcv);  break;

                     /*Station ID related but prioritize string version*/
                     case 1002:
                     case 1007:  if (!strid) { sprintf(stnid,"%i",bufr_descriptor_get_ivalue(bcv));  strtrim(stnid,' '); }; break;
                     case 1015:
                     case 1018:
                     case 1019:  sprintf(stnid,"%s",bufr_descriptor_get_svalue(bcv,&len)); strtrim(stnid,' '); strid=1; break;

                     /*Time displacement*/
                     case 4011:  yyyy+=bufr_descriptor_get_ivalue(bcv);
                         if(TMetElem_BUFRAdd(Obs,data,lat,lon,hgt,System_DateTime2Seconds(yyyy*10000+mm*100+dd,hh*10000+mn*100+ss,1),stnid,previd,&multi)) {
                            data=TMetElemData_New(ne,1,1);
                            data->Ne=0;
                         }
                         break;
                     case 4012:  mm+=bufr_descriptor_get_ivalue(bcv);
                         if(TMetElem_BUFRAdd(Obs,data,lat,lon,hgt,System_DateTime2Seconds(yyyy*10000+mm*100+dd,hh*10000+mn*100+ss,1),stnid,previd,&multi)) {
                            data=TMetElemData_New(ne,1,1);
                            data->Ne=0;
                         }
                         break;
                     case 4013:  dd+=bufr_descriptor_get_ivalue(bcv);
                         if(TMetElem_BUFRAdd(Obs,data,lat,lon,hgt,System_DateTime2Seconds(yyyy*10000+mm*100+dd,hh*10000+mn*100+ss,1),stnid,previd,&multi)) {
                            data=TMetElemData_New(ne,1,1);
                            data->Ne=0;
                         }
                         break;
                     case 4014:  hh+=bufr_descriptor_get_ivalue(bcv);
                         if(TMetElem_BUFRAdd(Obs,data,lat,lon,hgt,System_DateTime2Seconds(yyyy*10000+mm*100+dd,hh*10000+mn*100+ss,1),stnid,previd,&multi)) {
                            data=TMetElemData_New(ne,1,1);
                            data->Ne=0;
                         }
                         break;
                     case 4015:  mn+=bufr_descriptor_get_ivalue(bcv);
                         if(TMetElem_BUFRAdd(Obs,data,lat,lon,hgt,System_DateTime2Seconds(yyyy*10000+mm*100+dd,hh*10000+mn*100+ss,1),stnid,previd,&multi)) {
                            data=TMetElemData_New(ne,1,1);
                            data->Ne=0;
                         }
                         break;
                     case 4016:  ss+=bufr_descriptor_get_ivalue(bcv);
                         if(TMetElem_BUFRAdd(Obs,data,lat,lon,hgt,System_DateTime2Seconds(yyyy*10000+mm*100+dd,hh*10000+mn*100+ss,1),stnid,previd,&multi)) {
                            data=TMetElemData_New(ne,1,1);
                            data->Ne=0;
                         }
                         break;
                 }

                  /* If there are Associated Fields */
                  if (bcv->value->af)  {
                     BufrAF *af = bcv->value->af;
//                     sprintf( buf, "(0x%llx:%d bits)", af->bits, af->nbits );
                  }

                  switch(bcv->value->type) {
                     case VALTYPE_INT8:
                     case VALTYPE_INT32:
                        value=bufr_descriptor_get_ivalue(bcv);
                        break;
                     case VALTYPE_INT64:
                        value=bufr_descriptor_get_ivalue(bcv);
                        break;
                     case VALTYPE_FLT32:
                        value = bufr_descriptor_get_fvalue(bcv);
                        if (bufr_is_missing_float(value)) {
                           value=-999.0;
                        }
                        break;
                    case VALTYPE_FLT64:
                        value = bufr_descriptor_get_dvalue(bcv);
                        if (bufr_is_missing_double(value)) {
                           value=-999.0;
                        }
                        break;
                     case VALTYPE_STRING:
/*                        int   len;

                        char *str = bufr_descriptor_get_svalue(bcv,&len);
                        printf("VALUE=%s",str);
*/
                           value=-999.0;
                        break;
                  }
                  data->Code[data->Ne]=eb;
                  data->Data[data->Ne]=value;
                  data->Ne++;
               }
            }
         }
         /*Insert station in list if not already done*/
         TMetElem_BUFRAdd(Obs,data,lat,lon,hgt,System_DateTime2Seconds(yyyy*10000+mm*100+dd,hh*10000+mn*100+ss,1),stnid,previd,&multi);

      }
      bufr_free_dataset(dts);
   }

   Tcl_DecrRefCount(obj);
   fclose(fpBufr);
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_LoadBURP>
 * Creation     : Avril 2007 J.P. Gauthier
 *
 * But          : Chargement d'un fichier d'observations en format BURP.
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <File>     : Le nom du fichier
 *   <Obs>      : Observation
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int MetObs_LoadBURP(Tcl_Interp *Interp,char *File,TMetObs *Obs) {

   int       err,sz,handle,code=TCL_OK;
   int      *buf=NULL;
   time_t    time=0,dt;

   EntryTableB  **eb=NULL;

   Tcl_Obj      *obj;
   TMetLoc      *loc;
   TMetElemData *data;

   int      e,sz1=0,sz2=0,c;

   int      hhmm,flag,codtyp,blat,blon,hgt,dx,dy,dlay,yymmdd,oars,runn,nblk,sup=0,nsup=0,xaux=0,nxaux=0,mkr=0;;
   int      blkno,nelem,nval,nt,bfam,bdesc,btyp,nbit,bit0,datyp,bknat,bktyp,bkstp;
   char     stnid[10],previd[10];
   int     *elems=NULL,*tblval=NULL,*codes=NULL;
   float   *tblvalf=NULL;
   char     multi=0;

   Tcl_MutexLock(&MUTEX_BURPFILE);

   if (Obs->FId==-1)
      Obs->FId=cs_fstlockid();

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
      cs_fstunlockid(Obs->FId);
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
      cs_fstunlockid(Obs->FId);
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
      Obs->CodeType=codtyp;

      /*Skip if it is a resume or a header*/
      if (stnid[0]=='>')
         continue;

      /*Check if station already exists, unless this is a satobs file with multiple location for same id and station name is same as before*/
      strtrim(stnid,' ');
      loc=NULL;
      if (!multi || strcmp(previd,stnid)!=0)
         loc=TMetLoc_FindWithCoord(Obs,NULL,stnid,(blat-9000.0)/100.0,blon/100.0,hgt-400,MET_TYPEID,&multi);

      strcpy(previd,stnid);
      /*Insert station in list if not already done*/
      if (!loc) {
         loc=TMetLoc_New(Obs,stnid,NULL,(blat-9000.0)/100.0,blon/100.0,hgt-400);
         loc->Grid[0]=dx/10.0;
         loc->Grid[1]=dy/10.0;
      }
      if ((time=System_DateTime2Seconds(yymmdd,hhmm*100,1))<0)
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

         bknat=(btyp>>11)&0x0F;
         bktyp=(btyp>>4)%0x7F;
         bkstp=(btyp)&0x0F;

         /*Skip if empty*/
         if ((nelem*nval*nt)==0) {
            fprintf(stdout,"(WARNING) MetObs_LoadBURP: Found empty report\n");
            continue;
         }
         /*Resize temporary buffers if needed*/
         if (nelem>sz1) {
            sz1=nelem;
            elems=(int*)realloc(elems,sz1*sizeof(int));
            codes=(int*)realloc(codes,sz1*sizeof(int));
            eb=(EntryTableB**)realloc(eb,sz1*sizeof(EntryTableB*));
         }

         if (nelem*nval*nt>sz2) {
            sz2=nelem*nval*nt;
            tblval=(int*)realloc(tblval,sz2*sizeof(int));
            tblvalf=(float*)realloc(tblvalf,sz2*sizeof(float));
         }

         /*Extract info*/
         err=c_mrbxtr(buf,blkno,elems,tblval);
         err=c_mrbdcl(elems,codes,nelem);

         /*Test for superobs ..... ta daaaaaaa*/
         if (stnid[0]=='^') {
            if (stnid[1]=='^') {
               fprintf(stdout,"(DEBUG) MetObs_LoadBURP: Found super duper obs\n");
            } else {

            }
         }

         /*If this is a marker bloc*/
         mkr=0;
         if (bknat==0x3 || codes[0]>=200000) {
            mkr=nelem*nval*nt;
         } else {
            err=c_mrbcvt(elems,tblval,tblvalf,nelem,nval,nt,0);
         }

         /*Get the elements code list and cache it within obs object*/
         c=0;
         for(e=0;e<nelem;e++) {
            if (mkr) {
               if (codes[e]>=200000)
                  codes[e]-=200000;
            }
            c=codes[e];

            if (eb[e]=MetObs_BUFRFindTableCode(c)) {
               Tcl_SetIntObj(obj,eb[e]->descriptor);
               if (TclY_ListObjFind(Interp,Obs->Elems,obj)==-1) {
                  Tcl_ListObjAppendElement(Interp,Obs->Elems,Tcl_DuplicateObj(obj));
               }
               c=1;
            } else {
               fprintf(stdout,"(WARNING) MetObs_LoadBURP: Found invalid code (%i)\n",c);
               c=0;
               break;
            }
         }

         /*if the elements where ok, add the dataset*/
         if (c) data=TMetElem_Insert(loc,dt,time,bfam,bktyp,bkstp,nelem,nval,nt,mkr?NULL:tblvalf,mkr?tblval:NULL,eb);
      }
   }

   Tcl_DecrRefCount(obj);
   if (elems)   free(elems);
   if (codes)   free(codes);
   if (eb)      free(eb);
   if (tblval)  free(tblval);
   if (tblvalf) free(tblvalf);
   free(buf);

   /*Close the file*/
   c_mrfcls(Obs->FId);
   c_fclos(Obs->FId);
   cs_fstunlockid(Obs->FId);

   Tcl_MutexUnlock(&MUTEX_BURPFILE);
   return(code);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_LoadASCII>
 * Creation     : Avril 2006 J.P. Gauthier
 *
 * But          : Chargement d'un fichier d'observations en format ASCII
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <File>     : Le nom du fichier
 *   <Obs>      : Observation
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int MetObs_LoadASCII(Tcl_Interp *Interp,char *File,TMetObs *Obs) {

   TMetLoc  *loc;

   FILE    *stream;
   char    buf[256];
   char    *bytes=NULL;
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
         gtime[n]=System_DateTime2Seconds(date,time,1);


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
               loc->Coord.Lat=atof(tok[n]);
            } else if (strcmp(gtok[n],"LON")==0) {           /*Longitude information*/
               loc->Coord.Lon=atof(tok[n]);
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
               loc->Coord.Elev=atof(tok[n]);
            } else if (strcmp(gtok[n],"NO")==0) {            /*Number information*/
               loc->No=strdup(tok[n]);
               strrep(loc->No,'(',' ');
               strrep(loc->No,')',' ');
            } else if (strcmp(gtok[n],"ID")==0) {            /*Identificateur*/
               /*Insert station in list if not already done*/
               loc=TMetLoc_Find(Obs,NULL,tok[n],MET_TYPEID);
               if (!loc) {
                  loc=TMetLoc_New(Obs,tok[n],NULL,0.0,0.0,0.0);
               }
            } else if (strncmp(gtok[n],"DATA",4)==0) {       /*Values*/
/*TODO
               data=TMetElem_Insert(loc,0,gtime[n],0x0,nelem,nt*nval);

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
               if (data->Code[e]->descriptor==Item->Code[0]) {
                  for(v=0;v<data->Nv;v++) {
//                     for(t=0;t<data->Nt;t++) {
                        val=MetObs_GetData(data,e,v,t);
                        if (MET_VALID(val,Obs->NoData)) {
                           min=min<val?min:val;
                           max=max>val?max:val;
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
 *----------------------------------------------------------------------------
*/

int MetObs_Render(Tcl_Interp *Interp,TMetObs *Obs,ViewportItem *VP,Projection *Proj,GLuint GLMode) {

   TMetLoc      *loc;
   TMetElem     *elem;
   TMetElemData *data,*cdata;
   TDataSpec    *spec;
   EntryTableB  *eb;
   Vect3d        pix;
   Coord         co;
   char          buf[128];
   double        z,val,valid,dir,dx,dy,k;
   int           d,e,i,n,v,t,iy,idx,line,id,ne,mk,box[4],b;
   double        alpha=1.0;
   int           clat,clon,cdir,nobs,min[2],max[2],skip,flag;

   extern void Data_RenderBarbule(int Type,int Flip,float Axis,float Lat,float Lon,float Elev,float Speed,float Dir,float Size,Projection *Proj);

   if (GLRender->Resolution>2) {
      return(0);
   }

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

   if (Interp)
      Tcl_AppendResult(Interp,"%% Postscript des observations meteorologiques\n",(char*)NULL);

  if (Obs->Model->Topo && strlen(Obs->Model->Topo)) {
      glEnable(GL_DEPTH_TEST);
      eb=MetObs_BUFRFindTableDesc(Obs->Model->Topo);
   } else {
      eb=NULL;
   }

   /*Initialize rendering parameters per model items*/
   min[0]=min[1]=max[0]=max[1]=0;n=0;
   for(i=0;i<Obs->Model->NItem;i++) {
      if ((spec=Obs->Model->Items[i].Spec)) {

         /*Get value limits*/
         if (isnan(spec->Min) || isnan(spec->Max))
            MetObs_GetStat(Obs,&Obs->Model->Items[i]);

         /*Keep model limits*/
         min[0]=FMIN(min[0],Obs->Model->Items[i].X);
         min[1]=FMIN(min[1],Obs->Model->Items[i].Y);
         max[0]=FMAX(max[0],Obs->Model->Items[i].X);
         max[1]=FMAX(max[1],Obs->Model->Items[i].Y);
         n=FMAX(n,Obs->Model->Items[i].Spec->Size);

         /*Define rendering parameters*/
         DataSpec_Intervals(spec,spec->Min,spec->Max);
         DataSpec_Define(spec);
      }
   }

   /*Add spacing for crowd coverage*/
   min[0]*=Obs->Model->Space;
   min[1]*=Obs->Model->Space;
   max[0]*=Obs->Model->Space;
   max[1]*=Obs->Model->Space;
   n>>=1;
   min[0]-=n;
   min[1]-=n;
   max[0]+=n;
   max[1]+=n;

   /*For all of the sations*/
   loc=Obs->Loc;
   n=-1;
   nobs=0;

   glPushName(PICK_METOBS);

   while(loc) {

      line=0;
      n++;

      if (GLMode==GL_SELECT) {
         if (!VP->ForcePick && (n && !(n%10)) && Tcl_DoOneEvent(TCL_WINDOW_EVENTS|TCL_DONT_WAIT)) {
            break;
         }
      }

      /*Check if visible or delay test for grouped data (loc->Grid)*/
      if (loc->Grid[0]!=0.0 || Projection_Pixel(Proj,VP,loc->Coord,pix)) {

         /*Get the elements group for the specific time*/
         if ((elem=TMetElem_Find(loc,Obs->Time,Obs->Lag))) {

            /*Fix transparency on validity time persistance*/
            if (Obs->Persistance) {
               alpha=1.0-((double)(Obs->Time-elem->Time)/Obs->Persistance);

               /*Continue with next location if this data is too old to be seen*/
               if (alpha<=0.0) {
                  loc=loc->Next;
                  continue;
               }
            }

            if (loc->Grid[0]==0.0 && Obs->Model->Overspace && !glCrowdPush(pix[0]+min[0],pix[1]+min[1],pix[0]+max[0],pix[1]+max[1],Obs->Model->Overspace)) {
               loc=loc->Next;
               continue;
            }

            /*For grouped data, find location record indexes*/
            clat=clon=-1;
            if (loc->Grid[0]!=0.0 && loc->Grid[1]!=0.0) {
               for(d=0;d<elem->NData;d++) {
                  cdata=elem->EData[d];
                  for(e=0;e<cdata->Ne;e++) {
                     if (cdata->Code[e]->descriptor==5002 || cdata->Code[e]->descriptor==5001)
                        clat=e;
                     else if (cdata->Code[e]->descriptor==6002 || cdata->Code[e]->descriptor==6001)
                        clon=e;
                  }
                  if (clat!=-1 && clon!=-1) {
                     break;
                  }
               }
            }

            glPushName(n);

            /*Get station height*/
            z=Data_Level2Meter(loc->Level,loc->Coord.Elev);

            /*Loop on the model items*/
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

               /*Loop on the data elements*/
               for(d=0;d<elem->NData;d++) {
                  data=elem->EData[d];

                  cdir=-1;
                  if (spec->RenderVector) {
                     for(e=0;e<data->Ne;e++) {
                        if (data->Code[e]->descriptor==Obs->Model->Items[i].Code[1]) {
                           cdir=e;
                           break;
                        }
                     }
                  }

                  /*Check for data family matching (bit 3-5, 000=new,001=corrected,010=repeat,011=human corrected,100=reserved*/
                  flag=(data->Family&0x7)==0?data->Family|0x20:data->Family;
                  if (Obs->Family && ((Obs->FamilyOp=='O' && !(Obs->Family&flag)) || (Obs->FamilyOp=='A' && !(Obs->Family==flag)))) {
                      continue;
                  }

                  /*Check for data bktyp matching*/
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

                              /*Check markers*/
                              if (Obs->Marker) {
                                 mk=MetObs_GetMarker(data,e,v,t);
                                 if ((Obs->MarkerOp=='O' && !(mk&Obs->Marker)) || (Obs->MarkerOp=='A' && !(mk==Obs->Marker))) {
                                    continue;
                                 }
                              }

                              /*Check for validity*/
                              val=MetObs_GetData(data,e,v,t);
                              valid=MET_VALID(val,Obs->NoData);

                              if (valid) {
                                 if (val<spec->Min || val>spec->Max)
                                    continue;
                                 if (spec->InterNb && val<spec->Inter[0])
                                    continue;
                              }

                              /*Get height if specified*/
                              if (eb && (k=TMetElem_Height(data,eb->descriptor,ne,v,0))!=-999.0) {
                                 z=k;
                              }

                              /*Check coordinates for grouped data*/
                              if (clat!=-1 && clon!=-1) {
                                 co.Lat=MetObs_GetData(cdata,clat,0,t);
                                 co.Lon=MetObs_GetData(cdata,clon,0,t);
                                 co.Elev=z;

                                 if (!Projection_Pixel(Proj,VP,co,pix)) {
                                    continue;
                                 }
                                 if (Obs->Model->Overspace && !glCrowdPush(pix[0]+min[0],pix[1]+min[1],pix[0]+max[0],pix[1]+max[1],Obs->Model->Overspace)) {
                                    continue;
                                 }
                              }

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

                              /*Set position within projection*/
                              glPushMatrix();

                              if (Obs->Model->Flat) {
                                 if (loc->Pix[0]!=0.0 && loc->Pix[1]!=0.0) {
                                    glTranslated(loc->Pix[0],VPY(VP,loc->Pix[1]),0.0);
                                 } else {
                                    glTranslated(pix[0],pix[1],0.0);
                                 }
                              } else {
                                 if (clat!=-1 && clon!=-1) {
                                    Proj->Type->Locate(Proj,co.Lat,co.Lon,1);
                                 } else {
                                    Proj->Type->Locate(Proj,loc->Coord.Lat,loc->Coord.Lon,1);
                                 }
                                 glTranslated(0.0,0.0,ZM(Proj,z));
                                 glScalef(VP->Ratio,VP->Ratio,1.0);
                              }

                              dx=Obs->Model->Items[i].X*Obs->Model->Space+(Obs->Model->Flat?0:loc->Pix[0]);
                              dy=-Obs->Model->Items[i].Y*Obs->Model->Space+(Obs->Model->Flat?0:-loc->Pix[1]);

                              /*Draw the position line if needed*/
                              if ((loc->Pix[0]!=0.0 || loc->Pix[1]!=0.0) && !line) {
                                 if (Interp)
                                    glFeedbackInit(20,GL_2D);

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
                                    snprintf(buf,128,"(%.4f,%.4f)",loc->Coord.Lat,loc->Coord.Lon);
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
                                 /*Load up the symbols if not done yet*/
                                 if (!WMO_Symbol1) {
                                    WMO_Symbol1=Tk_GetFont(Interp,GLRender->TkWin,"-misc-weathersymbols-medium-r-normal--14-0-0-0-p-0-microsoft-symbol");
                                 }
                                  if (!WMO_Symbol2) {
                                    WMO_Symbol2=Tk_GetFont(Interp,GLRender->TkWin,"-macromedia-meteo_b-medium-r-normal--14-0-0-0-p-0-microsoft-symbol");
                                 }
                                 if (spec->WMO==1) { /*AUTO*/
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
                                 } else if (spec->WMO==2) { /*N*/
                                    if (val>=0 && val<=9) {
                                       spec->Font=WMO_Symbol1;
                                       Tk_GetFontMetrics(spec->Font,&spec->TKM);
                                       sprintf(buf,"%c",(int)val+40);
                                       MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                                    }
                                 } else if (spec->WMO==3) { /*WW*/
                                    if (val>=4 && val<=99) {
                                       spec->Font=WMO_Symbol1;
                                       Tk_GetFontMetrics(spec->Font,&spec->TKM);
                                       sprintf(buf,"%c",(int)val+100);
                                       MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                                    }
                                 } else if (spec->WMO==4) { /*CL*/
                                    if (val>=1 && val<=9) {
                                       spec->Font=WMO_Symbol2;
                                       Tk_GetFontMetrics(spec->Font,&spec->TKM);
                                       sprintf(buf,"%c",(int)val+34);
                                       MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                                    }
                                 } else if (spec->WMO==5) { /*CM*/
                                    if (val>=1 && val<=9) {
                                       spec->Font=WMO_Symbol2;
                                       Tk_GetFontMetrics(spec->Font,&spec->TKM);
                                       sprintf(buf,"%c",(int)val+44);
                                       MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                                    }
                                 } else if (spec->WMO==6) { /*CH*/
                                    if (val>=1 && val<=9) {
                                       spec->Font=WMO_Symbol2;
                                       Tk_GetFontMetrics(spec->Font,&spec->TKM);
                                       sprintf(buf,"%c",(int)val+55);
                                       MetObs_RenderInfo(Interp,spec,buf,VP,Proj,iy--,pix[0]+dx,pix[1]+dy);
                                    }
                                 } else if (spec->WMO==7) { /*A*/
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

                              glPopMatrix();

                              /*If we're dynamic, only show first of grouped data*/
                              if (clat && GLRender->Resolution!=1) {
                                 break;
                              }
                           }

                           /*If this is no grouped data, skip the rest if no height is specified, they'll all be overlapped anyway*/
                           if (clat==-1 && !eb) {
                              break;
                           }

                        }
                        /*TODO break if grouped data, until we can select the variables (ex:Per channel)*/
                        if (data->Nt>1)
                           break;
                     }
                     /*Skip the rest if no height is specified, and one has been drawn, they'll all be overlapped anyway*/
                     if (ne>=0 && (!eb || Proj->Scale==1.0)) {
                        break;
                     }
                  }
               }
               glPopName();
            }

            /*It is possible nothing was drawn so remove from crowd list if so*/
            if (Obs->Model->Overspace && skip) {
               glCrowdPop();
            };

            glPopName();
         }
      }
      loc=loc->Next;
   }


   if (Obs->Model->Flat) {
      glPopMatrix();
      glMatrixMode(GL_PROJECTION);
      glPopMatrix();
   }
   glPopName();
   glEnable(GL_CULL_FACE);
   glDisable(GL_BLEND);
   glDisable(GL_DEPTH_TEST);
   glDisableClientState(GL_VERTEX_ARRAY);
   glDisable(GL_DEPTH_TEST);

   if (GLRender->GLDebug)
      fprintf(stdout,"(DEBUG) MetObs_Render: Nb Loc=%i NbObs=%i\n",n,nobs);

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

   static CONST char *sopt[] = { "-FAMILY","-TYPE","-STYPE","-ELEMENT","-DESC","-UNIT","-CODE","-VALUE",NULL };
   enum                opt { FAMILY,TYPE,STYPE,ELEMENT,DESC,UNIT,CODE,VALUE };

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
