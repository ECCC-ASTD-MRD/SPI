/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers d'observations
 * Fichier      : tclMetModel.c
 * Creation     : Avril 2006 - J.P. Gauthier
 *
 * Description  : Fichier d'entete du module de modele de pointage.
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

static Tcl_HashTable MetModelTable;
static int MetModelInit=0;

static int MetModel_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int MetModel_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclMetModel_Init>
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
int TclMetModel_Init(Tcl_Interp *Interp) {

   if (!MetModelInit++) {
      Tcl_InitHashTable(&MetModelTable,TCL_STRING_KEYS);
   }
   Tcl_CreateObjCommand(Interp,"metmodel",MetModel_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetModel_Cmd>
 * Creation     : Avril 2006 J.P. Gauthier
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
static int MetModel_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   TMetModel   *mdl;
   TDataSpec   *spec;
   EntryTableB *eb;

   int         idx,j;
   static CONST char *sopt[] = { "create","free","configure","define","is","all","wipe",NULL };
   enum               opt { CREATE,FREE,CONFIGURE,DEFINE,IS,ALL,WIPE };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return TCL_ERROR;
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return TCL_ERROR;
   }

   switch ((enum opt)idx) {
      case CREATE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return TCL_ERROR;
         }
         return MetModel_Create(Interp,Tcl_GetString(Objv[2]));
         break;


      case FREE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return TCL_ERROR;
         }
         return MetModel_FreeHash(Interp,Tcl_GetString(Objv[2]));
         break;

      case CONFIGURE:
         if(Objc<4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id elem ?option?");
            return TCL_ERROR;
         }
         mdl=MetModel_Get(Tcl_GetString(Objv[2]));
         if (!mdl) {
            Tcl_AppendResult(Interp,"MetModel_Cmd: invalid model",(char*)NULL);
            return TCL_ERROR;
         }

         if (!(eb=MetObs_BUFRFindTableCodeOrDesc(Interp,Objv[3]))) {
            Tcl_AppendResult(Interp,"\n   MetModel_Cmd: Wrong element",(char*)NULL);
            return(TCL_ERROR);
         }
         for(j=0;j<mdl->NItem;j++) {
            if (mdl->Items[j].Code[0]==eb->descriptor) {
               if (strcmp(Tcl_GetString(Objv[4]),"-dataspec")==0) {
                  if (Objc==5) {
                     if (mdl->Items[j].Spec) {
                        mdl->Items[j].Spec->NRef++;
                        Tcl_SetObjResult(Interp,Tcl_NewStringObj(mdl->Items[j].Spec->Name,-1));
                     }
                  } else {
                     if ((spec=DataSpec_Get(Tcl_GetString(Objv[5])))) {
                       if (mdl->Items[j].Spec) {
                          DataSpec_FreeHash(Interp,mdl->Items[j].Spec->Name);
                        }
                        mdl->Items[j].Spec=spec;
                        spec->NRef++;
                     } else {
                        Tcl_AppendResult(Interp,"MetModel_Cmd: invalid configuration object",(char*)NULL);
                        return(TCL_ERROR);
                     }
                  }
               } else {
                  if (!mdl->Items[j].Spec) {
                     Tcl_AppendResult(Interp,"MetModel_Cmd: Data specification not defined",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  if (DataSpec_Config(Interp,mdl->Items[j].Spec,Objc-4,Objv+4)==TCL_ERROR) {
                     return(TCL_ERROR);
                  }
               }
            }
         }
         break;

      case DEFINE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id ?option?");
            return TCL_ERROR;
         }
         return MetModel_Define(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3);
         break;

      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return TCL_ERROR;
         }
         if (MetModel_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case ALL:
         TclY_HashAll(Interp,&MetModelTable);
         break;

      case WIPE:
         TclY_HashWipe(&MetModelTable,(TclY_HashFreeEntryDataFunc*)MetModel_Free);
         break;
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <MetModel_Define>
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
static int MetModel_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj     *obj,*sub;
   TMetModel   *mdl;
   EntryTableB *eb;
   int       i,j,n,d,k,idx;

   static CONST char *sopt[] = { "-items","-spacing","-flat","-topography","-overspace",NULL };
   enum                opt { ITEMS,SPACING,FLAT,TOPOGRAPHY,OVERSPACE };

   mdl=MetModel_Get(Name);
   if (!mdl) {
      Tcl_AppendResult(Interp,"\n   MetModel_Define: Model id unknown: \"",Name,"\"",(char*)NULL);
      return TCL_ERROR;
   }

   for (i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case ITEMS:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for(j=0;j<mdl->NItem;j++) {
                  sub=Tcl_NewListObj(0,NULL);
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(mdl->Items[j].X));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(mdl->Items[j].Y));
                  Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(mdl->Items[j].Code[0]));
                  if (mdl->Items[j].Code[1]) {
                     Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(mdl->Items[j].Code[1]));
                  }
                  Tcl_ListObjAppendElement(Interp,obj,sub);
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (mdl->Items) {
                  for (n=0;n<mdl->NItem;n++){
                     DataSpec_Free(mdl->Items[n].Spec);
                  }
                  free(mdl->Items);
                  mdl->Items=NULL;
                  mdl->NItem=0;
               }
               Tcl_ListObjLength(Interp,Objv[++i],&j);
               if (j) {
                  mdl->Items=(TMetModelItem*)malloc(j*sizeof(TMetModelItem));
                  mdl->NItem=0;

                  for(k=0;k<j;k++) {
                     Tcl_ListObjIndex(Interp,Objv[i],k,&sub);
                     Tcl_ListObjLength(Interp,sub,&d);

                     if (d<3) {
                        Tcl_AppendResult(Interp,"\n   MetModel_Define: Wrong number of parameters within model item, must be \"x y [code ..]\"",(char*)NULL);
                        return(TCL_ERROR);
                     } else {
                        Tcl_ListObjIndex(Interp,sub,0,&obj);
                        Tcl_GetIntFromObj(Interp,obj,&mdl->Items[mdl->NItem].X);
                        Tcl_ListObjIndex(Interp,sub,1,&obj);
                        Tcl_GetIntFromObj(Interp,obj,&mdl->Items[mdl->NItem].Y);
                        Tcl_ListObjIndex(Interp,sub,2,&obj);

                        if (!(eb=MetObs_BUFRFindTableCodeOrDesc(Interp,obj))) {
                           Tcl_AppendResult(Interp,"\n   MetModel_Define: Wrong element",(char*)NULL);
                           return(TCL_ERROR);
                        }
                        mdl->Items[mdl->NItem].Code[0]=eb->descriptor;
                        if (d==4) {
                           Tcl_ListObjIndex(Interp,sub,3,&obj);
                           if (!(eb=MetObs_BUFRFindTableCodeOrDesc(Interp,obj))) {
                              mdl->Items[mdl->NItem].Code[1]=0;
                           } else {
                              mdl->Items[mdl->NItem].Code[1]=eb->descriptor;
                           }
                        } else {
                           mdl->Items[mdl->NItem].Code[1]=0;
                        }
                        mdl->Items[mdl->NItem].Spec=NULL;
                        mdl->NItem++;
                     }
                  }
               }
            }
            break;

         case SPACING:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(mdl->Space));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&mdl->Space);
            }
            break;

         case FLAT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(mdl->Flat));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&mdl->Flat);
            }
            break;

         case OVERSPACE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(mdl->Overspace));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&mdl->Overspace);
            }
            break;

         case TOPOGRAPHY:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(mdl->Topo));
            } else {
               ++i;
               if (!strlen(Tcl_GetString(Objv[i]))) {
                  mdl->Topo=0;
               } else {
                  if (!(eb=MetObs_BUFRFindTableCodeOrDesc(Interp,Objv[i]))) {
                  Tcl_AppendResult(Interp,"\n   MetModel_Define: Wrong element",(char*)NULL);
                  return(TCL_ERROR);
                  }
                  if (!mdl->Topo || mdl->Topo!=eb->descriptor) {
                     mdl->Topo=eb->descriptor;
                  }
               }
            }
            break;
      }
   }
   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetModel_Create>
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
int MetModel_Create(Tcl_Interp *Interp,char *Name) {

   TMetModel *mdl;

   if (!(mdl=(TMetModel*)TclY_HashPut(Interp,&MetModelTable,Name,sizeof(TMetModel)))) {
      return(TCL_ERROR);
   }

   mdl->NItem    = 0;
   mdl->NRef     = 1;
   mdl->Space    = 25;
   mdl->Flat     = 1;
   mdl->Overspace= 0;
   mdl->Topo     = 0;
   mdl->Items    = NULL;
   mdl->Name     = strdup(Name);

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetModel_Get>
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
TMetModel* MetModel_Get(char *Name) {
   return((TMetModel*)TclY_HashGet(&MetModelTable,Name));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetModel_FreeHash>
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
int MetModel_FreeHash(Tcl_Interp *Interp,char *Name) {

   Tcl_HashEntry *entry;

   entry=TclY_FindHashEntry(&MetModelTable,Name);

   if(!entry) {
      Tcl_AppendResult(Interp,"\n   MetModel_FreeHash:  Observation name unknown: \"",Name,"\"",(char*)NULL);
      return TCL_ERROR;
   } else {
      if (MetModel_Free((TMetModel*)Tcl_GetHashValue(entry))) {
         TclY_DeleteHashEntry(entry);
      }
   }
   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetModels_Free>
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
int MetModel_Free(TMetModel *Model) {

   int n;

   if (--Model->NRef) {
      return(0);
   }

   if (Model->Items) {
      for (n=0;n<Model->NItem;n++){
         DataSpec_Free(Model->Items[n].Spec);
      }
      free(Model->Items);
   }

   free(Model->Name);
   free(Model);

  return(1);
}
