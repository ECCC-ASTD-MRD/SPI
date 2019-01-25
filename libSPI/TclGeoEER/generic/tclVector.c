/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers d'observations
 * Fichier      : tclVector.c
 * Creation     : Fevrier 2005 - J.P. Gauthier
 *
 * Description  : Fichier d'entete du module Vector.
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

#include "tclVector.h"
#include "tclData.h"

static Tcl_HashTable VectorTable;
static TVectorSpec VectorSpec;
static int VectorInit=0;

static int Vector_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int Vector_Spec(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int Vector_Stat(Tcl_Interp *Interp,TVector *Vec,int Objc,Tcl_Obj *CONST Objv[]);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclVector_Init>
 * Creation     : Fevrier 2005 J.P. Gauthier
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
int TclVector_Init(Tcl_Interp *Interp) {

   if (!VectorInit++) {
      Tcl_InitHashTable(&VectorTable,TCL_STRING_KEYS);
   }
   VectorSpec.Alloc=0;

   Tcl_CreateObjCommand(Interp,"vector",Vector_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_Cmd>
 * Creation      : Fevrier 2005 J.P. Gauthier
 *
 * But          : Effectuer les commandes du package
 *
 * Parametres    :
 *   <clientData>: Nom du vecteur
 *   <Interp>    : Interpreteur Tcl
 *   <Objc>      : Nombre d'arguments
 *   <Objv>      : Pointeur sur la liste des arguments (word)
 *
 * Retour        : Code de retour standard TCL
 *
 * Remarques     :
 *
 *---------------------------------------------------------------------------------------------------------------
*/

static int Vector_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   Tcl_Obj     *obj,*lst;
   TVector     *vec;
   int          idx,n,nobj,ns,e=0;
   char        *c,*cv;

   static CONST char *sopt[] = { "create","clear","dim","mem","set","append","get","copy","free","stats","length","sort","is","all","specification","wipe",NULL };
   enum               opt { CREATE,CLEAR,DIM,MEM,SET,APPEND,GET,COPY,FREE,STATS,LENGTH,SORT,IS,ALL,SPEC,WIPE };

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
         if (Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"vector [data]");
            return TCL_ERROR;
         }

         lst=NULL;
         /*Test for composant on data specification*/
         if (Objc==4) {
            Tcl_ListObjLength(Interp,Objv[3],&nobj);
            Tcl_ListObjIndex(Interp,Objv[3],0,&obj);
            Tcl_ListObjLength(Interp,obj,&ns);
            if (ns>1) {
               lst=Tcl_NewListObj(0,NULL);
               for(n=0;n<nobj;n++) {
                  Tcl_ListObjAppendElement(Interp,lst,Tcl_NewIntObj(n));
               }
            }
         }

         if (Vector_Create(Interp,Tcl_GetString(Objv[2]),lst)==TCL_ERROR) {
            return(TCL_ERROR);
         }
         if (Objc==4) {
            vec=Vector_Get(Tcl_GetString(Objv[2]));
            return(Vector_SetData(Interp,vec,Objv[3],-1));
         }
         break;

      case CLEAR:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"vector");
            return TCL_ERROR;
         }
         vec=Vector_Get(Tcl_GetString(Objv[2]));
         if (!vec) {
            Tcl_AppendResult(Interp,"Invalid vector",(char*)NULL);
            return(TCL_ERROR);
         } else {
            Vector_Clear(Interp,vec);
            Tcl_SetObjResult(Interp,Tcl_NewIntObj(vec->Nr));
         }
         break;

      case DIM:
         if(Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"vector [dims]");
            return(TCL_ERROR);
         }
         if(Objc==3) {
            vec=Vector_Get(Tcl_GetString(Objv[2]));
            if (!vec) {
               Tcl_AppendResult(Interp,"Invalid vector",(char*)NULL);
               return(TCL_ERROR);
            } else {
               Tcl_SetObjResult(Interp,vec->Cn);
            }
         } else {
            return(Vector_Create(Interp,Tcl_GetString(Objv[2]),Objv[3]));
         }
         break;

      case LENGTH:
         if(Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"vector [length]");
            return(TCL_ERROR);
         }
         vec=Vector_Get(Tcl_GetString(Objv[2]));
         if (!vec) {
            Tcl_AppendResult(Interp,"Invalid vector",(char*)NULL);
            return(TCL_ERROR);
         } else {
            n=-1;
            if(Objc==4) {
               Tcl_GetIntFromObj(Interp,Objv[3],&n);
            }
            Tcl_SetObjResult(Interp,Tcl_NewIntObj(Vector_Length(Interp,vec,n)));
         }
         break;

      case MEM:
         if(Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"vector [nb]");
            return(TCL_ERROR);
         }
         vec=Vector_Get(Tcl_GetString(Objv[2]));
         if (!vec) {
            Tcl_AppendResult(Interp,"Invalid vector",(char*)NULL);
            return(TCL_ERROR);
         } else {
            n=-1;
            if(Objc==4) {
               Tcl_GetIntFromObj(Interp,Objv[3],&n);
            }
            Tcl_SetObjResult(Interp,Tcl_NewIntObj(Vector_Mem(Interp,vec,n)));
         }
         break;

      case APPEND:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"vector data");
            return(TCL_ERROR);
         }
         vec=Vector_Get(Tcl_GetString(Objv[2]));
         if (!vec) {
            Tcl_AppendResult(Interp,"Invalid vector",(char*)NULL);
            return(TCL_ERROR);
         } else {
            return Vector_AppendData(Interp,vec,Objv[3],0.0);
         }
         break;

      case SET:

         if(Objc!=4 && Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"vector data [idx]");
            return(TCL_ERROR);
         }
         cv=strdup(Tcl_GetString(Objv[2]));
         n=-1;
         if (Objc==4) {
            n=strrindex(cv);
         }
         vec=Vector_Get(cv);
         free(cv);

         if (!vec) {
            Tcl_AppendResult(Interp,"Invalid vector",(char*)NULL);
            return(TCL_ERROR);
         } else {
            if(Objc==5) {
               c=Tcl_GetString(Objv[4]);
               if (c[0]=='e' && c[1]=='n' && c[2]=='d') {
                  c=&c[3];
                  e=1;
               }
               if (c[0]!='\0') {
                  Tcl_GetInt(Interp,c,&n);
               }
               if (e) n=vec->N-n-1;
            }
            return(Vector_SetData(Interp,vec,Objv[3],n));
         }
         break;

      case GET:
         if(Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"vector [idx]");
            return(TCL_ERROR);
         }
         cv=strdup(Tcl_GetString(Objv[2]));
         n=-1;
         if (Objc==3) {
            n=strrindex(cv);
         }
         vec=Vector_Get(cv);
         free(cv);

         if (!vec) {
            Tcl_AppendResult(Interp,"Invalid vector",(char*)NULL);
            return(TCL_ERROR);
         } else {
            if (Objc==4) {
               n=0;
               c=Tcl_GetString(Objv[3]);
               if (c[0]=='e' && c[1]=='n' && c[2]=='d') {
                  c=&c[3];
                  e=1;
               }
               if (c[0]!='\0') {
                  Tcl_GetInt(Interp,c,&n);
               }
               if (e) n=((vec->Cp&&vec->Cp[0])?vec->Cp[0]->N-n:vec->N-n)-1;
            }
            obj=Vector_GetData(Interp,vec,n,0);
            if (obj) {
               Tcl_SetObjResult(Interp,obj);
               return(TCL_OK);
            } else {
               return(TCL_ERROR);
            }
         }
         break;

      case STATS:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"vector ?option?");
            return(TCL_ERROR);
         }
         vec=Vector_Get(Tcl_GetString(Objv[2]));
         if (!vec) {
            Tcl_AppendResult(Interp,"Invalid vector",(char*)NULL);
            return(TCL_ERROR);
         } else {
            return Vector_Stat(Interp,vec,Objc-3,Objv+3);
         }
         break;

      case COPY:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"vectorto vectorfrom");
            return TCL_ERROR;
         }
         if (!Vector_Copy(Interp,Vector_Get(Tcl_GetString(Objv[3])),Tcl_GetString(Objv[2]))) {
            return(TCL_ERROR);
         } else {
            return(TCL_OK);
         }
         break;

      case FREE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"vector");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            Vector_Destroy(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case SPEC:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"?option?");
            return(TCL_ERROR);
         }
         return Vector_Spec(Interp,Tcl_GetString(Objv[2]),Objc-2,Objv+2);
         break;

      case SORT:
         if(Objc!=3 && Objc!=4 && Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"[-unique] vector [dim]");
            return(TCL_ERROR);
         }

         c=NULL;
         if (!(vec=Vector_Get(Tcl_GetString(Objv[2])))) {
            if (strcmp(Tcl_GetString(Objv[2]),"-unique")==0) {
               e=1;
               vec=Vector_Get(Tcl_GetString(Objv[3]));
               if (Objc==5) {
                 c=Tcl_GetString(Objv[4]);
               }
            } else {
               Tcl_AppendResult(Interp,"Invalid option",(char*)NULL);
               return(TCL_ERROR);
            }
         } else {
            if (Objc==4) {
               c=Tcl_GetString(Objv[3]);
            }
         }
         if (!vec) {
            Tcl_AppendResult(Interp,"Invalid vector",(char*)NULL);
            return(TCL_ERROR);
         } else {
            return(Vector_Sort(Interp,vec,c,e));
         }
         break;

      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"vector");
            return(TCL_ERROR);
         }
         if (Vector_Get(Tcl_GetString(Objv[2]))) {
             Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case ALL:
         TclY_HashAll(Interp,&VectorTable);
         break;

      case WIPE:
         TclY_HashWipe(&VectorTable,(TclY_HashFreeEntryDataFunc*)Vector_Free);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Vector_Spec>
 * Creation : Mars 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Specification des vecteurs.
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
static int Vector_Spec(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   int i,idx;

   static CONST char *sopt[] = { "-alloc",NULL };
   enum                opt { ALLOC };


   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case ALLOC:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(VectorSpec.Alloc));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&VectorSpec.Alloc);
               VectorSpec.Alloc=VectorSpec.Alloc<=1.0?1.0:VectorSpec.Alloc;
            }
            break;

      }
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <Vector_Stat>
 * Creation : Mars 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer des stats sur le vecteur.
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

static int Vector_Stat(Tcl_Interp *Interp,TVector *Vec,int Objc,Tcl_Obj *CONST Objv[]){

   int    i,idx,v;
   double val;

   static CONST char *sopt[] = { "-nodata","-max","-min","-avg",NULL };
   enum        opt {  NODATA,MAX,MIN,AVG };

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {

         case NODATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Vec->NoData));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Vec->NoData);
               if (Vec->Cp) {
                  for(v=0;v<Vec->N;v++) {
                     Vec->Cp[v]->NoData=Vec->NoData;
                  }
               }
            }
            break;

         case MAX:
            if (Objc==1) {
               VECTORMAX(Vec,val);
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(val));
            }
            break;

         case MIN:
            if (Objc==1) {
               VECTORMIN(Vec,val);
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(val));
            }
            break;

         case AVG:
            if (Objc==1) {
               val=0;
               for(v=0;v<Vec->N;v++) {
                  val+=Vec->V[v];
               }
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(val/Vec->N));
            }
            break;
      }
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_Create>
 * Creation     : Fevrier 2005 J.P. Gauthier
 *
 * But          : Creation d'un objet vecteur et insertion d'un nouveau nom dans la table.
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
int Vector_Create(Tcl_Interp *Interp,char *Name,Tcl_Obj *Comp) {

   Tcl_HashEntry *entry;
   Tcl_Obj       *obj;
   TVector       *vec;
   int            new,i;
   char           buf[256];

   entry=TclY_CreateHashEntry(&VectorTable,Name,&new);

   if (!new) {
      vec=(TVector*)Tcl_GetHashValue(entry);
      if (vec->V || vec->Cn) {
         Tcl_AppendResult(Interp,"\n   Vector_Create: Name already used: \"",Name,"\"",(char*)NULL);
         return(TCL_ERROR);
      }
   } else {
      if ((vec=(TVector*)malloc(sizeof(TVector)))) {
         vec->NoData=nan("NaN");
         vec->Nr=vec->N=0;
         vec->V=NULL;
         vec->Cn=NULL;
         vec->Cp=NULL;
         vec->Def=NULL;
      }
  }

   if (!vec){
      Tcl_AppendResult(Interp,"\n   Vector_Create: Could not allocate memory",(char*)NULL);
      return(TCL_ERROR);
   }

   /* Create the component vectors*/
   if (Comp) {
      Tcl_ListObjLength(Interp,Comp,&vec->N);
      Tcl_IncrRefCount(Comp);
      vec->Cn=Comp;
      if ((vec->Cp=(TVector**)malloc(vec->N*sizeof(TVector*)))) {

         for(i=0;i<vec->N;i++) {
            Tcl_ListObjIndex(Interp,vec->Cn,i,&obj);
            sprintf(buf,"%s.%s",Name,Tcl_GetString(obj));
            if (Vector_Create(Interp,buf,NULL)==TCL_ERROR) {
               return(TCL_ERROR);
            }
            vec->Cp[i]=Vector_Get(buf);
            vec->Cp[i]->NoData=vec->NoData;
         }
      } else {
         Tcl_AppendResult(Interp,"\n Vector_Create: Could not allocate memory for components",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   Tcl_SetHashValue(entry,vec);

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_Copy>
 * Creation     : Aout 2005 J.P. Gauthier
 *
 * But          : Copy d'un objet vector.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Vec>      : Vecteur a copier
 *   <Name>     : Nom du vecteur a creer
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
TVector *Vector_Copy(Tcl_Interp *Interp,TVector *Vec,char *Name) {

   Tcl_Obj *obj;
   TVector *new;
   int      i;
   char     buf[256];

   if ((new=Vector_Get(Name))) {
      if (new!=Vec) {
         Vector_Destroy(Interp,Name);
      } else {
         return(new);
      }
   }
        
   if (Vector_Create(Interp,Name,NULL)!=TCL_OK) {
       return(NULL);
   }

   new=Vector_Get(Name);
   new->N=new->Nr=Vec->N;
   new->NoData=Vec->NoData;

   if (Vec->Cp) {
      /*Copie des composantes recursivement*/

      if ((new->Cp=(TVector**)malloc(Vec->N*sizeof(TVector*)))) {
         new->Cn=Vec->Cn;
         Tcl_IncrRefCount(Vec->Cn);

         for(i=0;i<new->N;i++) {
            Tcl_ListObjIndex(Interp,new->Cn,i,&obj);
            sprintf(buf,"%s.%s",Name,Tcl_GetString(obj));
            if (!Vector_Copy(Interp,Vec->Cp[i],buf)) {
               return(NULL);
            }
            new->Cp[i]=Vector_Get(buf);
         }
      } else {
         return(NULL);
      }
   } else {
      /*Copie des donnees*/
      if ((new->V=(double*)malloc(new->N*sizeof(double)))) {
         memcpy(new->V,Vec->V,new->N*sizeof(double));
      } else {
         return(NULL);
      }
   }

   if (Vec->Def)
      Vector_GetDef(new);

   return(new);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_GetDef>
 * Creation     : Aout 2005 J.P. Gauthier
 *
 * But          : Retourner la representation TDef du vecteur.
 *
 * Parametres   :
 *   <Vec>      : Vecteur a copier
 *
 * Retour       : Representation TDef
 *
 * Remarques    : On initialise la representation si elle n'existe pas
 *
 *---------------------------------------------------------------------------------------------------------------
*/
struct TDef* Vector_GetDef(TVector *Vec) {

   int i;

   if (!Vec->Def) {
      Vec->Def=(struct TDef*)malloc(sizeof(struct TDef));
   }

   if (Vec->Def) {
      Vec->Def->NI=Vec->N;
      Vec->Def->NJ=1;
      Vec->Def->NK=1;
      Vec->Def->Data[0]=NULL;
      Vec->Def->Data[1]=NULL;
      Vec->Def->Data[2]=NULL;
      Vec->Def->Data[3]=NULL;
      Vec->Def->Type=TD_Float64;
      Vec->Def->NoData=Vec->NoData;
      Vec->Def->Idx=0;

      if (Vec->Cp) {
         Vec->Def->NJ=Vec->Cp[0]->N;
         Vec->Def->Data[0]=(char*)malloc(Vec->Def->NI*Vec->Def->NJ*sizeof(double));
         for(i=0;i<Vec->N;i++) {
            memcpy(&Vec->Def->Data[0][sizeof(double)*Vec->Def->NJ*i],Vec->Cp[i]->V,sizeof(double)*Vec->Def->NJ);
         }
      } else {
         Vec->Def->Data[0]=(char*)malloc(Vec->Def->NI*sizeof(double));
         memcpy(Vec->Def->Data[0],Vec->V,sizeof(double)*Vec->Def->NI);
      }
   }
   return(Vec->Def);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_Get>
 * Creation     : Fevrier 2005 J.P. Gauthier
 *
 * But          : Obtenir un objet vecteur en fonction de son nom dans la table de Tcl
 *
 * Parametres   :
 *   <Name>     : Nom de l'objet a obtenir.
 *
 * Retour       : Une structure TOVector ou un pointeur NULL si rien trouve.
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
TVector* Vector_Get(char *Name) {
   return((TVector*)TclY_HashGet(&VectorTable,Name));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_Destroy>
 * Creation     : Fevrier 2005 J.P. Gauthier
 *
 * But          : Destruction d'un vecteur a partir de son nom.
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
int Vector_Destroy(Tcl_Interp *Interp,char *Name) {

   Tcl_Obj       *obj;
   TVector       *vec=NULL;
   char           buf[256];
   int            i;

   if ((vec=(TVector*)TclY_HashDel(&VectorTable,Name))) {
      if (vec->Cn) {
         for(i=0;i<vec->N;i++) {
            Tcl_ListObjIndex(Interp,vec->Cn,i,&obj);
            sprintf(buf,"%s.%s",Name,Tcl_GetString(obj));
            Vector_Destroy(Interp,buf);
         }
         Tcl_DecrRefCount(vec->Cn);
         free(vec->Cp);
      }

      Vector_Free(vec);
      free(vec);
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_Free>
 * Creation     : Fevrier 2005 J.P. Gauthier
 *
 * But          : Suppression des valeur et de la memoire alloue du vecteur.
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
void Vector_Free(TVector *Vec) {

   if (Vec->V)   free(Vec->V);
   if (Vec->Def) {
      if (Vec->Def->Data[0]) free(Vec->Def->Data[0]);
      free(Vec->Def);
   }

   Vec->Nr=Vec->N=0;
   Vec->V=NULL;
   Vec->Def=NULL;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_GetData>
 * Creation     : Fevrier 2005 J.P. Gauthier
 *
 * But          : Recuperation des donnees d'un vecteur.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Vec>      : Vecteur
 *   <Idx>      : Index dans le vecteur
 *   <Sub>      : Sous-liste
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
Tcl_Obj* Vector_GetData(Tcl_Interp *Interp,TVector *Vec,int Idx,int Sub) {

   Tcl_Obj *obj=NULL;
   int      n;

   if (Vec->Cp) {
      obj=Tcl_NewListObj(0,NULL);
      for(n=0;n<Vec->N;n++) {
         Tcl_ListObjAppendElement(Interp,obj,Vector_GetData(Interp,Vec->Cp[n],Idx,1));
      }
   } else {
      if (Idx>=Vec->N) {
         obj=Tcl_NewListObj(0,NULL);
         Tcl_AppendResult(Interp,"Vector_GetData: Index value out of bound",(char*)NULL);
      } else {
         if (Idx>=0) {
            if (Vec->V[Idx]==Vec->NoData) {
               obj=Tcl_NewStringObj("-",-1);
            } else {
               obj=Tcl_NewDoubleObj(Vec->V[Idx]);
            }
         } else {
            obj=Tcl_NewListObj(0,NULL);
            for(n=0;n<Vec->N;n++) {
               if (Vec->V[n]==Vec->NoData) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
               } else {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Vec->V[n]));
               }
            }
         }
      }
   }
   return(obj);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_Lentgh>
 * Creation     : Fevrier 2005 J.P. Gauthier
 *
 * But          : Fixer la longueur d'un vecteur.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Vec>      : Vecteur
 *   <Len>      : Longueur des donnes
 *   <Mem>      : Longueur en memoire
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Vector_Length(Tcl_Interp *Interp,TVector *Vec,int Len) {

   int   i,n=0;

   if (!Vec) {
      Tcl_AppendResult(Interp,"Vector_Length: Invalid vector",(char*)NULL);
      return(-1);
   }

   if (Len==-1) {
      if (Vec->Cp) {
         if (!Vec->Cp[0]) {
            n=0;
         } else {
            n=Vector_Length(Interp,Vec->Cp[0],Len);
         }
      } else {
         n=Vec->N;
      }
   } else {
      if (Vec->Cp) {
         for(i=0;i<Vec->N;i++) {
            n=Vector_Length(Interp,Vec->Cp[i],Len);
         }
      } else {
         n=Vec->N=Len;
         if (Vec->N>Vec->Nr) {
            Vec->Nr=Vec->N;
            if (Vec->V) free(Vec->V);
            Vec->V=calloc(Vec->Nr,sizeof(double));

            if (!Vec->V) {
               Tcl_AppendResult(Interp,"Vector_Length: Unable to allocate memory",(char*)NULL);
               n=-1;
            }
         }
      }
   }
   return(n);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_Sort>
 * Creation     : Octobre 2005 J.P. Gauthier
 *
 * But          : Trier un vecteur.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Vec>      : Vecteur
 *   <Comp>     : Composante sur laquelle trie (NULL=aucune)
 *   <Unique>   : Supprimer les composantes dont l'index est le meme
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Vector_Swap(TVector *Vec,int IdxFrom,int IdxTo) {

   int    n;
   double v;

   if (Vec->Cp) {
      for(n=0;n<Vec->N;n++) {
         v=Vec->Cp[n]->V[IdxTo];
         Vec->Cp[n]->V[IdxTo]=Vec->Cp[n]->V[IdxFrom];
         Vec->Cp[n]->V[IdxFrom]=v;
       }
   } else {
      v=Vec->V[IdxTo];
      Vec->V[IdxTo]=Vec->V[IdxFrom];
      Vec->V[IdxFrom]=v;
   }
}

void Vector_QuickSort(TVector *Vec,int Comp,int start,int end) {

   int     l=start;
   int     r=end;
   double *v,p;

   v=Comp==-1?Vec->V:Vec->Cp[Comp]->V;

   if (end-start>=1) {

      p=v[start];

      while (r>l) {
         while (v[l]<=p && l<=end && r>l)
            l++;
         while (v[r]>p && r>=start && r>=l)
            r--;
         if (r>l)
            Vector_Swap(Vec,l,r);
      }
      Vector_Swap(Vec,start,r);

      Vector_QuickSort(Vec,Comp,start,r-1);
      Vector_QuickSort(Vec,Comp,r+1,end);
   } else {
      return;
   }
}

int Vector_Sort(Tcl_Interp *Interp,TVector *Vec,char *Comp,int Unique) {

   int      n,end,c,cn,d=0;
   Tcl_Obj *obj;

   if (!Vec) {
      Tcl_AppendResult(Interp,"Vector_Sort: Invalid vector",(char*)NULL);
      return(TCL_ERROR);
   }

   c=Vec->Cp?0:-1;
   end=Vec->Cp?Vec->Cp[0]->N:Vec->N;

   /*If a component is specified, find it*/
   if (Comp) {
      for(n=0;n<Vec->N;n++) {
         Tcl_ListObjIndex(Interp,Vec->Cn,n,&obj);
         if (strcmp(Tcl_GetString(obj),Comp)==0) {
            c=n;
            end=Vec->Cp[n]->N;
            break;
         }
      }
      if (n==Vec->N) {
         Tcl_AppendResult(Interp,"Vector_Sort: Invalid component",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   /*Sort on the specific component*/
   Vector_QuickSort(Vec,c,0,end-1);

   /*Check for repeted element*/
   if (Unique) {
      if (Comp) {
         for(n=0;n<Vec->Cp[c]->N-1-d;n++) {
            if (Vec->Cp[c]->V[n]==Vec->Cp[c]->V[n+1]) {
               for(cn=0;cn<Vec->N;cn++) {
                  memcpy(&Vec->Cp[cn]->V[n+1],&Vec->Cp[cn]->V[n+2],(Vec->Cp[cn]->N-(n+1))*sizeof(double));
                  Vec->Cp[cn]->N--;
               }
               n--;
               d++;
            }
         }
      } else {
         for(n=0;n<Vec->N-1-d;n++) {
            if (Vec->V[n]==Vec->V[n+1]) {
               memcpy(&Vec->V[n+1],&Vec->V[n+2],(Vec->N-(n+1))*sizeof(double));
               n--;
               d++;
            }
            Vec->N--;
         }
      }
   }

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_Clear>
 * Creation     : Aout 2005 J.P. Gauthier
 *
 * But          : Clear du vecteur en reinitialisant le compte a 0.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Vec>      : Vecteur
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Vector_Clear(Tcl_Interp *Interp,TVector *Vec) {

   int   i;

   if (!Vec) {
      Tcl_AppendResult(Interp,"Vector_Clear: Invalid vector",(char*)NULL);
   }

   if (Vec->Cp) {
      for(i=0;i<Vec->N;i++) {
         Vector_Clear(Interp,Vec->Cp[i]);
      }
   } else {
      Vec->N=0;
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_Mem>
 * Creation     : Fevrier 2005 J.P. Gauthier
 *
 * But          : Fixer l'espcae memoire d'un vecteur.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Vec>      : Vecteur
 *   <Mem>      : Longueur en memoire
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Vector_Mem(Tcl_Interp *Interp,TVector *Vec,int Mem) {

   int i,n=0;

   if (!Vec) {
      Tcl_AppendResult(Interp,"Vector_Length: Invalid vector",(char*)NULL);
      return(-1);
   }

   if (Mem==-1) {
      if (Vec->Cp) {
         if (!Vec->Cp[0]) {
            n=0;
         } else {
            n=Vector_Mem(Interp,Vec->Cp[0],Mem);
         }
      } else {
         n=Vec->Nr;
      }
   } else {
      if (Vec->Cp) {
         for(i=0;i<Vec->N;i++) {
            n=Vector_Mem(Interp,Vec->Cp[i],Mem);
         }
      } else {
         n=Vec->Nr=Mem;
         if (Vec->V) free(Vec->V);
         Vec->V=calloc(Vec->Nr,sizeof(double));

         if (!Vec->V) {
            Tcl_AppendResult(Interp,"Vector_Length: Unable to allocate memory",(char*)NULL);
            n=-1;
         }
      }
   }
   return(n);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_SetData>
 * Creation     : Fevrier 2005 J.P. Gauthier
 *
 * But          : Insertion de donnees dans un vecteur.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Vec>      : Vecteur
 *   <List>     : Liste des valeurs
 *   <Idx>      : Index de depart
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Vector_SetData(Tcl_Interp *Interp,TVector *Vec,Tcl_Obj *List,int Idx) {

   Tcl_Obj *obj,*sub;
   int      nobj,n;
   TVector *vec=NULL;

   if (!Vec) {
      Tcl_AppendResult(Interp,"Vector_SetData: Invalid vector",(char*)NULL);
      return(TCL_ERROR);
   }

   if ((vec=Vector_Get(Tcl_GetString(List)))) {
      nobj=vec->N;
   } else {
      Tcl_ListObjLength(Interp,List,&nobj);
   }

   if (Vec->Cp) {
      if (nobj!=Vec->N) {
         Tcl_AppendResult(Interp,"Vector_SetData: Component number invalid",(char*)NULL);
         return(TCL_ERROR);
      }
     for(n=0;n<Vec->N;n++) {
         if (vec) {
            Tcl_ListObjIndex(Interp,vec->Cn,n,&sub);
            obj=Tcl_DuplicateObj(List);
            Tcl_AppendStringsToObj(obj,".",Tcl_GetString(sub),(char*)NULL);
            if (Vector_SetData(Interp,Vec->Cp[n],obj,Idx)==TCL_ERROR) {
               return(TCL_ERROR);
            }
         } else {
            Tcl_ListObjIndex(Interp,List,n,&obj);
            if (Vector_SetData(Interp,Vec->Cp[n],obj,Idx)==TCL_ERROR) {
               return(TCL_ERROR);
            }
         }
      }
   } else {
      if (Idx>=0) {
         if (Idx+nobj-1>=Vec->N) {
            Tcl_AppendResult(Interp,"Vector_SetData: Index value out of bound",(char*)NULL);
            return(TCL_ERROR);
         }
      } else {
         Vector_Free(Vec);
         Vec->Nr=Vec->N=nobj;
         if (!(Vec->V=(double*)calloc(Vec->N,sizeof(double)))) {
            Tcl_AppendResult(Interp,"Vector_SetData: Unable to allocate data array",(char*)NULL);
            return(TCL_ERROR);
         }
      }

      if (vec) {
         memcpy(Vec->V,vec->V,nobj*sizeof(double));
      } else {
         for(n=0;n<nobj;n++) {
            Tcl_ListObjIndex(Interp,List,n,&obj);
            if (Tcl_GetDoubleFromObj(Interp,obj,&Vec->V[Idx>=0?Idx+n:n])==TCL_ERROR) {
               Vec->V[Idx>=0?Idx+n:n]=Vec->NoData;
            }
         }
      }
   }

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_AppendData>
 * Creation     : Fevrier 2005 J.P. Gauthier
 *
 * But          : Ajout de donnees dans un vecteur.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Vec>      : Vecteur
 *   <List>     : Liste des valeurs
 *   <Value>    : Valeur simple (Si pas de liste)
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Vector_AppendData(Tcl_Interp *Interp,TVector *Vec,Tcl_Obj *List,double Value) {

   Tcl_Obj *obj,*sub;
   int      n,nobj;
   TVector *vec=NULL;

   if (!Vec) {
      Tcl_AppendResult(Interp,"Vector_AppendData: Invalid vector",(char*)NULL);
      return(TCL_ERROR);
   }

   if (!List) {
      // Use a single Value
      nobj=1;
   } else if ((vec=Vector_Get(Tcl_GetString(List)))) {
      // Use a vector object
      nobj=vec->N;
   } else {
      // Use a list object
      Tcl_ListObjLength(Interp,List,&nobj);
   }

   if (Vec->Cp) {
      if (nobj!=Vec->N) {
         Tcl_AppendResult(Interp,"Vector_AppendData: Composant number invalid",(char*)NULL);
         return(TCL_ERROR);
      }
      for(n=0;n<Vec->N;n++) {
         if (vec) {
            Tcl_ListObjIndex(Interp,vec->Cn,n,&sub);
            obj=Tcl_DuplicateObj(List);
            Tcl_AppendStringsToObj(obj,".",Tcl_GetString(sub),(char*)NULL);
            Vector_AppendData(Interp,Vec->Cp[n],obj,Value);
         } else {
            Tcl_ListObjIndex(Interp,List,n,&obj);
            Vector_AppendData(Interp,Vec->Cp[n],obj,Value);
         }
      }
   } else {
      if (Vec->N+nobj>Vec->Nr) {

         if (VectorSpec.Alloc<=1.0) {
            Vec->Nr=Vec->N+nobj;
         } else {
            Vec->Nr=ceil((Vec->Nr==0?1:Vec->Nr)*VectorSpec.Alloc);
         }
         Vec->V=realloc(Vec->V,Vec->Nr*sizeof(double));
      }

      if (!List) {
         Vec->V[Vec->N]=Value;
      } else {
         if (vec) {
            memcpy(&Vec->V[Vec->N],vec->V,nobj*sizeof(double));
         } else {
            for(n=0;n<nobj;n++) {
               Tcl_ListObjIndex(Interp,List,n,&obj);
               if (Tcl_GetDoubleFromObj(Interp,obj,&Vec->V[Vec->N+n])==TCL_ERROR) {
                  Vec->V[Vec->N+n]=Vec->NoData;
               }
            }
         }
      }
      Vec->N+=nobj;
   }
   return(TCL_OK);
}
