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
 * Modification :
 *
 *   Nom        :
 *   Date       :
 *   Description:
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
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
 * Modification    :
 *   Nom         :
 *   Date        :
 *   Description :
 *---------------------------------------------------------------------------------------------------------------
*/

static int Vector_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   Tcl_Obj     *obj;
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

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case CREATE:
         if (Objc!=3 && Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"vector [data]");
            return TCL_ERROR;
         }

         /*Test for composant on data specification*/
         obj=NULL;
         if (Objc==4) {
            Tcl_ListObjLength(Interp,Objv[3],&nobj);
            Tcl_ListObjIndex(Interp,Objv[3],0,&obj);
            Tcl_ListObjLength(Interp,obj,&ns);
            if (ns) {
               obj=Tcl_NewListObj(0,NULL);
               for(n=0;n<nobj;n++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(n));
               }
            }
         }

         if (Vector_Create(Interp,Tcl_GetString(Objv[2]),obj)==TCL_ERROR) {
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
            return TCL_ERROR;
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
            return TCL_ERROR;
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
            return TCL_ERROR;
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
            return Vector_AppendData(Interp,vec,Objv[3]);
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
               if (e) n=vec->N+(--n);
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
               c=Tcl_GetString(Objv[3]);
               if (c[0]=='e' && c[1]=='n' && c[2]=='d') {
                  c=&c[3];
                  e=1;
               }
               if (c[0]!='\0') {
                  Tcl_GetInt(Interp,c,&n);
               }
               if (e) n=(vec->Cp&&vec->Cp[0])?vec->Cp[0]->N+(--n):vec->N+(--n);
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
            Vector_Destroy(Interp,Tcl_GetString(Objv[2]));
         }
         return(TCL_OK);
         break;

      case SPEC:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"?option?");
            return TCL_ERROR;
         }
         return Vector_Spec(Interp,Tcl_GetString(Objv[2]),Objc-2,Objv+2);
         break;

      case SORT:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"vector");
            return TCL_ERROR;
         }
         vec=Vector_Get(Tcl_GetString(Objv[2]));
         if (!vec) {
            Tcl_AppendResult(Interp,"Invalid vector",(char*)NULL);
            return(TCL_ERROR);
         } else {
            return Vector_Sort(Interp,vec);
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
         Tcl_HashAll(Interp,&VectorTable);
         break;

      case WIPE:
         Vector_Wipe();
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
static int Vector_Spec(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   int i,idx;

   static CONST char *sopt[] = { "-alloc",NULL };
   enum                opt { ALLOC };


   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
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
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/

static int Vector_Stat(Tcl_Interp *Interp,TVector *Vec,int Objc,Tcl_Obj *CONST Objv[]){

   int    i,idx,v;
   double val;

   static CONST char *sopt[] = { "-nodata","-max","-min","-avg",NULL };
   enum        opt {  NODATA,MAX,MIN,AVG };

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {

         case NODATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Vec->NoData));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Vec->NoData);
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int Vector_Create(Tcl_Interp *Interp,char *Name,Tcl_Obj *Comp) {

   Tcl_HashEntry *entry;
   Tcl_Obj       *obj;
   TVector       *vec;
   int            new,i;
   char           buf[256];

   entry=Tcl_CreateHashEntry(&VectorTable,Name,&new);

   if (!new) {
      vec=(TVector*)Tcl_GetHashValue(entry);
      if (vec->V || vec->Cn) {
         Tcl_AppendResult(Interp,"\n   Vector_Create:name already used: \"",Name,"\"",(char*)NULL);
         return TCL_ERROR;
      }
   } else {
      vec=(TVector*)malloc(sizeof(TVector));
      vec->NoData=nan("NaN");
      vec->Nr=vec->N=0;
      vec->V=NULL;
      vec->Cn=NULL;
      vec->Cp=NULL;
      vec->Def=NULL;
  }

   if (!vec){
      Tcl_AppendResult(Interp,"\n   Vector_Create : Could not allocate memory",(char*)NULL);
      return(TCL_ERROR);
   }

   /* Create the component vectors*/
   if (Comp) {
      Tcl_ListObjLength(Interp,Comp,&vec->N);
      Tcl_IncrRefCount(Comp);
      vec->Cn=Comp;
      vec->Cp=(TVector**)malloc(vec->N*sizeof(TVector*));

      for(i=0;i<vec->N;i++) {
         Tcl_ListObjIndex(Interp,vec->Cn,i,&obj);
         sprintf(buf,"%s.%s",Name,Tcl_GetString(obj));
         if (Vector_Create(Interp,buf,NULL)==TCL_ERROR) {
            return(TCL_ERROR);
         }
         vec->Cp[i]=Vector_Get(buf);
         vec->Cp[i]->NoData=vec->NoData;
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
TVector *Vector_Copy(Tcl_Interp *Interp,TVector *Vec,char *Name) {

   Tcl_Obj *obj;
   TVector *new;
   int      i;
   char     buf[256];

   if (new=Vector_Get(Name))
      if (new!=Vec)
         Vector_Destroy(Interp,Name);
      else
         return(new);

   if (Vector_Create(Interp,Name,NULL)!=TCL_OK) {
       return(NULL);
   }

   new=Vector_Get(Name);
   new->N=new->Nr=Vec->N;
   new->NoData=Vec->NoData;

   if (Vec->Cp) {
      /*Copie des composantes recursivement*/

      new->Cp=(TVector**)malloc(Vec->N*sizeof(TVector*));
      new->Cn=Vec->Cn;
      Tcl_IncrRefCount(Vec->Cn);

      for(i=0;i<new->N;i++) {
         Tcl_ListObjIndex(Interp,new->Cn,i,&obj);
         sprintf(buf,"%s.%s",Name,Tcl_GetString(obj));
         if (Vector_Copy(Interp,Vec->Cp[i],buf)!=TCL_OK) {
            return(NULL);
         }
         new->Cp[i]=Vector_Get(buf);
      }
   } else {
      /*Copie des donnees*/

      new->V=(double*)malloc(new->N*sizeof(double));
      memcpy(new->V,Vec->V,new->N*sizeof(double));
   }

   if (Vec->Def)
      Vector_GetDef(new);

   return(new);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Vector_GetDef>
 * Creation     : Aout 2005 J.P. Gauthier
 *
 * But          : Retourner la representation TDataDef du vecteur.
 *
 * Parametres   :
 *   <Vec>      : Vecteur a copier
 *
 * Retour       : Representation TDataDef
 *
 * Remarques    : On initialise la representation si elle n'existe pas
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
struct TDataDef* Vector_GetDef(TVector *Vec) {

   Tcl_Obj *obj;
   int      i,n=0;

   if (!Vec->Def) {
      Vec->Def=(struct TDataDef*)malloc(sizeof(struct TDataDef));
   }

   Vec->Def->NI=Vec->N;
   Vec->Def->NJ=1;
   Vec->Def->NK=1;
   Vec->Def->Data[0]=NULL;
   Vec->Def->Data[1]=NULL;
   Vec->Def->Data[2]=NULL;
   Vec->Def->Data[3]=NULL;
   Vec->Def->Type=TD_Float64;
   Vec->Def->NoData=Vec->NoData;

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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
TVector* Vector_Get(char *Name) {
   return((TVector*)Tcl_HashGet(&VectorTable,Name));
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int Vector_Destroy(Tcl_Interp *Interp,char *Name) {

   Tcl_Obj       *obj;
   TVector       *vec=NULL;
   char           buf[256];
   int            i;

   if ((vec=(TVector*)Tcl_HashDel(&VectorTable,Name))) {
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
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
               obj=Tcl_NewStringObj("-",0);
            } else {
               obj=Tcl_NewDoubleObj(Vec->V[Idx]);
            }
         } else {
            obj=Tcl_NewListObj(0,NULL);
            for(n=0;n<Vec->N;n++) {
               if (Vec->V[n]==Vec->NoData) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",0));
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int Vector_Length(Tcl_Interp *Interp,TVector *Vec,int Len) {

   int   i,n;

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
int Vector_Sort(Tcl_Interp *Interp,TVector *Vec) {

   int   i;

   if (!Vec) {
      Tcl_AppendResult(Interp,"Vector_Sort: Invalid vector",(char*)NULL);
      return(TCL_ERROR);
   }

   if (Vec->Cp) {
      for(i=0;i<Vec->N;i++) {
         Vector_Sort(Interp,Vec->Cp[i]);
      }
   } else {
      qsort(Vec->V,Vec->N,sizeof(double),QSort_Double);
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int Vector_Mem(Tcl_Interp *Interp,TVector *Vec,int Mem) {

   int i,n;

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
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int Vector_SetData(Tcl_Interp *Interp,TVector *Vec,Tcl_Obj *List,int Idx) {

   Tcl_Obj *obj;
   int      nobj,n;

   if (!Vec) {
      Tcl_AppendResult(Interp,"Vector_SetData: Invalid vector",(char*)NULL);
      return(TCL_ERROR);
   }

   Tcl_ListObjLength(Interp,List,&nobj);

   if (Vec->Cp) {
      if (nobj!=Vec->N) {
         Tcl_AppendResult(Interp,"Vector_SetData: Composant number invalid",(char*)NULL);
         return(TCL_ERROR);
      }
     for(n=0;n<Vec->N;n++) {
         Tcl_ListObjIndex(Interp,List,n,&obj);
         if (Vector_SetData(Interp,Vec->Cp[n],obj,Idx)==TCL_ERROR) {
            return(TCL_ERROR);
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
         Vec->V=(double*)calloc(Vec->N,sizeof(double));
      }

      for(n=0;n<nobj;n++) {
         Tcl_ListObjIndex(Interp,List,n,&obj);
         if (Tcl_GetDoubleFromObj(Interp,obj,&Vec->V[Idx>=0?Idx+n:n])==TCL_ERROR) {
            Vec->V[Idx>=0?Idx+n:n]=Vec->NoData;
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
 *   <Data>     : Liste des valeurs
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
int Vector_AppendData(Tcl_Interp *Interp,TVector *Vec,Tcl_Obj *List) {

   Tcl_Obj *obj;
   int      n,nobj;

   if (!Vec) {
      Tcl_AppendResult(Interp,"Vector_AppendData: Invalid vector",(char*)NULL);
      return(TCL_ERROR);
   }

   Tcl_ListObjLength(Interp,List,&nobj);

   if (Vec->Cp) {
      if (nobj!=Vec->N) {
         Tcl_AppendResult(Interp,"Vector_AppendData: Composant number invalid",(char*)NULL);
         return(TCL_ERROR);
      }
      for(n=0;n<Vec->N;n++) {
         Tcl_ListObjIndex(Interp,List,n,&obj);
         Vector_AppendData(Interp,Vec->Cp[n],obj);
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

      for(n=0;n<nobj;n++) {
         Tcl_ListObjIndex(Interp,List,n,&obj);
         if (Tcl_GetDoubleFromObj(Interp,obj,&Vec->V[Vec->N+n])==TCL_ERROR) {
            Vec->V[Vec->N+n]=Vec->NoData;
         }
      }
      Vec->N+=n;
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Vector_Wipe>
 * Creation : Fevrier 2005 - J.P. Gauthier - CMC/CMOE
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
void Vector_Wipe() {

   Tcl_HashSearch ptr;
   Tcl_HashEntry  *entry=NULL;

   printf("(INFO) Vector_Wipe: Wiping allocated memory\n");

   entry=Tcl_FirstHashEntry(&VectorTable,&ptr);

   while (entry) {
      Vector_Free((TVector*)Tcl_GetHashValue(entry));
      free((TVector*)Tcl_GetHashValue(entry));
      Tcl_DeleteHashEntry(entry);
      entry=Tcl_FirstHashEntry(&VectorTable,&ptr);
   }

   Tcl_DeleteHashTable(&VectorTable);
}
