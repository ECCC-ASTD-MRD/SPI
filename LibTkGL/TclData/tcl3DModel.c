/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Affichage de modele 3D
 * Fichier      : tcl3DModel.c
 * Creation     : Janvier 2003 - J.P. Gauthier
 *
 * Description  : Module de lecture de modele 3D
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <math.h>

#include "tcl3DModel.h"

static Tcl_HashTable ModelTable;
static int ModelInit=0;

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_Cmd>
 * Creation     : Janvier 2003 J.P. Gauthier
 *
 * But          : Effectuer les commandes du package
 *
 * Parametres    :
 *   <clientData>: Nom du modele
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

static int Model_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   int                idx;
   static CONST char *sopt[] = { "create","free","read","define","stats","configure","matrix","material","is",NULL };
   enum               opt { CREATE,FREE,READ,DEFINE,STATS,CONFIGURE,MATRIX,MATERIAL,IS };
   T3DModel           *m;
   TDataSpec          *spec;

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
            return(TCL_ERROR);
         }
         return(Model_Create(Interp,Tcl_GetString(Objv[2])));
         break;

      case READ:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id file");
            return(TCL_ERROR);
         }
         return(Model_Load(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3])));
         break;

      case FREE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         return(Model_Destroy(Interp,Tcl_GetString(Objv[2])));
         break;

      case DEFINE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"model ?option?");
            return(TCL_ERROR);
         }
         return(Model_Define(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case CONFIGURE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"model ?option?");
            return(TCL_ERROR);
         }
         m=Model_Get(Tcl_GetString(Objv[2]));
         if (!m) {
            Tcl_AppendResult(Interp,"invalid model",(char*)NULL);
            return(TCL_ERROR);
         }
         if (strcmp(Tcl_GetString(Objv[3]),"-dataspec")==0) {
            if (Objc==4) {
               if (m->Spec) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(m->Spec->Name,-1));
               }
            } else {
               if ((spec=DataSpec_Get(Tcl_GetString(Objv[4])))) {
                  if (m->Spec) {
                     DataSpec_FreeHash(Interp,m->Spec->Name);
                  }
                  m->Spec=spec;
                  spec->NRef++;
               } else {
                  Tcl_AppendResult(Interp,"Obs_Cmd: invalid configuration object",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
         } else {
            if (DataSpec_Config(Interp,m->Spec,Objc-3,Objv+3)==TCL_OK) {
               return(TCL_OK);
            } else {
               return(TCL_ERROR);
            }
         }
         break;

      case STATS:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"geometry ?option?");
            return TCL_ERROR;
         }
         return(Model_Stat(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case MATRIX:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id ?option?");
            return(TCL_ERROR);
         }
         return(Model_Matrix(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case MATERIAL:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id ?option?");
            return(TCL_ERROR);
         }
         return(Model_Material(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"obsid");
            return(TCL_ERROR);
         }
         if (Model_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Model_Define>
 * Creation : Juin 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Definition des parametres du modele.
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
static int Model_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj *obj;
   TGeoRef *ref;
   T3DModel  *mdl;
   int      i,idx;

   static CONST char *sopt[] = { "-active","-projection","-georef","-coordinate",NULL };
   enum               opt { ACTIVE,PROJECTION,GEOREF,COORDINATE };

   mdl=Model_Get(Name);
   if (!mdl) {
      Tcl_AppendResult(Interp,"\n   Model_Define: Model name unknown: \"",Name,"\"",(char *)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case ACTIVE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(mdl->Active));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&mdl->Active);
            }
            break;

         case GEOREF:
            if (Objc==1) {
               if (mdl->Ref) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(mdl->Ref->Name,-1));
               }
            } else {
               ref=GeoRef_Get(Tcl_GetString(Objv[++i]));
               if (!ref) {
                  Tcl_AppendResult(Interp,"\n   Model_Define: Georef name unknown: \"",Tcl_GetString(Objv[i]),"\"",(char *)NULL);
                  return TCL_ERROR;
               }
               if (ref!=mdl->Ref) {
                  if (mdl->Ref)
                     GeoRef_Destroy(Interp,mdl->Ref->Name);

                  mdl->Ref=ref;
                  GeoRef_Incr(mdl->Ref);
                  Model_Clean(mdl);
                  GeoRef_Size(mdl->Ref,mdl->Extent[0][0],mdl->Extent[0][1],0,mdl->Extent[1][0],mdl->Extent[1][1],1000,0);
              }
            }
            break;

        case PROJECTION:
            if (Objc==1) {
               if (mdl->Ref && mdl->Ref->String)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(mdl->Ref->String,-1));
            } else {
               ++i;
               if (strlen(Tcl_GetString(Objv[i]))==0) {
                  if (mdl->Ref) {
                      GeoRef_Destroy(Interp,mdl->Ref->Name);
                      mdl->Ref=NULL;
                      Model_Clean(mdl);
                  }
               } else {
                  if (mdl->Ref && mdl->Ref->String && strlen(mdl->Ref->String)==strlen(Tcl_GetString(Objv[i])) && strcmp(Tcl_GetString(Objv[i]),mdl->Ref->String)==0) {
                  } else {
                     GeoRef_Destroy(Interp,mdl->Ref->Name);
                     mdl->Ref=GeoRef_WKTSetup(0,0,0,0,NULL,NULL,0,0,0,0,Tcl_GetString(Objv[i]),NULL,NULL,NULL);
                     GeoRef_Size(mdl->Ref,mdl->Extent[0][0],mdl->Extent[0][1],0,mdl->Extent[1][0],mdl->Extent[1][1],1000,0);
                     Model_Clean(mdl);
                  }
               }
            }
            break;

         case COORDINATE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->Co.Lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->Co.Lon));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->Co.Elev));
               Tcl_SetObjResult(Interp,obj);
            } else {
            }
            break;
      }
   }

   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <Model_Stat>
 * Creation : Juin 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Definition des parametres du modele.
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
static int Model_Stat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj *lst;
   T3DModel  *mdl;
   int      idx;
   double   x,y,lat,lon;

   static CONST char *sopt[] = { "-project","-unproject","-extent",NULL };
   enum               opt { PROJECT,UNPROJECT,EXTENT };

   mdl=Model_Get(Name);
   if (!mdl) {
      Tcl_AppendResult(Interp,"\n   Model_Stat: Model name unknown: \"",Name,"\"",(char *)NULL);
      return TCL_ERROR;
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[0],sopt,"option",0,&idx)!=TCL_OK) {
      return TCL_ERROR;
   }

   lst=Tcl_NewListObj(0,NULL);

   switch ((enum opt)idx) {

      case PROJECT:
         Tcl_GetDoubleFromObj(Interp,Objv[1],&x);
         Tcl_GetDoubleFromObj(Interp,Objv[2],&y);
         if (mdl->Ref) {
            mdl->Ref->Project(mdl->Ref,x,y,&lat,&lon,1,1);
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lat));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lon));
         }
         break;

      case UNPROJECT:
         Tcl_GetDoubleFromObj(Interp,Objv[1],&lat);
         Tcl_GetDoubleFromObj(Interp,Objv[2],&lon);
         if (mdl->Ref) {
            mdl->Ref->UnProject(mdl->Ref,&x,&y,lat,lon,1,1);
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(x));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(y));
         }
         break;

      case EXTENT:
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(mdl->Extent[0][0]));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(mdl->Extent[0][1]));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(mdl->Extent[1][0]));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(mdl->Extent[1][1]));
         break;
   }
   Tcl_SetObjResult(Interp,lst);
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <Model_Matrix>
 * Creation : Juin 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Configuration des parametres la matrice du modele.
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
static int Model_Matrix(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj *obj;
   T3DModel  *mdl;
   double   tmp;
   int      i,idx;

   static CONST char *sopt[] = { "-translate","-rotate","-scale","-locate",NULL };
   enum               opt { TRANSLATE,ROTATE,SCALE,LOCATE };

   mdl=Model_Get(Name);
   if (!mdl) {
      Tcl_AppendResult(Interp,"\n   Model_Matrix: Model name unknown: \"",Name,"\"",(char *)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case TRANSLATE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->MatrixT[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->MatrixT[1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->MatrixT[2]));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->MatrixT[0]=tmp;
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->MatrixT[1]=tmp;
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->MatrixT[2]=tmp;
            }
            break;
         case ROTATE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->MatrixR[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->MatrixR[1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->MatrixR[2]));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->MatrixR[0]=tmp;
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->MatrixR[1]=tmp;
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->MatrixR[2]=tmp;
            }
            break;
         case SCALE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->MatrixS[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->MatrixS[1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->MatrixS[2]));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->MatrixS[0]=tmp;
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->MatrixS[1]=tmp;
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->MatrixS[2]=tmp;
            }
            break;
         case LOCATE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->Pos[0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->Pos[1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(mdl->Pos[2]));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->Pos[0]=tmp;
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->Pos[1]=tmp;
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->Pos[2]=tmp;
            }
            break;
      }
   }

   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <Model_Material>
 * Creation : Juin 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Configuration des parametres des materiaux du modele.
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
static int Model_Material(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   XColor *ci=NULL,co;
   T3DModel *mdl;
   double tmp;
   int    i,idx;
   char   buf[32];

   static CONST char *sopt[] = { "-ambient","-diffuse","-specular","-emissive","-shininess","-transparency",NULL };
   enum               opt { AMBIENT,DIFFUSE,SPECULAR,EMISSIVE,SHININESS,TRANSPARENCY };

   mdl=Model_Get(Name);
   if (!mdl) {
      Tcl_AppendResult(Interp,"\n   Model_Matrix: Model name unknown: \"",Name,"\"",(char *)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case AMBIENT:
            if (Objc==1) {
               co.red=mdl->Mt[0].Amb[0]*65535.0f;
               co.green=mdl->Mt[0].Amb[1]*65535.0f;
               co.blue=mdl->Mt[0].Amb[2]*65535.0f;
               Tcl_AppendResult(Interp,Tk_NameOfColor(&co),(char*)NULL);
            } else {
              ci=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[++i]);
               if (ci) {
                  mdl->Mt[0].Amb[0]=ci->red/65535.0f;
                  mdl->Mt[0].Amb[1]=ci->green/65535.0f;
                  mdl->Mt[0].Amb[2]=ci->blue/65535.0f;
                  Tk_FreeColor(ci);
               }
            }
            break;

         case DIFFUSE:
            if (Objc==1) {
               co.red=mdl->Mt[0].Dif[0]*65535.0f;
               co.green=mdl->Mt[0].Dif[1]*65535.0f;
               co.blue=mdl->Mt[0].Dif[2]*65535.0f;
               Tcl_AppendResult(Interp,Tk_NameOfColor(&co),(char*)NULL);
            } else {
               ci=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[++i]);
               if (ci) {
                  mdl->Mt[0].Dif[0]=ci->red/65535.0f;
                  mdl->Mt[0].Dif[1]=ci->green/65535.0f;
                  mdl->Mt[0].Dif[2]=ci->blue/65535.0f;
                  Tk_FreeColor(ci);
               }
            }
            break;

         case SPECULAR:
            if (Objc==1) {
               co.red=mdl->Mt[0].Spe[0]*65535.0f;
               co.green=mdl->Mt[0].Spe[1]*65535.0f;
               co.blue=mdl->Mt[0].Spe[2]*65535.0f;
               Tcl_AppendResult(Interp,Tk_NameOfColor(&co),(char*)NULL);
            } else {
               ci=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[++i]);
               if (ci) {
                  mdl->Mt[0].Spe[0]=ci->red/65535.0f;
                  mdl->Mt[0].Spe[1]=ci->green/65535.0f;
                  mdl->Mt[0].Spe[2]=ci->blue/65535.0f;
                  Tk_FreeColor(ci);
               }
            }
            break;

         case EMISSIVE:
            if (Objc==1) {
               co.red=mdl->Mt[0].Emi[0]*65535.0f;
               co.green=mdl->Mt[0].Emi[1]*65535.0f;
               co.blue=mdl->Mt[0].Emi[2]*65535.0f;
               Tcl_AppendResult(Interp,Tk_NameOfColor(&co),(char*)NULL);
            } else {
               ci=Tk_AllocColorFromObj(Interp,Tk_MainWindow(Interp),Objv[++i]);
               if (ci) {
                  mdl->Mt[0].Emi[0]=ci->red/65535.0f;
                  mdl->Mt[0].Emi[1]=ci->green/65535.0f;
                  mdl->Mt[0].Emi[2]=ci->blue/65535.0f;
                  Tk_FreeColor(ci);
               }
            }
            break;

         case SHININESS:
            if (Objc==1) {
               sprintf(buf,"%f",mdl->Mt[0].Shi/255.0f);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->Mt[0].Shi=tmp*255.0;
            }
            break;

         case TRANSPARENCY:
            if (Objc==1) {
               sprintf(buf,"%f",mdl->Mt[0].Dif[3]);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->Mt[0].Dif[3]=tmp;
            }
            break;
      }
   }

   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_Create>
 * Creation     : Mai 2002 J.P. Gauthier
 *
 * But          : Creation d'un objet Model et insertion d'un nouveau nom dans la table.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de l'image Model a creer
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Model_Create(Tcl_Interp *Interp,char *Name) {

   T3DModel* mdl;

   if (!(mdl=(T3DModel*)TclY_HashPut(Interp,&ModelTable,Name,sizeof(T3DModel)))) {
      return(TCL_ERROR);
   }

   if (!(mdl->Spec=DataSpec_Create(Interp,NULL))) {
      return(TCL_ERROR);
   }
   mdl->Spec->RenderTexture=1;

   Vect_Clear(mdl->Pos);
   Vect_Clear(mdl->MatrixT);
   Vect_Clear(mdl->MatrixR);
   Vect_Init(mdl->MatrixS,1.0,1.0,1.0);
   Vect_Init(mdl->Extent[0],1e32,1e32,1e32);
   Vect_Init(mdl->Extent[1],-1e32,-1e32,-1e32);

   mdl->Path=NULL;
   mdl->Active=1;
   mdl->Ref=NULL;
   mdl->Co.Lat=mdl->Co.Lon=-999.0;
   mdl->NObj=0;
   mdl->Obj=NULL;
   mdl->NMt=0;
   mdl->Mt=NULL;

   return(TCL_OK);
}

T3DObject *Model_ObjAdd(T3DModel *Model,int Nb) {

   T3DObject *obj;
   int        o;

   Model->NObj+=Nb;
   if (!(Model->Obj=(T3DObject*)realloc(Model->Obj,Model->NObj*sizeof(T3DObject)))) {
      return(NULL);
   }

   for(o=Model->NObj-Nb;o<Model->NObj;o++) {
      obj=&Model->Obj[o];

      obj->GLId=0;
      obj->Name[0]='\0';
      obj->NVr=0;
      obj->NFc=0;
      obj->Format=0;
      obj->Vr=NULL;
      obj->Nr=NULL;
      obj->Tx=NULL;
      obj->Fc=NULL;
      obj->Cl=NULL;

      Vect_Init(obj->Extent[0],0,0,0);
      Vect_Init(obj->Extent[1],0,0,0);
   }
   return(Model->Obj);
}
/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_Load>
 * Creation     : Mai 2002 J.P. Gauthier
 *
 * But          : Lecture des donnnees du modele dans un fichier.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de l'image Model a creer
 *   <Path>     : Nom du fichier
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Model_Load(Tcl_Interp *Interp,char *Name,char *Path) {

   T3DModel *mdl;
   int       c=1;

   mdl=Model_Get(Name);
   if (mdl) {
      Model_Destroy(Interp,Name);
   }

   Model_Create(Interp,Name);
   mdl=Model_Get(Name);
   mdl->Path=strdup(Path);

   if (!(c=Model_LoadMDL(mdl,Path))) {
      if (!(c=Model_Load3DS(mdl,Path))) {
         c=Model_LoadFLT(mdl,Path);
      }
   }
   if (c) Model_NormalCompute(mdl);

   return(c?TCL_OK:TCL_ERROR);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_Destroy>
 * Creation     : Mai 2002 J.P. Gauthier
 *
 * But          : Destruction d'une image Model a partir de son nom.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de l'image Model a detruire
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int Model_Destroy(Tcl_Interp *Interp,char *Name) {

   T3DModel *mdl=NULL;

   if ((mdl=(T3DModel*)TclY_HashDel(&ModelTable,Name))) {
      Model_Free(mdl);
      free(mdl);
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_Clean>
 * Creation     : Mars 2006 J.P. Gauthier
 *
 * But          : Liberation de la liste de rendue
 *
 * Parametres   :
 *   <M>        : Model
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model_Clean(T3DModel *M) {

   int o;

   for(o=0;o<M->NObj;o++) {
      if (M->Obj[o].GLId) {
         glDeleteLists(M->Obj[o].GLId,1);
         M->Obj[o].GLId=0;
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_Free>
 * Creation     : Mai 2002 J.P. Gauthier
 *
 * But          : Liberation de la memoire alloue poru un Model
 *
 * Parametres   :
 *   <M>        : Model
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model_Free(T3DModel *M) {

   int i;

   /*Material list*/
   for(i=0;i<M->NMt;i++) {
      if (M->Mt[i].Tex) glDeleteTextures(1,&M->Mt[i].Tex);
   }
   if (M->Mt) free(M->Mt);

   /*Object list*/
   for(i=0;i<M->NObj;i++) {
      Model_ObjFree(&M->Obj[i]);
   }

   /*Free projection*/
   if (M->Ref) {
      GeoRef_Destroy(NULL,M->Ref->Name);
   }
}

void Model_ObjFree(T3DObject *Obj) {

   int f;

   /*Vertex info*/
   if (Obj->Vr) free(Obj->Vr);
   if (Obj->Nr) free(Obj->Nr);
   if (Obj->Tx) free(Obj->Tx);
   if (Obj->Cl) free(Obj->Cl);

   /*Face list*/
   for (f=0;f<Obj->NFc;f++)
      free(Obj->Fc[f].Idx);

   if (Obj->Fc) free(Obj->Fc);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_Init>
 * Creation     : Janvier 2003 J.P. Gauthier
 *
 * But          : Initialisation des commandes Tcl pour utilisation des Models
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
int Model_Init(Tcl_Interp *Interp) {

   if (!ModelInit++) {
      Tcl_InitHashTable(&ModelTable,TCL_STRING_KEYS);
   }
   Tcl_CreateObjCommand(Interp,"model",Model_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_Get>
 * Creation     : Janvier 2003 J.P. Gauthier
 *
 * But          : Obtenir un model en fonction de son nom dans la table de Tcl ModelTable
 *
 * Parametres   :
 *   <Name>     : Nom de l'objet Model a obtenir.
 *
 * Retour       : Une structure Model ou un pointeur NULL si rien trouve.
 *
 * Remarques    :
 *---------------------------------------------------------------------------------------------------------------
*/
T3DModel* Model_Get(char *Name) {
   return((T3DModel*)TclY_HashGet(&ModelTable,Name));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_LoadMDL>
 * Creation     : Janvier 2003 J.P. Gauthier
 *
 * But          : Lire un fichier MDL
 *
 * Parametres   :
 *   <M>        : Objet Model
 *   <Path>     : Path du fichier
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Model_LoadMDL(T3DModel *M,char *Path) {

   int       i,m;
   T3DObject *obj;
   FILE      *file;

   if (!(file=fopen(Path,"r"))) {
      return(0);
   }

   /*Material list*/
   /*Number of material*/
   fread(&M->NMt,sizeof(int),1,file);
   if (M->NMt>256 || M->NMt<0) {
      fclose(file);
      M->NMt=0;
      return(0);
   }

#ifdef DEBUG
   printf("(DEBUG) Model_LoadMDL: M->NMt=%i\n",M->NMt);
#endif

   if (M->NMt<=0) {
      M->NMt=1;
      M->Mt=(TMaterial*)malloc(sizeof(TMaterial));
      M->Mt[0].Amb[0]=0.1;M->Mt[0].Amb[1]=0.1;M->Mt[0].Amb[2]=0.1;M->Mt[0].Amb[3]=1.0;
      M->Mt[0].Dif[0]=0.8;M->Mt[0].Dif[1]=0.8;M->Mt[0].Dif[2]=0.8;M->Mt[0].Dif[3]=1.0;
      M->Mt[0].Emi[0]=0.1;M->Mt[0].Emi[1]=0.1;M->Mt[0].Emi[2]=0.1;M->Mt[0].Emi[3]=1.0;
      M->Mt[0].Spe[0]=0.1;M->Mt[0].Spe[1]=0.6;M->Mt[0].Spe[2]=0.6;M->Mt[0].Spe[3]=1.0;
      M->Mt[0].Shi=255;
      M->Mt[0].Dif[3]=1.0;
      M->Mt[0].Tex=0;
      M->Mt[0].Path[0]='\0';
  } else {
      M->Mt=(TMaterial*)malloc(M->NMt*sizeof(TMaterial));
      for (i=0; i<M->NMt; i++) {
        fread(&M->Mt[i].Amb,sizeof(float),3,file);
        fread(&M->Mt[i].Dif,sizeof(float),3,file);
        fread(&M->Mt[i].Emi,sizeof(float),3,file);
        fread(&M->Mt[i].Spe,sizeof(float),3,file);
        fread(&M->Mt[i].Shi,sizeof(float),1,file);
        fread(&M->Mt[i].Dif[3],sizeof(float),1,file);
        M->Mt[i].Tex=0;
        M->Mt[i].Path[0]='\0';
      }
   }

   Model_ObjAdd(M,1);
   obj=&M->Obj[M->NObj-1];

   /*Vertex list*/
   /*Number of vertex*/
   fread(&obj->NVr,sizeof(int),1,file);
#ifdef DEBUG
   printf("(DEBUG) Model_LoadMDL: M->NVr=%i\n",obj->NVr);
#endif

   /*Format of vertex*/
   fread(&obj->Format,sizeof(int),1,file);
#ifdef DEBUG
  printf("(DEBUG) Model_LoadMDL: M->Format=%i\n",obj->Format);
#endif
   if (obj->NVr<=0 || obj->Format<=0) {
      printf("\n(ERROR) Model_LoadMDL : Invalid vertex format or number");
      fclose(file);
      return(0);
   }

   /*Allocate data*/
   if (obj->Format==F3VNT) obj->Tx=(Vect3f*)malloc(obj->NVr*sizeof(Vect3f));
   if (obj->Format>=F3VN)  obj->Nr=(Vect3f*)malloc(obj->NVr*sizeof(Vect3f));
                           obj->Vr=(Vect3f*)malloc(obj->NVr*sizeof(Vect3f));

   /*Vertex list*/
   for (i=0;i<obj->NVr;i++) {
                              fread(&obj->Vr[i],sizeof(Vect3f),1,file);
      if (obj->Format>=F3VN)  fread(&obj->Nr[i],sizeof(Vect3f),1,file);
      if (obj->Format==F3VNT) fread(&obj->Tx[i],sizeof(Vect3f),1,file);
   }

   /*Faces list*/
   /*Number of faces*/
   fread(&obj->NFc,sizeof(int),1,file);
#ifdef DEBUG
   printf("(DEBUG) Model_LoadMDL: M->NFc=%i\n",obj->NFc);
#endif

   obj->Fc=(TFace*)malloc(obj->NFc*sizeof(TFace));
   for (i=0;i<obj->NFc;i++) {
      fread(&m,sizeof(int),1,file);
      fread(&obj->Fc[i].NIdx,sizeof(unsigned char),1,file);

      obj->Fc[i].Mt=m<0?&M->Mt[0]:&M->Mt[m];
      obj->Fc[i].Idx=(unsigned int*)malloc(obj->Fc[i].NIdx*sizeof(unsigned int));
      fread(obj->Fc[i].Idx,sizeof(int),obj->Fc[i].NIdx,file);
   }
   fclose(file);

   return(1);
}

void Model_NormalCompute(T3DModel *M) {

   int v,o,f,nb=0,shared=0;
   T3DObject *obj;

   Vect3f *nr=NULL,*tmp=NULL,vr[3],sum;

   for (o=0;o<M->NObj;o++) {
      obj=&(M->Obj[o]);

     if (obj->Nr || !obj->NVr) {
         continue;
      }

      if (obj->NFc>nb) {
         nb=obj->NFc;
         nr=(Vect3f*)realloc(nr,nb*sizeof(Vect3f));
         tmp=(Vect3f*)realloc(tmp,nb*sizeof(Vect3f));
      }
      obj->Nr=(Vect3f*)malloc(obj->NVr*sizeof(Vect3f));

      /*Face normals*/
      for (f=0;f<obj->NFc;f++) {
         if (obj->Fc[f].NIdx>2 && obj->Vr) {
            Vect_Assign(vr[0],obj->Vr[obj->Fc[f].Idx[0]]);
            Vect_Assign(vr[1],obj->Vr[obj->Fc[f].Idx[1]]);
            Vect_Assign(vr[2],obj->Vr[obj->Fc[f].Idx[2]]);

            Vect_Substract(vr[0],vr[1],vr[0]);
            Vect_Substract(vr[1],vr[2],vr[1]);
            Vect3f_CrossProduct(nr[f],vr[0],vr[1]);
            Vect_Assign(tmp[f],nr[f]);     // Save the un-normalized normal for the vertex normals
            Vect3f_Normalize(nr[f]);
         } else {
            Vect_Init(nr[f],0.0,0.0,1.0);
            Vect_Init(tmp[f],0.0,0.0,1.0);
         }
      }

      /*Vertex normals*/
      for (v=0;v<obj->NVr;v++) {
         Vect_Init(sum,0.0,0.0,0.0);
         shared=0;

         for (f=0;f<obj->NFc;f++) {     // Check if the vertex is shared by another face
            if (obj->Fc[f].Idx[0]==v || obj->Fc[f].Idx[1]==v || obj->Fc[f].Idx[2]==v) {
               Vect_Add(sum,sum,tmp[f]); // Add the un-normalized normal of the shared face
               shared++;
            }
         }

         Vect_SDiv(obj->Nr[v],sum,-shared);
         Vect3f_Normalize(obj->Nr[v]);
      }
   }
   if (nr) free(nr);
   if (tmp) free(tmp);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_Render>
 * Creation     : Mai 2002 J.P. Gauthier
 *
 * But          : Rendu du modele a l'ecran.
 *
 * Parametres  :
 *   <Proj>     : La projection courante
 *   <VP>       : Le viewport ou le rendu doit etre fait
 *   <M>        : Modele a afficher
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Model_LOD(Projection *Proj,ViewportItem *VP,T3DModel *M,Vect3d *Extent) {

   Vect3d ex[4],lim,min,max;

   if (!M->Ref) {
      return(1);
   }

   ex[0][0]=Extent[0][0];ex[0][1]=Extent[0][1];ex[0][2]=Extent[0][2];
   ex[1][0]=Extent[0][0];ex[1][1]=Extent[1][1];ex[1][2]=Extent[0][2];
   ex[2][0]=Extent[1][0];ex[2][1]=Extent[0][1];ex[2][2]=Extent[0][2];
   ex[3][0]=Extent[1][0];ex[3][1]=Extent[1][1];ex[3][2]=Extent[1][2];

   M->Ref->Project(M->Ref,ex[0][0],ex[0][1],&ex[0][1],&ex[0][0],1,0);
   M->Ref->Project(M->Ref,ex[1][0],ex[1][1],&ex[1][1],&ex[1][0],1,0);
   M->Ref->Project(M->Ref,ex[2][0],ex[2][1],&ex[2][1],&ex[2][0],1,0);
   M->Ref->Project(M->Ref,ex[3][0],ex[3][1],&ex[3][1],&ex[3][0],1,0);
   Proj->Type->Project(Proj,ex,NULL,4);

   gluProject(ex[0][0],ex[0][1],ex[0][2],VP->GLModR,VP->GLProj,VP->GLView,&lim[0],&lim[1],&lim[2]);
   Vect_Assign(min,lim);
   Vect_Assign(max,lim);
   gluProject(ex[1][0],ex[1][1],ex[1][2],VP->GLModR,VP->GLProj,VP->GLView,&lim[0],&lim[1],&lim[2]);
   Vect_Min(min,min,lim);
   Vect_Max(max,max,lim);
   gluProject(ex[2][0],ex[2][1],ex[2][2],VP->GLModR,VP->GLProj,VP->GLView,&lim[0],&lim[1],&lim[2]);
   Vect_Min(min,min,lim);
   Vect_Max(max,max,lim);
   gluProject(ex[3][0],ex[3][1],ex[3][2],VP->GLModR,VP->GLProj,VP->GLView,&lim[0],&lim[1],&lim[2]);
   Vect_Min(min,min,lim);
   Vect_Max(max,max,lim);

   /*Is it big enough ???*/
   if (Vect_Weight(min,max)<5) {
      return(0);
   }

   /*Is it visible (in X,Y and Z) ???*/
   if (VOUT(min[0],max[0],1,VP->Width) || VOUT(min[1],max[1],1,VP->Height) || VOUT(min[2],max[2],-0.1,2.0)) {
      return(0);
   }
   return(1);
}

int Model_Render(Projection *Proj,ViewportItem *VP,T3DModel *M) {

   unsigned int i,j,o,idx;
   Vect3d       vr;
   Vect3f       vrf;
   T3DObject   *obj;
   char        *path=NULL;

   extern GLint Texture_Read(char *File);

   if (!M) {
      fprintf(stderr,"(ERROR) Model_Render: Invalid model object\n");
      return(0);
   }

   if (!M->Active) {
      return(0);
   }

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glEnable(GL_LIGHTING);
   glEnable(GL_LIGHT0);
   glEnable(GL_DEPTH_TEST);
   glEnable(GL_BLEND);
//   glEnable(GL_COLOR_MATERIAL);
   glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

   /*Parse material for texture setup*/
   for(i=0;i<M->NMt;i++) {
      if (!M->Mt[i].Tex && strlen(M->Mt[i].Path)) {
         path=strpath(M->Path,M->Mt[i].Path);
         M->Mt[i].Tex=Texture_Read(path);
         if (path) free(path);path=NULL;
      }
   }

   /*Position the model within geography*/
   if (!M->Ref) {
      Proj->Type->Locate(Proj,M->Pos[0],M->Pos[1],1);
      /*Positionner le modele*/
      glTranslatef(0.0,0.0,(M->Pos[2]*Proj->Scale+EARTHRADIUS)/EARTHRADIUS);
      /*On suppose que le modele est en metres alors on scale par rapport a la terre*/
      glScalef(1.0/EARTHRADIUS,1.0/EARTHRADIUS,1.0/EARTHRADIUS);
   }

   /*Local matrix manipulation*/
   glTranslatef(M->MatrixT[0],M->MatrixT[1],M->MatrixT[2]);
   glScalef(M->MatrixS[0],M->MatrixS[1],M->MatrixS[2]);
   glRotatef(M->MatrixR[0],1.0,0.0,0.0);
   glRotatef(M->MatrixR[1],0.0,0.0,1.0);
   glRotatef(M->MatrixR[2],0.0,1.0,0.0);

   /*Create display lists*/
   if (!M->Obj[0].GLId) {
      for(o=200;o<M->NObj;o++) {
         obj=&M->Obj[o];
         if (!obj->GLId) {

            obj->GLId=glGenLists(1);
            glNewList(obj->GLId,GL_COMPILE);

            Vect_Init(obj->Extent[0],1e32,1e32,1e32);
            Vect_Init(obj->Extent[1],-1e32,-1e32,-1e32);

            for (i=0;i<obj->NFc;i++) {
               if (obj->Fc[i].Mt) {
                  glMaterialf(GL_FRONT,GL_SHININESS,obj->Fc[i].Mt->Shi);
                  glMaterialfv(GL_FRONT,GL_AMBIENT,obj->Fc[i].Mt->Amb);
                  glMaterialfv(GL_FRONT,GL_DIFFUSE,obj->Fc[i].Mt->Dif);
                  glMaterialfv(GL_FRONT,GL_SPECULAR,obj->Fc[i].Mt->Spe);
                  glMaterialfv(GL_FRONT,GL_EMISSION,obj->Fc[i].Mt->Emi);
                  if (obj->Fc[i].Mt->Tex>0) {
                     glBindTexture(GL_TEXTURE_2D,obj->Fc[i].Mt->Tex);
                     glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);
                     glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_REPEAT);
                  }
               }

               switch(obj->Fc[i].NIdx) {
                  case 1:  glBegin(GL_POINTS); break;
                  case 2:  glBegin(GL_LINES); break;
                  case 3:  glBegin(GL_TRIANGLES); break;
                  case 4:  glBegin(GL_QUADS); break;
                  default: glBegin(GL_POLYGON); break;
               }

               for (j=0;j<obj->Fc[i].NIdx;j++) {
                  idx=obj->Fc[i].Idx[j];
                  if (idx<0) {
                     fprintf(stderr,"(ERROR) Model_Render: Invalid vertex index (%i) for obj %i on face %i\n",idx,o,i);
                     break;
                  }

                  if (obj->Tx) glTexCoord3fv(obj->Tx[idx]);
                  if (obj->Nr) glNormal3fv(obj->Nr[idx]);
                  if (obj->Cl) glColor4fv(obj->Cl[idx]);

                  /*Projection to georef*/
                  if (obj->Vr) {
                     if (M->Ref) {
                        M->Ref->Project(M->Ref,obj->Vr[idx][0],obj->Vr[idx][1],&M->Co.Lat,&M->Co.Lon,1,0);
                        M->Co.Elev=obj->Vr[idx][2];

                        Proj->Type->Project(Proj,&M->Co,&vr,1);
                        Vect_Assign(vrf,vr);
                        glVertex3fv(vrf);
                     } else {
                        glVertex3fv(obj->Vr[idx]);
                     }
                     Vect_Min(obj->Extent[0],obj->Extent[0],obj->Vr[idx]);
                     Vect_Max(obj->Extent[1],obj->Extent[1],obj->Vr[idx]);
                  }
              }
              glEnd();
            }
            glDisable(GL_TEXTURE_2D);
            glEndList();

            Vect_Min(M->Extent[0],M->Extent[0],obj->Extent[0]);
            Vect_Max(M->Extent[1],M->Extent[1],obj->Extent[1]);
         }
      }
   }

   if (Model_LOD(Proj,VP,M,M->Extent)) {
      for(o=0;o<M->NObj;o++) {
         obj=&M->Obj[o];

         if (Model_LOD(Proj,VP,M,obj->Extent)) {
            glPolygonMode(GL_FRONT,GL_FILL);
            if (M->Spec->RenderTexture) {
               glEnable(GL_TEXTURE_2D);
            } else {
               glDisable(GL_TEXTURE_2D);
            }

            if (M->Spec->Outline && M->Spec->Width) {
               glEnable(GL_POLYGON_OFFSET_FILL);
               glPolygonOffset(0.5,1.0);
            }

            if (M->Spec->Light) {
               glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
            } else {
               glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_REPLACE);
            }

            glCallList(obj->GLId);

            if (M->Spec->Outline && M->Spec->Width) {
               glDisable(GL_LIGHTING);
               glDisable(GL_TEXTURE_2D);
               glDisable(GL_POLYGON_OFFSET_FILL);
               glDash(&M->Spec->Dash);
               glLineWidth(ABS(M->Spec->Width));
               glColor4us(M->Spec->Outline->red,M->Spec->Outline->green,M->Spec->Outline->blue,M->Spec->Alpha*655.35);
               glPolygonMode(GL_FRONT,GL_LINE);
               glCallList(obj->GLId);
               glEnable(GL_LIGHTING);
            }
         }
      }
   }

   glDisable(GL_COLOR_MATERIAL);
   glDisable(GL_BLEND);
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_LIGHTING);
   glDisable(GL_LIGHT0);
   glPopMatrix();

   return(1);
}
