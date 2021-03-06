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
#include <math.h>
#include <malloc.h>

#include "App.h"
#include "tcl3DModel.h"
#include "tclGeoRef.h"

static Tcl_HashTable ModelTable;
static int ModelInit=0;
static int ModelSceneDepth=0;

static int Model_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int Model_Create(Tcl_Interp *Interp,char* Name);
static int Model_Destroy(Tcl_Interp *Interp,char *Name);
static int Model_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int Model_Stat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int Model_Matrix(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int Model_Material(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);

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
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",TCL_EXACT,&idx)!=TCL_OK) {
      return(TCL_ERROR);
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
            return(TCL_ERROR);
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
   TGeoRef  *ref;
   T3DModel *mdl;
   int       i,idx;
   double    tmpd;

   static CONST char *sopt[] = { "-projection","-georef","-coordinate","-name","-unitmeter",NULL };
   enum               opt { PROJECTION,GEOREF,COORDINATE,NAME,UNITMETER };

   mdl=Model_Get(Name);
   if (!mdl) {
      Tcl_AppendResult(Interp,"\n   Model_Define: Model name unknown: \"",Name,"\"",(char *)NULL);
      return(TCL_ERROR);
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {

         case NAME:
            if (Objc==1) {
               if (mdl->Name)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(mdl->Name,-1));
            } else {
               if (mdl->Name) free(mdl->Name); mdl->Name=NULL;
               if (strlen(Tcl_GetString(Objv[++i])))
                  mdl->Name=strdup(Tcl_GetString(Objv[i]));
            }
            break;

         case UNITMETER:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(mdl->Meter));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmpd);
               if (tmpd!=mdl->Meter) {
                  mdl->Meter=tmpd;
                  Model_Clean(mdl);
               }
            }
            break;

         case GEOREF:
            if (Objc==1) {
               if (mdl->GRef) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(mdl->GRef->Name,-1));
               }
            } else {
               ref=GeoRef_Get(Tcl_GetString(Objv[++i]));
               if (!ref) {
                  Tcl_AppendResult(Interp,"\n   Model_Define: Georef name unknown: \"",Tcl_GetString(Objv[i]),"\"",(char *)NULL);
                  return(TCL_ERROR);
               }
               if (ref!=mdl->GRef) {
                  if (mdl->GRef)
                     GeoRef_Destroy(Interp,mdl->GRef->Name);

                  mdl->GRef=ref;
                  GeoRef_Incr(mdl->GRef);
                  Model_Clean(mdl);
                  GeoRef_Size(mdl->GRef,mdl->Extent[0][0],mdl->Extent[0][1],mdl->Extent[1][0],mdl->Extent[1][1],0);
              }
            }
            break;

        case PROJECTION:
            if (Objc==1) {
               if (mdl->GRef && mdl->GRef->String)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(mdl->GRef->String,-1));
            } else {
               ++i;
               if (strlen(Tcl_GetString(Objv[i]))==0) {
                  if (mdl->GRef) {
                      GeoRef_Destroy(Interp,mdl->GRef->Name);
                      mdl->GRef=NULL;
                      Model_Clean(mdl);
                  }
               } else {
                  if (mdl->GRef && mdl->GRef->String && strlen(mdl->GRef->String)==strlen(Tcl_GetString(Objv[i])) && strcmp(Tcl_GetString(Objv[i]),mdl->GRef->String)==0) {
                  } else {
                     GeoRef_Destroy(Interp,mdl->GRef->Name);
                     mdl->GRef=GeoRef_Find(GeoRef_WKTSetup(0,0,NULL,0,0,0,0,Tcl_GetString(Objv[i]),NULL,NULL,NULL));
                     GeoRef_Size(mdl->GRef,mdl->Extent[0][0],mdl->Extent[0][1],mdl->Extent[1][0],mdl->Extent[1][1],0);
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

   return(TCL_OK);
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

   static CONST char *sopt[] = { "-project","-unproject","-extent","-llextent",NULL };
   enum               opt { PROJECT,UNPROJECT,EXTENT,LLEXTENT };

   mdl=Model_Get(Name);
   if (!mdl) {
      Tcl_AppendResult(Interp,"\n   Model_Stat: Model name unknown: \"",Name,"\"",(char *)NULL);
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[0],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   lst=Tcl_NewListObj(0,NULL);

   switch ((enum opt)idx) {

      case PROJECT:
         Tcl_GetDoubleFromObj(Interp,Objv[1],&x);
         Tcl_GetDoubleFromObj(Interp,Objv[2],&y);
         if (mdl->GRef) {
            mdl->GRef->Project(mdl->GRef,x,y,&lat,&lon,1,1);
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lat));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lon));
         } else {
            lat=mdl->Pos[0]+RAD2DEG(M2RAD(y*mdl->Meter));
            lon=mdl->Pos[1]+RAD2DEG(M2RAD(x*mdl->Meter));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lat));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lon));
         }
         break;

      case UNPROJECT:
         Tcl_GetDoubleFromObj(Interp,Objv[1],&lat);
         Tcl_GetDoubleFromObj(Interp,Objv[2],&lon);
         if (mdl->GRef) {
            mdl->GRef->UnProject(mdl->GRef,&x,&y,lat,lon,1,1);
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

      case LLEXTENT:
         /*If not calculated yet, get latlon extent*/
         if (mdl->GRef) {
            if (mdl->GRef->LLExtent.MinY==1e32) {
               mdl->GRef->Project(mdl->GRef,mdl->Extent[0][0],mdl->Extent[0][1],&mdl->GRef->LLExtent.MinY,&mdl->GRef->LLExtent.MinX,1,1);
               mdl->GRef->Project(mdl->GRef,mdl->Extent[1][0],mdl->Extent[1][1],&mdl->GRef->LLExtent.MaxY,&mdl->GRef->LLExtent.MaxX,1,1);
            }
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(mdl->GRef->LLExtent.MinY));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(mdl->GRef->LLExtent.MinX));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(mdl->GRef->LLExtent.MaxY));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(mdl->GRef->LLExtent.MaxX));
         }
         break;
   }
   Tcl_SetObjResult(Interp,lst);

   return(TCL_OK);
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
      return(TCL_ERROR);
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return(TCL_ERROR);
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

   return(TCL_OK);
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
      return(TCL_ERROR);
   }

   if (!mdl->Mt) {
      return(TCL_OK);
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return(TCL_ERROR);
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
               sprintf(buf,"%f",mdl->Mt[0].Shi/128.0);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&tmp);mdl->Mt[0].Shi=tmp*128.0;
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

   return(TCL_OK);
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
   Vect_Init(mdl->Extent[0],0.0,0.0,0.0);
   Vect_Init(mdl->Extent[1],0.0,0.0,0.0);

   mdl->Name=NULL;
   mdl->Path=NULL;
   mdl->GRef=NULL;
   mdl->Co.Lat=mdl->Co.Lon=-999.0;
   mdl->NObj=0;
   mdl->Obj=NULL;
   mdl->NMt=0;
   mdl->Mt=NULL;
   mdl->Scn=NULL;
   mdl->Meter=1.0;

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_SceneAdd>
 * Creation     : Janvier 2011 J.P. Gauthier
 *
 * But          : Ajout d'une nouvelle scene.
 *
 * Parametres   :
 *   <Model>    : Model
 *   <Parent>   : Scene parent
 *   <Nb>       : Nombre de scene a ajouter
 *
 * Retour       : Pointeur sur la nouvelle (premiere) scene(s)
 *
 * Remarques    :
 *    - Les scenes sont un arbre, chaque branche est une scene avec de possible sous-scene
 *    - Si Model est NULL, une scene orpheline est cree
 *    - Si Parent est NULL, une scene "root" est cree dans le modele
 *      Sinon, une sous-scene est cree dans la scene Parent
 *---------------------------------------------------------------------------------------------------------------
*/
T3DScene *Model_SceneAdd(T3DModel *Model,T3DScene *Parent,int Nb) {

   T3DScene    *scn,*scnp;
   unsigned int s,nscn;

   if (!Model) {
      if (!(scn=(T3DScene*)malloc(Nb*sizeof(T3DScene)))) {
         return(NULL);
      }
      nscn=Nb;
      scnp=scn;
   } else {
      if (!Parent) {
         if (Nb>1 || Model->Scn) {
            App_Log(ERROR,"%s: Root scene count must be 0",__func__);
         }
         if (!(Model->Scn=(T3DScene*)malloc(sizeof(T3DScene)))) {
            return(NULL);
         }
         nscn=1;
         scnp=Model->Scn;
      } else {
         Parent->NScn+=Nb;
         if (!(Parent->Scn=(T3DScene*)realloc(Parent->Scn,Parent->NScn*sizeof(T3DScene)))) {
            return(NULL);
         }
         nscn=Parent->NScn;
         scnp=Parent->Scn;
      }
   }

   for(s=nscn-Nb;s<nscn;s++) {
      scn=&scnp[s];

      scn->Name=NULL;
      scn->Mtx=NULL;
      scn->NObj=0;
      scn->Obj=NULL;
      scn->NScn=0;
      scn->Scn=NULL;
      scn->Parent=Parent;
   }
   return(&scnp[nscn-Nb]);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_SceneFree>
 * Creation     : Janvier 2011 J.P. Gauthier
 *
 * But          : Liberation de la memoire alloue pour un arbre de scene
 *
 * Parametres   :
 *   <Scene>    : Scene root de depart
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model_SceneFree(T3DScene *Scene) {

   int       n;

   if (Scene) {
      if (Scene->Name) free(Scene->Name);
      if (Scene->Mtx)  free(Scene->Mtx);
      if (Scene->Obj)  free(Scene->Obj);

      for(n=0;n<Scene->NScn;n++) {
         Model_SceneFree(&Scene->Scn[n]);
      }
      free(Scene->Scn);
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_SceneFind>
 * Creation     : Janvier 2011 J.P. Gauthier
 *
 * But          : Rechercher une scene.
 *
 * Parametres   :
 *   <Scene>    : Scene root de depart de la recherche
 *   <Name>     : Nom de la scene a rechercher
 *
 * Retour       : Pointeur sur la scene (ou NULL si non-existante)
 *
 * Remarques    :
 *---------------------------------------------------------------------------------------------------------------
*/
T3DScene *Model_SceneFind(T3DScene *Scene,char *Name) {

   T3DScene *scn=NULL;
   int       n;

   if (Scene->Name && strncmp(Scene->Name,Name,XMLSTRINGSIZE)==0) {
      return(Scene);
   }

   for(n=0;n<Scene->NScn;n++) {
      if ((scn=Model_SceneFind(&Scene->Scn[n],Name))) {
         break;
      }
   }
   return(scn);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_ObjectAdd>
 * Creation     : Janvier 2011 J.P. Gauthier
 *
 * But          : Ajout d'un nouvel objet.
 *
 * Parametres   :
 *   <Model>    : Model
 *   <Nb>       : Nombre de scene a ajouter
 *
 * Retour       : Pointeur sur le nouvel (premier) objet(s)
 *
 * Remarques    :
 *   - Les objets sont un tableau commun a toutes les scenes, ils sont reference directement
 *     par celles-ci
 *---------------------------------------------------------------------------------------------------------------
*/
T3DObject *Model_ObjectAdd(T3DModel *Model,int Nb) {

   T3DObject   *obj;
   unsigned int o;

   Model->NObj+=Nb;
   if (!(Model->Obj=(T3DObject*)realloc(Model->Obj,Model->NObj*sizeof(T3DObject)))) {
      return(NULL);
   }

   for(o=Model->NObj-Nb;o<Model->NObj;o++) {
      obj=&Model->Obj[o];

      obj->GLId=0;
      obj->Name=NULL;
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
   return(&Model->Obj[Model->NObj-Nb]);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_ObjectFree>
 * Creation     : Mai 2002 J.P. Gauthier
 *
 * But          : Liberation de la memoire alloue pour un Object
 *
 * Parametres   :
 *   <Obj>      : Object
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model_ObjectFree(T3DObject *Obj) {

   int f;

   if (Obj) {
      /*Vertex info*/
      if (Obj->Name) free(Obj->Name);
      if (Obj->Vr)   free(Obj->Vr);
      if (Obj->Nr)   free(Obj->Nr);
      if (Obj->Tx)   free(Obj->Tx);
      if (Obj->Cl)   free(Obj->Cl);

      /*Face list*/
      for (f=0;f<Obj->NFc;f++) {
         free(Obj->Fc[f].Idx);
         if (Obj->Fc[f].Name) free(Obj->Fc[f].Name);
      }

      if (Obj->Fc) free(Obj->Fc);
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_ObjectFind>
 * Creation     : Janvier 2011 J.P. Gauthier
 *
 * But          : Rechercher un objet.
 *
 * Parametres   :
 *   <Model>    : Model dans lequel rechercher
 *   <Name>     : Nom de l'objet a rechercher
 *
 * Retour       : Pointeur sur l'objet (ou NULL si non-existant)
 *
 * Remarques    :
 *---------------------------------------------------------------------------------------------------------------
*/
T3DObject *Model_ObjectFind(T3DModel *Model,char *Name) {

   T3DObject *obj=NULL;
   int       o;

   for(o=0;o<Model->NObj;o++) {
      if (strncmp(Model->Obj[o].Name,Name,XMLSTRINGSIZE)==0) {
         obj=&Model->Obj[o];
         break;
      }
   }
   return(obj);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_FaceAdd>
 * Creation     : Janvier 2011 J.P. Gauthier
 *
 * But          : Ajout de face a un objet.
 *
 * Parametres   :
 *   <Obj>      : Objet
 *   <Nb>       : Nombre de faces a ajouter
 *
 * Retour       : Pointeur sur la nouvelle (premiere) face(s)
 *
 * Remarques    :
 *---------------------------------------------------------------------------------------------------------------
*/
TFace *Model_ObjectFaceAdd(T3DObject *Obj,int Nb) {

   TFace       *fc;
   unsigned int f;

   Obj->NFc+=Nb;
   if (!(Obj->Fc=(TFace*)realloc(Obj->Fc,Obj->NFc*sizeof(TFace)))) {
      return(NULL);
   }

   for(f=Obj->NFc-Nb;f<Obj->NFc;f++) {
      fc=&Obj->Fc[f];
      fc->NIdx=0;
      fc->Idx=NULL;
      fc->Mt=NULL;
      fc->Name=NULL;
   }
   return(&Obj->Fc[Obj->NFc-Nb]);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_FaceFind>
 * Creation     : Juillet 2012 J.P. Gauthier
 *
 * But          : Rechercher un objet.
 *
 * Parametres   :
 *   <Model>    : Model dans lequel rechercher
 *   <Name>     : Nom de l'objet a rechercher
 *   <Object>   : Retour objet conetnant la face
 *
 * Retour       : Pointeur sur la face (ou NULL si non-existant)
 *
 * Remarques    :
 *---------------------------------------------------------------------------------------------------------------
*/
TFace *Model_FaceFind(T3DModel *Model,char *Name,T3DObject **Obj) {

   TFace *fc=NULL;
   int    o,f;

   for(o=0;o<Model->NObj;o++) {
      for(f=0;f<Model->Obj[o].NFc;f++) {
         if (Model->Obj[o].Fc[f].Name && strncmp(Model->Obj[o].Fc[f].Name,Name,XMLSTRINGSIZE)==0) {
            fc=&Model->Obj[o].Fc[f];
            break;
         }
      }
      if (fc) break;
   }
   if (Obj) {
      *Obj=fc?&Model->Obj[o]:NULL;
   }
   return(fc);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_MaterialAdd>
 * Creation     : Juillet 2012 J.P. Gauthier
 *
 * But          : Ajout de materiel a un modele.
 *
 * Parametres   :
 *   <Model>    : Model
 *   <Nb>       : Nombre de meteriel a ajouter
 *
 * Retour       : Pointeur sur le nouveau (premiere) materiel(s)
 *
 * Remarques    :
 *---------------------------------------------------------------------------------------------------------------
*/
TMaterial *Model_MaterialAdd(T3DModel *Model,int Nb) {

   TMaterial   *mt;
   unsigned int m;

   Model->NMt+=Nb;
   if (!(Model->Mt=(TMaterial*)realloc(Model->Mt,Model->NMt*sizeof(TMaterial)))) {
      return(NULL);
   }

   for(m=Model->NMt-Nb;m<Model->NMt;m++) {
      mt=&Model->Mt[m];

      mt->Amb[0]=mt->Amb[1]=mt->Amb[2]=mt->Amb[3]=1.0;
      mt->Dif[0]=mt->Dif[1]=mt->Dif[2]=mt->Dif[3]=1.0;
      mt->Spe[0]=mt->Spe[1]=mt->Spe[2]=mt->Spe[3]=1.0;
      mt->Emi[0]=mt->Emi[1]=mt->Emi[2]=mt->Emi[3]=1.0;
      mt->Shi=255.0;
      mt->Alpha=1.0;
      mt->Tex=0;
      mt->Name[0]='\0';
      mt->Path[0]='\0';
      mt->Target[0]='\0';
   }
   return(&Model->Mt[Model->NMt-Nb]);
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

   if (!(c=Model_LoadMDL(Interp,mdl,Path))) {
      if (!(c=Model_LoadOBJ(Interp,mdl,Path))) {
         if (!(c=Model_LoadCityGML(Interp,mdl,Path))) {
            if (!(c=Model_LoadKML(Interp,mdl,Path))) {
               if (!(c=Model_LoadDAE(Interp,mdl,Path))) {
                  if (!(c=Model_Load3DS(Interp,mdl,Path))) {
#ifdef HAVE_FLT
                     c=Model_LoadFLT(Interp,mdl,Path);
#endif
                  }
               }
            }
         }
      }
   }
   
   if (c) Model_NormalCompute(mdl,0);

   Model_Extent(mdl);

   return(c?TCL_OK:TCL_ERROR);
}

void Model_Extent(T3DModel *Model) {

   Matrix4d  mx;
   int       o;

   Vect_Init(Model->Extent[0],1e32,1e32,1e32);
   Vect_Init(Model->Extent[1],-1e32,-1e32,-1e32);

   Model_ExtentObj(Model);

   if (Model->Scn) {
      Matrix_Identity(mx);
      Model_ExtentScene(Model,Model->Scn,mx);
  } else {
     for(o=0;o<Model->NObj;o++) {
        Vect_Min(Model->Extent[0],Model->Extent[0],Model->Obj[o].Extent[0]);
        Vect_Max(Model->Extent[1],Model->Extent[1],Model->Obj[o].Extent[1]);
      }
   }
}

void Model_ExtentScene(T3DModel *Model,T3DScene *Scene,Matrix4d Matrix) {

   Matrix4d  mx,mx2;
   Vect3d    ex[2];
   int       o,s;

   /*If a displacement matrix is specified*/
   if (Scene->Mtx) {
      mx2[0][0]=Scene->Mtx[0];
      mx2[1][0]=Scene->Mtx[1];
      mx2[2][0]=Scene->Mtx[2];
      mx2[3][0]=Scene->Mtx[3];
      mx2[0][1]=Scene->Mtx[4];
      mx2[1][1]=Scene->Mtx[5];
      mx2[2][1]=Scene->Mtx[6];
      mx2[3][1]=Scene->Mtx[7];
      mx2[0][2]=Scene->Mtx[8];
      mx2[1][2]=Scene->Mtx[9];
      mx2[2][2]=Scene->Mtx[10];
      mx2[3][2]=Scene->Mtx[11];
      mx2[0][3]=Scene->Mtx[12];
      mx2[1][3]=Scene->Mtx[13];
      mx2[2][3]=Scene->Mtx[14];
      mx2[3][3]=Scene->Mtx[15];
      Matrix_MxM(mx,Matrix,mx2);
   } else {
      Matrix_Assign(mx,Matrix);
   }

   for(o=0;o<Scene->NObj;o++) {
      /*Apply transform ot scene objects*/
      Matrix_VxM(ex[0],Scene->Obj[o]->Extent[0],mx);
      Matrix_VxM(ex[1],Scene->Obj[o]->Extent[1],mx);

      Vect_Min(Model->Extent[0],Model->Extent[0],ex[0]);
      Vect_Max(Model->Extent[1],Model->Extent[1],ex[1]);
   }

   /*Recursive on sub-scenes*/
   for(s=0;s<Scene->NScn;s++) {
      Model_ExtentScene(Model,&Scene->Scn[s],mx);
   }
}

void Model_ExtentObj(T3DModel *Model) {

   T3DObject *obj;
   unsigned int o,f,i,idx;

   /*Calculate Extent for each object*/
   for (o=0;o<Model->NObj;o++) {
      obj=&(Model->Obj[o]);

      Vect_Init(obj->Extent[0],1e32,1e32,1e32);
      Vect_Init(obj->Extent[1],-1e32,-1e32,-1e32);

      for (f=0;f<obj->NFc;f++) {

         for (i=0;i<obj->Fc[f].NIdx;i++) {
            idx=obj->Fc[f].Idx[i];

            /*Test for overflow, should not happend but I've seen it on some models*/
            if (idx>obj->NVr) {
               App_Log(WARNING,"%s: Vertex overflow\n",__func__);
               break;
            }

            Vect_Min(obj->Extent[0],obj->Extent[0],obj->Vr[idx]);
            Vect_Max(obj->Extent[1],obj->Extent[1],obj->Vr[idx]);
         }
      }
   }
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
      Model_ObjectFree(&M->Obj[i]);
   }

   /*Scene list*/
   Model_SceneFree(M->Scn);

   /*Free projection*/
   if (M->GRef) {
      GeoRef_Destroy(NULL,M->GRef->Name);
   }

   if (M->Name) free(M->Name);
   if (M->Path) free(M->Path);
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
 * Nom          : <Model_NormalCompute>
 * Creation     : Janvier 2003 J.P. Gauthier
 *
 * But          : Calculer les normales par vertex
  *
 * Parametres   :
 *   <M>        : Modele
 *   <Force>    : Force recalculation
 *
 * Retour       :
 *
 * Remarques    :
 *---------------------------------------------------------------------------------------------------------------
*/
void Model_NormalCompute(T3DModel *M,int Force) {

   int v,o,f,n;
   T3DObject *obj;
   TFace     *fc;

   Vect3f nr,vr[3];

   for (o=0;o<M->NObj;o++) {
      obj=&(M->Obj[o]);

      if (!Force && (obj->Nr || !obj->NVr)) {
         continue;
      }

      if (!obj->Nr)
         obj->Nr=(Vect3f*)malloc(obj->NVr*sizeof(Vect3f));

      if (obj->Nr) {
         for (v=0;v<obj->NVr;v++) {
            Vect_Clear(obj->Nr[v]);
         }

         // Calculate face normal
         for (f=0;f<obj->NFc;f++) {
            fc=&obj->Fc[f];

            // Calculate face normal
            if (fc->NIdx>2) {
               Vect_Assign(vr[0],obj->Vr[fc->Idx[0]]);
               Vect_Assign(vr[1],obj->Vr[fc->Idx[1]]);
               Vect_Assign(vr[2],obj->Vr[fc->Idx[2]]);

               Vect_Substract(vr[1],vr[1],vr[0]);
               Vect_Substract(vr[0],vr[2],vr[0]);
               Vect3f_CrossProduct(nr,vr[0],vr[1]);
               Vect3f_Normalize(nr);
            } else {
               Vect_Init(nr,0.0,0.0,0.0);
            }

            // Add to vertex normal
            for (n=0;n<fc->NIdx;n++) {
               Vect_Add(obj->Nr[fc->Idx[n]],obj->Nr[fc->Idx[n]],nr);
            }
         }

         // Normalize per vertex normals
         for (v=0;v<obj->NVr;v++) {
            Vect3f_Normalize(obj->Nr[v]);
         }
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_LOD>
 * Creation     : Mai 2002 J.P. Gauthier
 *
 * But          : Verifier le niveau de details selon la vue
 *
 * Parametres  :
 *   <Proj>     : La projection courante
 *   <VP>       : Le viewport ou le rendu doit etre fait
 *   <M>        : Modele
 *   <Extent>   : Etendue du Modele
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Model_LOD(Projection *Proj,ViewportItem *VP,T3DModel *M,Vect3d *Extent) {

   Vect3d ex[4],lim,min,max;

   if (!M->GRef) {
      return(1);
   }

   ex[0][0]=Extent[0][0];ex[0][1]=Extent[0][1];ex[0][2]=Extent[0][2];
   ex[1][0]=Extent[0][0];ex[1][1]=Extent[1][1];ex[1][2]=Extent[0][2];
   ex[2][0]=Extent[1][0];ex[2][1]=Extent[0][1];ex[2][2]=Extent[0][2];
   ex[3][0]=Extent[1][0];ex[3][1]=Extent[1][1];ex[3][2]=Extent[1][2];


   M->GRef->Project(M->GRef,ex[0][0],ex[0][1],&ex[0][1],&ex[0][0],1,0);
   M->GRef->Project(M->GRef,ex[1][0],ex[1][1],&ex[1][1],&ex[1][0],1,0);
   M->GRef->Project(M->GRef,ex[2][0],ex[2][1],&ex[2][1],&ex[2][0],1,0);
   M->GRef->Project(M->GRef,ex[3][0],ex[3][1],&ex[3][1],&ex[3][0],1,0);
   Proj->Type->Project(Proj,(GeoVect*)ex,NULL,4);

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
int Model_Render(Projection *Proj,ViewportItem *VP,T3DModel *M) {

   unsigned int i,j,o,idx;
   Vect3d       vr;
   Vect3f       vrf;
   T3DObject   *obj;
   TMaterial   *mt;
   char        *path=NULL;

   extern GLint Texture_Read(char *File);

   if (!M || !M->Spec) {
      App_Log(ERROR,"%s: Invalid model object\n",__func__);
      return(0);
   }

   if (!M->Spec->Active) {
      return(0);
   }

   // Parse material for texture setup
   for(i=0;i<M->NMt;i++) {
      if (!M->Mt[i].Tex && strlen(M->Mt[i].Path)) {
         path=strpath(M->Path,M->Mt[i].Path);
         M->Mt[i].Tex=Texture_Read(path);
         if (path) free(path);path=NULL;
      }
   }
   
   // Create display lists
   if (!M->Obj[0].GLId) {

      for(o=0;o<M->NObj;o++) {
         obj=&M->Obj[o];

         /*If this object has vertices and has'nt been processed*/
         if (obj->Vr && !obj->GLId) {

            obj->GLId=glGenLists(1);
            glNewList(obj->GLId,GL_COMPILE);

            for (i=0;i<obj->NFc;i++) {

               if ((mt=obj->Fc[i].Mt)) {
// TODO: don'tuse model material, allows more flexibility for user
//                  glMaterialf(GL_FRONT_AND_BACK,GL_SHININESS,mt->Shi);
//                  glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,mt->Amb);
//                  glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,mt->Dif);
//                  glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,mt->Spe);
//                  glMaterialfv(GL_FRONT_AND_BACK,GL_EMISSION,mt->Emi);
                  if (mt->Tex>0) {
                     glBindTexture(GL_TEXTURE_2D,mt->Tex);
                     glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);
                     glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_REPEAT);
                  }
               }

               switch(obj->Fc[i].NIdx) {
                  case 1:  glBegin(GL_POINTS);    break;
                  case 2:  glBegin(GL_LINES);     break;
                  case 3:  glBegin(GL_TRIANGLES); break;
                  case 4:  glBegin(GL_QUADS);     break;
                  default: glBegin(GL_POLYGON);   break;
               }

               for (j=0;j<obj->Fc[i].NIdx;j++) {
                  idx=obj->Fc[i].Idx[j];

                  // Test for overflow, should not happend but I've seen it on some models
                  if (idx>obj->NVr) {
                     break;
                  }
                  if (obj->Tx && mt && mt->Tex>0) glTexCoord2fv(obj->Tx[idx]);
                  if (obj->Nr)                    glNormal3fv(obj->Nr[idx]);
// TODO: Necessary ?                 if (obj->Cl)                    glColor4fv(obj->Cl[idx]);

                  // Projection to georef
                  if (M->GRef) {
                     M->GRef->Project(M->GRef,obj->Vr[idx][0],obj->Vr[idx][1],&M->Co.Lat,&M->Co.Lon,1,0);
                     M->Co.Elev=obj->Vr[idx][2];
                     Proj->Type->Project(Proj,(GeoVect*)&M->Co,(GeoVect*)&vr,1);
                     Vect_Assign(vrf,vr);
                     glVertex3fv(vrf);
                  } else {
                     glVertex3fv(obj->Vr[idx]);
                  }
               }
               glEnd();
            }
            glEndList();
         }
      }
   }

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();

   // Position the model within geography
   if (!M->GRef) {
      Proj->Type->Locate(Proj,M->Pos[0],M->Pos[1],1);
      // Positionner le modele
      glTranslatef(0.0,0.0,(M->Pos[2]*Proj->Scale+EARTHRADIUS)/EARTHRADIUS);
      // On suppose que le modele est en metres alors on scale par rapport a la terre
      glScalef(M->Meter/EARTHRADIUS,M->Meter/EARTHRADIUS,M->Meter/EARTHRADIUS);
   }

   // Local matrix manipulation
   glTranslatef(M->MatrixT[0],M->MatrixT[1],M->MatrixT[2]);
   glScalef(M->MatrixS[0],M->MatrixS[1],M->MatrixS[2]);
   glRotatef(M->MatrixR[0],1.0,0.0,0.0);
   glRotatef(M->MatrixR[1],0.0,0.0,1.0);
   glRotatef(M->MatrixR[2],0.0,1.0,0.0);

   if (Model_LOD(Proj,VP,M,M->Extent)) {
      glEnable(GL_COLOR_MATERIAL);
      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);
      glEnable(GL_DEPTH_TEST);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);


      if (M->Scn) {
        Model_RenderScene(Proj,VP,M,M->Scn);
      } else {
        for(o=0;o<M->NObj;o++) {
            Model_RenderObject(Proj,VP,M,&M->Obj[o]);
         }
      }

      glDisable(GL_COLOR_MATERIAL);
      glDisable(GL_LIGHTING);
      glDisable(GL_LIGHT0);
      glDisable(GL_DEPTH_TEST);
      glDisable(GL_BLEND);
   }
   
   glPopMatrix();

   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_RenderObject>
 * Creation     : Mai 2002 J.P. Gauthier
 *
 * But          : Rendu d'un object.
 *
 * Parametres  :
 *   <Proj>     : La projection courante
 *   <VP>       : Le viewport ou le rendu doit etre fait
 *   <M>        : Modele a afficher
 *   <Obj>      : Object a afficher
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model_RenderObject(Projection *Proj,ViewportItem *VP,T3DModel *M,T3DObject *Obj) {

   if (Obj && Obj->GLId && Model_LOD(Proj,VP,M,Obj->Extent)) {
      if (M->Spec->RenderFace) {

         glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
         
         if (M->Mt) {
            glMaterialf(GL_FRONT_AND_BACK,GL_SHININESS,M->Mt[0].Shi);
            glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,M->Mt[0].Amb);
            glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,M->Mt[0].Dif);
            glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,M->Mt[0].Spe);
            glMaterialfv(GL_FRONT_AND_BACK,GL_EMISSION,M->Mt[0].Emi);
            if (M->Spec->RenderTexture) {
   //            glColor4f(1.0,1.0,1.0,M->Mt[0].Dif[3]);
               glEnable(GL_TEXTURE_2D);
            } else {
               glColor4f(M->Mt[0].Dif[0],M->Mt[0].Dif[1],M->Mt[0].Dif[2],M->Mt[0].Dif[3]);
            }
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

         glCallList(Obj->GLId);
         glDisable(GL_TEXTURE_2D);
         glDisable(GL_POLYGON_OFFSET_FILL);
      }

      if (M->Spec->Outline && M->Spec->Width) {
         glDisable(GL_LIGHTING);
         glDash(&M->Spec->Dash);
         glLineWidth(ABS(M->Spec->Width));
         glColor4us(M->Spec->Outline->red,M->Spec->Outline->green,M->Spec->Outline->blue,M->Spec->Alpha*655.35);
         glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
         glCallList(Obj->GLId);
         glEnable(GL_LIGHTING);        
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_RenderScene>
 * Creation     : Mai 2002 J.P. Gauthier
 *
 * But          : Rendu d'une scene.
 *
 * Parametres  :
 *   <Proj>     : La projection courante
 *   <VP>       : Le viewport ou le rendu doit etre fait
 *   <M>        : Modele a afficher
 *   <Scene>    : Scene a afficher
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Model_RenderScene(Projection *Proj,ViewportItem *VP,T3DModel *M,T3DScene *Scene) {

   int i;

   ModelSceneDepth++;

   if (GLRender->GLDebug) {
      for(i=0;i<ModelSceneDepth;i++) fprintf(stderr,"   ");
      fprintf(stderr,"(DEBUG) Rendering scene: %s\n",Scene->Name);
   }

   /*If a displacement matrix is specified*/
   if (Scene->Mtx) {
      glPushMatrix();
      glMultTransposeMatrixf(Scene->Mtx);
   }

   /*Display scene objects*/
   for(i=0;i<Scene->NObj;i++) {
      Model_RenderObject(Proj,VP,M,Scene->Obj[i]);
   }

   /*Recursive on sub-scenes*/
   for(i=0;i<Scene->NScn;i++) {
      Model_RenderScene(Proj,VP,M,&Scene->Scn[i]);
   }

   if (Scene->Mtx) {
      glPopMatrix();
   }

   ModelSceneDepth--;
}

int Model_Grid(Tcl_Interp *Interp,TData *Data,T3DModel *M,T3DScene *Scene,TDef_InterpV Mode) {

   int       i;
   T3DScene *scn;

   scn=Scene?Scene:M->Scn;

   ModelSceneDepth++;

   /*If a displacement matrix is specified
   if (Scene->Mtx) {
      glPushMatrix();
      glMultTransposeMatrixf(Scene->Mtx);
   }*/

   if (scn) {
      // Process scene objects
      for(i=0;i<scn->NObj;i++) {
         Model_GridObject(Data,M,scn->Obj[i],Mode);
      }

      // Recursive on sub-scenes
      for(i=0;i<scn->NScn;i++) {
         Model_Grid(Interp,Data,M,&scn->Scn[i],Mode);
      }
   } else {
      // Process model objects
      for(i=0;i<M->NObj;i++) {
         Model_GridObject(Data,M,&(M->Obj[i]),Mode);
      }  
   }

   ModelSceneDepth--;

   return(TCL_OK);
}

int Model_GridObject(TData *Data,T3DModel *M,T3DObject *Obj,TDef_InterpV Mode) {

   Vect3d *v,extent[2];
   Coord  co;
   int    f,p,idx,n=0;

   if (Obj->Vr) {

      for (f=0;f<Obj->NFc;f++) {

         if (!Obj->Fc[f].NIdx)
            continue;

         // Project face in data space
         v=VBuffer_Alloc(Obj->Fc[f].NIdx);
         Vect_Init(extent[0],1e32,1e32,1e32);
         Vect_Init(extent[1],-1e32,-1e32,-1e32);
         n=0;

         for (p=0;p<Obj->Fc[f].NIdx;p++) {
            idx=Obj->Fc[f].Idx[p];

            // Test for overflow, should not happend but I've seen it on some models
            if (idx>Obj->NVr) {
               App_Log(WARNING,"%s: Wrong number of vertices (%i>%i)\n",__func__,idx,Obj->NVr);
               break;
            }
            
            // Projection to georef
            if (M->GRef) {
               M->GRef->Project(M->GRef,Obj->Vr[idx][0],Obj->Vr[idx][1],&co.Lat,&co.Lon,1,1);
            } else {
               co.Lat=M->Pos[0]+M2DEG(Obj->Vr[idx][1]*M->Meter);
               co.Lon=M->Pos[1]+M2DEG(Obj->Vr[idx][0]*M->Meter);
            }
            co.Elev=Obj->Vr[idx][2];

            Data->GRef->UnProject(Data->GRef,&v[n][0],&v[n][1],co.Lat,co.Lon,1,1);
            v[n][2]=Obj->Vr[idx][2];
            
            Vect_Min(extent[0],extent[0],v[n]);
            Vect_Max(extent[1],extent[1],v[n]);
            extent[0][2]=extent[1][2]=0.0;
            n++;
         }

         // Process the face
         Model_Rasterize(Data->Def,Data->GRef,v,n,extent,0.0);
      }
   }
   return(1);
}

   /*Calculate plane equation*/
void Vect_PlaneEquation(Vect4d Plane,Vect3d V0,Vect3d V1,Vect3d V2) {
   Plane[0]=V0[1]*(V1[2]-V2[2]) + V1[1]*(V2[2]-V0[2]) + V2[1]*(V1[2]-V1[2]);
   Plane[1]=V0[2]*(V1[0]-V2[0]) + V1[2]*(V2[0]-V0[0]) + V2[2]*(V0[0]-V1[0]);
   Plane[2]=V0[0]*(V1[1]-V2[1]) + V1[0]*(V2[1]-V0[1]) + V2[0]*(V0[1]-V1[1]);
   Plane[2]=-(V0[0]*(V1[1]*V2[2]-V2[1]*V1[2]) + V1[0]*(V2[1]*V0[2]-V0[1]*V2[2]) + V2[0]*(V0[1]*V1[2]-V1[1]*V0[2]));
}
   /*Intersect with plane*/
int Vect_PlaneIntersect(Vect4d Plane,Vect3d V0,Vect3d V1) {

   double p,denom;

   denom=(Plane[0]*(V0[0]-V1[0])+Plane[1]*(V0[1]-V1[1])+Plane[2]*(V0[2]-V1[2]));
   if (denom!=0.0) {
      p=(Plane[0]*V0[0]+Plane[1]*V0[1]+Plane[2]*V0[2]+Plane[3])/denom;
      return(p>0 && p<=1);
   }
   return(0);
}

#define TRIAREA(V0,V1,V2) (abs(V0[0]*V1[1]+V1[0]*V2[1]+V2[0]*V0[1]-V0[0]*V2[1]-V2[0]*V1[1]-V1[0]*V0[1])/2)

   /*Intersect with face*/
int Vect_PlaneWithin(Vect3d Point,Vect3d V0,Vect3d V1,Vect3d V2) {

   double a,b,c,d;

   a=TRIAREA(V0,V1,V2);
   b=TRIAREA(V0,V1,Point);
   c=TRIAREA(V0,Point,V2);
   d=TRIAREA(Point,V1,V2);

   return(abs(b+c+d-a)<TINY_VALUE);
}

int PointInPoly(int NVr,Vect3d *Vr,double X,double Y) {
   
  int i,j,c=0;
  
  for (i=0, j=NVr-1; i<NVr; j=i++) {
    if (((Vr[i][1]>Y)!=(Vr[j][1]>Y)) && (X<(Vr[j][0]-Vr[i][0])*(Y-Vr[i][1])/(Vr[j][1]-Vr[i][1])+Vr[i][0]))
       c = !c;
  }
  return(c);
}

void Model_Rasterize(TDef *Def,TGeoRef *Ref,Vect3d *Vr,int NVr,Vect3d *Ex,double Value) {

   double val,zmax=-1e32;
   int    v,x,y,i;
//   Vect4d plane;
//   Vect3d v0,v1;

   if (!Vr || !Def || NVr<3)
      return;

   // Vertex in cell 
   for(v=0;v<NVr;v++) {
      x=lrint(Vr[v][0]);
      y=lrint(Vr[v][1]);
      zmax=fmax(Vr[v][2],zmax);
      
      if (DEF2DIN(Def,x,y)) {
         i=FIDX2D(Def,x,y);
         Def_Get(Def,0,i,val);
         if (Vr[v][2]>val)
            Def_Set(Def,0,i,Vr[v][2]);
      }
   }
   
   // Cell in polygon
   for(y=lrint(Ex[0][1]);y<=lrint(Ex[1][1]);y++) {
      for(x=lrint(Ex[0][0]);x<=lrint(Ex[1][0]);x++) {
         if (DEF2DIN(Def,x,y)) {
            i=FIDX2D(Def,x,y);
            Def_Get(Def,0,i,val);
            if (zmax>val) {
               if (PointInPoly(NVr,Vr,x,y))         { Def_Set(Def,0,i,zmax); continue; }
               if (PointInPoly(NVr,Vr,x-0.5,y-0.5)) { Def_Set(Def,0,i,zmax); continue; }
               if (PointInPoly(NVr,Vr,x-0.5,y+0.5)) { Def_Set(Def,0,i,zmax); continue; }
               if (PointInPoly(NVr,Vr,x+0.5,y+0.5)) { Def_Set(Def,0,i,zmax); continue; }
               if (PointInPoly(NVr,Vr,x+0.5,y-0.5)) { Def_Set(Def,0,i,zmax); continue; }
            }
         }
      }
   }
   
   // Using extent
//   for(y=lrint(Ex[0][1]);y<=lrint(Ex[1][1]);y++) {
//      for(x=lrint(Ex[0][0]);x<=lrint(Ex[1][0]);x++) {
//         if (FIN2D(Def,x,y))
//            Def_Set(Def,0,FIDX2D(Def,x,y),Value);
//      }
//   }

//    Vect_PlaneEquation(plane,Vr[0],Vr[1],Vr[2]);
//    for(z=(Ex[0][2]<0?0:Ex[0][2]);z<=(Ex[1][2]>=Def->NK-1?Def->NK-1:Ex[1][2]);z++) {
//       for(y=(Ex[0][1]<0?0:Ex[0][1]);y<=(Ex[1][1]>=Def->NJ-1?Def->NJ-1:Ex[1][1]);y++) {
//          for(x=(Ex[0][0]<0?0:Ex[0][0]);x<=(Ex[1][0]>=Def->NI-1?Def->NI-1:Ex[1][0]);x++) {
//             Vect_Init(v0,x,y,z-0.5);
//             Vect_Init(v1,x,y,z+0.5);
//             if (Vect_PlaneIntersect(plane,v0,v1) && Vect_PlaneWithin(v0,Vr[0],Vr[1],Vr[2]))
//                Def_Set(Def,0,FIDX2D(Def,x,y),Value);
//          }
//       }
//    }
}
