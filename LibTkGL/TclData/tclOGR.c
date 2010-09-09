/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : tclOGR.c
 * Creation     : Juillet 2004 - J.P. Gauthier
 *
 * Description  : Fonctions de manipulations et d'affichage de fichiers vectoriel.
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

#include <string.h>
#include <strings.h>

#include "tclOGR.h"
#include "tclGDAL.h"
#include "Projection.h"

/*Table contenant la liste des fichiers en memoire*/
static Tcl_HashTable OGR_FileTable;
static Tcl_HashTable OGR_LayerTable;
static Tcl_HashTable OGR_GeometryTable;
static int OGRInit=0;
static long GeometryNo=0;

static int OGR_FileCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int OGR_LayerCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int OGR_GeometryCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);

/*----------------------------------------------------------------------------
 * Nom      : <tclOGR_Init>
 * Creation : Juin 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser le package lors de l'inclusion par Tcl.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int TclOGR_Init(Tcl_Interp *Interp) {

   if (!OGRInit++) {
      OGRRegisterAll();

      Tcl_InitHashTable(&OGR_FileTable,TCL_STRING_KEYS);
      Tcl_InitHashTable(&OGR_LayerTable,TCL_STRING_KEYS);
      Tcl_InitHashTable(&OGR_GeometryTable,TCL_STRING_KEYS);
   }

   Tcl_CreateObjCommand(Interp,"ogrfile",OGR_FileCmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);
   Tcl_CreateObjCommand(Interp,"ogrlayer",OGR_LayerCmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);
   Tcl_CreateObjCommand(Interp,"ogrgeometry",OGR_GeometryCmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_GeometryGet>
 * Creation     : Juillet 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Obtenir une bande en fonction de son nom dans la table de Tcl
 *
 * Parametres   :
 *   <Name>     : Nom de l'objet bande a obtenir.
 *
 * Retour       : Une structure OGR_Layer ou un pointeur NULL si rien trouve.
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
OGRGeometryH* OGR_GeometryGet(char *Name) {

   return((OGRGeometryH)TclY_HashGet(&OGR_GeometryTable,Name));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_GeometryPut>
 * Creation     : Juillet 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Obtenir une bande en fonction de son nom dans la table de Tcl
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de l'objet bande a obtenir.
 *   <Feature>  : Feature.
 *
 * Retour       : Nom de la geometry.
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
Tcl_Obj* OGR_GeometryPut(Tcl_Interp *Interp,char *Name,OGRGeometryH Geometry) {

   char buf[64];

   if (Geometry) {
      if (!Name) {
         sprintf(buf,"GEOMETRY_____%li",GeometryNo++);
         Name=buf;
      }

      if (TclY_HashSet(Interp,&OGR_GeometryTable,Name,Geometry)==TCL_ERROR) {
         return(NULL);
      }
      return(Tcl_NewStringObj(Name,-1));
   } else {
      Tcl_AppendResult(Interp,"\n   OGR_GeometryPut: Invalid Geometry: \"",Name, "\"",(char *)NULL);
      return(NULL);
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_GeometryDestroy>
 * Creation     : Juillet 2005 J.P. Gauthier
 *
 * But          : Destruction d'une geometry OGR a partir de son nom.
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
int OGR_GeometryDestroy(Tcl_Interp *Interp,char *Name) {

   OGRGeometryH *geom=NULL;

   if ((geom=(OGRGeometryH*)TclY_HashDel(&OGR_GeometryTable,Name))) {
      OGR_G_DestroyGeometry(geom);
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_GeometryCmd>
 * Creation     : Juillet 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Effectuer les commandes du package
 *
 * Parametres    :
 *  <clientData> : Nom de l'objet
 *  <Interp>     : Interpreteur Tcl
 *  <Objc>       : Nombre d'arguments
 *  <Objv>       : Pointeur sur la liste des arguments
 *
 * Retour        : Code de retour standard TCL
 *
 * Remarques     :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int OGR_GeometryCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   OGRGeometryH         g0,g1;
   OGRwkbGeometryType   t;
   int                  idx,n;
   Tcl_Obj             *obj;

   static CONST char *sopt[] = { "create","copy","free","stats","define","is","all",NULL };
   enum                opt { CREATE,COPY,FREE,STATS,DEFINE,IS,ALL };

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
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"geometry type [Point,3D Point,Line String,3D Line String,Polygon,3D Polygon,Multi Point,3D Multi Point,Multi Line String,3D Multi Line String,Multi Polygon,3D Multi Polygon,Geometry Collection,3D Geometry Collection,Linear Ring]");
            return TCL_ERROR;
         }
         t=OGR_GeometryNameToType(Tcl_GetString(Objv[3]));
         if (t==wkbNone) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Invalid geometry type, must be Point,3D Point,Line String,3D Line String,Polygon,3D Polygon,Multi Point,3D Multi Point,Multi Line String,3D Multi Line String,Multi Polygon,3D Multi Polygon,Geometry Collection,3D Geometry Collection,Linear Ring\"",(char*)NULL);
            return(TCL_ERROR);
         }
         g0=OGR_G_CreateGeometry(t);
         obj=OGR_GeometryPut(Interp,Tcl_GetString(Objv[2]),g0);
         if (!obj)
            return(TCL_ERROR);
         Tcl_SetObjResult(Interp,obj);
         break;

      case COPY:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"geometryto geometryfrom");
            return(TCL_ERROR);
         }

         if (g0=OGR_GeometryGet(Tcl_GetString(Objv[3]))) {
            g1=OGR_G_Clone(g0);
            obj=OGR_GeometryPut(Interp,Tcl_GetString(Objv[2]),g1);
            if (!obj)
               return(TCL_ERROR);
            Tcl_SetObjResult(Interp,obj);
         } else {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Invalid geometry",(char*)NULL);
            return(TCL_ERROR);
         }
         break;

      case FREE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"geom");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            OGR_GeometryDestroy(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case STATS:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"geometry ?option?");
            return TCL_ERROR;
         }
         return(OGR_GeometryStat(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case DEFINE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"geometry ?option?");
            return TCL_ERROR;
         }
         return(OGR_GeometryDefine(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case IS:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"geometry");
            return TCL_ERROR;
         }
         if (OGR_GeometryGet(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case ALL:
         TclY_HashAll(Interp,&OGR_GeometryTable);
         break;
   }
   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerCmd>
 * Creation     : Juillet 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Effectuer les commandes du package
 *
 * Parametres    :
 *  <clientData> : Nom de l'objet
 *  <Interp>     : Interpreteur Tcl
 *  <Objc>       : Nombre d'arguments
 *  <Objv>       : Pointeur sur la liste des arguments
 *
 * Retour        : Code de retour standard TCL
 *
 * Remarques     :
 *
 *---------------------------------------------------------------------------------------------------------------
*/

static int OGR_LayerCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   double        x,y,lat,lon;
   int           idx,idxfi,all,n;
   long          f;
   GDAL_Band    *band;
   TData        *field;
   OGR_Layer    *layer;
   OGRGeometryH  geom;
   TGeoRef      *ref=NULL;
   TDataDef     *def=NULL;
   TDataSpec    *spec=NULL;
   Tcl_Obj      *lst,*obj;

   static CONST char *modepick[] = { "INTERSECT","INSIDE","OUTSIDE","NEAREST",NULL };
   static CONST char *sopt[] = { "create","free","sync","clean","clear","read","write","import","delete","interp","configure","stats","define","project","unproject","pick","sqlselect","is","all","wipe",NULL };
   enum                opt { CREATE,FREE,SYNC,CLEAN,CLEAR,READ,WRITE,IMPORT,DELETE,INTERP,CONFIGURE,STATS,DEFINE,PROJECT,UNPROJECT,PICK,SQLSELECT,IS,ALL,WIPE };

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
         if (Objc!=5 && Objc!=6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"file Id layer [georef]");
            return(TCL_ERROR);
         }
         if (Objc==6) {
            ref=GeoRef_Get(Tcl_GetString(Objv[5]));
            if (!ref) {
               Tcl_AppendResult(Interp,"\n   OGR_LayerCmd: invalid georeference object",(char*)NULL);
               return(TCL_ERROR);
            }
         } else {
            ref=GeoRef_WKTSetup(0,0,0,0,NULL,NULL,0,0,0,0,NULL,NULL,NULL,NULL);
         }
         if (!OGR_LayerInstanciate(OGR_FileGet(Interp,Tcl_GetString(Objv[2])),OGR_LayerCreate(Interp,Tcl_GetString(Objv[3])),Tcl_GetString(Objv[4]),ref)) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerCmd : Unable to create layer",(char*)NULL);
            return(TCL_ERROR);
         }
         break;

      case READ:
         if (Objc<5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer file index");
            return(TCL_ERROR);
         }
         Tcl_GetIntFromObj(Interp,Objv[4],&idxfi);
         return(OGR_LayerRead(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3]),idxfi));
         break;

      case WRITE:
         if (Objc<4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer file");
            return(TCL_ERROR);
         }
         return(OGR_LayerWrite(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3])));
         break;

      case SYNC:
         layer=OGR_LayerGet(Tcl_GetString(Objv[2]));
         for(f=0;f<layer->NFeature;f++) {
            OGR_L_SetFeature(layer->Layer,layer->Feature[f]);
         }
         break;

      case IMPORT:
         if (Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer fields");
            return(TCL_ERROR);
         }

         return(OGR_LayerImport(Interp,OGR_LayerGet(Tcl_GetString(Objv[2])),Objv[3]));
         break;

      case INTERP:
         if (Objc>9 || Objc<7) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer data field Type [Split] [Final] [Index list variable]");
            return(TCL_ERROR);
         }
         field=Data_Get(Tcl_GetString(Objv[3]));
         if (field) {
            ref=field->Ref;
            def=field->Def;
         } else {
            band=GDAL_BandGet(Tcl_GetString(Objv[3]));
            if (band) {
               ref=band->Ref;
               def=band->Def;
            } else {
               Tcl_AppendResult(Interp,"\n   OGR_LayerCmd : invalid data",(char*)NULL);
               return(TCL_ERROR);
            }
         }

         layer=OGR_LayerGet(Tcl_GetString(Objv[2]));
         f=OGR_FD_GetFieldIndex(layer->Def,Tcl_GetString(Objv[4]));
         if (f==-1) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerCmd: Invalid field",(char*)NULL);
            return(TCL_ERROR);
         }

         if (Tcl_GetString(Objv[5])[0]!='A' && Tcl_GetString(Objv[5])[0]!='C' && Tcl_GetString(Objv[5])[0]!='W' && Tcl_GetString(Objv[5])[0]!='I' && Tcl_GetString(Objv[5])[0]!='N') {
            Tcl_AppendResult(Interp,"\n   OGR_LayerCmd : interpolation type, must be, WITHIN, INTERSECT, AVERAGE, CONSERVATIVE or NORMALIZED_CONSERVATIVE",(char*)NULL);
            return(TCL_ERROR);
         }

         n=1;
         obj=NULL;
         if (Objc>6) {
            Tcl_GetIntFromObj(Interp,Objv[6],&n);
            obj=Objv[6];
         }

         all=1;
         obj=NULL;
         if (Objc>7) {
            if (Tcl_GetBooleanFromObj(Interp,Objv[7],&all)==TCL_ERROR) {
               obj=Objv[7];
            }
         }
         if (Objc>8) {
            obj=Objv[8];
         }
         return(OGR_LayerInterp(Interp,layer,f,ref,def,Tcl_GetString(Objv[5])[0],all,n,obj));
         break;

      case FREE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            OGR_LayerDestroy(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case DELETE:
         if (Objc<4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer ids");
            return(TCL_ERROR);
         }
         if (!(layer=OGR_LayerGet(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerCmd: Invalid layer",(char*)NULL);
            return(TCL_ERROR);
         }
         for(n=3;n<Objc;n++) {
            if (Tcl_GetLongFromObj(Interp,Objv[n],&f)==TCL_ERROR) {
               Tcl_AppendResult(Interp,"\n   OGR_LayerCmd: Invalid feature index",(char*)NULL);
               return(TCL_ERROR);
            }

            if (OGR_L_DeleteFeature(layer->Layer,f)!=OGRERR_NONE) {
               Tcl_AppendResult(Interp,"\n   OGR_LayerCmd: Cannot delete feature",(char*)NULL);
               return(TCL_ERROR);
            }
         }
         return(TCL_OK);
         break;

      case CLEAR:
         if (Objc!=4 && Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer field [value]");
            return TCL_ERROR;
         }
         layer=OGR_LayerGet(Tcl_GetString(Objv[2]));
         n=OGR_FD_GetFieldIndex(layer->Def,Tcl_GetString(Objv[3]));
         if (n==-1) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerCmd: Invalid field",(char*)NULL);
            return(TCL_ERROR);
         }
         x=0.0;
         if (Objc==5) {
            Tcl_GetDoubleFromObj(Interp,Objv[4],&x);
         }
         return(OGR_LayerClear(Interp,layer,n,x));
         break;

      case CLEAN:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer");
            return TCL_ERROR;
         }
         OGR_LayerClean(OGR_LayerGet(Tcl_GetString(Objv[2])));
         break;

      case PROJECT:
         if (Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer X Y");
            return TCL_ERROR;
         }

         Tcl_GetDoubleFromObj(Interp,Objv[3],&x);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&y);

         layer=OGR_LayerGet(Tcl_GetString(Objv[2]));
         if (!layer) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerCmd: invalid layer: ",(char *)NULL);
            return TCL_ERROR;
         } else {
            lst=Tcl_NewListObj(0,NULL);
            layer->Ref->Project(layer->Ref,x,y,&lat,&lon,1,1);
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lat));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(lon));
            Tcl_SetObjResult(Interp,lst);
            return TCL_OK;
         }
         break;

      case UNPROJECT:
         if (Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer lat lon");
            return TCL_ERROR;
         }

         Tcl_GetDoubleFromObj(Interp,Objv[3],&lat);
         Tcl_GetDoubleFromObj(Interp,Objv[4],&lon);
         layer=OGR_LayerGet(Tcl_GetString(Objv[2]));
         if (!layer) {
            Tcl_AppendResult(Interp,"\n   OGR_LayerCmd: invalid layer: ",(char *)NULL);
            return TCL_ERROR;
         } else {
            lst=Tcl_NewListObj(0,NULL);
            layer->Ref->UnProject(layer->Ref,&x,&y,lat,lon,1,1);
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(x));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(y));
            Tcl_SetObjResult(Interp,lst);
            return TCL_OK;
         }
         break;

      case CONFIGURE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer ?option?");
            return TCL_ERROR;
         }
         layer=OGR_LayerGet(Tcl_GetString(Objv[2]));
         if (!layer) {
            Tcl_AppendResult(Interp,"invalid layer",(char*)NULL);
            return TCL_ERROR;
         }
         if (strcmp(Tcl_GetString(Objv[3]),"-dataspec")==0) {
            if (Objc==4) {
               if (layer->Spec) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(layer->Spec->Name,-1));
               }
            } else {
               if ((spec=DataSpec_Get(Tcl_GetString(Objv[4])))) {
                  if (layer->Spec) {
                     DataSpec_FreeHash(Interp,layer->Spec->Name);
                  }
                  layer->Spec=spec;
                  spec->NRef++;
               } else {
                  Tcl_AppendResult(Interp,"Obs_Cmd: invalid configuration object",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
         } else {
            if (DataSpec_Config(Interp,layer->Spec,Objc-3,Objv+3)==TCL_OK) {
               return(TCL_OK);
            } else {
               return(TCL_ERROR);
            }
         }
         break;

      case STATS:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer ?option?");
            return TCL_ERROR;
         }
         return OGR_LayerStat(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3);
         break;

      case DEFINE:
         if (Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer ?option?");
            return TCL_ERROR;
         }
         return(OGR_LayerDefine(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case PICK:
         if (Objc!=4 && Objc!=5 && Objc!=6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer coords [all] [INTERSECT|INSIDE|OUTSIDE|NEAREST]");
            return TCL_ERROR;
         }

         all=0;f=0;
         if (Objc>4) {
            Tcl_GetBooleanFromObj(Interp,Objv[4],&all);
            if (Objc>5) {
               if (Tcl_GetIndexFromObj(Interp,Objv[5],modepick,"mode",0,&f)!=TCL_OK) {
                  return(TCL_ERROR);
               }
            }
         }

         /*Recuperation de la geometrie*/
         geom=OGR_GeometryGet(Tcl_GetString(Objv[3]));
         return(OGR_Pick(Interp,OGR_LayerGet(Tcl_GetString(Objv[2])),geom,Objv[3],all,f));
         break;

      case SQLSELECT:
         if (Objc!=5 && Objc!=6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer file sql_statement [geometry filter]");
            return(TCL_ERROR);
         }

         if (Objc==6) {
            return(OGR_LayerSQLSelect(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3]),Tcl_GetString(Objv[4]),Tcl_GetString(Objv[5])));
         } else {
            return(OGR_LayerSQLSelect(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3]),Tcl_GetString(Objv[4]),NULL));
         }
         break;

      case IS:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"layer");
            return TCL_ERROR;
         }
         if (OGR_LayerGet(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case ALL:
         TclY_HashAll(Interp,&OGR_LayerTable);
         break;

      case WIPE:
         TclY_HashWipe(&OGR_LayerTable,(TclY_HashFreeEntryDataFunc*)OGR_LayerFree);
         TclY_HashWipe(&OGR_GeometryTable,(TclY_HashFreeEntryDataFunc*)OGR_G_DestroyGeometry);
         break;
   }
   return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerCreate>
 * Creation     : Juin 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Creation d'un objet OGR_Layer et insertion d'un nouveau nom dans la table.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de la bande a creer
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
OGR_Layer* OGR_LayerCreate(Tcl_Interp *Interp,char *Name) {

   OGR_Layer *layer;

   if (!(layer=(OGR_Layer*)TclY_HashPut(Interp,&OGR_LayerTable,Name,sizeof(OGR_Layer)))) {
      return(NULL);
   }

   if (!(layer->Spec=DataSpec_Create(Interp,NULL))) {
      return(NULL);
   }
   layer->Spec->RenderTexture=1;

   /*Initialisation de la structure layer*/
   layer->Mask         = 0;
   layer->FMask        = 0;
   layer->Topo         = -1;
   layer->Extrude      = -1;
   layer->Map          = -1;
   layer->Active       = 1;
   layer->ExtrudeFactor= 1.0;
   layer->TopoFactor   = 1.0;
   layer->Loc          = NULL;
   layer->SQLed        = NULL;
   layer->Min          = 0.0;
   layer->Max          = 0.0;

   layer->GFeature   =0;
   layer->NFeature   =0;
   layer->LFeature   =0;
   layer->NSFeature  =0;
   layer->SFeature   =NULL;
   layer->Feature    =NULL;
   layer->Label      =NULL;
   layer->NLabel     =0;
   layer->Select     =NULL;

   layer->Layer      =NULL;
   layer->Feature    =NULL;
   layer->Def        =NULL;
   layer->Ref        =NULL;
   layer->Tag        =NULL;

   Vect_Init(layer->Vr[0],1e32,1e32,1e32);
   Vect_Init(layer->Vr[1],-1e32,-1e32,-1e32);

   return(layer);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerInstanciate>
 * Creation     : Juin 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Creation d'un objet OGR_Layer et insertion d'un nouveau nom dans la table.
 *
 * Parametres   :
 *   <File>     : Fichier OGR
 *   <Layer>    : Layer OGR
 *
 * Retour       : Adresse du layer
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
OGRLayerH OGR_LayerInstanciate(OGR_File *File,OGR_Layer *Layer,char *Name,TGeoRef *Ref){

   char **opt=NULL;

   if (!File || !Layer) {
      return(NULL);
   }

   if (OGR_DS_TestCapability(File->Data,ODsCCreateLayer)) {

      if (Ref) {
         Layer->Ref=Ref;
      } else {
         Layer->Ref=GeoRef_New();
      }
      Layer->Layer=OGR_DS_CreateLayer(File->Data,Name,Layer->Ref->Spatial,wkbUnknown,opt);
      Layer->Def=OGR_L_GetLayerDefn(Layer->Layer);
   }
   return(Layer->Layer);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerDestroy>
 * Creation     : Mai 2004 J.P. Gauthier
 *
 * But          : Destruction d'un layer OGR a partir de son nom.
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
int OGR_LayerDestroy(Tcl_Interp *Interp,char *Name) {

   OGR_Layer *layer=NULL;

   if ((layer=(OGR_Layer*)TclY_HashDel(&OGR_LayerTable,Name))) {
      DataSpec_FreeHash(Interp,layer->Spec->Name);
      OGR_LayerFree(layer);
      free(layer);
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerFree>
 * Creation     : Aout 2004 J.P. Gauthier
 *
 * But          : Liberer la memeoire associe a un layer OGR
 *
 * Parametres   :
 *   <Layer>    : Layer associe
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void OGR_LayerFree(OGR_Layer *Layer) {

   int n;

   for(n=0;n<Layer->NFeature;n++) {
      OGR_F_Destroy(Layer->Feature[n]);
   }

   Layer->Def=NULL;

   if (Layer->Select)   free(Layer->Select);
   if (Layer->SFeature) free(Layer->SFeature);
   if (Layer->Label)    free(Layer->Label);
   if (Layer->Loc)      free(Layer->Loc);
   if (Layer->Ref)      GeoRef_Destroy(NULL,Layer->Ref->Name);
   if (Layer->Tag)      Tcl_DecrRefCount(Layer->Tag);

   if (Layer->SQLed) {
      OGR_DS_ReleaseResultSet(Layer->SQLed,Layer->Layer);
   }

   OGR_LayerClean(Layer);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerGet>
 * Creation     : Juillet 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Obtenir une bande en fonction de son nom dans la table de Tcl
 *
 * Parametres   :
 *   <Name>     : Nom de l'objet bande a obtenir.
 *
 * Retour       : Une structure OGR_Layer ou un pointeur NULL si rien trouve.
 *
 * Remarques    :
 *    - On essaie avec le nom complet et si ca ne marche pas, on essaie en elenvant la derniere partie
 *      apres le dernier point, au cas ou un champs serait specifier (LAYER.FIELD)
 *---------------------------------------------------------------------------------------------------------------
*/
OGR_Layer* OGR_LayerGet(char *Name) {

   OGR_Layer *layer;
   char      *name;

   if (!(layer=(OGR_Layer*)TclY_HashGet(&OGR_LayerTable,Name))) {
      name=strndup(Name,rindex(Name,'.')-Name);
      layer=(OGR_Layer*)TclY_HashGet(&OGR_LayerTable,name);
      free(name);
   };

   return(layer);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerToDef>
 * Creation     : Aout 2010 J.P. Gauthier
 *
 * But          : Retourner la representation TDataDef du champs de la couche.
 *
 * Parametres   :
 *   <Layer>    : Couche
 *   <Field>    : Nom du vecteur
 *
 * Retour       : Representation TDataDef
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
struct TDataDef* OGR_LayerToDef(OGR_Layer *Layer,char *Field) {

   TDataDef  *def=NULL;
   TData_Type type=TD_Unknown;
   int        i,f;
   double     val;

   /*Get the field index*/
   if ((i=OGR_FD_GetFieldIndex(Layer->Def,Field))>-1) {

      /*Get the data type of the field*/
      switch (OGR_Fld_GetType(OGR_FD_GetFieldDefn(Layer->Def,i))) {
         case OFTInteger: type=TD_Int32; break;
         case OFTReal   : type=TD_Float64; break;
         default        : type=TD_Unknown;
      }

      /*If the field is not int or float, exit*/
      if (type!=TD_Unknown) {

         /*Get the data in*/
         def=DataDef_New(Layer->NFeature,1,1,1,type);

         for(f=0;f<Layer->NFeature;f++) {
            val=OGR_F_GetFieldAsDouble(Layer->Feature[f],i);
            Def_Set(def,0,f,val);
         }
      }
   }
   return(def);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <OGR_LayerFromDef>
 * Creation     : Aout 2010 J.P. Gauthier
 *
 * But          : Remettre le contenue d'un DataDef dans un champ de couche.
 *
 * Parametres   :
 *   <Layer>    : Couche
 *   <Field>    : Nom du vecteur
 *   <Def>      : data definition
 *
 * Retour       : Couche valide
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
OGR_Layer *OGR_LayerFromDef(OGR_Layer *Layer,char *Field,TDataDef *Def) {

   int    i,f;
   double val;

   /*Get the field index*/
   if ((i=OGR_FD_GetFieldIndex(Layer->Def,Field))>-1) {

      /*Put the results in*/
      for(f=0;f<Layer->NFeature;f++) {
         Def_Get(Def,0,f,val);
         OGR_F_SetFieldDouble(Layer->Feature[f],i,val);
      }
      return(Layer);
   } else {
      return(NULL);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_FileCmd>
 * Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Appel des commandes relies aux fichiers raster.
 *
 * Parametres     :
 *  <clientData>  : Donnees du module.
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
static int OGR_FileCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   OGR_File  *file;
   Tcl_Obj   *obj;

   int          n,idx,nidx,i,code;
   char        *driver=NULL;
   const char **list=NULL;

   static CONST char *sopt[] = { "open","close","format","driver","filename",NULL };
   enum                opt { OPEN,CLOSE,FORMAT,DRIVER,FILENAME};

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return TCL_ERROR;
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return TCL_ERROR;
   }

   switch ((enum opt)idx) {
      case OPEN:
         if(Objc!=5 && Objc!=6 && Objc!=7) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id mode filename [driver] [options]");
            return(TCL_ERROR);
         }
         if (Objc>=6)
            driver=Tcl_GetString(Objv[5]);

         if (Objc==7) {
            if (Tcl_SplitList(Interp,Tcl_GetString(Objv[6]),&nidx,&list)==TCL_ERROR) {
               Tcl_AppendResult(Interp,"\n   OGR_FileCmd : Invalid list of creation options",(char*)NULL);
               return(TCL_ERROR);
            }
         }

         code=OGR_FileOpen(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3])[0],Tcl_GetString(Objv[4]),driver,(char**)list);
         Tcl_Free((char*)list);
         return(code);
         break;

      case FORMAT:
         obj=Tcl_NewListObj(0,NULL);
         for (i=0;i<OGRGetDriverCount();i++) {
            OGRSFDriverH driver=OGRGetDriver(i);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(OGR_Dr_GetName(driver),-1));
         }
         Tcl_SetObjResult(Interp,obj);
         break;

     case DRIVER:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return TCL_ERROR;
         }
         if (!(file=OGR_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,Tcl_NewStringObj(OGR_Dr_GetName(file->Driver),-1));
         break;

      case CLOSE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fileid");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            OGR_FileClose(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case FILENAME:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         if (!(file=OGR_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,Tcl_NewStringObj(file->Name,0));
         return(TCL_OK);
         break;
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_FileClose>
 * Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Ferme le fichier OGR.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur du fichier
 *
 * Retour:
 *  <TCL_...> : Code de reussite.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int OGR_FileClose(Tcl_Interp *Interp,char *Id) {

   OGR_File *file=NULL;

   if ((file=(OGR_File*)TclY_HashDel(&OGR_FileTable,Id))) {
      OGR_DS_SyncToDisk(file->Data);

      /*For some unknown reason yet, this sgefaults on dataset openned in append mode*/
      if (file->Mode!='a' && file->Mode!='A') {
         OGR_DS_Destroy(file->Data);
      }

      free(file->Id);
      free(file);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_FileGet>
 * Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait l'adresse d'un champs de la liste des champs
 *            connu (HashTable) et en retourne la valeur.
 *
 * Parametres   :
 *  <Interp>    : Interpreteur TCL.
 *  <Name>      : Nom du champ
 *
 * Retour:
 *  <OGR_File>  : Pointeur sur la structure du fichier
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
OGR_File* OGR_FileGet(Tcl_Interp *Interp,char *Id){

   OGR_File *file;

   if (!(file=(OGR_File*)TclY_HashGet(&OGR_FileTable,Id))) {
      if (Interp) {
         Tcl_AppendResult(Interp,"OGR_FileGet: Unknown file",(char *)NULL);
      }
   }
   return(file);
}

int OGR_FilePut(Tcl_Interp *Interp,OGR_File *File){

   Tcl_HashEntry *entry;
   int            new;

   entry=TclY_CreateHashEntry(&OGR_FileTable,File->Id,&new);

   if (!new) {
      Tcl_AppendResult(Interp,"\n   OGR_FilePut: File already openned",(char *)NULL);
      return TCL_ERROR;
   }

   Tcl_SetHashValue(entry,File);

   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_FileOpen>
 * Creation : Juin 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'ouverture d'un fichier HDF.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *  <Mode>    : Mode d'ouverture (R ou W)
 *  <Name>    : Nom du fichier
 *  <Driver>  : Non du fichier
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *  <tcl>     : Liste des parametres des enregistrements contenue dans le fichier
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int OGR_FileOpen(Tcl_Interp *Interp,char *Id,char Mode,char *Name,char *Driver,char **Options) {

   OGRDataSourceH *source=NULL;
   OGRSFDriverH    driver=NULL;
   OGRLayerH       layer;
   OGRFeatureDefnH defn=NULL;
   OGR_File       *file;
   int             i;
   Tcl_Obj        *lst,*sublst;

   if (file=OGR_FileGet(Interp,Id)) {
      if (strcmp(Name,file->Name)==0 && Mode==file->Mode) {
         source=file->Data;
      } else {
         Tcl_AppendResult(Interp,"OGR_FileOpen: Cannot reuse openned file identificator ",Id,(char*)NULL);
         return(TCL_ERROR);
      }
   }

   if (!source) {
      if (Mode=='w' || Mode=='W') {            /*Create Mode*/
         if (!Driver) {
            Tcl_AppendResult(Interp," OGR_FileOpen: Invalid driver ",Driver,(char*)NULL);
            return(TCL_ERROR);
         }

         /* Look for the specified driver */
         for(i=0;i<OGRGetDriverCount();i++) {
            driver=OGRGetDriver(i);
            if (EQUAL(Driver,OGR_Dr_GetName(driver))) break;
         }

         if (!driver) {
            Tcl_AppendResult(Interp," OGR_FileOpen: Invalid driver ",Driver,(char*)NULL);
            return(TCL_ERROR);
         } else if (!OGR_Dr_TestCapability(driver,ODrCCreateDataSource)) {
            Tcl_AppendResult(Interp," OGR_FileOpen: This driver cannot create data sources ",Driver,(char*)NULL);
            return(TCL_ERROR);
         }

         source=OGR_Dr_CreateDataSource(driver,Name,Options);
         if (!source) {
            Tcl_AppendResult(Interp," OGR_FileOpen: Cannot open OGR file for writing, it probably already exist",Name,(char*)NULL);
            return(TCL_ERROR);
         }
      } else if (Mode=='a' ||  Mode=='A') {    /*Append Mode*/
         if (!(source=OGROpen(Name,1,&driver))) {
            Tcl_AppendResult(Interp," OGR_FileOpen: Cannot open OGR file ",Name,(char*)NULL);
            return(TCL_ERROR);
         }
      } else {                                 /*ReadOnly Mode*/
         if (!(source=OGROpen(Name,0,&driver))) {;
            Tcl_AppendResult(Interp," OGR_FileOpen: Cannot open OGR file ",Name,(char*)NULL);
            return(TCL_ERROR);
         }
      }

      file=(OGR_File*)malloc(sizeof(OGR_File));
      file->Mode=Mode;
      file->Id=strdup(Id);
      file->Name=strdup(Name);
      file->Driver=driver;
      file->Data=source;

      OGR_FilePut(Interp,file);
   }

   /* Loop over layers */
   lst=Tcl_NewListObj(0,NULL);
   for(i=0;i<OGR_DS_GetLayerCount(source);i++) {
       layer=OGR_DS_GetLayer(source,i);

       /* Get info about this layer */
       defn=OGR_L_GetLayerDefn(layer);

       sublst=Tcl_NewListObj(0,NULL);
       Tcl_ListObjAppendElement(Interp,sublst,Tcl_NewStringObj(Id,-1));
       Tcl_ListObjAppendElement(Interp,sublst,Tcl_NewIntObj(i));
       Tcl_ListObjAppendElement(Interp,sublst,Tcl_NewStringObj(OGR_FD_GetName(defn),-1));
       Tcl_ListObjAppendElement(Interp,lst,sublst);
   }
   Tcl_SetObjResult(Interp,lst);

   return(TCL_OK);
}
