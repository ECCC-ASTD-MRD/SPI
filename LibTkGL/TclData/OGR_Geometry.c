/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : OGR_Geometry.c
 * Creation     : Juillet 2005 - J.P. Gauthier
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

#include "tclOGR.h"

static Vect3d  **OGR_ArrayNr=NULL;
static Vect3d  *OGR_ArrayVr=NULL;
static Vect3d  *OGR_ArrayEx=NULL;
static int      OGR_ArraySize=0;

/*----------------------------------------------------------------------------
 * Nom      : <OGR_GeometryDefine>
 * Creation : Juillet 2005 J.P. Gauthier - CMC/CMOE
 *
 * But      : Definition des parametres de layer.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Name>    : Nom de la projection
 *  <Objc>    : Nombre d'arguments
 *  <Objv>    : Pointeur sur la liste des arguments
 *
 * Retour     :
 *  <TCL_...> : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int OGR_GeometryDefine(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   OGRGeometryH   geom,subgeom;
   int            i,j,idx,n,t,err;
   char          *buf;
   Vect3d         pt;
   Tcl_Obj       *obj;
   TGeoRef       *ref;

   static CONST char *sopt[] = { "-wkt","-gml","-kml","-geometry","-addgeometry","-space","-dimension","-type","-nb","-name","-points","-addpoint","-georef",NULL };
   enum                opt { WKT,GML,KML,GEOMETRY,ADDGEOMETRY,SPACE,DIMENSION,TYPE,NB,NAME,POINTS,ADDPOINT,GEOREF };

   geom=OGR_GeometryGet(Name);
   if (!geom) {
      Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Geometry name unknown: \"",Name,"\"",(char *)NULL);
      return(TCL_ERROR);
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {

         case WKT:
            if (Objc==1) {
               OGR_G_ExportToWkt(geom,&buf);
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
            } else {
               OGR_G_Empty(geom);

               buf=Tcl_GetString(Objv[++i]);
               err=OGR_G_ImportFromWkt(geom,&buf);
               if (err!=OGRERR_NONE) {
                  Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Unsupported geometry type\"",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case GML:
            if (Objc==1) {
               buf=OGR_G_ExportToGML(geom);
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
               CPLFree(buf);
            } else {
               OGR_G_Empty(geom);

               buf=Tcl_GetString(Objv[++i]);
               geom=OGR_G_CreateFromGML(buf);
               if (!geom) {
                  Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Unsupported geometry type\"",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case KML:
            if (Objc==1) {
               buf=OGR_G_ExportToKML(geom,NULL);
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
               CPLFree(buf);
            } else {
               Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Operation not supported\"",(char*)NULL);
               return(TCL_ERROR);
            }
            break;

         case GEOREF:
            if (Objc==1) {
            } else {
               if (!(ref=GeoRef_Get(Tcl_GetString(Objv[2])))) {
                  Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid georeference",(char*)NULL);
                  return(TCL_ERROR);
               }
               OGR_G_AssignSpatialReference(geom,ref->Spatial);
            }
            break;

         case GEOMETRY:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for (n=0;n<OGR_G_GetGeometryCount(geom);n++) {
                   Tcl_ListObjAppendElement(Interp,obj,OGR_GeometryPut(Interp,NULL,OGR_G_GetGeometryRef(geom,n)));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Objc<3) {
                  Tcl_WrongNumArgs(Interp,2,Objv,"direct geometry");
                  return(TCL_ERROR);
               }

               Tcl_GetBooleanFromObj(Interp,Objv[++i],&t);

               if (Tcl_ListObjLength(Interp,Objv[++i],&n)==TCL_ERROR) {
                  return(TCL_ERROR);
               }

               OGR_G_Empty(geom);

               for(j=0;j<n;) {
                  Tcl_ListObjIndex(Interp,Objv[i],j++,&obj);
                  if (subgeom=OGR_GeometryGet(Tcl_GetString(obj))) {
                     if (t) {
                        err=OGR_G_AddGeometryDirectly(geom,subgeom);
                     } else {
                        err=OGR_G_AddGeometry(geom,subgeom);
                     }
                     if (err!=OGRERR_NONE) {
                        Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Unsupported geometry type\"",(char*)NULL);
                        return(TCL_ERROR);
                     }
                  } else {
                     Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid sub geometry\"",(char*)NULL);
                     return(TCL_ERROR);
                  }
               }
            }
            break;

         case ADDGEOMETRY:
            if (Objc==1) {
               Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid number of geometry\"",(char*)NULL);
               return(TCL_ERROR);
            } else {
               if (Objc<3) {
                  Tcl_WrongNumArgs(Interp,2,Objv,"direct geometry");
                  return(TCL_ERROR);
               }

               Tcl_GetBooleanFromObj(Interp,Objv[++i],&t);

               if (Tcl_ListObjLength(Interp,Objv[++i],&n)==TCL_ERROR) {
                  return(TCL_ERROR);
               }

               for(j=0;j<n;) {
                  Tcl_ListObjIndex(Interp,Objv[i],j++,&obj);
                  if (subgeom=OGR_GeometryGet(Tcl_GetString(obj))) {
                     if (t) {
                        err=OGR_G_AddGeometryDirectly(geom,subgeom);
                     } else {
                        err=OGR_G_AddGeometry(geom,subgeom);
                     }
                     if (err!=OGRERR_NONE) {
                        Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Unsupported geometry type\"",(char*)NULL);
                        return(TCL_ERROR);
                     }
                  } else {
                     Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid sub geometry\"",(char*)NULL);
                     return(TCL_ERROR);
                  }
               }
            }
            break;

         case POINTS:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for (n=0;n<OGR_G_GetPointCount(geom);n++) {
                   OGR_G_GetPoint(geom,n,&pt[0],&pt[1],&pt[2]);
                   Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pt[0]));
                   Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pt[1]));
                   if (OGR_G_GetCoordinateDimension(geom)==3) {
                      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pt[2]));
                   }
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
                if (Tcl_ListObjLength(Interp,Objv[++i],&n)==TCL_ERROR) {
                  return(TCL_ERROR);
               }
               OGR_G_Empty(geom);

               if (n) {
                  if (n%OGR_G_GetCoordinateDimension(geom)!=0) {
                     Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid number of coordinates\"",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  pt[2]=0.0;
                  for(j=0;j<n;) {
                  Tcl_ListObjIndex(Interp,Objv[i],j++,&obj);
                  Tcl_GetDoubleFromObj(Interp,obj,&pt[0]);
                  Tcl_ListObjIndex(Interp,Objv[i],j++,&obj);
                  Tcl_GetDoubleFromObj(Interp,obj,&pt[1]);
                  if (OGR_G_GetCoordinateDimension(geom)==3) {
                     Tcl_ListObjIndex(Interp,Objv[i],j++,&obj);
                     Tcl_GetDoubleFromObj(Interp,obj,&pt[0]);
                     OGR_G_AddPoint(geom,pt[0],pt[1],pt[2]);
                  } else {
                     OGR_G_AddPoint_2D(geom,pt[0],pt[1]);
                  }
                  }
               }
            }
            break;

         case ADDPOINT:
            if (Objc!=3 && Objc!=4) {
               Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid number of coordinates\"",(char*)NULL);
               return(TCL_ERROR);
            } else {
                Tcl_GetDoubleFromObj(Interp,Objv[++i],&pt[0]);
                Tcl_GetDoubleFromObj(Interp,Objv[++i],&pt[1]);
                if (Objc==4) {
                   Tcl_GetDoubleFromObj(Interp,Objv[++i],&pt[2]);
                   OGR_G_AddPoint(geom,pt[0],pt[1],pt[2]);
                } else {
                   OGR_G_AddPoint_2D(geom,pt[0],pt[1]);
                }
            }
            break;

         case NB:
            if (Objc==1) {
               if (!(n=OGR_G_GetGeometryCount(geom))) {
                  n=OGR_G_GetPointCount(geom);
               }
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(n));
            }
            break;

         case TYPE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(OGRGeometryTypeToName(OGR_G_GetGeometryType(geom)),-1));
            } else {
            }
            break;

         case NAME:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(OGR_G_GetGeometryName(geom),-1));
            } else {
            }
            break;

         case SPACE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(OGR_G_GetDimension(geom)));
            } else {
            }
            break;

         case DIMENSION:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(OGR_G_GetCoordinateDimension(geom)));
            } else {
            }
            break;
      }
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_GeometryStat>
 * Creation : Juillet 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Defninitions de diverses fonctions.
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Name>        : Nom de l'objet
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
int OGR_GeometryStat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   OGRGeometryH  g0,g1;
   OGREnvelope   env;
   TGeoRef      *ref,*ref0;
   Tcl_Obj       *lst;
   int           idx,nseg;
   double        dist;

   static CONST char *sopt[] = { "-transform","-distance","-area","-centroid","-extent","-boundary","-buffer",
                                 "-convexhull","-intersection","-union,","-difference","-symmetricdifference",
                                 "-intersect","-equal","-disjoint","-touch","-cross","-within","-contain","-overlap","-simplify",NULL };
   enum                opt { TRANSFORM,DISTANCE,AREA,CENTROID,EXTENT,BOUNDARY,BUFFER,CONVEXHULL,INTERSECTION,
                             UNION,DIFFERENCE,SYMMETRICDIFFERENCE,INTERSECT,EQUAL,DISJOINT,TOUCH,CROSS,WITHIN,CONTAIN,OVERLAP,SIMPLIFY };

   g0=OGR_GeometryGet(Name);
   if (!g0) {
      Tcl_AppendResult(Interp,"\n   OGR_GeometryStat: Geometry name unknown: \"",Name,"\"",(char *)NULL);
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[0],sopt,"option",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {

      case TRANSFORM:
         if (Objc!=2 && Objc!=3) {
            Tcl_WrongNumArgs(Interp,0,Objv,"[georeffrom] georefto");
            return(TCL_ERROR);
         }
         if (Objc==3) {
            ref=GeoRef_Get(Tcl_GetString(Objv[2]));
            if (!ref || !ref->Spatial) {
               Tcl_AppendResult(Interp,"\n   OGR_GeometryStat: Georef name unknown or invalid spatial reference: \"",Tcl_GetString(Objv[2]),"\"",(char *)NULL);
               return(TCL_ERROR);
            }
            ref0=GeoRef_Get(Tcl_GetString(Objv[1]));
            if (!ref0 || !ref0->Spatial) {
               Tcl_AppendResult(Interp,"\n   OGR_GeometryStat: Georef name unknown or invalid spatial reference: \"",Tcl_GetString(Objv[1]),"\"",(char *)NULL);
               return(TCL_ERROR);
            }
            OGR_G_AssignSpatialReference(g0,ref0->Spatial);
         } else {
            ref=GeoRef_Get(Tcl_GetString(Objv[1]));
            if (!ref || !ref->Spatial) {
               Tcl_AppendResult(Interp,"\n   OGR_GeometryStat: Georef name unknown or invalid spatial reference: \"",Tcl_GetString(Objv[1]),"\"",(char *)NULL);
               return(TCL_ERROR);
            }
         }
         OGR_G_TransformTo(g0,ref->Spatial);
         break;

      case DISTANCE:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,2,Objv,"geometry");
            return(TCL_ERROR);
         }
         if (!(g1=OGR_GeometryGet(Tcl_GetString(Objv[1])))) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryStat: Invalid geometry",(char*)NULL);
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(OGR_G_Distance(g0,g1)));
         break;

      case AREA:
         Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(OGR_G_GetArea(g0)));
         break;

      case CENTROID:
         g1=(OGRGeometryH*)malloc(sizeof(OGRGeometryH));
         OGR_G_Centroid(g0,g1);
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,g1));
         break;

      case BOUNDARY:
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_GetBoundary(g0)));
         break;

      case EXTENT:
         lst=Tcl_NewListObj(0,NULL);
         OGR_G_GetEnvelope(g0,&env);
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(env.MinX));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(env.MinY));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(env.MaxX));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(env.MaxY));
         Tcl_SetObjResult(Interp,lst);
         break;

      case BUFFER:
         if (Objc!=3) {
            Tcl_WrongNumArgs(Interp,0,Objv,"dist nseg");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[1],&dist);
         Tcl_GetIntFromObj(Interp,Objv[2],&nseg);
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_Buffer(g0,dist,nseg)));
         break;

      case CONVEXHULL:
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_ConvexHull(g0)));
         break;

      case INTERSECTION:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"geometry");
            return(TCL_ERROR);
         }
         if (!(g1=OGR_GeometryGet(Tcl_GetString(Objv[1])))) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Invalid geometry",(char*)NULL);
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,GPC_OnOGR(GPC_INT,g0,g1)));
//         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_Intersection(g0,g1)));
         break;

      case UNION:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"geometry");
            return(TCL_ERROR);
         }
         if (!(g1=OGR_GeometryGet(Tcl_GetString(Objv[1])))) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Invalid geometry",(char*)NULL);
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,GPC_OnOGR(GPC_UNION,g0,g1)));
//         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_Union(g0,g1)));
         break;

      case DIFFERENCE:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"geometry");
            return(TCL_ERROR);
         }
         if (!(g1=OGR_GeometryGet(Tcl_GetString(Objv[1])))) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Invalid geometry",(char*)NULL);
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,GPC_OnOGR(GPC_DIFF,g0,g1)));
//         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_Difference(g0,g1)));
         break;

      case SYMMETRICDIFFERENCE:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"geometry");
            return(TCL_ERROR);
         }
         if (!(g1=OGR_GeometryGet(Tcl_GetString(Objv[1])))) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Invalid geometry",(char*)NULL);
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,GPC_OnOGR(GPC_XOR,g0,g1)));
//         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_SymmetricDifference(g0,g1)));
         break;

      case INTERSECT:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"geometry");
            return(TCL_ERROR);
         }
         if (!(g1=OGR_GeometryGet(Tcl_GetString(Objv[1])))) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Invalid geometry",(char*)NULL);
            return(TCL_ERROR);
         }
         if (GPC_Intersect(g0,g1,NULL,NULL)) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case EQUAL:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"geometry");
            return(TCL_ERROR);
         }
        if (!(g1=OGR_GeometryGet(Tcl_GetString(Objv[1])))) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Invalid geometry",(char*)NULL);
            return(TCL_ERROR);
         }
         if (OGR_G_Equals(g0,g1)) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case DISJOINT:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"geometry");
            return(TCL_ERROR);
         }
         if (!(g1=OGR_GeometryGet(Tcl_GetString(Objv[1])))) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Invalid geometry",(char*)NULL);
            return(TCL_ERROR);
         }
         if (OGR_G_Disjoint(g0,g1)) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case TOUCH:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"geometry");
            return(TCL_ERROR);
         }
         if (!(g1=OGR_GeometryGet(Tcl_GetString(Objv[1])))) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Invalid geometry",(char*)NULL);
            return(TCL_ERROR);
         }
         if (OGR_G_Touches(g0,g1)) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case CROSS:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"geometry");
            return(TCL_ERROR);
         }
         if (!(g1=OGR_GeometryGet(Tcl_GetString(Objv[1])))) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Invalid geometry",(char*)NULL);
            return(TCL_ERROR);
         }
         if (OGR_G_Crosses(g0,g1)) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case WITHIN:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"geometry");
            return(TCL_ERROR);
         }
         if (!(g1=OGR_GeometryGet(Tcl_GetString(Objv[1])))) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Invalid geometry",(char*)NULL);
            return(TCL_ERROR);
         }
         if (OGR_G_Within(g0,g1)) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case CONTAIN:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"geometry");
            return(TCL_ERROR);
         }
         if (!(g1=OGR_GeometryGet(Tcl_GetString(Objv[1])))) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Invalid geometry",(char*)NULL);
            return(TCL_ERROR);
         }
         if (OGR_G_Contains(g0,g1)) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case OVERLAP:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"geometry");
            return(TCL_ERROR);
         }
         if (!(g1=OGR_GeometryGet(Tcl_GetString(Objv[1])))) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Invalid geometry",(char*)NULL);
            return(TCL_ERROR);
         }
         if (OGR_G_Overlaps(g0,g1)) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case SIMPLIFY:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"tolerance");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[1],&dist);
         GPC_Simplify(dist,g0);
         break;
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_GeometryNameToTypet>
 * Creation : Aout J.P. Gauthier - CMC/CMOE
 *
 * But      : Installer la geometrie d'un feature.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Feature> : Feature
 *  <Geom>    : Geometrie
 *  <Desc>    : Liste de la geometrie
 *
 * Retour     :
 *
 * Remarques  : On doit proceder recursivement car la geometrie peut etre
 *              un container pour d'autres geometrie
 *
 *----------------------------------------------------------------------------
*/
OGRwkbGeometryType OGR_GeometryNameToType(char *Name) {

   if (strcmp(Name,"Unknown (any)")==0) {
      return(wkbUnknown);
   } else if (strcmp(Name,"Point")==0) {
      return(wkbPoint);
   } else if (strcmp(Name,"3D Point")==0) {
      return(wkbPoint25D);
   } else if (strcmp(Name,"Line String")==0) {
      return(wkbLineString);
   } else if (strcmp(Name,"3D Line String")==0) {
      return(wkbLineString25D);
   } else if (strcmp(Name,"Polygon")==0) {
      return(wkbPolygon);
   } else if (strcmp(Name,"3D Polygon")==0) {
      return(wkbPolygon25D);
   } else if (strcmp(Name,"Multi Point")==0) {
      return(wkbMultiPoint);
   } else if (strcmp(Name,"3D Multi Point")==0) {
      return(wkbMultiPoint25D);
   } else if (strcmp(Name,"Multi Line String")==0) {
      return(wkbMultiLineString);
   } else if (strcmp(Name,"3D Multi Line String")==0) {
      return(wkbMultiLineString25D);
   } else if (strcmp(Name,"Multi Polygon")==0) {
      return(wkbMultiPolygon);
   } else if (strcmp(Name,"3D Multi Polygon")==0) {
      return(wkbMultiPolygon25D);
   } else if (strcmp(Name,"Geometry Collection")==0) {
      return(wkbGeometryCollection);
   } else if (strcmp(Name,"3D Geometry Collection")==0) {
      return(wkbGeometryCollection25D);
   } else if (strcmp(Name,"Linear Ring")==0) {
      return(wkbLinearRing);
   } else if (strcmp(Name,"None")==0) {
      return(wkbNone);
   } else {
      return(wkbNone);
   }
}

int OGR_GeometrySet(Tcl_Interp *Interp,OGRGeometryH Geom,Tcl_Obj *Desc) {

   Tcl_Obj           *obj,*xobj,*yobj,*zobj;
   OGRGeometryH       geom=NULL;
   OGRwkbGeometryType type;
   int                i,n,dim=2;
   char              *elem;
   double             x,y,z;

   Tcl_ListObjLength(Interp,Desc,&n);
   if (n<2) {
      return(0);
   }
   Tcl_ListObjIndex(Interp,Desc,0,&obj);
   elem=Tcl_GetString(obj);

   if (strcmp(elem,"Unknown (any)")==0) {
      type=wkbUnknown;
   } else if (strcmp(elem,"Point")==0) {
      type=wkbPoint;
   } else if (strcmp(elem,"3D Point")==0) {
      type=wkbPoint25D;
      dim=3;
   } else if (strcmp(elem,"Line String")==0) {
      type=wkbLineString;
   } else if (strcmp(elem,"3D Line String")==0) {
      type=wkbLineString25D;
      dim=3;
   } else if (strcmp(elem,"Polygon")==0) {
      type=wkbPolygon;
   } else if (strcmp(elem,"3D Polygon")==0) {
      type=wkbPolygon25D;
      dim=3;
   } else if (strcmp(elem,"Multi Point")==0) {
      type=wkbMultiPoint;
   } else if (strcmp(elem,"3D Multi Point")==0) {
      type=wkbMultiPoint25D;
      dim=3;
   } else if (strcmp(elem,"Multi Line String")==0) {
      type=wkbMultiLineString;
   } else if (strcmp(elem,"3D Multi Line String")==0) {
      type=wkbMultiLineString25D;
      dim=3;
   } else if (strcmp(elem,"Multi Polygon")==0) {
      type=wkbMultiPolygon;
   } else if (strcmp(elem,"3D Multi Polygon")==0) {
      type=wkbMultiPolygon25D;
      dim=3;
   } else if (strcmp(elem,"Geometry Collection")==0) {
      type=wkbGeometryCollection;
   } else if (strcmp(elem,"3D Geometry Collection")==0) {
      type=wkbGeometryCollection25D;
      dim=3;
   } else if (strcmp(elem,"Linear Ring")==0) {
      type=wkbLinearRing;
   } else if (strcmp(elem,"None")==0) {
      type=wkbNone;
   } else {
      return(1);
   }

   geom=OGR_G_CreateGeometry(type);
   Tcl_ListObjIndex(Interp,Desc,1,&obj);

   if (!OGR_GeometrySet(Interp,geom,obj)) {
      for(i=1;i<n;i+=dim) {
         Tcl_ListObjIndex(Interp,Desc,i,&xobj);
         Tcl_ListObjIndex(Interp,Desc,i+1,&yobj);
         Tcl_GetDoubleFromObj(Interp,xobj,&x);
         Tcl_GetDoubleFromObj(Interp,yobj,&y);

         if (dim==3) {
            Tcl_ListObjIndex(Interp,Desc,i+2,&zobj);
            Tcl_GetDoubleFromObj(Interp,zobj,&z);
         } else {
            z=0.0;
         }
         OGR_G_AddPoint(geom,x,y,z);
      }
   }

   if (Geom) {
      OGR_G_AddGeometryDirectly(Geom,geom);
   } else {
   }

   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_GeometryGetObj>
 * Creation : Aout J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer la geometrie d'un feature.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Geom>    : Geometrie
 *
 * Retour     :
 *
 * Remarques  : On doit proceder recursivement car la geometrie peut etre
 *              un container pour d'autres geometrie
 *
 *----------------------------------------------------------------------------
*/
Tcl_Obj* OGR_GeometryGetObj(Tcl_Interp *Interp,OGRGeometryH Geom) {

   OGRGeometryH   subgeom;
   Vect3d         pt;
   int            j;

   Tcl_Obj       *obj;

   obj=Tcl_NewListObj(0,NULL);
   Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(OGRGeometryTypeToName(OGR_G_GetGeometryType(Geom)),-1));

   for(j=0;j<OGR_G_GetGeometryCount(Geom);j++) {
      subgeom=OGR_G_GetGeometryRef(Geom,j);
      Tcl_ListObjAppendElement(Interp,obj,OGR_GeometryGetObj(Interp,subgeom));
   }

   for(j=0;j<OGR_G_GetPointCount(Geom);j++) {
      OGR_G_GetPoint(Geom,j,&pt[0],&pt[1],&pt[2]);
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pt[0]));
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pt[1]));

      if (OGR_G_GetCoordinateDimension(Geom)==3) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pt[2]));
      }
   }
   return(obj);
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_GeomTess>
 * Creation : Juillet 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectuer la tessellation de la geometrie.
 *
 * Parametres :
 *  <Proj>    : Projection
 *  <Ref>     : Referentiel
 *  <Layer>   : Layer d'origine
 *  <Geom>    : Geometrie
 *  <Elev>    : Elevation
 *  <Extrude> : Extrusion
 *
 * Retour     :
 *
 * Remarques  :
 *
 *----------------------------------------------------------------------------
*/
void OGR_GeomTess(Projection *Proj,TGeoRef *Ref,OGR_Layer *Layer,OGRGeometryH Geom,double Elev,double Extrude) {

   unsigned long g,n,nv=0,pnv;
   Vect3d        nr,n0,n1;
   OGRGeometryH  geom;

   /*Reproject polygon vertices. Do it first to keep vertex adress fixed*/
   for(g=0,pnv=0;g<OGR_G_GetGeometryCount(Geom);g++) {
      geom=OGR_G_GetGeometryRef(Geom,g);
      if ((nv=OGR_GeometryProject(Proj,Ref,Layer,geom,Elev,0.0,pnv))) {
         pnv+=nv;
      }
   }

   /*Tessellate polygon*/
   gluTessBeginPolygon(GLRender->GLTess,NULL);
   for(g=0,pnv=0;g<OGR_G_GetGeometryCount(Geom);g++) {
      geom=OGR_G_GetGeometryRef(Geom,g);
      if ((nv=OGR_G_GetPointCount(geom))) {
         gluTessNormal(GLRender->GLTess,*OGR_ArrayNr[pnv][0],*OGR_ArrayNr[pnv][1],*OGR_ArrayNr[pnv][2]);
         gluTessBeginContour(GLRender->GLTess);
         glBegin(GL_LINE_STRIP);
         for(n=pnv;n<pnv+nv;n++) {
            glNormal3dv(*OGR_ArrayNr[n]);
            glVertex3dv(OGR_ArrayVr[n]);
            gluTessVertex(GLRender->GLTess,OGR_ArrayVr[n],OGR_ArrayVr[n]);
         }
         glEnd();
         gluTessEndContour(GLRender->GLTess);
         pnv+=nv;
       }
   }
   gluTessEndPolygon(GLRender->GLTess);

   /*Process extruded polygon*/
   if (Extrude!=0.0) {

      for(g=0,pnv=0;g<OGR_G_GetGeometryCount(Geom);g++) {
         geom=OGR_G_GetGeometryRef(Geom,g);
         if ((nv=OGR_GeometryProject(Proj,Ref,Layer,geom,Elev,Extrude,pnv))) {
            pnv+=nv;
         }
      }

      gluTessBeginPolygon(GLRender->GLTess,NULL);
      for(g=0,pnv=0;g<OGR_G_GetGeometryCount(Geom);g++) {
         geom=OGR_G_GetGeometryRef(Geom,g);
         if ((nv=OGR_G_GetPointCount(geom))) {

            gluTessNormal(GLRender->GLTess,*OGR_ArrayNr[pnv][0],*OGR_ArrayNr[pnv][1],*OGR_ArrayNr[pnv][2]);
            gluTessBeginContour(GLRender->GLTess);
            glBegin(GL_LINE_STRIP);
            for(n=pnv;n<pnv+nv;n++) {
               glNormal3dv(*OGR_ArrayNr[n]);
               glVertex3dv(OGR_ArrayEx[n]);
               gluTessVertex(GLRender->GLTess,OGR_ArrayEx[n],OGR_ArrayEx[n]);
            }
            glEnd();
            gluTessEndContour(GLRender->GLTess);

            /*Process sides*/
            glBegin(GL_QUAD_STRIP);
               for(n=pnv;n<pnv+nv;n++) {
                  /*Calculate normal from adjacent vertices*/
                  Vect_Substract(n0,OGR_ArrayVr[n],OGR_ArrayVr[n==(pnv+nv)-1?0:n+1]);
                  Vect_Substract(n1,OGR_ArrayVr[n==0?(pnv+nv)-1:n-1],OGR_ArrayVr[n]);
                  Vect_Add(nr,n0,n1);
                  nr[2]=0.0;
                  Vect_Mul(nr,nr,*OGR_ArrayNr[n]);
                  Vect_Normalize(nr);
                  glNormal3dv(nr);
                  glVertex3dv(OGR_ArrayVr[n]);
                  glVertex3dv(OGR_ArrayEx[n]);
               }
            glEnd();

            /*Reset the normal otherwise the roofs (tesselation) will be bad*/
            glBegin(GL_LINES);
               for(n=pnv;n<pnv+nv;n++) {
                  glNormal3dv(*OGR_ArrayNr[n]);
                  glVertex3dv(OGR_ArrayVr[n]);
                  glVertex3dv(OGR_ArrayEx[n]);
               }
            glEnd();
            pnv+=nv;
        }
      }
      gluTessEndPolygon(GLRender->GLTess);
   }
   gluTessNormal(GLRender->GLTess,0.0,0.0,0.0);
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_GeometryProject>
 * Creation : Juillet 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Projecter la geometrie dans le tableau de vertrice.
 *
 * Parametres :
 *  <Proj>    : Projection
 *  <Ref>     : Referentiel
 *  <Layer>   : Layer d'origine
 *  <Geom>    : Geometrie
 *  <Elev>    : Elevation
 *  <Extrude> : Extrusion
 *  <Size>    : Delta de dimension (Increment de rings)
 *
 * Retour     :
 *
 * Remarques  :
 *   - On projete tout les points/rings dans le tableau global qui est redimensionne
 *     si necessaire.
 *
 *----------------------------------------------------------------------------
*/
int OGR_GeometryProject(Projection *Proj,TGeoRef *Ref,OGR_Layer *Layer,OGRGeometryH Geom,double Elev,double Extrude,unsigned long Size) {

   int           handle=0,z=2;
   unsigned long n,nv=0,cnv=0;
   Vect3d        vr,*pvr,*cvr;
   Coord         co;

   extern Vect3d GDB_NMap[181][361];

   if ((nv=OGR_G_GetPointCount(Geom))) {

      /*Check the global vertex arrray size*/
      if (OGR_ArraySize<(nv+Size)) {
         OGR_ArrayNr=(Vect3d**)realloc(OGR_ArrayNr,(nv+Size)*sizeof(Vect3d*));
         OGR_ArrayVr=(Vect3d*)realloc(OGR_ArrayVr,(nv+Size)*sizeof(Vect3d));
         OGR_ArrayEx=(Vect3d*)realloc(OGR_ArrayEx,(nv+Size)*sizeof(Vect3d));
         OGR_ArraySize=(nv+Size);

         if (GLRender->GLDebug)
            fprintf(stderr,"(DEBUG) Increasing size to %i\n",OGR_ArraySize);
      }
      if (!OGR_ArrayVr || !OGR_ArrayEx || !OGR_ArrayNr) {
         fprintf(stderr,"(ERROR) OGR_GeometryProject: Unable to allocate temporary vertex buffer\n");
         return(0);
      }
      pvr=Extrude!=0.0?&OGR_ArrayEx[Size]:&OGR_ArrayVr[Size];

      /*If we need global topo info*/
      if (Layer && Layer->Topo==0)
         handle=gdb_mapopen(GDB_RES,GDB_MAP_DEM,&z);

      /*Project vertices*/
      for(n=0;n<nv;n++) {
         OGR_G_GetPoint(Geom,n,&vr[0],&vr[1],&vr[2]);
         /*Check for limits*/
         if (Layer) {
            Vect_Min(Layer->Vr[0],Layer->Vr[0],vr);
            Vect_Max(Layer->Vr[1],Layer->Vr[1],vr);
         }

         Ref->Project(Ref,vr[0],vr[1],&co.Lat,&co.Lon,1,0);
         co.Elev=vr[2];
         if (Layer && Layer->Topo>0) {
            co.Elev=Elev;
         }
         if (Layer && Layer->Topo==0) {
            gdb_mapget(handle,co.Lat,co.Lon,(void*)&z);co.Elev=z*Layer->TopoFactor;
         }
         pvr[n][0]=co.Lon;
         pvr[n][1]=co.Lat;
         pvr[n][2]=co.Elev+Extrude;
         OGR_ArrayNr[(n+Size)]=&GDB_NMap[(int)co.Lat+90][(int)co.Lon+180];
      }

      /*If everything is outside the projection*/
      if ((Proj->Type->Project(Proj,pvr,NULL,nv))==0) {
         nv=0;
      }

      if (handle)
         gdb_mapclose(handle);
   }
   return(nv);
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_GeometryRender>
 * Creation : Juillet 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectuer le rendue de la geometrie.
 *
 * Parametres :
 *  <Proj>    : Projection
 *  <Ref>     : Referentiel
 *  <Layer>   : Layer d'origine
 *  <Geom>    : Geometrie
 *  <Elev>    : Elevation
 *  <Extrude> : Extrusion
 *
 * Retour     :
 *
 * Remarques  :
 *
 *----------------------------------------------------------------------------
*/
void OGR_GeometryRender(Projection *Proj,TGeoRef *Ref,OGR_Layer *Layer,OGRGeometryH Geom,double Elev,double Extrude) {

   OGRGeometryH       subgeom;
   OGRwkbGeometryType type;
   GLenum             mode;
   unsigned long      n,nv=0;

   if (!Geom)
      return;

   /*Recursive loop on multi geometry object, unless polygon*/
   type=wkbFlatten(OGR_G_GetGeometryType(Geom));
   nv=OGR_G_GetGeometryCount(Geom);
   if (nv && type!=wkbPolygon) {
      for(n=0;n<nv;n++) {
         subgeom=OGR_G_GetGeometryRef(Geom,n);
         OGR_GeometryRender(Proj,Ref,Layer,subgeom,Elev,Extrude);
      }
   }

   /*switch on the object dimension to figure draw mode depending on extrusion*/
   switch(OGR_G_GetDimension(Geom)) {
      case 0: mode=Extrude!=0.0?GL_LINES:GL_POINTS; break;
      case 1: mode=Extrude!=0.0?GL_QUAD_STRIP:GL_LINE_STRIP; break;
      case 2: mode=GL_POLYGON; break;
      default: fprintf(stderr,"(ERROR) OGR_GeometryRender: Unable to render shape\n"); return; break;
   }

   if (mode==GL_POLYGON) {
      if (GLRender->GLTess)
         OGR_GeomTess(Proj,Ref,Layer,Geom,Elev,Extrude);
   } else {
      if ((nv=OGR_GeometryProject(Proj,Ref,Layer,Geom,Elev,0.0,0))) {
         if (Extrude!=0.0) {
            OGR_GeometryProject(Proj,Ref,Layer,Geom,Elev,Extrude,0);
         }
         glBegin(mode);
         for(n=0;n<nv;n++) {
            glNormal3dv(*OGR_ArrayNr[n]);
            if (Extrude!=0.0) {
               glVertex3dv(OGR_ArrayEx[n]);
            }
            glVertex3dv(OGR_ArrayVr[n]);
         }
         glEnd();

         if (mode==GL_QUAD_STRIP) {
            glBegin(GL_LINES);
            for(n=0;n<nv;n++) {
               glVertex3dv(OGR_ArrayEx[n]);
               glVertex3dv(OGR_ArrayVr[n]);
            }
            glEnd();

            glBegin(GL_LINE_STRIP);
            for(n=0;n<nv;n++) {
               glVertex3dv(OGR_ArrayVr[n]);
            }
            glEnd();

            glBegin(GL_LINE_STRIP);
            for(n=0;n<nv;n++) {
               glVertex3dv(OGR_ArrayEx[n]);
            }
            glEnd();
         }
      }
   }
}
