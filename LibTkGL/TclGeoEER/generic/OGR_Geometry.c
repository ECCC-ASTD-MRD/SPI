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

#include "App.h"
#include "tclOGR.h"

static Vect3d  **OGR_ArrayNr=NULL;
static Vect3d  *OGR_ArrayVr=NULL;
static Vect3d  *OGR_ArrayEx=NULL;

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
   int            i,j,idx,n,t,v,err,d,dobj=0;
   char          *buf;
   unsigned char *bytes;
   Vect3d         pt,*ppt;
   Tcl_Obj       *obj;
   TGeoRef       *ref;

   static CONST char *sopt[] = { "-wkt","-wkb","-gml","-kml","-json","-geometry","-addgeometry","-delgeometry","-space","-dimension","-type","-nb","-nbsub","-sub","-name","-points","-addpoint","-setpoint","-inspoint","-delpoint","-georef",NULL };
   enum                opt { WKT,WKB,GML,KML,JSON,GEOMETRY,ADDGEOMETRY,DELGEOMETRY,SPACE,DIMENSION,TYPE,NB,NBSUB,SUB,NAME,POINTS,ADDPOINT,SETPOINT,INSPOINT,DELPOINT,GEOREF };

   geom=OGR_GeometryGet(Name);
   if (!geom) {
      Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Geometry name unknown: \"",Name,"\"",(char *)NULL);
      return(TCL_ERROR);
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {

         case WKT:
            if (Objc-dobj==1) {
               OGR_G_ExportToWkt(geom,&buf);
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
               CPLFree(buf);
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

         case WKB:
            if (Objc-dobj==1) {
               if ((n=OGR_G_WkbSize(geom))) {
                  bytes=(unsigned char*)malloc(n);
                  OGR_G_ExportToWkb(geom,wkbNDR,bytes);
                  Tcl_SetObjResult(Interp,Tcl_NewByteArrayObj(bytes,n));
                  CPLFree(bytes);
               }
            } else {
               OGR_G_Empty(geom);

               buf=Tcl_GetString(Objv[++i]);
               err=OGR_G_ImportFromWkb(geom,(unsigned char*)buf,strlen(buf));
               if (err!=OGRERR_NONE) {
                  Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Unsupported geometry type\"",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            break;

         case GML:
            if (Objc-dobj==1) {
               if ((buf=OGR_G_ExportToGML(geom))) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
                  CPLFree(buf);
               }
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
            if (Objc-dobj==1) {
               if ((buf=OGR_G_ExportToKML(geom,NULL))) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
                  CPLFree(buf);
               }
            } else {
               Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Operation not supported\"",(char*)NULL);
               return(TCL_ERROR);
            }
            break;


         case JSON:
            if (Objc-dobj==1) {
               if ((buf=OGR_G_ExportToJson(geom))) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
                  CPLFree(buf);
               }
            } else {
               Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Operation not supported\"",(char*)NULL);
               return(TCL_ERROR);
            }
            break;

         case GEOREF:
            if (Objc-dobj==1) {
            } else {
               if (!(ref=GeoRef_Get(Tcl_GetString(Objv[2])))) {
                  Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid georeference",(char*)NULL);
                  return(TCL_ERROR);
               }
               OGR_G_AssignSpatialReference(geom,ref->Spatial);
            }
            break;

         case GEOMETRY:
            if (Objc-dobj==1) {
               obj=Tcl_NewListObj(0,NULL);
               for (n=0;n<OGR_G_GetGeometryCount(geom);n++) {
                   Tcl_ListObjAppendElement(Interp,obj,OGR_GeometryPut(Interp,NULL,OGR_G_GetGeometryRef(geom,n)));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Objc-dobj<3) {
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
                  if ((subgeom=OGR_GeometryGet(Tcl_GetString(obj)))) {
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
            if (Objc-dobj==1) {
               Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid number of geometry\"",(char*)NULL);
               return(TCL_ERROR);
            } else {
               if (Objc-dobj<3) {
                  Tcl_WrongNumArgs(Interp,2,Objv,"direct geometry");
                  return(TCL_ERROR);
               }

               Tcl_GetBooleanFromObj(Interp,Objv[++i],&t);

               if (Tcl_ListObjLength(Interp,Objv[++i],&n)==TCL_ERROR) {
                  return(TCL_ERROR);
               }

               for(j=0;j<n;) {
                  Tcl_ListObjIndex(Interp,Objv[i],j++,&obj);
                  if ((subgeom=OGR_GeometryGet(Tcl_GetString(obj)))) {

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

         case DELGEOMETRY:
            if (Objc-dobj==1) {
               Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid geometry index\"",(char*)NULL);
               return(TCL_ERROR);
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&n);
               if (n>=0 && n<OGR_G_GetGeometryCount(geom)) {
                  OGR_G_RemoveGeometry(geom,n,TRUE);
               }
            }
            break;

         case POINTS:
            if (Objc-dobj==1) {
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
            if (Objc-dobj!=3 && Objc-dobj!=4) {
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

         case INSPOINT:
            if (Objc-dobj!=4 && Objc-dobj!=5) {
               Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid number of coordinates",(char*)NULL);
               return(TCL_ERROR);
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&t);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&pt[0]);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&pt[1]);
               if (Objc-dobj==5) {
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&pt[2]);
               } else {
                  pt[2]=0.0;
               }

               n=OGR_G_GetPointCount(geom);
               d=OGR_G_GetCoordinateDimension(geom);
               if (t<0 || t>n) {
                  Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid coordinate index",(char*)NULL);
                  return(TCL_ERROR);
               }

               // If the index is within the range of points
               if ((ppt=(Vect3d*)malloc((n+1)*sizeof(Vect3d)))) {
                  v=0;
                  for(j=0;j<n;j++) {
                     if (j==t) v++;
                     OGR_G_GetPoint(geom,j,&ppt[v][0],&ppt[v][1],&ppt[v][2]);
                     v++;
                  }
                  ppt[t][0]=pt[0]; ppt[t][1]=pt[1]; ppt[t][2]=pt[2];

                  // GDAL 1.10
                  OGR_G_Empty(geom);
                  for(v=0;v<n+1;v++) {
                     OGR_G_SetPoint(geom,v,ppt[v][0],ppt[v][1],ppt[v][2]);
                  }
                  // GDAL 1.11
//                  OGR_G_SetPoints(geom,n-1,&ppt[v][0],3*sizeof(double),&ppt[v][1],3*sizeof(double),&ppt[v][2],3*sizeof(double));

                  if (d==2) OGR_G_FlattenTo2D(geom);

                  free(ppt);
               }
            }
            break;

         case DELPOINT:
            if (Objc-dobj!=2) {
               Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: No point index specified",(char*)NULL);
               return(TCL_ERROR);
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&t);

               n=OGR_G_GetPointCount(geom);
               d=OGR_G_GetCoordinateDimension(geom);

               // If the index is within the range of points
               if (t<n) {
                  if ((ppt=(Vect3d*)malloc(n*sizeof(Vect3d)))) {
                     v=0;
                     for(j=0;j<n;j++) {
                        if (j!=t) {
                           OGR_G_GetPoint(geom,j,&ppt[v][0],&ppt[v][1],&ppt[v][2]);
                           v++;
                        }
                     }

                     // GDAL 1.10
                     OGR_G_Empty(geom);
                     for(v=0;v<n-1;v++) {
                        OGR_G_SetPoint(geom,v,ppt[v][0],ppt[v][1],ppt[v][2]);
                     }
                     // GDAL 1.11
//                     OGR_G_SetPoints(geom,n-1,&ppt[v][0],3*sizeof(double),&ppt[v][1],3*sizeof(double),&ppt[v][2],3*sizeof(double));
                     if (d==2) OGR_G_FlattenTo2D(geom);

                     free(ppt);
                  }
               }
            }
            break;

         case SETPOINT:
            if (Objc-dobj!=4 && Objc-dobj!=5) {
               Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid number of coordinates",(char*)NULL);
               return(TCL_ERROR);
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&t);

               Tcl_GetDoubleFromObj(Interp,Objv[++i],&pt[0]);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&pt[1]);
               if (Objc-dobj==5) {
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&pt[2]);
                  OGR_G_SetPoint(geom,t,pt[0],pt[1],pt[2]);
               } else {
                  OGR_G_SetPoint_2D(geom,t,pt[0],pt[1]);
               }
            }
            break;

         case NB:
            if (Objc-dobj==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(OGR_G_GetPointCount(geom)));
            }
            break;

         case NBSUB:
            if (Objc-dobj==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(OGR_G_GetGeometryCount(geom)));
            }
            break;

          case SUB:
            if (Tcl_ListObjLength(Interp,Objv[++i],&n)==TCL_ERROR) {
               return(TCL_ERROR);
            }
            if (n) {
               // Find the subgeom
               for(j=0;j<n;j++) {
                  Tcl_ListObjIndex(Interp,Objv[i],j,&obj);
                  if (Tcl_GetIntFromObj(Interp,obj,&t)==TCL_ERROR) {
                     return(TCL_ERROR);
                  }
                  geom=OGR_G_GetGeometryRef(geom,t);
               }
               if (!geom) {
                  Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid sub-geometry",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
            dobj=2;
            break;

         case TYPE:
            if (Objc-dobj==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(OGRGeometryTypeToName(OGR_G_GetGeometryType(geom)),-1));
            } else {
            }
            break;

         case NAME:
            if (Objc-dobj==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(OGR_G_GetGeometryName(geom),-1));
            } else {
            }
            break;

         case SPACE:
            if (Objc-dobj==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(OGR_G_GetDimension(geom)));
            } else {
            }
            break;

         case DIMENSION:
            if (Objc-dobj==1) {
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
   Tcl_Obj       *lst,*obj;
   int           j,t,idx,n,nseg;
   double        dist,x0,y0,x1,y1;
   Vect3d        pt,ptp,vr;

   static CONST char *sopt[] = { "-sub","-transform","-distance","-area","-anglemin","-pointdist","-segmentdist","-pointonsurface","-centroid","-extent","-length","-boundary","-buffer",
                                 "-convexhull","-dissolve","-intersection","-union","-difference","-symmetricdifference","-intersectionpts",
                                 "-intersect","-equal","-disjoint","-touch","-cross","-within","-contain","-overlap",
                                 "-simplify","-segmentize","-delaunay","-close","-flatten","-topoint","-toline","-tomultiline","-topolygon","-tomultipolygon","-clean","-isempty","-isvalid","-issimple","-isring",NULL };
   enum                opt { SUB,TRANSFORM,DISTANCE,AREA,ANGLEMIN,POINTDIST,SEGMENTDIST,POINTONSURFACE,CENTROID,EXTENT,LENGTH,BOUNDARY,BUFFER,CONVEXHULL,DISSOLVE,INTERSECTION,
                             UNION,DIFFERENCE,SYMMETRICDIFFERENCE,INTERSECTIONPTS,INTERSECT,EQUAL,DISJOINT,TOUCH,CROSS,WITHIN,CONTAIN,
                             OVERLAP,SIMPLIFY,SEGMENTIZE,DELAUNAY,CLOSE,FLATTEN,TOPOINT,TOLINE,TOMULTILINE,TOPOLYGON,TOMULTIPOLYGON,CLEAN,ISEMPTY,ISVALID,ISSIMPLE,ISRING };

   g0=OGR_GeometryGet(Name);
   if (!g0) {
      Tcl_AppendResult(Interp,"\n   OGR_GeometryStat: Geometry name unknown: \"",Name,"\"",(char *)NULL);
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[0],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   if ((enum opt)idx==SUB) {
      if (Tcl_ListObjLength(Interp,Objv[1],&n)==TCL_ERROR) {
         return(TCL_ERROR);
      }
      if (n) {
         // Find the subgeom
         for(j=0;j<n;j++) {
            Tcl_ListObjIndex(Interp,Objv[1],j,&obj);
            if (Tcl_GetIntFromObj(Interp,obj,&t)==TCL_ERROR) {
               return(TCL_ERROR);
            }
            g0=OGR_G_GetGeometryRef(g0,t);
         }
         if (!g0) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid sub-geometry",(char*)NULL);
            return(TCL_ERROR);
         }
      }
      Objc-=2;
      Objv+=2;
      if (Tcl_GetIndexFromObj(Interp,Objv[0],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }
   }

   switch ((enum opt)idx) {
         
      case SUB: break;
      
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
         OGR_G_AssignSpatialReference(g0,ref->Spatial);
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
         Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(OGR_G_Area(g0)));
         break;

      case ANGLEMIN:
         if (Objc!=1) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid number of parameters\"",(char*)NULL);
            return(TCL_ERROR);
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(OGM_AngleMin(g0)));
         }
         break;

      case SEGMENTDIST:
         if (Objc!=3 && Objc!=4) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid number of coordinates\"",(char*)NULL);
            return(TCL_ERROR);
         } else {
            Tcl_GetDoubleFromObj(Interp,Objv[1],&vr[0]);
            Tcl_GetDoubleFromObj(Interp,Objv[2],&vr[1]);
            if (Objc==4)
               Tcl_GetDoubleFromObj(Interp,Objv[3],&vr[2]);

            obj=Tcl_NewListObj(0,NULL);
            OGR_G_GetPoint(g0,0,&pt[0],&pt[1],&pt[2]);
            for (n=1;n<OGR_G_GetPointCount(g0);n++) {
               OGR_G_GetPoint(g0,n,&ptp[0],&ptp[1],&ptp[2]);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(OGM_SegmentDist(pt,ptp,vr)));
               Vect_Assign(pt,ptp);
            }

            Tcl_SetObjResult(Interp,obj);
         }
         break;

      case POINTDIST:
         if (Objc!=3 && Objc!=4) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryDefine: Invalid number of coordinates\"",(char*)NULL);
            return(TCL_ERROR);
         } else {
            Tcl_GetDoubleFromObj(Interp,Objv[1],&vr[0]);
            Tcl_GetDoubleFromObj(Interp,Objv[2],&vr[1]);
            if (Objc==4)
               Tcl_GetDoubleFromObj(Interp,Objv[3],&vr[2]);

            obj=Tcl_NewListObj(0,NULL);
            for (n=0;n<OGR_G_GetPointCount(g0);n++) {
               OGR_G_GetPoint(g0,n,&pt[0],&pt[1],&pt[2]);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(hypot(vr[0]-pt[0],vr[1]-pt[1])));
            }
            Tcl_SetObjResult(Interp,obj);
         }
         break;

      case POINTONSURFACE:
         if ((g1=OGR_G_PointOnSurface(g0))) {
            lst=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(OGR_G_GetX(g1,0)));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(OGR_G_GetY(g1,0)));
            Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(OGR_G_GetZ(g1,0)));
            Tcl_SetObjResult(Interp,lst);
            OGR_G_DestroyGeometry(g1);
         }

      case CENTROID:
         g1=OGR_G_CreateGeometry(wkbPoint);
         OGR_G_Centroid(g0,g1);
         lst=Tcl_NewListObj(0,NULL);
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(OGR_G_GetX(g1,0)));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(OGR_G_GetY(g1,0)));
         Tcl_ListObjAppendElement(Interp,lst,Tcl_NewDoubleObj(OGR_G_GetZ(g1,0)));
         Tcl_SetObjResult(Interp,lst);
         OGR_G_DestroyGeometry(g1);
         break;

      case BOUNDARY:
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_Boundary(g0)));
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

      case LENGTH:
         Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(OGM_Length(g0)));
         break;

      case BUFFER:
         if (Objc!=2 && Objc!=3) {
            Tcl_WrongNumArgs(Interp,0,Objv,"dist [nseg]");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[1],&dist);

         if (Objc==3) {
            Tcl_GetIntFromObj(Interp,Objv[2],&nseg);
         } else {
            nseg=10;
         }

         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_Buffer(g0,dist,nseg)));
         break;

      case TOPOINT:
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_ForceToMultiPoint(g0)));
         break;

      case TOLINE:
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_ForceToLineString(g0)));
         break;

      case TOMULTILINE:
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_ForceToMultiLineString(g0)));
         break;

      case TOPOLYGON:
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_ForceToPolygon(g0)));
         break;

      case TOMULTIPOLYGON:
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_ForceToMultiPolygon(g0)));
         break;

      case CONVEXHULL:
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_ConvexHull(g0)));
         break;

      case DISSOLVE:
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGM_GPCOnOGRGeometry(GPC_UNION,g0)));
         break;
         
      case DELAUNAY:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"tolerance");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[1],&dist);
         if (!(g1=OGR_G_DelaunayTriangulation(g0,dist,FALSE))) {
            Tcl_AppendResult(Interp,"\n   OGR_GeometryCmd: Problem calculating Delaunay triangulation",(char*)NULL);
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,g1));
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
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGM_GPCOnOGR(GPC_INT,g0,g1)));
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
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGM_GPCOnOGR(GPC_UNION,g0,g1)));
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
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGM_GPCOnOGR(GPC_DIFF,g0,g1)));
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
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGM_GPCOnOGR(GPC_XOR,g0,g1)));
//         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGR_G_SymmetricDifference(g0,g1)));
         break;

      case INTERSECTIONPTS:
         if (Objc!=5) {
            Tcl_WrongNumArgs(Interp,0,Objv,"pt0x pt0y pt1x pt1y");
            return(TCL_ERROR);
         }
         if (Tcl_GetDoubleFromObj(Interp,Objv[1],&x0)!=TCL_OK
               || Tcl_GetDoubleFromObj(Interp,Objv[2],&y0)!=TCL_OK
               || Tcl_GetDoubleFromObj(Interp,Objv[3],&x1)!=TCL_OK
               || Tcl_GetDoubleFromObj(Interp,Objv[4],&y1)!=TCL_OK ) {
            Tcl_AppendResult(Interp,"Invalid segment coordinates\n",(char*)NULL);
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,OGR_GeometryPut(Interp,NULL,OGM_IntersectionPts(g0,x0,y0,x1,y1,NULL)));
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
         if (OGM_Intersect(g0,g1,NULL,NULL)) {
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

      case SEGMENTIZE:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"length");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[1],&dist);
         OGR_G_Segmentize(g0,dist);
         break;

      case SIMPLIFY:
         if (Objc!=2) {
            Tcl_WrongNumArgs(Interp,0,Objv,"tolerance");
            return(TCL_ERROR);
         }
         Tcl_GetDoubleFromObj(Interp,Objv[1],&dist);
         OGM_Simplify(dist,g0);
//         OGR_G_Simplify(g0,dist);
         break;

      case CLOSE:
         OGR_G_CloseRings(g0);
         break;

      case CLEAN:
         OGM_Clean(g0);
         break;

      case FLATTEN:
         OGR_G_FlattenTo2D(g0);
         break;

      case ISEMPTY:
         Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(OGR_G_IsEmpty(g0)));
         break;

      case ISVALID:
         Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(OGR_G_IsValid(g0)));
         break;

      case ISSIMPLE:
         Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(OGR_G_IsSimple(g0)));
         break;

      case ISRING:
         Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(OGR_G_IsRing(g0)));
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
/*
void OGR_GeomNURBS(OGRGeometryH Geom) {

   OGRGeometryH geom;
   GLUnurbsObj *nurb;
   Vect3f       temp[1000];
   Vect3d       t;
   int          i;
   unsigned int  g,nv=0,pnv;
   GLfloat      uknots[8]= { -1.570796f,-1.570796f,-1.570796f,0.000000f,0.000000f,1.570796f,1.570796f,1.570796f };
   GLfloat      vknots[12]= { 0.000000f,0.000000f,0.000000f,1.570796f,1.570796f,3.141593f,3.141593f,4.712389f,4.712389f,6.283185f,6.283185f,6.283185f };
   GLfloat      ctrls[180] = { 0.000000f,   0.000000f,  -1.000000f,  1.000000f,
                               0.000000f,   0.000000f,  -0.70710678f,0.707107f,
                               0.000000f,   0.000000f,  -1.000000f,  1.000000f,
                               0.000000f,   0.000000f,  -0.70710678f,0.707107f,
                               0.000000f,   0.000000f,  -1.000000f,  1.000000f,
                               0.000000f,   0.000000f,  -0.70710678f,0.707107f,
                               0.000000f,   0.000000f,  -1.000000f,  1.000000f,
                               0.000000f,   0.000000f,  -0.70710678f,0.707107f,
                               0.000000f,   0.000000f,  -1.000000f,  1.000000f,
                              -0.70710678f, 0.000000f,  -0.70710678f,0.707107f,
                              -0.50000000f, 0.50000000f,-0.50000000f,0.500000f,
                               0.000000f,   0.70710678f,-0.70710678f,0.707107f,
                               0.50000000f, 0.50000000f,-0.50000000f,0.500000f,
                               0.70710678f, 0.000000f,  -0.70710678f,0.707107f,
                               0.50000000f,-0.50000000f,-0.50000000f,0.500000f,
                               0.000000f,  -0.70710678f,-0.70710678f,0.707107f,
                              -0.50000000f,-0.50000000f,-0.50000000f,0.500000f,
                              -0.70710678f, 0.000000f,  -0.70710678f,0.707107f,
                              -1.000000f,   0.000000f,   0.000000f,  1.000000f,
                              -0.70710678f, 0.70710678f, 0.000000f,  0.707107f,
                               0.000000f,   1.000000f,   0.000000f,  1.000000f,
                               0.70710678f, 0.70710678f, 0.000000f,  0.707107f,
                               1.000000f,   0.000000f,   0.000000f,  1.000000f,
                               0.70710678f,-0.70710678f, 0.000000f,  0.707107f,
                               0.000000f,  -1.000000f,   0.000000f,  1.000000f,
                              -0.70710678f,-0.70710678f, 0.000000f,  0.707107f,
                              -1.000000f,   0.000000f,   0.000000f,  1.000000f,
                              -0.70710678f, 0.000000f,   0.70710678f,0.707107f,
                              -0.50000000f, 0.50000000f, 0.50000000f,0.500000f,
                               0.000000f,   0.70710678f, 0.70710678f,0.707107f,
                               0.50000000f, 0.50000000f, 0.50000000f,0.500000f,
                               0.70710678f, 0.000000f,   0.70710678f,0.707107f,
                               0.50000000f,-0.50000000f, 0.50000000f,0.500000f,
                               0.000000f,  -0.70710678f, 0.70710678f,0.707107f,
                              -0.50000000f,-0.50000000f, 0.50000000f,0.500000f,
                              -0.70710678f, 0.000000f,   0.70710678f,0.707107f,
                               0.000000f,   0.000000f,   1.000000f,  1.000000f,
                               0.000000f,   0.000000f,   0.70710678f,0.707107f,
                               0.000000f,   0.000000f,   1.000000f,  1.000000f,
                               0.000000f,   0.000000f,   0.70710678f,0.707107f,
                               0.000000f,   0.000000f,   1.000000f,  1.000000f,
                               0.000000f,   0.000000f,   0.70710678f,0.707107f,
                               0.000000f,   0.000000f,   1.000000f,  1.000000f,
                               0.000000f,   0.000000f,   0.70710678f,0.707107f,
                               0.000000f,   0.000000f,   1.000000f,  1.000000f };


   nurb=gluNewNurbsRenderer();
   gluNurbsProperty(nurb,GLU_NURBS_MODE,GLU_NURBS_TESSELLATOR);
   gluNurbsProperty(nurb,GLU_SAMPLING_TOLERANCE,10.0);
   gluNurbsProperty(nurb,GLU_DISPLAY_MODE,GLU_FILL);
   gluNurbsCallback(nurb,GLU_ERROR,glTessError);
   gluNurbsCallback(nurb,GLU_NURBS_BEGIN,glBegin);
   gluNurbsCallback(nurb,GLU_NURBS_VERTEX,glVertex3fv);
   gluNurbsCallback(nurb,GLU_NURBS_NORMAL,glNormal3fv);
   gluNurbsCallback(nurb,GLU_NURBS_END,glEnd);

   glRotatef(-90.0,1.0,0.0,0.0);
   glRotatef(90.0,0.0,0.0,1.0);
   glScalef(-1.0,1.0,1.0);
   gluBeginSurface(nurb);
   gluNurbsSurface(nurb,8,uknots,12,vknots,4*9,4,ctrls,3,3,GL_MAP2_VERTEX_4);
   for(g=0,pnv=0;g<OGR_G_GetGeometryCount(Geom);g++) {
      geom=OGR_G_GetGeometryRef(Geom,g);
      if ((nv=OGR_G_GetPointCount(geom))) {
         for(i=0;i<nv;i++) {
            OGR_G_GetPoint(geom,i,&t[0],&t[1],&t[2]);
            temp[i][1]=(t[0]+180.0)/360.0*M_2PI;
            temp[i][0]=t[1]/180.0*M_PI;
         }
         gluBeginTrim(nurb);
         gluPwlCurve(nurb,nv,temp,3,GLU_MAP1_TRIM_2);
         gluEndTrim(nurb);
      }
   }

   gluEndSurface(nurb);
   glScalef(-1.0,1.0,1.0);
   glRotatef(-90.0,0.0,0.0,1.0);
   glRotatef(90.0,1.0,0.0,0.0);

   gluDeleteNurbsRenderer(nurb);
}
*/

void OGR_GeomTess(Projection *Proj,TGeoRef *Ref,OGR_Layer *Layer,OGRGeometryH Geom,double Elev,double Extrude) {

   unsigned int  g,n,nv=0,pnv;
   Vect3d        nr,n0,n1;
   OGRGeometryH  geom;

   /*Reproject polygon vertices. Do it first to keep vertex adress fixed*/
   for(g=0,pnv=0;g<OGR_G_GetGeometryCount(Geom);g++) {
      geom=OGR_G_GetGeometryRef(Geom,g);
      if ((nv=OGR_GeometryProject(Proj,Ref,Layer,geom,Elev,0.0,pnv))) {
         pnv+=nv;
      }
   }
   
// OGR_GeomNURBS(Geom);
   /*Tessellate polygon*/
   if (Layer && Layer->Space==3) {
      glTessInitNr();
      gluTessBeginPolygon(GLRender->GLTess,NULL);
      for(g=0,pnv=0;g<OGR_G_GetGeometryCount(Geom);g++) {
         geom=OGR_G_GetGeometryRef(Geom,g);
         if ((nv=OGR_G_GetPointCount(geom))) {
            gluTessBeginContour(GLRender->GLTess);
            glBegin(GL_LINE_STRIP);

            /*Calculate normal from adjacent vertices*/
            Vect_Substract(n1,OGR_ArrayVr[pnv+1],OGR_ArrayVr[pnv]);
            Vect_Substract(n0,OGR_ArrayVr[pnv+2],OGR_ArrayVr[pnv]);
            Vect_CrossProduct(nr,n0,n1);
//            Vect_Mul(nr,nr,*OGR_ArrayNr[pnv]);
            Vect_Normalize(nr);
//            gluTessNormal(GLRender->GLTess,nr[0],nr[1],nr[2]);
            for(n=pnv;n<pnv+nv;n++) {
               Vect_Assign(OGR_ArrayEx[n],OGR_ArrayVr[n]);
               Vect_Assign(OGR_ArrayEx[n+1],nr);
               glNormal3dv(nr);
               glVertex3dv(OGR_ArrayVr[n]);
               gluTessVertex(GLRender->GLTess,OGR_ArrayVr[n],OGR_ArrayEx[n]);
            }
            glEnd();
            gluTessEndContour(GLRender->GLTess);
            pnv+=nv;
         }
      }
      gluTessEndPolygon(GLRender->GLTess);
      glTessInit();

   } else {
      gluTessBeginPolygon(GLRender->GLTess,NULL);
      for(g=0,pnv=0;g<OGR_G_GetGeometryCount(Geom);g++) {
         geom=OGR_G_GetGeometryRef(Geom,g);
         if ((nv=OGR_G_GetPointCount(geom))) {
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
   }

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
}

/*----------------------------------------------------------------------------
 * Nom      : <OGR_GeometryProject>
 * Creation : Juillet 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Projecter la geometrie dans le tableau de vertice.
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

int OGR_GeometryProject(Projection *Proj,TGeoRef *Ref,OGR_Layer *Layer,OGRGeometryH Geom,double Elev,double Extrude,unsigned int Size) {

   short         z=2;
   char          td=0;
   unsigned int  n,nv=0;
   Vect3d        *pvr,*cvr=NULL;
   Coord         co;

   extern Vect3d GDB_NMap[181][361];

   if ((nv=OGR_G_GetPointCount(Geom))) {

      OGR_ArrayVr=OGM_GetVect3d(nv+Size,OGM_ARRAY0);
      OGR_ArrayEx=OGM_GetVect3d(nv+Size,OGM_ARRAY1);
      OGR_ArrayNr=(Vect3d**)OGM_GetVect3d(nv+Size,OGM_ARRAYPTR);

      if (!OGR_ArrayVr || !OGR_ArrayNr || !OGR_ArrayEx) {
         return(0);
      }

      pvr=Extrude!=0.0?&OGR_ArrayEx[Size]:&OGR_ArrayVr[Size];
      co.Lat=co.Lon=0.0;

      /*Project vertices*/
      OGR_G_GetPoints(Geom,&pvr[0][0],sizeof(Vect3d),&pvr[0][1],sizeof(Vect3d),&pvr[0][2],sizeof(Vect3d));
      
      for(n=0;n<nv;n++) {
         Ref->Project(Ref,pvr[n][0],pvr[n][1],&co.Lat,&co.Lon,1,0);
         CLAMPLAT(co.Lat);
         CLAMPLON(co.Lon);
         co.Lon=CLAMP(co.Lon,-180.0,180.0);

         /*Keep latlon extent since it's not the same than the original extent reprojected to latlon*/
         Ref->LLExtent.MinX=Ref->LLExtent.MinX>co.Lon?co.Lon:Ref->LLExtent.MinX;
         Ref->LLExtent.MaxX=Ref->LLExtent.MaxX<co.Lon?co.Lon:Ref->LLExtent.MaxX;
         Ref->LLExtent.MinY=Ref->LLExtent.MinY>co.Lat?co.Lat:Ref->LLExtent.MinY;
         Ref->LLExtent.MaxY=Ref->LLExtent.MaxY<co.Lon?co.Lat:Ref->LLExtent.MaxY;

         co.Elev=pvr[n][2];
         td+=pvr[n][2]!=0.0;

         if (Layer) {
            /*Apply topography*/
            if (Layer->Topo>0) {
               co.Elev=Elev;
            } else if (Layer->Topo==0) {
#ifdef HAVE_GDB
               if (Proj->Geo->Maps[GDB_MAP_DEM]) {

                  gdb_mapget(Proj->Geo->Maps[GDB_MAP_DEM],co.Lat,co.Lon,(char*)&z);
                  co.Elev=z*Layer->Spec->TopoFactor;
               }
#endif
            }
         }

         pvr[n][0]=co.Lon;
         pvr[n][1]=co.Lat;
         pvr[n][2]=co.Elev+Extrude;
         OGR_ArrayNr[(n+Size)]=&GDB_NMap[(int)co.Lat+90][(int)co.Lon+180];
      }
      
      if (Layer->Space==3 && !td) {
         Layer->Space=2;
      }

      /*If everything is outside the projection*/
      if ((Proj->Type->Project(Proj,(GeoVect*)pvr,NULL,nv))==0) {
         nv=0;
      }

      /*Flip X coordinates when we cross -180/180 for cylindrical projections*/
      if (Proj->Type->Def==PROJCYLIN) {
         z=0;
         for(n=1;n<nv;n++) {
            cvr=!z?&pvr[n-1]:cvr;

            if (fabs(pvr[n][0]-*cvr[0])>1.0) {
               z=!z?(pvr[n][0]>0.0?-4:4):z;
               pvr[n][0]+=z;
            }
         }
      }
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
   unsigned int       n,nv=0;

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
      default: App_Log(ERROR,"%s: Unable to render shape\n",__func__); return; break;
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
