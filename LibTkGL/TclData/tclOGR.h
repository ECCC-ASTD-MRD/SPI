/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : tclOGR.h
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
 *    Boston, MA 02111-1307, USA.
 *
 * Modification :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *
 *=========================================================
 */

#ifndef _tclOGR_h
#define _tclOGR_h

#include "tkCanvVP.h"
#include "Projection.h"
#include "tclData.h"
#include "GeoRef.h"

#include "ogr_api.h"
#include "ogr_srs_api.h"
#include "cpl_string.h"

#include "gpc.h"

typedef struct OGR_File {
   OGRDataSourceH  Data;
   OGRSFDriverH    Driver;
   char           *Id;
   char           *Name;
   char            Mode;
} OGR_File;

typedef struct OGR_Layer {
   Tcl_Obj         *Tag;
   int              Active;

   OGRLayerH        Layer;
   OGRFeatureH     *Feature;
   OGRFeatureDefnH  Def;

   OGRDataSourceH   SQLed;
   char         *Select;

   TGeoRef      *Ref;          /*GeoReference*/
   TDataSpec    *Spec;         /*Specification des donnees*/

   Vect3d        Vr[2];
   int           Mask,FMask;   /*Masque*/
   GLuint        LFeature;
   long          NFeature;
   long         *SFeature;
   int           NSFeature;
   int          *Label;
   int           NLabel;

   int           Map;
   int           Topo,Extrude;
   double        TopoFactor,ExtrudeFactor;
   double        Min,Max;
   Coord        *Loc;          /* Position simple */
} OGR_Layer;

OGR_File* OGR_FileGet(Tcl_Interp *Interp,char *Id);
int       OGR_FilePut(Tcl_Interp *Interp,OGR_File *File);
void      OGR_Wipe();

int OGR_FileOpen(Tcl_Interp *Interp,char *Id,char Mode,char *Name,char *Driver);
int OGR_FileClose(Tcl_Interp *Interp,char *Id);

void OGR_FieldCreate(OGR_Layer *Layer,char *Field,char *Type,int Width);
double OGR_Centroid2D(OGRGeometryH Geom,double *X,double *Y);

OGR_Layer* OGR_LayerCreate(Tcl_Interp *Interp,char *Name);
OGRLayerH  OGR_LayerInstanciate(OGR_File *File,OGR_Layer *Layer,char *Name,TGeoRef *Ref);
void       OGR_LayerClean(OGR_Layer *Layer);
int        OGR_LayerClear(Tcl_Interp *Interp,OGR_Layer *Layer,int Field,double Value);
int        OGR_LayerDestroy(Tcl_Interp *Interp,char *Name);
int        OGR_LayerDefine(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
void       OGR_LayerFree(OGR_Layer *Layer);
OGR_Layer* OGR_LayerGet(char *Name);
int        OGR_LayerSQLSelect(Tcl_Interp *Interp,char *Name,char *FileId,char *Statement,char *Geom);
int        OGR_LayerRead(Tcl_Interp *Interp,char *Name,char *FileId,int Idx);
int        OGR_LayerWrite(Tcl_Interp *Interp,char *Name,char *FileId);
int        OGR_LayerRender(Tcl_Interp *Interp,Projection *Proj,ViewportItem *VP,OGR_Layer *Layer);
void       OGR_LayerLimit(OGR_Layer *Layer);
int        OGR_LayerImport(Tcl_Interp *Interp,OGR_Layer *Layer,Tcl_Obj *Fields);
int        OGR_LayerInterp(Tcl_Interp *Interp,OGR_Layer *Layer,int Field,TGeoRef *FromRef,TDataDef *FromDef,char Mode,int Final,int Prec,Tcl_Obj *List);
int        OGR_LayerStat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
void       OGR_LayerPreInit(OGR_Layer *Layer);
int        OGR_GridCell(OGRGeometryH Geom,TGeoRef *RefTo,TGeoRef *RefFrom,int I,int J,int Seg);

int                OGR_GeometryStat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
int                OGR_GeometryDefine(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
OGRwkbGeometryType OGR_GeometryNameToType(char *Name);
OGRGeometryH*      OGR_GeometryGet(char *Name);
int                OGR_GeometrySet(Tcl_Interp *Interp,OGRGeometryH Geom,Tcl_Obj *Desc);
int                OGR_GeometryDestroy(Tcl_Interp *Interp,char *Name);
int                OGR_GeometryProject(Projection *Proj,TGeoRef *Ref,OGR_Layer *Layer,OGRGeometryH Geom,double Elev,double Extrude,unsigned long Size);
void               OGR_GeometryRender(Projection *Proj,ViewportItem *VP,TGeoRef *Ref,OGR_Layer *Layer,OGRGeometryH Geom,double Elev,double Extrude);
Tcl_Obj*           OGR_GeometryGetObj(Tcl_Interp *Interp,OGRGeometryH Geom);
Tcl_Obj*           OGR_GeometryPut(Tcl_Interp *Interp,char *Name,OGRGeometryH Geom);

int      OGR_Pick(Tcl_Interp *Interp,OGR_Layer *Layer,OGRGeometryH *Geom,Tcl_Obj *List,int All);
Tcl_Obj* OGR_GetTypeObj(Tcl_Interp *Interp,OGRFieldDefnH Field,OGRFeatureH Feature,int Index);

int          GPC_PointPointIntersect(OGRGeometryH Geom0,OGRGeometryH Geom1);
int          GPC_PointLineIntersect(OGRGeometryH Geom0,OGRGeometryH Geom1);
int          GPC_PointPolyIntersect(OGRGeometryH Geom0,OGRGeometryH Geom1);
int          GPC_LinePolyIntersect(OGRGeometryH Geom0,OGRGeometryH Geom1);
double       GPC_Length(OGRGeometryH Geom);
double       GPC_SegmentLength(OGRGeometryH Geom);
int          GPC_SegmentIntersect(Vect3d PointA,Vect3d PointB,Vect3d PointC,Vect3d PointD,Vect3d Inter);
int          GPC_Intersect(OGRGeometryH Geom0,OGRGeometryH Geom1);
void         GPC_FromOGR(gpc_polygon* Poly,OGRGeometryH *Geom);
void         GPC_ToOGR(gpc_polygon *Poly,OGRGeometryH *Geom);
OGRGeometryH GPC_OnOGR(gpc_op Op,OGRGeometryH Geom0,OGRGeometryH Geom1);
OGRGeometryH GPC_Clip(OGRGeometryH Line,OGRGeometryH Poly);
int          GPC_ClipSegment(OGRGeometryH Line,OGRGeometryH Poly,OGRGeometryH Clip);

#endif
