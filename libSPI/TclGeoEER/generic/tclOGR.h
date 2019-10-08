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
 *=========================================================
 */

#ifndef _tclOGR_h
#define _tclOGR_h

#include "tkCanvVP.h"
#include "Projection.h"
#include "tclData.h"
#include "OGR.h"

#include "cpl_string.h"

OGR_File* OGR_FileGet(Tcl_Interp *Interp,char *Id);
int       OGR_FilePut(Tcl_Interp *Interp,OGR_File *File);
void      OGR_Wipe();

int OGR_FileOpen(Tcl_Interp *Interp,char *Id,char Mode,char *Name,char *Driver,char **Options);
int OGR_FileClose(Tcl_Interp *Interp,char *Id);

int                OGR_Pick(Tcl_Interp *Interp,OGR_Layer *Layer,OGRGeometryH *Geom,Tcl_Obj *List,int All,int Mode);
Tcl_Obj*           OGR_GetTypeObj(Tcl_Interp *Interp,OGRFieldDefnH Field,OGRFeatureH Feature,int Index);
int                OGR_SetTypeObj(Tcl_Interp *Interp,Tcl_Obj* Obj,OGRLayerH Layer,OGRFieldDefnH Field,OGRFeatureH Feature,int Index);

OGRFieldDefnH      OGR_FieldCreate(OGR_Layer *Layer,char *Field,char *Type,int Width,int Prec);

OGR_Layer*         OGR_LayerCreate(Tcl_Interp *Interp,char *Name,char *Desc,OGRwkbGeometryType Type,char **Options);
OGRLayerH          OGR_LayerInstanciate(OGR_File *File,OGR_Layer *Layer,char *Name,TGeoRef *GRef,char **Options);
void               OGR_LayerClean(OGR_Layer *Layer,int Index);
void               OGR_LayerCleanAll(TDataSpec *Spec,int Map,int Pos,int Seg);
int                OGR_LayerUpdate(OGR_Layer *Layer);
int                OGR_LayerClear(Tcl_Interp *Interp,OGR_Layer *Layer,int Field,double Value);
int                OGR_LayerDestroy(Tcl_Interp *Interp,char *Name);
int                OGR_LayerDefine(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
void               OGR_LayerFree(OGR_Layer *Layer);
OGR_Layer*         OGR_LayerGet(char *Name);
TDef*              OGR_LayerToDef(OGR_Layer *Layer,char *Field);
OGR_Layer*         OGR_LayerFromDef(OGR_Layer *Layer,char *Field,TDef *Def);
int                OGR_LayerSQLSelect(Tcl_Interp *Interp,char *Name,char *FileId,char *Statement,char *Geom);
int                OGR_LayerSelect(Tcl_Interp *Interp,OGR_Layer *Layer,Tcl_Obj *Predicates);
int                OGR_LayerSort(Tcl_Interp *Interp,OGR_Layer *Layer);
int                OGR_LayerRead(Tcl_Interp *Interp,char *Name,char *FileId,int Idx);
int                OGR_LayerReadFeature(Tcl_Interp *Interp,OGR_Layer *Layer);
int                OGR_LayerWrite(Tcl_Interp *Interp,char *Name,char *FileId);
int                OGR_LayerCopy(Tcl_Interp *Interp,char *From,char *To);
int                OGR_LayerRender(Tcl_Interp *Interp,Projection *Proj,ViewportItem *VP,OGR_Layer *Layer,int Mask);
void               OGR_LayerLimit(OGR_Layer *Layer);
int                OGR_LayerImport(Tcl_Interp *Interp,OGR_Layer *Layer,Tcl_Obj *Fields,int Grid);
int                OGR_LayerInterp(Tcl_Interp *Interp,OGR_Layer *Layer,int Field,TGeoRef *FromRef,TDef *FromDef,char Mode,int Final,int Prec,float *Index);
int                OGR_LayerStat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
void               OGR_LayerPreInit(OGR_Layer *Layer);
int                OGR_GridCell(OGRGeometryH Geom,TGeoRef *RefTo,TGeoRef *RefFrom,int I,int J,int Seg);

int                OGR_GeometryStat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
int                OGR_GeometryDefine(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
OGRwkbGeometryType OGR_GeometryNameToType(char *Name);
OGRGeometryH*      OGR_GeometryGet(char *Name);
int                OGR_GeometrySet(Tcl_Interp *Interp,OGRGeometryH Geom,Tcl_Obj *Desc);
int                OGR_GeometryDestroy(Tcl_Interp *Interp,char *Name);
int                OGR_GeometryProject(Projection *Proj,TGeoRef *GRef,OGR_Layer *Layer,OGRGeometryH Geom,double Elev,double Extrude,unsigned int Size);
void               OGR_GeometryRender(Projection *Proj,TGeoRef *GRef,OGR_Layer *Layer,OGRGeometryH Geom,double Elev,double Extrude);
Tcl_Obj*           OGR_GeometryGetObj(Tcl_Interp *Interp,OGRGeometryH Geom);
Tcl_Obj*           OGR_GeometryPut(Tcl_Interp *Interp,char *Name,OGRGeometryH Geom);

#endif
