/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : tclGDAL.h
 * Creation     : Juin 2004 - J.P. Gauthier
 *
 * Description  : Fonctions de manipulations et d'affichage de fichiers raster.
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

#ifndef _tclGDAL_h
#define _tclGDAL_h

#include "tkCanvVP.h"
#include "Projection.h"
#include "tclData.h"
#include "tclOGR.h"
#include "GeoRef.h"
#include "GeoTex.h"

#include "gdal.h"
#include "ogr_srs_api.h"
#include "cpl_string.h"

#define SCALEVALUE(V0,Vn,V) (Vn>V0?(V-V0)/(Vn-V0):1-(V-Vn)/(V0-Vn))

typedef struct GDAL_File {
   GDALDriverH     Driver;
   GDALDatasetH    Set;
   GDALColorInterp ColorInterp;

   TGeoRef     *Ref;       /*GeoReference*/
   char        *Name;
   char       **Meta;
   char        *Id;
   char         Mode;
   int          Sub;
} GDAL_File;

typedef struct GDAL_Band {
   Tcl_Obj      *Tag;
   GDAL_File    *File;

   GDALRasterBandH Band[256];
   GDAL_GCP     *GCPs;
   int           NbGCPs;

   TGeoTex      Tex;                  /* Tiled texture info */

   time_t        Date;                /* Valid time */
   TGeoRef      *Ref;                 /* GeoReference */
   TDataDef     *Def;                 /* Data definition */
   TDataSpec    *Spec;                /* Specification des donnees*/
   TDataStat    *Stat;                /* Data stats */
} GDAL_Band;

GDAL_File* GDAL_FileGet(Tcl_Interp *Interp,char *Id);
int        GDAL_FilePut(Tcl_Interp *Interp,GDAL_File *File);
int        GDAL_FileCreateCopy(Tcl_Interp *Interp,Tcl_Obj *Bands,char *Name,char *Driver);
int        GDAL_BandDefine(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
int        GDAL_BandStat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
TGeoRef*   GDAL_GeoRef(GDALDatasetH Set,GDALRasterBandH Band,GDAL_GCP *GCPs,int NbGCPs,int Nx,int Ny);
void       GDAL_Wipe();

int GDAL_FileOpen(Tcl_Interp *Interp,char *Id,char Mode,char *Name,char *Driver,char *Desc);
int GDAL_FileClose(Tcl_Interp *Interp,char *Id);

GDAL_Band* GDAL_BandCreate(Tcl_Interp *Interp,char *Name);
void       GDAL_BandClean(GDAL_Band *Band,int Map,int Pos,int Seg);
void       GDAL_BandCleanAll(TDataSpec *Spec,int Map,int Pos,int Seg);
GDAL_Band* GDAL_BandCopy(Tcl_Interp *Interp,GDAL_Band *Band,char *Name,int Def);
int        GDAL_BandDestroy(Tcl_Interp *Interp,char *Name);
GDAL_Band* GDAL_BandGet(char *Name);
void       GDAL_BandGetStat(GDAL_Band *Band);
int        GDAL_BandRead(Tcl_Interp *Interp,char *Name,char FileId[][128],int *Idxs,int NIdx,int X0,int Y0,int X1,int Y1,int BD,int Full);
int        GDAL_BandRender(Projection *Proj,ViewportItem *VP,GDAL_Band *Band);
int        GDAL_BandRenderTile(Projection *Proj,ViewportItem *VP,GDAL_Band *Band,TGeoTexTile *Tile,int Resolution);
int        GDAL_BandWrite(Tcl_Interp *Interp,Tcl_Obj *Bands,char *FileId,char **Options);
int        GDAL_BandTile(GDAL_Band *Band,Projection *Proj);
int        GDAL_Pick(Tcl_Interp *Interp,GDAL_Band *Band,Tcl_Obj *List);

void       Data_OGRProject(OGRGeometryH Geom,TGeoRef *FromRef,TGeoRef *ToRef);
int        Data_GridAverage(Tcl_Interp *Interp,TGeoRef *ToRef,TDataDef *ToDef,TGeoRef *FromRef,TDataDef *FromDef,double *Table,TDataDef *TmpDef,TDataInterp Mode,int Final);
int        Data_GridConservative(Tcl_Interp *Interp,TGeoRef *ToRef,TDataDef *ToDef,TGeoRef *FromRef,TDataDef *FromDef,char Mode,int Final,int Prec,Tcl_Obj *List);
int        Data_GridOGR(Tcl_Interp *Interp,TDataDef *Def,TGeoRef *Ref,OGR_Layer *Layer,char Mode,char Type,int Final,char *Field,double Value);
int        Data_GridOGRQuad(Tcl_Interp *Interp,Tcl_Obj *List,TDataDef *Def,TGeoRef *Ref,OGRGeometryH Geom,char Mode,char Type,double Area,double Value,int X0,int Y0,int X1,int Y1,int Z);

#endif
