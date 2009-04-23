/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : GeoRef.h
 * Creation     : Mars 2005 - J.P. Gauthier
 *
 * Description  : Fonctions de manipulations de projections.
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

#ifndef _GeoRef_h
#define _GeoRef_h

#include <tcl.h>

#include "projects.h"
#include "gdal.h"
#include "gdal_alg.h"
#include "ogr_api.h"
#include "ogr_srs_api.h"
#include "Vector.h"
#include "eerUtils.h"
#include "tclUtils.h"
#include "EZTile.h"

#define GRID_NONE     0x0
#define GRID_REGULAR  0x1
#define GRID_VARIABLE 0x2
#define GRID_WRAP     0x4
#define GRID_SPARSE   0x8
#define GRID_TILE     0x10
#define GRID_VERTICAL 0x20
#define GRID_RADIAL   0x40

//#define REFDEFAULT "GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]]"
//#define REFDEFAULT "GEOGCS[\"NAD83",DATUM[\"North_American_Datum_1983\",SPHEROID[\"GRS 1980\",6378137,298.257222101]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.0174532925199433]]"
#define REFDEFAULT "GEOGCS[\"WGS84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS84\",6378137,298.257223563]],PRIMEM[\"Greenwich\",0],UNIT[\"degree\",0.0174532925199433]]"

#define REFCOORD(REF,N,C)\
   if (REF->Grid[1]!='\0') {\
      REF->Project(REF,REF->Lon[N],REF->Lat[N],&C.lat,&C.lon,1,1);\
   } else {\
      C.lat=REF->Lat[N];\
      C.lon=REF->Lon[N];\
   }

#define TRANSFORM(REF,X,Y,IX,IY)\
   if (REF->Transform) {\
      X=REF->Transform[0]+REF->Transform[1]*(IX)+REF->Transform[2]*(IY);\
      Y=REF->Transform[3]+REF->Transform[4]*(IX)+REF->Transform[5]*(IY);\
   } else {\
      X=IX;\
      Y=IY;\
   }

#define INVTRANSFORM(REF,X,Y,IX,IY)\
   if (REF->InvTransform) {\
      X=REF->InvTransform[0]+REF->InvTransform[1]*(IX)+REF->InvTransform[2]*(IY);\
      Y=REF->InvTransform[3]+REF->InvTransform[4]*(IX)+REF->InvTransform[5]*(IY);\
   } else {\
      X=IX;\
      Y=IY;\
   }

#define GeoRef_ScanX(X) (((float*)GeoScanX)[X]-1.0)
#define GeoRef_ScanY(X) (((float*)GeoScanY)[X]-1.0)

/*Structure pour les coordonees latlon*/
typedef struct Coord {
   double Lon,Lat,Elev;
} Coord;

typedef union {
   Vect3d V;
   Coord  C;
} GeoVect;

struct TDataDef;
struct TGeoRef;

typedef int    (TGeoRef_Project)   (struct TGeoRef *Ref,double X,double Y,double *Lat,double *Lon,int Extrap,int Transform);
typedef int    (TGeoRef_UnProject) (struct TGeoRef *Ref,double *X,double *Y,double Lat,double Lon,int Extrap,int Transform);
typedef int    (TGeoRef_Value)     (struct TGeoRef *Ref,void *Def,char Mode,int C,double X,double Y,double Z,float *Length,float *ThetaXY);
typedef double (TGeoRef_Distance)  (struct TGeoRef *Ref,double X0,double Y0,double X1, double Y1);
typedef int    (TGeoRef_Check)     (struct TGeoRef *Ref);

typedef struct THeightRef {
   float *Levels;       /*Levels list*/
   int    LevelType;    /*Type of levels*/
   int    LevelNb;      /*Number of Levels*/
   float  PTop;         /*Pressure at top of atmosphere*/
   float  PRef;         /*Refference pressure*/
   float  RCoef[2];     /*Hybrid level coefficient*/
   float  ETop;         /*Eta coordinate a top*/
   float  *A,*B;        /*Pressure calculation factors*/
} TZRef;

typedef struct TGeoRef {
   char*   Name;
   int     Id;                                            /*Id de la georeference (>=0 = ezscint)*/
   int     NRef;                                          /*Nombre de reference a la georeference*/
   int     Type;                                          /*Type de grille*/
   int     BD;                                            /*Bordure*/
   int     X0,Y0,Z0,X1,Y1,Z1;                             /*Grid limits*/
   Vect3d **Pos;                                          /*Coordonnees des points de grilles (World)*/

   float *Levels;       /*Levels list*/
   int    LevelType;    /*Type of levels*/
   int    LevelNb;      /*Number of Levels*/
   float  Top;         /*Pressure at top of atmosphere*/
   float  Ref;         /*Refference pressure*/
   float  Coef[2];     /*Hybrid level coefficient*/
   float  ETop;         /*Eta coordinate a top*/
   float  *A,*B;        /*Pressure calculation factors*/

   Coord  Loc;                                            /*(Radar) Localisation du centre de reference*/
   double CTH,STH;                                        /*(Radar) sin and cos of sweep angle*/
   int    R;                                              /*(Radar) Rayon autour du centre de reference en bin*/
   double ResR,ResA;                                      /*(Radar) Resolutions en distance et azimuth*/

   float        *Lat,*Lon,*Hgt;                           /*Coordonnees des points de grilles (Spherical)*/
   float        *AX,*AY;                                  /*Coordonnees des points de grilles (Spherical)*/
   unsigned int *Idx,NIdx;                                /*Index dans les positions*/

   char                          Grid[3];                 /*Type de grille*/
   char                         *String;                  /*OpenGIS WKT String description*/
   OGRCoordinateTransformationH  Function,InvFunction;    /*Projection functions*/
   OGRSpatialReferenceH          Spatial;                 /*Spatial reference*/
   double                       *Transform,*InvTransform; /*Transformation functions*/
   void                         *GCPTransform;            /*GPC derivative transform (1,2,3 order)*/
   void                         *TPSTransform;            /*GPC Thin Spline transform*/
   void                         *RPCTransform;            /*GPC Rigorous Projection Model transform*/

   struct TGeoRef    *RefFrom;                            /*Georeference de reference (coupe verticale,...)*/

   TGeoRef_Project   *Project;
   TGeoRef_UnProject *UnProject;
   TGeoRef_Value     *Value;
   TGeoRef_Distance  *Distance;
} TGeoRef;

typedef struct TGeoScan {
   TGeoRef *ToRef,*FromRef;                               /*Referentiels de transformations*/
   double *X,*Y;
   unsigned int *V;                                       /*Coordonnees et valeurs*/
   unsigned int N,S;                                      /*Nombre de coordonnees et dimension*/
   int X0,Y0,Z0,X1,Y1,Z1;                                 /*Grid limits*/
   int DX,DY;                                             /*Longueur em X et Y*/
   int Valid;                                             /*Indicateur de validitee*/
} TGeoScan;

TGeoRef* GeoRef_Get(char *Name);
Tcl_Obj* GeoRef_Put(Tcl_Interp *Interp,char *Name,TGeoRef *Ref);
void     GeoRef_Incr(TGeoRef *Ref);
void     GeoRef_Decr(TGeoRef *Ref);
int      GeoRef_Destroy(Tcl_Interp *Interp,char *Name);
int      GeoRef_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
int      GeoRef_Within(TGeoRef *Ref0,TGeoRef *Ref1);
int      GeoRef_WithinRange(TGeoRef *Ref,double Lat0,double Lon0,double Lat1,double Lon1,int In);
int      GeoRef_Intersect(TGeoRef *Ref0,TGeoRef *Ref1,int *X0,int *Y0,int *X1,int *Y1,int Border);
int      GeoRef_Equal(TGeoRef *Ref0,TGeoRef *Ref1);
TGeoRef* GeoRef_New();
TGeoRef* GeoRef_Copy(TGeoRef *Ref);
TGeoRef *GeoRef_HardCopy(TGeoRef *Ref);
TGeoRef* GeoRef_Reference(TGeoRef *Ref);
void     GeoRef_Size(TGeoRef *Ref,int X0,int Y0,int Z0,int X1,int Y1,int Z1,int BD);
TGeoRef* GeoRef_Resize(TGeoRef *Ref,int NI,int NJ,int NK,int Type,float *Levels);
int      GeoRef_Free(TGeoRef *Ref);
void     GeoRef_Clear(TGeoRef *Ref,int New);
TGeoRef* GeoRef_Find(TGeoRef *Ref);
void     GeoRef_Qualify(TGeoRef *Ref);
int      GeoRef_Limits(TGeoRef *Ref,double *Lat0,double *Lon0,double *Lat1,double *Lon1);
int      GeoRef_Valid(TGeoRef *Ref);

TGeoRef* GeoRef_RDRSetup(double Lat,double Lon,double Height,int R,double ResR,double ResA,int NTheta,float *Theta);
TGeoRef* GeoRef_RPNSetup(int NI,int NJ,int NK,int Type,float *Levels,char *GRTYP,int IG1,int IG2,int IG3,int IG4,int FID);
TGeoRef* GeoRef_WKTSetup(int NI,int NJ,int NK,int Type,float *Levels,char *String,double *Transform,double *InvTransform,OGRSpatialReferenceH Spatial);
void     GeoRef_WKTSet(TGeoRef *Ref,char *String,double *Transform,double *InvTransform,OGRGeometryH Geometry);
TGeoRef* GeoRef_RDRCheck(double Lat,double Lon,double Height,double Radius,double ResR,double ResA);
void     GeoRef_Expand(TGeoRef *Ref);

void* GeoScan_Init(TGeoScan *Scan,TGeoRef *To,TGeoRef *From,int X0,int Y0,int X1,int Y1);
void  GeoScan_Clear(TGeoScan *Scan);

double GeoFunc_RadialPointRatio(Coord C1,Coord C2,Coord C3);
int    GeoFunc_RadialPointOn(Coord C1,Coord C2,Coord C3,Coord *CR);
int    GeoFunc_RadialIntersect(Coord C1,Coord C2,double CRS13,double CRS23,Coord *C3);

#endif
