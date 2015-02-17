/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : GeoTex.h
 * Creation     : Avril 2007 - J.P. Gauthier
 *
 * Description  : Fonctions de manipulations et de texture geographique.
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
#ifndef _GeoTex_h
#define _GeoTex_h

#include "Projection.h"

#define GeoTex_Index(T,NC,X,Y) (((Y+1)*(T->Nx+2)+(X+1))*(NC))
#define GeoTex_Val(Dim,Tile,Comp,Idx,Val) {\
switch(Dim) {\
   case GL_UNSIGNED_BYTE:  Val=((unsigned char*)Tile)[Comp+Idx];break;\
   case GL_BYTE:           Val=((char*)Tile)[Comp+Idx]; break;\
   case GL_UNSIGNED_SHORT: Val=((unsigned short*)Tile)[Comp+Idx];break;\
   case GL_SHORT:          Val=((short*)Tile)[Comp+Idx];break;\
   case GL_UNSIGNED_INT:   Val=((unsigned int*)Tile)[Comp+Idx];break;\
   case GL_INT:            Val=((int*)Tile)[Comp+Idx];break;\
   case GL_FLOAT:          Val=((float*)Tile)[Comp+Idx];break;\
   case GL_DOUBLE:         Val=((double*)Tile)[Comp+Idx];break;\
   }\
}

#define GEOTEX_NEW    0x0
#define GEOTEX_DATA   0x1
#define GEOTEX_COOR   0x2
#define GEOTEX_CLRDTA 0x4
#define GEOTEX_CLRCOO 0x8
#define GEOTEX_CLRTEX 0x10
#define GEOTEX_CLRALL 0x1C

typedef struct TGeoTexTile {
   char         *Data;         /* Tile data */
   char          Flag;         /* State flag*/
   unsigned int  Res;          /* Tile resolution*/
   unsigned int  Dx,Dy;        /* Pixel start */
   unsigned int  Nx,Ny;        /* Resolution pixel size */
   unsigned int  Rx,Ry;        /* Real pixel size */
   GDB_Box       Box;          /* Projected coordinates (Coverage) */
   Vect3d       *Tl;           /* Texture coordinates */
   Vect3d       *Nr;           /* Normal coordinates */
   unsigned int  Tlx,Tly;      /* Number of texture coordinates Tlx*Tly */
   GLuint        Tx;           /* GL Texture number */

   struct TGeoTexTile *Sub[4];  /* Tree branches */
} TGeoTexTile;

typedef struct TGeoTex {
   Tcl_ThreadId  ThreadId;
   Projection   *Proj;                /* Projection used for the tile*/
   TGeoTexTile  *Tile;                /* Sub tile list */
   unsigned int  Res,ResN;            /* Texture resolution*/
   int           Nx,Ny;               /* Pixel size */
   int           Indexed;             /* Texture indexee ?*/
   float         Scale[4];            /* Texture scaling*/
   float         Bias[4];             /* Texture bias*/
   GLuint        Type,IType;          /* OpenGL Texture types*/
   GLenum        Dim;                 /* OpenGL Texture dimension*/
} TGeoTex;

struct GDAL_Band;

Tcl_ThreadCreateType GeoTex_ThreadProc(ClientData clientData);

void         GeoTex_Signal(TGeoTex *Tex,int Flags);
void         GeoTex_Clear(TGeoTex *Tex,char Flags,int Res,int Nb);
TGeoTexTile* GeoTex_ClearTile(TGeoTexTile *Tile,char Flags,int Res,int Nb);
Tcl_Obj*     GeoTex_AppendValueObj(Tcl_Interp *Interp,TGeoTex *Tex,int X,int Y);
double       GeoTex_ValueGet(TDef *Def,TGeoTex *Tex,int Res,int C,double X,double Y,double Z);
int          GeoTex_Get(struct GDAL_Band *Band,TGeoTexTile *Tile);
int          GeoTex_Texture(struct GDAL_Band *Band,TGeoTexTile *Tile);
TGeoTexTile *GeoTex_Pick(TGeoTex *Tex,int Res,int *X,int *Y);
int          GeoTex_Limit(struct GDAL_Band *Band,TGeoTexTile *Tile,Projection *Proj);
void         GeoTex_Sample(struct GDAL_Band *Band,TGeoTexTile *Tile,Projection *Proj);
TGeoTexTile *GeoTex_New(struct GDAL_Band *Band,int Resolution,int X0,int Y0);
void         GeoTex_Qualify(struct GDAL_Band *Band);
TGeoTexTile *GeoTex_New(struct GDAL_Band *Band,int Resolution,int X0,int Y0);
int          GeoTex_Parse(struct GDAL_Band* Band,TGeoTexTile **Tile,Projection *Proj,ViewportItem *VP,int Resolution,int X0,int Y0);
int          GeoTex_Render(struct GDAL_Band *Band,TGeoTexTile *Tile,Projection *Proj,ViewportItem *VP,int Lock);
int          GeoTex_Resolution(struct GDAL_Band *Band,Projection *Proj);

#endif
