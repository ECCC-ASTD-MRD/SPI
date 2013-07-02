/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Projection orthographique de la carte vectorielle.
 * Fichier   : GeoData.h
 * Creation  : Novembre 2001 - J.P. Gauthier
 *
 * Description: Fichier de definition du module de donnees geographiques.
 *
 * Remarques :
 *   -On utilise la base de donnees GBD de Michel Grenier
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
 *==============================================================================
 */

#ifndef _GeoData_h_
#define _GeoData_h_

#include "tcl.h"
#include "gdb.h"
#include "GeoRef.h"

#define GDB_DEGX 72
#define GDB_DEGY 36

#define GDB_RES 128
#define GDB_VIS 1
#define GDB_LOW 0
#define GDB_OUT -1

#define GDB_PNT 0
#define GDB_LIN 1
#define GDB_FIL 2
#define GDB_MAP 3
#define GDB_TXT 4

#define GDB_DATA -1
#define GDB_LAND 1
#define GDB_SEA  2

#define GDB_NONE   0x00
#define GDB_VECTOR 0x01
#define GDB_RASTER 0x02
#define GDB_TEXT   0x04
#define GDB_MASK   0x08
#define GDB_FILL   0x10
#define GDB_ALL    0x1F

#define GDB_FORCE  0xFF

#define VBuffer_BigEnough(V,SIZE) (V->Size>=(SIZE))

typedef struct TVBuffer {
   Vect3d      *Buffer;
   unsigned int Size;
   unsigned int Incr;
   unsigned int Max;
} TVBuffer;

unsigned int VBuffer_Init(void);
TVBuffer*    VBuffer_Get(void);
Vect3d*      VBuffer_Copy(Vect3d *To,unsigned int Size);
unsigned int VBuffer_Check(void);
Vect3d*      VBuffer_Alloc(unsigned int Size);

typedef enum { GDB_TYPE_MAP,GDB_TYPE_TEXT,GDB_TYPE_COAST,GDB_TYPE_LAKE,GDB_TYPE_RIVER,GDB_TYPE_POLIT,GDB_TYPE_ADMIN,GDB_TYPE_CITY,GDB_TYPE_ROAD,GDB_TYPE_RAIL,GDB_TYPE_PLACE } GDB_Type;

typedef struct GDB_Params {
   int    Topo,Bath,Text,Mask;
   int    Coast,Lake,River,Polit,Admin,City,Road,Rail,Place;
   int    CoordLoc,CoordNum;
   double CoordDef;
} GDB_Params;

typedef struct GDB_Box {
   int    Nb;
   Coord  Co[8];
   Vect3d Vr[8];
} GDB_Box;

typedef struct GDB_Geo {
   Vect3d         *Loc;
   GDB_Box         Box;
   unsigned int    List;
   struct GDB_Geo *Next;
} GDB_Geo;

typedef struct GDB_Txt {
   Coord           Co;
   char           *String;
   struct GDB_Txt *Next;
} GDB_Txt;

typedef struct GDB_Map {
   Vect3d        *Vr;
   int            Width,Height,Size;
   short         *Map;
   unsigned char *Mask;
   unsigned int   Tex;
} GDB_Map;

typedef struct GDB_Tile {
   unsigned char Res;
   char          Flag;
   GDB_Box       Box;

   struct GDB_Map  Topo;
   struct GDB_Geo *Coast,*FCoast,*FCoastIn;
   struct GDB_Geo *Lake,*FLake,*FLakeIn;
   struct GDB_Geo *River;
   struct GDB_Geo *Polit;
   struct GDB_Geo *Admin;
   struct GDB_Geo *Road;
   struct GDB_Geo *Rail;
   struct GDB_Txt *TPlace;
   struct GDB_Txt *TCity;
} GDB_Tile;

typedef struct GDB_Data {
   int         DegT,DegX,DegY,Res;
   GDB_Tile    Tile[GDB_DEGX][GDB_DEGY];
   GDB_Params  Params;
} GDB_Data;

int  GDB_Init(GDB_Data *Geo);
int  GDB_GetMap(GDB_Data *Geo,double Lat,double Lon);
void GDB_GeoFree(GDB_Geo *Geo);
void GDB_TileFree(GDB_Tile *Tile,int Force);
void GDB_TileFreeAll(GDB_Data *Geo,int Type);
void GDB_TileFreeType(GDB_Data *Geo,GDB_Type Type);

Vect3d*      VBuffer_Alloc(unsigned int Size);
Vect3d*      VBuffer_Copy(Vect3d *To,unsigned int Size);
unsigned int VBuffer_Check();
unsigned int VBuffer_Init();

Tcl_ThreadCreateType GDB_ThreadProc(ClientData clientData);
int                  GDB_ThreadQueueIsEmpty(Tcl_ThreadId Id);
#endif
