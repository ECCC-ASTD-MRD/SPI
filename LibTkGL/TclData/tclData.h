/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de divers fichiers de donnees
 * Fichier      : tclData.h
 * Creation     : Fevrier 2003 - J.P. Gauthier
 *
 * Description  : Fonctions generales applicables a divers types de donnees.
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

#ifndef _tclData_h
#define _tclData_h

#include <math.h>
#include <malloc.h>
#include <strings.h>
#include <stdlib.h>

#include "eerUtils.h"
#include "tclUtils.h"
#include "GeoRef.h"
#include "tclDataSpec.h"
#include "tclVector.h"
#include "Vector.h"
#include "glStuff.h"

#define TDTYPE(A,B)         (A->Type>B->Type?A:B)
#define DEFCLAMP(D,X,Y)      X=(X>D->NI-1?D->NI-1:(X<0?0:X));Y=(Y>D->NJ-1?D->NJ-1:(Y<0?0:Y))
#define DEF2DIN(D,I,J)      ((I)>=D->Limits[0][0] && (I)<=D->Limits[0][1] && (J)>=D->Limits[1][0] && (J)<=D->Limits[1][1])
#define DEF3DIN(D,I,J,K)    ((I)>=D->Limits[0][0] && (I)<=D->Limits[0][1] && (J)>=D->Limits[1][0] && (J)<=D->Limits[1][1] && (K)>=D->Limits[2][0] && (K)<=D->Limits[2][1])

#define REF_PROJ  0
#define REF_COOR  1
#define REF_GRID  2

#define PICK_NONE      0x00
#define PICK_TRAJ      0x01
#define PICK_OBS       0x02
#define PICK_METOBS    0x04
#define PICK_FSTDFIELD 0x08
#define PICK_ALL       0xFF

#define SPHERE 1
#define EXP    2
#define GAUSS  3
#define LINEAR 4
#define PARSE_LEN  4096

typedef enum {
   TD_NEAREST                        = 0,
   TD_LINEAR                         = 1,
   TD_CUBIC                          = 2,
   TD_NORMALIZED_CONSERVATIVE        = 3,
   TD_CONSERVATIVE                   = 4,
   TD_MAXIMUM                        = 5,
   TD_MINIMUM                        = 6,
   TD_SUM                            = 7,
   TD_AVERAGE                        = 8,
   TD_VARIANCE                       = 9,
   TD_SQUARE                         = 10,
   TD_NORMALIZED_COUNT               = 11,
   TD_COUNT                          = 12,
   TD_LENGTH_CONSERVATIVE            = 13,
   TD_LENGTH_ALIASED                 = 14,
   TD_LENGTH_NORMALIZED_CONSERVATIVE = 15,
   TD_NOP                            = 16,
   TD_ACCUM                          = 17,
   TD_BUFFER                         = 18
} TData_Interp;

typedef enum {
    TD_Unknown = 0,
    TD_Binary  = 1,
    TD_UByte   = 2,
    TD_Byte    = 3,
    TD_UInt16  = 4,
    TD_Int16   = 5,
    TD_UInt32  = 6,
    TD_Int32   = 7,
    TD_UInt64  = 8,
    TD_Int64   = 9,
    TD_Float32 = 10,
    TD_Float64 = 11,
} TData_Type;

extern int TData_Size[];

#define Def_Pointer(Def,Comp,Idx,Ptr) Ptr=Def->Data[Comp]+Idx*TData_Size[Def->Type];
#define Def_PointerMode(Def,Idx,Ptr) Ptr=Def->Mode+Idx*TData_Size[Def->Type];

#define Def_Set(Def,Comp,Idx,Val) {\
switch(Def->Type) {\
   case TD_Unknown:break;\
   case TD_Binary: break;\
   case TD_UByte:  ((unsigned char*)Def->Data[Comp])[Idx]=Val;break;\
   case TD_Byte:   ((char*)Def->Data[Comp])[Idx]=Val; break;\
   case TD_UInt16: ((unsigned short*)Def->Data[Comp])[Idx]=Val;break;\
   case TD_Int16:  ((short*)Def->Data[Comp])[Idx]=Val;break;\
   case TD_UInt32: ((unsigned int*)Def->Data[Comp])[Idx]=Val;break;\
   case TD_Int32:  ((int*)Def->Data[Comp])[Idx]=Val;break;\
   case TD_UInt64: ((unsigned long long*)Def->Data[Comp])[Idx]=Val;break;\
   case TD_Int64:  ((long long*)Def->Data[Comp])[Idx]=Val;break;\
   case TD_Float32:((float*)Def->Data[Comp])[Idx]=Val;break;\
   case TD_Float64:((double*)Def->Data[Comp])[Idx]=Val;break;\
   }\
}

#define Def_GetQuad(Def,Comp,Idx,Val) {\
switch(Def->Type) {\
   case TD_Unknown:Val[0]=Val[1]=Val[2]=Val[3]=0.0;break;\
   case TD_Binary: Val[0]=Val[1]=Val[2]=Val[3]=0.0;break;\
   case TD_UByte:  Val[0]=((unsigned char*)Def->Data[Comp])[Idx[0]];\
                   Val[1]=((unsigned char*)Def->Data[Comp])[Idx[1]];\
                   Val[2]=((unsigned char*)Def->Data[Comp])[Idx[2]];\
                   Val[3]=((unsigned char*)Def->Data[Comp])[Idx[3]];\
                   break;\
   case TD_Byte:   Val[0]=((char*)Def->Data[Comp])[Idx[0]];\
                   Val[1]=((char*)Def->Data[Comp])[Idx[1]];\
                   Val[2]=((char*)Def->Data[Comp])[Idx[2]];\
                   Val[3]=((char*)Def->Data[Comp])[Idx[3]];\
                   break;\
   case TD_UInt16: Val[0]=((unsigned short*)Def->Data[Comp])[Idx[0]];\
                   Val[1]=((unsigned short*)Def->Data[Comp])[Idx[1]];\
                   Val[2]=((unsigned short*)Def->Data[Comp])[Idx[2]];\
                   Val[3]=((unsigned short*)Def->Data[Comp])[Idx[3]];\
                   break;\
   case TD_Int16:  Val[0]=((short*)Def->Data[Comp])[Idx[0]];\
                   Val[1]=((short*)Def->Data[Comp])[Idx[1]];\
                   Val[2]=((short*)Def->Data[Comp])[Idx[2]];\
                   Val[3]=((short*)Def->Data[Comp])[Idx[3]];\
                   break;\
   case TD_UInt32: Val[0]=((unsigned int*)Def->Data[Comp])[Idx[0]];\
                   Val[1]=((unsigned int*)Def->Data[Comp])[Idx[1]];\
                   Val[2]=((unsigned int*)Def->Data[Comp])[Idx[2]];\
                   Val[3]=((unsigned int*)Def->Data[Comp])[Idx[3]];\
                   break;\
   case TD_Int32:  Val[0]=((int*)Def->Data[Comp])[Idx[0]];\
                   Val[1]=((int*)Def->Data[Comp])[Idx[1]];\
                   Val[2]=((int*)Def->Data[Comp])[Idx[2]];\
                   Val[3]=((int*)Def->Data[Comp])[Idx[3]];\
                   break;\
   case TD_UInt64: Val[0]=((unsigned long long*)Def->Data[Comp])[Idx[0]];\
                   Val[1]=((unsigned long long*)Def->Data[Comp])[Idx[1]];\
                   Val[2]=((unsigned long long*)Def->Data[Comp])[Idx[2]];\
                   Val[3]=((unsigned long long*)Def->Data[Comp])[Idx[3]];\
                   break;\
   case TD_Int64:  Val[0]=((long long*)Def->Data[Comp])[Idx[0]];\
                   Val[1]=((long long*)Def->Data[Comp])[Idx[1]];\
                   Val[2]=((long long*)Def->Data[Comp])[Idx[2]];\
                   Val[3]=((long long*)Def->Data[Comp])[Idx[3]];\
                   break;\
   case TD_Float32:Val[0]=((float*)Def->Data[Comp])[Idx[0]];\
                   Val[1]=((float*)Def->Data[Comp])[Idx[1]];\
                   Val[2]=((float*)Def->Data[Comp])[Idx[2]];\
                   Val[3]=((float*)Def->Data[Comp])[Idx[3]];\
                   break;\
   case TD_Float64:Val[0]=((double*)Def->Data[Comp])[Idx[0]];\
                   Val[1]=((double*)Def->Data[Comp])[Idx[1]];\
                   Val[2]=((double*)Def->Data[Comp])[Idx[2]];\
                   Val[3]=((double*)Def->Data[Comp])[Idx[3]];\
                   break;\
   }\
}

#define Def_Get(Def,Comp,Idx,Val) {\
switch(Def->Type) {\
   case TD_Unknown:Val=0.0;break;\
   case TD_Binary: Val=0.0;break;\
   case TD_UByte:  Val=((unsigned char*)Def->Data[Comp])[Idx];break;\
   case TD_Byte:   Val=((char*)Def->Data[Comp])[Idx]; break;\
   case TD_UInt16: Val=((unsigned short*)Def->Data[Comp])[Idx];break;\
   case TD_Int16:  Val=((short*)Def->Data[Comp])[Idx];break;\
   case TD_UInt32: Val=((unsigned int*)Def->Data[Comp])[Idx];break;\
   case TD_Int32:  Val=((int*)Def->Data[Comp])[Idx];break;\
   case TD_UInt64: Val=((unsigned long long*)Def->Data[Comp])[Idx];break;\
   case TD_Int64:  Val=((long long*)Def->Data[Comp])[Idx];break;\
   case TD_Float32:Val=((float*)Def->Data[Comp])[Idx];break;\
   case TD_Float64:Val=((double*)Def->Data[Comp])[Idx];break;\
   }\
}

#define Def_GetMod(Def,Idx,Val) {\
switch(Def->Type) {\
   case TD_Unknown:Val=0.0;break;\
   case TD_Binary: Val=0.0;break;\
   case TD_UByte:  Val=((unsigned char*)Def->Mode)[Idx];break;\
   case TD_Byte:   Val=((char*)Def->Mode)[Idx]; break;\
   case TD_UInt16: Val=((unsigned short*)Def->Mode)[Idx];break;\
   case TD_Int16:  Val=((short*)Def->Mode)[Idx];break;\
   case TD_UInt32: Val=((unsigned int*)Def->Mode)[Idx];break;\
   case TD_Int32:  Val=((int*)Def->Mode)[Idx];break;\
   case TD_UInt64: Val=((unsigned long long*)Def->Mode)[Idx];break;\
   case TD_Int64:  Val=((long long*)Def->Mode)[Idx];break;\
   case TD_Float32:Val=((float*)Def->Mode)[Idx];break;\
   case TD_Float64:Val=((double*)Def->Mode)[Idx];break;\
   }\
}

#define Def_SetMod(Def,Idx,Val) {\
switch(Def->Type) {\
   case TD_Unknown:break;\
   case TD_Binary: break;\
   case TD_UByte:  ((unsigned char*)Def->Mode)[Idx]=Val;break;\
   case TD_Byte:   ((char*)Def->Mode)[Idx]=Val; break;\
   case TD_UInt16: ((unsigned short*)Def->Mode)[Idx]=Val;break;\
   case TD_Int16:  ((short*)Def->Mode)[Idx]=Val;break;\
   case TD_UInt32: ((unsigned int*)Def->Mode)[Idx]=Val;break;\
   case TD_Int32:  ((int*)Def->Mode)[Idx]=Val;break;\
   case TD_UInt64: ((unsigned long long*)Def->Mode)[Idx]=Val;break;\
   case TD_Int64:  ((long long*)Def->Mode)[Idx]=Val;break;\
   case TD_Float32:((float*)Def->Mode)[Idx]=Val;break;\
   case TD_Float64:((double*)Def->Mode)[Idx]=Val;break;\
   }\
}

typedef struct TDataStat {
   double Min,Max,Avg;      /*Minimum maximum et moyenne de l'enregistrement*/
   Coord  MinLoc,MaxLoc;    /*Coordonnees des minimums et maximums*/
} TDataStat;

typedef struct TDataDef {
   double  NoData;            /*Valeur de novalue*/
   double *Buffer;            /*Buffer temporaire*/
   int    *Accum;             /*Accumulation Buffer temporaire*/
   char   *Mask;              /*Masque a appliquer au traitement sur le champs*/
   char   *Data[4];           /*Composantes du champs (Pointeurs sur les donnees)*/
   char   *Mode;              /*Module des champs Data is vectoriel*/
   char   *Pres;              /*Pression au sol*/
   OGRGeometryH *Pick,*Poly;  /*Geometry used in various interpolation method*/

   TData_Type Type;           /*Type de donnees du champs*/
   int NI,NJ,NK,NC;           /*Dimensions du champs*/

   int     CellDim;           /*Defined grid point coverage, point=1 or area=2*/
   char    Container;         /*Container pointant sur d'autres donnees*/
   int     Level;             /*Niveau courant*/
   int     Limits[3][2];      /*Limits of processing in grid points*/
   double  CoordLimits[2][2]; /*Limits of processing in latlon*/
} TDataDef;

struct TData;

typedef Vect3d* (TData_Grid)     (struct TData *Field,void *Proj,int Level);
typedef void    (TData_Free)     (struct TData *Field);
typedef void    (TData_Copy)     (void *To, void *From);
typedef int     (TData_ReadCube) (Tcl_Interp *Interp,struct TData *Field,int Invert,int LevelFrom,int LevelTo);
typedef void    (TData_Set)      (struct TData *Field);

typedef struct TData {
   Tcl_Obj      *Tag;
   void         *Head;      /*Entete de l'enregistrement (metadata)*/

   TGeoRef      *Ref;       /*Reference geographique horizontale*/
   TDataDef     *Def;       /*Definition des donnees*/
   TDataSpec    *Spec;      /*Specification des donnees (pour l'affichage)*/
   TDataStat    *Stat;      /*Statistiques de l'enregistrement*/

   TData_Set      *Set;     /*Fonction d'initialisation*/
   TData_Free     *Free;    /*Fonction de liberation*/
   TData_Copy     *Copy;    /*Fonction de copie de champs*/
   TData_ReadCube *ReadCube;/*Fonction de lecture du cube de donnees (niveaux verticaux)*/
   TData_Grid     *Grid;    /*Fonction de recuperation de la grille (geo-localisation)*/

   TList  *Segments;        /*Liste d'objets de rendue*/
   float  *Map;             /*Texture du champs*/
} TData;

#include "Vertex.h"

void     Data_FromString(char *String,TDataDef *Def,int Comp,int Idx);
Tcl_Obj *Data_Val2Obj(TDataDef *Def,double Val);

void     Data_Clean(TData *Data,int Map,int Pos,int Seg);
void     Data_CleanAll(TDataSpec *Spec,int Map,int Pos,int Seg);
int      Data_Free(TData *Field);
int      Data_FreeHash(Tcl_Interp *Interp,char *Name);
TData*   Data_Copy(Tcl_Interp *Interp,TData *Field,char *Name,int Def);
int      Data_Cut(Tcl_Interp *Interp,TData **Field,char *Cut,double *Lat,double *Lon,int NbF,int NbC);
TData*   Data_Get(char *Name);
TData*   Data_GetShell(Tcl_Interp *Interp,char *Name);
void     Data_GetStat(TData *Field);
Tcl_Obj* Data_HighLow(Tcl_Interp *Interp,TData *Field,int High,int Tile);
TData*   Data_Valid(Tcl_Interp *Interp,char *Name,int NI,int NJ,int NK,int Dim,TData_Type Type);
Tcl_Obj* Data_AppendValueObj(Tcl_Interp *Interp,TDataDef *Def,int X,int Y);
int      Data_ValSet(TData *Field,float I,float J,float Val);
void     Data_ValGetMatrix(Tcl_Interp *Interp,TData *Field,int Type);
int      Data_ValPutMatrix(Tcl_Interp *Interp,TData *Field,Tcl_Obj *List);
int      Data_Within(TData *Field,float Val);
int      Data_WithinNb(TData *Field);
void     Data_Wipe();
void     Data_PreInit(TData *Data);
int      Data_GridInterpolate(Tcl_Interp *Interp,TGeoRef *ToRef,TDataDef *ToDef,TGeoRef *FromRef,TDataDef *FromDef);
int      Data_Stat(Tcl_Interp *Interp,TData *Field,int Objc,Tcl_Obj *CONST Objv[]);

int       Data_DefCompat(TDataDef *DefTo,TDataDef *DefFrom);
TDataDef *Data_DefCopy(TDataDef *Def);
TDataDef *Data_DefCopyPromote(TDataDef *Def,TData_Type Type);
void      Data_DefFree(TDataDef *Def);
void      Data_DefClear(TDataDef *Def);
TDataDef *Data_DefNew(int NI,int NJ,int NK,int Dim,TData_Type Type);
TDataDef *Data_DefResize(TDataDef *Def,int NI,int NJ,int NK);
int       Data_DefSort(Tcl_Interp *Interp,Tcl_Obj *List);
int       Data_DefTile(TDataDef *DefTo,TDataDef *DefTile,int X0,int Y0);

double    Data_Level2Meter(int Type,double Level);
double    Data_Level2Pressure(TGeoRef *Ref,const double Level,double P0,int K);

#endif
