/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de divers fichiers de donnees
 * Fichier      : tclDataDef.h
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

#ifndef _tclDataDef_h
#define _tclDataDef_h

#include <math.h>
#include <strings.h>
#include <stdlib.h>

#include "eerUtils.h"
#include "eerStruct.h"
#include "tclUtils.h"
#include "ogr_api.h"

#define DEFSELECTTYPE(A,B)  (A->Type>B->Type?A:B)
#define DEFCLAMP(D,X,Y)      X=(X>D->NI-1?D->NI-1:(X<0?0:X));Y=(Y>D->NJ-1?D->NJ-1:(Y<0?0:Y))
#define DEF2DIN(D,I,J)      ((I)>=D->Limits[0][0] && (I)<=D->Limits[0][1] && (J)>=D->Limits[1][0] && (J)<=D->Limits[1][1])
#define DEF3DIN(D,I,J,K)    ((I)>=D->Limits[0][0] && (I)<=D->Limits[0][1] && (J)>=D->Limits[1][0] && (J)<=D->Limits[1][1] && (K)>=D->Limits[2][0] && (K)<=D->Limits[2][1])

#define Def_Pointer(DEF,COMP,IDX,PTR) PTR=DEF->Data[COMP]+(IDX)*TData_Size[DEF->Type];
#define Def_PointerMode(DEF,IDX,PTR) PTR=DEF->Mode+(IDX)*TData_Size[DEF->Type];

#define Def_Set(DEF,COMP,IDX,VAL) {\
switch(DEF->Type) {\
   case TD_Unknown:break;\
   case TD_Binary: break;\
   case TD_UByte:  ((unsigned char*)DEF->Data[COMP])[IDX]=VAL;break;\
   case TD_Byte:   ((char*)DEF->Data[COMP])[IDX]=VAL; break;\
   case TD_UInt16: ((unsigned short*)DEF->Data[COMP])[IDX]=VAL;break;\
   case TD_Int16:  ((short*)DEF->Data[COMP])[IDX]=VAL;break;\
   case TD_UInt32: ((unsigned int*)DEF->Data[COMP])[IDX]=VAL;break;\
   case TD_Int32:  ((int*)DEF->Data[COMP])[IDX]=VAL;break;\
   case TD_UInt64: ((unsigned long long*)DEF->Data[COMP])[IDX]=VAL;break;\
   case TD_Int64:  ((long long*)DEF->Data[COMP])[IDX]=VAL;break;\
   case TD_Float32:((float*)DEF->Data[COMP])[IDX]=VAL;break;\
   case TD_Float64:((double*)DEF->Data[COMP])[IDX]=VAL;break;\
   }\
}

#define Def_GetQuad(DEF,COMP,IDX,VAL) {\
switch(DEF->Type) {\
   case TD_UByte:  VAL[0]=((unsigned char*)DEF->Data[COMP])[IDX[0]];\
                   VAL[1]=((unsigned char*)DEF->Data[COMP])[IDX[1]];\
                   VAL[2]=((unsigned char*)DEF->Data[COMP])[IDX[2]];\
                   VAL[3]=((unsigned char*)DEF->Data[COMP])[IDX[3]];\
                   break;\
   case TD_Byte:   VAL[0]=((char*)DEF->Data[COMP])[IDX[0]];\
                   VAL[1]=((char*)DEF->Data[COMP])[IDX[1]];\
                   VAL[2]=((char*)DEF->Data[COMP])[IDX[2]];\
                   VAL[3]=((char*)DEF->Data[COMP])[IDX[3]];\
                   break;\
   case TD_UInt16: VAL[0]=((unsigned short*)DEF->Data[COMP])[IDX[0]];\
                   VAL[1]=((unsigned short*)DEF->Data[COMP])[IDX[1]];\
                   VAL[2]=((unsigned short*)DEF->Data[COMP])[IDX[2]];\
                   VAL[3]=((unsigned short*)DEF->Data[COMP])[IDX[3]];\
                   break;\
   case TD_Int16:  VAL[0]=((short*)DEF->Data[COMP])[IDX[0]];\
                   VAL[1]=((short*)DEF->Data[COMP])[IDX[1]];\
                   VAL[2]=((short*)DEF->Data[COMP])[IDX[2]];\
                   VAL[3]=((short*)DEF->Data[COMP])[IDX[3]];\
                   break;\
   case TD_UInt32: VAL[0]=((unsigned int*)DEF->Data[COMP])[IDX[0]];\
                   VAL[1]=((unsigned int*)DEF->Data[COMP])[IDX[1]];\
                   VAL[2]=((unsigned int*)DEF->Data[COMP])[IDX[2]];\
                   VAL[3]=((unsigned int*)DEF->Data[COMP])[IDX[3]];\
                   break;\
   case TD_Int32:  VAL[0]=((int*)DEF->Data[COMP])[IDX[0]];\
                   VAL[1]=((int*)DEF->Data[COMP])[IDX[1]];\
                   VAL[2]=((int*)DEF->Data[COMP])[IDX[2]];\
                   VAL[3]=((int*)DEF->Data[COMP])[IDX[3]];\
                   break;\
   case TD_UInt64: VAL[0]=((unsigned long long*)DEF->Data[COMP])[IDX[0]];\
                   VAL[1]=((unsigned long long*)DEF->Data[COMP])[IDX[1]];\
                   VAL[2]=((unsigned long long*)DEF->Data[COMP])[IDX[2]];\
                   VAL[3]=((unsigned long long*)DEF->Data[COMP])[IDX[3]];\
                   break;\
   case TD_Int64:  VAL[0]=((long long*)DEF->Data[COMP])[IDX[0]];\
                   VAL[1]=((long long*)DEF->Data[COMP])[IDX[1]];\
                   VAL[2]=((long long*)DEF->Data[COMP])[IDX[2]];\
                   VAL[3]=((long long*)DEF->Data[COMP])[IDX[3]];\
                   break;\
   case TD_Float32:VAL[0]=((float*)DEF->Data[COMP])[IDX[0]];\
                   VAL[1]=((float*)DEF->Data[COMP])[IDX[1]];\
                   VAL[2]=((float*)DEF->Data[COMP])[IDX[2]];\
                   VAL[3]=((float*)DEF->Data[COMP])[IDX[3]];\
                   break;\
   case TD_Float64:VAL[0]=((double*)DEF->Data[COMP])[IDX[0]];\
                   VAL[1]=((double*)DEF->Data[COMP])[IDX[1]];\
                   VAL[2]=((double*)DEF->Data[COMP])[IDX[2]];\
                   VAL[3]=((double*)DEF->Data[COMP])[IDX[3]];\
                   break;\
   case TD_Unknown:\
   case TD_Binary: \
   default:        VAL[0]=VAL[1]=VAL[2]=VAL[3]=0.0;break;\
   }\
}

#define Def_GetQuadMod(DEF,IDX,VAL) {\
switch(DEF->Type) {\
   case TD_UByte:  VAL[0]=((unsigned char*)DEF->Mode)[IDX[0]];\
                   VAL[1]=((unsigned char*)DEF->Mode)[IDX[1]];\
                   VAL[2]=((unsigned char*)DEF->Mode)[IDX[2]];\
                   VAL[3]=((unsigned char*)DEF->Mode)[IDX[3]];\
                   break;\
   case TD_Byte:   VAL[0]=((char*)DEF->Mode)[IDX[0]];\
                   VAL[1]=((char*)DEF->Mode)[IDX[1]];\
                   VAL[2]=((char*)DEF->Mode)[IDX[2]];\
                   VAL[3]=((char*)DEF->Mode)[IDX[3]];\
                   break;\
   case TD_UInt16: VAL[0]=((unsigned short*)DEF->Mode)[IDX[0]];\
                   VAL[1]=((unsigned short*)DEF->Mode)[IDX[1]];\
                   VAL[2]=((unsigned short*)DEF->Mode)[IDX[2]];\
                   VAL[3]=((unsigned short*)DEF->Mode)[IDX[3]];\
                   break;\
   case TD_Int16:  VAL[0]=((short*)DEF->Mode)[IDX[0]];\
                   VAL[1]=((short*)DEF->Mode)[IDX[1]];\
                   VAL[2]=((short*)DEF->Mode)[IDX[2]];\
                   VAL[3]=((short*)DEF->Mode)[IDX[3]];\
                   break;\
   case TD_UInt32: VAL[0]=((unsigned int*)DEF->Mode)[IDX[0]];\
                   VAL[1]=((unsigned int*)DEF->Mode)[IDX[1]];\
                   VAL[2]=((unsigned int*)DEF->Mode)[IDX[2]];\
                   VAL[3]=((unsigned int*)DEF->Mode)[IDX[3]];\
                   break;\
   case TD_Int32:  VAL[0]=((int*)DEF->Mode)[IDX[0]];\
                   VAL[1]=((int*)DEF->Mode)[IDX[1]];\
                   VAL[2]=((int*)DEF->Mode)[IDX[2]];\
                   VAL[3]=((int*)DEF->Mode)[IDX[3]];\
                   break;\
   case TD_UInt64: VAL[0]=((unsigned long long*)DEF->Mode)[IDX[0]];\
                   VAL[1]=((unsigned long long*)DEF->Mode)[IDX[1]];\
                   VAL[2]=((unsigned long long*)DEF->Mode)[IDX[2]];\
                   VAL[3]=((unsigned long long*)DEF->Mode)[IDX[3]];\
                   break;\
   case TD_Int64:  VAL[0]=((long long*)DEF->Mode)[IDX[0]];\
                   VAL[1]=((long long*)DEF->Mode)[IDX[1]];\
                   VAL[2]=((long long*)DEF->Mode)[IDX[2]];\
                   VAL[3]=((long long*)DEF->Mode)[IDX[3]];\
                   break;\
   case TD_Float32:VAL[0]=((float*)DEF->Mode)[IDX[0]];\
                   VAL[1]=((float*)DEF->Mode)[IDX[1]];\
                   VAL[2]=((float*)DEF->Mode)[IDX[2]];\
                   VAL[3]=((float*)DEF->Mode)[IDX[3]];\
                   break;\
   case TD_Float64:VAL[0]=((double*)DEF->Mode)[IDX[0]];\
                   VAL[1]=((double*)DEF->Mode)[IDX[1]];\
                   VAL[2]=((double*)DEF->Mode)[IDX[2]];\
                   VAL[3]=((double*)DEF->Mode)[IDX[3]];\
                   break;\
   case TD_Unknown:\
   case TD_Binary:\
   default:        VAL[0]=VAL[1]=VAL[2]=VAL[3]=0.0;\
   }\
}

#define Def_Get(DEF,COMP,IDX,VAL) {\
switch(DEF->Type) {\
   case TD_UByte:  VAL=((unsigned char*)DEF->Data[COMP])[IDX];break;\
   case TD_Byte:   VAL=((char*)DEF->Data[COMP])[IDX]; break;\
   case TD_UInt16: VAL=((unsigned short*)DEF->Data[COMP])[IDX];break;\
   case TD_Int16:  VAL=((short*)DEF->Data[COMP])[IDX];break;\
   case TD_UInt32: VAL=((unsigned int*)DEF->Data[COMP])[IDX];break;\
   case TD_Int32:  VAL=((int*)DEF->Data[COMP])[IDX];break;\
   case TD_UInt64: VAL=((unsigned long long*)DEF->Data[COMP])[IDX];break;\
   case TD_Int64:  VAL=((long long*)DEF->Data[COMP])[IDX];break;\
   case TD_Float32:VAL=((float*)DEF->Data[COMP])[IDX];break;\
   case TD_Float64:VAL=((double*)DEF->Data[COMP])[IDX];break;\
   case TD_Unknown:\
   case TD_Binary:\
   default        :VAL=0.0;\
   }\
}

#define Def_GetMod(DEF,IDX,VAL) {\
switch(DEF->Type) {\
   case TD_UByte:  VAL=((unsigned char*)DEF->Mode)[IDX];break;\
   case TD_Byte:   VAL=((char*)DEF->Mode)[IDX]; break;\
   case TD_UInt16: VAL=((unsigned short*)DEF->Mode)[IDX];break;\
   case TD_Int16:  VAL=((short*)DEF->Mode)[IDX];break;\
   case TD_UInt32: VAL=((unsigned int*)DEF->Mode)[IDX];break;\
   case TD_Int32:  VAL=((int*)DEF->Mode)[IDX];break;\
   case TD_UInt64: VAL=((unsigned long long*)DEF->Mode)[IDX];break;\
   case TD_Int64:  VAL=((long long*)DEF->Mode)[IDX];break;\
   case TD_Float32:VAL=((float*)DEF->Mode)[IDX];break;\
   case TD_Float64:VAL=((double*)DEF->Mode)[IDX];break;\
   case TD_Unknown:\
   case TD_Binary:\
   default        :VAL=0.0;\
   }\
}

#define Def_SetMod(DEF,IDX,VAL) {\
switch(DEF->Type) {\
   case TD_Unknown:break;\
   case TD_Binary: break;\
   case TD_UByte:  ((unsigned char*)DEF->Mode)[IDX]=VAL;break;\
   case TD_Byte:   ((char*)DEF->Mode)[IDX]=VAL; break;\
   case TD_UInt16: ((unsigned short*)DEF->Mode)[IDX]=VAL;break;\
   case TD_Int16:  ((short*)DEF->Mode)[IDX]=VAL;break;\
   case TD_UInt32: ((unsigned int*)DEF->Mode)[IDX]=VAL;break;\
   case TD_Int32:  ((int*)DEF->Mode)[IDX]=VAL;break;\
   case TD_UInt64: ((unsigned long long*)DEF->Mode)[IDX]=VAL;break;\
   case TD_Int64:  ((long long*)DEF->Mode)[IDX]=VAL;break;\
   case TD_Float32:((float*)DEF->Mode)[IDX]=VAL;break;\
   case TD_Float64:((double*)DEF->Mode)[IDX]=VAL;break;\
   }\
}

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


//static int TData_Size[]={ 0,0,1,1,2,2,4,4,8,8,4,8 };
extern int TData_Size[];

typedef struct TDataDef {
   double  NoData;            /*Valeur de novalue*/
   double *Buffer;            /*Buffer temporaire*/
   int    *Accum;             /*Accumulation Buffer temporaire*/
   char   *Mask;              /*Masque a appliquer au traitement sur le champs*/
   char   *Data[4];           /*Composantes du champs (Pointeurs sur les donnees)*/
   char   *Mode;              /*Module des champs Data is vectoriel*/
   float  *Pres,*Height;      /*Pression au sol*/
   OGRGeometryH *Pick,*Poly;  /*Geometry used in various interpolation method*/

   TData_Type Type;           /*Type de donnees du champs*/
   int NI,NJ,NK,NC,NIJ;       /*Dimensions du champs*/
   int Idx;                   /*Index displacement into supergrid*/

   int     CellDim;           /*Defined grid point coverage, point=1 or area=2*/
   char    Container;         /*Container pointant sur d'autres donnees*/
   int     Level;             /*Niveau courant*/
   int     Limits[3][2];      /*Limits of processing in grid points*/
   double  CoordLimits[2][2]; /*Limits of processing in latlon*/
   int     Sample;            /*Sample interval in grid points*/
   TList  *Segments;          /*Liste d'objets de rendue*/
} TDataDef;

void      DataDef_Clear(TDataDef *Def);
int       DataDef_Compat(TDataDef *DefTo,TDataDef *DefFrom);
TDataDef *DataDef_Copy(TDataDef *Def);
TDataDef *DataDef_CopyPromote(TDataDef *Def,TData_Type Type);
void      DataDef_Free(TDataDef *Def);
TDataDef *DataDef_New(int NI,int NJ,int NK,int Dim,TData_Type Type);
TDataDef *DataDef_Resize(TDataDef *Def,int NI,int NJ,int NK);
int       DataDef_Tile(TDataDef *DefTo,TDataDef *DefTile,int X0,int Y0);

#endif
