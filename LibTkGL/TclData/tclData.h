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

#include "eerUtils.h"
#include "tclUtils.h"
#include "GeoRef.h"
#include "tclDataSpec.h"
#include "tclDataDef.h"
#include "tclVector.h"
#include "Vector.h"
#include "glStuff.h"

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

typedef struct TDataStat {
   double Min,Max,Avg;      /*Minimum maximum et moyenne de l'enregistrement*/
   Coord  MinLoc,MaxLoc;    /*Coordonnees des minimums et maximums*/
} TDataStat;

struct TData;

typedef Vect3d* (TData_Grid)     (struct TData *Field,void *Proj,int Level);
typedef void    (TData_Free)     (struct TData *Field);
typedef void    (TData_Copy)     (void *To, void *From);
typedef int     (TData_ReadCube) (Tcl_Interp *Interp,struct TData *Field,int Invert,double LevelFrom,double LevelTo,Tcl_Obj *List);
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

int      Data_FieldCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);

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
int      Data_GridInterpolate(Tcl_Interp *Interp,char Degree,TGeoRef *ToRef,TDataDef *ToDef,TGeoRef *FromRef,TDataDef *FromDef);
int      Data_Stat(Tcl_Interp *Interp,TData *Field,int Objc,Tcl_Obj *CONST Objv[]);

int       Data_DefSort(Tcl_Interp *Interp,Tcl_Obj *List);

#endif
