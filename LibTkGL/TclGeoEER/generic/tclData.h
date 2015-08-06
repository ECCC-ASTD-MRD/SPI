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
#include "tclDataSpec.h"
#include "GeoRef.h"
#include "Def.h"
#include "tclVector.h"
#include "Vector.h"
#include "Array.h"
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
   TD_RPN,
   TD_GRIB,
   TD_RADAR
} TDataType;

typedef struct TDataStat {
   int   *Histo,HistoBin;   // Histogramme des valeurs
   double Min,Max,Avg;      // Minimum maximum et moyenne de l'enregistrement
   Coord  MinLoc,MaxLoc;    // Coordonnees des minimums et maximums
} TDataStat;

typedef struct TDataVector {
   char  *UU,*VV,*WW;       // Vector component names
   double WWFactor;         // Factor to apply to third component
} TDataVector;

struct TData;

typedef Vect3d* (TData_Grid)     (struct TData *Field,void *Proj,int Level);
typedef void    (TData_Free)     (struct TData *Field);
typedef void    (TData_Copy)     (void *To, void *From);
typedef int     (TData_ReadCube) (Tcl_Interp *Interp,struct TData *Field,int Invert,double LevelFrom,double LevelTo,Tcl_Obj *List);
typedef void    (TData_Set)      (struct TData *Field);
typedef int     (TData_Define)   (Tcl_Interp *Interp,struct TData *Field,int Objc,Tcl_Obj *CONST Objv[]);

typedef struct TData {
   Tcl_Obj      *Tag;
   void         *Head;       // Entete de l'enregistrement (metadata)

   TGeoPos      *GPos;       // Reference des points de grilles
   TGeoRef      *GRef;       // Reference geographique horizontale
   TZRef        *ZRef;       // Reference geographique verticale

   TDef         *Def,**SDef; // Definition des donnees
   TDataSpec    *Spec;       // Specification des donnees (pour l'affichage)
   TDataStat    *Stat;       // Statistiques de l'enregistrement
   TDataType    Type;        // Type de donnee

   TData_Set      *Set;      // Fonction d'initialisation
   TData_Free     *Free;     // Fonction de liberation
   TData_Copy     *Copy;     // Fonction de copie de champs
   TData_ReadCube *ReadCube; // Fonction de lecture du cube de donnees (niveaux verticaux)
   TData_Grid     *Grid;     // Fonction de recuperation de la grille (geo-localisation)
   TData_Define   *Define;   // Fonction de recuperation/definition des parametres
   
   float  *Map;              // Texture du champs
} TData;

#include "Vertex.h"

int      Data_FieldCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[],TDataType Type);

void     Data_FromString(char *String,TDef *Def,int Comp,int Idx);
Tcl_Obj *Data_Val2Obj(TDef *Def,double Val);

void     Data_Clean(TData *Data,int Map,int Pos,int Seg);
void     Data_CleanAll(TDataSpec *Spec,int Map,int Pos,int Seg);
int      Data_Free(TData *Field);
int      Data_FreeHash(Tcl_Interp *Interp,char *Name);
TData*   Data_Copy(Tcl_Interp *Interp,TData *Field,char *Name,int Def,int Alias);
int      Data_Cut(Tcl_Interp *Interp,TData **Field,char *Cut,double *Lat,double *Lon,int NbF,int NbC,int Comp);
TData*   Data_Get(char *Name);
TData*   Data_GetShell(Tcl_Interp *Interp,char *Name);
int      Data_Sort(Tcl_Interp *Interp,Tcl_Obj *List);
Tcl_Obj* Data_HighLow(Tcl_Interp *Interp,TData *Field,int High,int Tile);
TData*   Data_Valid(Tcl_Interp *Interp,char *Name,int NI,int NJ,int NK,int Dim,TDef_Type Type);
Tcl_Obj* Data_AppendValueObj(Tcl_Interp *Interp,TDef *Def,int X,int Y);
int      Data_ValSet(TData *Field,double I,double J,double Val);
void     Data_ValGetMatrix(Tcl_Interp *Interp,TData *Field,int Component,int Flip);
int      Data_ValPutMatrix(Tcl_Interp *Interp,TData *Field,int Component,Tcl_Obj *List);
int      Data_Within(TData *Field,float Val);
int      Data_WithinNb(TData *Field);
void     Data_Wipe();
void     Data_PreInit(TData *Data);
int      Data_Stat(Tcl_Interp *Interp,TData *Field,int Objc,Tcl_Obj *CONST Objv[]);
int      Data_GetAreaValue(Tcl_Interp *Interp,int Mode,TData *Field,int Objc,Tcl_Obj *CONST Objv[]);

void     Data_GetStat(TData *Field);
void     Data_StatFree(TDataStat *Stat);

TDataVector *Data_VectorTableCheck(char *Var,int *Idx);
TDataVector *Data_VectorTableAdd(void);

int       Data_DefSort(Tcl_Interp *Interp,Tcl_Obj *List);

#endif
