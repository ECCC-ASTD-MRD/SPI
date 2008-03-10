/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers d'observations
 * Fichier      : tclObs.h
 * Creation     : Fevrier 2003 - J.P. Gauthier
 *
 * Description  : Fichier d'entete du module Obs.
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

#ifndef _tclObs_h
#define _tclObs_h

#include "tclData.h"

#define OBSVALID(V)  (V!=-979.0f && V!=-980.0f && V!=-999.0f)

typedef struct TLoc {
   int    Ref;        /*Nombre de reference a la structure*/
   int    Nb;         /*Nombre de localisations*/
   int    NbInfo;     /*Nombre d'informations de l'entete, non temporelles*/
   char   **Id;       /*Identificateurs des localisations*/
   char   **No;       /*Numero des localisations*/
   char   **Head;     /*Token d'entete non temporel (x NbInfo) */
   char   ***Info;    /*Information descriptive (x NbInfo) */
   Coord  *Coord;     /*Position (lat,lon,elevation)*/
} TLoc;

typedef struct TObs {
   Tcl_Obj   *Tag;
   int        Date;
   int        Time;
   int        Min,Max;
   int        LevelType;

   TLoc      *Loc;
   TDataSpec *Spec;       /*Specification des donnees*/
   TDataDef  *Def;        /*Definition des donnees*/
} TObs;

int   TclObs_Init(Tcl_Interp*);
TObs* Obs_Get(char *Name);

TObs*   Obs_Copy(Tcl_Interp *Interp,TObs *Obs,char *Name,int Def);
int     Obs_Extract(Tcl_Interp *Interp,TObs *Obs,TData *Field);
void    Obs_Free(TObs *Obs);
Vect3d *Obs_Grid(TGeoRef *Ref,TObs *Obs,int *NObs,int Extrap);
void    Obs_GetStat(TObs *Obs);
int     Obs_LoadASCII(Tcl_Interp* Interp,char* File,char *Token);
int     Obs_WriteASCII(Tcl_Interp *Interp,char *File,Tcl_Obj *List,char *Title);
int     Obs_LocFree(TLoc *Loc);
int     Obs_Union(Tcl_Interp *Interp,Tcl_Obj *List,char *Token);
int     Obs_Intersection(Tcl_Interp *Interp,Tcl_Obj *List,char *Token);
int     Obs_LocFind(TLoc *Loc,char *Value,char **Array);
void    Obs_Wipe();
void    Obs_PreInit(TObs *Obs);

#endif
