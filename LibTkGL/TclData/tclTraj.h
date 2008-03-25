/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers de trajectoires
 * Fichier      : tclTraj.h
 * Creation     : Fevrier 2003 - J.P. Gauthier
 *
 * Description  : Fichier d'entete du module Traj.
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

#ifndef _tclTRAJ_h
#define _tclTRAJ_h

#include "tclData.h"

typedef struct TParcel {
   long   Date;
   Coord  Co;
   float  Sig,Pres,X,Dist,Speed;
} TParcel;

typedef struct TTraj {
   Tcl_Obj      *Tag;
   TDataSpec    *Spec;  /*Specification des donnees*/

   float    Min,Max;
   int      Lapse;
   long     Date;
   int      AP;
   int      Back;
   int      NPr;
   char     Path[256];
   char     Id[32];
   char     Model[16];
   int      Mode;
   char     Type;
   TParcel *Pr;
} TTraj;

void   Traj_Free(TTraj *Traj);
TTraj* Traj_Get(char *Name);
int    TclTraj_Init(Tcl_Interp*);
int    Traj_Load(Tcl_Interp *Interp,char *File,TTraj **Traj);
int    Traj_LoadCMC(Tcl_Interp *Interp,FILE *Stream,char *File,TTraj **Traj);
int    Traj_LoadARL(Tcl_Interp *Interp,FILE *Stream,char *File,TTraj **Traj);
void   Traj_Wipe();

#endif
