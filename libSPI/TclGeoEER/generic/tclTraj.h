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
#include "Traj.h"

TTraj*   Traj_New();
void     Traj_Free(TTraj *Traj);
TTraj*   Traj_Get(char *Name);
Tcl_Obj* Traj_Put(Tcl_Interp *Interp,char *Name,TTraj *Traj);
int      TclTraj_Init(Tcl_Interp*);
int      Traj_Load(Tcl_Interp *Interp,char *File,TTraj **Traj);
int      Traj_LoadCMC(Tcl_Interp *Interp,FILE *Stream,char *File,TTraj **Traj);
int      Traj_LoadARL(Tcl_Interp *Interp,FILE *Stream,char *File,TTraj **Traj);
void     Traj_Wipe();

#endif
