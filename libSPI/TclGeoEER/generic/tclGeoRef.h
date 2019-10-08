/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : tclGeoRef.h
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

#ifndef _tclGeoRef_h
#define _tclGeoRef_h

#include <tcl.h>

#include "eerUtils.h"
#include "tclUtils.h"
#include "Vector.h"
#include "GeoRef.h"

TGeoRef* GeoRef_Get(char *Name);
Tcl_Obj* GeoRef_Put(Tcl_Interp *Interp,char *Name,TGeoRef *Ref);
int      GeoRef_Destroy(Tcl_Interp *Interp,char *Name);
TGeoRef* GeoRef_Find(TGeoRef *Ref);

int      GeoPos_Free(TGeoPos *GPos);
TGeoPos* GeoPos_Find(TGeoRef *GRef,TZRef *ZRef);
TGeoPos* GeoPos_Copy(TGeoPos *GPos);
#endif
