/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Projection diverses de la carte vectorielle.
 * Fichier   : ProjCam.h
 * Creation  : Janvier 2000 - J.P. Gauthier
 *
 * Description: Fichier de definition des parametres des cameras.
 *
 * Remarques :
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

#ifndef _ProjCam_h
#define _ProjCam_h

#include <stdlib.h>
#include <string.h>

#include <tcl.h>
#include <tk.h>
#include "Vector.h"
#include "GeoRef.h"

typedef struct ProjCam {
   struct ProjCam *Controls;
   Vect3d          From,To,Up,Basis;
   Coord           Focal;
   int             NbC;
   int             Show;
   int             Update;
   double          Lens,Dist,A,Aspect,Clip,Frame,Pix;
   double          FOV[2],DOV[2];
} ProjCam;


ProjCam* ProjCam_Get(const char* Name);

int  ProjCam_Init(Tcl_Interp *Interp);
void ProjCam_CircleFrom(ProjCam *Cam,double ThetaXY,double ThetaYZ,double Delta);
void ProjCam_CircleTo(ProjCam *Cam,double ThetaXZ,double ThetaYZ,double Delta);
void ProjCam_ParamsInit(ProjCam *Cam);
void ProjCam_Fly(ProjCam *Cam);
void ProjCam_Place(ProjCam *Cam);

#endif
