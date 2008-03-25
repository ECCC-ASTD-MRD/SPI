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

#include <malloc.h>
#include <string.h>

#include <tcl.h>
#include <tk.h>
#include "tkglCanvas.h"
#include "Vector.h"

typedef struct ProjCam {
   Vect3d  From,To,Up,Basis;
   Vect3d *CFrom,*CTo,*CUp;
   int     NbC;
   int     Show;
   int     Update;
   double  Lens,Dist,A,Aspect,Clip,Frame,Pix;
} ProjCam;


ProjCam* ProjCam_Get(char* Name);
int ProjCam_Init(Tcl_Interp *Interp);

#endif
