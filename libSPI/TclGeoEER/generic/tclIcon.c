/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de divers fichiers de donnees
 * Fichier      : tclIcon.c
 * Creation     : Juillet 2015 - J.P. Gauthier
 *
 * Description  : Fonctions generales de configuration d'affichage
 *                applicables a divers types de donnees.
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
#ifdef YOYO

#include <math.h>

#include "tclIcon.h"

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "nanosvg.h"

static Tcl_HashTable TIcon_Table;
static int           TIconInit=0;

static int Icon_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);

CONST char *ICONS[]   = { "NONE","TRIANGLE","SQUARE","VBAR","HBAR","CIRCLE","PENTAGON","HEXAGON","LOZENGE","LIGHTNING","X","+","*","ARROW" };
CONST char *WMOS[]    = { "NONE","AUTO","N","WW","CL","CM","CH","A","UV" };

// If you add icons here, look in GDAL_Band.c for references to this array
TIcon IconList[]={
 { GL_POINT,     1, { 0.0,0.0 } },
 { GL_POLYGON,   3, { -1.0,-1.0, 0.0,1.0, 1.0,-1.0 } },
 { GL_POLYGON,   4, { -1.0,-1.0, -1.0,1.0, 1.0,1.0, 1.0,-1.0 } },
 { GL_POLYGON,   4, { -0.5,-1.0, -0.5,1.0, 0.5,1.0, 0.5,-1.0 } },
 { GL_POLYGON,   4, { -1.0,-0.5, -1.0,0.5, 1.0,0.5, 1.0,-0.5 } },
 { GL_POLYGON,   12,{ 1.0,0.0, 0.87,0.50, 0.50,0.87, 0.0,1.0, -0.50,0.87, -0.87,0.50, -1.0,0.0, -0.87,-0.50, -0.50,-0.87, -0.00,-1.00, 0.50,-0.87, 0.87,-0.50 } },
 { GL_POLYGON,   5, { -0.59,-0.81, 0.59,-0.81, 0.95,0.31, 0.00,1.00, -0.95,0.31 } },
 { GL_POLYGON,   6, { -0.50,-0.87, 0.50,-0.87, 1.0,0.0, 0.5,0.87, -0.5,0.87, -1.0,0.0 } },
 { GL_POLYGON,   4, { -1.0,0.0, 0.0,1.0, 1.0,0.0, 0.0,-1.0 } },
 { GL_TRIANGLES, 12, { 1.0,1.0, -0.6,0.4, -0.2,0.0, 0.0,0.2, -0.2,0.0, 0.2,0.0, 0.2,0.0, 0.0,-0.2, -0.2,0.0,  0.6,-0.4, -1.0,-1.0, 0.2,0.0 } },
 { GL_LINES,     4, { -1.0,-1.0, 1.0,1.0, -1.0,1.0, 1.0,-1.0 } },
 { GL_LINES,     4, { -1.0,0.0, 1.0,0.0, 0.0,-1.0, 0.0,1.0 } },
 { GL_LINES,     8, { -1.0,0.0, 1.0,0.0, 0.0,-1.0, 0.0,1.0, -1.0,-1.0, 1.0,1.0, -1.0,1.0, 1.0,-1.0 } },
 { GL_POLYGON,   7, { 0.0,0.0, 0.25,-0.5, 0.1,-0.5, 0.1,-1.0, -0.1,-1.0, -0.1,-0.5, -0.25,-0.5 } } };

typedef TIcon { 
   NSVGimage *svg;
   int Width,Height;
   char *Img;
   
} TIcon;

int Icon_ReadSVG(TIcon *Icon,char *Filename) {

   Icon->svg=nsvgParseFromFile(Filename,"px",96.0f);
   
   return(TCL_OK);
}

int Icon_Display(TIcon *Icon,) {

   nsvgRasterize(rast, image, 0,0,1, img, w, h, w*4);
   
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclIcon_Init>
 * Creation     : Juillet 2015 J.P. Gauthier
 *
 * But          : Initialisation des commandes Tcl pour utilisation des obseravtions
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *
 * Retour       : Code de retour Tcl
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
 */
int TclIcon_Init(Tcl_Interp *Interp) {

   if (!TIconInit++) {
      Tcl_InitHashTable(&TIcon_Table,TCL_STRING_KEYS);
   }
   Tcl_CreateObjCommand(Interp,"icon",Icon_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);
   
   if (!(rast=nsvgCreateRasterizer())) {
      Tcl_AppendResult(Interp,"TclIcon_Init: Could not create rasterizer",(char*)NULL);
      return(TCL_ERROR);  
   }

   return(TCL_OK);
}
#endif