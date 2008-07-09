/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Creation de graph dans le canvas Tk.
 * Fichier   : tkCanvGraph.h
 * Creation  : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Affichage et manipulation de graph dans le canvas.
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

#ifndef _tkCanvGraph_h
#define _tkCanvGraph_h

#include <malloc.h>
#include <stdio.h>
#include <string.h>

#include "tkglCanvas.h"
#include "tkCanvColorbar.h"
#include "tkGraphItem.h"
#include "tkGraphAxis.h"

#define SETRECT(VAR,X0,Y0,X1,Y1)   VAR[0]=X0; VAR[1]=Y0; VAR[2]=X1; VAR[3]=Y0; VAR[4]=X1; VAR[5]=Y1; VAR[6]=X0; VAR[7]=Y1;
#define SETLINE(VAR,X0,Y0,X1,Y1)   VAR[0]=X0; VAR[1]=Y0; VAR[2]=X1; VAR[3]=Y1;

typedef struct GraphItem  {
   Tk_Item        header;          /*Generic stuff that's the same for all types*/
   Tk_Canvas      canvas;          /*Pointeur sur le canvas contenant le viewport*/
   Tk_Anchor      anchor;          /*Where to anchor pixmap relative to (x,y)*/
   Tk_Font        Font;            /*Font for drawing text*/
   Tk_TextLayout  Text;            /*Cached text layout information*/

   double         x,y;             /*Coordinates of positioning point for pixmap*/
   int            Width,Height;    /*Dimensions du viewport*/
   int            xg[2],yg[2];     /*Coordinates of graph inside*/
   int            ax[32],ay[32],ah[32];
   int            xi,yi;           /*Coordinates of item header*/
   int            BDLegend;        /*Bordure de la legende*/
   int            Legend;          /*Affichage de la legende*/
   int            BDWidth;         /*Largeur de la bordure*/
   XColor        *BGColor;         /*Couleur d'arriere plan*/
   XColor        *FGColor;         /*Couleur d'avant plan*/
   XColor        *FillColor;       /*Couleur de remplissage*/

   char          *Command;         /*Nom de la commande de transformation*/

   char          *Type;            /*Type de graph XY,XYZ,PIE,TEPHI,HODO*/
   double         Angle;           /*Graph angle*/

   Tk_Item       *TitleItem;
   char          *Title;
   int            TitleWidth,TitleHeight;

   char         **Item;
   char          *ItemStr;         /*Liste des donnees associees (String)*/
   int            NItem;
   ColorbarItem  *CB;              /*Colorbar*/

   int            ISide,NSide;       /*Nunber of syde by side item*/
   int            Alpha,AlphaLegend; /*Transparency */
   int            Update;            /*Need Update */
   GLubyte       *Frame;             /*Frame de retention*/
} GraphItem;

#endif
