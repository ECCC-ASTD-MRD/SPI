/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Creation de graph dans le canvas Tk.
 * Fichier      : tkGraphAxis.h
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * Description  : Fichier d'entete du module d'echelle de graph.
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
 *
 * Modification :
 *
 *   Nom        :
 *   Date       :
 *   Description:
 *
 *=========================================================
 */

#ifndef _tkGraphAxis_h
#define _tkGraphAxis_h

#include "tkglCanvas.h"
#include "tkCanvGraph.h"
#include "tclData.h"
#include "tclVector.h"

#define AXISVALUE(SC,VAL) (SC->Type=='O'?(VAL>0.0?SC->Off+(log10(VAL)-SC->T0)*SC->Delta:0.0):SC->Off+(VAL-SC->T0)*SC->Delta)
#define AXISPPOS(SC,POS)  (SC->Type=='O'?pow(10,(POS-SC->Off)/SC->Delta+SC->T0):(POS-SC->Off)/SC->Delta+SC->T0)

#define HORIZONTAL 0x01
#define VERTICAL   0x02
#define NOTDONE    0x00
#define DONEX      0x01
#define DONEY      0x02

typedef struct TGraphAxis {
   Tk_Dash        Dash;          /*Pointille*/
   Tk_Font        Font;          /*Font for drawing text*/
   Tk_TextLayout  Text;          /*Cached text layout information*/
   Tk_Justify     Justify;
   Tk_Anchor      Anchor;
   char         **Label;         /*Liste des labels corespondant aux intervals*/
   double        *Inter;         /*Liste des intervals*/
   int            InterNb;       /*Nombre d'interval*/
   double         Min,Max;       /*Minimum et maximum de l'axe*/
   double         T0,T1;         /*Minimum et maximum de l'axe*/
   double         Angle;         /*Text angle*/
   double         Offset[2];     /*Decallage superieur en %*/
   double         Off;           /*Decallage su depart*/
   double         Incr;          /*Increment*/
   double         Delta;         /*Delta de deplacement en pixel par unite d'increment*/
   double        *HighLight;     /*Valeurs a mettre en evidence*/
   int            HighLightNb;
   int            Numbered;      /*Numerotation de l'axe*/
   int            All;           /*Verification des overlap de label*/

   int            Order;         /*Ordre de grandeur des valeurs*/
   char           Done;          /*Flag de completion de process*/
   char           Pos[3];        /*Indicateur de point d'attache (LL,UL,LR,UR)*/

   Tk_Item       *UnitItem;
   char          *Unit;          /*Unite de l'echelle*/
   int            UnitWidth,UnitHeight;
   char           Type;          /*Type d'echelle LOGARITHMIC ou LINEAR (O,I)*/
   double         Mark;          /*Affichage des barres d'increments*/
   XColor        *Color;         /*Couleur*/
   int            Width;         /*Largeur de l'echelle*/
   XColor        *GridColor;     /*Couleur de la grille*/
   int            GridWidth;     /*Largeur de la grille*/
   XColor        *HighLightColor;/*Couleur de la grille*/
   int            HighLightWidth;/*Largeur de la grille*/
} TGraphAxis;

int TclGraphAxis_Init(Tcl_Interp *Interp);

TGraphAxis* GraphAxis_Get(char *Name);
#endif
