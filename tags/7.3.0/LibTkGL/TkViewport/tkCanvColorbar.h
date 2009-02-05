/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Projection orthographique de la carte vectorielle.
 * Fichier   : tkCanvColormap.h
 * Creation  : Fevrier 2002
 *
 * Description: Affichage et manipulation de palette.
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

#ifndef _tkCanvColorbar_h
#define _tkCanvColorbar_h

#include "tclData.h"
#include "tkglCanvas.h"

/*The structure below defines the record for each pixmap item.*/

typedef struct ColorbarItem  {
   Tk_Item        header;          /*Generic stuff that's the same for all types*/
   Tk_Canvas      canvas;          /*Pointeur sur le canvas contenant le viewport*/
   Tk_Anchor      anchor;          /*Where to anchor pixmap relative to (x,y)*/
   Tk_Font        Font;            /*Font for drawing text*/
   Tk_FontMetrics tkm;             /*Font metrics*/
   double         x,y;             /*Coordinates of positioning point for pixmap*/
   int            Width,Height;    /*Dimensions du viewport*/
   XColor        *BGColor;         /*Couleur d'arriere plan*/
   XColor        *FGColor;         /*Couleur d'avant plan*/
   char          **Data;           /*Liste des donnees associees*/
   char          *DataStr;         /*Liste des donnees associees (String)*/
   int            NbData;          /*Nombre de donnees associees*/
   int            Alpha;           /*Transparency*/
   int            Id;              /*Identification enable*/
   char          *Orient;          /*Orientation de la couleur*/
   int            BarSplit;        /*Separer les couleurs*/
   int            BarWidth;        /*Largeur de la barre de couleur*/
   int            BarBorder;       /*Largeur de la bordure de la couleur*/
   Tk_Justify     BarSide;         /*Side on which to put the bar*/
} ColorbarItem;

int    ColorbarCreate(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[]);
void   ColorbarDelete(Tk_Canvas Canvas,Tk_Item *Item,Display *Disp);
void   ColorbarDisplay(Tk_Canvas Canvas,Tk_Item *Item,Display *Disp,Drawable Drawt,int X,int Y,int Width,int Height);
void   ColorbarBBox(Tk_Canvas Canvas,ColorbarItem *CB);
int    Colorbar_RenderContour(Tcl_Interp *Interp,ColorbarItem *CB,TDataSpec *Spec,int Y1);
int    Colorbar_RenderId(Tcl_Interp *Interp,ColorbarItem *CB,TDataSpec *Spec,int Y1);
void   Colorbar_RenderTexture(Tcl_Interp *Interp,ColorbarItem *CM,TDataSpec *Spec,int Y1,int Y2);
void   Colorbar_RenderVector(Tcl_Interp *Interp,ColorbarItem *CM,TDataSpec *Spec,int Y2);
void   Colorbar_RenderText(ColorbarItem *CB,int X,int Y,Tk_Justify Side,char *Text,TDataSpec *Spec);
int    ColorbarCoords(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[]);
int    ColorbarToArea(Tk_Canvas Canvas,Tk_Item *Item,double *RectPtr);
double ColorbarToPoint(Tk_Canvas Canvas,Tk_Item *Item,double *CoordPtr);
int    ColorbarToPostscript(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Prepass);
int    ColorbarConfigure(Tcl_Interp *Interp,Tk_Canvas Canvas,Tk_Item *Item,int Argc,Tcl_Obj *CONST Argv[],int Flags);
void   ColorbarScale(Tk_Canvas Canvas,Tk_Item *Item, double OriginX,double OriginY,double ScaleX,double ScaleY);
void   ColorbarTranslate(Tk_Canvas Canvas,Tk_Item *Item,double DeltaX,double DeltaY);
#endif
