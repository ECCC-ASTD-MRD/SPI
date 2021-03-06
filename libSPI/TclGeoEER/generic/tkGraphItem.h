/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Creation de graph dans le canvas Tk.
 * Fichier      : tkGraphIem.h
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * Description  : Fichier d'entete du module d'Item.
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
 *=========================================================
 */

#ifndef _tkGraphItem_h
#define _tkGraphItem_h

#include "tkglCanvas.h"
#include "tkCanvGraph.h"
#include "tclData.h"
#include "tclVector.h"
#include "GeoData.h"

enum GraphType { NONE,LINE,SPLINE,BAR,WIDEBAR,HISTOGRAM,RASTER,BOXPLOT,MINMAX };

typedef struct TGraphItem {
   Tcl_Interp    *Interp;
   Tk_Font        Font;                       /*Font for drawing text*/
   Tk_Dash        Dash;                       /*pointille*/
   Tk_TextLayout  Text;                       /*Cached text layout information*/
   Tk_Anchor      Anchor;                     /*Ancrage des valeurs*/
   XColor        *Outline;                    /*Couleur d'arriere plan*/
   XColor        *Fill;                       /*Couleur d'avant plan*/
   XColor        *IconFill;                   /*Couleur d'avant plan des icones*/
   XColor        *IconOutline;                /*Couleur d'arriere plan des icones*/
   double         IconXFillValue;             /*Valeur de remplissage dans l'axe X*/
   double         IconXShowValue;             /*Valeur d'affichage de l'icone dans l'axe X*/
   char          *ImageString;
   T_glBitmap    *Stipple;                    /*Bitmap pour le stipple de mode fill*/
   T_glBitmap    *Bitmap;                     /*Bitmap pour les points*/
   Tcl_Obj       *Colors;                     /*Liset de couleurs*/

   double         Origin;                     /*Origine du graph selon l'axe et l'orientation*/
   int            Width;                      /*line width*/
   double         Size;                       /*Icon Size*/
   char          *Orient;                     /*Orientation (X,Y,Z,XY,YZ,XZ)*/
   char          *Fit;                        /*Fitting type*/
   int            Avg;                        /*Disaply average line*/
   int            Icon;                       /*Icone*/
   int            Value;                      /*Affichage des valeurs*/
   int            Alpha;                      /*Transparence */
   int            Type;                       /*Type de graph (NONE,LINE,SPLINE,BAR,HISTOGRAM,RASTER)*/
   Tk_Item       *DescItem;
   char          *Desc;                       /*Description de la donnee*/
   int            DescWidth,DescHeight;
   char          *XData,*YData,*ZData,*Data;  /*Donnee*/
   char          *XAxis,*YAxis,*ZAxis,*WAxis; /*Axes*/
   char          *Tag;                        /*Identificateur informatif*/

   char          *Speed,*Dir;
   char          *ErrorData,*MedianData,*HighData,*LowData,*MinData,*MaxData; /*BoxPlot data*/

} TGraphItem;

int TclGraphItem_Init(Tcl_Interp *Interp);

TGraphItem* GraphItem_Get(char *Name);

#endif
