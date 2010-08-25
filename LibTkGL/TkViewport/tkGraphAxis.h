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
 *=========================================================
 */

#ifndef _tkGraphAxis_h
#define _tkGraphAxis_h

#include "tkglCanvas.h"
#include "tkCanvGraph.h"
#include "tclData.h"
#include "tclVector.h"

#define AXISVALUE(SC,VAL) (SC->Type=='N'?(VAL>0.0?SC->Offset[0]+(log(VAL)-SC->T0)*SC->Delta:0.0):(SC->Type=='O'?(VAL>0.0?SC->Offset[0]+(log10(VAL)-SC->T0)*SC->Delta:0.0):SC->Offset[0]+(VAL-SC->T0)*SC->Delta))
#define AXISPPOS(SC,POS)  (SC->Type=='N'?exp((POS-SC->Offset[0])/SC->Delta+SC->T0):(SC->Type=='O'?pow(10,(POS-SC->Offset[0])/SC->Delta+SC->T0):(POS-SC->Offset[0])/SC->Delta+SC->T0))

#define HORIZONTAL 0x01
#define VERTICAL   0x02
#define NOTDONE    0x00
#define DONEX      0x01
#define DONEY      0x02

#define  FOOT    0.3048
#define  T_MIN    -140.0
#define  T_MAX    150.0
#define  TH_MIN   200
#define  TH_MAX   500
#define  P_MAX    1200.0
#define  P_MIN    0.0
#define  MSNGV   -2.0e6          /*missing value*/
#define  AZ      273.16          /* absolute zero */
#define  g_0   9.81     /* standard gravity */
#define  R_d   287.0    /* gas constant for dry air */
#define  R_v   462.0    /* gas constant for water vapor */
#define  cp_d  1005.0      /* specific heats for dry air */
#define  cv_d  718.0
#define  L_v   2.5e6    /* latent heat of vaporization */
#define  L_s   2.834e6     /* latent heat of sublimation */
#define  p_trop  226.32
#define  h_trop  11000.236
#define  c_trop  44332.3
#define  c_strat  6341.1
#define  t_cut   -40.0
#define  Omega 7.292e-5 /* absolute angular speed of Earth */
//#define  COSA  0.751806
//#define  SINA  0.659385

#define  A  45
#define  COSA  cos(DEG2RAD(A))
#define  SINA  sin(DEG2RAD(A))

typedef struct TGraphAxis {
   Tk_Dash        Dash;          /*Pointille*/
   Tk_Font        Font;          /*Font for drawing text*/
   Tk_TextLayout  Text;          /*Cached text layout information*/
   Tk_Justify     Justify;
   Tk_Anchor      Anchor;
   char         **Label;         /*Liste des labels corespondant aux intervals*/
   double        *Inter;         /*Liste des intervals*/
   int            InterNb;       /*Nombre d'interval*/
   int            Offset[2];     /*Decallage superieur en %*/
   double         Min,Max;       /*Minimum et maximum de l'axe*/
   double         T0,T1;         /*Minimum et maximum de l'axe*/
   double         DT0,DT1;       /*Minimum et maximum de l'axe en coordonnees du graph*/
   double         Angle;         /*Text angle*/
   double         Incr;          /*Increment*/
   double         Delta;         /*Delta de deplacement en pixel par unite d'increment*/
   int            Numbered;      /*Numerotation de l'axe*/
   int            Spacing;       /*Espacement des labels*/
   int            Format;        /*Format de l'axe*/
   int            Mod;           /*Forcer les limites modulo l'increment*/

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
   double        *Grid;          /*Valeurs ou mettre la grille*/
   int            GridNb;
   XColor        *GridColor;     /*Couleur de la grille*/
   int            GridWidth;     /*Largeur de la grille*/
   double        *HighLight;     /*Valeurs a mettre en evidence*/
   int            HighLightNb;
   XColor        *HighLightColor;/*Couleur de la grille*/
   int            HighLightWidth;/*Largeur de la grille*/
} TGraphAxis;

int TclGraphAxis_Init(Tcl_Interp *Interp);

TGraphAxis* GraphAxis_Get(char *Name);

void   GraphAxis_Clear(TGraphAxis *Axis);
void   GraphAxis_Define(TGraphAxis *Axis,TVector *Vec,int Delta);
double GraphAxis_Incr(TGraphAxis *Axis);
int    GraphAxis_Displacement(TGraphAxis *Axis,Tk_Font Font,Tk_FontMetrics TKM,int Side,char *String,int *DX,int *DY);
void   GraphAxis_Print(TGraphAxis *Axis,char *String,double Value,int DOrder);
void   GraphAxis_Wipe();
#endif
