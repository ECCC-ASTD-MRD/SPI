/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Package de gestion des palette de couleur.
 * Fichier   : tclCMap.h
 * Creation  : Aout 98 - J.P. Gauthier - CMC/CMOE - 421 4642
 *
 * Description: Creation de palette de couleur et gestion de celles ci.
 *
 * Remarques :
 *   -Base en partie sur le code de Vanh Souvanlasy (Juin 1994)
 *
 * License   :
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

#ifndef _tclCMap_h
#define _tclCMap_h

#include <tcl.h>

#define CR_MAX    256    /*Nombre de colorcell a allouer en RGBA*/

typedef struct CMap_Rec {
   char         *Name;
   char          Type[4][16];
   int           Interp;
   int           InvertX[4],InvertY[4];
   int           Ratio[4],RatioMin,RatioMax,Alpha;
   unsigned char Table[CR_MAX][4];
   unsigned char Control[CR_MAX][4];
   unsigned char Color[CR_MAX][4];
   float         Curve[CR_MAX][4];
   int           NbPixels;
   double        Min[4],Max[4];
} CMap_Rec;

CMap_Rec* CMap_Get(char *Name);
CMap_Rec* CMap_New(char* Name,int Nb);
int       CMap_Put(Tcl_Interp *Interp,CMap_Rec *Map);

int CMap_GetImage(Tcl_Interp *Interp,CMap_Rec *CMap,char* Img);
int CMap_Create(Tcl_Interp *Interp,char *Name);
int CMap_ColorList(Tcl_Interp *Interp,CMap_Rec *CMap,int Comp,int Mode);
int CMap_GetColorString(Tcl_Interp *Interp,CMap_Rec *CMap,int Index);
int CMap_Read(Tcl_Interp *Interp,CMap_Rec *CMap,char *RGBFile);
int CMap_Write(Tcl_Interp *Interp,CMap_Rec *CMap,char *RGBFile);
int CMap_PostscriptColor(Tcl_Interp *Interp,CMap_Rec *CMap,int Index);
int CMap_Free(char *Name);

void CMap_ControlDefine(CMap_Rec *CMap);
void CMap_RatioDefine(CMap_Rec *CMap);
void CMap_CurveDefine(CMap_Rec *CMap);

int  CMap_SelImage(Tcl_Interp *Interp,char Mode,char *Img,double Val);
void CMap_HSV2RGB(double *R,double *G,double *B,double H,double S,double V);
void CMap_RGB2HSV(double R,double G,double B,double *H,double *S,double *V);
#endif
