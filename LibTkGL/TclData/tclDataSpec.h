/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de divers fichiers de donnees
 * Fichier      : tclDataSpec.h
 * Creation     : Mai 2006 - J.P. Gauthier
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

#ifndef _tclConfigSpec_h
#define _tclConfigSpec_h

#include <tk.h>
#include <tcl.h>
#include "tclCMap.h"
#include "ogr_api.h"
#include "Vector.h"
#include "eerStruct.h"
#include "glStuff.h"

#define DATASPEC_MAX 256
#define DATASPEC_NOTSET 0x00
#define DATASPEC_MINSET 0x01
#define DATASPEC_MAXSET 0x02
#define DATASPEC_ALLSET 0x03

#define VECTORSIZE(SPEC,VAL) ((SPEC->RenderVector==ARROW?SPEC->Size+SPEC->Size*SPEC->SizeRange*(SPEC->InterNb?((VAL-SPEC->Inter[0])/(SPEC->Inter[SPEC->InterNb-1]-SPEC->Inter[0])):((VAL-SPEC->Min)/(SPEC->Max-SPEC->Min))):SPEC->Size+2*SPEC->Width))
#define SPEC2VAL(SPEC,VAL)    (SPEC?VAL/SPEC->ValFactor-SPEC->ValDelta:VAL)
#define VAL2SPEC(SPEC,VAL)    (SPEC?(VAL+SPEC->ValDelta)*SPEC->ValFactor:VAL)

#define COL2VAL(IDX,SPEC,VAL) {\
   if (SPEC->InterNb>0){\
      VAL=SPEC->Inter[(int)(IDX/SPEC->MapFactor)];\
   } else {\
      VAL=IDX/SPEC->MapFactor+SPEC->Min;\
   }\
}

#define VAL2COL(IDX,SPEC,VAL) {\
   int l;\
   IDX=SPEC->MapBellow?0:-2;\
   if (SPEC->InterNb>0) {\
      for(l=0;l<SPEC->InterNb;l++) { \
         if (VAL>=SPEC->Inter[l]) { \
            IDX=(l+1)*SPEC->MapFactor; \
         } else { \
            break; \
         } \
      } \
      if (VAL>SPEC->Inter[SPEC->InterNb-1] && !SPEC->MapAbove) IDX=-2;\
   } else if ((VAL<=SPEC->Max || SPEC->MapAbove) && (VAL>=SPEC->Min || SPEC->MapBellow)) {\
      IDX=(VAL-SPEC->Min)*SPEC->MapFactor;\
      IDX=IDX<0?0:IDX;\
      IDX=IDX>SPEC->Map->NbPixels-1?SPEC->Map->NbPixels-1:IDX;\
   }\
}

typedef enum { VNONE,BARBULE,ARROW,STREAMLINE,STREAMLINE3D } TDataSpecVECTOR;

typedef struct TIcon {
   int     Type;
   int     Nb;
   double  Co[DATASPEC_MAX];
} TIcon;

typedef struct TDataSpec {
   char       *Name;                /*Identificateur information*/
   int         Set;                 /*Flag de configuration*/
   int         NRef;                /*Compteur de reference*/

   int             Active;              /*Flag d'activation de l'affichage*/
   CMap_Rec       *Map;                 /*Palette Associee*/
   XColor         *Outline,*HighLine;   /*Couleur des segments*/
   XColor         *Fill,*HighFill;      /*Couleur de remplissage*/
   Tk_Font         Font;                /*Font for drawing text*/
   Tk_FontMetrics  TKM;                 /*Font Metrics*/
   Tk_Dash         Dash;                /*Pointille*/
   T_glBitmap     *Stipple;             /*Stipple bitmap for filling item*/
   char           *Sprite;              /*Sprite image*/
   Tk_PhotoHandle SpriteImg;
   char           *Desc;                /*Descripteur des donnees*/
   char           *Unit;                /*Unite des donnees*/
   int             ZType;               /*Type de coordonnees verticale*/
   char           *Topo;                /*Modulateur 3D*/
   double          TopoFactor;          /*Facteur du modulateur 3D*/
   char           *Extrude;             /*Modulateur 3D*/
   double          ExtrudeFactor;       /*Facteur du modulateur 3D*/
   int             Interp;              /*Interpolation GL*/
   float           Inter[DATASPEC_MAX]; /*Intervalles de contours*/
   Tcl_Obj        *InterLabels;         /*Libelle des intervalles*/
   Tcl_Obj        *InterVals;           /*Intervalles*/
   Tcl_Obj        *OGRMask;             /*Masque vectoriel*/
   int             InterNb;             /*Nombre d'intervalles de contour*/
   int             InterMode;           /*Mode de calcul des intervalles de contour*/
   double          InterModeParam;      /*Parameter du mode de calcul des intervalles de contours*/
   int             InterO;              /*Ordre de grandeur du max des intervalles*/
   int             InterM;              /*Ordre de grandeur de la mantisse*/
   int             Alpha;               /*Transparence globale*/
   int             Light;               /*Illumination*/
   int             Sample;              /*Distance de sampling des valeurs*/
   char            SampleType;          /*Type de sampling des valeurs*/
   int             TexSample,TexSize,TexRes; /*Parametres des textures*/
   double          TexStep;
   double          Step;                /*Step de calcul*/
   int             Width;               /*Largeur*/
   int             Icon;                /*Icone*/
   int             Mark;                /*Marquage*/
   int             Style;               /*Style*/
   char           *LabelVar;            /*Variable pour les libelles*/
   char           *SizeVar;             /*Variable pour les dimensions*/
   double          Size,SizeRange,SizeMin,SizeMax;/*Dimension*/
   char           *MapVar;              /*Variable pour appliquer la palette*/
   double          Min,Max;             /*Minimum et Maximum des valeurs*/
   int             MinMax;              /*Indicateur de selection des min max*/
   double          ValFactor;           /*Facteur multiplicatif d'ajustement des valeurs*/
   double          ValDelta;            /*Facteur delta d'ajustement des valeurs*/
   double          MapFactor;           /*Facteur d'index dans la palette*/
   int             MapAll;              /*Applique la palette aux vectoriel*/
   int             MapAbove,MapBellow;  /*Applique la palette en dehors des limites min max*/
   int             GridVector;          /*Orientation des donnees vectorielles*/
   int             WMO;                 /*WMO plotting type*/

   int             RangeNb;             /*Nombre de range*/
   float           Range[DATASPEC_MAX]; /*Liste des ranges*/

   int             Cube[6];             /*Cube de selection*/
   char            Axis;                /*Axe*/

   char*           InterpDegree;    /*Degree d'interpolation*/
   char*           ExtrapDegree;    /*Degree d'extrapolation*/
   int             RenderTexture;   /*Effectuer le rendue de la texture du champs*/
   int             RenderFace;      /*Effectuer le rendue des faces*/
   int             RenderGrid;      /*Effectuer le rendue de la grille*/
   int             RenderCoord;     /*Effectuer le rendue des coordonnees*/
   int             RenderContour;   /*Effectuer le rendue des contours*/
   int             RenderLabel;     /*Effectuer le rendue des labels des niveaux*/
   int             RenderParticle;  /*Effectuer le rendue de champs lagrangien*/
   TDataSpecVECTOR RenderVector;    /*Effectuer le rendue de champs vectoriel*/
   int             RenderValue;     /*Effectuer le rendue des valeurs*/
   int             RenderVol;       /*Effectuer le rendue du volume*/

   Vect3d          Pos[DATASPEC_MAX];
   int             PosNb;
} TDataSpec;

TDataSpec* DataSpec_Create(Tcl_Interp *Interp,char *Name);
TDataSpec* DataSpec_New();
TDataSpec* DataSpec_Get(char *Name);
int        DataSpec_Config(Tcl_Interp *Interp,TDataSpec *Spec,int Objc,Tcl_Obj *CONST Objv[]);
int        DataSpec_Copy(Tcl_Interp *Interp,char *To,char *From);
void       DataSpec_Clean(TDataSpec *Spec,int Map,int Pos,int Seg);
void       DataSpec_Clear(TDataSpec *Spec);
int        DataSpec_Free(TDataSpec *Spec);
int        DataSpec_FreeHash(Tcl_Interp *Interp,char *Name);
void       DataSpec_Define(TDataSpec *Spec);
void       DataSpec_Wipe();
void       DataSpec_Format(TDataSpec *Spec,double Val,char *Str);
void       DataSpec_Intervals(TDataSpec *Spec,double Min,double Max);

TIcon* Icon_Parse(Tcl_Interp *Interp,Tcl_Obj *List);
void   Icon_Free(TIcon *Icon);

#endif
