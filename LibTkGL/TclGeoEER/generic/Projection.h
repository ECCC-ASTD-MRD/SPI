/*===============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Projection diverses de la carte vectorielle.
 * Fichier   : Projection.h
 * Creation  : Janvier 2000 - J.P. Gauthier
 *
 * Description: Fichier d'entete du module Projection.
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

#ifndef _Projection_h
#define _Projection_h

#include "tkCanvVP.h"
#include "Vector.h"
#include "GeoData.h"

#define PROJPLANE  0
#define PROJCYLIN  1
#define PROJGLOBE  2
#define PROJSPHERE 3

#define MAXGEOSEG   2000                              /*Nombre maximal de segment par boite*/

#define ZM(P,H)     (1.0+H*P->Scale*P->ZFactor) /*Projection de l'elevation*/

#define CYLCHECK(D,V)             (V=(D>2.0f)?V-4.0f:(D<-2.0f)?V+4.0f:V)
#define PROJCHECK(P,V)            if (P->Type->Def==PROJCYLIN) { CYLCHECK((V-P->L),V); }
#define CYLFLIP(D,V)              ((V-D)<-2.0f?4:((V-D)>2.0f?-4:0))

typedef struct Projection {
   struct ProjectionType *Type;       /*Type de projection*/
   GDB_Data              *Geo;        /*Donnees geographiques*/
   Tcl_Obj               *Data;       /*Liste des donnees associees*/
   char                  *License;    /*Licenses des donnees associees*/
   int                   NbData;      /*Nombre d'image*/
   double                PixDist;     /*Distance en metres entre deux pixels*/
   long                  Date,Late;   /*Date en secondes*/
   Coord                 SunPos;      /*Position du soleil*/
   Vect4f                LightPos;    /*Position du soleil dans l'espace GL*/
   Vect3d                Nr;          /*Normale*/
   int                   Sun;         /*Activation du soleil*/
   int                   Draw;        /*Affichage de la geo*/
   int                   Loading;     /*Indicateur de lecture en arriere plan*/
   int                   MinSize;     /*Dimension minimale des features a afficher*/
   float                 L,LI,LJ,TLI; /*Longueur des axes*/
   double                Scale;       /*Facteur d'ajustement de l'elevation*/
   int                   TAxis;       /*Axis Type*/
   Coord                 ZAxis;       /*Activation de l'echelle 3D*/
   double                ZFactor;     /*Facteur d'application de l'elevation*/
   Vect3d                ZPos;        /*Position centrale*/
   double                Lat,Lon,I,J; /*Coordonnees de positionement*/
   double                SLat,CLat;   /*Sin et Cos des coordonnees en radians*/
   ViewportItem         *VP;          /*Definitions des parametres d'affichages du Viewport*/
   TGeoRef              *Ref;         /*GeoReference des donnees projection grille*/
   int                   Geographic;  /*Indicateur de projection geographique*/
   int                   Perspective; /*Affichage en perspective*/
} Projection;

typedef int           (Projection_CallLocate)       (Projection *Proj,double Lat,double Lon,int Undo);
typedef void          (Projection_CallRender)       (Projection *Proj,GLuint List,Vect3d *Data,unsigned int *Idx,char *Col,float* Tex,int Mode,int Nb,int Stride,Vect3d V0,Vect3d V1);
typedef void          (Projection_CallDraw)         (Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
typedef int           (Projection_CallUnProject)    (ViewportItem *VP,Projection *Proj,Coord *Loc,Vect3d Pix);
typedef unsigned long (Projection_CallProject)      (const Projection* restrict const Proj,GeoVect *Loc,GeoVect *Pix,long Nb);

typedef Tcl_Obj* (Projection_CallProjectPoint) (Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord Pt1,int Any);
typedef Tcl_Obj* (Projection_CallProjectLine)  (Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord *Co,int NbCo);

typedef struct ProjectionType {
   int                         Def;
   char                        *Name;
   Projection_CallLocate       *Locate;
   Projection_CallRender       *Render;
   Projection_CallDraw         *DrawFirst,*DrawLast,*DrawGlobe;
   Projection_CallUnProject    *UnProject;
   Projection_CallProject      *Project;
   Projection_CallProjectPoint *ProjectPoint;
   Projection_CallProjectLine  *ProjectLine;
} ProjectionType;

Projection*     Projection_Get(char *Name);
ProjectionType* Projection_GetType(char *Type);
void            Projection_Setup(ViewportItem *VP,Projection *Proj,int GL);
void            Projection_Set(ViewportItem *VP,Projection *Proj);

void Projection_Clean(Tcl_Interp *Interp,Projection *Proj,int Mode);
void Projection_Clip(Projection *Proj);
void Projection_UnClip(Projection *Proj);
int  Projection_Init(Tcl_Interp *Interp);
int  Projection_Pixel(Projection *Proj,ViewportItem *VP,Coord Co,Vect3d Pix);
int  Projection_Render(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,int Mode);
int  Projection_Transform(Tcl_Interp *Interp,char* Name,ViewportItem *VP,int argc,char **argv);
int  Projection_Map(Tcl_Interp *Interp,Coord *Pos,char Type,Tcl_Obj *List);
Tcl_Obj* Projection_Path(Tcl_Interp *Interp,Tcl_Obj *List,double Dist);
Tcl_Obj* Grid_Path(Tcl_Interp *Interp,Projection *Proj,Tcl_Obj *List,double Dist);

int Projection_CreateType(Tcl_Interp *Interp,
          char                        *Name
         ,int                         Def
         ,Projection_CallLocate       *Locate
         ,Projection_CallRender       *Render
         ,Projection_CallDraw         *DrawFirst
         ,Projection_CallDraw         *DrawLast
         ,Projection_CallDraw         *DrawGlobe
         ,Projection_CallUnProject    *UnProject
         ,Projection_CallProject      *Project
         ,Projection_CallProjectPoint *ProjectPoint
         ,Projection_CallProjectLine  *ProjectLine);


#define DELTA(P0,P1)         ((P1[1]-P0[1])/(P1[0]-P0[0]))
#define LIANGCOPY(X,Y,PT,NB) PT[NB][0]=X;PT[NB][1]=Y;NB=NB+1;

int LiangBarsky_LineClip2D(Vect3d Pt1,Vect3d Pt2,int *C1,int *C2,double X0,double Y0,double X1,double Y1);
int LiangBarsky_LineClipT(double Denom,double Num,double *Te,double *Tl);
int LiangBarsky_PolygonClip2D(Vect3d *Pt,int Nb,Vect3d *OutPt,int *OutNb,double X0,double Y0,double X1,double Y1);

#endif

