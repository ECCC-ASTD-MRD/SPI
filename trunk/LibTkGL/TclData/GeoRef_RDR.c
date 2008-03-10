/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : GeoRef_RDR.c
 * Creation     : Mars 2005 - J.P. Gauthier
 *
 * Description  : Fonctions de manipulations de projections aux standard RADAR
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
 * Modification :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *
 *=========================================================
 */
#include "tclData.h"

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_RDRDistance>
 * Creation     : Mars 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Calculer la distance entre deux points.
 *
 * Parametres    :
 *   <Ref>       : Pointeur sur la reference geographique
 *   <X0>        : coordonnee en X dans la projection/grille
 *   <Y0>        : coordonnee en Y dans la projection/grille
 *   <X0>        : coordonnee en X dans la projection/grille
 *   <Y0>        : coordonnee en Y dans la projection/grille
 *
 * Retour       : Distance
 *
 * Remarques   :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
double GeoRef_RDRDistance(TGeoRef *Ref,double X0,double Y0,double X1, double Y1) {

   return(0.0);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_RDRValue>
 * Creation     : Mars 2005 J.P. Gauthier - CMC/CMOE
 *
 * But          : Extraire la valeur d'une matrice de donnees.
 *
 * Parametres    :
 *   <Ref>       : Pointeur sur la reference geographique
 *   <Def>       : Pointeur sur la definition de la donnee
 *   <Mode>      : Mode d'interpolation (N=NEAREST,L=LINEAR);
 *   <C>         : Composante
 *   <X>         : coordonnee en X dans la projection/grille
 *   <Y>         : coordonnee en Y dans la projection/grille
 *   <Z>         : coordonnee en Z dans la projection/grille
 *   <Length>    : Module interpolee
 *   <ThetaXY>   : Direction interpolee
 *
 * Retour       : Inside (1 si a l'interieur du domaine).
 *
 * Remarques   :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoRef_RDRValue(TGeoRef *Ref,TDataDef *Def,char Mode,int C,double Azimuth,double Dist,double Sweep,float *Length,float *ThetaXY){

   float    x,y,z;
   int      valid=0,mem,ix,iy;

   *Length=Def->NoData;

   /*Si on est a l'interieur de la grille ou que l'extrapolation est activee*/
   if (C<Def->NC && Dist<=Ref->R) {

      x=Azimuth/Ref->ResA;
      y=Dist/Ref->ResR;
      z=Sweep;

      mem=Def->NI*Def->NJ*(int)Sweep;

      ix=ROUND(x);
      iy=ROUND(y);

      if (Def->Type<=9 || Mode=='N' || (x==ix && y==iy)) {
         mem+=iy*Def->NI+ix;
         Def_Get(Def,C,mem,*Length);
      } else {
         *Length=VertexValN(Ref,Def,C,x,y,z);
      }
      valid=1;
   }
   return(valid);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_RDRProject>
 * Creation     : Mars 2005 J.P. Gauthier - CMC/CMOE
 *
 * But          : Projeter une coordonnee de projection en latlon.
 *
 * Parametres    :
 *   <Ref>       : Pointeur sur la reference geographique
 *   <X>         : coordonnee en X dans la projection/grille
 *   <Y>         : coordonnee en Y dans la projection/grille
 *   <Lat>       : Latitude
 *   <Lon>       : Longitude
 *   <Extrap>    : Extrapolation hors grille
 *   <Transform> : Appliquer la transformation
 *
 * Retour       : Inside (1 si a l'interieur du domaine).
 *
 * Remarques   :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoRef_RDRProject(TGeoRef *Ref,double X,double Y,double *Lat,double *Lon,int Extrap,int Transform) {

   Coord loc0;
   double x,d;

   if (Y>Ref->R && !Extrap) {
      *Lat=-999.0;
      *Lon=-999.0;
      return(0);
   }

   loc0.lat=DEG2RAD(Ref->Loc.lat);
   loc0.lon=DEG2RAD(Ref->Loc.lon);
   x=DEG2RAD(X);
   d=M2RAD(Y);

   *Lat=asin(sin(loc0.lat)*cos(d)+cos(loc0.lat)*sin(d)*cos(x));
   *Lon=fmod(loc0.lon+(atan2(sin(x)*sin(d)*cos(loc0.lat),cos(d)-sin(loc0.lat)*sin(*Lat)))+M_PI,M_2PI)-M_PI;
   *Lat=RAD2DEG(*Lat);
   *Lon=RAD2DEG(*Lon);

   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_RDRUnProject>
 * Creation     : Mars 2005 J.P. Gauthier - CMC/CMOE
 *
 * But          : Projeter une latlon en position grille.
 *
 * Parametres    :
 *   <Ref>       : Pointeur sur la reference geographique
 *   <X>         : coordonnee en X dans la projection/grille
 *   <Y>         : coordonnee en Y dans la projection/grille
 *   <Lat>       : Latitude
 *   <Lon>       : Longitude
 *   <Extrap>    : Extrapolation hors grille
 *   <Transform> : Appliquer la transformation
 *
 * Retour       : Inside (1 si a l'interieur du domaine).
 *
 * Remarques   :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoRef_RDRUnProject(TGeoRef *Ref,double *X,double *Y,double Lat,double Lon,int Extrap,int Transform) {

   Coord loc0;
   double x,d;

   loc0.lat=DEG2RAD(Ref->Loc.lat);
   loc0.lon=DEG2RAD(Ref->Loc.lon);
   Lat=DEG2RAD(Lat);
   Lon=DEG2RAD(Lon);

   d=fabs(DIST(0.0,loc0.lat,loc0.lon,Lat,Lon));
   x=-RAD2DEG(COURSE(loc0.lat,loc0.lon,Lat,Lon));
   *X=x<0?x+360:x;
   *Y=d;

   if (d>Ref->R)
      return(0);

   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_RDRSetup>
 * Creation     : Avril 2006 J.P. Gauthier - CMC/CMOE
 *
 * But          : Definir le referetiel de type Radar
 *
 * Parametres   :
 *    <Lat>     : Latitude du centre
 *    <Lon>     : Longitude du centre
 *    <Height>  : Altitude du centre
 *    <Radius>  : Rayon
 *    <ResR>    : Resolution en distance
 *    <ResA>    : Resolution en azimuth
 *
 * Retour       :
 *
 * Remarques    :
 *
 * Modification   :
 *   Nom        :
 *   Date       :
 *   Description:
 *---------------------------------------------------------------------------------------------------------------
*/
TGeoRef* GeoRef_RDRSetup(double Lat,double Lon,double Height,double Radius,double ResR,double ResA,int NTheta,float *Theta) {

   TGeoRef *ref;

   ref=GeoRef_New();

   ref->Grid[0]='R';
   ref->Loc.lat=Lat;
   ref->Loc.lon=Lon;
   ref->Loc.elev=Height;
   ref->R=Radius;
   ref->ResR=ResR;
   ref->ResA=ResA;

   ref->LevelType=LVL_THETA;
   ref->LevelNb=NTheta;
   ref->Levels=(float*)calloc(ref->LevelNb+1,sizeof(float));
   if (Theta)
      memcpy(ref->Levels,Theta,ref->LevelNb*sizeof(float));

   ref->Project=GeoRef_RDRProject;
   ref->UnProject=GeoRef_RDRUnProject;
   ref->Value=GeoRef_RDRValue;
   ref->Distance=GeoRef_RDRDistance;

   return(GeoRef_Find(ref));
}
