/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : GeoRef_RPN.c
 * Creation     : Mars 2005 - J.P. Gauthier
 *
 * Description  : Fonctions de manipulations de projections aux standard RPN.
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
#include "tclData.h"

void     GeoRef_Expand(TGeoRef *Ref);
double   GeoRef_RPNDistance(TGeoRef *Ref,double X0,double Y0,double X1, double Y1);
int      GeoRef_RPNValue(TGeoRef *Ref,TDataDef *Def,char Mode,int C,double X,double Y,double Z,float *Length,float *ThetaXY);
int      GeoRef_RPNProject(TGeoRef *Ref,double X,double Y,double *Lat,double *Lon,int Extrap,int Transform);
int      GeoRef_RPNUnProject(TGeoRef *Ref,double *X,double *Y,double Lat,double Lon,int Extrap,int Transform);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_Expand>
 * Creation     : Mars 2005 J.P. Gauthier - CMC/CMOE
 *
 * But          : Effectuer l'expansion des axes de la grilles selon les >> ^^.
 *
 * Parametres    :
 *   <Ref>       : Pointeur sur la reference geographique
 *
 * Retour       :
 *
 * Remarques   :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GeoRef_Expand(TGeoRef *Ref) {

   if (Ref->Id>=0 && !Ref->AX && Ref->Grid[0]=='Z') {
      Ref->AX=(float*)calloc((int)Ref->X1+1,sizeof(float));
      Ref->AY=(float*)calloc((int)Ref->Y1+1,sizeof(float));
      EZLock_RPNInt();
      c_gdgaxes(Ref->Id,Ref->AX,Ref->AY);
      EZUnLock_RPNInt();
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_RPNDistance>
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
 *---------------------------------------------------------------------------------------------------------------
*/
double GeoRef_RPNDistance(TGeoRef *Ref,double X0,double Y0,double X1, double Y1) {

   float i[2],j[2],lat[2],lon[2];

   i[0]=X0+1.0;
   j[0]=Y0+1.0;
   i[1]=X1+1.0;
   j[1]=Y1+1.0;

   EZLock_RPNInt();
   c_gdllfxy(Ref->Id,&lat,&lon,&i,&j,2);
   EZUnLock_RPNInt();

   X0=DEG2RAD(lon[0]);
   X1=DEG2RAD(lon[1]);
   Y0=DEG2RAD(lat[0]);
   Y1=DEG2RAD(lat[1]);

   return(DIST(0.0,Y0,X0,Y1,X1));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_RPNValue>
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
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoRef_RPNValue(TGeoRef *Ref,TDataDef *Def,char Mode,int C,double X,double Y,double Z,float *Length,float *ThetaXY){

   Vect3d   b,v;
   float    x,y;
   void     *p0,*p1;
   int      valid=0,mem,ix,iy,n;

   *Length=Def->NoData;

   /*In case of triangle meshe*/
   if (Ref->Grid[0]=='M') {
      if (C<Def->NC && X>=0 && Y>=0) {
         b[0]=X-(int)X;
         b[1]=Y-(int)Y;
         b[2]=1.0-b[0]-b[1];
         ix=(int)X;

         if (Mode=='N') {
            n=(b[0]>b[1]?(b[0]>b[2]?0:2):(b[1]>b[2]?1:2));
            Def_Get(Def,C,Ref->Idx[ix+n],v[0]);
            *Length=v[0];
         } else {
            Def_Get(Def,C,Ref->Idx[ix],v[0]);
            Def_Get(Def,C,Ref->Idx[ix+1],v[1]);
            Def_Get(Def,C,Ref->Idx[ix+2],v[2]);

            *Length=Bary_Interp1D(b,v);
         }
         return(1);
      } else {
         return(0);
      }
   }

   /*Si on est a l'interieur de la grille ou que l'extrapolation est activee*/
   if (C<Def->NC && X>=(Ref->X0-0.5) && Y>=(Ref->Y0-0.5) && Z>=0 && X<=(Ref->X1+0.5) && Y<=(Ref->Y1+0.5) && Z<=Def->NK-1) {
      valid=1;

      /*Index memoire du niveau desire*/
      mem=Def->NI*Def->NJ*(int)Z;

      /*Pour un champs vectoriel*/
      x=X+1.0;
      y=Y+1.0;

      if (Ref && Ref->Grid[0]=='V') {
         ix=ROUND(X);
         iy=ROUND(Y);

         if (Def->Data[1]) {
            Def_GetMod(Def,FIDX2D(Def,ix,iy),*Length);
         } else {
            *Length=VertexVal(Ref,Def,-1,X,Y,0.0);
         }
         return(valid);
      }

      if (Ref->Grid[0]!='Y' && Ref->Grid[0]!='P' && Def->Data[1] && !C) {
         if (Ref && Ref->Id>-1) {
            Def_Pointer(Def,0,mem,p0);
            Def_Pointer(Def,1,mem,p1);
            EZLock_RPNInt();
            c_gdxywdval(Ref->Id,Length,ThetaXY,p0,p1,&x,&y,1);
            EZUnLock_RPNInt();
         }
      } else {
         ix=ROUND(X);
         iy=ROUND(Y);

         if (Ref->Grid[0]=='Y' || Ref->Grid[0]=='P') {
            X=ix;
            Y=iy;
         }

         if (Def->Type<=9 || Mode=='N' || (X==ix && Y==iy)) {
            mem+=iy*Def->NI+ix;
            Def_Get(Def,C,mem,*Length);
            if (Def->Data[1] && !C)
               Def_Get(Def,1,mem,*ThetaXY);
         } else {
            if (Ref && Ref->Id>-1) {
               Def_Pointer(Def,C,mem,p0);
               EZLock_RPNInt();
               c_gdxysval(Ref->Id,Length,p0,&x,&y,1);
               EZUnLock_RPNInt();
            } else {
               *Length=VertexVal(Ref,Def,C,X,Y,Z);
            }
         }
      }
   }
   return(valid);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_RPNProject>
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
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoRef_RPNProject(TGeoRef *Ref,double X,double Y,double *Lat,double *Lon,int Extrap,int Transform) {

   float i,j,lat,lon;
   int   idx;

/*
      if (Ref->Grid[0]=='P' || Ref->Grid[0]=='Y' || Ref->Grid[0]=='M') {
      *Lon=X;
      *Lat=Y;
      return(1);
   }
*/

   /*Verifier si la grille est valide et que l'on est dans la grille*/
   if (Ref->Id<0 || X<(Ref->X0-0.5) || Y<(Ref->Y0-0.5) || X>(Ref->X1+0.5) || Y>(Ref->Y1+0.5)) {
      if (!Extrap || Ref->Id<0) {
         *Lat=-999.0;
         *Lon=-999.0;
         return(0);
      }
   }

   if (Ref->Type&GRID_SPARSE) {
      if (Ref->Lon && Ref->Lat) {
         idx=Y*(Ref->X1-Ref->X0)+X;
         *Lat=Ref->Lat[idx];
         *Lon=Ref->Lon[idx];
         return(1);
      } else {
         return(0);
      }
   }

   i=X+1.0;
   j=Y+1.0;
   EZLock_RPNInt();
   c_gdllfxy(Ref->Id,&lat,&lon,&i,&j,1);
   EZUnLock_RPNInt();
   *Lat=lat;
   *Lon=lon>180?lon-=360:lon;

   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_RPNUnProject>
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
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoRef_RPNUnProject(TGeoRef *Ref,double *X,double *Y,double Lat,double Lon,int Extrap,int Transform) {

   float  i,j,lat,lon,d=1e32,dx=1.0;
   int    n,di,dj,idx;
   Vect3d b;

   *X=-1.0;
   *Y=-1.0;

   if (Ref->Type&GRID_SPARSE) {
      if (Ref->Lon && Ref->Lat) {
         if (Ref->Grid[0]=='M') {
            for(n=0;n<Ref->NIdx-3;n+=3) {
               if (Bary_Get(b,Lon,Lat,Ref->Lon[Ref->Idx[n]],Ref->Lat[Ref->Idx[n]],
                  Ref->Lon[Ref->Idx[n+1]],Ref->Lat[Ref->Idx[n+1]],Ref->Lon[Ref->Idx[n+2]],Ref->Lat[Ref->Idx[n+2]])) {

                  *X=n+b[0];
                  *Y=n+b[1];
                  return(1);
               }
            }
            return(0);
         } else {
            for(dj=0;dj<=(Ref->Y1-Ref->Y0);dj++) {
               for(di=0;di<=(Ref->X1-Ref->X0);di++) {

                  idx=dj*(Ref->X1-Ref->X0)+di;
                  dx=hypot(fabs(Lon-Ref->Lon[idx]),fabs(Lat-Ref->Lat[idx]));

                  if (dx<0.1 && dx<d) {
                     *X=di;*Y=dj;d=dx;
                  }
               }
            }

            if (d<1.0) {
               return(1);
            } else {
               return(0);
            }
         }
      } else {
         return(0);
      }
   }

   if (Lat<=90.0 && Lat>=-90.0 && Lon!=-999.0 && Ref->Id>-1) {

      lon=Lon<0?Lon+360:Lon;
      lat=Lat;

      /*Extraire la valeur du point de grille*/
      EZLock_RPNInt();
      c_gdxyfll(Ref->Id,&i,&j,&lat,&lon,1);
      EZUnLock_RPNInt();

      *X=i-1.0;
      *Y=j-1.0;

      /*Si on est a l'interieur de la grille*/
      if (*X>(Ref->X1+0.5) || *Y>(Ref->Y1+0.5) || *X<(Ref->X0-0.5) || *Y<(Ref->Y0-0.5)) {
         if (!Extrap) {
            *X=-1.0;
            *Y=-1.0;
         }
         return(0);
      }
   } else {
     *X=-1.0;
     *Y=-1.0;
     return(0);
   }

   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_RPNSetup>
 * Creation     : Avril 2005 J.P. Gauthier - CMC/CMOE
 *
 * But          : Definir le referetiel de type RPN
 *
 * Parametres   :
 *    <NI>      : Dimension en X
 *    <NJ>      : Dimension en Y
 *    <NK>      : Diemnsion en Z
 *    <Type>    : Type de niveaux
 *    <Levels>  : Liste des niveaux
 *    <GRTYP>   : Type de grille
 *    <IG1>     : Descripteur IG1
 *    <IG2>     : Descripteur IG2
 *    <IG3>     : Descripteur IG3
 *    <IG4>     : Descripteur IG4
 *    <FID>     : Identificateur du fichier
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
TGeoRef* GeoRef_RPNSetup(int NI,int NJ,int NK,int Type,float *Levels,char *GRTYP,int IG1,int IG2,int IG3,int IG4,int FID) {

   TGeoRef *ref;

   ref=GeoRef_New();
   GeoRef_Size(ref,0,0,0,NI-1,NJ-1,NK-1,0);

   if ((NI>1 || NJ>1) && GRTYP[0]!='X' && GRTYP[0]!='P' && GRTYP[0]!='M' && GRTYP[0]!='V' && ((GRTYP[0]!='Z' && GRTYP[0]!='Y') || FID!=-1)) {
      EZLock_RPNInt();
      ref->Id=c_ezqkdef(NI,NJ,GRTYP,IG1,IG2,IG3,IG4,FID);
      EZUnLock_RPNInt();
   } else {
      ref->Id=-1;
   }

   ref->IG1=IG1;
   ref->IG2=IG2;
   ref->IG3=IG3;
   ref->IG4=IG4;
   ref->ZRef.Type=Type;
   ref->ZRef.LevelNb=NK;
   ref->ZRef.Levels=(float*)calloc(ref->ZRef.LevelNb+1,sizeof(float));
   if (Levels)
      memcpy(ref->ZRef.Levels,Levels,ref->ZRef.LevelNb*sizeof(float));

   ref->Grid[0]=GRTYP[0];
   ref->Project=GeoRef_RPNProject;
   ref->UnProject=GeoRef_RPNUnProject;
   ref->Value=GeoRef_RPNValue;
   ref->Distance=GeoRef_RPNDistance;
   ref->Height=NULL;

   return(GeoRef_Find(ref));
}
