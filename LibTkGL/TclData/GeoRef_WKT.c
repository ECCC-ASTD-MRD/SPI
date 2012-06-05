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
 * Description  : Fonctions de manipulations de projections aux standard WKT.
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

int      GeoRef_WKTValue(TGeoRef *Ref,TDataDef *Def,char Mode,int C,double X,double Y,double Z,double *Length,double *ThetaXY);
int      GeoRef_WKTProject(TGeoRef *Ref,double X,double Y,double *Lat,double *Lon,int Extrap,int Transform);
int      GeoRef_WKTUnProject(TGeoRef *Ref,double *X,double *Y,double Lat,double Lon,int Extrap,int Transform);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_WKTDistance>
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
double GeoRef_WKTDistance(TGeoRef *Ref,double X0,double Y0,double X1, double Y1) {

   double i[2],j[2],lat[2],lon[2];

   X0+=Ref->X0;
   X1+=Ref->X0;
   Y0+=Ref->Y0;
   Y1+=Ref->Y0;

   if (Ref->Grid[1]=='Z' || (Ref->Spatial && OSRIsGeographic(Ref->Spatial))) {
      GeoRef_WKTProject(Ref,X0,Y0,&lat[0],&lon[0],1,1);
      GeoRef_WKTProject(Ref,X1,Y1,&lat[1],&lon[1],1,1);
      return(DIST(0.0,DEG2RAD(lat[0]),DEG2RAD(lon[0]),DEG2RAD(lat[1]),DEG2RAD(lon[1])));
   } else {
      if (Ref->Transform) {
         i[0]=Ref->Transform[0]+Ref->Transform[1]*X0+Ref->Transform[2]*Y0;
         j[0]=Ref->Transform[3]+Ref->Transform[4]*X0+Ref->Transform[5]*Y0;
         i[1]=Ref->Transform[0]+Ref->Transform[1]*X1+Ref->Transform[2]*Y1;
         j[1]=Ref->Transform[3]+Ref->Transform[4]*X1+Ref->Transform[5]*Y1;
      } else {
         i[0]=X0;
         j[0]=Y0;
         i[1]=X1;
         j[1]=Y1;
      }
      return(hypot(j[1]-j[0],i[1]-i[0])*OSRGetLinearUnits(Ref->Spatial,NULL));
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_WKTValue>
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
int GeoRef_WKTValue(TGeoRef *Ref,TDataDef *Def,char Mode,int C,double X,double Y,double Z,double *Length,double *ThetaXY){

   double  x,y;
   int     valid=0,mem,ix,iy;

  *Length=Def->NoData;

   /*Si on est a l'interieur de la grille ou que l'extrapolation est activee*/
   if (C<Def->NC && X>=(Ref->X0-0.5) && Y>=(Ref->Y0-0.5) && Z>=0 && X<=(Ref->X1+0.5) && Y<=(Ref->Y1+0.5) && Z<=Def->NK-1) {

      valid=1;
      X-=Ref->X0;
      Y-=Ref->Y0;
      DEFCLAMP(Def,X,Y);

      /*Index memoire du niveau desire*/
      mem=Def->NI*Def->NJ*(int)Z;

      ix=ROUND(X);
      iy=ROUND(Y);

      if (Def->Type<=9 || Mode=='N' || (X==ix && Y==iy)) {
         mem+=iy*Def->NI+ix;
         Def_GetMod(Def,mem,*Length);

         /*Pour un champs vectoriel*/
         if (Def->Data[1]) {
            Def_Get(Def,0,mem,x);
            Def_Get(Def,1,mem,y);
            *ThetaXY=180+RAD2DEG(atan2(x,y));
         }
      } else {
         *Length=VertexVal(Ref,Def,-1,X,Y,Z);
         /*Pour un champs vectoriel*/
         if (Def->Data[1]) {
            x=VertexVal(Ref,Def,0,X,Y,Z);
            y=VertexVal(Ref,Def,1,X,Y,Z);
            *ThetaXY=180+RAD2DEG(atan2(x,y));
         }
      }
   }
   return(valid);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_WKTProject>
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
int GeoRef_WKTProject(TGeoRef *Ref,double X,double Y,double *Lat,double *Lon,int Extrap,int Transform) {

   double dx,dy,x,y,z=0.0;
   int    sx,sy,d,ok;

   if (X>=(Ref->X1+0.5) || Y>=(Ref->Y1+0.5) || X<=(Ref->X0-0.5) || Y<=(Ref->Y0-0.5)) {
      if (!Extrap) {
         *Lon=-999.0;
         *Lat=-999.0;
         return(0);
      }
   }

   /* In case of non-uniform grid, figure out where in the position vector we are */
   if (Ref->Grid[1]=='Z') {
      if (Ref->Lon && Ref->Lat) {
         sx=floor(X);sx=CLAMP(sx,Ref->X0,Ref->X1);
         X=sx==X?Ref->Lon[sx]:ILIN(Ref->Lon[sx],Ref->Lon[sx+1],X-sx);

         d=Ref->X1-Ref->X0+1;
         sy=floor(Y);sy=CLAMP(sy,Ref->Y0,Ref->Y1);
         Y=sy==Y?Ref->Lat[sy*d]:ILIN(Ref->Lat[sy*d],Ref->Lat[(sy+1)*d],Y-sy);
      }
   } else if (Ref->Grid[1]=='X' || Ref->Grid[1]=='Y') {
      if (Ref->Lon && Ref->Lat) {
         sx=floor(X);sx=CLAMP(sx,Ref->X0,Ref->X1);
         sy=floor(Y);sy=CLAMP(sy,Ref->Y0,Ref->Y1);
         dx=X-sx;;
         dy=Y-sy;

         d=sy*(Ref->X1-Ref->X0+1)+sx;
         X=Ref->Lon[d];
         Y=Ref->Lat[d];

         if (++sx<=Ref->X1) {
            d=sy*(Ref->X1-Ref->X0+1)+sx;
            X+=(Ref->Lon[d]-X)*dx;
         }

         if (++sy<=Ref->Y1) {
            d=sy*(Ref->X1-Ref->X0+1)+(sx-1);
            Y+=(Ref->Lat[d]-Y)*dy;
         }
      }
   }

   /* Transform the point into georeferenced coordinates */
   x=X;
   y=Y;
   if (Transform) {
      if (Ref->Transform) {
         x=Ref->Transform[0]+Ref->Transform[1]*X+Ref->Transform[2]*Y;
         y=Ref->Transform[3]+Ref->Transform[4]*X+Ref->Transform[5]*Y;
      } else if (Ref->GCPTransform) {
         GDALGCPTransform(Ref->GCPTransform,FALSE,1,&x,&y,&z,&ok);
      } else if (Ref->TPSTransform) {
         GDALGCPTransform(Ref->TPSTransform,FALSE,1,&x,&y,&z,&ok);
      } else if (Ref->RPCTransform) {
         GDALGCPTransform(Ref->RPCTransform,FALSE,1,&x,&y,&z,&ok);
      }
   }

   /* Transform to latlon */
   if (Ref->Function) {
      if (!OCTTransform(Ref->Function,1,&x,&y,NULL)) {
         *Lon=-999.0;
         *Lat=-999.0;
         return(0);
      }
   }

   *Lon=x;
   *Lat=y;

   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_WKTUnProject>
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
int GeoRef_WKTUnProject(TGeoRef *Ref,double *X,double *Y,double Lat,double Lon,int Extrap,int Transform) {

   double x,y,z=0.0,d=1e32,sd;
   int    s,dx,dy,ok,idx;

   if (Lat<=90.0 && Lat>=-90.0 && Lon!=-999.0) {

      x=Lon;
      y=Lat;

      /*Longitude from 0 to 360*/
      if (Ref->InvTransform && Ref->InvTransform[0]==0) {
         x=x<0?x+360:x;
      }

      /* Transform from latlon */
      if (Ref->InvFunction) {
         if (!OCTTransform(Ref->InvFunction,1,&x,&y,NULL)) {
            *X=-1.0;
            *Y=-1.0;
            return(0);
         }
      }

      /* Transform from georeferenced coordinates */
      *X=x;
      *Y=y;
      if (Transform) {
         if (Ref->InvTransform) {
            *X=Ref->InvTransform[0]+Ref->InvTransform[1]*x+Ref->InvTransform[2]*y;
            *Y=Ref->InvTransform[3]+Ref->InvTransform[4]*x+Ref->InvTransform[5]*y;
         } else if (Ref->GCPTransform) {
            GDALGCPTransform(Ref->GCPTransform,TRUE,1,X,Y,&z,&ok);
         } else if (Ref->TPSTransform) {
            GDALTPSTransform(Ref->TPSTransform,TRUE,1,X,Y,&z,&ok);
         } else if (Ref->RPCTransform) {
            GDALRPCTransform(Ref->RPCTransform,TRUE,1,X,Y,&z,&ok);
         }
      }

      /* In case of non-uniform grid, figure out where in the position vector we are */
      if (Ref->Grid[1]=='Z') {
         if (Ref->Lon && Ref->Lat) {
            s=Ref->X0;
            /*Check if vector is increasing*/
            if (Ref->Lon[s]<Ref->Lon[s+1]) {
               while(s<=Ref->X1 && *X>Ref->Lon[s]) s++;
            } else {
               while(s<=Ref->X1 && *X<Ref->Lon[s]) s++;
            }
            if (s>Ref->X0) {
               /*We're in so interpolate postion*/
               if (s<=Ref->X1) {
                  *X=(*X-Ref->Lon[s-1])/(Ref->Lon[s]-Ref->Lon[s-1])+s-1;
               } else {
                  *X=(*X-Ref->Lon[Ref->X1])/(Ref->Lon[Ref->X1]-Ref->Lon[Ref->X1-1])+s-1;
               }
            } else {
               /*We're out so extrapolate position*/
               *X=(*X-Ref->Lon[0])/(Ref->Lon[1]-Ref->Lon[0])+s;
            }

            s=Ref->Y0;dx=Ref->X1-Ref->X0+1;
            /*Check if vector is increasing*/
            if (Ref->Lat[s*dx]<Ref->Lat[(s+1)*dx]) {
               while(s<=Ref->Y1 && *Y>Ref->Lat[s*dx]) s++;
            } else {
               while(s<=Ref->Y1 && *Y<Ref->Lat[s*dx]) s++;
            }
            if (s>Ref->Y0) {
               /*We're in so interpolate postion*/
               if (s<=Ref->Y1) {
                  *Y=(*Y-Ref->Lat[(s-1)*dx])/(Ref->Lat[s*dx]-Ref->Lat[(s-1)*dx])+s-1;
               } else {
                  *Y=(*Y-Ref->Lat[Ref->Y1*dx])/(Ref->Lat[Ref->Y1*dx]-Ref->Lat[(Ref->Y1-1)*dx])+s-1;
               }
            } else {
               /*We're out so extrapolate position*/
               *Y=(*Y-Ref->Lat[0])/(Ref->Lat[dx]-Ref->Lat[0])+s;
            }
         }
      }

      if (Ref->Grid[1]=='X' || Ref->Grid[1]=='Y') {
         if (Ref->Lon && Ref->Lat) {
            for(dy=0;dy<=(Ref->Y1-Ref->Y0);dy++) {
               for(dx=0;dx<=(Ref->X1-Ref->X0);dx++) {

                  idx=dy*(Ref->X1-Ref->X0+1)+dx;
                  sd=fabs(*X-Ref->Lon[idx])+fabs(*Y-Ref->Lat[idx]);

                  if (sd<d) {
                     x=dx;y=dy;d=sd;
                  }
               }
            }
            *X=x;
            *Y=y;
         }
      }

      /*Check the grid limits*/
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
   }
   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoRef_WKTSetup>
 * Creation     : Juin 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Definir les fonctions de transformations WKT
 *
 * Parametres   :
 *   <Ref>      : Pointeur sur la reference geographique
 *   <String>   : Description de la projection
 *   <Geometry> : Geometrie d'ou extraire la reference spatiale (optionel=NULL)
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoRef_WKTSet(TGeoRef *Ref,char *String,double *Transform,double *InvTransform,OGRSpatialReferenceH Spatial) {

   OGRSpatialReferenceH llref=NULL;
   char                *string=NULL;

   if (String)
      string=strdup(String);

   GeoRef_Clear(Ref,0);

   if (Transform) {
      if (!Ref->Transform)
         Ref->Transform=(double*)calloc(6,sizeof(double));
      memcpy(Ref->Transform,Transform,6*sizeof(double));
   } else {
      if (Ref->Transform) {
         free(Ref->Transform);
         Ref->Transform=NULL;
      }
   }

   if (InvTransform) {
      if (!Ref->InvTransform)
         Ref->InvTransform=(double*)calloc(6,sizeof(double));
      memcpy(Ref->InvTransform,InvTransform,6*sizeof(double));
   } else {
     if (Ref->InvTransform) {
         free(Ref->InvTransform);
         Ref->InvTransform=NULL;
      }
   }

   if (Spatial) {
      Ref->Spatial=OSRClone(Spatial);
      OSRExportToWkt(Ref->Spatial,&string);
   } else if (string) {
      Ref->Spatial=OSRNewSpatialReference(NULL);
      if (OSRSetFromUserInput(Ref->Spatial,string)==OGRERR_FAILURE) {
        fprintf(stdout,"(WARNING) GeoRef_WKTSet: Unable to create spatial reference.\n");
        return(0);
      }
   } else {
      string=strdup(REFDEFAULT);
      Ref->Spatial=OSRNewSpatialReference(string);
      fprintf(stdout,"(WARNING) GeoRef_WKTSet: Unable to find spatial reference, assuming default (latlon)\n");
   }

   if (Ref->String)
      free(Ref->String);
   Ref->String=string;

   if (Ref->Spatial) {
      llref=OSRCloneGeogCS(Ref->Spatial);

      if (llref) {
         Ref->Function=OCTNewCoordinateTransformation(Ref->Spatial,llref);
         Ref->InvFunction=OCTNewCoordinateTransformation(llref,Ref->Spatial);
         OSRDestroySpatialReference(llref);
      }
   } else {
      fprintf(stdout,"(WARNING) GeoRef_WKTSet: Unable to get spatial reference\n");
      return(0);
   }

   Ref->Project=GeoRef_WKTProject;
   Ref->UnProject=GeoRef_WKTUnProject;
   Ref->Value=GeoRef_WKTValue;
   Ref->Distance=GeoRef_WKTDistance;
   Ref->Height=NULL;

   return(1);
}

TGeoRef *GeoRef_WKTSetup(int NI,int NJ,int NK,int Type,float *Levels,char *GRTYP,int IG1,int IG2,int IG3,int IG4,char *String,double *Transform,double *InvTransform,OGRSpatialReferenceH Spatial) {

   TGeoRef *ref;

   ref=GeoRef_New();
   GeoRef_Size(ref,0,0,0,NI>0?NI-1:0,NJ>0?NJ-1:0,NK>0?NK-1:0,0);
   GeoRef_WKTSet(ref,String,Transform,InvTransform,Spatial);

   ref->Grid[1]=ref->Grid[2]='\0';
   if (GRTYP) {
      ref->Grid[0]=GRTYP[0];
      ref->Grid[1]=GRTYP[1];
   } else {
      ref->Grid[0]='W';
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

   return(GeoRef_Find(ref));
}
