/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de fichiers standards.
 * Fichier   : Vertex.c
 * Creation  : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Fonctions de manipulations et de traitements des vertex.
 *
 * Remarques :
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

#include "tclData.h"
#include "Vertex.h"

int Bary_Get(Vect3d B,double X,double Y,double X0,double Y0,double X1,double Y1,double X2,double Y2) {

   double b,x0,x1,x2,y0,y1,y2;

   x0=X0-X; y0=Y0-Y;
   x1=X1-X; y1=Y1-Y;
   x2=X2-X; y2=Y2-Y;

   b=1.0/((X1-X0)*(Y2-Y0)-(X2-X0)*(Y1-Y0));
   B[0]=(x1*y2-x2*y1)*b;
   B[1]=(x2*y0-x0*y2)*b;
   B[2]=(x0*y1-x1*y0)*b;

   return(B[0]>=0 && B[1]>=0 && B[2]>=0);
}

int Bary_Interp(Vect3d B,Vect3d P,Vect3d P0,Vect3d P1,Vect3d P2) {

   P[0]=B[0]*P0[0]+B[1]*P1[0]+B[2]*P2[0];
   P[1]=B[0]*P0[1]+B[1]*P1[1]+B[2]*P2[1];
   P[2]=B[0]*P0[2]+B[1]*P1[2]+B[2]*P2[2];

   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <VertexGradient>
 * Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Obtenir le gradient d'un point tridimentionne
 *            a l'interieur d'un voxel
 *
 * Parametres :
 *   <Ref>    : Georeference
 *   <Def>    : Definitions des donnees
 *   <Nr>     : Point a l'interieur du voxel
 *
 * Retour:
 *   <Nr>     : Gradient normalise
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void VertexGradient(TGeoRef *Ref,TDataDef *Def,Vect3d Nr) {

   Vect3d v;

   Vect_Assign(v,Nr);
   Nr[0]=VertexVal(Ref,Def,v[0]-0.5,v[1],v[2])-VertexVal(Ref,Def,v[0]+0.5,v[1],v[2]);
   Nr[1]=VertexVal(Ref,Def,v[0],v[1]-0.5,v[2])-VertexVal(Ref,Def,v[0],v[1]+0.5,v[2]);
   Nr[2]=VertexVal(Ref,Def,v[0],v[1],v[2]-0.5)-VertexVal(Ref,Def,v[0],v[1],v[2]+0.5);

//   Vect_Mul(Nr,Nr,v);
}

/*----------------------------------------------------------------------------
 * Nom      : <VertexInterp>
 * Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Interpoler lineairement la position de coupe d'une isosurface
 *            sur un cote entre deux vertex
 *
 * Parametres :
 *   <Pi>     : Point resultant
 *   <P0>     : Point 1
 *   <P1>     : Point 2
 *   <V0>     : Valeur au point P0
 *   <V1>     : Valeur au point P1
 *   <Level>  : Niveau
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void VertexInterp(Vect3d Pi,Vect3d P0,Vect3d P1,double V0,double V1,double Level) {

   double mu;

   if (ABS(Level-V0) < TINY_VALUE) {
      Vect_Assign(Pi,P0);
      return;
   }

   if (ABS(Level-V1) < TINY_VALUE) {
      Vect_Assign(Pi,P1);
      return;
   }
   if (ABS(V0-V1) < TINY_VALUE) {
      Vect_Assign(Pi,P0);
      return;
   }

   mu = (Level-V0)/(V1-V0);

   Pi[0]=P0[0] + mu*(P1[0] - P0[0]);
   Pi[1]=P0[1] + mu*(P1[1] - P0[1]);
   Pi[2]=P0[2] + mu*(P1[2] - P0[2]);
}

/*----------------------------------------------------------------------------
 * Nom      : <VertexQuad_Linear>
 * Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage d'un quad en le subdivisant de maniere recursive.
 *
 * Parametres     :
 *  <Field>       : Champs
 *  <P0,P1,P2,P3> : Localisation du quad
 *  <C0,C1,C2,C3> : Couleurs du quad
 *  <V0,V1,V2,V3> : Valeurs du quad
 *  <Depth>       : Profondeur de l'appel recursif
 *
 * Retour:
 *
 * Remarques :
 *    -Les appels recursifs arrete quand la profondeur maximale est atteinte ou
 *     que le quad n'a plus besion d'etre divise (difference de couleur entre les vertex
 *     minime)
 *
 *----------------------------------------------------------------------------
*/

void VertexQuad_Linear(TData *Field,Vect3d P0,Vect3d P1,Vect3d P2,Vect3d P3,int C0,int C1,int C2,int C3,double V0,double V1,double V2,double V3,int Depth,int Base){

   int    c,c0,c1,c2,c3;
   float  v,v0,v1,v2,v3;
   Vect3d p,p0,p1,p2,p3;

   /*Interpolate the localisations*/
   Vect_Interp(p0,P0,P1,0.5);
   Vect_Interp(p1,P1,P2,0.5);
   Vect_Interp(p2,P2,P3,0.5);
   Vect_Interp(p3,P3,P0,0.5);
   Vect_Interp(p,p0,p2,0.5);

   /*Interpolate the values*/
/*
   v0=ILIN(V0,V1,0.5);
   v1=ILIN(V1,V2,0.5);
   v2=ILIN(V2,V3,0.5);
   v3=ILIN(V3,V0,0.5);
   v=ILIN(v0,v2,0.5);
*/
   v=(V0+V1+V2+V3)*0.25;
   v0=(V0+V1)*0.5;
   v1=(V1+V2)*0.5;
   v2=(V2+V3)*0.5;
   v3=(V3+V0)*0.5;

   /*Interpolate the indexes*/
   VAL2COL(c ,Field->Spec,v);
   VAL2COL(c0,Field->Spec,v0);
   VAL2COL(c1,Field->Spec,v1);
   VAL2COL(c2,Field->Spec,v2);
   VAL2COL(c3,Field->Spec,v3);

//   Depth>>=1;
   Depth--;

   /*Draw if visible*/
   if (C0>-1 || c0>-1 || c>-1 || c3>-1) {
      if (Depth && (ABS(C0-c0)>1 || ABS(c0-c)>1 || ABS(c-c3)>1 || ABS(c3-C0)>1)) {
         VertexQuad_Linear(Field,P0,p0,p,p3,C0,c0,c,c3,V0,v0,v,v3,Depth,Base);
      } else {
         VR(P0,C0,Base);
         VR(p0,c0,Base);
         VR(p,c,Base);
         VR(p3,c3,Base);
      }
   }

   if (c0>-1 || C1>-1 || c1>-1 || c>-1) {
      if (Depth && (ABS(c0-C1)>1 || ABS(C1-c1)>1 || ABS(C1-c)>1 || ABS(c-c0)>1)) {
         VertexQuad_Linear(Field,p0,P1,p1,p,c0,C1,c1,c,v0,V1,v1,v,Depth,Base);
      } else {
         VR(p0,c0,Base);
         VR(P1,C1,Base);
         VR(p1,c1,Base);
         VR(p,c,Base);
      }
   }

   if (c>-1 || c1>-1 || C2>-1 || c2>-1) {
      if (Depth && (ABS(c-c1)>1 || ABS(c1-C2)>1 || ABS(C2-c2)>1 || ABS(c2-c)>1)) {
         VertexQuad_Linear(Field,p,p1,P2,p2,c,c1,C2,c2,v,v1,V2,v2,Depth,Base);
      } else {
         VR(p,c,Base);
         VR(p1,c1,Base);
         VR(P2,C2,Base);
         VR(p2,c2,Base);
      }
   }

   if (c3>-1 || c>-1 || c2>-1 || C3>-1) {
      if (Depth && (ABS(c3-c)>1 || ABS(c-c2)>1 || ABS(c2-C3)>1 || ABS(C3-c3)>1)) {
         VertexQuad_Linear(Field,p3,p,p2,P3,c3,c,c2,C3,v3,v,v2,V3,Depth,Base);
      } else {
         VR(p3,c3,Base);
         VR(p,c,Base);
         VR(p2,c2,Base);
         VR(P3,C3,Base);
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <VertexQuad_Nearest>
 * Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage d'un quad en sans interpolation.
 *
 * Parametres     :
 *  <Field>       : Champs
 *  <P0,P1,P2,P3> : Localisation du quad
 *  <C0,C1,C2,C3> : Couleurs du quad
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void VertexQuad_Nearest(TData *Field,Vect3d P0,Vect3d P1,Vect3d P2,Vect3d P3,int C0,int C1,int C2,int C3,int Base){

   Vect3d p,p0,p1,p2,p3;

   /*Interpolate the localisations*/
   Vect_Interp(p0,P0,P1,0.5);
   Vect_Interp(p1,P1,P2,0.5);
   Vect_Interp(p2,P2,P3,0.5);
   Vect_Interp(p3,P3,P0,0.5);
   Vect_Interp(p,p0,p2,0.5);

   /*Draw quads*/
   if (C0>-1) {
      VR(P0,C0,Base);
      VR(p0,C0,Base);
      VR(p,C0,Base);
      VR(p3,C0,Base);
   }

   if (C1>-1) {
      VR(p0,C1,Base);
      VR(P1,C1,Base);
      VR(p1,C1,Base);
      VR(p,C1,Base);
   }

   if (C2>-1) {
      VR(p,C2,Base);
      VR(p1,C2,Base);
      VR(P2,C2,Base);
      VR(p2,C2,Base);
   }

   if (C3>-1) {
      VR(p3,C3,Base);
      VR(p,C3,Base);
      VR(p2,C3,Base);
      VR(P3,C3,Base);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <VertexLoc>
 * Creation : Mars 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Interpoler la position d'un point de grille en X,Y,Z.
 *
 * Parametres :
 *  <Ref>     : Georeference
 *  <Def>     : Definition des donnees
 *  <Vr>      : Vertex resultant
 *  <X>       : Coordonnee en X ([0 NI-1])
 *  <Y>       : Coordonnee en Y ([0 NJ-1])
 *  <Z>       : Coordonnee en Z ([0 NK-1])
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int VertexLoc(TGeoRef *Ref,TDataDef *Def,Vect3d Vr,double X,double Y,double Z) {

   Vect3d v00,v01,v10,v11,vt0,vt1,**pos;
   int    i,j,k,k1,idxj,idxj1;
   int    idx0,idx1,idx2,idx3;

   if (X>Def->NI-1 || Y>Def->NJ-1 || Z>Def->NK-1 || X<0 || Y<0 || Z<0) {
      return(0);
   }

   pos=Ref->Pos;
   i=floor(X);X-=i;
   j=floor(Y);Y-=j;
   k=floor(Z);Z-=k;

   idxj=Def->NI*j;

   /*Get gridpoint indexes and check for close match*/
   if (Y<TINY_VALUE) {
      idxj1=idxj;
   } else {
      idxj1=Def->NI*(j+1);
   }

   idx0=idxj+i;
   idx3=idxj1+i;

   if (X<TINY_VALUE) {
      idx1=idx0;
      idx2=idx3;
   } else {
      idx1=idx0+1;
      idx2=idx3+1;
   }

   /*3D Interpolation case*/
   if (Z>=TINY_VALUE) {

      k1=k+1;

      /*Interpolate over X*/
      Vect_InterpC(v00,pos[k][idx0],pos[k][idx1],X);
      Vect_InterpC(v01,pos[k][idx3],pos[k][idx2],X);
      Vect_InterpC(v10,pos[k1][idx0],pos[k1][idx1],X);
      Vect_InterpC(v11,pos[k1][idx3],pos[k1][idx2],X);

      /*Interpolate over Y*/
      Vect_InterpC(vt0,v00,v01,Y);
      Vect_InterpC(vt1,v10,v11,Y);

      /*Interpolate over Z*/
      Vect_InterpC(Vr,vt0,vt1,Z);
   } else {
      /*Interpolate over X*/
      Vect_InterpC(v00,pos[k][idx0],pos[k][idx1],X);
      Vect_InterpC(v01,pos[k][idx3],pos[k][idx2],X);

      /*Interpolate over Y*/
      Vect_InterpC(Vr,v00,v01,Y);
   }
   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <VertexVal>
 * Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Interpoler lineairement la valeur d'un point
 *            a l'interieur d'un voxel pour une les deux composante (UV=vitesse)
 *
 * Parametres :
 *   <Ref>    : Georeference
 *   <Def>    : Definition des donnees
 *   <X>      : Position en X
 *   <Y>      : Position en Y
 *   <Z>      : Position en Z (z<0, Interpolation 2D seulement)
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
float VertexVal(TGeoRef *Ref,TDataDef *Def,double X,double Y,double Z) {

   int    i,j,k,idxi,idxj,idxj1,idxk=0,idxk1;
   double val0,val1,val2,val3,cube[2][4];
   int    idx0,idx1,idx2,idx3;

   if (X>Def->NI-1 || Y>Def->NJ-1 || Z>Def->NK-1 || X<0 || Y<0 || Z<0) {
      return(0.0f);
   }

   i=floor(X);X-=i;
   j=floor(Y);Y-=j;

   idxj=Def->NI*j;

   /*Get gridpoint indexes and check for close match*/
   if (Y<TINY_VALUE) {
      idxj1=idxj;
   } else {
      idxj1=Def->NI*(j+1);
   }

   idx0=idxj+i;
   idx3=idxj1+i;

   if (X<TINY_VALUE) {
      idx1=idx0;
      idx2=idx3;
   } else {
      idx1=idx0+1;
      idx2=idx3+1;
   }

   /*3D Interpolation case*/
   if (Z>0) {
      k=floor(Z);Z-=k;

      idxk=FSIZE2D(Def)*k;
      if (Z<TINY_VALUE) {
         idxk1=idxk;
      } else {
         idxk1=FSIZE2D(Def)*(k+1);
      }

      idxi=idx0+idxk1;  Def_GetMod(Def,idxi,cube[1][0]);
      idxi=idx1+idxk1;  Def_GetMod(Def,idxi,cube[1][1]);
      idxi=idx3+idxk1;  Def_GetMod(Def,idxi,cube[1][3]);
      idxi=idx2+idxk1;  Def_GetMod(Def,idxi,cube[1][2]);
   }

   idxi=idx0+idxk;  Def_GetMod(Def,idxi,cube[0][0]);
   idxi=idx1+idxk;  Def_GetMod(Def,idxi,cube[0][1]);
   idxi=idx3+idxk;  Def_GetMod(Def,idxi,cube[0][3]);
   idxi=idx2+idxk;  Def_GetMod(Def,idxi,cube[0][2]);

   /*3D Interpolation case*/
   if (Z>0) {
      /*Interpolate over X*/
      val0=ILIN(cube[0][0],cube[0][1],X);
      val1=ILIN(cube[1][0],cube[1][1],X);
      val2=ILIN(cube[0][3],cube[0][2],X);
      val3=ILIN(cube[1][3],cube[1][2],X);

      /*Interpolate over Y*/
      val0=ILIN(val0,val2,Y);
      val1=ILIN(val1,val3,Y);

      /*Interpolate over Z*/
      return(ILIN(val0,val1,Z));
   } else {
      /*Interpolate over X*/
      val0=ILIN(cube[0][0],cube[0][1],X);
      val2=ILIN(cube[0][3],cube[0][2],X);

      /*Interpolate over Y*/
      return(ILIN(val0,val2,Y));
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <VertexValN>
 * Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Interpoler lineairement la valeur d'un point
 *            a l'interieur d'un voxel pour une seule composante
 *
 * Parametres :
 *   <Ref>    : Georeference
 *   <Def>    : Definition des donnees
 *   <X>      : Position en X
 *   <Y>      : Position en Y
 *   <Z>      : Position en Z (z<0, Interpolation 2D seulement)
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
float VertexValN(TGeoRef *Ref,TDataDef *Def,int Idx,double X,double Y,double Z) {

   int   i,j,k,idxi[4],idxj,idxj1,idxk=0,idxk1;
   float val0,val1,val2,val3,cube[2][4];
   int   idx0,idx1,idx2,idx3;

   if (X>Def->NI-1 || Y>Def->NJ-1 || Z>Def->NK-1 || X<0 || Y<0 || Z<0) {
      return(0.0f);
   }

   i=floor(X);X-=i;
   j=floor(Y);Y-=j;

   idxj=Def->NI*j;

   /*Get gridpoint indexes and check for close match*/
   if (Y<TINY_VALUE) {
      idxj1=idxj;
   } else {
      idxj1=Def->NI*(j+1);
   }

   idx0=idxj+i;
   idx3=idxj1+i;

   if (X<TINY_VALUE) {
      idx1=idx0;
      idx2=idx3;
   } else {
      idx1=idx0+1;
      idx2=idx3+1;
   }

   /*3D Interpolation case*/
   if (Z>0) {
      k=floor(Z);Z-=k;

      idxk=FSIZE2D(Def)*k;
      if (Z<TINY_VALUE) {
         idxk1=idxk;
      } else {
         idxk1=FSIZE2D(Def)*(k+1);
      }

      idxi[0]=idx0+idxk1;
      idxi[1]=idx1+idxk1;
      idxi[3]=idx3+idxk1;
      idxi[2]=idx2+idxk1;
      Def_GetQuad(Def,Idx,idxi,cube[1]);
   }

   /* Get the grid points value */

   idxi[0]=idx0+idxk;
   idxi[1]=idx1+idxk;
   idxi[3]=idx3+idxk;
   idxi[2]=idx2+idxk;
   Def_GetQuad(Def,Idx,idxi,cube[0]);

   /*3D Interpolation case*/
   if (Z>0) {
      /*Interpolate over X*/
      val0=ILIN(cube[0][0],cube[0][1],X);
      val1=ILIN(cube[1][0],cube[1][1],X);
      val2=ILIN(cube[0][3],cube[0][2],X);
      val3=ILIN(cube[1][3],cube[1][2],X);

      /*Interpolate over Y*/
      val0=ILIN(val0,val2,Y);
      val1=ILIN(val1,val3,Y);

      /*Interpolate over Z*/
      return(ILIN(val0,val1,Z));
   } else {
      /*Interpolate over X*/
      val0=ILIN(cube[0][0],cube[0][1],X);
      val2=ILIN(cube[0][3],cube[0][2],X);

      /*Interpolate over Y*/
      return(ILIN(val0,val2,Y));
   }
}

