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
   Nr[0]=VertexVal(Ref,Def,-1,v[0]-0.5,v[1],v[2])-VertexVal(Ref,Def,-1,v[0]+0.5,v[1],v[2]);
   Nr[1]=VertexVal(Ref,Def,-1,v[0],v[1]-0.5,v[2])-VertexVal(Ref,Def,-1,v[0],v[1]+0.5,v[2]);
   Nr[2]=VertexVal(Ref,Def,-1,v[0],v[1],v[2]-0.5)-VertexVal(Ref,Def,-1,v[0],v[1],v[2]+0.5);

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
   Vect_Mid(p0,P0,P1);
   Vect_Mid(p1,P1,P2);
   Vect_Mid(p2,P2,P3);
   Vect_Mid(p3,P3,P0);
   Vect_Mid(p,p0,p2);

   /*Interpolate the values*/
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

   Depth--;

   /*Draw if visible*/
   if (C0>-1 || c0>-1 || c>-1 || c3>-1) {
      if (Depth && ((C0!=c0) || (c0!=c) || (c!=c3) || (c3!=C0))) {
         VertexQuad_Linear(Field,P0,p0,p,p3,C0,c0,c,c3,V0,v0,v,v3,Depth,Base);
      } else {
         VR(P0,C0,Base);
         VR(p0,c0,Base);
         VR(p,c,Base);
         VR(p3,c3,Base);
      }
   }

   if (c0>-1 || C1>-1 || c1>-1 || c>-1) {
      if (Depth && ((c0!=C1) || (C1!=c1) || (C1!=c) || (c!=c0))) {
         VertexQuad_Linear(Field,p0,P1,p1,p,c0,C1,c1,c,v0,V1,v1,v,Depth,Base);
      } else {
         VR(p0,c0,Base);
         VR(P1,C1,Base);
         VR(p1,c1,Base);
         VR(p,c,Base);
      }
   }

   if (c>-1 || c1>-1 || C2>-1 || c2>-1) {
      if (Depth && ((c!=c1) || (c1!=C2) || (C2!=c2) || (c2!=c))) {
         VertexQuad_Linear(Field,p,p1,P2,p2,c,c1,C2,c2,v,v1,V2,v2,Depth,Base);
      } else {
         VR(p,c,Base);
         VR(p1,c1,Base);
         VR(P2,C2,Base);
         VR(p2,c2,Base);
      }
   }

   if (c3>-1 || c>-1 || c2>-1 || C3>-1) {
      if (Depth && ((c3!=c) || (c!=c2) || (c2!=C3) || (C3!=c3))) {
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
   Vect_Mid(p0,P0,P1);
   Vect_Mid(p1,P1,P2);
   Vect_Mid(p2,P2,P3);
   Vect_Mid(p3,P3,P0);
   Vect_Mid(p,p0,p2);

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

   Vect3d        v00,v01,v10,v11,v0,v1,**pos;
   unsigned long i,j,k,k1;
   unsigned long idx0,idx1,idx2,idx3;

   if (X>Def->NI-1 || Y>Def->NJ-1 || Z>Def->NK-1 || X<0 || Y<0 || Z<0) {
      return(0);
   }

   pos=Ref->Pos;
   i=X;X-=i;
   j=Y;Y-=j;
   k=Z;Z-=k;

   /*Get gridpoint indexes*/
   idx0=Def->Idx+j*Def->NI+i;
   idx1=idx0+1;
   idx3=(j==Def->NJ-1)?idx0:idx0+Def->NI;
   idx2=idx3+1;

   /*3D Interpolation case*/
   if (Z>TINY_VALUE) {

      k1=k+1;

      Vect_InterpC(v00,pos[k][idx0],pos[k1][idx0],Z);
      Vect_InterpC(v10,pos[k][idx1],pos[k1][idx1],Z);
      Vect_InterpC(v11,pos[k][idx2],pos[k1][idx2],Z);
      Vect_InterpC(v01,pos[k][idx3],pos[k1][idx3],Z);
   } else {
      Vect_Assign(v00,pos[k][idx0]);
      Vect_Assign(v10,pos[k][idx1]);
      Vect_Assign(v01,pos[k][idx3]);
      Vect_Assign(v11,pos[k][idx2]);
   }

   /*Interpolate over X*/
   if (X>TINY_VALUE) {
      Vect_InterpC(v0,v00,v10,X);
      Vect_InterpC(v1,v01,v11,X);
   }  else {
      Vect_Assign(v0,v00);
      Vect_Assign(v1,v01);
   }

   /*Interpolate over Y*/
   if (Y>TINY_VALUE) {
      Vect_InterpC(Vr,v0,v1,Y);
   } else {
      Vect_Assign(Vr,v0);
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
 *   <Idx>    : Composantes (-1=mode)
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
float VertexVal(TGeoRef *Ref,TDataDef *Def,int Idx,double X,double Y,double Z) {

   double        cube[2][4];
   unsigned long i,j,k,idx[4],idxk;

   if (X>Def->NI-1 || Y>Def->NJ-1 || Z>Def->NK-1 || X<0 || Y<0 || Z<0) {
      return(0);
   }

   i=X;X-=i;
   j=Y;Y-=j;
   k=Z;Z-=k;

   // Get gridpoint indexes
   idxk=Def->NIJ;

   idx[0]=(k?idxk*k:0)+j*Def->NI+i;
   idx[1]=idx[0]+1;
   idx[3]=(j==Def->NJ-1)?idx[0]:idx[0]+Def->NI;
   idx[2]=idx[3]+1;
   if (Idx==-1) {
      Def_GetQuadMod(Def,idx,cube[0]);
   } else {
      Def_GetQuad(Def,Idx,idx,cube[0]);
   }
   
   // If either value is nodata then interpolation will be nodata as well
   if (cube[0][0]==Def->NoData || cube[0][1]==Def->NoData || cube[0][2]==Def->NoData || cube[0][3]==Def->NoData) {
      return(Def->NoData);
   }
   
   // 3D Interpolation case
   if (Z>TINY_VALUE) {

      idx[0]+=idxk;
      idx[1]+=idxk;
      idx[3]+=idxk;
      idx[2]+=idxk;
      if (Idx==-1) {
         Def_GetQuadMod(Def,idx,cube[1]);
      } else {
         Def_GetQuad(Def,Idx,idx,cube[1]);
      }
      // If either value is nodata then interpolation will be nodata as well
      if (cube[1][0]==Def->NoData || cube[1][1]==Def->NoData || cube[1][2]==Def->NoData || cube[1][3]==Def->NoData) {
         return(Def->NoData);
      }

      cube[0][0]=ILIN(cube[0][0],cube[1][0],Z);
      cube[0][1]=ILIN(cube[0][1],cube[1][1],Z);
      cube[0][2]=ILIN(cube[0][2],cube[1][2],Z);
      cube[0][3]=ILIN(cube[0][3],cube[1][3],Z);
   }

   /*Interpolate over X*/
   if (X>TINY_VALUE) {
      cube[0][0]=ILIN(cube[0][0],cube[0][1],X);
      cube[0][3]=ILIN(cube[0][3],cube[0][2],X);
   }

   /*Interpolate over Y*/
   if (Y>TINY_VALUE) {
      cube[0][0]=ILIN(cube[0][0],cube[0][3],Y);
   }

   return(cube[0][0]);
}

double VertexValV(TGeoRef *Ref,TDataDef *Def,double X,double Y,double Z,Vect3d V) {

   double        cube[3][2][4];
   unsigned long i,j,k,idx[4],idxk;

   if (X>Def->NI-1 || Y>Def->NJ-1 || Z>Def->NK-1 || X<0 || Y<0 || Z<0) {
      return(0);
   }

   i=X;X-=i;
   j=Y;Y-=j;
   k=Z;Z-=k;

   // Get gridpoint indexes
   idxk=Def->NIJ;

   idx[0]=(k?idxk*k:0)+j*Def->NI+i;
   idx[1]=idx[0]+1;
   idx[3]=(j==Def->NJ-1)?idx[0]:idx[0]+Def->NI;
   idx[2]=idx[3]+1;
   
   Def_GetQuad(Def,0,idx,cube[0][0]);
   Def_GetQuad(Def,1,idx,cube[1][0]);
   if (Def->Data[2]) Def_GetQuad(Def,2,idx,cube[2][0]);
   
   // If either value is nodata then interpolation will be nodata as well
   if (cube[0][0][0]==Def->NoData || cube[0][0][1]==Def->NoData || cube[0][0][2]==Def->NoData || cube[0][0][3]==Def->NoData) {
      return(Def->NoData);
   }
   
   // 3D Interpolation case
   if (Z>TINY_VALUE) {

      idx[0]+=idxk;
      idx[1]+=idxk;
      idx[3]+=idxk;
      idx[2]+=idxk;
      Def_GetQuad(Def,0,idx,cube[0][1]);
      Def_GetQuad(Def,1,idx,cube[1][1]);
      if (Def->Data[2]) Def_GetQuad(Def,2,idx,cube[2][1]);
      
      // If either value is nodata then interpolation will be nodata as well
      if (cube[0][1][0]==Def->NoData || cube[0][1][1]==Def->NoData || cube[0][1][2]==Def->NoData || cube[0][1][3]==Def->NoData) {
         return(Def->NoData);
      }

      cube[0][0][0]=ILIN(cube[0][0][0],cube[0][1][0],Z);
      cube[0][0][1]=ILIN(cube[0][0][1],cube[0][1][1],Z);
      cube[0][0][2]=ILIN(cube[0][0][2],cube[0][1][2],Z);
      cube[0][0][3]=ILIN(cube[0][0][3],cube[0][1][3],Z);
      cube[1][0][0]=ILIN(cube[1][0][0],cube[1][1][0],Z);
      cube[1][0][1]=ILIN(cube[1][0][1],cube[1][1][1],Z);
      cube[1][0][2]=ILIN(cube[1][0][2],cube[1][1][2],Z);
      cube[1][0][3]=ILIN(cube[1][0][3],cube[1][1][3],Z);
      if (Def->Data[2]) {
         cube[2][0][0]=ILIN(cube[2][0][0],cube[2][1][0],Z);
         cube[2][0][1]=ILIN(cube[2][0][1],cube[2][1][1],Z);
         cube[2][0][2]=ILIN(cube[2][0][2],cube[2][1][2],Z);
         cube[2][0][3]=ILIN(cube[2][0][3],cube[2][1][3],Z);
      }
   }
   
   V[0]=cube[0][0][0];
   V[1]=cube[1][0][0];
   V[2]=Def->Data[2]?cube[2][0][0]:0.0;
   
   /*Interpolate over X*/
   if (X>TINY_VALUE) {
      V[0]=cube[0][0][0]=ILIN(cube[0][0][0],cube[0][0][1],X);
           cube[0][0][3]=ILIN(cube[0][0][3],cube[0][0][2],X);
      V[1]=cube[1][0][0]=ILIN(cube[1][0][0],cube[1][0][1],X);
           cube[1][0][3]=ILIN(cube[1][0][3],cube[1][0][2],X);
      if (Def->Data[2]) {
         V[2]=cube[2][0][0]=ILIN(cube[2][0][0],cube[2][0][1],X);
              cube[2][0][3]=ILIN(cube[2][0][3],cube[2][0][2],X);
      }
   }

   /*Interpolate over Y*/
   if (Y>TINY_VALUE) {
      V[0]=ILIN(cube[0][0][0],cube[0][0][3],Y);
      V[1]=ILIN(cube[1][0][0],cube[1][0][3],Y);
      if (Def->Data[2]) {
         V[2]=ILIN(cube[2][0][0],cube[2][0][3],Y);
      }
   }
   
   return(0);
}
