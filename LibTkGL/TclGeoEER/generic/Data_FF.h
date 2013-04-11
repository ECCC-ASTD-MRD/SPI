/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de fichiers standards.
 * Fichier   : Data_FF.h
 * Creation  : Septembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Definitions du module de rendue.
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

#ifndef _FF_H
#define _FF_H

#include "Projection.h"
#include "tclData.h"

#define FF_BOX   0x01
#define FF_TXT   0x02
#define FF_ALL   0x03

#define FF_NONE   0x00
#define FF_BOTTOM 0x01
#define FF_RIGHT  0x02
#define FF_TOP    0x04
#define FF_LEFT   0x08
#define FF_EQUAL  0x0F

#define FFSTREAMLEN 2048

#define NORMALIZE(U,V) {\
   float N;\
   N=sqrt(U*U+V*V);\
   U=U/N;\
   V=V/N;\
}

#define RK(R,S,V)    R[0]=S*V[0];R[1]=S*V[1];R[2]=S*V[2];
#define RKT(R,V1,V2) R[0]=0.5*(V1[0]+V2[0]);R[1]=0.5*(V1[1]+V2[1]);R[2]=0.5*(V1[2]+V2[2]);

static float *FFStreamMap;

typedef struct TKrigging {
   double *Matrix;
   double *Weight;
   double *V;
   int     Wrap;
   int     N;
   int     Mode;
   double  C0,C1,A;
} TKrigging;

float         FFCellResolution(ViewportItem *VP,Projection *Proj,Vect3d G0,Vect3d G1);
int           FFCellProcess(ViewportItem *VP,Projection *Proj,Vect3d G0,Vect3d G1,Vect3d G2,Vect3d G3,Vect3d Dim);
float        *FFStreamMapSetup1D(double Delta);

int           FFContour(int Mode,TGeoRef *Ref,TDataDef *Def,TDataStat *Stat,Projection *Proj,int NbInter,float *Inter,int Depth,int Limit);
int           FFContour_Quad(TGeoRef *Ref,TDataDef *Def,unsigned char *PMatrix,int X,int Y,int Z,float Inter,int Mode,int Side,int Depth,int Limit);

int           FFMarchingCube(TGeoRef *Ref,TDataDef *Def,Projection *Proj,double Level);
float        *FFStreamMapSetup1D(double Delta);
int           FFStreamLine(TGeoRef *Ref,TDataDef *Def,ViewportItem *VP,Vect3d *Stream,float *Map,double X,double Y,double Z,int MaxIter,double Step,double Min,double Res,int Mode,int ZDim);
int           FFKrigging(TGeoRef *Ref,TDataDef *Def,Vect3d *Pos,int NPos,double C0,double C1,double A,int Mode);
double        FFKrigging_Value(TKrigging *Krig,Vect3d *Pos,double X,double Y,double *Error);

int     LUDecompose(double *Matrix,int N,int *Perm,int *d);
void    LUBackSub(double *Matrix,int N,int *Perm,double *Vect);
double *LUInvert(double *Matrix,int N);

#endif
