/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de fichiers standards.
 * Fichier   : Vertex.h
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
 * Modification:
 *
 *   Nom         : -
 *   Date        : -
 *   Description : -
 *
 *==============================================================================
 */

#ifndef _VERTEX_H
#define _VERTEX_H

#define VR(V,C,C0)       glColor4ubv(Field->Spec->Map->Color[C<0?C0:C]);glVertex3dv(V)
#define VRT(V,C,C0)      glTexCoord1f(C<0.0?C0:C);glVertex3dv(V)

void  VertexGradient(TGeoRef *Ref,TDataDef *Def,Vect3d Nr);
int   VertexLoc(TGeoRef *Ref,TDataDef *Def,Vect3d Vr,double X,double Y,double Z);
float VertexVal(TGeoRef *Ref,TDataDef *Def,double X,double Y,double Z);
float VertexValN(TGeoRef *Ref,TDataDef *Def,int Idx,double X,double Y,double Z);
void  VertexInterp(Vect3d Pi,Vect3d P0,Vect3d P1,double V0,double V1,double Level);
void  VertexQuad_Linear(TData *Field,Vect3d P0,Vect3d P1,Vect3d P2,Vect3d P3,int C0,int C1,int C2,int C3,double V0,double V1,double V2,double V3,int Depth,int Base);
void  VertexQuad_Nearest(TData *Field,Vect3d P0,Vect3d P1,Vect3d P2,Vect3d P3,int C0,int C1,int C2,int C3,int Base);

#endif
