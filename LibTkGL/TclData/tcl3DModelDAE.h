/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Affichage de modele 3D
 * Fichier      : tcl3DModelDAE.h
 * Creation     : Aout2007 - J.P. Gauthier
 *
 * Description  : Module de lecture de modele 3D en format Collada DAE
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

#ifndef _tcl3DModelDAE_h
#define _tcl3DModelDAE_h

#define DAEBUFSIZE 8192

typedef struct DAESource {
   char         Alias;
   char        *Id;
   char        *Text;
   unsigned int TextLen;
   unsigned int Nb;
   unsigned int Dim;
   float       *Array;
} DAESource;

typedef struct DAEData {
   char      Tag[256];
   T3DModel  *Model;
   T3DScene  *Scene,*Nodes;
   T3DObject *Object;
   TFace     *Fc;
   TList     *Sources;
   int        NFc,NVr,VrDim,VrType;
   int        VrOffset,NrOffset,TxOffset;
   DAESource *VrSource,*NrSource,*TxSource;
} DAEData;

int        ModelDAE_SourceExpand(DAESource *Source);
void       ModelDAE_SourceFree(DAESource *Source);
DAESource *ModelDAE_SourceFind(DAEData *Data,char* Id);

#endif