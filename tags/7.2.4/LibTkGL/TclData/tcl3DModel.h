/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Application de photos geotiff diverses
 * Fichier      : tcl3DModel.h
 * Creation     : Janvier 2003 - J.P. Gauthier
 *
 * Description  : Module de lecture de modele 3D
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

#ifndef _MODEL_H
#define _MODEL_H

#include "tkCanvVP.h"
#include "Projection.h"
#include "tclData.h"
#include "GeoRef.h"
#include "Helpers.h"

#define F3V     3
#define F3VN    6
#define F3VNT   9

typedef struct TMaterial {
   float  Amb[4];
   float  Dif[4];
   float  Spe[4];
   float  Emi[4];
   float  Shi;
   char   Path[256];
   char   Name[256];
   GLint  Tex;
} TMaterial;

typedef struct TFace {
   unsigned int NIdx;
   unsigned int *Idx;
   TMaterial    *Mt;                  /*Material*/
} TFace;

typedef struct T3DObject {

   char          Name[256];
   GLuint        GLId;                /*Identificateur de list d'affichage*/

   GLuint        NVr;                 /*Nombre de Vertex*/
   GLuint        NFc;                 /*Nombre de Face*/
   Vect3d        Extent[2];           /*Extent*/
   int           Format;             /*Format de Vertex*/

   Vect4f        *Cl;                 /*Color list*/
   Vect3f        *Vr;                 /*Vertex list*/
   Vect3f        *Nr;                 /*Normal list*/
   Vect3f        *Tx;                 /*Texture coordinate list*/
   TFace         *Fc;                 /*Face list*/
} T3DObject;

typedef struct T3DModel {
   char         *Path;
   TGeoRef      *Ref;                 /*GeoReference*/
   TDataSpec    *Spec;                /*Specification des donnees*/

   int           Active;
   Vect3d        Extent[2];          /*Extent*/

   Coord         Co;
   Vect3f        Pos;                 /*Position*/
   Vect3f        MatrixT;             /*Translation*/
   Vect3f        MatrixS;             /*Scaling*/
   Vect3f        MatrixR;             /*Rotation*/

   int           NObj;                /*Number of object*/
   T3DObject    *Obj;                 /*Object list*/

   int           NMt;                 /*Number of materials*/
   TMaterial    *Mt;                  /*Material list*/
} T3DModel;

static int Model_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int Model_Create(Tcl_Interp *Interp,char* Name);
static int Model_Destroy(Tcl_Interp *Interp,char *Name);
static int Model_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int Model_Stat(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int Model_Matrix(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int Model_Material(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);

void Model_Free(T3DModel *M);
void Model_Clean(T3DModel *M);
int  Model_Init(Tcl_Interp*);
int  Model_Load(Tcl_Interp* Interp,char* Name,char *Path);
int  Model_LoadMDL(T3DModel *M,char *Path);
int  Model_Render(Projection* Proj,ViewportItem* VP,T3DModel *M);
void Model_NormalCompute(T3DModel *M);

void Model_ObjFree(T3DObject *Obj);
T3DObject* Model_ObjAdd(T3DModel *Model,int Nb);

T3DModel* Model_Get(char *Name);

#endif
