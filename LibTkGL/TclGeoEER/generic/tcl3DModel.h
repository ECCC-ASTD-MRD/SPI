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
 *=========================================================
 */

#ifndef _tcl3DModel_h
#define _tcl3DModel_h

#include "tkCanvVP.h"
#include "Projection.h"
#include "tclData.h"
#include "GeoRef.h"
#include "eerUtils.h"
#include "Vector.h"
#include "Matrix.h"

#define XMLBUFSIZE    8192
#define XMLSTRINGSIZE 256

#define F3V     3
#define F3VN    6
#define F3VNT   9

typedef struct TMaterial {
   float  Amb[4];
   float  Dif[4];
   float  Spe[4];
   float  Emi[4];
   float  Shi;
   float  Alpha;
   char   Path[XMLSTRINGSIZE];
   char   Name[XMLSTRINGSIZE];
   char   Target[XMLSTRINGSIZE];
   GLuint Tex;
} TMaterial;

typedef struct TFace {
   char         *Name;
   unsigned int NIdx;
   unsigned int *Idx;
   TMaterial    *Mt;                  // Material
} TFace;

typedef struct T3DObject {
   char         *Name;
   GLuint        GLId;                // Identificateur de liste d'affichage

   GLuint        NVr;                 // Nombre de Vertex
   GLuint        NFc;                 // Nombre de Face
   Vect3d        Extent[2];           // Extent
   int           Format;              // Format de Vertex

   Vect4f        *Cl;                 // Color list
   Vect3f        *Vr;                 // Vertex list
   Vect3f        *Nr;                 // Normal list
   Vect3f        *Tx;                 // Texture coordinate list
   TFace         *Fc;                 // Face list
} T3DObject;

typedef struct T3DScene {
   char         *Name;
   float        *Mtx;                 // Transformation matrix

   unsigned int  NObj;                // Number of object
   T3DObject    **Obj;                // Object list

   unsigned int  NScn;                // Number of scene items
   struct T3DScene *Scn;
   struct T3DScene *Parent;
} T3DScene;

typedef struct T3DModel {
   char         *Name;
   char         *Path;
   TGeoRef      *GRef;                // GeoReference
   TDataSpec    *Spec;                // Specification des donnees

   Vect3d        Extent[2];           // Extent
   double        Meter;               // Meter value (Meter is reference)

   Coord         Co;
   Vect3f        Pos;                 // Position
   Vect3f        MatrixT;             // Translation
   Vect3f        MatrixS;             // Scaling
   Vect3f        MatrixR;             // Rotation

   unsigned int  NMt;                 // Number of materials
   TMaterial    *Mt;                  // Material list

   unsigned int  NObj;                // Number of object
   T3DObject    *Obj;                 // Object list

   T3DScene     *Scn;                 // Scene items
} T3DModel;

void Model_Free(T3DModel *M);
void Model_Clean(T3DModel *M);
int  Model_Init(Tcl_Interp*);
int  Model_Load(Tcl_Interp* Interp,char* Name,char *Path);
int  Model_LoadFLT(Tcl_Interp* Interp,T3DModel *M,char *Path);
int  Model_LoadMDL(Tcl_Interp* Interp,T3DModel *M,char *Path);
int  Model_Load3DS(Tcl_Interp* Interp,T3DModel *M,char *Path);
int  Model_LoadDAE(Tcl_Interp* Interp,T3DModel *M,char *Path);
int  Model_LoadKML(Tcl_Interp* Interp,T3DModel *M,char *Path);
int  Model_LoadCityGML(Tcl_Interp* Interp,T3DModel *M,char *Path);
int  Model_Render(Projection* Proj,ViewportItem* VP,T3DModel *M);
void Model_RenderObject(Projection *Proj,ViewportItem *VP,T3DModel *M,T3DObject *Obj);
void Model_RenderScene(Projection *Proj,ViewportItem *VP,T3DModel *M,T3DScene *Scene);
void Model_NormalCompute(T3DModel *M,int Force);
int  Model_Grid(Tcl_Interp *Interp,TData *Data,T3DModel *M,T3DScene *Scene,TDef_InterpV Mode);
int  Model_GridObject(TData *Data,T3DModel *M,T3DObject *Obj,TDef_InterpV Mode);
void Model_Rasterize(TDef *Def,TGeoRef *Ref,Vect3d *Vr,int NVr,Vect3d *Ex,double Value);

T3DScene*  Model_SceneAdd(T3DModel *Model,T3DScene* Parent,int Nb);
void       Model_SceneFree(T3DScene *Scene);
T3DScene*  Model_SceneFind(T3DScene *Scene,char *Name);
T3DObject* Model_ObjectAdd(T3DModel *Model,int Nb);
void       Model_ObjectFree(T3DObject *Obj);
T3DObject *Model_ObjectFind(T3DModel *Model,char *Name);
TFace*     Model_ObjectFaceAdd(T3DObject *Obj,int Nb);
TFace*     Model_FaceFind(T3DModel *Model,char *Name,T3DObject **Obj);
TMaterial *Model_MaterialAdd(T3DModel *Model,int Nb);

void Model_Extent(T3DModel *Model);
void Model_ExtentObj(T3DModel *Model);
void Model_ExtentScene(T3DModel *Model,T3DScene *Scene,Matrix4d Matrix);

T3DModel*  Model_Get(char *Name);

#endif
