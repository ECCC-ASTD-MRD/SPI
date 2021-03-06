/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers d'observations
 * Fichier      : tclVector.h
 * Creation     : Fevrier 2005 - J.P. Gauthier
 *
 * Description  : Fichier d'entete du module Vector.
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
 *
 *=========================================================
 */
#ifndef _tclVector_h
#define _tclVector_h

#include "tcl.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define VECTORMAX(VEC,VAR) {\
   int v=0;\
   VAR=-HUGE_VAL;\
   for(v=0;v<VEC->N;v++) {\
       VAR=(VEC->V[v]>VAR && VEC->V[v]!=VEC->NoData)?VEC->V[v]:VAR;\
   }\
}

#define VECTORMIN(VEC,VAR) {\
   int v=0;\
   VAR=HUGE_VAL;\
   for(v=0;v<VEC->N;v++) {\
       VAR=(VEC->V[v]<VAR && VEC->V[v]!=VEC->NoData)?VEC->V[v]:VAR;\
   }\
}

struct TDef;

typedef struct TVectorSpec {
   double Alloc;       /* Mode d'allocation memoire (facteur d'agrandissement) */
} TVectorSpec;

typedef struct TVector {
   double          *V;      /* Valeurs */
   double           NoData; /* Valeur de novalue*/
   int              N,Nr;   /* Longueur du vecteur et longueur memoire*/
   Tcl_Obj         *Cn;     /* Noms des composantes */
   struct TVector **Cp;     /* Pointeur sur les composantes */
   struct TDef *Def;    /* Representation TDef temporaire */
} TVector;

int TclVector_Init(Tcl_Interp *Interp);

TVector*         Vector_Get(char *Name);
struct TDef*     Vector_GetDef(TVector *Vec);
struct TDef**    Vector_GetCompDefs(TVector *Vec);

int      Vector_Create(Tcl_Interp *Interp,char *Name,Tcl_Obj *Comp);
void     Vector_Clear(Tcl_Interp *Interp,TVector *Vec);
TVector* Vector_Copy(Tcl_Interp *Interp,TVector *Vec,char *Name);
int      Vector_Destroy(Tcl_Interp *Interp,char *Name);
int      Vector_AppendData(Tcl_Interp *Interp,TVector *Vec,Tcl_Obj *List,double Value);
int      Vector_SetData(Tcl_Interp *Interp,TVector *Vec,Tcl_Obj *List,int Idx);
Tcl_Obj* Vector_GetData(Tcl_Interp *Interp,TVector *Vec,int Idx,int Sub);
int      Vector_Sort(Tcl_Interp *Interp,TVector *Vec,char *Comp,int Unique);
int      Vector_Length(Tcl_Interp *Interp,TVector *Vec,int Len);
int      Vector_Mem(Tcl_Interp *Interp,TVector *Vec,int Mem);
void     Vector_Free(TVector *Vec);
void     Vector_Wipe();
int      Vector_ValidateLUT(Tcl_Interp *Interp,TVector *Lut);

void Vector_Swap(TVector *Vec,int IdxFrom,int IdxTo);
void Vector_QuickSort(TVector *Vec,int Comp,int start,int end);
#endif
