/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers d'observations
 * Fichier      : tclObs.h
 * Creation     : Fevrier 2003 - J.P. Gauthier
 *
 * Description  : Fichier d'entete du module Obs.
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

#ifndef _tclMetObs_h
#define _tclMetObs_h

#include "tclData.h"
#include "bufr_api.h"
#include "bufr_array.h"
#include "bufr_local.h"

#define MetObs_GetData(MD,E,V,T) (MD->Data[(T*MD->Nv+V)*MD->Ne+E])
#define MetObs_SetData(MD,E,V,T,O) (MD->Data[(T*MD->Nv+V)*MD->Ne+E]=O)
#define MET_VALID(V,N)  (V!=-979.0f && V!=-980.0f && V!=-999.0f && V!=N)
#define MET_TYPEID   0x0
#define MET_TYPENO   0x1
#define MET_TYPETG   0x2
#define MET_STATENEW 0x0
#define MET_STATESCO 0x1
#define MET_STATEHCO 0x2

typedef struct TBUFRTable {
   unsigned int No;
   char         Desc[64];
   char         Unit[16];
} TBUFRTable;

typedef struct TMetModelItem {
   int        X,Y;
   int        Code[3];
   TDataSpec *Spec;       /*Specification des donnees*/
} TMetModelItem;

typedef struct TMetModel {
   char *Name;
   char *Topo;
   int NItem,NRef,Space,Flat;
   TMetModelItem *Items;
} TMetModel;

typedef struct TMetElemData {

   int           Nv,Nt,Ne;        /*Data dimensions*/
   int           St;              /*Data state descriptor*/
   float        *Data;            /*Donnees temporelles et spatiales*/
   EntryTableB **Code;            /*Codes de la donnee*/
} TMetElemData;

typedef struct TMetElem {

   time_t        Time;      /*Date de validite*/
   int           NData;     /*Nombre d'element pour ce temps*/
   TMetElemData **EData;    /*Donnees des elements*/

   struct TMetElem *Next;
} TMetElem;

typedef struct TMetLoc {

   char    *Id;             /*Identificateur*/
   char    *No;             /*Numero*/
   char     Tag[16];        /*Tag unique*/
   char   **Info;           /*Autres informations*/
   Coord    Coord;          /*Position (lat,lon,elevation)*/
   Vect3d   Pix;            /*Pixel ou positioner la station statique*/
   int      Grid[3];        /*Dimension de la grille pour les donnees regroupees*/
   int      Level;

   TMetElem *Elems;         /*Liste temporelle des donnees*/

   struct TMetLoc *Next;
} TMetLoc;

typedef struct TMetObs {
   Tcl_Obj *Tag;
   Tcl_Obj *Elems;
   char    *Desc;

   char    **Info;
   int      NbInfo;
   time_t   Time,Time0,Time1;
   time_t   Cache;
   time_t   Persistance;
   float    NoData;
   int      Strict;
   int      FId;

   TMetLoc    *Loc;        /*Liste des localisations (stations)*/
   TMetModel  *Model;      /*Modele d'affichage*/
} TMetObs;

int      TclMetObs_Init(Tcl_Interp *Interp);

TMetObs *MetObs_Get(char *Name);
char*    MetObs_GetTag(TMetObs *Obs,int No);
Vect3d  *MetObs_Grid(Tcl_Interp *Interp,TGeoRef *Ref,TMetObs *Obs,long Time,Tcl_Obj *Elem,int *NObs,int Extrap);
int      MetObs_Copy(Tcl_Interp *Interp,TMetObs *Obs,char *Name);
int      MetObs_Extract(Tcl_Interp *Interp,TMetObs *Obs,TData *Field);
void     MetObs_Free(TMetObs *Obs);
void     MetObs_GetStat(TMetObs *Obs,TMetModelItem *Item);
int      MetObs_Load(Tcl_Interp *Interp,char *File,TMetObs *Obs);
int      MetObs_LoadBURP(Tcl_Interp *Interp,char *File,TMetObs *Obs);
int      MetObs_LoadBUFR(Tcl_Interp *Interp,char *File,TMetObs *Obs);
int      MetObs_LoadASCII(Tcl_Interp* Interp,char* File,TMetObs *Obs);
int      MetObs_WriteASCII(Tcl_Interp *Interp,char *File,Tcl_Obj *List,char *Title);
void     MetObs_LocFree(TMetLoc *Loc);
int      MetObs_Union(Tcl_Interp *Interp,Tcl_Obj *List,char *Token);
int      MetObs_Intersection(Tcl_Interp *Interp,Tcl_Obj *List,char *Token);
void     MetObs_Wipe();
EntryTableB *MetObs_BUFRFindTableCode(unsigned int Code);
EntryTableB *MetObs_BUFRFindTableDesc(char *Desc);
EntryTableB *MetObs_BUFRFindTableCodeOrDesc(Tcl_Interp *Interp,Tcl_Obj *Code);

TMetLoc *TMetLoc_Find(TMetObs *Obs,TMetLoc *From,char *Id,int Type);
TMetLoc *TMetLoc_FindWithCoord(TMetObs *Obs,TMetLoc *From,char *Id,double Lat,double Lon,double Elev,int Type,char *Multi);
TMetLoc *TMetLoc_New(TMetObs *Obs,char *Id,char *No,double Lat,double Lon,double Elev);

TMetElemData *TMetElem_Insert(TMetLoc *Loc,time_t Min,time_t Time,int Ne,int Nv,int Nt,float *Data,EntryTableB **Entry);
TMetElemData *TMetElem_InsertCopy(TMetLoc *Loc,time_t Min,time_t Time,TMetElemData *Data);
TMetElemData **TMetElem_Add(TMetLoc *Loc,time_t Time);
TMetElem     *TMetElem_Find(TMetLoc *Loc,long Time,int Exact);
void          TMetElem_Clean(TMetLoc *Loc,time_t Time);
void          TMetElem_Free(TMetElem *Elem);
float         TMetElem_Value(TMetElemData *Data,int Code,int Ne,int Nv,int Nt);
float         TMetElem_Height(TMetElemData *Data,int Code,int Ne,int Nv,int Nt);

void          TMetElemData_Free(TMetElemData *Data);

TMetElemData *MetReport_Get(char *Name);
Tcl_Obj      *MetReport_Put(Tcl_Interp *Interp,char *Name,TMetElemData *Report);
int           MetReport_Destroy(Tcl_Interp *Interp,char *Name);

TMetModel* MetModel_Get(char *Name);
int        MetModel_Create(Tcl_Interp *Interp,char *Name);
int        MetModel_Free(TMetModel *Model);
int        MetModel_FreeHash(Tcl_Interp *Interp,char *Name);
void       MetModel_Wipe();

#endif
