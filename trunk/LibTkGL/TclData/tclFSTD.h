/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de fichiers standards.
 * Fichier   : tclFSTD.h
 * Creation  : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Utilisation des fichiers standards RPN dans des scripts Tcl et
 *              dans les projections.
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

#ifndef _tclFSTD_h
#define _tclFSTD_h

#include "tclData.h"
#include "EZTile.h"
#include "EZVrInt.h"

#define FSTD_LISTALL   0
#define FSTD_LISTVAR   1
#define FSTD_LISTDATEV 2
#define FSTD_LISTIP1   3

typedef struct TFSTDVector {
   char  *UU,*VV,*WW;
} TFSTDVector;

typedef struct FSTD_File {
   char *CId;              /*Identificateur du fichier*/
   char *Name;             /*Path complet du fichier*/
   char Mode;              /*Mode d'ouverture du fichier (r,w,a)*/
   int  Open;              /*Etat du fichier*/
   unsigned int Id;        /*Numero d'unite du fichier*/
   int  No;                /*Numero interne du fichier*/
} FSTD_File;

/*Structure d'entete de champs*/
typedef struct FSTD_Head {
   FSTD_File *FID;        /*Fichier dont provient le champs*/
   int  KEY;              /*Cle du champs*/
   int  DATEO;            /*Date d'origine du champs*/
   int  DATEV;            /*Date de validitee du champs*/
   int  DEET;
   int  NPAS;
   int  NBITS;
   int  DATYP;            /*Type de donnees*/
   int  IP1,IP2,IP3;      /*Specificateur du champs*/
   char TYPVAR[3];        /*Type de variable*/
   char NOMVAR[5];        /*Nom de la variable*/
   char ETIKET[13];       /*Etiquette du champs*/
   int  IG1,IG2,IG3,IG4;
   int  SWA;
   int  LNG;
   int  DLTF;
   int  UBC;
   int  EX1,EX2,EX3;
}  FSTD_Head;

int  TclFSTD_Init(Tcl_Interp *Interp);
TFSTDVector *FSTD_VectorTableCheck(char *Var,int *Idx);

TData* FSTD_FieldCreate(Tcl_Interp *Interp,char *Name,int NI,int NJ,int NK,int Type);
void   FSTD_FieldDataGet(Tcl_Interp *Interp,TData *Field);
int    FSTD_FieldDataPut(Tcl_Interp *Interp,TData *Field,char* ValueList);
int    FSTD_FieldDefine(Tcl_Interp *Interp,TData *Field,int Objc,Tcl_Obj *CONST Objv[]);
int    FSTD_FieldFind(Tcl_Interp *Interp,char *Id,int Max,int DateV,char* Eticket,int IP1,int IP2,int IP3,char* TypVar,char* NomVar);
void   FSTD_FieldFree(TData *Field);
int    FSTD_FieldGridInterpolate(Tcl_Interp *Interp,TData *FieldTo,TData *FieldFrom,int Mode);
int    FSTD_FieldVertInterpolate(Tcl_Interp *Interp,TData *FieldTo,TData *FieldFrom,TData *ZFieldTo,TData *ZFieldFrom);
int    FSTD_FieldTimeInterpolate(Tcl_Interp *Interp,int Stamp,char *Name,TData *Field0,TData *Field1);
int    FSTD_FieldList(Tcl_Interp *Interp,FSTD_File *File,int Mode,char *Var);
int    FSTD_FieldRead(Tcl_Interp *Interp,char *Name,char *Id,int Key,int DateV,char *Eticket,int DIP1,int IP2,int IP3,char *TypVar,char *NomVar);
int    FSTD_FieldReadHead(Tcl_Interp *Interp,char *Id,int Key);
int    FSTD_FieldReadLevels(Tcl_Interp *Interp,TData *Field,int Invert);
int    FSTD_FieldReadMesh(TData *Field);
int    FSTD_FieldStat(Tcl_Interp *Interp,TData *Field,int Objc,Tcl_Obj *CONST Objv[]);
void   FSTD_FieldSetTo(TData *FieldTo,TData *FieldFrom);
int    FSTD_FieldWrite(Tcl_Interp *Interp,char *Id,TData *Field,int NPack,int Rewrite,int Compress);
void   FSTD_FieldSet(TData *Data);

double  FSTD_IP2Meter(int IP);
double  FSTD_IP2Level(int IP,int *Type);
int     FSTD_Level2IP(float Level,int Type);
void    FSTD_DataMap(TData *Field,int Idx);

Vect3d* FSTD_Grid(TData *Field,void *Proj);
void    FSTD_HeadCopy(void *To,void *From);

int         FSTD_FileClose(Tcl_Interp *Interp,char *Id);
int         FSTD_FileOpen(Tcl_Interp *Interp,char *Id,char Mode,char *Name);
FSTD_File*  FSTD_FileGet(Tcl_Interp *Interp,char *Id);
int         FSTD_FileSet(Tcl_Interp *Interp,FSTD_File *File);
int         FSTD_FileUnset(Tcl_Interp *Interp,FSTD_File *File);

#endif



