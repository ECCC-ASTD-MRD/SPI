/*==============================================================================
* Environnement Canada
* Centre Meteorologique Canadian
* 2100 Trans-Canadienne
* Dorval, Quebec
*
* Projet    : Librairie Tcl de fichiers GRIB.
* Fichier   : tclGRIB.h
* Creation  : Decembre 2002 - J.P. Gauthier - CMC/CMOE
*
* Description: Utilisation des fichiers GRIB dans des scripts Tcl et
*              dans les projections.
*
* Remarques :
*
*==============================================================================
*/

#ifndef _tclGRIB_h
#define _tclGRIB_h

#include "grib_api.h"
#include "tclData.h"
#include "tclFSTD.h"

#define GRIB_STRLEN 512
#define GRIB_TABLESIZE 4096

struct GRIB_File;

typedef struct GRIB_Head {
   struct GRIB_File *FID;
   grib_handle *Handle;

   int  Version;
   char NOMVAR[GRIB_STRLEN];
   char CENTER[GRIB_STRLEN];
   long KEY;
   int  IP1;
   long DATEV;
   long DATEO;
} GRIB_Head;

typedef struct GRIB_File {
   char       *Id;
   char       *Path;
   char        Mode;
   FILE       *Handle;
   long        Size;
   GRIB_Head  *Table;
   int         TableNb;
} GRIB_File;


int TclGRIB_Init(Tcl_Interp *Interp);

int        GRIB_FileOpen(Tcl_Interp *Interp,char* Id,char Mode,char* Name,int Index);
int        GRIB_FileClose(Tcl_Interp *Interp,char *Id);
GRIB_File* GRIB_FileGet(Tcl_Interp *Interp,char *Id);
int        GRIB_FilePut(Tcl_Interp *Interp,GRIB_File *File);

int        GRIB_FieldRead(Tcl_Interp *Interp,char *Name,char *File,long Key);
void       GRIB_FieldFree(TData *Data);
void       GRIB_FieldSet(TData *Data);
int        GRIB_FieldList(Tcl_Interp *Interp,GRIB_File *File,int Mode,char *Var);
GRIB_Head *GRIB_FieldFind(GRIB_File *File,int DATEV,int IP1,char* NOMVAR);
void       GRIB_HeadCopy(void *To,void *From);
Vect3d*    GRIB_Grid(TData *Field,void *Proj,int Level);

int     GRIB_GetLevel(GRIB_Head *Head,float *Level,int *LevelType);
int     GRIB_GetData(GRIB_Head *Head,TDataDef *Def,int Idx,double Factor);

int GRIB_FieldDefine(Tcl_Interp *Interp,TData *Field,int Objc,Tcl_Obj *CONST Objv[]);

OGRSpatialReferenceH GRIB_WKTProjCS(Tcl_Interp* Interp,grib_handle* Handle);

#endif



