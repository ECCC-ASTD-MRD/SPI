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

#ifdef HAVE_GRIB

#include "grib_api.h"
#include "tclData.h"
#include "tclFSTD.h"

#define GRIB_STRLEN 512
#define GRIB_TABLESIZE 4096

struct TGRIBFile;

typedef struct TGRIBHeader {
   struct TGRIBFile *FID;
   grib_handle *Handle;

   int  Version;
   char NOMVAR[GRIB_STRLEN];
   long KEY;
   int  IP1;
   long DATEV;
   long DATEO;
} TGRIBHeader;

typedef struct TGRIBFile {
   char        *Id;
   char        *Path;
   char         Mode;
   FILE        *Handle;
   long         Size;
   TGRIBHeader *Table;
   int          TableNb;
} TGRIBFile;


int TclGRIB_Init(Tcl_Interp *Interp);

int        GRIB_FileOpen(Tcl_Interp *Interp,char* Id,char Mode,char* Name,int Index);
int        GRIB_FileClose(Tcl_Interp *Interp,char *Id);
TGRIBFile* GRIB_FileGet(Tcl_Interp *Interp,char *Id);
int        GRIB_FilePut(Tcl_Interp *Interp,TGRIBFile *File);

TData     *GRIB_FieldCreate(Tcl_Interp *Interp,char *Name,char *Sample,int NI,int NJ,int NK,TDef_Type Type);
int        GRIB_FieldRead(Tcl_Interp *Interp,char *Name,char *File,long Key);
int        GRIB_FieldWrite(Tcl_Interp *Interp,char *Id,TData *Field,int NPack,int Compress);
void       GRIB_FieldFree(TData *Data);
int        GRIB_FieldImport(Tcl_Interp *Interp,TData *Field,TData *RPN);
void       GRIB_FieldSet(TData *Data);
int        GRIB_FieldList(Tcl_Interp *Interp,TGRIBFile *File,int Mode,char *Var);
TGRIBHeader *GRIB_FieldFind(TGRIBFile *File,int DATEV,int IP1,char* NOMVAR);
void       GRIB_HeadCopy(void *To,void *From);
Vect3d*    GRIB_Grid(TData *Field,void *Proj,int Level);

int     GRIB_GridGet(Tcl_Interp *Interp,TData *Field,int NI,int NJ,int NK);
int     GRIB_GetLevel(TGRIBHeader *Head,float *Level,int *LevelType);
int     GRIB_GetData(TGRIBHeader *Head,TDef *Def,int Idx,double Factor);

int GRIB_FieldDefine(Tcl_Interp *Interp,TData *Field,int Objc,Tcl_Obj *CONST Objv[]);

OGRSpatialReferenceH GRIB_WKTProjCS(Tcl_Interp* Interp,grib_handle* Handle);

#endif
#endif


