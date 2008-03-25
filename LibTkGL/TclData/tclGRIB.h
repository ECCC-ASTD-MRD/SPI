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

typedef struct GRIB_File {
   char  *Id;
   char  *Path;
   char   Mode;
   FILE  *Handle;
   long   Size;
} GRIB_File;

typedef struct GRIB_Head {
   grib_handle *Handle;

   time_t       Valid;      /*Date de validite*/
} GRIB_Head;

int TclGRIB_Init(Tcl_Interp *Interp);

int        GRIB_FileOpen(Tcl_Interp *Interp,char* Id,char Mode,char* Name);
int        GRIB_FileClose(Tcl_Interp *Interp,char *Id);
GRIB_File* GRIB_FileGet(char *Id);
int        GRIB_FilePut(Tcl_Interp *Interp,GRIB_File *File);

void    GRIB_FieldFree(TData *Data);
void    GRIB_FieldSet(TData *Data);
void    GRIB_HeadCopy(void *To,void *From);
Vect3d* GRIB_Grid(TData *Field,void *Proj);
#endif



