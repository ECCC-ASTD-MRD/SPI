/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers d'observations
 * Fichier      : tclMetDataset.h
 * Creation     : Mai 2008 - J.P. Gauthier
 *
 * Description  : Fonctions de manipulation des Dataset et Template BUFR par Tcl.
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
#ifndef _tclMetDataset_h
#define _tclMetDataset_h

#ifdef HAVE_ECBUFR

#include "tclData.h"
#include "bufr_api.h"
#include "bufr_array.h"
#include "bufr_local.h"

typedef struct BUFR_Datasubset {
   BUFR_Sequence  *BSeq;
   BufrDDOp       *BDDO;
   ListNode       *Node;
} BUFR_Datasubset;

int TclMetDataset_Init(Tcl_Interp *Interp);

BUFR_Dataset*    MetDataset_Get(char *Name);
Tcl_Obj*         MetDataset_Put(Tcl_Interp *Interp,char *Name,BUFR_Dataset *Set);
BUFR_Datasubset* MetDatasubset_Get(char *Name);
int              MetDatasubset_Put(Tcl_Interp *Interp,char *Name,BUFR_Datasubset *SSet);
BUFR_Template*   MetTemplate_Get(char *Name);
Tcl_Obj*         MetTemplate_Put(Tcl_Interp *Interp,char *Name,BUFR_Template *Tmp);

FILE* MetDatafile_Get(char *Name);
int MetDatafile_Put(Tcl_Interp *Interp,char *Name,FILE *File);

Tcl_Obj*       MetDataset_Code2Obj(Tcl_Interp *Interp,BufrDescriptor *BCV);
Tcl_Obj*       MetDataset_Value2Obj(BufrValue *V);
int            MetDataset_Obj2Code(Tcl_Interp *Interp,BufrDescriptor *BCV,Tcl_Obj *Obj);

#endif
#endif