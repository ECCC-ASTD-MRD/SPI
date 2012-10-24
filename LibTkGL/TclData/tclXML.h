/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture de fichiers d'observations SWOB
 * Fichier      : tclXML.h
 * Creation     : Octobre 2012 - J.P. Gauthier
 *
 * Description  : Fonctions generale de parsing du XML avec expath.
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

#ifndef _tclXML_h
#define _tclXML_h

#include <tcl.h>
#include <string.h>
#include <expat.h>
#include "Vector.h"

#define XMLBUFSIZE 8192

typedef struct XML_Data {

   Tcl_Interp  *Interp;
   void        *Specific;
   char         Bloc;
   char        *Buf;
   unsigned int BufLen,BufRLen;
} XML_Data;

void XML_CharHandler(void *Data,const char *Txt,int Len);
void XML_CharReset(void *Data);
int  XML_ArrayCheck(char *Txt,unsigned int Len,char Sep);
int  XML_ArrayExpandVect(char *Txt,unsigned int Len,char Sep,int Dim,Vect3f *Array);

#endif
