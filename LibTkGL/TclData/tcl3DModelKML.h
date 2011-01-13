/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Affichage de modele 3D
 * Fichier      : tcl3DModelKML.h
 * Creation     : Janvier 2011 - J.P. Gauthier
 *
 * Description  : Module de lecture de modele 3D en format Collada DAE
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


#ifndef _tcl3DModelKML_h
#define _tcl3DModelKML_h

#define XMLBUFSIZE 8192

typedef struct KMLData {
   char        Tag[256];
   char        *Path;
   char        *Buf;
   unsigned int BufLen;
   char         AltMode;
   T3DModel    *Model;

} KMLData;

#endif
