/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de fichiers standards.
 * Fichier   : tclRadar.h
 * Creation  : Avril 2006 - J.P. Gauthier - CMC/CMOE
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

#ifdef LNK_URP

#ifndef _tclRADAR_h
#define _tclRADAR_h

#include "tclData.h"
#include "GeoRef.h"

#include "URP.h"
#include "drpdecode.h"
#include "radarEqs.h"
#include "GetRadarHeaderParameter.h"

extern char   *getDateTime(RADAR_DATA*);
extern char   *getMinorProductType(RADAR_DATA*);
extern char   *getSiteId(RADAR_DATA*);
extern char   *getSiteName(RADAR_DATA*);
extern char   *getDataFormat(RADAR_DATA*);
extern int     getHornHeight(RADAR_DATA*);
extern int     getGroundHeight(RADAR_DATA *);
extern float   getLatitude(RADAR_DATA *);
extern float   getLongitude(RADAR_DATA *);
extern int     getScanTimeInterval(RADAR_DATA *);
extern float   getNoise(RADAR_DATA *);
extern float   getReadbackElev(RADAR_DATA *);
extern float   getRangeBinResolution(RADAR_DATA*);
extern int     getTheta(RADAR_DATA*);
extern float   getThetaResolution(RADAR_DATA*);
extern int     getZCal(RADAR_DATA*);
extern int     getMultiPRFFlag(RADAR_DATA*);
extern int     getPRF(RADAR_DATA*);
extern int     getClutterFilter(RADAR_DATA*);
extern int     getWaveLength(RADAR_DATA*);

typedef struct Radar_File {
   char *CId;              /*Identificateur du fichier*/
   char *Name;             /*Path complet du fichier*/
   char Mode;              /*Mode d'ouverture du fichier (r,w,a)*/

   TGeoRef   *Ref;         /*GeoReference commmune a tout les scans*/
   RADAR_DATA Data;        /*Donnees du radar*/
} Radar_File;

typedef struct Radar_Head {
   RADAR_DATA *Data;
   int         Scan;
} Radar_Head;

int  TclRadar_Init(Tcl_Interp *Interp);

int         Radar_FileOpen(Tcl_Interp *Interp,char *Id,char Mode,char *Name);
int         Radar_FileClose(Tcl_Interp *Interp,char *Id);
Radar_File* Radar_FileGet(Tcl_Interp *Interp,char *Id);
void        Radar_FileParse(RADAR_DATA *Data);

int     Radar_Read(Tcl_Interp *Interp,char *Id,char* File,int Scan);
void    Radar_Free(TData *Rad);
Vect3d* Radar_Grid(TData *Rad,void *Proj,int Level);
void    Radar_HeadCopy(void *To,void *From);
void    Radar_Set(TData *Data);

CONST char* Radar_GetTypeString(DATA_TYPE Type);

#endif
#endif


