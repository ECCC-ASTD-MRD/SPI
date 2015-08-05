/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de fichiers standards.
 * Fichier   : tclRadar.c
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

#ifdef HAVE_URP

#include "tclRADAR.h"
#include "tclGeoRef.h"
#include "Projection.h"

static Tcl_HashTable Radar_FileTable;
static int           RadarInit=0;

static int Radar_FileCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int Radar_ScanCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);

static CONST char *RadarDataType[] = { "Uncorrected LOG Z","Corrected LOG Z","Radial Velocity","Spectrum Width","Differential Reflectivity","KDP (Differential Phase)","PhiDP (Differential Phase)","SQI","RhoHv","Signal to Noise",NULL };

CONST char* Radar_GetTypeString(DATA_TYPE Type) {
   switch(Type) {
      case     UNCOR_Z         : return(RadarDataType[0]); break;
      case     COR_Z           : return(RadarDataType[1]); break;
      case     RADIAL_VELOCITY : return(RadarDataType[2]); break;
      case     SPECTRUM_WIDTH  : return(RadarDataType[3]); break;
      case     ZDR             : return(RadarDataType[4]); break;
      case     KDP             : return(RadarDataType[5]); break;
      case     PHIDP           : return(RadarDataType[6]); break;
      case     SQI             : return(RadarDataType[7]); break;
      case     RHOHV           : return(RadarDataType[8]); break;
      case     SNR             : return(RadarDataType[9]); break;
   }
   return(NULL);
}

/*----------------------------------------------------------------------------
 * Nom      : <tclRadar_Init>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser le package lors de l'inclusion par Tcl.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int TclRadar_Init(Tcl_Interp *Interp) {

   Tcl_CreateObjCommand(Interp,"radarfile",Radar_FileCmd,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);
   Tcl_CreateObjCommand(Interp,"radarscan",Radar_ScanCmd,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);

   if (!RadarInit++) {
      Tcl_InitHashTable(&Radar_FileTable,TCL_STRING_KEYS);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Radar_FileCmd>
 * Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Appel des commandes relies aux fichiers radars.
 *
 * Parametres     :
 *  <clientData>  : Donnees du module.
 *  <Interp>      : Interpreteur TCL.
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int Radar_FileCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   Radar_File         *file;
   int                n,idx;
   static CONST char *sopt[] = { "open","close","filename",NULL };
   enum               opt { OPEN,CLOSE,FILENAME };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return TCL_ERROR;
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",TCL_EXACT,&idx)!=TCL_OK) {
      return TCL_ERROR;
   }

   switch ((enum opt)idx) {
      case OPEN:
         if(Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id mode filename");
            return(TCL_ERROR);
         }
         return (Radar_FileOpen(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3])[0],Tcl_GetString(Objv[4])));
         break;

      case CLOSE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fileid");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            Radar_FileClose(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case FILENAME:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         if (!(file=Radar_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,Tcl_NewStringObj(file->Name,-1));
         return(TCL_OK);
         break;
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <Radar_ScanCmd>
 * Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Appel des commandes relies aux donnes radar.
 *
 * Parametres     :
 *  <clientData>  : Donnees du module.
 *  <Interp>      : Interpreteur TCL.
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int Radar_ScanCmd (ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   int         idx,v;
   static CONST char *sopt[] = { "read",NULL };
   enum                opt { READ };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",TCL_EXACT,&idx)!=TCL_OK) {
      return(Data_FieldCmd(clientData,Interp,Objc,Objv,TD_RADAR));
   }

   switch ((enum opt)idx) {
      case READ:
         if(Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id file scan");
            return(TCL_ERROR);
         }
         Tcl_GetIntFromObj(Interp,Objv[4],&v);
         return(Radar_Read(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3]),v));

         break;
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <Radar_FileGet>
 * Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer l'adresse de la structure du fichier correspondant
 *            a l'Id.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *
 * Retour:
 *  <Bool>    : Code de reussite
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
Radar_File* Radar_FileGet(Tcl_Interp *Interp,char *Id){

   Radar_File *file;

   if (!(file=(Radar_File*)TclY_HashGet(&Radar_FileTable,Id))) {
      if (Interp) {
         Tcl_AppendResult(Interp,"Radar_FileGet: Unknown file",(char *)NULL);
      }
   }
   return(file);
}

/*----------------------------------------------------------------------------
 * Nom      : <Radar_FileOpen>
 * Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'ouverture d'un fichier radar.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *  <Mode>    : Mode d'ouverture (R ou W)
 *  <Name>    : Non du fichier
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *  <tcl>     : Liste des parametres des enregistrements contenue dans le fichier
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Radar_FileOpen(Tcl_Interp *Interp,char *Id,char Mode,char *Name){

   Tcl_HashEntry *entry;
   Tcl_Obj       *obj;
   Radar_File    *file;
   int            new,v,sz;
   char          *buf=NULL;
   float         th[256];

   if (Mode!='r' && Mode!='R') {
      Tcl_AppendResult(Interp,"   Radar_FileOpen: Can only open in read mode for now",(char*)NULL);
      return TCL_ERROR;
   }

   /* Creer l'entree dans la liste table de fichiers standards */
   entry=TclY_CreateHashEntry(&Radar_FileTable,Id,&new);
   if (!new) {
      Tcl_AppendResult(Interp,"   Radar_FileOpen: File already opened, cannot reuse openned file identificator ",Id,(char*)NULL);
      return(TCL_ERROR);
   }

   if (!(file=(Radar_File*)malloc(sizeof(Radar_File)))) {
      Tcl_AppendResult(Interp,"   Radar_FileOpen: Unable to allocate memory",(char*)NULL);
      return(TCL_ERROR);
   }
   file->Mode=Mode;
   file->Name=strdup(Name);
   file->CId=strdup(Id);

   Tcl_SetHashValue(entry,file);

   /*Read in the data*/
   if (!(buf=readFile(Name,&sz))) {
      Tcl_AppendResult(Interp,"   Radar_FileOpen: Invalid radar file",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Decode the data*/
   if ((DecodeRadarData(buf,sz,&file->Data)==-1)) {
      Tcl_AppendResult(Interp,"   Radar_FileOpen: Unable to decode radar data",(char*)NULL);
      free(buf);
      return(TCL_ERROR);
   }

   if (file->Data.numScans<=0) {
      Tcl_AppendResult(Interp,"   Radar_FileOpen: No volume scans found in file",(char*)NULL);
      free(buf);
      return(TCL_ERROR);
   }

//   Radar_FileParse(&file->Data);
   for (v=0;v<file->Data.volScan[0]->numSweeps;v++) {            /*Loop on the Sweeps*/
      th[v]=file->Data.volScan[0]->sweep[v]->elevationAngle;
   }

   file->GRef=GeoRef_Find(GeoRef_RDRSetup(getLatitude(&file->Data),getLongitude(&file->Data),getGroundHeight(&file->Data)+getHornHeight(&file->Data),
       file->Data.volScan[0]->sweep[0]->maxNumBinsInSweep,file->Data.binResolutionKM*1000,file->Data.azimuthResolutionDegree));
   file->ZRef=ZRef_Define(LVL_ANGLE,file->Data.volScan[0]->numSweeps,th);

   obj=Tcl_NewListObj(0,NULL);
   for(v=0;v<file->Data.numScans;v++) {
       Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(Radar_GetTypeString(file->Data.volScan[v]->dataType),-1));
   }
   Tcl_SetObjResult(Interp,obj);
   free(buf);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Radar_FileClose>
 * Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Ferme le fichier standard a la RPN.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *
 * Retour:
 *  <TCL_...> : Code de reussite.
 *
 * Remarques :
 *
 *------------------------------------------------------------s----------------
*/
int Radar_FileClose(Tcl_Interp *Interp,char *Id){

   Radar_File *file=NULL;

   if ((file=(Radar_File*)TclY_HashDel(&Radar_FileTable,Id))) {
      FreeRadarData(&file->Data);
      GeoRef_Destroy(Interp,file->GRef->Name);
      free(file->Name);
      free(file->CId);
      free(file);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Radar_FileParse>
 * Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Parcourir une structure de donnees radar et en afficher les informations
 *            a l'Id.
 *
 * Parametres :
 *  <Data>    : Donnees d'un radar
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Radar_FileParse(RADAR_DATA *Data) {

   int v,s,r;

   for(v=0;v<Data->numScans;v++) {
      fprintf(stdout,"(DEBUG) Scan %i, MaxRay : %i\n",v,Data->volScan[v]->maxNumRaysInVolume);

      for(s=0;s<Data->volScan[v]->numSweeps;s++) {
         fprintf(stdout,"(DEBUG)    Sweep %i, MaxBin : %i    Elevation Angle : %f\n",s,Data->volScan[v]->sweep[s]->maxNumBinsInSweep,Data->volScan[v]->sweep[s]->elevationAngle);

         for(r=0;r<Data->volScan[v]->sweep[s]->numRays;r++) {
            fprintf(stdout,"(DEBUG)       Ray %i, Nb Bins : %i   Range : (%i - %i)   Azimuth : (%f - %f)\n",r,
               Data->volScan[v]->sweep[s]->rays[r]->numBins,
               Data->volScan[v]->sweep[s]->rays[r]->startRange,
               Data->volScan[v]->sweep[s]->rays[r]->endRange,
               Data->volScan[v]->sweep[s]->rays[r]->startAzimuth,
               Data->volScan[v]->sweep[s]->rays[r]->endAzimuth);
         }
      }
   }
}

#endif
