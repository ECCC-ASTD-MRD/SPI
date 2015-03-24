/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de fichiers standards.
 * Fichier   : Radar_Scan.c
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

/*----------------------------------------------------------------------------
 * Nom      : <Radar_Set>
 * Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Determiner la validite d'un champ de par ses dimension ou en
 *            creer un nouveau si necessaire.
 *
 * Parametres     :
 *  <TData>       : Pointeur sur la donnee
 *
 * Retour:
 *  <Field>       : Champs valide pour les parametres demandees
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Radar_Set(TData *Data){

   if (Data->Head && Data->Free)
      Data->Free(Data);

   Data->Head=(Radar_Head*)malloc(sizeof(Radar_Head));

   Data->Type=TD_RADAR;
   Data->Set=Radar_Set;
   Data->Free=Radar_Free;
   Data->Copy=Radar_HeadCopy;
   Data->Grid=Radar_Grid;
   Data->ReadCube=NULL;
   Data->Define=Radar_ScanDefine;
}

/*----------------------------------------------------------------------------
 * Nom      : <Radar_ScanDefine>
 * Creation : Avril 2006 J.P. Gauthier
 *
 * But      : Definition des donnees'observations
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Name>    : Nom de la projection
 *  <Objc>    : Nombre d'arguments
 *  <Objv>    : Liste des arguments
 *
 * Retour     :
 *  <TCL_...> : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Radar_ScanDefine(Tcl_Interp *Interp,TData *Rad,int Objc,Tcl_Obj *CONST Objv[]){

   Radar_Head *head=(Radar_Head*)Rad->Head;
   Tcl_Obj    *obj;
   TGeoRef    *ref;
   int         i,idx,v=-1,date,time;
   char        buf[64];
   double      th;
   Coord       loc;

   static CONST char *sopt[] = { "-TYPE","-SCAN","-AZIMUTHRESOLUTION","-BINRESOLUTION","-SITEID","-SITENAME","-LOCATION",\
      "-SWEEPANGLE","-DATE","-NOMVAR","-PRODUCT","-NOISE","-FILTER","-ZCAL","-NYQUIST","-NBSWEEP","-NBAZIMUTH","-NBBIN","-GEOREF",NULL };
   enum                opt { TYPE,SCAN,AZRES,BNRES,SITEID,SITENAME,LOCATION,SWEEPANGLE,DATE,NOMVAR,PRODUCT,NOISE,FILTER,ZCAL,NYQUIST,NBSWEEP,NBAZIMUTH,NBBIN,GEOREF };

   for (i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case GEOREF:
            if (Objc==1) {
               if (Rad->Ref)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(Rad->Ref->Name,-1));
            } else {
               ref=GeoRef_Get(Tcl_GetString(Objv[++i]));
               if (!ref) {
                  Tcl_AppendResult(Interp,"\n   Radar_ScanDefine: Georef name unknown: \"",Tcl_GetString(Objv[i]),"\"",(char *)NULL);
                  return TCL_ERROR;
               }
               if (ref!=Rad->Ref) {
                  GeoRef_Destroy(Interp,Rad->Ref->Name);
                  Rad->Ref=ref;
                  GeoRef_Incr(Rad->Ref);
                  Data_Clean(Rad,1,1,1);
               }
            }
            break;

         case TYPE:
            if (Objc==1) {
               switch(head->Data->radarType) {
                  case IRIS   : Tcl_SetObjResult(Interp,Tcl_NewStringObj("IRIS",-1)); break;
                  case RDP    : Tcl_SetObjResult(Interp,Tcl_NewStringObj("RDP",-1)); break;
                  case NEXRAD : Tcl_SetObjResult(Interp,Tcl_NewStringObj("NEXRAD",-1)); break;
                  case MCGILL : Tcl_SetObjResult(Interp,Tcl_NewStringObj("MCGILL",-1)); break;
                  case KINGRDR: Tcl_SetObjResult(Interp,Tcl_NewStringObj("KINGRDR",-1)); break;
                  case KINGRDD: Tcl_SetObjResult(Interp,Tcl_NewStringObj("KINGRDD",-1)); break;
               }
            } else {
            }
            break;

         case NOMVAR:
         case SCAN:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Radar_GetTypeString(head->Data->volScan[head->Scan]->dataType),-1));
            } else {
            }
            break;

         case AZRES:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(head->Data->azimuthResolutionDegree));
            } else {
            }
            break;

         case BNRES:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(head->Data->binResolutionKM));
            } else {
            }
            break;

         case DATE:
            if (Objc==1) {
               strcpy(buf,getDateTime(head->Data));
               time=atoi(&buf[8])*100;
               buf[8]='\0';
               date=atoi(buf);
               Tcl_SetObjResult(Interp,Tcl_NewLongObj(System_DateTime2Seconds(date,time,1)));
            } else {
            }
            break;

         case SITEID:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(getSiteId(head->Data),-1));
            } else {
            }
            break;

         case SITENAME:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(getSiteName(head->Data),-1));
            } else {
            }
            break;

         case PRODUCT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(getMinorProductType(head->Data),-1));
            } else {
            }
            break;

         case NOISE:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(getNoise(head->Data)));
            } else {
            }
            break;

         case FILTER:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(getClutterFilter(head->Data)));
            } else {
            }
            break;

         case ZCAL:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(getZCal(head->Data)));
            } else {
            }
            break;

         case NYQUIST:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(getVNyquist(head->Data)));
            } else {
            }
            break;

         case SWEEPANGLE:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for(v=0;v<head->Data->volScan[head->Scan]->numSweeps;v++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(head->Data->volScan[head->Scan]->sweep[v]->elevationAngle));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
            }
            break;

         case NBSWEEP:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Rad->Def->NK));
            }
            break;

         case NBAZIMUTH:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Rad->Def->NI));
            }
            break;

         case NBBIN:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Rad->Def->NJ));
            }
            break;
         case LOCATION:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Rad->Ref->Loc.Lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Rad->Ref->Loc.Lon));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Rad->Ref->Loc.Elev));
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Objc!=4) {
                  Tcl_AppendResult(Interp,"\n   GeoRef_Define: Invalid location, must be 3 \"",(char*)NULL);
                  return TCL_ERROR;
               }
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc.Lat);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc.Lon);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&loc.Elev);

               if (loc.Lat!=Rad->Ref->Loc.Lat || loc.Lon!=Rad->Ref->Loc.Lon  || loc.Elev!=Rad->Ref->Loc.Elev) {
                  ref=Rad->Ref;
                  Rad->Ref=GeoRef_Find(GeoRef_RDRSetup(loc.Lat,loc.Lon,loc.Elev,Rad->Ref->R,Rad->Ref->ResR,Rad->Ref->ResA,Rad->Def->NK,Rad->Ref->ZRef.Levels));
                  GeoRef_Destroy(Interp,ref->Name);
                  Data_Clean(Rad,1,1,1);
                  th=DEG2RAD(head->Data->volScan[head->Scan]->sweep[Rad->Def->Level]->elevationAngle);
                  Rad->Ref->CTH=cos(th);
                  Rad->Ref->STH=sin(th);
               }
            }
            break;
      }
   }
   return TCL_OK;
}

void Radar_HeadCopy(void *To,void *From) {
   memcpy(To,From,sizeof(Radar_Head));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Radar_Free>
 * Creation     : Avril 2006 J.P. Gauthier
 *
 * But          : Liberation de la memoire associe a une donnee.
 *
 * Parametres   :
 *   <Rad>      : Radar
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void Radar_Free(TData *Rad) {

   Radar_Head *head=(Radar_Head*)Rad->Head;

   if (Rad && head)
      free(head);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Radar_Read>
 * Creation     : Avril 2006 J.P. Gauthier
 *
 * But          : Lire un scan de radar.
 *
 * Parametres   :
 *  <Interp>    : Interpreteur Tcl
 *  <Id>        : Identificateur du scan
 *  <File>      : Identificateur du radar
 *  <Scan>      : Index du scan dans le radar
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Radar_Read(Tcl_Interp *Interp,char *Id,char* File,int Scan) {

   Radar_File    *file;
   TData         *rad;
   int            ni,nj,nk;

   file=Radar_FileGet(Interp,File);
   if (!file) {
      Tcl_AppendResult(Interp,"\n   Radar_Read: Invalid radar file",(char*)NULL);
      return TCL_ERROR;
   }

   if (Scan<0 || Scan>=file->Data.numScans) {
      Tcl_AppendResult(Interp,"   Radar_Read: Invalid scan index",(char*)NULL);
      return TCL_ERROR;
   }

   ni=360/file->Data.azimuthResolutionDegree+1;
   nj=file->Data.volScan[0]->sweep[0]->maxNumBinsInSweep;
   nk=file->Data.volScan[0]->numSweeps;

   rad=Data_Valid(Interp,Id,ni,nj,nk,1,TD_Float32);
   Radar_Set(rad);
   ((Radar_Head*)rad->Head)->Data=&file->Data;
   ((Radar_Head*)rad->Head)->Scan=Scan;
   rad->Ref=GeoRef_Copy(file->Ref);

   rad->Spec->Desc=strdup(Radar_GetTypeString(file->Data.volScan[Scan]->dataType));

   Radar_Parse(rad);

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Radar_Parse>
 * Creation     : Avril 2006 J.P. Gauthier
 *
 * But          : Parcourir les donnees d'un scan de radar et les inserer dans notre structure TData.
 *
 * Parametres   :
 *  <Rad>       : Structure de donnees de radar scan
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Radar_Parse(TData *Rad) {

   Radar_Head *head=(Radar_Head*)Rad->Head;
   VOLUME     *vol;
   RAY        *ray;
   int         i,j,k,bin;
   double      val,th;

   vol=head->Data->volScan[head->Scan];

   for (k=0;k<Rad->Def->NK;k++) {            /*Loop on the Sweeps*/
      for (j=0;j<Rad->Def->NJ;j++) {            /*Loop on the Bins*/
         for (i=0;i<Rad->Def->NI;i++) {           /*Loop on the Azimuths*/

            if (i==Rad->Def->NI-1) {
               Def_Get(Rad->Def,0,FIDX3D(Rad->Def,0,j,k),val);
               Def_Set(Rad->Def,0,FIDX3D(Rad->Def,i,j,k),val);
            } else {
               ray=vol->sweep[k]->rays[i];
               bin=(ray->endRange-ray->startRange)/ray->numBins;
               bin=j*head->Data->binResolutionKM;
               bin=bin>ray->numBins?-1:bin;

               val=((unsigned char*)ray->rangeBin)[bin];
               val=N_DBZ(val);
               Def_Set(Rad->Def,0,FIDX3D(Rad->Def,i,j,k),val);
            }
         }
      }
   }

   th=DEG2RAD(vol->sweep[Rad->Def->Level]->elevationAngle);
   Rad->Ref->CTH=cos(th);
   Rad->Ref->STH=sin(th);
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Radar_Grid>
 * Creation     : Avril 2006 J.P. Gauthier
 *
 * But          : Extraire la grille a partir des rays,, sweeps et scan d'un radar.
 *
 * Parametres   :
 *  <Rad>       : Structure de donnees de radar scan
 *  <Proj>      : Projection
 *  <Level>     : Niveau a calculer
 *
 * Retour       :
 *  <Vect3d*>   : Pointeur sur les positions (NULL si invalide)
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
Vect3d* Radar_Grid(TData *Rad,void *Proj,int Level) {

   Radar_Head *head=(Radar_Head*)Rad->Head;
   VOLUME     *vol;
   Coord      coord;
   double     az,dt,th,sth;
   int        i,j,idxi;

   if (Rad->Ref->Pos && Rad->Ref->Pos[Level])
      return(Rad->Ref->Pos[Level]);

   /*Allocate memory for various levels*/
   if (!Rad->Ref->Pos)
      Rad->Ref->Pos=(Vect3d**)calloc(Rad->Ref->ZRef.LevelNb,sizeof(Vect3d*));

   if (!Rad->Ref->Pos[Level]) {
      Rad->Ref->Pos[Level]=(Vect3d*)malloc(FSIZE2D(Rad->Def)*sizeof(Vect3d));
      if (!Rad->Ref->Pos[Level]) {
         fprintf(stderr,"(ERROR) FSTD_Grid: Not enough memory to calculate gridpoint location");
         return(NULL);
      }
   }
   vol=head->Data->volScan[0];

   if (Rad->Ref->Grid[0]=='V') {
      for (j=0;j<Rad->Def->NJ;j++) {
         idxi=j*Rad->Def->NI;
         th=DEG2RAD(Rad->Ref->ZRef.Levels[j]);
         sth=sin(th);
         for (i=0;i<Rad->Def->NI;i++) {
            coord.Lat=Rad->Ref->Lat[i];
            coord.Lon=CLAMPLON(Rad->Ref->Lon[i]);
            Rad->Ref->RefFrom->UnProject(Rad->Ref->RefFrom,&az,&dt,coord.Lat,coord.Lon,1,0);
            coord.Elev=Rad->Ref->Loc.Elev+sth*dt;
            if (Proj) {
               ((Projection*)Proj)->Type->Project(((Projection*)Proj),(GeoVect*)&coord,(GeoVect*)&Rad->Ref->Pos[Level][idxi],1);
            } else {
               Vect_Init(Rad->Ref->Pos[Level][idxi],Rad->Ref->Lat[i],Rad->Ref->Lon[i],coord.Elev);
            }
            idxi++;
         }
      }
   } else {
      th=DEG2RAD(vol->sweep[Level]->elevationAngle);
      sth=sin(th);
      for (j=0;j<Rad->Def->NJ;j++) {            /*Loop on the Bins*/
         idxi=j*Rad->Def->NI;
         for (i=0;i<Rad->Def->NI;i++,idxi++) {           /*Loop on the Azimuths*/

            /*Figure out table plane indexes*/
            if (i==Rad->Def->NI-1) {
               Vect_Assign(Rad->Ref->Pos[Level][idxi],Rad->Ref->Pos[Level][j*Rad->Def->NI]);
            } else {
               dt=j*head->Data->binResolutionKM*1000;
               Rad->Ref->Pos[Level][idxi][2]=Rad->Ref->Loc.Elev+sth*dt;
               Rad->Ref->Project(Rad->Ref,i,j,&Rad->Ref->Pos[Level][idxi][1],&Rad->Ref->Pos[Level][idxi][0],0,1);
            }
         }
      }
      ((Projection*)Proj)->Type->Project(((Projection*)Proj),(GeoVect*)Rad->Ref->Pos[Level],NULL,FSIZE2D(Rad->Def));
   }
   return(Rad->Ref->Pos[Level]);
}
#endif
