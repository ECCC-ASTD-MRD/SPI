/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture de fichiers d'observations SWOB
 * Fichier      : tclMetObs_SWOB.c
 * Creation     : Octobre 2012 - J.P. Gauthier
 *
 * Description  : Fichier d'entete du module Obs.
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
#ifdef HAVE_ECBUFR

#include "App.h"
#include "tclMetObs.h"
#include "tclXML.h"

#define SWOB_ELEMENTS     1
#define SWOB_IDELEMENTS   2
#define SWOB_SAMPLINGTIME 3
#define SWOB_RESULTTIME   4

static int SWOB_DESCRIPTOR=10000000;

typedef struct SWOB_Data {
   TMetObs      *Obs;
   TMetLoc      *Loc;
   TMetElemData *EData;
   char         *Id,*Name;
   char         *Ename,*Euom,*Evalue;
   Coord         Co;
   time_t        Sec;
} SWOB_Data;

void SWOB_StartHandler(void *Data,const char *Elem,const char **Attr) {

   XML_Data    *data=(XML_Data*)Data;
   SWOB_Data   *swob=(SWOB_Data*)data->Specific;
   int          i;
   char        *qname,*quom,*qvalue,multi=0;

   XML_CharReset(Data);
   XML_Check(Data,Elem,"om:ObservationCollection");

   qname=quom=qvalue=NULL;

   if (Elem && XML_Valid(Data)) {
      if (strcmp(Elem,"identification-elements")==0) {
         data->Bloc=SWOB_IDELEMENTS;
         if (swob->Name) free(swob->Name); swob->Name=NULL;
         if (swob->Id)   free(swob->Id);   swob->Id=NULL;

      } else if (strcmp(Elem,"elements")==0) {
         data->Bloc=SWOB_ELEMENTS;
         swob->EData=TMetElemData_New(0,1,1);

      } else if (strcmp(Elem,"element")==0) {
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"code-src")==0)  { }
            if (strcmp(Attr[i],"code-type")==0) { }
            if (strcmp(Attr[i],"group")==0)     { }
            if (strcmp(Attr[i],"orig-name")==0) { }
            if (strcmp(Attr[i],"name")==0)      swob->Ename=strdup(Attr[i+1]);
            if (strcmp(Attr[i],"uom")==0)       swob->Euom=strdup(Attr[i+1]);
            if (strcmp(Attr[i],"value")==0)     swob->Evalue=strdup(Attr[i+1]);
         }
      } else if (strcmp(Elem,"qualifier")==0) {
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"code-src")==0)  { }
            if (strcmp(Attr[i],"code-type")==0) { }
            if (strcmp(Attr[i],"group")==0)     { }
            if (strcmp(Attr[i],"orig-name")==0) { }
            if (strcmp(Attr[i],"name")==0)      qname=(char*)Attr[i+1];
            if (strcmp(Attr[i],"uom")==0)       quom=(char*)Attr[i+1];
            if (strcmp(Attr[i],"value")==0)     qvalue=(char*)Attr[i+1];
         }
      } else if (strcmp(Elem,"om:result")==0) {
         if (!swob->Name) swob->Name=strdup("NIL");
         if (!swob->Id)   swob->Id=strdup("NIL");
         swob->Loc=TMetLoc_FindWithCoord(swob->Obs,NULL,swob->Name,swob->Co.Lat,swob->Co.Lon,swob->Co.Elev,MET_TYPEID,&multi);
         if (!swob->Loc) {
            swob->Loc=TMetLoc_New(swob->Obs,swob->Name,swob->Id,swob->Co.Lat,swob->Co.Lon,swob->Co.Elev);
         }
      } else if (strcmp(Elem,"om:samplingTime")==0) {
         data->Bloc=SWOB_SAMPLINGTIME;
      } else if (strcmp(Elem,"om:resultTime")==0) {
         data->Bloc=SWOB_RESULTTIME;
      }

      if (swob->Ename && swob->Evalue) {
          switch (data->Bloc) {
            case SWOB_IDELEMENTS:
               if (strcmp(swob->Ename,"wmo_station_id")==0 || strcmp(swob->Ename,"stn_nam")==0) swob->Name=strdup(swob->Evalue);
               if (strcmp(swob->Ename,"wmo_station_number")==0 ||strcmp(swob->Ename,"stn_id")==0 || strcmp(swob->Ename,"clim_id")==0  || strcmp(swob->Ename,"msc_id")==0 || strcmp(swob->Ename,"icao_stn_id")==0) swob->Id=strdup(swob->Evalue);
               if (strcmp(swob->Ename,"lat")==0) swob->Co.Lat=atof(swob->Evalue);
               if (strcmp(swob->Ename,"long")==0) swob->Co.Lon=atof(swob->Evalue);
               if (strcmp(swob->Ename,"stn_elev")==0) swob->Co.Elev=atof(swob->Evalue);
               break;

            case SWOB_ELEMENTS:
                if (qname && strcmp(qname,"statistical_significance")==0) {
                   swob->Ename=strcatalloc(swob->Ename,":");
                   swob->Ename=strcatalloc(swob->Ename,qvalue);
                }
                break;
         }
      }
   }
}

void SWOB_EndHandler(void *Data,const char *Elem) {

   XML_Data    *data=(XML_Data*)Data;
   SWOB_Data   *swob=(SWOB_Data*)data->Specific;
   BUFR_Tables *table;
   EntryTableB *eb;
   Tcl_Obj     *obj;
   Vect3f       vf;

   if (Elem && XML_Valid(Data)) {
      if (strcmp(Elem,"elements")==0) {
         TMetElem_Add(swob->Loc,swob->EData,swob->Sec);
         data->Bloc=XML_NIL;

      } else if (strcmp(Elem,"element")==0) {
         switch (data->Bloc) {
            case SWOB_IDELEMENTS:
               break;

            case SWOB_ELEMENTS:
               // Check if element exists
               if (!(eb=MetObs_BUFRFindTableDesc(swob->Ename))) {
                  table=MetObs_GetTables();
                  eb=bufr_new_EntryTableB();

                  eb->descriptor=SWOB_DESCRIPTOR++;
                  eb->description=strdup(swob->Ename);
                  eb->unit=strdup(swob->Euom);
                  arr_add(table->master.tableB,(char*)&eb);
               }
               // Add element to list of obs elements
               obj=Tcl_NewIntObj(eb->descriptor);
               if (TclY_ListObjFind(data->Interp,swob->Obs->Elems,obj)==-1) {
                  Tcl_ListObjAppendElement(data->Interp,swob->Obs->Elems,Tcl_DuplicateObj(obj));
               }
               Tcl_DecrRefCount(obj);

               TMetElemData_Resize(swob->EData,swob->EData?swob->EData->Ne+1:1,1,1);
               swob->EData->Data[swob->EData->Ne]=atof(swob->Evalue);
               swob->EData->Code[swob->EData->Ne]=eb;
               swob->EData->Ne++;
               break;
         }
         if (swob->Ename)  free(swob->Ename);
         if (swob->Euom)   free(swob->Euom);
         if (swob->Evalue) free(swob->Evalue);

      } else if (strcmp(Elem,"gml:pos")==0) {
         // Get the station coordinates
         if (XML_ArrayCheck(data,' ')) {
            XML_ArrayExpandVect(data,' ',(float*)&vf);
            swob->Co.Lat=vf[0];
            swob->Co.Lon=vf[1];
         }

      } else if (strcmp(Elem,"gml:timePosition")==0 && data->Bloc==SWOB_SAMPLINGTIME) {
         // Split the IS8601 format by inserting end of string char
         data->Buf[4]=data->Buf[7]=data->Buf[10]=data->Buf[13]=data->Buf[16]=data->Buf[19]='\0';
         swob->Sec=System_DateTime2Seconds(atoi(&data->Buf[0])*10000+atoi(&data->Buf[5])*100+atoi(&data->Buf[8]),atoi(&data->Buf[11])*10000+atoi(&data->Buf[14])*100+atoi(&data->Buf[17]),1);
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_LoadSWOB>
 * Creation     : Mars 2008 J.P. Gauthier
 *
 * But          : Chargement d'un fichier d'observations en format SWOB.
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <File>     : Le nom du fichier
 *   <Obs>      : Observation
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int MetObs_LoadSWOB(Tcl_Interp *Interp,char *File,TMetObs *Obs) {

   XML_Parser parser;
   SWOB_Data *swob;
   int        state=1;

   /*Create expat XML parser*/
   if (!(parser=XML_ParserCreate(NULL))) {
      App_Log(ERROR,"%s: Couldn't initiate XML parser\n",__func__);
      return(0);
   }

   /*Data to be used while parsing*/
   swob=(SWOB_Data*)calloc(1,sizeof(SWOB_Data));
   swob->Obs=Obs;

   /*Initialise expat XML parser*/
   XML_SetElementHandler(parser,SWOB_StartHandler,SWOB_EndHandler);

   /*Parse the XML*/
   state=XML_ParseFile(Interp,parser,swob,File);

   XML_ParserFree(parser);
   free(swob);

   return(state?TCL_OK:TCL_ERROR);
}

#endif