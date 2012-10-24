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

#include "tclMetObs.h"
#include "tclXML.h"

#define SWOB_NIL          0
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
   Coord         Co;
   time_t        Sec;
} SWOB_Data;

void SWOB_StartHandler(void *Data,const char *Elem,const char **Attr) {

   XML_Data    *data=(XML_Data*)Data;
   SWOB_Data   *swob=(SWOB_Data*)data->Specific;
   BUFR_Tables *table;
   EntryTableB *eb;
   Tcl_Obj     *obj;
   int          i;
   char        *name,*uom,*value,multi=0;

   XML_CharReset(Data);
   obj=Tcl_NewIntObj(0);

   name=uom=value=NULL;

   if (Elem) {
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
            if (strcmp(Attr[i],"name")==0)      name=Attr[i+1];
            if (strcmp(Attr[i],"uom")==0)       uom=Attr[i+1];
            if (strcmp(Attr[i],"value")==0)     value=Attr[i+1];
         }
      } else if (strcmp(Elem,"qualifier")==0) {
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"code-src")==0)  { }
            if (strcmp(Attr[i],"code-type")==0) { }
            if (strcmp(Attr[i],"group")==0)     { }
            if (strcmp(Attr[i],"orig-name")==0) { }
            if (strcmp(Attr[i],"name")==0)      { }
            if (strcmp(Attr[i],"uom")==0)       { }
            if (strcmp(Attr[i],"value")==0)     { }
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

      if (name && value) {
          switch (data->Bloc) {
            case SWOB_IDELEMENTS:
               if (strcmp(name,"wmo_station_id")==0 || strcmp(name,"stn_name")==0) swob->Name=strdup(value);
               if (strcmp(name,"wmo_station_number")==0 ||strcmp(name,"stn_id")==0 || strcmp(name,"icao_stn_id")==0) swob->Id=strdup(value);
               if (strcmp(name,"lat")==0) swob->Co.Lat=atof(value);
               if (strcmp(name,"long")==0) swob->Co.Lon=atof(value);
               if (strcmp(name,"stn_elev")==0) swob->Co.Elev=atof(value);
               break;

            case SWOB_ELEMENTS:
               // Check if element exists
               if (!(eb=MetObs_BUFRFindTableDesc(name))) {
                  table=MetObs_GetTables();
                  eb=bufr_new_EntryTableB();

                  eb->descriptor=SWOB_DESCRIPTOR++;
                  eb->description=strdup(name);
                  eb->unit=strdup(uom);
                  arr_add(table->master.tableB,(char*)&eb);
               }
               // Add element to list of obs elements
               Tcl_SetIntObj(obj,eb->descriptor);
               if (TclY_ListObjFind(data->Interp,swob->Obs->Elems,obj)==-1) {
                  Tcl_ListObjAppendElement(data->Interp,swob->Obs->Elems,Tcl_DuplicateObj(obj));
               }
               TMetElemData_Resize(swob->EData,swob->EData?swob->EData->Ne+1:1,1,1);
               swob->EData->Data[swob->EData->Ne]=atof(value);
               swob->EData->Code[swob->EData->Ne]=eb;
               swob->EData->Ne++;
               break;
         }
      }
   }
}

void SWOB_EndHandler(void *Data,const char *Elem) {

   XML_Data  *data=(XML_Data*)Data;
   SWOB_Data *swob=(SWOB_Data*)data->Specific;
   char      *buf,*c;
   Vect3f     vf;

   if (Elem) {
      if (strcmp(Elem,"elements")==0) {
          switch (data->Bloc) {
            case SWOB_IDELEMENTS:
               break;

            case SWOB_ELEMENTS:
               TMetElem_Add(swob->Loc,swob->EData,swob->Sec);
               break;
         }
         data->Bloc=SWOB_NIL;

      } else if (strcmp(Elem,"gml:pos")==0) {
         // Get the station coordinates
         if (XML_ArrayCheck(data->Buf,data->BufLen,' ')) {
            XML_ArrayExpandVect(data->Buf,data->BufLen,' ',2,&vf);
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
 * But          : Chargement d'un fichier d'observations en format BUFR.
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

   FILE      *file;
   XML_Parser parser;
   XML_Data   data;
   SWOB_Data *swob;
   int        len,state=TCL_OK;
   void      *buf;

   /*Create expat XML parser*/
   parser=XML_ParserCreate(NULL);
   if (!parser) {
      fprintf(stderr,"(ERROR) MetObs_LoadSWOB: Couldn't initiate XML parser\n");
      return(0);
   }

   if (!(file=fopen(File,"r"))) {
      return(0);
   }

   /*Data to be used while parsing*/
   swob=(SWOB_Data*)calloc(1,sizeof(SWOB_Data));
   swob->Obs=Obs;

   memset(&data,0x0,sizeof(XML_Data));
   data.Interp=Interp;
   data.Specific=swob;

   /*Initialise expat XML parser*/
   XML_SetUserData(parser,&data);
   XML_SetElementHandler(parser,SWOB_StartHandler,SWOB_EndHandler);
   XML_SetCharacterDataHandler(parser,XML_CharHandler);

   /*Parse the XML by chunk*/
   for (;;) {
      if (!(buf=XML_GetBuffer(parser,XMLBUFSIZE))) {
         fprintf(stderr,"(ERROR) MetObs_LoadSWOB: Could not allocate XML IO buffer\n");
         state=TCL_ERROR;
         break;
      }

      len=fread(buf,1,XMLBUFSIZE,file);
      if (ferror(file)) {
         fprintf(stderr,"(ERROR) MetObs_LoadSWOB: Read error on %s\n",File);
         state=TCL_ERROR;
         break;
      }

      if (!XML_ParseBuffer(parser,len,len==0)) {
         fprintf(stderr,"(ERROR) MetObs_LoadSWOB: XML Parse error at line %li:\n\t%s\n",XML_GetCurrentLineNumber(parser),XML_ErrorString(XML_GetErrorCode(parser)));
         state=TCL_ERROR;
         break;
      }

      if (!len)
         break;
    }

    XML_ParserFree(parser);
    free(swob);
    fclose(file);

    return(state);
}
