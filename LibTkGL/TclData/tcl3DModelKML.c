/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Affichage de modele 3D
 * Fichier      : tcl3DModelKML.c
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

#include "tcl3DModel.h"
#include "tclXML.h"

typedef struct KML_Data {
   char        *Path;
   char        *Buf;
   unsigned int BufLen;
   char         AltMode;
   T3DModel    *Model;

} KML_Data;

void ModelKML_StartHandler(void *Data,const char *Elem,const char **Attr) {

   XML_Data  *data=(XML_Data*)Data;
   KML_Data  *kml=(KML_Data*)data->Specific;

   XML_CharReset(Data);
   XML_Check(Data,Elem,"kml");

   if (Elem && XML_Valid(Data)) {
   }
}

void ModelKML_EndHandler(void *Data,const char *Elem) {

   XML_Data  *data=(XML_Data*)Data;
   KML_Data  *kml=(KML_Data*)data->Specific;
   char      *buf,*c;

  if (Elem && XML_Valid(Data)) {
      if (strcmp(Elem,"name")==0) {
         kml->Model->Name=strdup(data->Buf);
      } else

      if (strcmp(Elem,"altitudeMode")==0) {
         kml->AltMode=data->Buf[10];
      } else

      if (strcmp(Elem,"longitude")==0) {
         kml->Model->Pos[1]=atof(data->Buf);
      } else

      if (strcmp(Elem,"latitude")==0) {
         kml->Model->Pos[0]=atof(data->Buf);
      } else

      if (strcmp(Elem,"altitude")==0) {
         kml->Model->Pos[2]=atof(data->Buf);
      } else

      if (strcmp(Elem,"heading")==0) {
         kml->Model->MatrixR[0]=atof(data->Buf);
      } else

      if (strcmp(Elem,"tilt")==0) {
         kml->Model->MatrixR[1]=atof(data->Buf);
      } else

      if (strcmp(Elem,"roll")==0) {
         kml->Model->MatrixR[2]=atof(data->Buf);
      } else

      if (strcmp(Elem,"x")==0) {
         kml->Model->MatrixS[0]=atof(data->Buf);
      } else

      if (strcmp(Elem,"y")==0) {
         kml->Model->MatrixS[1]=atof(data->Buf);
      } else

      if (strcmp(Elem,"z")==0) {
         kml->Model->MatrixS[2]=atof(data->Buf);
      } else

      if (strcmp(Elem,"href")==0) {
         buf=(char*)malloc(strlen(kml->Path)+data->BufLen+1);
         strcpy(buf,kml->Path);
         if (c=strrchr(buf,'/')) {
            strcpy(c+1,data->Buf);
#ifdef DEBUG
            fprintf(stderr,"(DEBUG) ModelKML_EndHandler: Found Collada external ref %s\n",buf);
#endif
         }
         Model_LoadDAE(data->Interp,kml->Model,buf);
         free(buf);
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_LoadKML>
 * Creation     : Janvier 2003 J.P. Gauthier
 *
 * But          : Lire un fichier KML (pour lire le Collada en href)
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <M>        : Objet Model
 *   <Path>     : Path du fichier
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
 int Model_LoadKML(Tcl_Interp* Interp,T3DModel *M,char *Path) {

   XML_Parser  parser;
   KML_Data   *kml;
   int         state=1;

   /*Create expat XML parser*/
   if (!(parser=XML_ParserCreate(NULL))) {
      fprintf(stderr,"(ERROR) Model_LoadKML: Couldn't initiate XML parser\n");
      return(0);
   }

   /*Data to be used while parsing*/
   kml=(KML_Data*)malloc(sizeof(KML_Data));
   kml->Model=M;
   kml->AltMode='G';
   kml->Path=Path;

   /*Initialise expat XML parser*/
   XML_SetElementHandler(parser,ModelKML_StartHandler,ModelKML_EndHandler);

   /*Parse the XML*/
   state=XML_ParseFile(Interp,parser,kml,Path);
   XML_ParserFree(parser);

   /*Free associates parsing data structure*/
   free(kml);

   return(state);
}
