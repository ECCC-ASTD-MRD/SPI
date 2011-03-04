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
#include "tcl3DModelKML.h"
#include <string.h>
#include <expat.h>

void ModelKML_StartHandler(void *Data,const char *Elem,const char **Attr) {

   KMLData   *data=(KMLData*)Data;

   if (Elem) {
      strncpy(data->Tag,Elem,255);

      if (data->Buf) {
         free(data->Buf);
         data->Buf=NULL;
         data->BufLen=0;
      }
   }
}

void ModelKML_EndHandler(void *Data,const char *Elem) {

   KMLData   *data=(KMLData*)Data;
   char      *buf,*c;

   if (Elem) {
      data->Tag[0]='\0';

      if (strcmp(Elem,"name")==0) {
         data->Model->Name=data->Buf;
         data->Buf=NULL;
         data->BufLen=0;
      } else

      if (strcmp(Elem,"altitudeMode")==0) {
         data->AltMode=data->Buf[10];
      } else

      if (strcmp(Elem,"longitude")==0) {
         data->Model->Pos[1]=atof(data->Buf);
      } else

      if (strcmp(Elem,"latitude")==0) {
         data->Model->Pos[0]=atof(data->Buf);
      } else

      if (strcmp(Elem,"altitude")==0) {
         data->Model->Pos[2]=atof(data->Buf);
      } else

      if (strcmp(Elem,"heading")==0) {
         data->Model->MatrixR[0]=atof(data->Buf);
      } else

      if (strcmp(Elem,"tilt")==0) {
         data->Model->MatrixR[1]=atof(data->Buf);
      } else

      if (strcmp(Elem,"roll")==0) {
         data->Model->MatrixR[2]=atof(data->Buf);
      } else

      if (strcmp(Elem,"x")==0) {
         data->Model->MatrixS[0]=atof(data->Buf);
      } else

      if (strcmp(Elem,"y")==0) {
         data->Model->MatrixS[1]=atof(data->Buf);
      } else

      if (strcmp(Elem,"z")==0) {
         data->Model->MatrixS[2]=atof(data->Buf);
      } else

      if (strcmp(Elem,"href")==0) {
         buf=(char*)malloc(strlen(data->Path)+data->BufLen+1);
         strcpy(buf,data->Path);
         if (c=strrchr(buf,'/')) {
            strcpy(c+1,data->Buf);
#ifdef DEBUG
            fprintf(stderr,"(DEBUG) ModelKML_EndHandler: Found Collada external ref %s\n",buf);
#endif
         }
         Model_LoadDAE(data->Model,buf);
         free(buf);
      }
   }
}

void ModelKML_CharHandler(void *Data,const char *Txt,int Len) {

   KMLData *data=(KMLData*)Data;

   if (Txt && Len) {
      data->Buf=realloc(data->Buf,data->BufLen+Len+1);
      memcpy(data->Buf+data->BufLen,Txt,Len);
      data->Buf[data->BufLen+Len]='\0';
      data->BufLen+=Len;
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_LoadKML>
 * Creation     : Janvier 2003 J.P. Gauthier
 *
 * But          : Lire un fichier KML (pour lire le Collada en href)
 *
 * Parametres   :
 *   <M>        : Objet Model
 *   <Path>     : Path du fichier
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Model_LoadKML(T3DModel *M,char *Path) {

   FILE      *file;
   XML_Parser parser;
   KMLData   *data;
   int        len,state=1;
   void      *buf;

   /*Create expat XML parser*/
   parser=XML_ParserCreate(NULL);
   if (!parser) {
      fprintf(stderr,"(ERROR) Model_LoadKML: Couldn't initiate XML parser\n");
      return(0);
   }

   if (!(file=fopen(Path,"r"))) {
      return(0);
   }

   /*Data to be used while parsing*/
   data=(KMLData*)malloc(sizeof(KMLData));
   data->Model=M;
   data->Path=Path;
   data->Buf=NULL;
   data->BufLen=0;
   data->AltMode='G';

   /*Initialise expat XML parser*/
   XML_SetUserData(parser,data);
   XML_SetElementHandler(parser,ModelKML_StartHandler,ModelKML_EndHandler);
   XML_SetCharacterDataHandler(parser,ModelKML_CharHandler);

   /*Parse the XML by chunk*/
   for (;;) {
      if (!(buf=XML_GetBuffer(parser,XMLBUFSIZE))) {
         fprintf(stderr,"(ERROR) Model_LoadKML: Could not allocate XML IO buffer\n");
         state=0;
         break;
      }

      len=fread(buf,1,XMLBUFSIZE,file);
      if (ferror(file)) {
         fprintf(stderr,"(ERROR) Model_LoadKML: Read error on %s\n",Path);
         state=0;
         break;
      }

      if (!XML_ParseBuffer(parser,len,len==0)) {
         fprintf(stderr,"(ERROR) Model_LoadKML: XML Parse error at line %d:\n\t%s\n",XML_GetCurrentLineNumber(parser),XML_ErrorString(XML_GetErrorCode(parser)));
         state=0;
         break;
      }

      if (!len)
         break;
    }
    XML_ParserFree(parser);

    /*Free associates parsing data structure*/
    if (data->Buf) free(data->Buf);
    free(data);

    fclose(file);
    return(state);
}
