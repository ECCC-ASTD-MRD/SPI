/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture de fichiers d'observations SWOB
 * Fichier      : tclXML.c
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

#include "tclXML.h"

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <XML_CharHandler>
 * Creation     : Octobre 2012 J.P. Gauthier
 *
 * But          : Generic XML char handler for expath.
 *
 * Parametres   :
 *   <Data>     : XML opaque data pointer
 *   <Txt>      : Char stream (not null terminated)
 *   <Len>      : Char stream length
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void XML_CharHandler(void *Data,const char *Txt,int Len) {

   XML_Data *data=(XML_Data*)Data;

   if (Txt && Len) {
      if ((data->BufLen+Len+1)>data->BufRLen) {
         data->BufRLen=data->BufLen+Len+1;
         data->Buf=realloc(data->Buf,data->BufRLen);
      }
      memcpy(data->Buf+data->BufLen,Txt,Len);
      data->Buf[data->BufLen+Len]='\0';
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) XML_CharHandler: (%i) .%s.\n",data->BufLen,data->Buf);
#endif
      data->BufLen+=Len;
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <XML_CharReset>
 * Creation     : Octobre 2012 J.P. Gauthier
 *
 * But          : Clear internal char buffer
 *
 * Parametres   :
 *   <Data>     : XML opaque data pointer
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void XML_CharReset(void *Data) {

   XML_Data *data=(XML_Data*)Data;

   if (data->Buf && data->BufLen) {
      data->Buf[0]='\0';
      data->BufLen=0;
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <XML_ArrayCheck>
 * Creation     : Octobre 2012 J.P. Gauthier
 *
 * But          : Check number of item in char stream array.
 *
 * Parametres   :
 *   <Data>     : XML opaque data pointer
 *   <Sep>      : Array item separator char
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int XML_ArrayCheck(void *Data,char Sep) {

   XML_Data *data=(XML_Data*)Data;
   char *tok=NULL;
   long   n=0,s;

   if (data->Buf && data->BufLen) {

      // Get rid of carriage returns
      tok=data->Buf;
      s=(long)data->Buf+data->BufLen;

      while((long)tok<s) {
         if (*tok=='\n')
            *tok=' ';
         tok++;
      }

      // Count number of items
      n=0;
      tok=data->Buf;
      while((long)tok<s && (tok=strchr(tok,Sep))) {
         // Check for consecutive token
         if (*tok!=*(tok-1)) n++;
         tok++;
      }

#ifdef DEBUG
      fprintf(stdout,"(DEBUG) XML_ArrayCheck: Found %i items in array\n",n);
#endif
   }
   return(n);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <XML_ArrayExpandVect>
 * Creation     : Octobre 2012 J.P. Gauthier
 *
 * But          : Expand char stream into an array.
 *
 * Parametres   :
 *   <Data>     : XML opaque data pointer
 *   <Sep>      : Array item separator char
 *   <Array>    : Array
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int XML_ArrayExpandVect(void *Data,char Sep,float *Array) {

   XML_Data *data=(XML_Data*)Data;
   char *tok,*save=NULL;
   int   n;

   if (data->Buf && data->BufLen) {

      // Parse all tokens
      n=0;
      tok=strtok_r(data->Buf,&Sep,&save);
      while(tok) {
         Array[n++]=atof(tok);

         //Check for buffer overrun
         if (save>=(data->Buf+data->BufLen)-1)
            break;

         tok=strtok_r(NULL,&Sep,&save);
      }
   }

   return(n);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <XML_ParseFile>
 * Creation     : Octobre 2012 J.P. Gauthier
 *
 * But          : Parse an XML file
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Parser>   : XML parser
 *   <Data>     : Specific data object
 *   <Path>     : XML file path
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int XML_ParseFile(Tcl_Interp *Interp,XML_Parser Parser,void *Data,char *Path) {

   FILE     *file;
   XML_Data  data;
   void     *buf;
   int       len,state=1;

   if (!(file=fopen(Path,"r"))) {
      return(0);
   }

   /*Initialise expat XML parser*/
   memset(&data,0x0,sizeof(XML_Data));
   data.Interp=Interp;
   data.Specific=Data;
   data.Bloc=XML_INIT;

   XML_SetUserData(Parser,&data);
   XML_SetCharacterDataHandler(Parser,XML_CharHandler);

   /*Parse the XML by chunk*/
   for (;;) {
      if (!(buf=XML_GetBuffer(Parser,XML_BUFSIZE))) {
         fprintf(stderr,"(ERROR) XML_Parse: Could not allocate XML IO buffer\n");
         state=0;
         break;
      }

      len=fread(buf,1,XML_BUFSIZE,file);
      if (ferror(file)) {
         fprintf(stderr,"(ERROR) XML_Parse: Read error on %s\n",Path);
         state=0;
         break;
      }

      if (!XML_ParseBuffer(Parser,len,len==0)) {
         fprintf(stderr,"(ERROR) XML_Parse: Parse error at line %li:\n\t%s\n",XML_GetCurrentLineNumber(Parser),XML_ErrorString(XML_GetErrorCode(Parser)));
         state=0;
         break;
      }

      if (data.Bloc==XML_BAD) {
         fprintf(stderr,"(INFO) XML_Parse: Wrong file format\n");
         state=0;
         break;
      }

      if (!len)
         break;
   }

   fclose(file);

   return(state);
}
