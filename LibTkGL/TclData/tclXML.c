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
 *   <Txt>      : Char stream
 *   <Len>      : Char stream length
 *   <Sep>      : Array item separator char
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int XML_ArrayCheck(char *Txt,unsigned int Len,char Sep) {

   char *tok=NULL;
   int   n=0;

   if (Txt && Len) {
      // Get rid of carriage returns
      tok=Txt;
      while(tok<Txt+Len) {
         if (*tok=='\n')
            *tok=' ';
         tok++;
      }

      // Count number of items
      n=1;
      tok=Txt;
      while(tok<(Txt+Len) && (tok=strchr(tok,Sep))) {
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
 *   <Txt>      : Char stream
 *   <Len>      : Char stream length
 *   <Sep>      : Array item separator char
 *   <Dim>      : Item dimensions (nb coordinates per item)
 *   <Array>    : Array
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int XML_ArrayExpandVect(char *Txt,unsigned int Len,char Sep,int Dim,Vect3f *Array) {

   char *tok,*save=NULL;
   int   i,n;

   if (Txt && Len) {

      // Parse all tokens
      n=0;
      i=0;
      tok=strtok_r(Txt,&Sep,&save);
      while(tok) {
         Array[n][i++]=atof(tok);

         if (i==Dim) {
            i=0;
            n++;
         }

         //Check for buffer overrun
         if (save>=(Txt+Len)-1)
            break;
         tok=strtok_r(NULL,&Sep,&save);
      }
   }

   return(n);
}
