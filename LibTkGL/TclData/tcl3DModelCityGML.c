/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Affichage de modele 3D
 * Fichier      : tcl3DModelCityGML.c
 * Creation     : Juillet 2012 - J.P. Gauthier
 *
 * Description  : Module de lecture de modele 3D en format CityGML
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
#include "tcl3DModelCityGML.h"
#include <string.h>
#include <expat.h>

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

void ModelCityGML_StartHandler(void *Data,const char *Elem,const char **Attr) {

   CityGMLData *data=(CityGMLData*)Data;
   T3DScene    *scn,*child;
   T3DObject   *obj=NULL;
   char         dim;
   int          i;

   if (data->Buf) {
      data->Buf[0]='\0';
      data->BufLen=0;
   }

   if (Elem) {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) ModelCityGML_StartHandler: Token %s\n",Elem);
#endif

      strncpy(data->Tag,Elem,255);

      if (strcmp(Elem,"cityObjectMember")==0) {
//         data->Scene=Model_SceneAdd(data->Model,data->Scene,1);
      } else
fprintf(stderr,"=== %s\n",Elem);
      if (strcmp(Elem,"bldg:Building")==0 || strcmp(Elem,"citygml:Building")==0 || strcmp(Elem,"bldg:BuildingInstallation")==0 || strcmp(Elem,"dem:TINRelief")==0 ||
         strcmp(Elem,"trans:Road")==0) {
         data->Object=Model_ObjectAdd(data->Model,1);
         data->Object->NFc=0;
         data->Object->NVr=0;

         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"gml:id")==0) {
               data->Object->Name=strndup(Attr[i+1],256);
            }
         }
      } else

      if (strcmp(Elem,"gml:Envelope")==0) {
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"srsName")==0) {
               data->Model->Ref=GeoRef_New();
               GeoRef_WKTSet(data->Model->Ref,Attr[i+1],NULL,NULL,NULL);
            }
         }
         data->Env=1;
      } else

      if (strcmp(Elem,"gml:Polygon")==0 || strcmp(Elem,"gml:Triangle")==0) {
         if (!data->Object) {
            fprintf(stdout,"(ERROR) No object defined\n");
         }
         data->Fc=Model_ObjectFaceAdd(data->Object,1);

         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"gml:id")==0) {
               data->Fc->Name=strndup(Attr[i+1],256);
            }
         }
      } else

      if (strcmp(Elem,"gml:posList")==0) {
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"srsDimension")==0) {
               data->VrDim=atoi(Attr[i+1]);
            }
         }
      } else

      if (strcmp(Elem,"app:target")==0) {
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"uri")==0) {
               if ((data->Fc=Model_FaceFind(data->Model,&Attr[i+1][1],&obj))) {
                  data->Object=obj;
                  data->Fc->Mt=data->Mt;
               }
            }
         }
      } else

      if (strcmp(Elem,"app:surfaceDataMember")==0) {
         data->Mt=Model_MaterialAdd(data->Model,1);
      }
   }
}

void ModelCityGML_EndHandler(void *Data,const char *Elem) {

   CityGMLData *data=(CityGMLData*)Data;
   T3DObject   *obj=NULL;
   TFace       *fc=NULL;
   char        *buf,*c;
   int          n,i;

   if (Elem) {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) ModelCityGML_EndHandler: Token %s\n",Elem);
#endif
      data->Tag[0]='\0';

      if (strcmp(Elem,"gml:Envelope")==0) {
         data->Env=0;
      } else

      if (strcmp(Elem,"gml:posList")==0 || strcmp(Elem,"gml:pos")==0) {
         if (!data->Env && data->Object && (n=XML_ArrayCheck(data->Buf,data->BufLen,' '))) {
            n/=3;
            data->Object->Vr=realloc(data->Object->Vr,(data->Object->NVr+n)*sizeof(Vect3f));
            XML_ArrayExpandVect(data->Buf,data->BufLen,' ',3,&data->Object->Vr[data->Object->NVr]);
            data->Object->NVr+=n;
            data->Fc->NIdx+=n;
         }
      } else

      if (strcmp(Elem,"gml:LinearRing")==0) {
         data->Fc->Idx=(unsigned int*)malloc(data->Fc->NIdx*sizeof(unsigned int));
         for(n=0;n<data->Fc->NIdx;n++) {
            data->Fc->Idx[n]=data->Object->NVr-data->Fc->NIdx+n;
         }
      } else

      if (strcmp(Elem,"app:diffuseColor")==0) {
         if (data->Mt) {
            if ((n=XML_ArrayCheck(data->Buf,data->BufLen,' '))) {
               XML_ArrayExpandVect(data->Buf,data->BufLen,' ',3,data->Mt->Dif);
               data->Mt->Dif[3]=1.0;
            }
         }
      } else

      if (strcmp(Elem,"app:emissiveColor")==0) {
         if (data->Mt) {
            if ((n=XML_ArrayCheck(data->Buf,data->BufLen,' '))) {
               XML_ArrayExpandVect(data->Buf,data->BufLen,' ',3,data->Mt->Emi);
               data->Mt->Emi[3]=1.0;
            }
         }
      } else

      if (strcmp(Elem,"app:specularColor")==0) {
         if (data->Mt) {
            if ((n=XML_ArrayCheck(data->Buf,data->BufLen,' '))) {
               XML_ArrayExpandVect(data->Buf,data->BufLen,' ',3,data->Mt->Spe);
               data->Mt->Spe[3]=1.0;
            }
         }
      } else

      if (strcmp(Elem,"app:shininess")==0) {
         if (data->Mt) {
            if ((n=XML_ArrayCheck(data->Buf,data->BufLen,' '))) {
               XML_ArrayExpandVect(data->Buf,data->BufLen,' ',1,&data->Mt->Shi);
            }
         }
      } else

      if (strcmp(Elem,"app:transparency")==0) {
         if (data->Mt) {
            if ((n=XML_ArrayCheck(data->Buf,data->BufLen,' '))) {
               XML_ArrayExpandVect(data->Buf,data->BufLen,' ',1,&data->Mt->Alpha);
            }
         }
      } else

         if (strcmp(Elem,"app:imageURI")==0) {
         if (data->Mt) {
            strncpy(data->Mt->Path,data->Buf,256);
         }
      } else

      if (strcmp(Elem,"app:textureCoordinates")==0) {
         if (data->Fc) {
            if (!data->Object->Tx) {
               data->Object->Tx=(Vect3f*)calloc(data->Object->NVr,sizeof(Vect3f));
            }
            if (data->Object->Tx) {
               if ((n=XML_ArrayCheck(data->Buf,data->BufLen,' '))) {
                  n=XML_ArrayExpandVect(data->Buf,data->BufLen,' ',2,&data->Object->Tx[data->Fc->Idx[0]]);
                  for(i=0;i<n;i++) {
                     data->Object->Tx[data->Fc->Idx[0]+i][1]=1.0-data->Object->Tx[data->Fc->Idx[0]+i][1];
                  }
               }
            }
         }
      } else

      if (strcmp(Elem,"app:target")==0 && strlen(data->Buf)>1) {
         if ((data->Fc=Model_FaceFind(data->Model,&data->Buf[1],&obj))) {
            data->Object=obj;
            data->Fc->Mt=data->Mt;
         }
      }
   }
}

void ModelCityGML_CharHandler(void *Data,const char *Txt,int Len) {

   CityGMLData *data=(CityGMLData*)Data;

   if (Txt && Len) {
      if ((data->BufLen+Len+1)>data->BufRLen) {
         data->BufRLen=data->BufLen+Len+1;
         data->Buf=realloc(data->Buf,data->BufRLen);
      }
      memcpy(data->Buf+data->BufLen,Txt,Len);
      data->Buf[data->BufLen+Len]='\0';
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) ModelCityGML_CharHandler: (%i) .%s.\n",data->BufLen,data->Buf);
#endif
      data->BufLen+=Len;
   }
}

void NS_StartHandler(void *Data,const char *Prefix,const char *URI) {

  printf("in %s => %s\n", Prefix ? Prefix : "(null)", URI ? URI : "(null)");
}

void NS_EndHandler(void *Data,const char *Prefix) {

  printf("ou %s\n", Prefix ? Prefix : "(null)");
}


/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_LoadCityGML>
 * Creation     : Janvier 2003 J.P. Gauthier
 *
 * But          : Lire un fichier CityGML
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
int Model_LoadCityGML(T3DModel *M,char *Path) {

   FILE        *file;
   XML_Parser   parser;
   CityGMLData *data;
   int          len,state=1;
   void        *buf;

   /*Create expat XML parser*/
   parser=XML_ParserCreate(NULL);
//   parser=XML_ParserCreateNS(NULL,'.');

   if (!parser) {
      fprintf(stderr,"(ERROR) Model_LoadCityGML: Couldn't initiate XML parser\n");
      return(0);
   }

   if (!(file=fopen(Path,"r"))) {
      return(0);
   }

   /*Data to be used while parsing*/
   data=(CityGMLData*)malloc(sizeof(CityGMLData));
   data->Model=M;
   data->Buf=NULL;
   data->BufLen=data->BufRLen=0;
   data->Env=0;

   data->Scene=NULL;
   data->Object=NULL;
   data->Fc=NULL;
   data->Mt=NULL;

   /*Initialise expat XML parser*/
   XML_SetUserData(parser,data);
   XML_SetCharacterDataHandler(parser,ModelCityGML_CharHandler);
   XML_SetElementHandler(parser,ModelCityGML_StartHandler,ModelCityGML_EndHandler);
//   XML_SetNamespaceDeclHandler(parser,NS_StartHandler,NS_EndHandler);

   /*Parse the XML by chunk*/
   for (;;) {
      if (!(buf=XML_GetBuffer(parser,XMLBUFSIZE))) {
         fprintf(stderr,"(ERROR) Model_LoadCityGML: Could not allocate XML IO buffer\n");
         state=0;
         break;
      }

      len=fread(buf,1,XMLBUFSIZE,file);
      if (ferror(file)) {
         fprintf(stderr,"(ERROR) Model_LoadCityGML: Read error on %s\n",Path);
         state=0;
         break;
      }

      if (!XML_ParseBuffer(parser,len,len==0)) {
         fprintf(stderr,"(ERROR) Model_LoadCityGML: XML Parse error at line %li:\n\t%s\n",XML_GetCurrentLineNumber(parser),XML_ErrorString(XML_GetErrorCode(parser)));
         state=0;
         break;
      }

      if (!len)
         break;
   }
   XML_ParserFree(parser);

   /*Parse materials
   fprintf(stderr,"---1\n");
   for (len=0;len<M->NMt;len++) {
      data->Mt=&M->Mt[len];
      if ((data->Fc=Model_FaceFind(data->Model,&data->Mt->Target[1],NULL))) {
         data->Fc->Mt=data->Mt;
      }
   }*/

   /*Free associates parsing data structure*/
   free(data);

   fclose(file);
   return(state);
}
