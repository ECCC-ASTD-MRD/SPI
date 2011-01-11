/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Affichage de modele 3D
 * Fichier      : tcl3DModelDAE.c
 * Creation     : Aout2007 - J.P. Gauthier
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
#include "tcl3DModelDAE.h"
#include <string.h>
#include <expat.h>

int ModelDAE_SourceExpand(DAESource *Source) {

   char *tok,*save=NULL;
   int   n=0;

   if (Source->Text) {
      if (!Source->Array) {
         Source->Array=(float*)malloc(Source->Nb*sizeof(float));
      }

      /*Parse all tokens*/
      tok=strtok_r(Source->Text," ",&save);

      while(tok) {
         if (n>=Source->Nb) {
            fprintf(stdout,"(ERROR) ModelDAE_SourceExpand: Overflow of float-array\n");
            break;
         }
         Source->Array[n++]=atof(tok);

         /*Check for buffer overrun*/
         if (save>=(Source->Text+Source->TextLen))
               break;
         tok=strtok_r(NULL," ",&save);

      }
      fprintf(stdout,"(DEBUG) ModelDAE_SourceExpand: Parsed %i array values out of %i\n",n,Source->Nb);

      free(Source->Text);
      Source->Text=NULL;
      Source->TextLen=0;
   }

   return(n);
}

void ModelDAE_SourceFree(DAESource *Source) {

   if (Source->Id)    free(Source->Id);    Source->Id=NULL;

   if (!Source->Alias) {
      if (Source->Text)  free(Source->Text);  Source->Text=NULL;
      if (Source->Array) free(Source->Array); Source->Array=NULL;
   }
   Source->Nb=0;
}

DAESource *ModelDAE_SourceFind(DAEData *Data,char* Id) {

   DAESource *src;
   TList     *tmp=Data->Sources;

   while(tmp) {
      src=(DAESource*)tmp->Data;
      if (strcmp(src->Id,Id)==0) {
         ModelDAE_SourceExpand(src);
         return((DAESource*)tmp->Data);
      }
      tmp=tmp->Next;
   }
   return(NULL);
}

void ModelDAE_StartHandler(void *Data,const char *Elem,const char **Attr) {

   DAEData   *data=(DAEData*)Data;
   DAESource *src,*tmp;
   int       i,nfc;

   if (Elem) {
      strncpy(data->Tag,Elem,255);

      /*Source tag*/
      if (strcmp(Elem,"source")==0) {
         fprintf(stdout,"(DEBUG) ModelDAE_StartHandler: found source\n");
         if (!(data->Sources=TList_Add(data->Sources,(DAESource*)malloc(sizeof(DAESource))))) {
            fprintf(stdout,"(ERROR) ModelDAE_StartHandler: Could not allocate memory for node\n");
         }
         src=(DAESource*)data->Sources->Data;
         src->Alias=0;
         src->TextLen=0;
         src->Text=NULL;
         src->Array=NULL;

         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"id")==0) {
               src->Id=strdup(Attr[i+1]);
            }
         }
      } else

      /*Array tag*/
      if (strcmp(Elem,"float_array")==0) {
         fprintf(stdout,"(DEBUG) ModelDAE_StartHandler: found source\n");
         src=(DAESource*)data->Sources->Data;

         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"count")==0) {
               src->Nb=atoi(Attr[i+1]);
            }
         }
      } else

      /*Accessor tag*/
      if (strcmp(Elem,"accessor")==0) {
         fprintf(stdout,"(DEBUG) ModelDAE_StartHandler: found accessor\n");
         src=(DAESource*)data->Sources->Data;

         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"stride")==0) {
               src->Dim=atoi(Attr[i+1]);
            }
         }
      } else

      /*Vertices tag*/
      if (strcmp(Elem,"vertices")==0) {
         fprintf(stdout,"(DEBUG) ModelDAE_StartHandler: found vertices\n");
         data->Sources=TList_Add(data->Sources,(DAESource*)malloc(sizeof(DAESource)));
         src=(DAESource*)data->Sources->Data;
         src->Alias=1;
         src->TextLen=0;
         src->Text=NULL;
         src->Array=NULL;
         src->Nb=0;

         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"id")==0) {
               src->Id=strdup(Attr[i+1]);
            }
         }
      } else

      /*Geometry tag*/
      if (strcmp(Elem,"geometry")==0) {
         fprintf(stdout,"(DEBUG) ModelDAE_StartHandler: Adding object\n");
         data->Object=Model_ObjAdd(data->Model,1);
      } else

      /*Triangle tag (Faces)*/
      if (strcmp(Elem,"triangles")==0) {
         data->NFc=0;
         data->NVr=3;
         data->VrDim=0;
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"material")==0) {
            }
            if (strcmp(Attr[i],"count")==0) {
               data->NFc=atoi(Attr[i+1]);
            }
         }

         if (data->NFc) {
            fprintf(stdout,"(DEBUG) ModelDAE_StartHandler: Adding %i faces\n",data->NFc);
            data->Fc=Model_ObjFaceAdd(data->Object,data->NFc);
         }
      }  else

      /*Triangle tag (Faces)*/
      if (strcmp(Elem,"lines")==0) {
         data->NFc=0;
         data->NVr=2;
         data->VrDim=0;
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"material")==0) {
            }
            if (strcmp(Attr[i],"count")==0) {
               data->NFc=atoi(Attr[i+1]);
            }
         }

         if (data->NFc) {
            fprintf(stdout,"(DEBUG) ModelDAE_StartHandler: Adding %i segments\n",data->NFc);
            data->Fc=Model_ObjFaceAdd(data->Object,data->NFc);
         }
      }  else

      /*p array tag*/
      if (strcmp(Elem,"p")==0) {
         fprintf(stdout,"(DEBUG) ModelDAE_StartHandler: found p\n");
         data->Sources=TList_Add(data->Sources,(DAESource*)malloc(sizeof(DAESource)));
         src=(DAESource*)data->Sources->Data;
         src->Alias=0;
         src->TextLen=0;
         src->Text=NULL;
         src->Array=NULL;
         data->VrDim+=1;
         data->VrType=0;
         src->Nb=data->NFc*data->VrDim*data->NVr;
         src->Id=strdup("p");
      } else

      /*Input definition tag*/
      if (strcmp(Elem,"input")==0) {
         fprintf(stdout,"(DEBUG) ModelDAE_StartHandler: Input tag\n");
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"semantic")==0) {
               if (strcmp(Attr[i+1],"POSITION")==0) {
                  data->VrType=0;
               } else if (strcmp(Attr[i+1],"VERTEX")==0) {
                  data->VrType=F3V;
               } else if (strcmp(Attr[i+1],"NORMAL")==0) {
                  data->VrType=F3VN;
               } else if (strcmp(Attr[i+1],"TEXCOORD")==0) {
                  data->VrType=F3VNT;
               }
            } else if (strcmp(Attr[i],"source")==0) {
               switch(data->VrType) {
                  case F3V:   if (!(data->VrSource=ModelDAE_SourceFind(data,(char*)(Attr[i+1]+1)))) {
                                 fprintf(stderr,"(ERROR) ModelDAE_StartHandler: Can't find Vr source array %s\n",Attr[i+1]);
                              }
                              break;

                  case F3VN:  if (!(data->NrSource=ModelDAE_SourceFind(data,(char*)(Attr[i+1]+1)))) {
                                 fprintf(stderr,"(ERROR) ModelDAE_StartHandler: Can't find Nr source array %s\n",Attr[i+1]);
                              }
                              break;

                  case F3VNT: if (!(data->TxSource=ModelDAE_SourceFind(data,(char*)(Attr[i+1]+1)))) {
                                 fprintf(stderr,"(ERROR) ModelDAE_StartHandler: Can't find Tx source array %s\n",Attr[i+1]);
                              }
                              break;
                  default:   src=(DAESource*)data->Sources->Data;
                             if (!(tmp=ModelDAE_SourceFind(data,(char*)(Attr[i+1]+1)))) {
                                fprintf(stderr,"(ERROR) ModelDAE_StartHandler: Can't find position source array %s\n",Attr[i+1]);
                              } else {
                                 src->Array=tmp->Array;
                                 src->Nb=tmp->Nb;
                                 src->Dim=tmp->Dim;
                              }
                              break;
               }
            } else if (strcmp(Attr[i],"offset")==0) {
               switch(data->VrType) {
                  case F3V:   data->VrOffset=atoi(Attr[i+1]);
                              data->VrDim=data->VrOffset>data->VrDim?data->VrOffset:data->VrDim;
                              break;

                  case F3VN:  data->NrOffset=atoi(Attr[i+1]);
                              data->VrDim=data->NrOffset>data->VrDim?data->NrOffset:data->VrDim;
                              break;

                  case F3VNT: data->TxOffset=atoi(Attr[i+1]);
                              data->VrDim=data->TxOffset>data->VrDim?data->TxOffset:data->VrDim;
                              break;
               }
            }
         }
      }
   }
}

void ModelDAE_EndHandler(void *Data,const char *Elem) {

   DAEData   *data=(DAEData*)Data;
   DAESource *p;
   int        i,v,f,nb;

   if (Elem) {
      data->Tag[0]='\0';

      /*p array tag*/
      if (strcmp(Elem,"p")==0) {
         if (!(p=ModelDAE_SourceFind(data,"p"))) {
            fprintf(stderr,"(ERROR) ModelDAE_EndHandler: Can't find p source array\n");
         } else {
            for(i=0,f=0;i<p->Nb;i+=data->VrDim*data->NVr,f++) {
               data->Fc[f].NIdx=data->NVr;
               data->Fc[f].Idx=(unsigned int*)malloc(data->NVr*sizeof(unsigned int));
               data->Fc[f].Mt=NULL;

               for(v=0;v<data->NVr;v++) {
                  if (data->VrSource) {
                     data->Fc[f].Idx[v]=p->Array[i+data->VrOffset+data->VrDim*v];
                  }
                  if (data->NrSource) {
   //                  data->Fc[f].Idx[0]=p->Array[i+data->NrOffset];
                  }
                  if (data->TxSource) {
   //                  data->Fc[f].Idx[0]=p->Array[i+data->TxOffset];
                  }
               }
            }
            nb=i/(data->VrDim*data->NVr);
            fprintf(stderr,"(DEBUG) ModelDAE_EndHandler: Parsed %i vertices\n",nb);

            ModelDAE_SourceFree(p);
            data->Sources=TList_Del(data->Sources,p);

            if (data->VrSource) {
               if (!data->Object->Vr) {
                  data->Object->Vr=(Vect3f*)malloc(data->VrSource->Nb/data->NVr*sizeof(Vect3f));
                  for(i=0;i<data->VrSource->Nb/data->NVr;i++) {
                     for(v=0;v<data->NVr;v++) {
                        data->Object->Vr[i][v]=data->VrSource->Array[data->NVr*i+v];
                     }
                 }
               }
            }
            if (data->NrSource) {
               if (!data->Object->Nr) {
                  data->Object->Nr=(Vect3f*)malloc(data->NrSource->Nb/data->NVr*sizeof(Vect3f));
                  for(i=0;i<data->NrSource->Nb/data->NVr;i++) {
                     for(v=0;v<data->NVr;v++) {
                        data->Object->Nr[i][v]=data->NrSource->Array[data->NVr*i+v];
                     }
                  }
               }
            }
            if (data->TxSource) {
            }
         }
         data->VrDim=0;
         data->VrType=0;
      }
   }
}

void ModelDAE_CharHandler(void *Data,const char *Txt,int Len) {

   DAEData   *data=(DAEData*)Data;
   DAESource *src;

   if (Txt && Len) {
      if (strcmp(data->Tag,"float_array")==0 || strcmp(data->Tag,"p")==0) {
         if (!data->Sources || !(src=(DAESource*)data->Sources->Data)) {
            fprintf(stdout,"(ERROR) ModelDAE_charHandler: Source not defined yet\n");
         } else {
            src->Text=realloc(src->Text,src->TextLen+Len);
            memcpy(src->Text+src->TextLen,Txt,Len);
            src->TextLen+=Len;
         }
      }
   }
}

void
proc_hndl(void *data, const char *target, const char *pidata) {
//  printf("\n%4d: Processing Instruction - ", Eventcnt++);
//  printcurrent((XML_Parser) data);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_LoadDAE>
 * Creation     : Janvier 2003 J.P. Gauthier
 *
 * But          : Lire un fichier Collada DAE
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
int Model_LoadDAE(T3DModel *M,char *Path) {

   FILE      *file;
   XML_Parser parser;
   DAEData   *data;
   int        len,state=1;
   void      *buf;

   /*Create expat XML parser*/
   parser=XML_ParserCreate(NULL);
   if (!parser) {
      fprintf(stderr,"Model_LoadDAE: Couldn't initiate XML parser\n");
      return(0);
   }

   if (!(file=fopen(Path,"r"))) {
      return(0);
   }

   /*Data to be used while parsing*/
   data=(DAEData*)malloc(sizeof(DAEData));
   data->Model=M;
   data->Object=NULL;
   data->Sources=NULL;
   data->Fc=NULL;
   data->VrSource=data->NrSource=data->TxSource=NULL;

   /*Initialise expat XML parser*/
   XML_SetUserData(parser,data);
   XML_SetElementHandler(parser,ModelDAE_StartHandler,ModelDAE_EndHandler);
   XML_SetCharacterDataHandler(parser,ModelDAE_CharHandler);
   XML_SetProcessingInstructionHandler(parser,proc_hndl);

   /*Parse the XML by chunk*/
   for (;;) {
      if (!(buf=XML_GetBuffer(parser,DAEBUFSIZE))) {
         fprintf(stderr,"Model_LoadDAE: Colud not allocate XML IO buffer\n");
         state=0;
         break;
      }

      len=fread(buf,1,DAEBUFSIZE,file);
      if (ferror(file)) {
         fprintf(stderr,"Model_LoadDAE: Read error on %s\n",Path);
         state=0;
         break;
      }

      if (!XML_ParseBuffer(parser,len,len==0)) {
         fprintf(stderr,"Model_LoadDAE: XML Parse error at line %d:\n\t%s\n",XML_GetCurrentLineNumber(parser),XML_ErrorString(XML_GetErrorCode(parser)));
         state=0;
         break;
      }

      if (!len)
         break;
    }
    XML_ParserFree(parser);

    /*Free associates parsing data structure*/
    TList_Clear(data->Sources,ModelDAE_SourceFree);
    free(data);

    fclose(file);
    return(state);
}
