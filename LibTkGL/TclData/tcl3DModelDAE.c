/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Affichage de modele 3D
 * Fichier      : tcl3DModelDAE.c
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

typedef struct DAESource {
   char         Alias;
   char        *Id;
   unsigned int Nb;
   unsigned int Dim;
   float       *Array;
} DAESource;

typedef struct DAE_Data {
   T3DModel  *Model;
   T3DScene  *Scene,*Nodes;
   T3DObject *Object;
   TFace     *Fc;
   TList     *Sources;
   int        NFc,NVr,VrDim,VrType;
   int        VrOffset,NrOffset,TxOffset;
   DAESource *VrSource,*NrSource,*TxSource;
} DAE_Data;

void       ModelDAE_SourceFree(DAESource *Source);
DAESource *ModelDAE_SourceFind(DAE_Data *Data,char* Id);

void ModelDAE_SourceFree(DAESource *Source) {

   if (Source) {
      if (Source->Id)    free(Source->Id);    Source->Id=NULL;

      if (!Source->Alias) {
         if (Source->Array) free(Source->Array); Source->Array=NULL;
      }
      Source->Nb=0;
   }
}

DAESource *ModelDAE_SourceFind(DAE_Data *Data,char* Id) {

   DAESource *src;
   TList     *tmp=Data->Sources;

   while(tmp) {
      src=(DAESource*)tmp->Data;
      if (strcmp(src->Id,Id)==0) {
         return((DAESource*)tmp->Data);
      }
      tmp=tmp->Next;
   }
   return(NULL);
}

void ModelDAE_StartHandler(void *Data,const char *Elem,const char **Attr) {

   T3DScene  *scn,*child;
   T3DObject *obj=NULL;
   XML_Data  *data=(XML_Data*)Data;
   DAE_Data  *dae=(DAE_Data*)data->Specific;
   DAESource *src,*tmp;
   int       i,o,nfc;

   XML_CharReset(Data);
   XML_Check(Data,Elem,"COLLADA");

   if (Elem && XML_Valid(Data)) {
#ifdef DEBUG
         fprintf(stdout,"(DEBUG) ModelDAE_StartHandler: Token %s\n",Elem);
#endif
      /*Unit tag*/
      /*Get the unit reference more "meter", Collada uses meter as reference*/
      if (strcmp(Elem,"unit")==0) {
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"name")==0) {
            }
            if (strcmp(Attr[i],"meter")==0) {
               dae->Model->Meter=atof(Attr[i+1]);
            }
         }
      } else

      /*Source tag*/
      /*Create a new source with name "id" and add to sources list*/
      if (strcmp(Elem,"source")==0) {
         if (!(dae->Sources=TList_Add(dae->Sources,(DAESource*)malloc(sizeof(DAESource))))) {
            fprintf(stdout,"(ERROR) ModelDAE_StartHandler: Could not allocate memory for node\n");
         }
         src=(DAESource*)dae->Sources->Data;
         src->Alias=0;
         src->Array=NULL;
         src->Id=NULL;

         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"id")==0) {
               src->Id=strndup(Attr[i+1],256);
            }
         }
      } else

      /*Array tag*/
      /*Initialise current source with "count" nb elements*/
      if (strcmp(Elem,"float_array")==0) {
         src=(DAESource*)dae->Sources->Data;

         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"count")==0) {
               src->Nb=atoi(Attr[i+1]);
            }
         }
      } else

      /*Accessor tag*/
      /*Initialise current source with "stride" dimension*/
      if (strcmp(Elem,"accessor")==0) {
         src=(DAESource*)dae->Sources->Data;

         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"stride")==0) {
               src->Dim=atoi(Attr[i+1]);
            }
         }
      } else

      /*Vertices tag*/
      /*Create new source which is alias to source "Id" defining positions*/
      if (strcmp(Elem,"vertices")==0) {
         dae->Sources=TList_Add(dae->Sources,(DAESource*)malloc(sizeof(DAESource)));
         src=(DAESource*)dae->Sources->Data;
         src->Alias=1;
         src->Array=NULL;
         src->Id=NULL;
         src->Nb=0;

         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"id")==0) {
               src->Id=strndup(Attr[i+1],256);
            }
         }
      } else

      /*Node library tag*/
      /*Initialize scene node tree and set current scene to this node*/
      if (strcmp(Elem,"library_nodes")==0) {
         dae->Nodes=Model_SceneAdd(NULL,NULL,1);
         dae->Scene=dae->Nodes;
      } else

      /*Visual scene library tag*/
      /*Move the current scene to the model scenes*/
      if (strcmp(Elem,"library_visual_scenes")==0) {
         dae->Scene=Model_SceneAdd(dae->Model,NULL,1);
      } else

      /*Node tag*/
      /*Add new node with name "id" to current node and set current to new*/
      if (strcmp(Elem,"node")==0) {
         dae->Scene=Model_SceneAdd(dae->Model,dae->Scene,1);
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"id")==0) {
               dae->Scene->Name=strndup(Attr[i+1],256);
            }
         }
      } else

      /*Geometry instanciation tag*/
      /*Add geometry with name "url" to current scene*/
      if (strcmp(Elem,"instance_geometry")==0) {
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"url")==0) {
               if (obj=Model_ObjectFind(dae->Model,(char*)(Attr[i+1]+1))) {
                  dae->Scene->NObj++;
                  dae->Scene->Obj=(T3DObject**)realloc(dae->Scene->Obj,dae->Scene->NObj*sizeof(T3DObject*));
                  dae->Scene->Obj[dae->Scene->NObj-1]=obj;
               } else {
                  fprintf(stderr,"(ERROR) ModelDAE_StartHandler: Could not find object \"%s\"\n",Attr[i+1]);
               }
            }
         }
      } else

      /*Node instanciation tag*/
      /*Add node with name "url" to current scene*/
      if (strcmp(Elem,"instance_node")==0) {
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"url")==0) {
               if (child=Model_SceneFind(dae->Nodes,(char*)(Attr[i+1]+1))) {
                  scn=Model_SceneAdd(dae->Model,dae->Scene,1);
                  memcpy(scn,child,sizeof(T3DScene));
               } else {
                  fprintf(stdout,"(ERROR) ModelDAE_StartHandler: Could not find node \"%s\" in node library\n",Attr[i+1]);
               }
            }
         }
      } else

      /*Geometry tag*/
      /*Create new geometry object with name "id"*/
      if (strcmp(Elem,"geometry")==0) {
         dae->Object=Model_ObjectAdd(dae->Model,1);
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"id")==0) {
               dae->Object->Name=strndup(Attr[i+1],256);
            }
         }
      } else

      /*Triangle tag (Faces)*/
      /*Add "count" triangles to objects faces*/
      if (strcmp(Elem,"triangles")==0) {
         dae->NFc=0;
         dae->NVr=3;
         dae->VrDim=0;
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"material")==0) {
            }
            if (strcmp(Attr[i],"count")==0) {
               dae->NFc=atoi(Attr[i+1]);
            }
         }

         if (dae->NFc) {
            dae->Fc=Model_ObjectFaceAdd(dae->Object,dae->NFc);
         }
      }  else

      /*Lines tag (Faces)*/
      /*Add "count" lines to objects faces*/
      if (strcmp(Elem,"lines")==0) {
         dae->NFc=0;
         dae->NVr=2;
         dae->VrDim=0;
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"material")==0) {
            }
            if (strcmp(Attr[i],"count")==0) {
               dae->NFc=atoi(Attr[i+1]);
            }
         }

         if (dae->NFc) {
            dae->Fc=Model_ObjectFaceAdd(dae->Object,dae->NFc);
         }
      }  else

      /*p array tag*/
      /*Create source name p which contains current geometry data to be processed*/
      if (strcmp(Elem,"p")==0) {
         dae->Sources=TList_Add(dae->Sources,(DAESource*)malloc(sizeof(DAESource)));
         src=(DAESource*)dae->Sources->Data;
         src->Alias=0;
         src->Array=NULL;
         dae->VrDim+=1;
         dae->VrType=0;
         src->Nb=dae->NFc*dae->VrDim*dae->NVr;
         src->Id=strdup("p");
      } else

      /*Matrix array tag*/
      /*Create source name matrix which contains current transformation matrix to be processed*/
      if (strcmp(Elem,"matrix")==0) {
         dae->Sources=TList_Add(dae->Sources,(DAESource*)malloc(sizeof(DAESource)));
         src=(DAESource*)dae->Sources->Data;
         src->Alias=0;
         src->Array=NULL;
         src->Nb=16;
         src->Id=strdup("matrix");
      } else

      /*Input definition tag*/
      /*Set current object "semantic" input for "source" to be processed with "offset"*/
      if (strcmp(Elem,"input")==0) {
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"semantic")==0) {
               if (strcmp(Attr[i+1],"POSITION")==0) {
                  dae->VrType=0;
               } else if (strcmp(Attr[i+1],"VERTEX")==0) {
                  dae->VrType=F3V;
               } else if (strcmp(Attr[i+1],"NORMAL")==0) {
                  dae->VrType=F3VN;
               } else if (strcmp(Attr[i+1],"TEXCOORD")==0) {
                  dae->VrType=F3VNT;
               }
            } else if (strcmp(Attr[i],"source")==0) {
               switch(dae->VrType) {
                  case F3V:   if (!(dae->VrSource=ModelDAE_SourceFind(dae,(char*)(Attr[i+1]+1)))) {
                                 fprintf(stderr,"(ERROR) ModelDAE_StartHandler: Can't find Vr source array %s\n",Attr[i+1]);
                              }
                              break;

                  case F3VN:  if (!(dae->NrSource=ModelDAE_SourceFind(dae,(char*)(Attr[i+1]+1)))) {
                                 fprintf(stderr,"(ERROR) ModelDAE_StartHandler: Can't find Nr source array %s\n",Attr[i+1]);
                              }
                              break;

                  case F3VNT: if (!(dae->TxSource=ModelDAE_SourceFind(dae,(char*)(Attr[i+1]+1)))) {
                                 fprintf(stderr,"(ERROR) ModelDAE_StartHandler: Can't find Tx source array %s\n",Attr[i+1]);
                              }
                              break;
                  default:   src=(DAESource*)dae->Sources->Data;
                             if (!(tmp=ModelDAE_SourceFind(dae,(char*)(Attr[i+1]+1)))) {
                                fprintf(stderr,"(ERROR) ModelDAE_StartHandler: Can't find position source array %s\n",Attr[i+1]);
                              } else {
                                 src->Array=tmp->Array;
                                 src->Nb=tmp->Nb;
                                 src->Dim=tmp->Dim;
                              }
                              break;
               }
            } else if (strcmp(Attr[i],"offset")==0) {
               switch(dae->VrType) {
                  case F3V:   dae->VrOffset=atoi(Attr[i+1]);
                              dae->VrDim=dae->VrOffset>dae->VrDim?dae->VrOffset:dae->VrDim;
                              break;

                  case F3VN:  dae->NrOffset=atoi(Attr[i+1]);
                              dae->VrDim=dae->NrOffset>dae->VrDim?dae->NrOffset:dae->VrDim;
                              break;

                  case F3VNT: dae->TxOffset=atoi(Attr[i+1]);
                              dae->VrDim=dae->TxOffset>dae->VrDim?dae->TxOffset:dae->VrDim;
                              break;
               }
            }
         }
      }
   }
}

void ModelDAE_EndHandler(void *Data,const char *Elem) {

   XML_Data  *data=(XML_Data*)Data;
   DAE_Data  *dae=(DAE_Data*)data->Specific;
   DAESource *s;
   int        i,v,f,n;

   if (Elem && XML_Valid(Data)) {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) ModelDAE_EndHandlerr: Token %s\n",Elem);
#endif
      if (strcmp(Elem,"float_array")==0 || strcmp(Elem,"matrix")==0 || strcmp(Elem,"p")==0) {
         s=(DAESource*)dae->Sources->Data;
         if ((n=XML_ArrayCheck(Data,' '))) {
            if (!s->Array) {
               s->Array=(float*)malloc((n+1)*sizeof(float));
            }
            XML_ArrayExpandVect(Data,' ',s->Array);
         }
      }

      /*p array tag*/
      if (strcmp(Elem,"p")==0) {
         s=(DAESource*)dae->Sources->Data;

         for(i=0,f=0;i<s->Nb;i+=dae->VrDim*dae->NVr,f++) {
            dae->Fc[f].NIdx=dae->NVr;
            dae->Fc[f].Idx=(unsigned int*)malloc(dae->NVr*sizeof(unsigned int));
            dae->Fc[f].Mt=NULL;

            for(v=0;v<dae->NVr;v++) {
               if (dae->VrSource) {
                  dae->Fc[f].Idx[v]=s->Array[i+dae->VrOffset+dae->VrDim*v];
               }
               if (dae->NrSource) {
   //               dae->Fc[f].Idx[0]=p->Array[i+dae->NrOffset];
               }
               if (dae->TxSource) {
   //               dae->Fc[f].Idx[0]=p->Array[i+dae->TxOffset];
               }
            }
         }
#ifdef DEBUG
         fprintf(stderr,"(DEBUG) ModelDAE_EndHandler: Parsed %i vertices\n",i/(dae->VrDim*dae->NVr));
#endif
         if (dae->VrSource) {
            if (!dae->Object->Vr) {
               n=dae->VrSource->Nb/dae->VrSource->Dim;
               dae->Object->NVr=n;
               dae->Object->Vr=(Vect3f*)malloc(dae->Object->NVr*sizeof(Vect3f));
               for(i=0;i<dae->Object->NVr;i++) {
                  for(v=0;v<dae->VrSource->Dim;v++) {
                     dae->Object->Vr[i][v]=dae->VrSource->Array[dae->VrSource->Dim*i+v];
                  }
               }
            }
         }
         if (dae->NrSource) {
            if (!dae->Object->Nr) {
               n=dae->NrSource->Nb/dae->NrSource->Dim;
               dae->Object->Nr=(Vect3f*)malloc(n*sizeof(Vect3f));
               for(i=0;i<n;i++) {
                  for(v=0;v<dae->VrSource->Dim;v++) {
                     dae->Object->Nr[i][v]=dae->NrSource->Array[dae->VrSource->Dim*i+v];
                  }
               }
            }
         }
         if (dae->TxSource) {
         }

         dae->VrDim=0;
         dae->VrType=0;
      } else

      /*matrix array tag*/
      if (strcmp(Elem,"matrix")==0) {
         if (!(s=ModelDAE_SourceFind(dae,"matrix"))) {
            fprintf(stderr,"(ERROR) ModelDAE_EndHandler: Can't find matrix source array\n");
         } else {
            dae->Scene->Mtx=(float*)malloc(16*sizeof(float));
            memcpy(dae->Scene->Mtx,s->Array,16*sizeof(float));
         }
      } else

      /*Node tag*/
      if (strcmp(Elem,"node")==0) {
         if (dae->Scene->Parent) {
            dae->Scene=dae->Scene->Parent;
         }
      } else

      /*Geometry tag*/
      if (strcmp(Elem,"geometry")==0) {
         dae->Object=NULL;
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_LoadDAE>
 * Creation     : Janvier 2003 J.P. Gauthier
 *
 * But          : Lire un fichier Collada DAE
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
int Model_LoadDAE(Tcl_Interp* Interp,T3DModel *M,char *Path) {

   XML_Parser parser;
   DAE_Data  *dae;
   int        state=1;

   /*Create expat XML parser*/
   if (!(parser=XML_ParserCreate(NULL))) {
      fprintf(stderr,"(ERROR) Model_LoadDAE: Couldn't initiate XML parser\n");
      return(0);
   }

   /*Data to be used while parsing*/
   dae=(DAE_Data*)malloc(sizeof(DAE_Data));
   dae->Model=M;
   dae->Scene=NULL;
   dae->Object=NULL;
   dae->Sources=NULL;
   dae->Fc=NULL;
   dae->VrSource=dae->NrSource=dae->TxSource=NULL;

   /*Initialise expat XML parser*/
   XML_SetElementHandler(parser,ModelDAE_StartHandler,ModelDAE_EndHandler);

   /*Parse the XML*/
   state=XML_ParseFile(Interp,parser,dae,Path);
   XML_ParserFree(parser);

   /*Free associates parsing data structure*/
   TList_Clear(dae->Sources,ModelDAE_SourceFree);
   free(dae);

   return(state);
}
