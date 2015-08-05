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
#include "tclXML.h"

#define GML_ENVELOPE 1

typedef struct GML_Data {
   T3DModel  *Model;
   T3DScene  *Scene,*Nodes;
   T3DObject *Object;
   TMaterial *Mt;
   TFace     *Fc;
   int        NFc,NVr,VrDim;
} GML_Data;

void ModelCityGML_StartHandler0(void *Data,const char *Elem,const char **Attr) {

   XML_Data  *data=(XML_Data*)Data;
   GML_Data  *gml=(GML_Data*)data->Specific;
   int        i;

   XML_CharReset(Data);
   XML_Check(Data,Elem,"CityModel");

   if (Elem && XML_Valid(Data)) {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) ModelCityGML_StartHandler: Token %s\n",Elem);
#endif

      if (strcmp(Elem,"cityObjectMember")==0) {
//         gml->Scene=Model_SceneAdd(gml->Model,gml->Scene,1);
      } else

      if (strcmp(Elem,"bldg:Building")==0 || strcmp(Elem,"citygml:Building")==0 || strcmp(Elem,"bldg:BuildingInstallation")==0 || strcmp(Elem,"dem:TINRelief")==0 ||
         strcmp(Elem,"trans:Road")==0) {
         gml->Object=Model_ObjectAdd(gml->Model,1);
         gml->Object->NFc=0;
         gml->Object->NVr=0;

         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"gml:id")==0) {
               gml->Object->Name=strndup(Attr[i+1],256);
            }
         }
      } else

      if (strcmp(Elem,"gml:Envelope")==0) {
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"srsName")==0) {
               gml->Model->GRef=GeoRef_New();
               GeoRef_WKTSet(gml->Model->GRef,(char*)Attr[i+1],NULL,NULL,NULL);
            }
         }
         data->Bloc=GML_ENVELOPE;
      } else

      if (strcmp(Elem,"gml:Polygon")==0 || strcmp(Elem,"gml:Triangle")==0) {
         if (!gml->Object) {
            fprintf(stdout,"(ERROR) No object defined\n");
         }
         gml->Fc=Model_ObjectFaceAdd(gml->Object,1);

         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"gml:id")==0) {
               gml->Fc->Name=strndup(Attr[i+1],256);
            }
         }
      } else

      if (strcmp(Elem,"gml:posList")==0) {
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"srsDimension")==0) {
               gml->VrDim=atoi(Attr[i+1]);
            }
         }
      }
   }
}

void ModelCityGML_StartHandler1(void *Data,const char *Elem,const char **Attr) {

   XML_Data  *data=(XML_Data*)Data;
   GML_Data  *gml=(GML_Data*)data->Specific;
   T3DObject *obj=NULL;
   int        i;

   XML_CharReset(Data);
   XML_Check(Data,Elem,"CityModel");

   if (Elem && XML_Valid(Data)) {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) ModelCityGML_StartHandler: Token %s\n",Elem);
#endif

      if (strcmp(Elem,"app:target")==0) {
         for (i=0;Attr[i];i+=2) {
            if (strcmp(Attr[i],"uri")==0) {
               if ((gml->Fc=Model_FaceFind(gml->Model,(char*)&Attr[i+1][1],&obj))) {
                  gml->Object=obj;
                  gml->Fc->Mt=gml->Mt;
               }
            }
         }
      } else

      if (strcmp(Elem,"app:surfaceDataMember")==0) {
         gml->Mt=Model_MaterialAdd(gml->Model,1);
      }
   }
}

void ModelCityGML_EndHandler0(void *Data,const char *Elem) {

   XML_Data  *data=(XML_Data*)Data;
   GML_Data  *gml=(GML_Data*)data->Specific;
   int        n;

   if (Elem && XML_Valid(Data)) {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) ModelCityGML_EndHandler: Token %s\n",Elem);
#endif
      if (strcmp(Elem,"gml:Envelope")==0) {
         data->Bloc=XML_NIL;
      } else

      if (strcmp(Elem,"gml:posList")==0 || strcmp(Elem,"gml:pos")==0) {
         if (!data->Bloc==GML_ENVELOPE && gml->Object && (n=XML_ArrayCheck(Data,' '))) {
            n/=3;
            gml->Object->Vr=realloc(gml->Object->Vr,(gml->Object->NVr+n+1)*sizeof(Vect3f));
            XML_ArrayExpandVect(Data,' ',(float*)&gml->Object->Vr[gml->Object->NVr]);
            gml->Object->NVr+=n;
            gml->Fc->NIdx+=n;
         }
      } else

      if (strcmp(Elem,"gml:LinearRing")==0) {
         gml->Fc->Idx=(unsigned int*)malloc(gml->Fc->NIdx*sizeof(unsigned int));
         for(n=0;n<gml->Fc->NIdx;n++) {
            gml->Fc->Idx[n]=gml->Object->NVr-gml->Fc->NIdx+n;
         }
      }
   }
}

void ModelCityGML_EndHandler1(void *Data,const char *Elem) {

   XML_Data  *data=(XML_Data*)Data;
   GML_Data  *gml=(GML_Data*)data->Specific;
   int        n,i;

   if (Elem && XML_Valid(Data)) {
#ifdef DEBUG
      fprintf(stdout,"(DEBUG) ModelCityGML_EndHandler: Token %s\n",Elem);
#endif

      if (strcmp(Elem,"app:diffuseColor")==0) {
         if (gml->Mt) {
            if ((n=XML_ArrayCheck(Data,' '))) {
               XML_ArrayExpandVect(Data,' ',gml->Mt->Dif);
               gml->Mt->Dif[3]=1.0;
            }
         }
      } else

      if (strcmp(Elem,"app:emissiveColor")==0) {
         if (gml->Mt) {
            if ((n=XML_ArrayCheck(Data,' '))) {
               XML_ArrayExpandVect(Data,' ',gml->Mt->Emi);
               gml->Mt->Emi[3]=1.0;
            }
         }
      } else

      if (strcmp(Elem,"app:specularColor")==0) {
         if (gml->Mt) {
            if ((n=XML_ArrayCheck(Data,' '))) {
               XML_ArrayExpandVect(Data,' ',gml->Mt->Spe);
               gml->Mt->Spe[3]=1.0;
            }
         }
      } else

      if (strcmp(Elem,"app:shininess")==0) {
         if (gml->Mt) {
            if ((n=XML_ArrayCheck(Data,' '))) {
               XML_ArrayExpandVect(Data,' ',&gml->Mt->Shi);
            }
         }
      } else

      if (strcmp(Elem,"app:transparency")==0) {
         if (gml->Mt) {
            if ((n=XML_ArrayCheck(Data,' '))) {
               XML_ArrayExpandVect(Data,' ',&gml->Mt->Alpha);
            }
         }
      } else

         if (strcmp(Elem,"app:imageURI")==0) {
         if (gml->Mt) {
            strncpy(gml->Mt->Path,data->Buf,256);
         }
      } else

      if (strcmp(Elem,"app:textureCoordinates")==0) {
         if (gml->Fc) {
            if (!gml->Object->Tx) {
               gml->Object->Tx=(Vect3f*)calloc(gml->Object->NVr,sizeof(Vect3f));
            }
            if (gml->Object->Tx) {
               if ((n=XML_ArrayCheck(Data,' '))) {
                  n=XML_ArrayExpandVect(Data,' ',(float*)&gml->Object->Tx[gml->Fc->Idx[0]]);
                  for(i=0;i<n;i++) {
                     gml->Object->Tx[gml->Fc->Idx[0]+i][1]=1.0-gml->Object->Tx[gml->Fc->Idx[0]+i][1];
                  }
               }
            }
         }
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_LoadCityGML>
 * Creation     : Janvier 2003 J.P. Gauthier
 *
 * But          : Lire un fichier CityGML
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
int Model_LoadCityGML(Tcl_Interp *Interp,T3DModel *M,char *Path) {

   XML_Parser  parser;
   GML_Data   *gml;
   int         state=1;

   /*Create expat XML parser*/
   if (!(parser=XML_ParserCreate(NULL))) {
      fprintf(stderr,"(ERROR) Model_LoadCityGML: Couldn't initiate XML parser\n");
      return(0);
   }

   /*Data to be used while parsing*/
   gml=(GML_Data*)malloc(sizeof(GML_Data));
   gml->Model=M;
   gml->Scene=NULL;
   gml->Object=NULL;
   gml->Fc=NULL;
   gml->Mt=NULL;

   /*Initialise expat XML parser*/
   XML_SetElementHandler(parser,ModelCityGML_StartHandler0,ModelCityGML_EndHandler0);

   /*Parse the XML*/
   state=XML_ParseFile(Interp,parser,gml,Path);

//   parser=XML_ParserCreate(NULL);
//   XML_SetElementHandler(parser,ModelCityGML_StartHandler1,ModelCityGML_EndHandler1);
//   state=XML_ParseFile(Interp,parser,gml,Path);

   XML_ParserFree(parser);

   /*Parse materials
   for (len=0;len<M->NMt;len++) {
      gml->Mt=&M->Mt[len];
      if ((gml->Fc=Model_FaceFind(gml->Model,&gml->Mt->Target[1],NULL))) {
         gml->Fc->Mt=data->Mt;
      }
   }*/

   /*Free associates parsing data structure*/
   free(gml);

   return(state);
}
