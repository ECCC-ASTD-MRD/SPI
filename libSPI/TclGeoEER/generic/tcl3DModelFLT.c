/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Affichage de modele 3D
 * Fichier      : tcl3DModel3DS.c
 * Creation     : Aout2007 - J.P. Gauthier
 *
 * Description  : Module de lecture de modele 3D en format 3DS
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
#ifdef HAVE_FLT

#include <stdlib.h>

#include "App.h"
#include "tcl3DModel.h"
#include "flt.h"

static T3DObject *GOBJ=NULL;
static int        NVR,NFCE,NOBJ,NMT,NSW;
static float      GCOL[4]={ 0.0,0.0,0.0,0.0 };

unsigned int ModelFLT_NodeCount(FltNode *Node,int Type);
int          ModelFLT_NodeProcess(T3DModel *M,FltNode *Node,FltFile *FLT);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_LoadFLT>
 * Creation     : Aout 2007 J.P. Gauthier
 *
 * But          : Lire un fichier MDL
 *
 * Parametres   :
 *   <M>        : Objet Model
 *   <Path>     : Path complet du fichier MDL
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Model_LoadFLT(Tcl_Interp* Interp,T3DModel *M,char *Path) {

   FltFile     *flt;
   unsigned int nobj;

   if (!(flt=fltOpen(Path))) {
      return(0);
   }

   fltParse(flt,0);

   if (!flt || !flt->header) {
      return(0);
   }

   GOBJ=NULL;
   NOBJ=-1;
   NMT=0;
   GCOL[0]=GCOL[1]=GCOL[2]=GCOL[3]=0.0;

   App_Log(DEBUG,"%s: Projection Type %i\n",__func__,flt->header->projectionType);
   App_Log(DEBUG,"%s: Ellipsoid Type %i\n",__func__,flt->header->earthEllipsoidModel);
   App_Log(DEBUG,"%s: Coordinates units %i\n",__func__,flt->header->coordUnits);
   App_Log(DEBUG,"%s: Origin %f,%f\n",__func__,flt->header->originDBLatitude,flt->header->originDBLongitude);
   App_Log(DEBUG,"%s: Extent %f,%f - %f,%f\n",__func__,flt->header->swDBLatitude,flt->header->swDBLongitude,flt->header->neDBLatitude,flt->header->neDBLongitude);

   nobj=ModelFLT_NodeCount((FltNode*)(flt->header),FLTRECORD_OBJECT);
   App_Log(DEBUG,"%s: Found %i object\n",__func__,nobj);
   Model_ObjectAdd(M,nobj);

   M->NMt=ModelFLT_NodeCount((FltNode*)(flt->header),FLTRECORD_MATERIAL);
   App_Log(DEBUG,"%s: Found %i material\n",__func__,M->NMt);
   M->NMt=1024;
   M->Mt=(TMaterial*)calloc(M->NMt,sizeof(TMaterial));

   ModelFLT_NodeProcess(M,(FltNode*)(flt->header),flt);

   fltClose(flt);
   fltFileFree(flt);

   return(1);
}

unsigned int ModelFLT_NodeCount(FltNode *Node,int Type) {

   unsigned int i,n=0;

   if (Node->type==Type) {
      n+=(Node->type==FLTRECORD_VERTEXLIST?((FltVertexList*)Node)->numVerts:1);
   }

   for (i=0;i<Node->numChildren;i++)
      n+=ModelFLT_NodeCount(Node->child[i],Type);

   return(n);
}

int ModelFLT_NodeProcess(T3DModel *M,FltNode *Node,FltFile *FLT) {

   FltExternalReference *ext=(FltExternalReference*)Node;
   FltFace              *face=(FltFace*)Node;
   FltVertexList        *vert=(FltVertexList*)Node;
   FltNode              *attr;
   FltMaterial          *mat;
//   FltMultiTexture      *tex,*mtex=(FltMultiTexture*)Node;
   char                  path[MAX_PATHLEN],file[MAX_PATHLEN];
   unsigned int          i,v,m,ok=1;
   TMaterial             tmat;

   attr=Node->attr;
   while(attr) {
      ModelFLT_NodeProcess(M,attr,FLT);
      attr=attr->next;
   }

   /*create instance definitions*/
   switch (Node->type) {
      case FLTRECORD_COMMENT:
         App_Log(DEBUG,"%s: FLTRECORD_COMMENT (%s)\n",__func__,((FltComment*)Node)->text);
         break;

      case FLTRECORD_EXTERNALREFERENCE:
         App_Log(DEBUG,"%s: FLTRECORD_EXTERNALREFERENCE\n",__func__);
         /*Check for older software that did not append ".flt" to the DB name*/
         path[0]=0;
         strcat(path,ext->path);

         if (!strstr(ext->path,".flt"))
            strcat(path,".flt");

         if (fltFindFile(FLT,path,file)) {
            App_Log(DEBUG,"%s: External reference (%s)\n",__func__,file);
/*
            e = (sglNode *)loadModelFLT(filepath, 0 );
            if(!e)
            {
               sglPrintWarning() << "sglLoadFile_flt::buildScene error: "
                                 << "Unable to open external reference: '"
                                 << fname << "'" << endl;
               delete ext_group;
               return;
            }

            ext_group->addChild( e );

            // set name to be the external ref file name
            ext_group->setName(fext->path);

            if (fltUserData)
               ext_group->setUserData( fltUserData );

            // in case no transform found
            ext_group->loadIdentity();

            // get the transformation matrix, if it exists in attributes
            find_node = fext->node.attr;

            while (find_node)
            {
               if (find_node->type == FLTRECORD_MATRIX)
               {
                  sglMat4f mat4;
                  FltMatrix * fmat = (FltMatrix *)find_node;

                  mat4.set(fmat->matrix[0], fmat->matrix[1],
                           fmat->matrix[2], fmat->matrix[3],
                           fmat->matrix[4], fmat->matrix[5],
                           fmat->matrix[6], fmat->matrix[7],
                           fmat->matrix[8], fmat->matrix[9],
                           fmat->matrix[10], fmat->matrix[11],
                           fmat->matrix[12], fmat->matrix[13],
                           fmat->matrix[14], fmat->matrix[15]);

                  ext_group->setMatrix(mat4);
                  break;
               }

               find_node = find_node->next;
            }

            if (parent)
               parent->addChild(ext_group);
         } else {
            delete ext_group;
            App_Log(ERROR,"%s: External reference (%s) not found\n",__func__,file);
*/
         }
         break;

      case FLTRECORD_MATRIX:
         App_Log(DEBUG,"%s: FLTRECORD_MATRIX\n,__func__");
/*
         glMatrixMode(GL_MODELVIEW);
         glPushMatrix();
         glMultMatrixf(((FltMatrix *)Node)->matrix);
*/
         break;

      case FLTRECORD_MESH:
         App_Log(DEBUG,"%s: FLTRECORD_MESH\n",__func__);
         break;

      case FLTRECORD_FACE:
         App_Log(DEBUG,"%s: FLTRECORD_FACE\n",__func__);

         /*Skip hidden faces*/
         if (!GOBJ->Fc || (face->miscFlags & FLTFACEMF_HIDDEN)) {
            break;
         }

         NFCE++;

         if (NFCE>GOBJ->NFc) {
            App_Log(ERROR,"%s: Too many faces (%i>%i)\n",__func__,NFCE,GOBJ->NFc);
            ok=0;
            break;
         }
         
         /*Face Color*/
         if (face->textureWhite) {
            GCOL[0]=GCOL[1]=GCOL[2]=GCOL[3]=1.0;
         } else if (face->miscFlags & FLTFACEMF_PACKEDCOLOR) {
            GCOL[0]=(face->packedColorPrimary & 0x000000ff)/255.0f;
            GCOL[1]=((face->packedColorPrimary >> 8) & 0x000000ff)/255.0f;
            GCOL[2]=((face->packedColorPrimary >> 16) & 0x000000ff)/255.0f;
            GCOL[3]=((face->packedColorPrimary >> 24) & 0x000000ff)/255.0f;
         } else {
            fltLookupColor(FLT,face->primaryColorIndex?face->colorNameIndex:face->primaryColorIndex,&GCOL[0],&GCOL[1],&GCOL[2],&GCOL[3]);
         }

         /*Materials*/
         memset(&tmat,0x0,sizeof(TMaterial));
         if (face->materialIndex!=-1 && (mat=fltLookupMaterial(FLT,face->materialIndex))) {
             tmat.Amb[0]=mat->ambientRed; tmat.Amb[1]=mat->ambientGreen; tmat.Amb[2]=mat->ambientBlue;
             tmat.Dif[0]=mat->diffuseRed; tmat.Dif[1]=mat->diffuseGreen; tmat.Dif[2]=mat->diffuseBlue;
             tmat.Spe[0]=mat->specularRed; tmat.Spe[1]=mat->specularGreen; tmat.Spe[2]=mat->specularBlue;
             tmat.Emi[0]=mat->emissiveRed; tmat.Emi[1]=mat->emissiveGreen; tmat.Emi[2]=mat->emissiveBlue;
             tmat.Shi=mat->shininess;
             tmat.Path[0]='\0';
             tmat.Name[0]='\0';
             tmat.Tex=0;
         }
         tmat.Amb[3]=1.0;tmat.Dif[3]=1.0;tmat.Spe[3]=1.0; tmat.Emi[3]=1.0;

         /*Check for textures*/
         if (face->texturePatternIndex!=-1) {
            FltTexture *t=fltLookupTexture(FLT,face->texturePatternIndex);
            if (t)
               strcpy(tmat.Path,t->ID);
         }

         /*Insert into model material list*/
         if (face->materialIndex!=-1 || face->texturePatternIndex!=-1) {
            for(m=0;m<NMT;m++) {
               if (memcmp(&M->Mt[m],&tmat,sizeof(TMaterial))==0) {
                  break;
               }
            }
            if (m>=NMT) {
               memcpy(&M->Mt[m],&tmat,sizeof(TMaterial));
               NMT++;
            }
            GOBJ->Fc[NFCE].Mt=&(M->Mt[m]);
         }

         /*Billboard Flags*/
         if (face->billboardFlags==FLTFACEBB_AXIALROTATE || face->billboardFlags==FLTFACEBB_POINTROTATE) {
            if (face->billboardFlags==FLTFACEBB_AXIALROTATE) {
            } else {
            }
         }
         break;

      case FLTRECORD_VERTEXLIST:
         App_Log(DEBUG,"%s: FLTRECORD_VERTEXLIST (%i)\n",__func__,vert->numVerts);

         if (!GOBJ->Fc || !GOBJ->Vr || NSW) {
            break;
         }

         if (NVR+vert->numVerts>GOBJ->NVr) {
            App_Log(ERROR,"%s: Too many vertices (%i>%i)\n",__func__,NVR+vert->numVerts,GOBJ->NVr);
            ok=0;
            break;
         }
         
         GOBJ->Fc[NFCE].NIdx=vert->numVerts;
         GOBJ->Fc[NFCE].Idx=(unsigned int*)calloc(GOBJ->Fc[NFCE].NIdx,sizeof(unsigned int));
         if (!GOBJ->Nr && (vert->list[0]->localFlags & FVHAS_NORMAL))  GOBJ->Nr=(Vect3f*)malloc(GOBJ->NVr*sizeof(Vect3f));
         if (!GOBJ->Tx && (vert->list[0]->localFlags & FVHAS_TEXTURE)) GOBJ->Tx=(Vect3f*)malloc(GOBJ->NVr*sizeof(Vect3f));
         if (!GOBJ->Cl && (vert->list[0]->localFlags & FVHAS_COLOR))   GOBJ->Cl=(Vect4f*)malloc(GOBJ->NVr*sizeof(Vect4f));

         /*Vertices*/
         for(v=0;v<vert->numVerts;v++) {
            i=NVR+v;

            if (GOBJ->Nr && (vert->list[v]->localFlags & FVHAS_NORMAL)) {
               GOBJ->Nr[i][0]=vert->list[v]->i; GOBJ->Nr[i][1]=vert->list[v]->j;GOBJ->Nr[i][2]=-vert->list[v]->k;
            }
            if (GOBJ->Tx && (vert->list[v]->localFlags & FVHAS_TEXTURE)) {
               GOBJ->Tx[i][0]=vert->list[v]->u; GOBJ->Tx[i][1]=-vert->list[v]->v;GOBJ->Tx[i][2]=0.0;
            }
            if (GOBJ->Cl &&(vert->list[v]->localFlags & FVHAS_COLOR)) {
               fltLookupColor(FLT,vert->list[v]->colorIndex,&GOBJ->Cl[i][0],&GOBJ->Cl[i][1],&GOBJ->Cl[i][2],&GOBJ->Cl[i][3]);
            }

            if (GOBJ->Vr) {
               GOBJ->Vr[i][0]=vert->list[v]->x;GOBJ->Vr[i][1]=vert->list[v]->y;GOBJ->Vr[i][2]=vert->list[v]->z;
            } else {
               App_Log(ERROR,"%s: Object vertices list has not been initialized (%i)\n",__func__,GOBJ->NVr);
            }
            App_Log(DEBUG,"%s: Vertex(%i) (%f,%f,%f)\n",__func__,v,vert->list[v]->x,vert->list[v]->y,vert->list[v]->z);

            GOBJ->Fc[NFCE].Idx[v]=i;
         }
         NVR+=vert->numVerts;
         break;

      case FLTRECORD_MULTITEXTURE:
/*
         glEnable(GL_TEXTURE_2D);

         if (mtex->mask & FLTMT_HASLAYER1) {
            tex=fltLookupTexture(FLT,mtex->layer[0].index);
            glBindTexture(GL_TEXTURE_2D,gggg);
            glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
         }
*/
//jusqua 7
         break;

      case FLTRECORD_INSTANCEDEFINITION:
         App_Log(DEBUG,"%s: FLTRECORD_INSTANCEDEFINITION\n",__func__);
        break;
      case FLTRECORD_INSTANCEREFERENCE:
         App_Log(DEBUG,"%s: FLTRECORD_INSTANCEREFERENCE\n",__func__);
         break;
      case FLTRECORD_LOD:
         App_Log(DEBUG,"%s: FLTRECORD_LOD\n",__func__);
         break;
      case FLTRECORD_SWITCH:
         App_Log(DEBUG,"%s: FLTRECORD_SWITCH\n",__func__);
         /*We have to bypass the switch (no clue what they're used for*/
         NSW=1;
         break;
      case FLTRECORD_DOF:
         App_Log(DEBUG,"%s: FLTRECORD_DOF\n",__func__);
         break;

      case FLTRECORD_OBJECT:
         NOBJ++;
         NSW=0;
         App_Log(DEBUG,"%s: FLTRECORD_OBJECT (%i)\n",__func__,NOBJ);
         GOBJ=&(M->Obj[NOBJ]);

         if (NOBJ>M->NObj) {
            ok=0;
            App_Log(ERROR,"%s: Too many objects (%i>%i)\n",__func__,NOBJ,M->NObj);    
            break;
         }
         NFCE=ModelFLT_NodeCount(Node,FLTRECORD_FACE);
         Model_ObjectFaceAdd(GOBJ,NFCE);
         NFCE=-1;
         App_Log(DEBUG,"%s: Found %i Face\n",__func__,GOBJ->NFc);
         NVR=0;
         GOBJ->NVr=ModelFLT_NodeCount(Node,FLTRECORD_VERTEXLIST);
         GOBJ->Vr=(Vect3f*)calloc(GOBJ->NVr,sizeof(Vect3f));
         App_Log(DEBUG,"%s: Found %i Vertex\n",__func__,GOBJ->NVr);
         break;

      case FLTRECORD_GROUP:
         App_Log(DEBUG,"%s: FLTRECORD_GROUP\n",__func__);
         break;
      case FLTRECORD_BSP:
         App_Log(DEBUG,"%s: FLTRECORD_BSP\n",__func__);
         break;
   }

   // Parse tree children
   if (ok) {
      for (i=0;i<Node->numChildren;i++)
         if (!(ok=ModelFLT_NodeProcess(M,Node->child[i],FLT))) {
            break;
         }
   }

   return(ok);
}

#endif