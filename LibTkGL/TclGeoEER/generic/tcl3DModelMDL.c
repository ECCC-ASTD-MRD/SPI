/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Affichage de modele 3D
 * Fichier      : tcl3DModelMDL.c
 * Creation     : Aout2007 - J.P. Gauthier
 *
 * Description  : Module de lecture de modele 3D en format MDL
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

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_LoadMDL>
 * Creation     : Janvier 2003 J.P. Gauthier
 *
 * But          : Lire un fichier MDL
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
int Model_LoadMDL(Tcl_Interp* Interp,T3DModel *M,char *Path) {

   int       i,m,f;
   T3DObject *obj;
   FILE      *file;

   if (!(file=fopen(Path,"r"))) {
      return(0);
   }

   /*Material list*/
   /*Number of material*/
   f=fread(&M->NMt,sizeof(int),1,file);
   if (M->NMt>256 || M->NMt<0) {
      fclose(file);
      M->NMt=0;
      return(0);
   }

#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Model_LoadMDL: M->NMt=%i\n",M->NMt);
#endif

   if (M->NMt<=0) {
      M->NMt=1;
      M->Mt=(TMaterial*)malloc(sizeof(TMaterial));
      M->Mt[0].Amb[0]=0.1;M->Mt[0].Amb[1]=0.1;M->Mt[0].Amb[2]=0.1;M->Mt[0].Amb[3]=1.0;
      M->Mt[0].Dif[0]=0.8;M->Mt[0].Dif[1]=0.8;M->Mt[0].Dif[2]=0.8;M->Mt[0].Dif[3]=1.0;
      M->Mt[0].Emi[0]=0.1;M->Mt[0].Emi[1]=0.1;M->Mt[0].Emi[2]=0.1;M->Mt[0].Emi[3]=1.0;
      M->Mt[0].Spe[0]=0.1;M->Mt[0].Spe[1]=0.6;M->Mt[0].Spe[2]=0.6;M->Mt[0].Spe[3]=1.0;
      M->Mt[0].Shi=255;
      M->Mt[0].Dif[3]=1.0;
      M->Mt[0].Tex=0;
      M->Mt[0].Path[0]='\0';
  } else {
      M->Mt=(TMaterial*)malloc(M->NMt*sizeof(TMaterial));
      for (i=0; i<M->NMt; i++) {
        f=fread(&M->Mt[i].Amb,sizeof(float),3,file);
        f=fread(&M->Mt[i].Dif,sizeof(float),3,file);
        f=fread(&M->Mt[i].Emi,sizeof(float),3,file);
        f=fread(&M->Mt[i].Spe,sizeof(float),3,file);
        f=fread(&M->Mt[i].Shi,sizeof(float),1,file);
        f=fread(&M->Mt[i].Dif[3],sizeof(float),1,file);
        M->Mt[i].Tex=0;
        M->Mt[i].Path[0]='\0';
      }
   }

   Model_ObjectAdd(M,1);
   obj=&M->Obj[M->NObj-1];

   /*Vertex list*/
   /*Number of vertex*/
   f=fread(&obj->NVr,sizeof(int),1,file);
#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Model_LoadMDL: M->NVr=%i\n",obj->NVr);
#endif

   /*Format of vertex*/
   f=fread(&obj->Format,sizeof(int),1,file);
#ifdef DEBUG
   fprintf(stdout,"(DEBUG) Model_LoadMDL: M->Format=%i\n",obj->Format);
#endif

   if (obj->NVr<=0 || obj->Format<=0) {
      fprintf(stderr,"\n(ERROR) Model_LoadMDL : Invalid vertex format or number");
      fclose(file);
      return(0);
   }

   /*Allocate data*/
   if (obj->Format==F3VNT) obj->Tx=(Vect3f*)malloc(obj->NVr*sizeof(Vect3f));
   if (obj->Format>=F3VN)  obj->Nr=(Vect3f*)malloc(obj->NVr*sizeof(Vect3f));
                           obj->Vr=(Vect3f*)malloc(obj->NVr*sizeof(Vect3f));

   /*Vertex list*/
   for (i=0;i<obj->NVr;i++) {
                              f=fread(&obj->Vr[i],sizeof(Vect3f),1,file);
      if (obj->Format>=F3VN)  f=fread(&obj->Nr[i],sizeof(Vect3f),1,file);
      if (obj->Format==F3VNT) f=fread(&obj->Tx[i],sizeof(Vect3f),1,file);
   }

   /*Faces list*/
   /*Number of faces*/
   f=fread(&obj->NFc,sizeof(int),1,file);
#ifdef DEBUG
   fprintf(stderr,"(DEBUG) Model_LoadMDL: M->NFc=%i\n",obj->NFc);
#endif

   obj->Fc=(TFace*)malloc(obj->NFc*sizeof(TFace));
   for (i=0;i<obj->NFc;i++) {
      f=fread(&m,sizeof(int),1,file);
      f=fread(&obj->Fc[i].NIdx,sizeof(unsigned char),1,file);

      obj->Fc[i].Mt=m<0?&M->Mt[0]:&M->Mt[m];
      obj->Fc[i].Idx=(unsigned int*)malloc(obj->Fc[i].NIdx*sizeof(unsigned int));
      f=fread(obj->Fc[i].Idx,sizeof(int),obj->Fc[i].NIdx,file);
   }
   fclose(file);

   return(1);
}
