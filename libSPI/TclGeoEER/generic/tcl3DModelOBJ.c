/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Affichage de modele 3D
 * Fichier      : tcl3DModelOBJ.c
 * Creation     : Janvier 2015 - J.P. Gauthier
 *
 * Description  : Module de lecture de modele 3D en format OBJ (Wavefront)
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

#include "App.h"
#include "tcl3DModel.h"

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Model_LoadOBJL>
 * Creation     : Janvier 2015 J.P. Gauthier
 *
 * But          : Lire un fichier OBJ
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
int Model_LoadOBJ(Tcl_Interp* Interp,T3DModel *M,char *Path) {

   char      *tok,*save=NULL;
   float      w;
   int        v,n,t,f,nvt,nvn,tx,nr;
   Vect3f    *vt,*vn;
   T3DObject *obj;
   FILE      *file;
   char      buf[512];
   
   if (!(file=fopen(Path,"r"))) {
      return(0);
   }

   obj=Model_ObjectAdd(M,1);
 
   // Pre-pass to count objects
   nvn=nvt=0;
   while(!feof(file)) {
      if (!fgets(buf,512,file)) break;
      
      if (buf[0]=='v' && buf[1]==' ') obj->NVr++;
      if (buf[0]=='v' && buf[1]=='t') nvt++;
      if (buf[0]=='v' && buf[1]=='n') nvn++;
      if (buf[0]=='f')                obj->NFc++;
   }
   
   if (!obj->NVr) {
      fclose(file);
      return(0);
   }
   
   // Allocate needed arrays
   vt=(Vect3f*)malloc(nvt*sizeof(Vect3f));
   vn=(Vect3f*)malloc(nvn*sizeof(Vect3f));

   obj->Vr=(Vect3f*)malloc(obj->NVr*sizeof(Vect3f));
   obj->Tx=(Vect3f*)malloc(obj->NVr*sizeof(Vect3f));
   obj->Nr=(Vect3f*)malloc(obj->NVr*sizeof(Vect3f));
   obj->Fc=(TFace*)malloc(obj->NFc*sizeof(TFace));

   // Read pass
   rewind(file);
   
   v=f=n=t=0;
   while(!feof(file)) {
      if (!fgets(buf,512,file)) break;

      switch(buf[0]) {
         case '#': break;
         case 'v': // Vertex info
                   switch(buf[1]) {
                      case ' ': // Vertex position
                                sscanf(&buf[2],"%f %f %f %f",&obj->Vr[v][0],&obj->Vr[v][1],&obj->Vr[v][2],&w);
                                v++;
                                break;
                      case 't': // Vertex texture coordinate
                                sscanf(&buf[3],"%f %f %f",&vt[t][0],&vt[t][1],&vt[t][2]);
                                t++;
                                break;
                      case 'n': // Vertex normal
                                sscanf(&buf[3],"%f %f %f",&vn[n][0],&vn[n][1],&vn[n][2]);
                                n++;
                                break;
                      case 'p': // Vertex parameters
                                break;
                   }
                   break;
                   
         case 'f': // Face info
                   obj->Fc[f].NIdx=strtok_count(&buf[1],' ');
                   if (obj->Fc[f].NIdx<3)
                      continue;
                   obj->Fc[f].Idx=(unsigned int*)malloc(obj->Fc[f].NIdx*sizeof(unsigned int));
                   
                   // Parse all tokens
                   tok=strtok_r(&buf[1]," ",&save);
                   n=0;
                   while(tok && n<obj->Fc[f].NIdx) {
                      sscanf(tok,"%i/%i/%i",&obj->Fc[f].Idx[n],&tx,&nr);
                      obj->Fc[f].Idx[n]--;
                      Vect_Assign(obj->Tx[obj->Fc[f].Idx[n]],vt[tx-1]);
                      Vect_Assign(obj->Nr[obj->Fc[f].Idx[n]],vn[nr-1]);
                      tok=strtok_r(NULL," ",&save);
                      n++;
                   }
                   f++;
                   break;
      }
   }
   fclose(file);

   free(vt);
   free(vn);
   
   return(1);
}
