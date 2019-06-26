/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de rendue donnees.
 * Fichier   : Render.c
 * Creation  : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Fonctions d'affichage de donnees.
 *
 * Remarques :
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
 *==============================================================================
 */

#include "App.h"
#include "Triangle.h"
#include "Data_FF.h"

int Data_RenderShaderParticle(TData *Field,ViewportItem *VP,Projection *Proj);
int Data_RenderShaderTexture(TData *Field,ViewportItem *VP,Projection *Proj);
int Data_RenderShaderStream(TData *Field,ViewportItem *VP,Projection *Proj);
int Data_RenderShaderMesh(TData *Field,ViewportItem *VP,Projection *Proj);
int Data_RenderShaderRayCasting(TData *Field,ViewportItem *VP,Projection *Proj);

extern int Data_RenderTexture(TData *Field,ViewportItem *VP,Projection *Proj);
/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderShaderParticle>
 * Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue les calculs et l'affichage pour le type particule sur le GPU.
 *
 * Parametres :
 *  <Champs>  : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_RenderShaderParticle(TData *Field,ViewportItem *VP,Projection *Proj) {

   int     n;
   double  val=0.0;
   float   min,rng,inter[DATASPEC_MAX];
   Vect3d *pos;

   GLuint      tx[3],att0;
   GLhandleARB prog;
   Tk_PhotoImageBlock img;

   if (!Field->GRef || !Field->GPos || !Field->GPos->Pos[Field->Def->Level]) {
      return(0);
   }

   /*Do we need transparency*/
   if (Field->Spec->Map->Alpha || Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }
   glColor4f(1.0,1.0,1.0,Field->Spec->Alpha/100.0);

   min=Field->Spec->Min;
   rng=Field->Spec->Max-Field->Spec->Min;
   pos=Field->GPos->Pos[Field->Def->Level];
   prog=GLRender->Prog[PROG_FIELD];

   glUseProgramObjectARB(prog);
   glGenTextures(3,tx);

   /*Setup 1D Colormap Texture*/
   glActiveTexture(GL_TEXTURE0);
   glBindTexture(GL_TEXTURE_1D,tx[0]);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
   glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,Field->Spec->Map->NbPixels,0,GL_RGBA,GL_UNSIGNED_BYTE,Field->Spec->Map->Color);

   /*Setup 1D Interval Texture*/
   glActiveTexture(GL_TEXTURE1);
   glBindTexture(GL_TEXTURE_RECTANGLE_ARB,tx[1]);
   if (Field->Spec->InterNb) {
      glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
      glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
      for(n=0;n<Field->Spec->InterNb;n++) inter[n]=Field->Spec->Inter[n];
      glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GL_FLOAT_R32_NV,Field->Spec->InterNb,1,0,GL_LUMINANCE,GL_FLOAT,inter);
   }

   /*Setup point sprites*/
   if (Field->Spec->SpriteImg) {
      glActiveTexture(GL_TEXTURE2);
      glBindTexture(GL_TEXTURE_2D,tx[2]);
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
      Tk_PhotoGetImage(Field->Spec->SpriteImg,&img);
      glTexImage2D(GL_TEXTURE_2D,0,img.pixelSize,img.width,img.height,0,GL_RGBA,GL_UNSIGNED_BYTE,(GLvoid*)img.pixelPtr);

      glEnable(GL_POINT_SPRITE_ARB);
      glTexEnvi(GL_POINT_SPRITE_ARB,GL_COORD_REPLACE_ARB,GL_TRUE);
      glEnable(GL_VERTEX_PROGRAM_POINT_SIZE);
      glEnable(GL_BLEND);
      glDepthMask(GL_FALSE);
      glPointSize(1.0);
      glUniform1iARB(GLShader_UniformGet(prog,"Sprite"),2);
   } else {
      glPointSize(Field->Spec->RenderParticle+0.1);
   }

   glUniform1iARB(GLShader_UniformGet(prog,"Colormap"),0);
   glUniform1iARB(GLShader_UniformGet(prog,"Interval"),1);
   glUniform1fARB(GLShader_UniformGet(prog,"Cylindric"),(Proj->Type->Def==PROJCYLIN?Proj->L:-999.0));
   glUniform1fARB(GLShader_UniformGet(prog,"Min"),min);
   glUniform1fARB(GLShader_UniformGet(prog,"Range"),rng);
   glUniform1iARB(GLShader_UniformGet(prog,"Nb"),Field->Spec->InterNb);
   glUniform1iARB(GLShader_UniformGet(prog,"Above"),Field->Spec->MapAbove);
   glUniform1iARB(GLShader_UniformGet(prog,"Bellow"),Field->Spec->MapBellow);
   att0=GLShader_AttribGet(prog,"Vd");

   /*Projeter les particules*/
   glEnable(GL_TEXTURE_2D);
   glBegin(GL_POINTS);
   for(n=0;n<FSIZE2D(Field->Def);n++) {
      Def_Get(Field->Def,0,n,val);
      glVertexAttrib1fARB(att0,val);
      glNormal3dv(pos[n]);
      glVertex3dv(pos[n]);
   }
   glEnd();

   glDeleteTextures(3,tx);
   glUseProgramObjectARB(0);
   glActiveTexture(GL_TEXTURE0);
   glDisable(GL_BLEND);

   glDisable(GL_POINT_SPRITE_ARB);
   glDisable(GL_VERTEX_PROGRAM_POINT_SIZE_NV);
   glTexEnvi(GL_POINT_SPRITE_ARB,GL_COORD_REPLACE_ARB,GL_FALSE);
   glDepthMask(GL_TRUE);
   glDisable(GL_ALPHA_TEST);

   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderShaderMesh>
 * Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue les calculs et l'affichage pour le type triangle mesh sur le GPU.
 *
 * Parametres :
 *  <Champs>  : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_RenderShaderMesh(TData *Field,ViewportItem *VP,Projection *Proj) {

   int          n,mask;
   unsigned int idx[3];
   double       val=0.0;
   float        min,rng,inter[DATASPEC_MAX];
   Vect3d      *pos;
   Vect3d       b,p,p0,p1,p2;

   GLuint      tx[2],att0;
   GLhandleARB prog;

   if (!Field->GRef || !Field->GPos || !Field->GPos->Pos[Field->Def->Level]) {
      return(0);
   }

   glDisable(GL_CULL_FACE);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   
   // Do we need transparency
   if (Field->Spec->Map->Alpha || Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }
   glColor4f(1.0,1.0,1.0,Field->Spec->Alpha/100.0);

   min=Field->Spec->Min;
   rng=Field->Spec->Max-Field->Spec->Min;
   pos=Field->GPos->Pos[Field->Def->Level];
   prog=GLRender->Prog[PROG_FIELD];

   glUseProgramObjectARB(prog);
   glGenTextures(2,tx);

   // Setup 1D Colormap Texture
   glActiveTexture(GL_TEXTURE0);
   glBindTexture(GL_TEXTURE_1D,tx[0]);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
   glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,Field->Spec->Map->NbPixels,0,GL_RGBA,GL_UNSIGNED_BYTE,Field->Spec->Map->Color);

   // Setup 1D Interval Texture
   glActiveTexture(GL_TEXTURE1);
   glBindTexture(GL_TEXTURE_RECTANGLE_ARB,tx[1]);
   if (Field->Spec->InterNb) {
      glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
      glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
      
      for(n=0;n<Field->Spec->InterNb;n++) inter[n]=Field->Spec->Inter[n];
      glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GL_FLOAT_R32_NV,Field->Spec->InterNb,1,0,GL_LUMINANCE,GL_FLOAT,inter);
   }
   glUniform1iARB(GLShader_UniformGet(prog,"Colormap"),0);
   glUniform1iARB(GLShader_UniformGet(prog,"Interval"),1);
   glUniform1fARB(GLShader_UniformGet(prog,"Cylindric"),(Proj->Type->Def==PROJCYLIN?Proj->L:-999.0));
   glUniform1fARB(GLShader_UniformGet(prog,"Min"),min);
   glUniform1fARB(GLShader_UniformGet(prog,"Range"),rng);
   glUniform1iARB(GLShader_UniformGet(prog,"Nb"),Field->Spec->InterNb);
   glUniform1iARB(GLShader_UniformGet(prog,"Above"),Field->Spec->MapAbove);
   glUniform1iARB(GLShader_UniformGet(prog,"Bellow"),Field->Spec->MapBellow);
   att0=GLShader_AttribGet(prog,"Vd");

   mask=Field->Spec->Mask && Field->Def->Mask;

   glBegin(GL_TRIANGLES);
   if (Field->Spec->InterpDegree[0]=='L') {
      for(n=0;n<Field->GRef->NIdx-3;n+=3) {
         idx[0]=Field->GRef->Idx[n];
         idx[1]=Field->GRef->Idx[n+1];
         idx[2]=Field->GRef->Idx[n+2];
         
         // Check for mask
         if (mask && !(Field->Def->Mask[idx[0]] && Field->Def->Mask[idx[1]] && Field->Def->Mask[idx[2]])) {
            continue;
         }
         Def_GetMod(Field->Def,idx[0],val);
         glVertexAttrib1fARB(att0,val);
         glVertex3dv(pos[idx[0]]);
         Def_GetMod(Field->Def,idx[1],val);
         glVertexAttrib1fARB(att0,val);
         glVertex3dv(pos[idx[1]]);
         Def_GetMod(Field->Def,idx[2],val);
         glVertexAttrib1fARB(att0,val);
         glVertex3dv(pos[idx[2]]);
      }
   } else {
      for(n=0;n<Field->GRef->NIdx-3;n+=3) {
         idx[0]=Field->GRef->Idx[n];
         idx[1]=Field->GRef->Idx[n+1];
         idx[2]=Field->GRef->Idx[n+2];
         
         Vect_Init(b,1.0/3.0,1.0/3.0,1.0/3.0);
         Bary_InterpPos(b,p,pos[idx[0]],pos[idx[1]],pos[idx[2]]);
         Vect_Init(b,0.0,0.5,0.5);
         Bary_InterpPos(b,p0,pos[idx[0]],pos[idx[1]],pos[idx[2]]);
         Vect_Init(b,0.5,0.0,0.5);
         Bary_InterpPos(b,p1,pos[idx[0]],pos[idx[1]],pos[idx[2]]);
         Vect_Init(b,0.5,0.5,0.0);
         Bary_InterpPos(b,p2,pos[idx[0]],pos[idx[1]],pos[idx[2]]);

         if (!mask || Field->Def->Mask[idx[0]]) {         
            Def_GetMod(Field->Def,idx[0],val);
            glVertexAttrib1fARB(att0,val);
            glVertex3dv(pos[idx[0]]);
            glVertex3dv(p);
            glVertex3dv(p1);
            glVertex3dv(pos[idx[0]]);
            glVertex3dv(p);
            glVertex3dv(p2);
         }

         if (!mask || Field->Def->Mask[idx[1]]) {         
            Def_GetMod(Field->Def,idx[1],val);
            glVertexAttrib1fARB(att0,val);
            glVertex3dv(pos[idx[1]]);
            glVertex3dv(p);
            glVertex3dv(p0);
            glVertex3dv(pos[idx[1]]);
            glVertex3dv(p);
            glVertex3dv(p2);
         }

         if (!mask || Field->Def->Mask[idx[2]]) {         
            Def_GetMod(Field->Def,idx[2],val);
            glVertexAttrib1fARB(att0,val);
            glVertex3dv(pos[idx[2]]);
            glVertex3dv(p);
            glVertex3dv(p0);
            glVertex3dv(pos[idx[2]]);
            glVertex3dv(p);
            glVertex3dv(p1);
         }
      }
   }
   glEnd();

   glDeleteTextures(2,tx);
   glUseProgramObjectARB(0);
   glActiveTexture(GL_TEXTURE0);
   glDisable(GL_BLEND);

   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderShaderTexture>
 * Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage de la texture du champs sur le GPU.
 *
 * Parametres  :
 *  <Field>    : Champs
 *  <VP>       : Parametres du viewport
 *  <Proj>     : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_RenderShaderTexture(TData *Field,ViewportItem *VP,Projection *Proj){

   int     n,i,j,idxk,idx0,idx1,ox=0,dp,dn,mask=0;
   float   min,rng,inter[DATASPEC_MAX],fi,fj;
   Vect3d *pos,normal,v[2];
   float  *buf=NULL;
   char   *ptr;

   GLuint      tx[4];
   GLhandleARB prog;

   if (GLRender->Resolution>2) {
      return(0);
   }
   
   if (!Field->GRef || !Field->GPos || Field->GRef->Grid[0]=='Y') {
      return(0);
   }

   if (Field->GRef->Grid[0]=='M') {
      Data_RenderShaderMesh(Field,VP,Proj);
      return(1);
   }

   if (Field->Spec->Stipple) {
      glEnable(GL_POLYGON_STIPPLE);
      glPolygonStipple(Field->Spec->Stipple->Data);
   }
   
   if ((Proj->Type->Def!=PROJGLOBE) && Field->GRef->Grid[0]!='V') {
      glEnable(GL_CULL_FACE);
   } else {
      glDisable(GL_CULL_FACE);
   }
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

   // Do we need transparency
   if (Field->Spec->Map->Alpha || (Field->Spec->Alpha<100 && !(Field->Spec->RenderVector>ARROW))) {
      glEnable(GL_BLEND);
   }
   glColor4f(1.0,1.0,1.0,Field->Spec->Alpha/100.0);

   min=Field->Spec->Min;
   rng=fabsf(Field->Spec->Max-Field->Spec->Min);
   pos=&Field->GPos->Pos[Field->Def->Level][Field->Def->Idx];
   if (Field->GRef->Grid[0]=='U') {
      idxk=FSIZE2D(Field->SDef[0])*Field->Def->Level+Field->Def->Idx;
      Def_PointerMode(Field->SDef[0],idxk,ptr);
   } else {
      idxk=FSIZE2D(Field->Def)*Field->Def->Level;
      Def_PointerMode(Field->Def,idxk,ptr);
   }

//   prog=GLShader_Load("/home/afsr/005/eer_Tools/LibTkGL/TclData","FieldTex");
   prog=GLRender->Prog[PROG_FIELDTEX];
   glUseProgramObjectARB(prog);
   glGenTextures(4,tx);

   //Setup 1D Colormap Texture
   glActiveTexture(GL_TEXTURE0);
   glBindTexture(GL_TEXTURE_1D,tx[0]);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
   glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,Field->Spec->Map->NbPixels,0,GL_RGBA,GL_UNSIGNED_BYTE,Field->Spec->Map->Color);

   // Setup 1D Interval Texture
   glActiveTexture(GL_TEXTURE1);
   glBindTexture(GL_TEXTURE_RECTANGLE_ARB,tx[1]);
   if (Field->Spec->InterNb) {
      glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
      glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
      
      for(n=0;n<Field->Spec->InterNb;n++) inter[n]=Field->Spec->Inter[n];
      glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GL_FLOAT_R32_NV,Field->Spec->InterNb,1,0,GL_LUMINANCE,GL_FLOAT,inter);
   }

   // Setup 2D Data Texture
   glActiveTexture(GL_TEXTURE2);
   glBindTexture(GL_TEXTURE_RECTANGLE_ARB,tx[2]);
   glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
   glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MAG_FILTER,GL_NEAREST);

   // Why the hell GL_FLOAT_R32_NV accepts only Float32, I don't know, here's the quick fix
   if (Field->Def->Type!=TD_Float32) {
      if ((buf=(float*)malloc(Field->Def->NI*Field->Def->NJ*sizeof(float)))) {
         for(i=0;i<Field->Def->NI*Field->Def->NJ;i++) {
             Def_GetMod(Field->Def,idxk+i,buf[i]);
         }
      }
      ptr=(char*)buf;
   } 
   
   // Seems that GL_FLOAT_R32_NV is not recognized on ATI cards but GL_INTENSITY_FLOAT32_ATI is recognized on NVidia
   glTexImage2D(GL_PROXY_TEXTURE_RECTANGLE_ARB,0,GLRender->Vendor==ATI?GL_INTENSITY_FLOAT32_ATI:GL_FLOAT_R32_NV,Field->Def->NI,Field->Def->NJ,0,GL_LUMINANCE,GL_FLOAT,ptr);
   glGetTexLevelParameteriv(GL_PROXY_TEXTURE_RECTANGLE_ARB,0,GL_TEXTURE_WIDTH,&dp);
   if (dp) {
      glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GLRender->Vendor==ATI?GL_INTENSITY_FLOAT32_ATI:GL_FLOAT_R32_NV,Field->Def->NI,Field->Def->NJ,0,GL_LUMINANCE,GL_FLOAT,ptr);
      if (buf) free(buf);
   
      // Setup 2D Mask Texture
      if (Field->Spec->Mask && Field->Def->Mask) {
         mask=1;
         glActiveTexture(GL_TEXTURE3);
         glBindTexture(GL_TEXTURE_RECTANGLE_ARB,tx[3]);
         glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
         glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
         glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GL_ALPHA,Field->Def->NI,Field->Def->NJ,0,GL_ALPHA,GL_BYTE,Field->Def->Mask);         
      }
   } else {
      App_Log(WARNING,"%s: Texture is too big to fit on GPU, switching to software renderer\n",__func__);
      glDeleteTextures(4,tx);
      glUseProgramObjectARB(0);
      if (buf) free(buf);
   //   GLShader_UnInstall(prog);

      glActiveTexture(GL_TEXTURE0);
      glEnable(GL_CULL_FACE);
      glDisable(GL_BLEND);
      return(Data_RenderTexture(Field,VP,Proj));
   }

   glUniform1iARB(GLShader_UniformGet(prog,"Colormap"),0);
   glUniform1iARB(GLShader_UniformGet(prog,"Interval"),1);
   glUniform1iARB(GLShader_UniformGet(prog,"Data"),2);
   glUniform1iARB(GLShader_UniformGet(prog,"Mask"),3);
   glUniform1iARB(GLShader_UniformGet(prog,"IsMask"),mask);
   glUniform1fARB(GLShader_UniformGet(prog,"Cylindric"),(Proj->Type->Def==PROJCYLIN?Proj->L:-999.0));
   glUniform1fARB(GLShader_UniformGet(prog,"Min"),min);
   glUniform1fARB(GLShader_UniformGet(prog,"Range"),rng);
   glUniform1iARB(GLShader_UniformGet(prog,"Nb"),Field->Spec->InterNb);
   glUniform1iARB(GLShader_UniformGet(prog,"Bi"),(Field->Spec->InterpDegree[0]=='N'?0:1));
   glUniform1iARB(GLShader_UniformGet(prog,"Above"),Field->Spec->MapAbove);
   glUniform1iARB(GLShader_UniformGet(prog,"Bellow"),Field->Spec->MapBellow);
   glUniform1iARB(GLShader_UniformGet(prog,"Sun"),Proj->Sun);

   // Grille avec loop sur la longitude
   if (Field->GRef->Type&GRID_WRAP && Proj->Type->Def!=PROJPLANE) {
      ox=1;
   }

   // Resolution selon la dimension des cellules (mid-grid) et la vue
   dp=Proj->PixDist/Field->GRef->Distance(Field->GRef,Field->Def->NI>>1,Field->Def->NJ>>1,(Field->Def->NI>>1)+1,Field->Def->NJ>>1)*20;
   dp=(dp<1 || Field->GRef->Grid[0]=='V'|| (Proj->Ref && Proj->Ref->Type&GRID_PSEUDO))?1:dp;
   
   if (Proj->Type->Def==PROJCYLIN || Field->GRef->Grid[0]=='X') {
      dp=CLAMP(dp,1,2);
   } else {      
      dp=CLAMP(dp,1,20);
   }
   dn=dp*Field->Def->NI;
   
   // Process gridpoints
   for(j=0;j<Field->Def->NJ;j+=dp) {
      idx0=j*Field->Def->NI;
      
      // If next iteration goes over, use the last j
      if (j>=Field->Def->NJ-dp) dn=(Field->Def->NJ-j-1)*Field->Def->NI;

      glBegin(GL_QUAD_STRIP);
      for(i=0;i<(Field->Def->NI+dp);i+=dp) {

         fi=i;
         
         // If the next index is over the grid limit
         if (i>=Field->Def->NI) {
            if (ox) {
               // If the grid wraps around, use the first point
               idx0-=Field->Def->NI;
            } else {
               // If not, use the last point
               fi=Field->Def->NI-1;
               idx0=(j+1)*Field->Def->NI-1;
            }
         }
         
         idx1=idx0+dn;
         
         fi+=0.5f;
         fj=(float)j+0.5f;
            
         glTexCoord2f(fi,fj+dp);
         for(int i=0;i<3;i++){
            v[0][i]=pos[((idx1+1)%Field->Def->NI==0)?idx1:idx1+1][i] - pos[(idx1%Field->Def->NI==0)?idx1:idx1-1][i];
            v[1][i]=pos[(idx1%(Field->Def->NI*Field->Def->NJ)>Field->Def->NI*(Field->Def->NJ-1))?idx1:idx1+Field->Def->NI][i] - pos[(idx1%(Field->Def->NI*Field->Def->NJ)<Field->Def->NI)?idx1:idx1-Field->Def->NI][i];
         }
         Vect_CrossProduct(normal,v[0],v[1]);
         glNormal3dv(normal);
         glVertex3dv(pos[idx1]);

         glTexCoord2f(fi,fj);
         for(int i=0;i<3;i++){
            v[0][i]=pos[((idx0+1)%Field->Def->NI==0)?idx0:idx0+1][i]-pos[(idx0%Field->Def->NI==0)?idx0:idx0-1][i];
            v[1][i]=pos[(idx0%(Field->Def->NI*Field->Def->NJ)>Field->Def->NI*(Field->Def->NJ-1))?idx0:idx0+Field->Def->NI][i] - pos[(idx0%(Field->Def->NI*Field->Def->NJ)<Field->Def->NI)?idx0:idx0-Field->Def->NI][i];
         }
         Vect_CrossProduct(normal,v[0],v[1]);
         glNormal3dv(normal);
         glVertex3dv(pos[idx0]);

         idx0+=dp;
      }
      glEnd();
   }
   glDeleteTextures(4,tx);
   glUseProgramObjectARB(0);
//   GLShader_UnInstall(prog);

   glActiveTexture(GL_TEXTURE0);
   glDisable(GL_POLYGON_STIPPLE);
   glEnable(GL_CULL_FACE);
   glDisable(GL_BLEND);

   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderShaderRayCasting>
 * Creation : Juin 2019 - A. Germain - CMC
 *
 * But      : Effectue l'affichage du chmaps en trois dimension sur le GPU.
 *
 * Parametres  :
 *  <Field>    : Champs
 *  <VP>       : Parametres du viewport
 *  <Proj>     : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_RenderShaderRayCasting(TData *Field,ViewportItem *VP,Projection *Proj){
   GLuint tx[2];
   GLhandleARB prog;

   prog=GLRender->Prog[PROG_RAYCASTING];
   glUseProgramObjectARB(prog);
   glGenTextures(2,tx);

   /*Setup 1D Colormap Texture*/
   glActiveTexture(GL_TEXTURE0);
   glBindTexture(GL_TEXTURE_1D,tx[0]);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
   glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,Field->Spec->Map->NbPixels,0,GL_RGBA,GL_UNSIGNED_BYTE,Field->Spec->Map->Color);
   glUniform1iARB(GLShader_UniformGet(prog,"Colormap"),0);

   glActiveTexture(GL_TEXTURE1);
   glBindTexture(GL_TEXTURE_3D,tx[1]);
   glTexParameteri(GL_TEXTURE_3D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_3D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
   glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
   glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_BORDER);

   int width=Field->Def->NI;
   int height=Field->Def->NJ;
   int depth=Field->Def->NK;

   float* data = malloc(depth*height*width*sizeof(float));
   float temp = 0.0;
   float max = 0.0;

   for(int k=0; k<depth; k++){
      for(int j=0; j<height; j++){
         for(int i=0; i<width; i++){
            Def_GetMod(Field->Def,(k*height*width+j*width+i),temp);
            if(temp>max)max = temp;
         }
      }
   }

   for(int k=0; k<depth; k++){
      for(int j=0; j<height; j++){
         for(int i=0; i<width; i++){
            Def_GetMod(Field->Def,(k*height*width+j*width+i),temp);
            data[k*height*width+j*width+i]=temp/max;
         }
      }
   }

   glTexImage3D(GL_TEXTURE_3D,0,GL_RED,width,height,depth,0,GL_RED, GL_FLOAT,data);
   glUniform1iARB(GLShader_UniformGet(prog,"TextureData3D"),1);
   free(data);

   float camDir[3]= {-(float)VP->Cam->From[0]+(float)VP->Cam->To[0],
                     -(float)VP->Cam->From[1]+(float)VP->Cam->To[1],
                     -(float)VP->Cam->From[2]+(float)VP->Cam->To[2]};
   glUniform3fvARB(GLShader_UniformGet(prog,"CameraDir"),1,camDir);

   glUniform1fARB(GLShader_UniformGet(prog,"Elev"),(float)Proj->Scale);
   glUniform1fARB(GLShader_UniformGet(prog,"MaxData"),max);
   glUniform1fARB(GLShader_UniformGet(prog,"MinDataDisplay"),(float)Field->Spec->Min);
   glUniform1fARB(GLShader_UniformGet(prog,"MaxDataDisplay"),(float)Field->Spec->Max);

   glDisable(GL_LIGHTING);
   glEnable(GL_BLEND);
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   glPointSize(2.0);
   glEnable(GL_CULL_FACE);
   glCullFace(GL_FRONT);
   glFrontFace(GL_CCW);

   float step = 0.005;
   float offset = step/2.0;
   float v[3] = {0.0,0.0,0.0};
   float quad[4][3]={{0.0,0.0,0.0},{0.0,0.0,0.0},{0.0,0.0,0.0},{0.0,0.0,0.0}};

   int MinX = Field->Def->Limits[0][0];// = 300;
   int MaxX = Field->Def->Limits[0][1];// = 600;
   int MinY = Field->Def->Limits[1][0];// = 100;
   int MaxY = Field->Def->Limits[1][1];// = 800;
   int MinZ = Field->Def->Limits[2][0];// = 5;
   int MaxZ = Field->Def->Limits[2][1];// = 50;

   //all in [0,1]
   float limitDisplayMinX = ((float)MinX/width);
   float limitDisplayMaxX = ((float)MaxX/width);
   float limitDisplayMinY = ((float)MinY/height);
   float limitDisplayMaxY = ((float)MaxY/height);

   //in the limits of LI and LJ
   limitDisplayMinX = limitDisplayMinX*2*Proj->LI-Proj->LI;
   limitDisplayMaxX = limitDisplayMaxX*2*Proj->LI-Proj->LI;
   limitDisplayMinY = limitDisplayMinY*2*Proj->LJ-Proj->LJ;
   limitDisplayMaxY = limitDisplayMaxY*2*Proj->LJ-Proj->LJ;

   //in [1,2]
   float limitDisplayMinZ = ((float)MinZ/depth)*Proj->Scale/200+1.0;
   float limitDisplayMaxZ = ((float)MaxZ/depth)*Proj->Scale/200+1.0;

   //face dessus
   float fixPos = limitDisplayMaxZ;
   float upLimit = limitDisplayMaxY;
   float downLimit = limitDisplayMinY;
   float leftLimit = limitDisplayMinX;
   float rightLimit = limitDisplayMaxX;
   glBegin(GL_QUADS);
      for(int i=0; (downLimit+(float)i*step)<upLimit; i++){
         for(int j=0; (leftLimit+(float)j*step)<rightLimit; j++){
            v[0] = leftLimit+(float)j*step;
            v[1] = downLimit+(float)i*step;
            v[2] = fixPos;

            quad[0][0] = v[0] + offset;
            quad[0][1] = v[1] + offset;
            quad[0][2] = v[2];

            quad[1][0] = v[0] + offset;
            quad[1][1] = v[1] - offset;
            quad[1][2] = v[2];

            quad[2][0] = v[0] - offset;
            quad[2][1] = v[1] - offset;
            quad[2][2] = v[2];

            quad[3][0] = v[0] - offset;
            quad[3][1] = v[1] + offset;
            quad[3][2] = v[2];

            glVertex3fv(quad[0]);
            glVertex3fv(quad[1]);
            glVertex3fv(quad[2]);
            glVertex3fv(quad[3]);
         }
      }
   glEnd();

   //face Sud
   fixPos = limitDisplayMinY;
   upLimit = limitDisplayMaxZ;
   downLimit = limitDisplayMinZ;
   leftLimit = limitDisplayMinX;
   rightLimit = limitDisplayMaxX;
   glBegin(GL_QUADS);
      for(int i=0; (downLimit+(float)i*step)<upLimit; i++){
         for(int j=0; (leftLimit+(float)j*step)<rightLimit; j++){
            v[0] = leftLimit+(float)j*step;
            v[1] = fixPos;
            v[2] = downLimit+(float)i*step;

            quad[0][0] = v[0] + offset;
            quad[0][1] = v[1];
            quad[0][2] = v[2] + offset;

            quad[1][0] = v[0] + offset;
            quad[1][1] = v[1];
            quad[1][2] = v[2] - offset;

            quad[2][0] = v[0] - offset;
            quad[2][1] = v[1];
            quad[2][2] = v[2] - offset;

            quad[3][0] = v[0] - offset;
            quad[3][1] = v[1];
            quad[3][2] = v[2] + offset;

            glVertex3fv(quad[0]);
            glVertex3fv(quad[1]);
            glVertex3fv(quad[2]);
            glVertex3fv(quad[3]);
         }
      }
   glEnd();

   //face Nord
   fixPos = limitDisplayMaxY;
   glBegin(GL_QUADS);
      for(int i=0; (downLimit+(float)i*step)<upLimit; i++){
         for(int j=0; (leftLimit+(float)j*step)<rightLimit; j++){
            v[0] = leftLimit+(float)j*step;
            v[1] = fixPos;
            v[2] = downLimit+(float)i*step;

            quad[0][0] = v[0] + offset;
            quad[0][1] = v[1];
            quad[0][2] = v[2] + offset;

            quad[1][0] = v[0] - offset;
            quad[1][1] = v[1];
            quad[1][2] = v[2] + offset;

            quad[2][0] = v[0] - offset;
            quad[2][1] = v[1];
            quad[2][2] = v[2] - offset;

            quad[3][0] = v[0] + offset;
            quad[3][1] = v[1];
            quad[3][2] = v[2] - offset;

            glVertex3fv(quad[0]);
            glVertex3fv(quad[1]);
            glVertex3fv(quad[2]);
            glVertex3fv(quad[3]);
         }
      }
   glEnd();

   //face est
   fixPos = limitDisplayMaxX;
   leftLimit = limitDisplayMinY;
   rightLimit = limitDisplayMaxY;
   glBegin(GL_QUADS);
      for(int i=0; (downLimit+(float)i*step)<upLimit; i++){
         for(int j=0; (leftLimit+(float)j*step)<rightLimit; j++){
            v[0] = fixPos;
            v[1] = leftLimit+(float)j*step;
            v[2] = downLimit+(float)i*step;

            quad[0][0] = v[0];
            quad[0][1] = v[1] + offset;
            quad[0][2] = v[2] + offset;

            quad[1][0] = v[0];
            quad[1][1] = v[1] + offset;
            quad[1][2] = v[2] - offset;

            quad[2][0] = v[0];
            quad[2][1] = v[1] - offset;
            quad[2][2] = v[2] - offset;

            quad[3][0] = v[0];
            quad[3][1] = v[1] - offset;
            quad[3][2] = v[2] + offset;

            glVertex3fv(quad[0]);
            glVertex3fv(quad[1]);
            glVertex3fv(quad[2]);
            glVertex3fv(quad[3]);
         }
      }
   glEnd();

   //face ouest
   fixPos = limitDisplayMinX;
   glBegin(GL_QUADS);
      for(int i=0; (downLimit+(float)i*step)<upLimit; i++){
         for(int j=0; (leftLimit+(float)j*step)<rightLimit; j++){
            v[0] = fixPos;
            v[1] = leftLimit+(float)j*step;
            v[2] = downLimit+(float)i*step;

            quad[0][0] = v[0];
            quad[0][1] = v[1] + offset;
            quad[0][2] = v[2] + offset;

            quad[1][0] = v[0];
            quad[1][1] = v[1] - offset;
            quad[1][2] = v[2] + offset;

            quad[2][0] = v[0];
            quad[2][1] = v[1] - offset;
            quad[2][2] = v[2] - offset;

            quad[3][0] = v[0];
            quad[3][1] = v[1] + offset;
            quad[3][2] = v[2] - offset;

            glVertex3fv(quad[0]);
            glVertex3fv(quad[1]);
            glVertex3fv(quad[2]);
            glVertex3fv(quad[3]);
         }
      }
   glEnd();

   glDeleteTextures(2,tx);
   glUseProgramObjectARB(0);
   glActiveTexture(GL_TEXTURE0);
   glDisable(GL_BLEND);
   glEnable(GL_LIGHTING);
   return(1);
}
