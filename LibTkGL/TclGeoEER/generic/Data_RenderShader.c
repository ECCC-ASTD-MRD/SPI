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

#include "Data_FF.h"

int Data_RenderShaderParticle(TData *Field,ViewportItem *VP,Projection *Proj);
int Data_RenderShaderTexture(TData *Field,ViewportItem *VP,Projection *Proj);
int Data_RenderShaderStream(TData *Field,ViewportItem *VP,Projection *Proj);
int Data_RenderShaderMesh(TData *Field,ViewportItem *VP,Projection *Proj);

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
   float   min,rng;
   Vect3d *pos;

   GLuint      tx[3],att0;
   GLhandleARB prog;
   Tk_PhotoImageBlock img;

   if (!Field->Ref || !Field->Ref->Pos) {
      return(0);
   }

   /*Do we need transparency*/
   if (Field->Spec->Map->Alpha || Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }
   glColor4f(1.0,1.0,1.0,Field->Spec->Alpha/100.0);

   min=Field->Spec->Min;
   rng=Field->Spec->Max-Field->Spec->Min;
   pos=Field->Ref->Pos[Field->Def->Level];
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
      glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GL_FLOAT_R32_NV,Field->Spec->InterNb,1,0,GL_LUMINANCE,GL_FLOAT,Field->Spec->Inter);
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

   int     n;
   double  val=0.0;
   float   min,rng;
   Vect3d *pos;
   Vect3d b,p,p0,p1,p2;

   GLuint      tx[2],att0;
   GLhandleARB prog;

   if (!Field->Ref || !Field->Ref->Pos) {
      return(0);
   }

   /*Do we need transparency*/
   if (Field->Spec->Map->Alpha || Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }
   glColor4f(1.0,1.0,1.0,Field->Spec->Alpha/100.0);

   min=Field->Spec->Min;
   rng=Field->Spec->Max-Field->Spec->Min;
   pos=Field->Ref->Pos[Field->Def->Level];
   prog=GLRender->Prog[PROG_FIELD];

   glUseProgramObjectARB(prog);
   glGenTextures(2,tx);

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
      glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GL_FLOAT_R32_NV,Field->Spec->InterNb,1,0,GL_LUMINANCE,GL_FLOAT,Field->Spec->Inter);
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

   glBegin(GL_TRIANGLES);
   if (Field->Spec->InterpDegree[0]=='L') {
      for(n=0;n<Field->Ref->NIdx;n++) {
         Def_Get(Field->Def,0,Field->Ref->Idx[n],val);
         glVertexAttrib1fARB(att0,val);
         glVertex3dv(pos[Field->Ref->Idx[n]]);
      }
   } else {
      for(n=0;n<Field->Ref->NIdx-3;n+=3) {
         Vect_Init(b,1.0/3.0,1.0/3.0,1.0/3.0);
         Bary_Interp(b,p,pos[Field->Ref->Idx[n]],pos[Field->Ref->Idx[n+1]],pos[Field->Ref->Idx[n+2]]);
         Vect_Init(b,0.0,0.5,0.5);
         Bary_Interp(b,p0,pos[Field->Ref->Idx[n]],pos[Field->Ref->Idx[n+1]],pos[Field->Ref->Idx[n+2]]);
         Vect_Init(b,0.5,0.0,0.5);
         Bary_Interp(b,p1,pos[Field->Ref->Idx[n]],pos[Field->Ref->Idx[n+1]],pos[Field->Ref->Idx[n+2]]);
         Vect_Init(b,0.5,0.5,0.0);
         Bary_Interp(b,p2,pos[Field->Ref->Idx[n]],pos[Field->Ref->Idx[n+1]],pos[Field->Ref->Idx[n+2]]);

         Def_Get(Field->Def,0,Field->Ref->Idx[n],val);
         glVertexAttrib1fARB(att0,val);
         glVertex3dv(pos[Field->Ref->Idx[n]]);
         glVertex3dv(p);
         glVertex3dv(p1);
         glVertex3dv(pos[Field->Ref->Idx[n]]);
         glVertex3dv(p);
         glVertex3dv(p2);

         Def_Get(Field->Def,0,Field->Ref->Idx[n+1],val);
         glVertexAttrib1fARB(att0,val);
         glVertex3dv(pos[Field->Ref->Idx[n+1]]);
         glVertex3dv(p);
         glVertex3dv(p0);
         glVertex3dv(pos[Field->Ref->Idx[n+1]]);
         glVertex3dv(p);
         glVertex3dv(p2);

         Def_Get(Field->Def,0,Field->Ref->Idx[n+2],val);
         glVertexAttrib1fARB(att0,val);
         glVertex3dv(pos[Field->Ref->Idx[n+2]]);
         glVertex3dv(p);
         glVertex3dv(p0);
         glVertex3dv(pos[Field->Ref->Idx[n+2]]);
         glVertex3dv(p);
         glVertex3dv(p1);
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
 * Nom      : <Data_RenderShaderStream>
 * Creation : Avril 2008 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage de streamlines de champs vectoriels sur le GPU.
 *
 * Parametres :
 *  <Field>   : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_RenderShaderStream(TData *Field,ViewportItem *VP,Projection *Proj){

   double i,j,dt;
   int    b,f,len,pi,pj,dz;
   float  step;
   Vect3d pix,*vbuf;
   Coord  coo;

   if (GLRender->Resolution>2) {
      return(0);
   }

   if (!Field->Ref || !Field->Ref->Pos || !Field->Spec->Width || !Field->Spec->Outline) {
      return(0);
   }

   /*Do we need transparency*/
   if (Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }
   glColor4us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue,Field->Spec->Alpha*655.35);

   /*Setup 1D Texture*/
   glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,256,0,GL_RGBA,GL_UNSIGNED_BYTE,FFStreamMap);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);
   glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL);
   glEnable(GL_TEXTURE_1D);

   if (!Field->Map) {
      Field->Map=(float*)malloc(sizeof(float)*FFSTREAMLEN);
      for(len=0;len<FFSTREAMLEN;len++) {
         Field->Map[len]=(float)len/FFSTREAMLEN*32;
      }

   }

   glMatrixMode(GL_TEXTURE);
   if (GLRender->Delay<GL_STOP) {
      Field->Spec->TexStep+=0.01;
      Field->Spec->TexStep=Field->Spec->TexStep>1.0?0.0:Field->Spec->TexStep;
  }

   glEnable(GL_STENCIL_TEST);
   glStencilMask(0x2);
   glStencilFunc(GL_NOTEQUAL,0x2,0x2);
   glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);

   glReadBuffer(GL_STENCIL);
   glEnableClientState(GL_VERTEX_ARRAY);
   glColor3us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue);
   glEnable(GL_BLEND);

   pi=pj=-1;
   dz=Field->Spec->Sample*10;
   dt=0.0;
   len=512;
   step=0.0;

   vbuf=VBuffer_Alloc(len*2+1);

   /*Recuperer les latlon des pixels sujets*/
   for (pix[0]=0;pix[0]<VP->Width;pix[0]+=dz) {
      for (pix[1]=0;pix[1]<VP->Height;pix[1]+=dz) {

         Proj->Type->UnProject(VP,Proj,&coo,pix);
         if (coo.Lat==-999.0) {
            continue;
         }

         if (Field->Ref->UnProject(Field->Ref,&i,&j,coo.Lat,coo.Lon,0,1) && i<Field->Def->NI-2 && j<Field->Def->NJ-2) {
            /*Get the cell resolution, if not the same, to use as step size for a constant spacing*/
            if (pi!=(int)i ||  pj!=(int)j) {
               pi=i;
               pj=j;
               step=5.0/FFCellResolution(VP,Proj,Field->Ref->Pos[Field->Def->Level][FIDX2D(Field->Def,pi,pj)],Field->Ref->Pos[Field->Def->Level][FIDX2D(Field->Def,pi+1,pj+1)]);
            }


            /*Get the streamline */
            b=FFStreamLine(Field->Ref,Field->Def,VP,vbuf,NULL,i,j,Field->Def->Level,len,-step,Field->Spec->Min,0,REF_PROJ,0);
            f=FFStreamLine(Field->Ref,Field->Def,VP,&vbuf[len],NULL,i,j,Field->Def->Level,len,step,Field->Spec->Min,0,REF_PROJ,0);

            /* If we have at least some part of it */
            glPushMatrix();
            glTranslatef(-Field->Spec->TexStep-(dt+=0.15),0.0,0.0);
            if (b+f>2) {
               glLineWidth(Field->Spec->Width);
               glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
               Proj->Type->Render(Proj,0,&vbuf[len-b],NULL,NULL,Field->Map,GL_LINE_STRIP,b+f,0,NULL,NULL);

               glLineWidth(8*Field->Spec->Width);
               glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
               Proj->Type->Render(Proj,0,&vbuf[len-b],NULL,NULL,NULL,GL_LINE_STRIP,b+f,0,NULL,NULL);
            }
            glPopMatrix();
         }
      }
   }

   glClear(GL_STENCIL_BUFFER_BIT);
   glStencilMask(0xf);
   glStencilFunc(GL_EQUAL,0x0,0xf);
   glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
   glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
   glDisable(GL_TEXTURE_1D);
   glDisableClientState(GL_VERTEX_ARRAY);

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

   int     i,j,idxk,idx0,idx1,ox=0,dp,dn,mask=0;
   float   min,rng,fi,fj,ti=0.0;
   Vect3d *pos;
   float  *buf=NULL;
   char   *ptr,out=0;

   GLuint      tx[4];
   GLhandleARB prog;

   if (GLRender->Resolution>2) {
      return(0);
   }
   
   if (!Field->Ref || !Field->Ref->Pos || Field->Ref->Grid[0]=='Y') {
      return(0);
   }

   if (Field->Ref->Grid[0]=='M') {
      Data_RenderShaderMesh(Field,VP,Proj);
      return(1);
   }

   if (Proj->Type->Def==PROJCYLIN && Field->Ref->Grid[0]!='V') {
      glEnable(GL_CULL_FACE);
   } else {
      glDisable(GL_CULL_FACE);
   }
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

   /*Do we need transparency*/
   if (Field->Spec->Map->Alpha || Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }

   min=Field->Spec->Min;
   rng=Field->Spec->Max-Field->Spec->Min;
   pos=&Field->Ref->Pos[Field->Def->Level][Field->Def->Idx];
   idxk=FSIZE2D(Field->Def)*Field->Def->Level;
   Def_PointerMode(Field->Def,idxk,ptr);

//   prog=GLShader_Load("/home/afsr/005/eer_Tools/LibTkGL/TclData","FieldTex");
   prog=GLRender->Prog[PROG_FIELDTEX];
   glUseProgramObjectARB(prog);
   glGenTextures(4,tx);

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
      glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GL_FLOAT_R32_NV,Field->Spec->InterNb,1,0,GL_LUMINANCE,GL_FLOAT,Field->Spec->Inter);
   }

   /*Setup 2D Data Texture*/
   glActiveTexture(GL_TEXTURE2);
   glBindTexture(GL_TEXTURE_RECTANGLE_ARB,tx[2]);
   glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
   glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_WRAP_S,GL_REPEAT);

   /*Why the hell GL_FLOAT_R32_NV accepts only Float32, I don't know, here's the quick fix*/
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
   
      /*Setup 2D Mask Texture*/
      if (Field->Def->Mask) {
         mask=1;
         glActiveTexture(GL_TEXTURE3);
         glBindTexture(GL_TEXTURE_RECTANGLE_ARB,tx[3]);
         glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
         glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
         glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_WRAP_S,GL_REPEAT);
         glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GL_ALPHA,Field->Def->NI,Field->Def->NJ,0,GL_ALPHA,GL_BYTE,Field->Def->Mask);         
      }
   } else {
      fprintf(stdout,"(WARNING) Texture is too big to fit on GPU, switching to software renderer.\n");
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

   /*Grille avec loop sur la longitude*/
   if (Field->Ref->Type&GRID_WRAP && Proj->Type->Def!=PROJPLANE) {
      ox=1;
   }

   /*Resolution selon la dimension des cellules (mid-grid) et la vue*/   
   dp=Proj->PixDist/Field->Ref->Distance(Field->Ref,Field->Def->NI>>1,Field->Def->NJ>>1,(Field->Def->NI>>1)+1,Field->Def->NJ>>1)*20;
   
   if (Proj->Type->Def==PROJCYLIN) {
      dp=CLAMP(dp,1,2);
   } else {      
      dp=CLAMP(dp,1,20);
   }
   dn=dp*Field->Def->NI;
   
   /*Process gridpoints*/
   for(j=0;j<Field->Def->NJ-dp;j+=dp) {
      idx0=j*Field->Def->NI;

      glBegin(GL_QUAD_STRIP);
      for(i=0;i<(Field->Def->NI+dp);i+=dp) {

         /*If the next index is over the size*/
         if (i>=Field->Def->NI) {
            if (ox) {
               /*If the grid wraps around, use the first point*/
               fi=0;ti=Field->Def->NI;
               idx0=j*Field->Def->NI;
            } else {
               /*If not, use the last point*/
               fi=Field->Def->NI-1;
               idx0=(j+1)*Field->Def->NI-1;
            }
         } else {
            fi=i;
            ti=0;
         }
         
         idx1=idx0+dn;

         fi+=0.5f;
         fj=(float)j+0.5f;
            
         glTexCoord2f(fi+ti,fj+dp);
//         glNormal3dv(pos[idx1]);
         glVertex3dv(pos[idx1]);
         glTexCoord2f(fi+ti,fj);
//         glNormal3dv(pos[idx0]);
         glVertex3dv(pos[idx0]);

         idx0+=dp;
      }
      glEnd();
      
      // If it's the last, exit loop
      if (out) break;
      
      // Make sure last step goes to NJ-1
      if (j>(Field->Def->NJ-dp-dp-1)) {
         j=Field->Def->NJ-dp-dp-1;
         out=1;
      }
   }

   glDeleteTextures(4,tx);
   glUseProgramObjectARB(0);
//   GLShader_UnInstall(prog);

   glActiveTexture(GL_TEXTURE0);
   glEnable(GL_CULL_FACE);
   glDisable(GL_BLEND);

   return(1);
}
