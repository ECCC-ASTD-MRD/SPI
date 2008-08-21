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

int   Data_RenderShaderParticle(TData *Field,ViewportItem *VP,Projection *Proj);
int   Data_RenderShaderTexture(TData *Field,ViewportItem *VP,Projection *Proj);

static int GL_Type[]={ GL_BITMAP,GL_BITMAP,GL_UNSIGNED_BYTE,GL_BYTE,GL_UNSIGNED_SHORT,GL_SHORT,
                       GL_UNSIGNED_INT,GL_INT,GL_UNSIGNED_INT,GL_INT,GL_FLOAT,GL_FLOAT };

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
   double  val;
   float   min,rng;
   Vect3d *pos;

   GLuint      tx[3],att0;
   GLfloat     atten[]={ 0.0, 0.1, 0.0 };
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
   pos=Field->Ref->Pos;
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
   glUniform1fARB(GLShader_UniformGet(prog,"Cylindric"),(Proj->Type->Def==PROJCYLIN?Proj->Params->L:-999.0));
   glUniform1fARB(GLShader_UniformGet(prog,"Min"),min);
   glUniform1fARB(GLShader_UniformGet(prog,"Range"),rng);
   glUniform1iARB(GLShader_UniformGet(prog,"Nb"),Field->Spec->InterNb);
   att0=GLShader_AttribGet(prog,"Vd");

   /*Projeter les particules*/
   glBegin(GL_POINTS);
   glEnable(GL_TEXTURE_2D);
   for(n=0;n<FSIZE3D(Field->Def);n++) {
      Def_Get(Field->Def,0,n,val);
      glVertexAttrib1fARB(att0,val);
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
   double  val;
   float   min,rng;
   Vect3d *pos;

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
   pos=Field->Ref->Pos;
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
   glUniform1fARB(GLShader_UniformGet(prog,"Cylindric"),(Proj->Type->Def==PROJCYLIN?Proj->Params->L:-999.0));
   glUniform1fARB(GLShader_UniformGet(prog,"Min"),min);
   glUniform1fARB(GLShader_UniformGet(prog,"Range"),rng);
   glUniform1iARB(GLShader_UniformGet(prog,"Nb"),Field->Spec->InterNb);
   glUniform1iARB(GLShader_UniformGet(prog,"Bi"),(Field->Spec->InterpDegree[0]=='N'?0:1));
   att0=GLShader_AttribGet(prog,"Vd");

   glBegin(GL_TRIANGLES);
   for(n=0;n<Field->Ref->NIdx;n++) {
      Def_Get(Field->Def,0,Field->Ref->Idx[n],val);
      glVertexAttrib1fARB(att0,val);
      glVertex3dv(pos[Field->Ref->Idx[n]]);
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

   double i,j,width,dt;
   int    b,f,len,pi,pj,dz;
   float  step;
   Vect3d pix;
   Coord  coo;

   extern Vect3d GDB_VBuf[];

   if (GLRender->Resolution>2) {
      return(0);
   }

   if (!Field->Ref || !Field->Ref->Pos || !Field->Spec->Outline) {
      return(0);
   }

   /*Setup 1D Texture*/
   glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,256,0,GL_RGBA,GL_UNSIGNED_BYTE,StreamMap);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);
   glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL);
   glEnable(GL_TEXTURE_1D);

   if (!Field->Map) {
      Field->Map=(float*)malloc(sizeof(float)*STREAMLEN);
      for(len=0;len<STREAMLEN;len++) {
         Field->Map[len]=(float)len/STREAMLEN*32;
      }

   }

   glMatrixMode(GL_TEXTURE);
   if (GLRender->Delay<2000) {
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
   len=512;
   dz=Field->Spec->Sample*10;
   dt=0.0;

   width=Field->Spec->RenderTexture?Field->Spec->Size*0.2:Field->Spec->Size*0.1;

   /*Recuperer les latlon des pixels sujets*/
   for (pix[0]=0;pix[0]<VP->Width;pix[0]+=dz) {
      for (pix[1]=0;pix[1]<VP->Height;pix[1]+=dz) {

         Proj->Type->UnProject(VP,Proj->Params,&coo,pix);
         if (coo.lat==-999.0) {
            continue;
         }

         if (Field->Ref->UnProject(Field->Ref,&i,&j,coo.lat,coo.lon,0,1) && i<Field->Def->NI-2 && j<Field->Def->NJ-2) {
            /*Get the cell resolution, if not the same, to use as step size for a constant spacing*/
            if (pi!=(int)i ||  pj!=(int)j) {
               pi=i;
               pj=j;
               step=5.0/FFCellResolution(VP,Proj,Field->Ref->Pos[FIDX2D(Field->Def,pi,pj)],Field->Ref->Pos[FIDX2D(Field->Def,pi+1,pj+1)]);
            }


            /*Get the streamline */
            b=FFStreamLine(Field->Ref,Field->Def,VP,GDB_VBuf,NULL,i,j,Field->Def->Level,len,-step,Field->Spec->Min,0,REF_PROJ,0);
            f=FFStreamLine(Field->Ref,Field->Def,VP,&GDB_VBuf[len],NULL,i,j,Field->Def->Level,len,step,Field->Spec->Min,0,REF_PROJ,0);

            /* If we have at least some part of it */
            glPushMatrix();
            glTranslatef(-Field->Spec->TexStep-(dt+=0.15),0.0,0.0);
            if (b+f>2) {
               glLineWidth(width);
               glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
               Proj->Type->Render(Proj,0,&GDB_VBuf[len-b],NULL,NULL,Field->Map,GL_LINE_STRIP,b+f,NULL,NULL);

               glLineWidth(8*width);
               glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
               Proj->Type->Render(Proj,0,&GDB_VBuf[len-b],NULL,NULL,NULL,GL_LINE_STRIP,b+f,NULL,NULL);
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

   int     dr,ri,rj,i,j,idxk,idx0,idx1,ox;
   float   min,rng;
   Vect3d *pos;
   float  *buf;
   char   *ptr;

   GLuint      tx[3];
   GLhandleARB prog;

   if (GLRender->Resolution>2) {
      return(0);
   }

   if (Field->Ref->Grid[0]=='M') {
      Data_RenderShaderMesh(Field,VP,Proj);
      return(1);
   }

   if (!Field->Ref || !Field->Ref->Pos) {
      return(0);
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
   glColor4f(1.0,1.0,1.0,Field->Spec->Alpha/100.0);

   min=Field->Spec->Min;
   rng=Field->Spec->Max-Field->Spec->Min;
   pos=Field->Ref->Pos;
   idxk=FSIZE2D(Field->Def)*Field->Def->Level;
   Def_PointerMode(Field->Def,idxk,ptr);

//   prog=GLShader_Load("/home/afsr/005/eer_Tools/LibTkGL/TclData","FieldTex");
   prog=GLRender->Prog[PROG_FIELDTEX];
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
   glBindTexture(GL_TEXTURE_RECTANGLE_ARB,tx[1]);
   glActiveTexture(GL_TEXTURE1);
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

   /*Why the hell GL_FLOAT_R32_NV accepts only Float32, I don't know, here's the quick fix*/
   if (Field->Def->Type!=TD_Float32) {
      if (buf=(float*)malloc(Field->Def->NI*Field->Def->NJ*sizeof(float))) {
         for(i=0;i<Field->Def->NI*Field->Def->NJ;i++) {
             Def_GetMod(Field->Def,idxk+i,buf[i]);
         }
         glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GL_FLOAT_R32_NV,Field->Def->NI,Field->Def->NJ,0,GL_LUMINANCE,GL_FLOAT,buf);
         free(buf);
      }
   } else {
      glTexImage2D(GL_TEXTURE_RECTANGLE_ARB,0,GL_FLOAT_R32_NV,Field->Def->NI,Field->Def->NJ,0,GL_LUMINANCE,GL_Type[Field->Def->Type],ptr);
   }

   glUniform1iARB(GLShader_UniformGet(prog,"Colormap"),0);
   glUniform1iARB(GLShader_UniformGet(prog,"Interval"),1);
   glUniform1iARB(GLShader_UniformGet(prog,"Data"),2);
   glUniform1fARB(GLShader_UniformGet(prog,"Cylindric"),(Proj->Type->Def==PROJCYLIN?Proj->Params->L:-999.0));
   glUniform1fARB(GLShader_UniformGet(prog,"Min"),min);
   glUniform1fARB(GLShader_UniformGet(prog,"Range"),rng);
   glUniform1iARB(GLShader_UniformGet(prog,"Nb"),Field->Spec->InterNb);
   glUniform1iARB(GLShader_UniformGet(prog,"Bi"),(Field->Spec->InterpDegree[0]=='N'?0:1));

   /*Grille avec loop sur la longitude*/
   if (Field->Ref->Type&GRID_WRAP && Proj->Type->Def!=PROJPLANE) {
      ox=1;
   }

   /*Process gridpoints*/
   dr=2;
   rj=dr;
   for(j=0;j<Field->Def->NJ-rj && rj;j+=rj) {

      ri=dr;
      idx0=idxk+j*Field->Def->NI-ri;
      glBegin(GL_QUAD_STRIP);
      for(i=0;i<Field->Def->NI && ri;i+=ri) {
         idx0+=ri;
         idx1=idx0+rj*Field->Def->NI;

         glTexCoord2f((float)i+0.5,(float)j+rj+0.5);
         glVertex3dv(pos[idx1]);
         glTexCoord2f((float)i+0.5,(float)j+0.5);
         glVertex3dv(pos[idx0]);

         if (i+ri>Field->Def->NI-1 && ri==dr) {
            ri=Field->Def->NI-1-i;
         }
      }
      glEnd();

      if (j+rj>Field->Def->NJ-1-rj && rj==dr) {
         rj=Field->Def->NJ-1-j-rj;
      }
   }

   glDeleteTextures(3,tx);
   glUseProgramObjectARB(0);
//   GLShader_UnInstall(prog);

   glActiveTexture(GL_TEXTURE0);
   glEnable(GL_CULL_FACE);
   glDisable(GL_BLEND);
   return 1;
}

