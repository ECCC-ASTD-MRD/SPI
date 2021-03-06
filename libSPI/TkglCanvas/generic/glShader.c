/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Conversion du canvas Tk en Canvas GL.
 * Fichier   : glShader.c
 * Creation  : Aout 2005 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Fonctions de support au shader GL.
 *
 * Remarques :
 *
 * License   :
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

#include "glStuff.h"

void GLShader_InfoLog(GLhandleARB Object) {

   GLsizei    len=0,*chr=NULL;
   GLcharARB *log;

   glGetObjectParameterivARB(Object,GL_OBJECT_INFO_LOG_LENGTH_ARB,&len);

   if (len>2) {
      log=(GLcharARB*)alloca(len);
      if (!log) {
         fprintf(stdout,"(WARNING) glInfoLog: Could not allocate log buffer\n");
      } else {
         glGetInfoLogARB(Object,len,chr,log);
         printf("(DEBUG) glInfoLog:\n%s\n",log);
      }
   }
   glErrorCheck("GLShader_InfoLog",0);
}

GLint GLShader_UniformGet(const GLhandleARB Prog,const GLcharARB *Name) {

   GLint loc;

   loc=glGetUniformLocationARB(Prog,Name);

   if (loc==-1)
      fprintf(stderr,"(ERROR) GL_UniformGet: Unable to find uniform named \"%s\"\n",Name);

   glErrorCheck("GLShader_UniformGet",0);
   return(loc);
}

GLint GLShader_AttribGet(const GLhandleARB Prog,const GLcharARB *Name) {

   GLint loc;

   loc=glGetAttribLocationARB(Prog,Name);

   if (loc==-1)
      fprintf(stderr,"(ERROR) GL_AttribGet: Unable to find attribute named \"%s\"\n",Name);

   glErrorCheck("GLShader_AttribGet",0);
   return(loc);
}

GLhandleARB GLShader_Load(const GLcharARB *Path,const GLcharARB *Name) {

   GLcharARB *vert=NULL,*frag=NULL,*geom=NULL,prog=0,file[256];
   int geomShaderExist = 0;
   FILE *f;
   struct stat st;

   strcpy(file,Path);
   strcat(file,"/SHV_");
   strcat(file,Name);
   strcat(file,".glsl");
   stat(file,&st);
   f=fopen(file,"r");
   if (!f) {
      fprintf(stderr,"(ERROR) GLShader_Load: unable to load vertex shader %s\n",file);
   } else {
      vert=(GLcharARB*)malloc(st.st_size+1);

      if (!fread(vert,1,st.st_size,f)) {
         fprintf(stderr,"(ERROR) GLShader_Load: Problem while reading vertex shader %s\n",file);
      }
      vert[st.st_size]='\0';
      fclose(f);
   }

   strcpy(file,Path);
   strcat(file,"/SHF_");
   strcat(file,Name);
   strcat(file,".glsl");
   stat(file,&st);
   f=fopen(file,"r");
   if (!f) {
      fprintf(stderr,"(ERROR) GLShader_Load: unable to load fragment shader %s\n",file);
   } else {
      frag=(GLcharARB*)malloc(st.st_size+1);
      if (!fread(frag,1,st.st_size,f)) {
         fprintf(stderr,"(ERROR) GLShader_Load: Problem while reading fragment shader %s\n",file);
      }
      frag[st.st_size]='\0';
      fclose(f);
   }

   //looks for a geometry shader
   strcpy(file,Path);
   strcat(file,"/SHG_");
   strcat(file,Name);
   strcat(file,".glsl");
   stat(file,&st);
   f=fopen(file,"r");
   if (!f) {
      fprintf(stderr,"(INFO) GLShader_Load: geometry shader may not exist %s\n",file);
   } else {
      geomShaderExist = 1;
      geom=(GLcharARB*)malloc(st.st_size+1);
      if (!fread(geom,1,st.st_size,f)) {
         fprintf(stderr,"(ERROR) GLShader_Load: Problem while reading geometry shader %s\n",file);
      }
      geom[st.st_size]='\0';
      fclose(f);
   }

   if(geomShaderExist && vert && frag && geom){
      prog=GLShader_InstallGeom(vert,frag,geom);
      free(vert);
      free(frag);
      free(geom);
   }else if (vert && frag) {
      prog=GLShader_Install(vert,frag);
      free(vert);
      free(frag);
   }
   if (!prog) fprintf(stderr,"(ERROR) GLShader_Load: Unable to install program %s\n",file);

   return(prog);
}


GLhandleARB GLShader_Install(const GLcharARB *VertSrc,const GLcharARB *FragSrc) {

   GLhandleARB vert,frag,prog;
   GLint       status;

   /* Source the code */
   vert=glCreateShaderObjectARB(GL_VERTEX_SHADER_ARB);
   frag=glCreateShaderObjectARB(GL_FRAGMENT_SHADER_ARB);

   glShaderSourceARB(vert,1,&VertSrc,NULL);
   glShaderSourceARB(frag,1,&FragSrc,NULL);

   /* Compile the vertex shader */
   glCompileShaderARB(vert);
   glErrorCheck("GLShader_Install",0);
   glGetObjectParameterivARB(vert,GL_OBJECT_COMPILE_STATUS_ARB,&status);
   GLShader_InfoLog(vert);

   if (!status)
      return(0);

   /* Compile the fragment shader */
   glCompileShaderARB(frag);
   glErrorCheck("GLShader_Install",0);
   glGetObjectParameterivARB(frag,GL_OBJECT_COMPILE_STATUS_ARB,&status);
   GLShader_InfoLog(frag);

   if (!status)
      return(0);

   /* Link both into a program */
   prog=glCreateProgramObjectARB();
   glAttachObjectARB(prog,vert);
   glAttachObjectARB(prog,frag);

   glBindAttribLocationARB(prog,1,"Vd");

   glLinkProgramARB(prog);

   glErrorCheck("GLShader_Install",0);
   glGetObjectParameterivARB(prog,GL_OBJECT_LINK_STATUS_ARB,&status);
   GLShader_InfoLog(prog);

   if (!status)
      return(0);

   /* Start usign the program*/
   glDeleteObjectARB(vert);
   glDeleteObjectARB(frag);

   glErrorCheck("GLShader_Install",0);
   return(prog);
}

void GLShader_UnInstall(GLhandleARB Prog) {

   glDeleteObjectARB(Prog);
}

//install vertex, geometry and fragment shaders
GLhandleARB GLShader_InstallGeom(const GLcharARB *VertSrc,const GLcharARB *FragSrc,const GLcharARB *GeomSrc) {

   GLhandleARB vert,frag,prog,geom;
   GLint       status;

   /* Source the code */
   vert=glCreateShaderObjectARB(GL_VERTEX_SHADER_ARB);
   frag=glCreateShaderObjectARB(GL_FRAGMENT_SHADER_ARB);
   geom=glCreateShaderObjectARB(GL_GEOMETRY_SHADER_ARB);

   glShaderSourceARB(vert,1,&VertSrc,NULL);
   glShaderSourceARB(frag,1,&FragSrc,NULL);
   glShaderSourceARB(geom,1,&GeomSrc,NULL);

   /* Compile the vertex shader */
   glCompileShaderARB(vert);
   glErrorCheck("GLShader_InstallGeom",0);
   glGetObjectParameterivARB(vert,GL_OBJECT_COMPILE_STATUS_ARB,&status);
   GLShader_InfoLog(vert);

   if (!status)
      return(0);

   /* Compile the fragment shader */
   glCompileShaderARB(frag);
   glErrorCheck("GLShader_InstallGeom",0);
   glGetObjectParameterivARB(frag,GL_OBJECT_COMPILE_STATUS_ARB,&status);
   GLShader_InfoLog(frag);

   if (!status)
      return(0);

   /* Compile the fragment shader */
   glCompileShaderARB(geom);
   glErrorCheck("GLShader_InstallGeom",0);
   glGetObjectParameterivARB(geom,GL_OBJECT_COMPILE_STATUS_ARB,&status);
   GLShader_InfoLog(geom);

   if (!status)
      return(0);

   /* Link both into a program */
   prog=glCreateProgramObjectARB();
   glAttachObjectARB(prog,vert);
   glAttachObjectARB(prog,frag);
   glAttachObjectARB(prog,geom);

   glBindAttribLocationARB(prog,1,"Vd");

   glLinkProgramARB(prog);

   glErrorCheck("GLShader_InstallGeom",0);
   glGetObjectParameterivARB(prog,GL_OBJECT_LINK_STATUS_ARB,&status);
   GLShader_InfoLog(prog);

   if (!status)
      return(0);

   /* Start usign the program*/
   glDeleteObjectARB(vert);
   glDeleteObjectARB(frag);
   glDeleteObjectARB(geom);

   glErrorCheck("GLShader_InstallGeom",0);
   return(prog);
}
