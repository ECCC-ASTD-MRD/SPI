/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Conversion du canvas Tk en Canvas GL.
 * Fichier   : glStuff.c
 * Creation  : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Fonctions de support au canvas GL.
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

#include <sys/resource.h>
#include <unistd.h>

GLParams *GLRender=NULL;  /* Structure globale des parametres OpenGL */

static float glArrayCircle[2*360*GL_ARCSTEP+2];
static float glArrayArrow[14] = { 0.0f,0.0f, 0.25f,-0.5f, 0.1f,-0.5f, 0.1f,-1.0f, -0.1f,-1.0f, -0.1f,-0.5f, -0.25f,-0.5f };
static char *glExtString[]={ "GL_ARB_multisample","_GL_ARB_texture_compression","GL_ARB_vertex_buffer_object",NULL };

static Tcl_HashTable glFontIdTable;
static Tcl_HashTable glBitmapTable;

static int  glRender_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int  glInfo(Tcl_Interp *Interp,char *Argv);

static GLdouble glTessTmp[GL_MAXTESS][6];
static GLuint   glTessNo=0;

void glTessError(GLenum Err) {
   fprintf(stderr,"(ERROR) TessError: %s\n",gluErrorString(Err));
}

GLdouble *glTessTmpGet() {
   return glTessTmp[(glTessNo=glTessNo<(GL_MAXTESS-1)?glTessNo+1:0)];
}

static void glTessCombineNr(GLdouble coords[3],GLdouble *d[4],GLfloat w[4],GLdouble **Out) {

   *Out=glTessTmp[glTessNo];

   glTessTmp[glTessNo][0]=coords[0];
   glTessTmp[glTessNo][1]=coords[1];
   glTessTmp[glTessNo][2]=coords[2];

   glTessTmp[glTessNo][3]=d[0][3];
   glTessTmp[glTessNo][4]=d[0][4];
   glTessTmp[glTessNo][5]=d[0][5];
// glTessTmp[glTessNo][3]=(d[0]?w[0]*d[0][3]:0.0)+(d[1]?w[1]*d[1][3]:0.0)+(d[2]?w[2]*d[2][3]:0.0)+(d[3]?w[3]*d[3][3]:0.0);
// glTessTmp[glTessNo][4]=(d[0]?w[0]*d[0][4]:0.0)+(d[1]?w[1]*d[1][4]:0.0)+(d[2]?w[2]*d[2][4]:0.0)+(d[3]?w[3]*d[3][4]:0.0);
// glTessTmp[glTessNo][5]=(d[0]?w[0]*d[0][5]:0.0)+(d[1]?w[1]*d[1][5]:0.0)+(d[2]?w[2]*d[2][5]:0.0)+(d[3]?w[3]*d[3][5]:0.0);
   
   /*Iterate through the temporary tessselation vertices*/
   glTessNo=glTessNo<(GL_MAXTESS-1)?glTessNo+1:0;
}

static void glTessVertexNr(GLdouble *Vertex) {
   glNormal3dv(Vertex+3);
   glVertex3dv(Vertex);
}

static void glTessCombine(GLdouble coords[3],GLdouble *d[4],GLfloat w[4],GLdouble **Out) {

   *Out=glTessTmp[glTessNo];

   glTessTmp[glTessNo][0]=coords[0];
   glTessTmp[glTessNo][1]=coords[1];
   glTessTmp[glTessNo][2]=coords[2];
   
   /*Iterate through the temporary tessselation vertices*/
   glTessNo=glTessNo<(GL_MAXTESS-1)?glTessNo+1:0;
}

static void glTessVertex(GLdouble *Vertex) {

   glNormal3dv(Vertex);
   glVertex3dv(Vertex);
}

void glTessInit() {
   
   gluTessCallback(GLRender->GLTess,GLU_TESS_BEGIN,(_GLUfuncptr)glBegin);
   gluTessCallback(GLRender->GLTess,GLU_TESS_VERTEX,(_GLUfuncptr)glTessVertex);
   gluTessCallback(GLRender->GLTess,GLU_TESS_END,(_GLUfuncptr)glEnd);
   gluTessCallback(GLRender->GLTess,GLU_TESS_COMBINE,(_GLUfuncptr)glTessCombine);
   gluTessCallback(GLRender->GLTess,GLU_TESS_ERROR,(_GLUfuncptr)glTessError);

   gluTessProperty(GLRender->GLTess,GLU_TESS_BOUNDARY_ONLY,GL_FALSE);
   gluTessProperty(GLRender->GLTess,GLU_TESS_WINDING_RULE,GLU_TESS_WINDING_ODD);
}

void glTessInitNr() {
   
   gluTessCallback(GLRender->GLTess,GLU_TESS_BEGIN,(_GLUfuncptr)glBegin);
   gluTessCallback(GLRender->GLTess,GLU_TESS_VERTEX,(_GLUfuncptr)glTessVertexNr);
   gluTessCallback(GLRender->GLTess,GLU_TESS_END,(_GLUfuncptr)glEnd);
   gluTessCallback(GLRender->GLTess,GLU_TESS_COMBINE,(_GLUfuncptr)glTessCombineNr);
   gluTessCallback(GLRender->GLTess,GLU_TESS_ERROR,(_GLUfuncptr)glTessError);

   gluTessProperty(GLRender->GLTess,GLU_TESS_BOUNDARY_ONLY,GL_FALSE);
   gluTessProperty(GLRender->GLTess,GLU_TESS_WINDING_RULE,GLU_TESS_WINDING_ODD);
}
/*----------------------------------------------------------------------------
 * Nom      : <glInfo>
 * Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Obtenir des informations sur OpenGL (GL).
 *
 * Parametres :
 *  <Interp>     : Interpreteur TCL
 *  <Argv>       : Pointeur sur l'argument
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int glInfo(Tcl_Interp *Interp,char *Argv){

   char buf[64];

   if (strcmp(Argv,"GL_VERSION") == 0) {
      Tcl_AppendResult(Interp,glGetString(GL_VERSION),(char *)NULL);
   } else if (strcmp(Argv,"GL_VENDOR") == 0) {
      Tcl_AppendResult(Interp,glGetString(GL_VENDOR),(char *)NULL);
   } else if (strcmp(Argv,"GL_RENDERER") == 0) {
      Tcl_AppendResult(Interp,glGetString(GL_RENDERER),(char *)NULL);
   } else if (strcmp(Argv,"GL_EXTENSIONS") == 0) {
      Tcl_AppendResult(Interp,glGetString(GL_EXTENSIONS),(char *)NULL);
   } else if (strcmp(Argv,"GLU_VERSION") == 0) {
      Tcl_AppendResult(Interp,gluGetString(GLU_VERSION),(char *)NULL);
   } else if (strcmp(Argv,"GLU_EXTENSIONS") == 0) {
      Tcl_AppendResult(Interp,gluGetString(GLU_EXTENSIONS),(char *)NULL);
   } else if (strcmp(Argv,"GLX_VERSION") == 0) {
      Tcl_AppendResult(Interp,glXGetClientString(GLRender->XDisplay,GLX_VERSION),(char *)NULL);
   } else if (strcmp(Argv,"GLX_VENDOR") == 0) {
      Tcl_AppendResult(Interp,glXGetClientString(GLRender->XDisplay,GLX_VENDOR),(char *)NULL);
   } else if (strcmp(Argv,"GLX_EXTENSIONS") == 0) {
      Tcl_AppendResult(Interp,glXGetClientString(GLRender->XDisplay,GLX_EXTENSIONS),(char *)NULL);
   } else if (strcmp(Argv,"X_VISUAL") == 0) {
      if (GLRender->GLVis->class == PseudoColor) {
         Tcl_AppendResult(Interp,"PseudoColor",(char *)NULL);
      } else if (GLRender->GLVis->class == DirectColor) {
         Tcl_AppendResult(Interp,"DirectColor",(char *)NULL);
      } else if (GLRender->GLVis->class == TrueColor) {
         Tcl_AppendResult(Interp,"TrueColor",(char *)NULL);
      } else if (GLRender->GLVis->class == StaticColor) {
         Tcl_AppendResult(Interp,"StaticColor",(char *)NULL);
      }
   } else if (strcmp(Argv,"X_DEPTH") == 0) {
      sprintf(buf,"%i",GLRender->GLVis->depth);
      Tcl_AppendResult(Interp,buf,(char *)NULL);
   } else {
      Tcl_AppendResult(Interp,"\n   wrong option: must be \"GLX_VERSION GLX_VENDOR GLX_EXTENSIONS GL_VERSION GL_VENDOR GL_RENDERER GL_EXTENSIONS GLU_VERSION GLU_EXTENSIONS X_DEPTH X_VISUAL\"",(char*)NULL);
      return TCL_ERROR;
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <glCheckExtension>
 * Creation : Novemnre 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Determiner l'extistence d'une extension.
 *
 * Parametres :
 *  <ExtName> : Nom de l'extension
 *
 * Retour:
 *  <Existence> : True ou False
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
GLboolean glCheckExtension(char *ExtName) {

   char *p=(char*)glGetString(GL_EXTENSIONS);
   char *end;
   int   len;

   len=strlen(ExtName);
   end=p+strlen(p);

   while (p<end) {
      int n=strcspn(p," ");
      if ((len==n) && (strncmp(ExtName,p,n)==0)) {
         return GL_TRUE;
      }
      p+=(n+1);
   }
   return GL_FALSE;
}

/*----------------------------------------------------------------------------
 * Nom      : <initialisationGLLightingParams>
 * Creation : Mai 2019 A. Germain
 *
 * But      : Initialiser les parametres qui sont utilises pour
 *              l'illumination avec des valeurs par defaut
 *
 * Parametres :
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void initialisationGLLightingParams(){
    GLRender->GLLighting = malloc(sizeof(GLLightingParams));
    GLRender->GLLighting->GLLightAmbiant = 0.1;
    GLRender->GLLighting->GLLightDiffuse = 1.5;
    GLRender->GLLighting->GLLightSpecular = 0.6;
    GLRender->GLLighting->GLMaterialSpecular = 1.0;
    GLRender->GLLighting->GLMaterialShininess = 64;
}

/*----------------------------------------------------------------------------
 * Nom      : <glRender_Cmd>
 * Creation : Juillet 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Gestion des parametres du renderer.
 *
 * Parametres :
 *  <clientData> : Nom du viewpoint
 *  <Interp>     : Interpreteur TCL
 *  <Objc>       : Nombre d'arguments
 *  <Objv>       : Pointeur sur la liste des arguments
 *
 * Retour:
 *  <TCL_...>    : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int  glRender_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   int      i,n;
   int      idx;
   Tcl_Obj *obj;

   static CONST char *sopt[] = { "-init","-shutdown","-shaderpath","-shaders","-resolution","-aliasing","-fsaa","-dithering","-shading","-filtering","-zbuffer","-time","-xexpose","-xbatch","-debug","-direct","-shaderavailable","-info","-delay","-wait","-usethreads","-lightambiant","-lightdiffuse","-lightspecular","-materialspecular","-materialshininess",NULL };
   enum                opt { INIT,SHUTDOWN,SHADERPATH,SHADERS,RESOLUTION,ALIASING,FSAA,DITHERING,SHADING,FILTERING,ZBUFFER,TIME,XEXPOSE,XBATCH,DEBUG,DIRECT,SHADERAVAILABLE,INFO,DELAY,WAIT,USETHREADS,LIGHTAMBIANT,LIGHTDIFFUSE,LIGHTSPECULAR,MATERIALSPECULAR,MATERAILSHININESS };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return TCL_ERROR;
   }

   for(i=1;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"command",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {
         case INIT:
            glXCanvasInit(Interp,Tk_MainWindow(Interp));
            break;

         case SHUTDOWN:
            glXShutDown();
            break;

         case SHADERPATH:
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(GLRender->ShaderPath,-1));
            } else {
               if (GLRender->ShaderPath) {
                  free(GLRender->ShaderPath);
                  GLRender->ShaderPath=NULL;
               }
               if (strlen(Tcl_GetString(Objv[++i]))) {
                  GLRender->ShaderPath=strdup(Tcl_GetString(Objv[i]));
               }
            }
            break;

         case SHADERS:
            if (Objc==2) {
               obj=Tcl_NewListObj(0,NULL);
               for(n=0;n<GLRender->ShaderNb;n++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(GLRender->Shaders[n],-1));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (GLRender->Shaders) {
                  Tcl_Free((char*)GLRender->Shaders);
                  GLRender->Shaders=NULL;
               }
               if ((Tcl_SplitList(Interp,Tcl_GetString(Objv[++i]),&GLRender->ShaderNb,(const char***)&GLRender->Shaders)==TCL_ERROR)) {
                  return(TCL_ERROR);
               }
               if (GLRender->ShaderNb>GL_MAXSHADER) {
                  Tcl_AppendResult(Interp,"glRender_Cmd: too many shaders",(char*) NULL);
                  return(TCL_ERROR);
               }
           }
           break;
            
         case RESOLUTION:
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(GLRender->Resolution));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&GLRender->Resolution);
            }
            break;

         case ALIASING:
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GLRender->GLAlias));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&GLRender->GLAlias);
               glDefineParams(Interp);
            }
            break;

         case FSAA:
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(GLRender->GLFSAA));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&GLRender->GLFSAA);
               glDefineParams(Interp);
            }
            break;
            
         case DITHERING:
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GLRender->GLDither));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&GLRender->GLDither);
               glDefineParams(Interp);
            }
            break;

         case SHADING:
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GLRender->GLShade));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&GLRender->GLShade);
               glDefineParams(Interp);
            }
            break;

         case FILTERING:
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GLRender->GLFilter));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&GLRender->GLFilter);
               glDefineParams(Interp);
            }
            break;

         case ZBUFFER:
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GLRender->GLZBuf));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&GLRender->GLZBuf);
            }
            break;

         case TIME:
            Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(GLRender->RenderTime));
            break;

         case SHADERAVAILABLE:
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GLRender->ShaderAvailable));
            break;

         case DIRECT:
            if (GLRender->GLDirect) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
            } else {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
            }
            break;

         case INFO:
            glInfo(Interp,Tcl_GetString(Objv[++i]));
            break;

         case XBATCH:
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GLRender->XBatch));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&GLRender->XBatch);
            }
            break;

         case XEXPOSE:
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(GLRender->XExpose));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&GLRender->XExpose);
            }
            break;

         case DELAY:
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(GLRender->Delay));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&GLRender->Delay);
               
               if (Objc>3 && i+1<Objc && Tcl_GetString(Objv[i+1])[0]!='-') {
                  if (GLRender->DelayProc) {
                     Tcl_DecrRefCount(GLRender->DelayProc);
                  }
                  GLRender->DelayProc=Tcl_DuplicateObj(Objv[++i]);
                  Tcl_IncrRefCount(GLRender->DelayProc);
               }
            }
            break;

         case WAIT:
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GLRender->Wait));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&GLRender->Wait);
            }
            break;

        case USETHREADS:
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GLRender->UseThreads));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&GLRender->UseThreads);
            }
            break;
            
         case DEBUG:
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(GLRender->GLDebug));
            } else {
               Tcl_GetBooleanFromObj(Interp,Objv[++i],&GLRender->GLDebug);
            }
            break;
         case LIGHTAMBIANT:
         {
            if(GLRender->GLLighting == NULL) {
                initialisationGLLightingParams();
            }
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj((double)GLRender->GLLighting->GLLightAmbiant));
            } else {
               double x=0;
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&x);
               GLRender->GLLighting->GLLightAmbiant = (float)x;
            }
            break;
         }
         case LIGHTDIFFUSE:
         {
            if(GLRender->GLLighting == NULL) {
                initialisationGLLightingParams();
            }
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj((double)GLRender->GLLighting->GLLightDiffuse));
            } else {
               double x=0;
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&x);
               GLRender->GLLighting->GLLightDiffuse = (float)x;
            }
            break;
         }
         case LIGHTSPECULAR:
         {
            if(GLRender->GLLighting == NULL) {
                initialisationGLLightingParams();
            }
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj((double)GLRender->GLLighting->GLLightSpecular));
            } else {
               double x=0;
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&x);
               GLRender->GLLighting->GLLightSpecular = (float)x;
            }
            break;
         }
         case MATERIALSPECULAR:
         {
            if(GLRender->GLLighting == NULL) {
                initialisationGLLightingParams();
            }
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj((double)GLRender->GLLighting->GLMaterialSpecular));
            } else {
               double x=0;
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&x);
               GLRender->GLLighting->GLMaterialSpecular = (float)x;
            }
            break;
         }
         case MATERAILSHININESS:
         {
            if(GLRender->GLLighting == NULL) {
                initialisationGLLightingParams();
            }
            if (Objc==2) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(GLRender->GLLighting->GLMaterialShininess));
            } else {
               int x=0;
               Tcl_GetIntFromObj(Interp,Objv[++i],&x);
               GLRender->GLLighting->GLMaterialShininess = x;
            }
            break;
         }
      }
   }
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <trRasterPos2i>
 * Creation : Novembre 2003 J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectuer les transformations necessaire pour changer la position
 *            raster d'OpenGL selon les tuiles
 *
 *
 * Parametres :
 *  <X>          : Coordonne x du coin superieur gauche
 *  <Y>          : Coordonne y du coin superieur gauche
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void trRasterPos2i(int X,int Y) {

   if (GLRender->TRCon) {
      int y;

      y=GLRender->TRCon->ImageHeight-(GLRender->TRCon->Rows*GLRender->TRCon->TileHeightNB);
      y=GLRender->TRCon->CurrentRow==GLRender->TRCon->Rows-1?0:-y;

      X-=GLRender->TRCon->CurrentColumn*GLRender->TRCon->TileWidthNB;
      Y+=(GLRender->TRCon->Rows-GLRender->TRCon->CurrentRow-1)*GLRender->TRCon->TileHeightNB-y;

      glMatrixMode(GL_MODELVIEW);
      glPushMatrix();
      glLoadIdentity();
      glMatrixMode(GL_PROJECTION);
      glPushMatrix();
      glLoadIdentity();
      glOrtho(0.0,GLRender->TRCon->CurrentTileWidth-1,GLRender->TRCon->CurrentTileHeight-1,0.0,-1.0,1.0);

      glRasterPos2i(0,0);
      glBitmap(0,0,0.0f,0.0f,X,Y,NULL);

      glPopMatrix();
      glMatrixMode(GL_MODELVIEW);
      glPopMatrix();
   } else {
      glRasterPos2i(0,0);
      glBitmap(0,0,0.0f,0.0f,X,Y,NULL);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <trBuffer>
 * Creation : Novembre 2003 J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer la tuile buffer OpenGL et le retourner a Tk dans une
 *            image TK.
 *
 * Parametres :
 *  <Interp>     : Interpreteur TCL
 *  <Img>        : Identificateur de l'image TK
 *  <Buffer>     : Buffer a lire (GL_FRONT|GL_BACK);
 *  <X>          : Coordonne x du coin superieur gauche
 *  <Y>          : Coordonne y du coin superieur gauche
 *  <Width>      : Coordonne x du coin inferieur droit
 *  <Height>     : Hauteur du buffer originale
 *  <TR>         : Contexte TR (Tile renderer)
 *
 * Retour:
 *  <TCL_...>    : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/

int trBuffer(Tcl_Interp *Interp,char* Img,int Buffer,int X,int Y,int Width,int Height,TRcontext *TR){

   Tk_PhotoImageBlock data;
   Tk_PhotoHandle     handle;
   int                i,ix,iy,dx,dy,result=TCL_ERROR;

   /*Calculer le depassement si il y a*/
   ix=TR->CurrentColumn*TR->TileWidthNB-X;
   dx=ix<0?-ix:0;

   iy=TR->ImageHeight-(TR->CurrentRow*TR->TileHeightNB+TR->TileHeightNB)-Y;
   dy=iy<0?-iy:0;
   if (ix>Width || iy>Height || ix+TR->TileWidthNB<0 || iy+TR->TileHeightNB<0) {
      return(TCL_OK);
   }

   /*Recuperer le handle de l'image specifie*/
   if ((handle=Tk_FindPhoto(Interp,Img))) {

      /*Definire les parametres du block de donnees */
      if (Tk_PhotoSetSize(Interp,handle,Width,Height)==TCL_OK) {
         data.width=TR->TileWidthNB-dx;
         data.height=TR->TileHeightNB-dy;
         data.pixelSize=3;
         data.pitch=data.width*data.pixelSize;
         data.offset[0]=0;
         data.offset[1]=1;
         data.offset[2]=2;
         data.offset[3]=0;
         if (!(data.pixelPtr=(unsigned char*)malloc(data.width*data.height*data.pixelSize*sizeof(unsigned char)))) {
           Tcl_AppendResult(Interp,"trBuffer: Unable to allocate image",(char*)NULL);
           result=TCL_ERROR;
         } else {

            /* Recuperer le buffer OpenGL en inversant en Y*/
            glReadBuffer(Buffer);
            for(i=0;i<data.height;i++) {
               glReadPixels(TR->TileBorder+dx,TR->TileHeightNB-i-1+TR->TileBorder-dy,data.width,1,GL_RGB,GL_UNSIGNED_BYTE,&data.pixelPtr[i*data.pitch]);
            }
            
            /* Envoyer le data dans l'image Tk */
            result=Tk_PhotoPutBlock(Interp,handle,&data,(ix<0?0:ix),(iy<0?0:iy),data.width,data.height,TK_PHOTO_COMPOSITE_SET);
            free(data.pixelPtr);
         }
      }
   }

   return(result);
}

/*----------------------------------------------------------------------------
 * Nom      : <glBuffer>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer le buffer OpenGL et le retourner a Tk dnas une
 *            image TK.
 *
 * Parametres :
 *  <Interp>     : Interpreteur TCL
 *  <Img>        : Identificateur de l'image TK
 *  <Buffer>     : Buffer a lire (GL_FRONT|GL_BACK);
 *  <X0>         : Coordonne x du coin superieur gauche
 *  <Y0>         : Coordonne y du coin superieur gauche
 *  <W           : Largeur
 *  <H>          : Hauteur
 *  <Height>     : Hauteur du buffer originale
 *
 * Retour:
 *  <TCL_...>    : Code de reussite Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int glBuffer(Tcl_Interp *Interp,char* Img,int Buffer,int X0,int Y0,int W,int H,int Height){

   Tk_PhotoImageBlock data;
   Tk_PhotoHandle     handle;
   int                i,result=TCL_ERROR;

   data.width=W;
   data.height=H;

   /*Recuperer le handle de l'image specifie*/
   if ((handle=Tk_FindPhoto(Interp,Img))) {

      /*Definire les parametres du bock de donnees*/
      Tk_PhotoSetSize(Interp,handle,data.width,data.height);

      data.pixelSize=4;
      data.pitch=data.width*data.pixelSize;
      data.offset[0]=0;
      data.offset[1]=1;
      data.offset[2]=2;
      data.offset[3]=3;
      if (!(data.pixelPtr=(unsigned char*)malloc(data.width*data.height*data.pixelSize*sizeof(unsigned char)))) {
         Tcl_AppendResult(Interp,"glBuffer: Unable to allocate image",(char*)NULL);
         result=TCL_ERROR;
      } else {

         /*Recuperer le buffer OpenGL*/
         glReadBuffer(Buffer);
         for(i=0;i<data.height;i++) {
            glReadPixels(X0,Height-(Y0+i)-1,data.width,1,GL_RGBA,GL_UNSIGNED_BYTE,&data.pixelPtr[i*data.pitch]);
         }
         /*Envoyer le data dans l'image Tk*/
         result=Tk_PhotoPutBlock(Interp,handle,&data,0,0,data.width,data.height,TK_PHOTO_COMPOSITE_SET);
         free(data.pixelPtr);
      }
   }

   return(result);
}

/*----------------------------------------------------------------------------
 * Nom      : <glHashTableInit>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser les tables contenant les structures GL.
 *
 * Parametres :
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void glHashTableInit() {

   Tcl_InitHashTable(&glFontIdTable,TCL_STRING_KEYS);
   Tcl_InitHashTable(&glBitmapTable,TCL_STRING_KEYS);
}

/*----------------------------------------------------------------------------
 * Nom      : <glFontUse>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Selectionner la police et avtiver la liste d'affichage correspondante.
 *
 * Parametres :
 *   <Disp>   : Display
 *   <FontId> : Identificateur de la police
 *
 * Retour:
 *   <base>   : Point de depart de la liste d'affichage pour la police
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int glFontUse(Display *Disp,Tk_Font FontId) {

   Tcl_HashEntry  *entry;
   int             new;
   unsigned int    f0,f1;
   Font            fid;
   const char     *key;
   T_glFont       *glfont=NULL;
   XFontStruct    *def;

   fid=Tk_FontId(FontId);
   key=Tk_NameOfFont(FontId);
   entry=Tcl_FindHashEntry(&glFontIdTable,key);

   if (entry) {
      glfont=(T_glFont*)Tcl_GetHashValue(entry);

      if (glfont->fid!=fid) {
         /*Free font*/
         glXFontFree(glfont);
         glfont=NULL;
      }
   }

   if (!glfont) {
      /* Get the font */
      glfont=(T_glFont*)malloc(sizeof(T_glFont));
      glfont->fid=fid;
      def=XQueryFont(Disp,fid);
      if (!def) {
         fprintf(stderr,"(ERROR) glFontSet: Unable to get font information.\n");
      }
      f0=def->min_char_or_byte2;
      f1=def->max_char_or_byte2;

      glfont->nchar=(GLuint)f1+1;
      glfont->list=glGenLists(glfont->nchar);
      glfont->tex=(GLuint*)malloc(glfont->nchar*sizeof(GLuint));
      glGenTextures(glfont->nchar,glfont->tex);

      if (!glfont->list || !glfont->tex)
         fprintf(stderr,"(ERROR) glFontSet: Unable to allocate font display list.\n");

//      glXUseXFont(def->fid,f0,f1-f0+1,base+f0);
         glXFontTexture(def->fid,f0,f1-f0+1,glfont->list+f0,glfont->tex);

      if (!entry)
         entry=Tcl_CreateHashEntry(&glFontIdTable,key,&new);

      Tcl_SetHashValue(entry,glfont);

      GLRender->TkFont=FontId;
   }

   glListBase(glfont->list);

   return(glfont->list);
}

/*----------------------------------------------------------------------------
 * Nom      : <glBitmapFree>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Liberer la structure d'un bitmap.
 *
 * Parametres  :
 *  <Bitmap>   : Donnees du bitmap
 *
 * Retour      :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void glBitmapFree(T_glBitmap *Bitmap) {

   if (Bitmap) {
      if (Bitmap->Data) free(Bitmap->Data);
      if (Bitmap->Name) free(Bitmap->Name);

      free(Bitmap);
      Bitmap=NULL;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <glBitmapParseProc>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Gestion de structure glBitmap par Tk.
 *
 * Parametres  :
 *
 * Retour      :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int glBitmapParseProc(ClientData Data,Tcl_Interp *Interp,Tk_Window TkWin,const char *Value,char *WidgRec,int Offset){

   Tcl_HashEntry  *entry;
   int             new;
   T_glBitmap    **pbitmap=(T_glBitmap**)(WidgRec+Offset);
   T_glBitmap     *bitmap=NULL;
   int             len = strlen(Value);

   extern char *TkGetBitmapData();

   if (len > 0) {
      entry=Tcl_CreateHashEntry(&glBitmapTable,Value,&new);

      if (new) {
         bitmap=(T_glBitmap*)malloc(sizeof(T_glBitmap));

         /* Copy string the long way since "strdup" isn't C99 conformant. */
         len = (len + 1) * sizeof(char);
         bitmap->Name = (char*) malloc(len);
         memcpy(bitmap->Name, Value, len);

         bitmap->Data=(unsigned char*)TkGetBitmapData(Interp,NULL,++Value,&bitmap->Width,&bitmap->Height,&bitmap->HotX,&bitmap->HotY);
         DataFlip(bitmap->Data,NULL,bitmap->Width,bitmap->Height,0);

         Tcl_SetHashValue(entry,bitmap);
      } else {
         bitmap=(T_glBitmap*)Tcl_GetHashValue(entry);
      }
   }

   *pbitmap=bitmap;
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <glBitmapPrintProc>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Gestion de structure glBitmap par Tk.
 *
 * Parametres  :
 *
 * Retour      :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
const char *glBitmapPrintProc(ClientData Data,Tk_Window TkWin,char *WidgRec,int Offset,Tcl_FreeProc **FreeProcPtr){

   T_glBitmap **pbitmap=(T_glBitmap**)(WidgRec+Offset);
   T_glBitmap *bitmap=*pbitmap;

   return(bitmap->Name);
}

/*----------------------------------------------------------------------------
 * Nom      : <DataFlip>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cette procedure Inverse les lignes d'un bitmap X.
 *
 * Parametres  :
 *  <DataIn>   : Donnees In
 *  <DataOut>  : Donnees Out (Si NULL alors on flip dans DataIn lui meme)
 *  <Width>    : Largeur
 *  <Height>   : Hauteur
 *  <Size>     : Dimension
 *
 * Retour      :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void DataFlip(unsigned char *DataIn,unsigned char *DataOut,int Width,int Height,int Size){

   unsigned char *tmp;
   int           h0,h1,sz;

   if (Size) {
      sz=Size*Width;
   } else {
      sz=BITWIDTH(Width);
   }

   if (DataOut) {
      for (h0=0,h1=Height-1;h0<(Height>>1);h0++,h1--) {
         memcpy(DataOut+(h1*sz),DataIn+(h0*sz),sz);
         memcpy(DataOut+(h0*sz),DataIn+(h1*sz),sz);
      }
   } else {
      if ((tmp=(unsigned char*)alloca(sz))) {
         for (h0=0,h1=Height-1;h0<(Height>>1);h0++,h1--) {
            memcpy(tmp,DataIn+(h0*sz),sz);
            memcpy(DataIn+(h0*sz),DataIn+(h1*sz),sz);
            memcpy(DataIn+(h1*sz),tmp,sz);
         }
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <glPostscriptBitmap>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cette procedure genere du code Postscript pour un bitmap X.
 *
 * Parametres  :
 *  <Interp>   : Interpreteur Tcl/Tk
 *  <Bitmap>   : Donnees du bitmap
 *  <Width>    : Largeur du bitmap
 *  <Height>   : Hauteur du bitmap
 *  <psObj>    : Postscript object
 *
 * Retour      :
 *  <TCL...>   : Code de retour TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/

int glPostscriptBitmap(Tcl_Interp *Interp,Tcl_Obj *psObj,T_glBitmap *Bitmap,int Width,int Height) {

   unsigned char bm;
   char          string[8];
   int           x,y,idx,sz;

   if (Width*Height/8>65536) {
      Tcl_ResetResult(Interp);
      Tcl_AppendResult(Interp,"can't generate Postscript for bitmaps bigger than 64 K",(char*)NULL);
      return(TCL_ERROR);
   }

   sz=BITWIDTH(Width);

   if (psObj) {
      Tcl_AppendToObj(psObj,"<",-1);
   } else {
      Tcl_AppendResult(Interp,"<",(char*)NULL);
   }

   for (y=0;y<Height;y++) {
      idx=y*sz;
      for (x=0;x<sz;x++) {
          bm=Bitmap->Data[idx+x];
          BITSWAP(bm);

          if (psObj) {
             Tcl_AppendPrintfToObj(psObj,"%02x",bm);
          } else {
             sprintf(string,"%02x",bm);
             Tcl_AppendResult(Interp,string,(char*)NULL);
          }
      }
      if (psObj) {
         Tcl_AppendToObj(psObj, "\n", -1);
      } else {
         Tcl_AppendResult(Interp,"\n",(char*)NULL);
      }
   }

   if (psObj) {
      Tcl_AppendToObj(psObj, ">", -1);
   } else {
      Tcl_AppendResult(Interp,">",(char*)NULL);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <glPostscriptStipple>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cette procedure genere du code Postscript pour un stippe bitmap X.
 *
 * Parametres  :
 *  <Interp>   : Interpreteur Tcl/Tk
 *  <Bitmap>   : Donnees du bitmap
 *  <psObj>    : Postscript object
 *
 * Retour      :
 *  <TCL...>   : Code de retour TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int glPostscriptStipple(Tcl_Interp *Interp,Tcl_Obj *psObj,T_glBitmap *Bitmap) {

   if (psObj) {
      Tcl_AppendToObj(psObj,"32 32 ",-1);
   } else {
      Tcl_AppendResult(Interp,"32 32 ",(char*)NULL);
   }
   if (glPostscriptBitmap(Interp,psObj,Bitmap,32,32) != TCL_OK) {
      return(TCL_ERROR);
   }
   if (psObj) {
      Tcl_AppendToObj(psObj," StippleFill\n",-1);
   } else {
      Tcl_AppendResult(Interp," StippleFill\n",(char*)NULL);
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <glGenCircle>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Generer une liste de points decrivant un cercle unitaire avec.
 *            increment en 1/46 degree
 *
 * Parametres  :
 *
 * Retour      :
 *
 * Remarques :
 *    StartAngle is relative to the 3 O'Clock position
 *    EndAngle is relative to the StartAngle
 *
 *----------------------------------------------------------------------------
*/
void glGenCircle(float Start,float Extent) {

   float angle;
   float step=1.0/GL_ARCSTEP;

   for(angle=0;angle<360;angle+=step) {
      glArrayCircle[(int)(angle*GL_ARCSTEP*2)]   = (float)cos(angle*0.01745329251994);
      glArrayCircle[(int)(angle*GL_ARCSTEP*2+1)] = (float)sin(angle*0.01745329251994);
   }
   glArrayCircle[(int)(angle*GL_ARCSTEP*2)]   = glArrayCircle[0];
   glArrayCircle[(int)(angle*GL_ARCSTEP*2+1)] = glArrayCircle[1];
}

/*----------------------------------------------------------------------------
 * Nom      : <glDrawArc>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Afficher une arc de cercle
 *
 * Parametres  :
 *
 * Retour      :
 *
 * Remarques :
 *    StartAngle is relative to the 3 O'Clock position
 *    EndAngle is relative to the StartAngle
 *    Styles : PIESLICE_STYLE, CHORD_STYLE, ARC_STYLE
 *
 *----------------------------------------------------------------------------
*/
void glDrawArc(float Start,float Extent,int Inter,GLint Mode,int Style) {

   float i;

   Inter=Inter<0?1:Inter;

   if (Extent>360)
      Extent-=360.0;

   if (Extent<0) {
     Start+=Extent;
     Extent=-Extent;
   }

   if (Start<0.0)
      Start+=360.0;

   if (Start>360.0)
      Start-=360.0;

   glBegin(Mode);
      if (Style==0)
         glVertex2f(0.0,0.0);

      for(i=Start;i<(Start+Extent);i+=1.0) {
         if (i>360.0)
            glVertex2fv(&glArrayCircle[2*((int)(i-360.0)*GL_ARCSTEP)]);
         else
            glVertex2fv(&glArrayCircle[2*((int)i*GL_ARCSTEP)]);
      }

      if (Style==0)
         glVertex2f(0.0,0.0);
      if (Style==1)
         glVertex2fv(&glArrayCircle[(int)(2*Start)*GL_ARCSTEP]);

   glEnd();
}

/*----------------------------------------------------------------------------
 * Nom      : <glDrawCircle>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Afficher un cercle
 *
 * Parametres  :
 *
 * Retour      :
 *
 * Remarques :
 *    StartAngle is relative to the 3 O'Clock position
 *    EndAngle is relative to the StartAngle
 *
 *----------------------------------------------------------------------------
*/
void glDrawCircle(int Inter,GLint Mode) {

   Inter=Inter<0?1:Inter;

   glEnableClientState(GL_VERTEX_ARRAY);
   glVertexPointer(2,GL_FLOAT,Inter*sizeof(float)*2,glArrayCircle);
   glDrawArrays(Mode,0,(360*GL_ARCSTEP)/Inter+1);
   glDisableClientState(GL_VERTEX_ARRAY);
}

/*----------------------------------------------------------------------------
 * Nom      : <glDrawArrow>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Afficher une fleche
 *
 * Parametres  :
 *
 * Retour      :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void glDrawArrow(GLint Mode) {

   glEnableClientState(GL_VERTEX_ARRAY);
   glVertexPointer(2,GL_FLOAT,0,glArrayArrow);
   glDrawArrays(Mode,0,7);
   glDisableClientState(GL_VERTEX_ARRAY);
}

/*----------------------------------------------------------------------------
 * Nom      : <DashConvert>
 * Creation : Aout 2002 - J.P. Gauthier & David Dube - CMC/CMOE
 *
 * But      : Convertit les dash format Tk au format standard X
 *
 * Parametres  :
 *  <l>     : Donnees du bitmap
 *  <p>     : Largeur du bitmap
 *  <n>     : Hauteur du bitmap
 *  <width> : Hauteur du bitmap
 *
 * Retour    :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int DashConvert(char *l,CONST char *p,int n,double width) {

   int result = 0;
   int size, intWidth;

   if (n<0)
      n = strlen(p);

   intWidth = (int) (width + 0.5);
   if (intWidth < 1)
      intWidth = 1;

   while (n-- && *p) {
      switch (*p++) {
         case ' ':
            if (result) {
               if (l) {
                  l[-1] += intWidth + 1;
               }
               continue;
            } else {
               return 0;
            }
            break;
         case '_':
            size = 8;
            break;
         case '-':
            size = 6;
            break;
         case ',':
            size = 4;
            break;
         case '.':
            size = 2;
            break;
         default:
            return -1;
      }
      if (l) {
         *l++ = size * intWidth;
         *l++ = 4 * intWidth;
      }
      result += 2;
   }
   return result;
}

GLushort glDash(Tk_Dash *Dash) {

   GLushort pattern = 0x0;  /*Final bit pattern*/
   GLushort mask    = 0x1;  /*Iterative bit mask*/
   int      on      = 0xFF; /* Indicate if current bit is marked or not. */
   int      bits    = 16;   /* Pattern as a maximum number of 16 bits    */
   int      nbits   = 0;    /* Number of bits */
   int      i       = 0;

   char     *array,*p,*q=NULL;
   int      nb      = 0;    /*Number of dash items*/

   if (Dash->number<-1 || (Dash->number==-1 && Dash->pattern.array[1]!=',')) {
      nb = -Dash->number;
      p = (nb>sizeof(char *))?Dash->pattern.pt:Dash->pattern.array;
      q = (char*)alloca(2*(unsigned int)nb);
      nb = DashConvert(q,p,nb,1.0);
      array=q;
   } else if (Dash->number>=2) {
      array = (char*)(Dash->number>sizeof(char*)?Dash->pattern.pt:Dash->pattern.array);
      nb=Dash->number;
   } else {
      glDisable(GL_LINE_STIPPLE);
      return 0;
   }


   /* For each char in the pattern */
   while(bits>=0) {

      /* Divide the length by two since we can expand it with GL*/
      nbits=array[i]>>1;

      if (on) {
         /* Filled bits */
         if (bits-nbits<=1) break;

         for(;nbits>0;nbits--) {
            bits--;
            pattern|=mask<<bits;
         }
      } else {
         /* Blank bits */
         bits-=nbits;
      }

      on^=0xFF;

      /* if not filled, repeat*/
      if(++i==nb)
         i=0;
   }

   glEnable(GL_LINE_STIPPLE);
   glLineStipple(4,pattern);

   return pattern;
}

void DashPrint(char *String,Tk_Dash *Dash) {

   char *p;
   int i=Dash->number;

   if (i<0) {
      i=-i;
      p=(i>(int)sizeof(char*))?Dash->pattern.pt:Dash->pattern.array;
      memcpy(String,p,(unsigned int)i);
      String[i]=0;
      return;
   } else if (!i) {
      String[0]='\0';
      return;
   }
   p=(i>(int)sizeof(char*))?Dash->pattern.pt:Dash->pattern.array;
   sprintf(String,"%d ",*p++ & 0xff);
   while(--i) {
      sprintf(String+strlen(String)," %d",*p++&0xff);
   }
}

void glPostscriptDash(Tcl_Interp *Interp,Tk_Dash *Dash,int Width) {

   int i;

   char string[41];
   char pattern[11];
   char *ptr;
   char *str = string;
   char *lptr = pattern;

   if (!Dash) {
      Tcl_AppendResult(Interp,"[] 0 setdash\n",(char*)NULL);
      return;
   }

   if (Dash->number>10) {
      str=(char*)alloca((unsigned int)(1+4*Dash->number));
   } else if (Dash->number<-5) {
      str=(char*)alloca((unsigned int)(1-8*Dash->number));
      lptr=(char*)alloca((unsigned int)(1-2*Dash->number));
   }
   ptr=(char*)((fabs(Dash->number)>sizeof(char*))?Dash->pattern.pt:Dash->pattern.array);
   if (Dash->number>0) {
      char *ptr0=ptr;
      sprintf(str, "[%d", *ptr++ & 0xff);
      i=Dash->number-1;
      while (i--) {
         sprintf(str+strlen(str), " %d", *ptr++ & 0xff);
      }
      Tcl_AppendResult(Interp,str,(char*)NULL);

      if (Dash->number&1) {
         Tcl_AppendResult(Interp, " ", str+1, (char *)NULL);
      }
      sprintf(str, "] %d setDash\n", 0);
      Tcl_AppendResult(Interp, str, (char *)NULL);
      ptr = ptr0;
   } else if (Dash->number < 0) {
      if ((i = DashConvert(lptr, ptr, -Dash->number, Width)) != 0) {
         char *lptr0 = lptr;
         sprintf(str, "[%d", *lptr++ & 0xff);
         while (--i) {
            sprintf(str+strlen(str), " %d", *lptr++ & 0xff);
         }
         Tcl_AppendResult(Interp, str, (char *)NULL);
         sprintf(str, "] %d setdash\n", 0);
         Tcl_AppendResult(Interp, str, (char *)NULL);
         lptr = lptr0;
      } else {
         Tcl_AppendResult(Interp,"[] 0 setdash\n",(char*)NULL);
      }
   } else {
      Tcl_AppendResult(Interp,"[] 0 setdash\n",(char*)NULL);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <glPickInit>
 * Creation : Decembre 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser le mode "selection" d'OpenGL et allouer la memoire
 *            necessaire au traitement.
 *
 * Parametres :
 *  <WinX>    : Position en X
 *  <WinY>    : Position en Y
 *  <DX>      : Dimension en X
 *  <DY>      : Dimension en Y
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void glPickInit(double WinX,double WinY,double DX,double DY){

   GLint vp[4];

   glSelectBuffer(512,GLRender->GLPick);
   glRenderMode(GL_SELECT);
   glInitNames();

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();

   glGetIntegerv(GL_VIEWPORT,vp);
//   gluPickMatrix(WinX,vp[3]-WinY,DX,DY,vp);
   gluPickMatrix(WinX,WinY,DX,DY,vp);
}

/*----------------------------------------------------------------------------
 * Nom      : <glPickProcess>
 * Creation : Decembre 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Parcourir les resultats du mode "selection" d'OpenGL et retourner
 *            la primitive selectionne la plus pres (ZBuffer).
 *
 * Parametres :
 *
 * Retour:
 *    <uint>  : Nom (Identificateur) de la primitive
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int glPickProcess(){

   int picks,i,j;
   GLuint names,*pname=NULL,*ptr,*ptro,n=0,z=0xffffffff;

   ptro=ptr=GLRender->GLPick;
   picks=glRenderMode(GL_RENDER);

   if (picks>0) {
      for(i=0;i<picks;i++) {
         names=*ptr;
         ptr++;
         if (*ptr<z) {
              n=names;
              z=*ptr;
              pname=ptr+2;
         }
         ptr+=names+2;
      }

      for(j=0;j<n;j++,pname++,ptro++) {
         *ptro=*pname;
      }
   }
   return(picks);
}

/*----------------------------------------------------------------------------
 * Nom      : <glFeedbackInit>
 * Creation : Novembre 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser le mode "feedback" d'OpenGL et allouer la memoire
 *            necessaire au traitement.
 *
 * Parametres :
 *  <Size>    : Dimension du tableau.
 *  <GLMode>  : Mode.
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
GLfloat* glFeedbackInit(unsigned long Size,int GLMode) {

   if (GLRender->GLFeedSize<Size) {
      GLRender->GLFeed=(GLfloat*)realloc(GLRender->GLFeed,Size*sizeof(GLfloat));
      GLRender->GLFeedSize=Size;
   }

   if (!GLRender->GLFeed) {
      fprintf(stderr,"(ERROR) GLFeedbackInit: Not enough memory to initiate OpenGL feedback mode");
   } else {
      glFeedbackBuffer(Size,GLMode,GLRender->GLFeed);
      glRenderMode(GL_FEEDBACK);
   }
   return(GLRender->GLFeed);
}

/*----------------------------------------------------------------------------
 * Nom      : <glFeedbackProcess>
 * Creation : Novembre 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Parcourir les resultats du mode "feedback" d'OpenGL et creer du
 *            postscript. On libere aussi l'espace memoire.
 *
 * Parametres :
 *
 * Retour:
 *  <Interp> : Interpreteur TCL.
 *  <GLMode> : Mode.
 *
 * Remarques :
 *    - Quand des segments de lignes sont traites en "Array", il arrive que le
 *      debut de la ligne passe inappercu. C'est pour ca qu'on fait une verification sur le "move"
 *      pour etre sur que le "moveto" du postscript a ete effectue avant de faire le "lineto".
 *
 *----------------------------------------------------------------------------
*/
int glFeedbackProcess(Tcl_Interp *Interp,int GLMode) {

   int     no=0,token,d=0,nb,n,move=0,poly=0;
   int     pn,pno;
   char    buf[256];
   float   px=0.0,py=0.0;

   nb=glRenderMode(GL_RENDER);
   switch(GLMode) {
      case GL_2D: d=0; break;
      case GL_3D: d=1; break;
   }

   while(no<nb) {

      token=(int)GLRender->GLFeed[no++];

      switch(token) {

         case GL_POINT_TOKEN :
            sprintf(buf,"%1.3f %1.3f moveto %1.3f %1.3f lineto\n",
               GLRender->GLFeed[no],GLRender->GLFeed[no+1],GLRender->GLFeed[no],GLRender->GLFeed[no+1]);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
            break;

         case GL_LINE_RESET_TOKEN :
            move=1;
            sprintf(buf,"%1.3f %1.3f moveto %1.3f %1.3f lineto\n"
               ,GLRender->GLFeed[no],GLRender->GLFeed[no+1],GLRender->GLFeed[no+2],GLRender->GLFeed[no+3]);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
            no+=2+d;
            px=GLRender->GLFeed[no];
            py=GLRender->GLFeed[no+1];
            break;

         case GL_LINE_TOKEN :
            if (!move) {
               move=1;
               sprintf(buf,"%1.3f %1.3f moveto %1.3f %1.3f lineto\n",
                  GLRender->GLFeed[no],GLRender->GLFeed[no+1],GLRender->GLFeed[no+2],GLRender->GLFeed[no+3]);
            } else {
               /* On verifie le dernier point pour voir si on peut faire une linestrip plutot qu'un segment*/
               if (px!=GLRender->GLFeed[no] || py!=GLRender->GLFeed[no+1]) {
                  sprintf(buf,"%1.3f %1.3f moveto %1.3f %1.3f lineto\n",
                     GLRender->GLFeed[no],GLRender->GLFeed[no+1],GLRender->GLFeed[no+2],GLRender->GLFeed[no+3]);
               } else {
                  sprintf(buf,"%1.3f %1.3f lineto\n",GLRender->GLFeed[no+2],GLRender->GLFeed[no+3]);
               }
            }
            no+=2+d;
            px=GLRender->GLFeed[no];
            py=GLRender->GLFeed[no+1];
            Tcl_AppendResult(Interp,buf,(char*)NULL);
            break;

         case GL_POLYGON_TOKEN :
            poly=1;

            if (no>0) {
               Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);
            }

            pn=n=(int)GLRender->GLFeed[no];
            no++;
            pno=no;
            sprintf(buf,"%1.3f %1.3f moveto",GLRender->GLFeed[pno],GLRender->GLFeed[pno+1]);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
            while (--pn) {
               pno+=2+d;
               sprintf(buf," %1.3f %1.3f lineto",GLRender->GLFeed[pno],GLRender->GLFeed[pno+1]);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            }
            Tcl_AppendResult(Interp," eofill\n",(char*)NULL);

            /* Il faut reimprimer les contours pour s'assurer que les joints corrects*/
            sprintf(buf,"%1.3f %1.3f moveto",GLRender->GLFeed[no],GLRender->GLFeed[no+1]);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
            while (--n) {
               no+=2+d;
               sprintf(buf," %1.3f %1.3f lineto",GLRender->GLFeed[no],GLRender->GLFeed[no+1]);
               Tcl_AppendResult(Interp,buf,(char*)NULL);
            }
            Tcl_AppendResult(Interp," closepath stroke\n",(char*)NULL);
            break;
         default: fprintf(stderr,"(ERROR) GLFeedbackProcess : Bad Token identifier (%i)\n",no);
       }
       no+=2+d;
   }

   if (no>0 && !poly) {
      Tcl_AppendResult(Interp,"stroke\n",(char*)NULL);
   }

   return(no);
}

/*----------------------------------------------------------------------------
 * Nom      : <glPostscriptBuffer>
 * Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Generer du postscript pour un pixmap.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Buffer>  : Buffer a lire (GL_FRONT|GL_BACK);
 *  <Width>   : Largeur de l'image.
 *  <Height>  : Hauteur de l'image.
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void glPostscriptBuffer(Tcl_Interp *Interp,int Buffer,int X0,int Y0,int Width,int Height){

   char   buf[128];
   GLubyte *pix;
   int    n;
   XColor cells[256];

   pix=(GLubyte*)malloc(Width*Height*3*sizeof(GLubyte));

   if (pix) {

      /*Recuperer le buffer OpenGL selon le mode RGBA ou COLORINDEX*/
      glReadBuffer(Buffer);
      if (GLRender->XMode==GLX_RGBA) {
         glReadPixels(X0,Y0,Width,Height,GL_RGB,GL_UNSIGNED_BYTE,pix);
      } else {
         glReadPixels(X0,Y0,Width,Height,GL_COLOR_INDEX,GL_UNSIGNED_BYTE,pix);

         for(n=0;n<256;n++){
            cells[n].pixel=n;
            cells[n].flags=DoRed|DoGreen|DoBlue;
    }
         XQueryColors(GLRender->XDisplay,GLRender->XColormap,cells,256);
      }

      /*Initialiser l'image dans le fichier postscript*/
      sprintf(buf,"\n%% Postscript d'un buffer OpenGL de dimension %ix%i\n/pix %i string def\n/curfile currentfile def\n",
         Width,Height,Width);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
      sprintf(buf,"gsave\n%i %i 8 matrix\n",Width,Height);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
      Tcl_AppendResult(Interp,"[curfile pix /readhexstring cvx /pop cvx] cvx bind\nfalse 3 colorimage\n",(char*)NULL);

      /*Imprimer l'image end RGB*/
      if (GLRender->XMode==GLX_RGBA) {
         for(n=0;n<Height*Width*3;n+=3){
            sprintf(buf,"%02x%02x%02x",pix[n],pix[n+1],pix[n+2]);
            Tcl_AppendResult(Interp,buf,(char *)NULL);
         }
       } else {
         for(n=0;n<Height*Width;n++){
            sprintf(buf,"%02x%02x%02x",cells[pix[n]].red>>8,cells[pix[n]].green>>8,cells[pix[n]].blue>>8);
            Tcl_AppendResult(Interp,buf,(char *)NULL);
         }
      }
      Tcl_AppendResult(Interp,"\ngrestore\n",(char*)NULL);
      free(pix);
   } else {
      Tcl_AppendResult(Interp,"Could not allocate pixel buffer\n",(char*)NULL);
   }
}

void trPostscriptBuffer(Tcl_Interp *Interp,int Buffer,int X0,int Y0,int Width,int Height,TRcontext *TR){

   char   buf[128];
   GLubyte *pix;
   int    n,w,h;
   XColor cells[256];

   /*Determiner les dimensions necessaires*/
   w=TR->CurrentTileWidth-2*TR->TileBorder;
   h=TR->CurrentTileHeight-2*TR->TileBorder;
   w=Width<w-X0?Width:w-X0;
   h=Height<h-Y0?Height:h-Y0;

   pix=(GLubyte*)malloc(h*w*3*sizeof(GLubyte));

   if (pix) {

      /*Initialiser l'image dans le fichier postscript*/
      sprintf(buf,"\n%% Postscript d'un buffer OpenGL de dimension %ix%i\n/pix %i string def\n/curfile currentfile def\n",w,h,w);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
      sprintf(buf,"gsave\n%i %i translate\n%i %i 8 matrix\n",TR->CurrentColumn*TR->TileWidthNB,TR->CurrentRow*TR->TileHeightNB,w,h);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
      Tcl_AppendResult(Interp,"[curfile pix /readhexstring cvx /pop cvx] cvx bind\nfalse 3 colorimage\n",(char*)NULL);

      /*Imprimer l'image end RGB*/
      if (GLRender->XMode==GLX_RGBA) {
         glReadPixels(X0+TR->TileBorder,Y0+TR->TileBorder,w,h,GL_RGB,GL_UNSIGNED_BYTE,pix);

         for(n=0;n<h*w*3;n+=3){
            sprintf(buf,"%02x%02x%02x",pix[n],pix[n+1],pix[n+2]);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
         }
       } else {
          glReadPixels(X0+TR->TileBorder,Y0+TR->TileBorder,w,h,GL_COLOR_INDEX,GL_UNSIGNED_BYTE,pix);

          for(n=0;n<256;n++){
             cells[n].pixel=n;
             cells[n].flags=DoRed|DoGreen|DoBlue;
          }
          XQueryColors(GLRender->XDisplay,GLRender->XColormap,cells,256);

          for(n=0;n<h*w;n++){
            sprintf(buf,"%02x%02x%02x",cells[pix[n]].red>>8,cells[pix[n]].green>>8,cells[pix[n]].blue>>8);
            Tcl_AppendResult(Interp,buf,(char*)NULL);
         }
      }

      /*Pour un raison heteroclyte, quand les tuiles sont plus petite il manque 2 bytes*/
      if (w>TR->CurrentTileWidth || h>TR->CurrentTileHeight) {
         sprintf(buf,"aa");
         Tcl_AppendResult(Interp,buf,(char*)NULL);
      }
      Tcl_AppendResult(Interp,"\ngrestore\n",(char*)NULL);
      free(pix);
   } else {
      Tcl_AppendResult(Interp,"Could not allocate pixel buffer\n",(char*)NULL);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <glPostscriptRectangle>
 * Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Generer du postscript pour un rectangle.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Canvas>  : Canvas Tk
 *  <X1>      : Coordonne X du 1er point
 *  <Y1>      : Coordonne Y du 1er point
 *  <X2>      : Coordonne X du 2e point
 *  <Y2>      : Coordonne Y du 2e point
 *  <Color>   : Couleur du texte
 *  <Fill>    : Remplissage
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void glPostscriptRectangle(Tcl_Interp *Interp,Tk_Canvas Canvas,int X1,int Y1,int X2,int Y2,XColor *Color,int Fill){

   char buf[256];

   if (Color)
      Tk_CanvasPsColor(Interp,Canvas,Color);

   sprintf(buf,"%i %i moveto %i %i lineto %i %i lineto %i %i lineto closepath",X1,Y1,X2,Y1,X2,Y2,X1,Y2);
   if (Fill) {
      Tcl_AppendResult(Interp,buf," fill\n",(char*)NULL);
   } else {
      Tcl_AppendResult(Interp,buf," stroke\n",(char*)NULL);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <glPostscriptText>
 * Creation : Aout 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Generer du postscript pour une ligne de texte.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Canvas>  : Canvas Tk
 *  <Text>    : Texte a imprimer
 *  <X1>      : Coordonne X du point
 *  <Y1>      : Coordonne Y du point
 *  <Angle>   : Angle de rotation
 *  <Color>   : Couleur du texte
 *  <XOff>    : 0 pour nw/w/sw anchor , -0.5 pour n/center/s, -1.0 pour ne/e/se
 *  <YOff>    : 0 pour nw/n/ne anchor, +0.5 pour w/center/e, +1.0 pour sw/s/se
 *  <Justify> : 0 pour left, 0.5 pour center, 1 pour right
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void glPostscriptText(Tcl_Interp *Interp,Tk_Canvas Canvas,char* Text,int X1,int Y1,int Angle,XColor *Color,float XOff,float YOff,float Justify){

   char txt[256];

   if (Color)
      Tk_CanvasPsColor(Interp,Canvas,Color);

   sprintf(txt,"gsave %i %i %i [[(",Angle,X1,Y1);
   Tcl_AppendResult(Interp,txt,(char*)NULL);
   Tcl_AppendResult(Interp,Text,(char*)NULL);
   sprintf(txt,")]] 18 %1.1f %1.1f %1.1f false DrawText grestore\n",XOff,YOff,Justify);
   Tcl_AppendResult(Interp,txt,(char*)NULL);
}

void glPostscriptTextBG(Tcl_Interp *Interp,Tk_Canvas Canvas,int X,int Y,int Theta,int Width,int Height,int DeltaX,int DeltaY,XColor *Color,int Clip){

   char buf[256];

   sprintf(buf,"%i %i translate %i rotate ",X,Y,Theta);
   Tcl_AppendResult(Interp,buf,(char*)NULL);
   sprintf(buf,"%i %i moveto %i %i lineto ",-DeltaX,-DeltaY,Width+DeltaX,-DeltaY);
   Tcl_AppendResult(Interp,buf,(char*)NULL);
   sprintf(buf,"%i %i lineto %i %i lineto ",Width+DeltaX,Height+DeltaY,-DeltaX,Height+DeltaY);
   Tcl_AppendResult(Interp,buf,(char*)NULL);
   sprintf(buf,"%i rotate %i %i translate ",-Theta,-X,-Y);
   Tcl_AppendResult(Interp,buf,(char*)NULL);

   if (Clip) {
      sprintf(buf,"closepath\n");
      Tcl_AppendResult(Interp,buf,(char*)NULL);
   } else {
      Tk_CanvasPsColor(Interp,Canvas,Color);
      Tcl_AppendResult(Interp,"closepath fill\n",(char*)NULL);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <glStencilMaskQuad>
 * Creation : Aout 2008 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Appliquer une region rectangulaire dans le stencil buffer.
 *
 * Parametres :
 *  <X>       : Coordonnee X
 *  <Y>       : Coordonnee Y
 *  <Width>   : Largeur
 *  <Height>  : Hauteur
 *  <Theta>   : Angle
 *  <DX>      : Buffer en X
 *  <DY>      : Buffer en Y
 *
 * Retour:
 *  <Exist>   : Trouve ou non
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void glStencilMaskQuad(int X,int Y,int Width,int Height,int Theta,int DX,int DY) {

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();

   glTranslatef(X,Y,0.0);
   glRotatef(Theta,0.0,0.0,1.0);

   glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
   glBegin(GL_QUADS);
      glVertex2i(-DX,-DY);
      glVertex2i(Width+DX,-DY);
      glVertex2i(Width+DX,Height+DY);
      glVertex2i(-DX,Height+DY);
   glEnd();
   glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);

   glPopMatrix();
}

/*----------------------------------------------------------------------------
 * Nom      : <glTextureFit>
 * Creation : Juin 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Calculer le facteur 2 le plus pres pour une dimension de texture.
 *
 * Parametres :
 *  <Size>    : Dimension
 *  <Delta>   : Ratio de tolerance
 *
 * Retour:
 *  <Size>    : Dimension optimale (0=invalide)
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int glTextureFit(int Size,float Tolerance) {

   GLint min,max;

   /*Initialiser le max au maximum permis par l'implementation*/
   glGetIntegerv(GL_MAX_TEXTURE_SIZE,&max);
   min=max>>1;

   while (max) {
      if (Size>min) {
         /* Nous avons trouve notre interval.*/
         if ((float)(Size-min)/(max-min)<=Tolerance)
            return(min);
         else
            return(max);
      } else {
         max=min;
         min=max>>1;
      }
   }
   return(0);
}

/*----------------------------------------------------------------------------
 * Nom      : <glXShutDown>
 * Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Cette procedure effectue le cleanup des structures.
 *
 * Parametres  :
 *  <Interp>   : Interpreteur Tcl/Tk
 *
 * Retour      :
 *  <TCL...>   : Code de retour TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void glXShutDown() {

   if (GLRender->GLTess) {
      gluDeleteTess(GLRender->GLTess);
   }

   if (GLRender->GLQuad) {
      gluDeleteQuadric(GLRender->GLQuad);
   }

   if (GLRender->GLCon) {
     glXDestroyContext(GLRender->XDisplay,GLRender->GLCon);
   }

  if (GLRender->GLVis) {
      XFree(GLRender->GLVis);
   }

   if (GLRender->GLFeed) free(GLRender->GLFeed);
   free(GLRender);
}

/*----------------------------------------------------------------------------
 * Nom      : <glDefineParams>
 * Creation : Aout 2002 - J.P. Gauthier & David Dube
 *
 * But      : Definir les parametres de rendu et d'etat d'OpenGL.
 *
 * Parametres :
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/

int glDefineParams(){

   int   n;

   /*Si on n'a pas de contexte actif*/
   if (!GLRender->GLCon)
      return(TCL_OK);

   /*Buffer parameters*/
   glPixelStorei(GL_UNPACK_ALIGNMENT,1);
   glPixelStorei(GL_PACK_ALIGNMENT,1);
   glEnable(GL_NORMALIZE);
   glDisable(GL_CULL_FACE);
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_STENCIL_TEST);
   glClearDepth(1000.0);
   glClearStencil(0x0);
   glStencilMask(0xFF);
   glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

   /*Rendering parameters*/
   if (GLRender->GLDither) {
      glEnable(GL_DITHER);
   } else {
      glDisable(GL_DITHER);
   }
   if (GLRender->GLAlias) {
      glEnable(GL_LINE_SMOOTH);
      glEnable(GL_POINT_SMOOTH);
      glEnable(GL_POLYGON_SMOOTH);
   } else {
      glDisable(GL_LINE_SMOOTH);
      glDisable(GL_POINT_SMOOTH);
      glDisable(GL_POLYGON_SMOOTH);
   }

   if (GLRender->GLFSAA && GLRender->Ext[ARB_multisample]) {
      glEnable(GL_MULTISAMPLE_ARB);
   } else {
      glDisable(GL_MULTISAMPLE_ARB);
   }

   if (GLRender->GLShade && GLRender->XMode==GLX_RGBA) {
      glShadeModel(GL_SMOOTH);
   } else {
      glShadeModel(GL_FLAT);
   }

   /*Texturing parameters*/
   if (GLRender->GLFilter) {
      glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
      glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
   } else {
      glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
      glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
   }

   glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);
   glHint(GL_POLYGON_SMOOTH_HINT,GL_NICEST);

   /*Parse needed extensions*/
   if (GLRender->Set) {
      n=0;
      while(glExtString[n]) {
         GLRender->Ext[n]=glCheckExtension(glExtString[n]);
//         GLRender->Ext[n]=GL_FALSE;
         n++;
      }

      memset(GLRender->Prog,0x0,GL_MAXSHADER*sizeof(GLhandleARB));
      if (GLRender->ShaderAvailable && GLRender->ShaderPath && GLRender->Shaders) {
         for (n=0;n<GLRender->ShaderNb;n++){     
            if (!(GLRender->Prog[n]=GLShader_Load(GLRender->ShaderPath,GLRender->Shaders[n]))) {
               fprintf(stderr,"(WARNING) glDefineParams: Unable to initialize shader program \"%s\"\n",GLRender->Shaders[n]);
               GLRender->ShaderAvailable=0;
            }
         }
      }
      GLRender->Set=0;
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <glInit>
 * Creation : Aout 2002 - J.P. Gauthier
 *
 * But      : Initialise la structure GLRender
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void glInit(Tcl_Interp *Interp) {

   Tcl_CreateObjCommand(Interp,"glrender",glRender_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);

   GLRender = (GLParams*)malloc(sizeof(GLParams));

   /*Initialisation des parametres*/

   GLRender->TkWin           = NULL;
   GLRender->TkFont          = NULL;
   GLRender->XColormap       = 0;
   GLRender->XDisplay        = NULL;
   GLRender->XScreen         = NULL;
   GLRender->XScreenNo       = 0;
   GLRender->XBatch          = 0;
   GLRender->XPix            = None;
   GLRender->XMode           = GLX_RGBA;
   GLRender->XExpose         = 0;
   GLRender->GLDebug         = 0;
   GLRender->GLCon           = NULL;
   GLRender->GLPCon          = NULL;
   GLRender->GLVis           = NULL;
   GLRender->GLAlias         = 0;
   GLRender->GLFSAA          = 1;
   GLRender->GLDirect        = True;
   GLRender->GLDither        = 0;
   GLRender->GLShade         = 1;
   GLRender->GLFilter        = 1;
   GLRender->GLZBuf          = 0;
   GLRender->GLFeed          = NULL;
   GLRender->GLFeedSize      = 0;
   GLRender->GLPBuf          = None;
   GLRender->GLPix           = None;
   GLRender->RenderTime      = 0.0;
   GLRender->Resolution      = 1;
   GLRender->ShaderAvailable = 1;
   GLRender->GLTess          = gluNewTess();
   GLRender->GLQuad          = gluNewQuadric();
   GLRender->TRCon           = NULL;
   GLRender->Set             = 1;
   GLRender->Soft            = 0;
   GLRender->UseThreads      = 1;
   GLRender->Wait            = False;
   GLRender->Delay           = GL_STOP;
   GLRender->DelayProc       = NULL;
   GLRender->Shaders         = NULL;
   GLRender->ShaderPath      = NULL;
   GLRender->ShaderNb        = 0;
   GLRender->GLLighting      = NULL;

   memset(GLRender->Ext,0x0,sizeof(GLboolean));

   /*Magnifying paameters*/
   GLRender->MagScale=1;
   GLRender->MagX=GLRender->MagY=GLRender->MagD=0;

   glTessInit();
}

/*----------------------------------------------------------------------------
 * Nom      : <glXCanvasInit>
 * Creation : Aout 2002 - J.P. Gauthier & David Dube
 *
 * But      : Initialise le canvas OpenGL pour GLX
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <TkWin>   : Identificateur de fenetre Tk
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
// #include <EGL/egl.h>
// int glXCanvasInitEGL(Tcl_Interp *Interp,Tk_Window TkWin) {
// 
//    int        *attrTrue;
//    const char *vendor;
// 
//    int attrHard[]={ GLX_RENDER_TYPE,GLX_RGBA_BIT, GLX_DOUBLEBUFFER,1, GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT | GLX_PBUFFER_BIT,
//               GLX_RED_SIZE,1, GLX_GREEN_SIZE,1, GLX_BLUE_SIZE,1, GLX_ALPHA_SIZE,1,
//               GLX_DEPTH_SIZE,1, GLX_STENCIL_SIZE,1, GLX_SAMPLE_BUFFERS_ARB,False, GLX_SAMPLES_ARB,0, None };
// 
//    int attrSoft[]={ GLX_RENDER_TYPE,GLX_RGBA_BIT, GLX_DOUBLEBUFFER,1, GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT | GLX_PBUFFER_BIT,
//               GLX_RED_SIZE,1, GLX_GREEN_SIZE,1, GLX_BLUE_SIZE,1, GLX_ALPHA_SIZE,1,
//               GLX_DEPTH_SIZE,1, GLX_STENCIL_SIZE,1, None };
// 
//    int attrMin[]={ GLX_RENDER_TYPE,GLX_RGBA_BIT, GLX_DOUBLEBUFFER,1, GLX_DRAWABLE_TYPE,GLX_WINDOW_BIT,None };
//    
//    // Setup some useful pointers
//    GLRender->TkWin    = TkWin;                    /* Tk window */
//    GLRender->XDisplay = Tk_Display(TkWin);        /* Get the XDisplay of the main window */
//    GLRender->XScreen  = Tk_Screen(TkWin);         /* Get the XID of the application */
//    GLRender->XScreenNo= Tk_ScreenNumber(TkWin);   /* Get the screen number */
// 
//    EGLDisplay egl_dpy;
//    EGLint     major,minor;
//    EGLint     GLConfigNb;
//    EGLConfig  eglCfg;
//    EGLSurface egl_sfc;
//    EGLContext egl_ctx;
//    
//    // Get display and initialize
//    egl_dpy=eglGetDisplay(EGL_DEFAULT_DISPLAY);
//    fprintf(stderr,"------ display = %p\n",egl_dpy);
//    if (!eglInitialize(egl_dpy,&major,&minor)) {
//       Tcl_AppendResult(Interp,"glXCanvasInit: Could not find EGL interface",(char*)NULL);
//       return(TCL_ERROR);      
//    }
//    
//    fprintf(stderr,"------ version = %i.%i\n",major,minor);
// 
//    // Check which implementation we are using
//    vendor=eglQueryString(egl_dpy,EGL_VENDOR);
//    fprintf(stderr,"------ vendor = %s\n",vendor);
//    if (strstr(vendor,"NVIDIA")) {
//       GLRender->Vendor=NVIDIA;
//       GLRender->Soft=0;
//    } else if (strstr(vendor,"ATI")) {
//       GLRender->Vendor=ATI;
//       GLRender->Soft=0;
//    } else if (strstr(vendor,"VirtualGL")) {
//       GLRender->Vendor=VIRTUALGL;
//       GLRender->Soft=0;
//    } else {
//       GLRender->Vendor=MESA;
//       GLRender->Soft=1;
//    }
// 
//    // Check for full scene anti-aliasing (MESA breaks the stencil buffer when enabling FSAA)
//    if (GLRender->Soft) {
//       attrTrue=attrSoft;
//    } else {
//       attrTrue=attrHard;
//       if (GLRender->Ext[ARB_multisample]) {
//          attrTrue[19]=True;
//          attrTrue[21]=GLRender->GLFSAA;
//       }
//    }
// 
//    if (!GLRender->GLCon) {
//       
//       static const EGLint configAttribs[] = {
//                EGL_SURFACE_TYPE, EGL_PBUFFER_BIT,
//                EGL_BLUE_SIZE, 8,
//                EGL_GREEN_SIZE, 8,
//                EGL_RED_SIZE, 8,
//                EGL_ALPHA_SIZE, 8,
//                EGL_DEPTH_SIZE, 8,
//                EGL_STENCIL_SIZE, 8,
//                EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT,
//                EGL_NONE
//       };    
// 
// //      static const int pbufferWidth = 9;
// //      static const int pbufferHeight = 9;
// 
// //      static const EGLint pbufferAttribs[] = {
// //            EGL_WIDTH, pbufferWidth,
// //            EGL_HEIGHT, pbufferHeight,
// //            EGL_NONE,
// //      };
// 
// 
//       // Select an appropriate configuration
//       if ((eglChooseConfig(egl_dpy,configAttribs,&eglCfg,1,&GLConfigNb)==EGL_FALSE) || !GLConfigNb) {
//          fprintf(stderr,"(WARNING) eglChooseConfig: Unable to select a standard configuration, trying for minimal\n");
// //         GLRender->GLConfig=glXChooseFBConfig(GLRender->XDisplay,GLRender->XScreenNo,attrMin,&GLRender->GLConfigNb);
// //         if (!GLRender->GLConfigNb) {
// //            Tcl_AppendResult(Interp,"glXCanvasInit: Unable to select a minimal configuration",(char*)NULL);
// //            return(TCL_ERROR);
// //         }
//       }
// 
//       // Create surface
//       egl_sfc=eglCreateWindowSurface(egl_dpy,eglCfg,Tk_WindowId(TkWin),NULL);
//       if (egl_sfc==EGL_NO_SURFACE) {
//          Tcl_AppendResult(Interp,"glXCanvasInit: Could not create EGL window surface",(char*)NULL);
//          return(TCL_ERROR);      
//       }
//    
//    // 3. Create a surface
// //      EGLSurface eglSurf = eglCreatePbufferSurface(egl_dpy, eglCfg,pbufferAttribs);
//       // Bind the API
//       eglBindAPI(EGL_OPENGL_API);
// 
//       // Create a context and make it current
//       egl_ctx=eglCreateContext(egl_dpy, eglCfg, EGL_NO_CONTEXT, NULL);
//       if (egl_ctx==EGL_NO_CONTEXT) {
//          Tcl_AppendResult(Interp,"glXCanvasInit: Could not create EGL context",(char*)NULL);
//          return(TCL_ERROR);      
//       }
//       
//       eglMakeCurrent(egl_dpy,egl_sfc,egl_sfc,egl_ctx);
//  GLRender->ShaderAvailable=1;
//  
// //      if ((GLRender->GLDirect=glXIsDirect(GLRender->XDisplay,GLRender->GLCon))) {
// //         GLRender->ShaderAvailable=1;
// //      } else {
// //         GLRender->ShaderAvailable=0;
// //      }
//       if (GLRender->Soft) {
//          GLRender->ShaderAvailable=0;
//          GLRender->GLDirect=False;
//       }
// 
//       /*Generer les primitives de base*/
//       glGenCircle(0,360);
//    }
// 
//    return(TCL_OK);
// }


int glXCanvasInit(Tcl_Interp *Interp,Tk_Window TkWin) {

   int         glmin,glmaj,gl;
   int        *attrTrue;
   const char *vendor;

   int attrHard[]={ GLX_RENDER_TYPE,GLX_RGBA_BIT, GLX_DOUBLEBUFFER,1, GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT | GLX_PBUFFER_BIT,
              GLX_RED_SIZE,1, GLX_GREEN_SIZE,1, GLX_BLUE_SIZE,1, GLX_ALPHA_SIZE,1,
              GLX_DEPTH_SIZE,1, GLX_STENCIL_SIZE,1, GLX_SAMPLE_BUFFERS_ARB,False, GLX_SAMPLES_ARB,0, None };

   int attrSoft[]={ GLX_RENDER_TYPE,GLX_RGBA_BIT, GLX_DOUBLEBUFFER,1, GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT | GLX_PBUFFER_BIT,
              GLX_RED_SIZE,1, GLX_GREEN_SIZE,1, GLX_BLUE_SIZE,1, GLX_ALPHA_SIZE,1,
              GLX_DEPTH_SIZE,1, GLX_STENCIL_SIZE,1, None };

   int attrMin[]={ GLX_RENDER_TYPE,GLX_RGBA_BIT, GLX_DOUBLEBUFFER,1, GLX_DRAWABLE_TYPE,GLX_WINDOW_BIT,None };


   /*Setup some useful pointers*/
   GLRender->TkWin    = TkWin;                    /* Tk window */
   GLRender->XDisplay = Tk_Display(TkWin);        /* Get the XDisplay of the main window */
   GLRender->XScreen  = Tk_Screen(TkWin);         /* Get the XID of the application */
   GLRender->XScreenNo= Tk_ScreenNumber(TkWin);   /* Get the screen number */

   /*Check which implementation we are using*/
   vendor=glXGetClientString(GLRender->XDisplay,GLX_VENDOR);
   if (strstr(vendor,"NVIDIA")) {
      GLRender->Vendor=NVIDIA;
      GLRender->Soft=0;
   } else if (strstr(vendor,"ATI")) {
      GLRender->Vendor=ATI;
      GLRender->Soft=0;
   } else if (strstr(vendor,"VirtualGL")) {
      GLRender->Vendor=VIRTUALGL;
      GLRender->Soft=0;
   } else {
      GLRender->Vendor=MESA;
      GLRender->Soft=1;
   }

   /*Check for full scene anti-aliasing (MESA breaks the stencil buffer when enabling FSAA)*/
   if (GLRender->Soft) {
      attrTrue=attrSoft;
   } else {
      attrTrue=attrHard;
      if (GLRender->Ext[ARB_multisample]) {
         attrTrue[19]=True;
         attrTrue[21]=GLRender->GLFSAA;
      }
   }

   /* GLX Version */
   gl=glXQueryVersion(GLRender->XDisplay,&glmaj,&glmin);
   if (!gl) {
      Tcl_AppendResult(Interp,"glXCanvasInit: Could not find GLX extensionsr",(char*)NULL);
      return(TCL_ERROR);
   }

   if (!GLRender->GLCon) {
      GLRender->GLConfig=glXChooseFBConfig(GLRender->XDisplay,GLRender->XScreenNo,attrTrue,&GLRender->GLConfigNb);
      if (!GLRender->GLConfigNb) {
         fprintf(stderr,"(WARNING) glXCanvasInit: Unable to select a standard configuration, trying for minimal\n");
         GLRender->GLConfig=glXChooseFBConfig(GLRender->XDisplay,GLRender->XScreenNo,attrMin,&GLRender->GLConfigNb);
         if (!GLRender->GLConfigNb) {
            Tcl_AppendResult(Interp,"glXCanvasInit: Unable to select a minimal configuration",(char*)NULL);
            return(TCL_ERROR);
         }
      }

      GLRender->GLCon=glXCreateNewContext(GLRender->XDisplay,GLRender->GLConfig[0],GLX_RGBA_TYPE,NULL,GLRender->GLDirect);
      GLRender->GLVis=glXGetVisualFromFBConfig(GLRender->XDisplay,GLRender->GLConfig[0]);
      if (!GLRender->GLVis) {
         Tcl_AppendResult(Interp,"glXCanvasInit: Could not get a valid visual",(char*)NULL);
         return(TCL_ERROR);
      }

      if ((GLRender->GLDirect=glXIsDirect(GLRender->XDisplay,GLRender->GLCon))) {
         GLRender->ShaderAvailable=1;
      } else {
         GLRender->ShaderAvailable=0;
      }
      if (GLRender->Soft) {
         GLRender->ShaderAvailable=0;
         GLRender->GLDirect=False;
      }

      GLRender->XColormap=XCreateColormap(GLRender->XDisplay,RootWindowOfScreen(GLRender->XScreen),GLRender->GLVis->visual,AllocNone);

      /*Generer les primitives de base*/
      glGenCircle(0,360);
   }

   Tk_SetWindowVisual(TkWin,GLRender->GLVis->visual,GLRender->GLVis->depth,GLRender->XColormap);
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <glXGetPBuffer>
 * Creation : Novembre 2003 - J.P. Gauthier
 *
 * But      : Initialise un pBuffer GLX pour le rendue offscreen
 *
 * Parametres :
 *  <TkWin>   : Identificateur de fenetre Tk
 *  <Width>   : Largeur
 *  <Height>  : Hauteur
 *
 * Retour:
 *   <GLXPbuffer> : PBuffer handle
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
GLXPbuffer glXGetPBuffer(Tk_Window TkWin,unsigned int *Width,unsigned int *Height) {

   unsigned int n;
   GLXPbuffer   pbuf=None;

   int pattr[]={ GLX_PBUFFER_WIDTH,0, GLX_PBUFFER_HEIGHT,0, GLX_LARGEST_PBUFFER,True, GLX_PRESERVED_CONTENTS,True, None };

   /*Max out at 2048x2048 cause the theyre seems to be problems with the returned size if too big*/
   pattr[1]=(*Width>2048?2048:*Width);
   pattr[3]=(*Height>2048?2048:*Height);

   for (n=0;n<GLRender->GLConfigNb;n++) {
      if ((pbuf=glXCreatePbuffer(GLRender->XDisplay,GLRender->GLConfig[n],pattr)))
         break;
   }

   /*Check returned Width and Height to make sure they're as requested*/
   if (pbuf) {
      /*This call blows up for driver different from NVIDIAs*/
      if (GLRender->Vendor==NVIDIA) {
         glXQueryDrawable(GLRender->XDisplay,pbuf,GLX_WIDTH,Width);
         glXQueryDrawable(GLRender->XDisplay,pbuf,GLX_HEIGHT,Height);
      } else {
         /*In soft mode, assume that everything is ok since with mesa 6.4 and higher it returns 0 for Width and Height*/
         *Width=pattr[1];
         *Height=pattr[3];
      }
   }

   if (!pbuf || *Width==0 || *Height==0) {
      fprintf(stderr,"(ERROR) glXGetPBuffer: Unable to allocate PBuffer\n");
      return(0);
   }
   if (pattr[1]!=*Width || pattr[3]!=*Height) {
      fprintf(stderr,"(WARNING) glXGetPBuffer: Maximum size available is %i x %i instead of requested is %i x %i\n",*Width,*Height,pattr[1],pattr[3]);
   }

   return(pbuf);
}

/*----------------------------------------------------------------------------
 * Nom      : <glXFreePBuffer>
 * Creation : Novembre 2003 - J.P. Gauthier
 *
 * But      : Liberer le pBuffer
 *
 * Parametres :
 *   <PBuf>   : PBuffer handle
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int glXFreePBuffer(GLXPbuffer PBuf) {

   glXMakeContextCurrent(GLRender->XDisplay,None,None,NULL);

   if (PBuf) {
      glXDestroyPbuffer(GLRender->XDisplay,PBuf);
   }
   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <glXGetPixmap>
 * Creation : Novembre 2003 - J.P. Gauthier
 *
 * But      : Initialise un pixmap GLX pour le rendue offscreen
 *
 * Parametres :
 *  <TkWin>   : Identificateur de fenetre Tk
 *  <Width>   : Largeur
 *  <Height>  : Hauteur
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int glXGetPixmap(Tk_Window TkWin,unsigned int *Width,unsigned int *Height) {

   int n=0;
   GLXFBConfig *config;

   int attr[]={ GLX_RENDER_TYPE,GLX_RGBA_BIT, GLX_DOUBLEBUFFER,1, GLX_DRAWABLE_TYPE,GLX_PIXMAP_BIT,
              GLX_RED_SIZE,1, GLX_GREEN_SIZE,1, GLX_BLUE_SIZE,1, GLX_ALPHA_SIZE,1,
              GLX_DEPTH_SIZE,1, GLX_STENCIL_SIZE,1, None };

   config=glXChooseFBConfig(GLRender->XDisplay,GLRender->XScreenNo,attr,&n);
   if (!n) {
      fprintf(stderr,"(ERROR) glXGetPixmap: Unable to select a configuration\n");
      return(0);
   }

   GLRender->GLPCon=glXCreateNewContext(GLRender->XDisplay,config[0],GLX_RGBA_TYPE,NULL,False);
   GLRender->XPix=Tk_GetPixmap(GLRender->XDisplay,Tk_WindowId(TkWin),*Width,*Height,Tk_Depth(TkWin));
   GLRender->GLPix=glXCreatePixmap(GLRender->XDisplay,config[0],GLRender->XPix,NULL);
   glXQueryDrawable(GLRender->XDisplay,GLRender->GLPBuf,GLX_WIDTH,Width);
   glXQueryDrawable(GLRender->XDisplay,GLRender->GLPBuf,GLX_HEIGHT,Height);

   if (!GLRender->GLPix) {
      fprintf(stderr,"(ERROR) glXGetPixmap: Unable to allocate GLPixmap\n");
      return(0);
   }

   if (!glXMakeContextCurrent(GLRender->XDisplay,GLRender->GLPix,GLRender->GLPix,GLRender->GLPCon)) {
      fprintf(stderr,"(ERROR) glXGetPixmap: Unable to link the pixmap to the GLXContext\n");
      glXFreePixmap();
      return(0);
   }

   free(config);
   glDefineParams();
   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <glFreePixmap>
 * Creation : Novembre 2003 - J.P. Gauthier
 *
 * But      : Liberer le pixmap GLX
 *
 * Parametres :
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int glXFreePixmap() {

   glXMakeContextCurrent(GLRender->XDisplay,None,None,NULL);

   if (GLRender->GLPCon) {
      glXDestroyContext(GLRender->XDisplay,GLRender->GLPCon);
      GLRender->GLPCon=NULL;
   }

   if (GLRender->GLPix) {
      glXDestroyGLXPixmap(GLRender->XDisplay,GLRender->GLPix);
      Tk_FreePixmap(GLRender->XDisplay,GLRender->XPix);
      GLRender->GLPix=None;
      GLRender->XPix=None;
   }
   return(1);
}
