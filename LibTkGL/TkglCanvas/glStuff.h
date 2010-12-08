/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Conversion du canvas Tk en Canvas GL.
 * Fichier   : glStuff.h
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

#ifndef _GLSTUFF_H
#define _GLSTUFF_H

#include <tk.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#define GL_GLEXT_PROTOTYPES
#include <GL/glx.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <math.h>
#include <malloc.h>
#include <time.h>
#include <tr.h>

#ifndef M_PI
#define M_PI        3.14159265358979323846   /*Pi*/
#endif

#define BITSWAP(b)  (b=(b<<7)|((b<<5)&0x40)|((b<<3)&0x20)|((b<<1)&0x10) | (b>>7)|((b>>5)&0x02)|((b>>3)&0x04)|((b>>1)&0x08))
#define BITWIDTH(w) ((w+7)/8)

#define ARCSTEP   64

#define CR_MAX    256    /*Nombre de colorcell a allouer en RGBA*/
#define CI_MAX    228    /*Nombre de colorcell a allouer en COLORINDEX*/
#define CI_BASE   40     /*Nombre de colorcell dedie au couleurs de base en COLORINDEX*/

#define GL_ALL    0
#define GL_VECTOR 1
#define GL_RASTER 2
#define GL_MASK   4

#define SHADER_MAX 32

#define glErrorCheck(MSG,EXIT) {\
   GLenum err;\
   if ((err=glGetError())!=GL_NO_ERROR) {\
      fprintf(stderr,"(GLERROR) %s: %s\n",MSG,gluErrorString(err));\
      if (EXIT) {\
         exit(1);\
      }\
   }\
}

#define glPrint(Interp,Canvas,Buf,X,Y,A) {\
   if (Interp) {\
      glPostscriptText(Interp,Canvas,Buf,X+1,Y,-A,NULL,0,1.0,0.0);\
   } else {\
      glDrawString(X,Y,A,Buf,strlen(Buf),0,0);\
   }\
}

#define glPrintFlip(Interp,Canvas,Buf,X,Y,A) {\
   if (Interp) {\
      glPostscriptText(Interp,Canvas,Buf,X+1,Y,-A,NULL,0,1.0,0.0);\
   } else {\
      glDrawString(X,Y,A,Buf,strlen(Buf),0,1);\
   }\
}

enum ExtToken { ARB_multisample, ARB_texture_compression, ARB_vertex_buffer_object };
enum ProgToken { PROG_FIELD=0, PROG_FIELDTEX=1, PROG_DATATEX=2, PROG_TOPOTEX=3 };
enum GLVendor { NVIDIA=0, ATI=1, MESA=2 };

/* Structure pour OpenGL */
typedef struct GLParams  {

   int    Resolution;           /*Qualite de l'affichage*/
   double RenderTime;           /*Temps en seconds pour effectuer le rendue du viewport*/
   double MemRes;               /*Memoire residente utilisee (Mb)*/
   int    ShaderAvailable;      /*Est-ce que l'implementation est capable de Shader*/

   /*Parametres Tk*/
   Tk_Window    TkWin;          /*Current Tk window*/
   Tk_Font      TkFont;         /*Current Tk Font*/

   /*Parametres X*/
   Colormap      XColormap;     /*Window colormap*/
   int           XMode;         /*Mode de rendue (Color index ou RGB)*/
   int           XBatch;        /*Mode de rendue en batch*/
   Pixmap        XPix;          /*Pixmap X*/
   Display      *XDisplay;
   Screen       *XScreen;
   int           XScreenNo;
   int           XExpose;

   /*Parametres OpenGL*/
   TRcontext    *TRCon;         /*Contexte du Tile Renderer*/

#ifdef WIN32
   HGLRC        GLCon;          /*Contexte OpenGL*/
   int         *GLConfig;       /*Liste de configuration*/
#else
   GLXContext   GLCon;          /*Contexte OpenGL*/
   GLXContext   GLPCon;         /*Contexte OpenGL Pixmap/Pbuffer*/
   GLXFBConfig *GLConfig;       /*Liste de configuration*/
   int          GLConfigNb;     /*Nombre de configurations*/
   GLXPbuffer   GLPBuf;         /*Pbuffer GL*/
   GLXPixmap    GLPix;          /*Pixmap GL*/
#endif

   GLfloat      *GLFeed;        /*Buffer pour le mode feedback*/
   unsigned long GLFeedSize;    /*Dimension du buffer pour feedback*/
   GLuint        GLPick[512];   /*Buffer pour le mode selection*/

   XVisualInfo *GLVis;          /*Visuel utilise*/

   int GLDebug;                 /*Mode Debug*/
   int GLDirect;                /*Direct rendering*/
   int GLAlias;                 /*Anti Aliasing*/
   int GLDither;                /*Dithering*/
   int GLShade;                 /*Shading*/
   int GLFilter;                /*Texture filtering*/
   int GLZBuf;                  /*Utilisation du ZBuffer*/
   int GLFSAA;                  /*Utilisation du Full Scene Anti-Aliasing*/
   double GLLight;              /*Lightning - Positionnement du soleil (Date YYYYMMDDHHMMSS ou -1 pour courant)*/
   GLUtesselator *GLTess;       /*Tessalation object */
   GLUquadric    *GLQuad;       /*Quadric object*/

   char          *Soft;             /*Type de Renderer*/
   int            Delay;            /*Delai des frames*/
   GLboolean      Ext[32];          /*Liste des extensions*/
   GLboolean      Set;              /*Liste des extensions*/
   GLhandleARB    Prog[SHADER_MAX]; /*Liste des shaders*/
   int            Vendor;           /*Graphic driver vendor*/

   double MagScale,MagX,MagY,MagD;   /*Magnifying paameters*/
} GLParams;

typedef struct T_glBitmap {
  unsigned char *Data;
  char          *Name;
  int            Width,Height;
  int            HotX,HotY;
} T_glBitmap;

typedef struct T_glFont {
  int            fid;
  int            nchar;
  GLuint        *tex;
  GLuint         list;
} T_glFont;

extern GLParams *GLRender;  /* Structure globale des parametres OpenGL */

void  DataFlip(unsigned char *DataIn,unsigned char *DataOut,int Width,int Height,int Size);
int   DashConvert(char *l,CONST char *p,int n,double width);
void  DashPrint(char *String,Tk_Dash *Dash);
void  glHashTableInit();
void  glInit(Tcl_Interp *Interp);
void  glFontInit();
void  glDrawString(int X,int Y,int Theta,char *String,int Len,int UTF,int Flip);
void  glDrawStringBG(int X,int Y,int Theta,int Width,int Height,int DeltaX,int DeltaY);

void       glXFontFree(T_glFont *glfont);
void       glXFontTexture(Font font,int first,int count,int listbase,int *tex);
int        glXCanvasInit(Tcl_Interp *Interp,Tk_Window TkWin);
void       glXShutDown();
int        glXGetPixmap(Tk_Window TkWin,int *Width,int *Height);
GLXPbuffer glXGetPBuffer(Tk_Window TkWin,int *Width,int *Height);
int        glXFreePBuffer(GLXPbuffer PBuf);
int        glXFreePixmap();

int      glDefineParams();
void     glBitmapFree(T_glBitmap *Bitmap);
int      glBitmapParseProc(ClientData Data,Tcl_Interp *Interp,Tk_Window TkWin,char *Value,char *WidgRec,int Offset);
char    *glBitmapPrintProc(ClientData Data,Tk_Window TkWin,char *WidgRec,int Offset,Tcl_FreeProc **FreeProcPtr);
int      glBuffer(Tcl_Interp *Interp,char* Img,int Buffer,int X0,int Y0,int W,int H,int Height);
GLfloat *glFeedbackInit(int Size,int GLMode);
int      glFeedbackProcess(Tcl_Interp *Interp,int GLMode);
int      glPostscriptBitmap(Tcl_Interp *Interp,T_glBitmap *Bitmap,int Width,int Height);
void     glPostscriptDash(Tcl_Interp *Interp,Tk_Dash *Dash,int Width);
int      glPostscriptStipple(Tcl_Interp *Interp,T_glBitmap *Bitmap);
void     trPostscriptBuffer(Tcl_Interp *Interp,int Buffer,int X0,int Y0,int Width,int Height,TRcontext *TR);
void     glPostscriptBuffer(Tcl_Interp *Interp,int Buffer,int X0,int Y0,int Width,int Height);
void     glPostscriptRectangle(Tcl_Interp *Interp,Tk_Canvas Canvas,int X1,int Y1,int X2,int Y2,XColor *Color,int Fill);
void     glPostscriptText(Tcl_Interp *Interp,Tk_Canvas Canvas,char* Text,int X1,int Y1,int Angle,XColor *Color,float XOff,float YOff,float Justify);
void     glPostscriptTextBG(Tcl_Interp *Interp,Tk_Canvas Canvas,int X,int Y,int Theta,int Width,int Height,int DeltaX,int DeltaY,XColor *Color,int Clip);
int      glPostscripTextLayout(Tcl_Interp *Interp,Tk_Canvas canvas,Tk_TextLayout layout,XColor *color,T_glBitmap *stipple,int angle,int x,int y,Tk_Anchor anchor,Tk_Justify justify);
GLuint   glStencilMaskCheck(int X,int Y,int Width,int Height,int Ref);
void     glStencilMaskQuad(int X,int Y,int Width,int Height,int Theta,int DX,int DY);
int      glTextureFit(int Size,float Tolerance);
int      glFontUse(Display *Disp,Tk_Font FontId);
GLushort glDash(Tk_Dash *Dash);
void     glDrawArrow(GLint Mode);
void     glDrawCircle(int Inter,GLint Mode);
void     glDrawArc(float Start,float Extent,int Inter,GLint Mode,int Style);

void     glPickInit(double WinX,double WinY,double DX,double DY);
int      glPickProcess();

void      glTessError(GLenum Err);
GLdouble *glTessTmpGet();

int      trBuffer(Tcl_Interp *Interp,char* Img,int Buffer,int X,int Y,int Width,int Height,TRcontext *TR);
void     trRasterPos2i(int X,int Y);
#endif
