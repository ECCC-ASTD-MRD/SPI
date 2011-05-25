#include <string.h>
#include "glStuff.h"

/*----------------------------------------------------------------------------
 * Nom      : <glDrawString>
 * Creation : Mars 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Afficher une chaine de caracteres (textures) en OpenGL.
 *
 * Parametres :
 *   <X>      : Coordonnee en X
 *   <Y>      : Coordonnee en Y
 *   <Theta>  : Angle
 *   <String> : Chaine
 *   <Len>    : Longueur de la chaine
 *   <UTF>    : Est-ce une UTF ???
 *   <Flip>   : Est-ce que l'on inverse en Y
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void glDrawStringBG(int X,int Y,int Theta,int Width,int Height,int DeltaX,int DeltaY) {

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();

   glTranslatef(X,Y,0.0);
   glRotatef(Theta,0.0,0.0,1.0);

   glBegin(GL_QUADS);
      glVertex2i(-DeltaX,-DeltaY);
      glVertex2i(Width+DeltaX,-DeltaY);
      glVertex2i(Width+DeltaX,Height+DeltaY);
      glVertex2i(-DeltaX,Height+DeltaY);
   glEnd();

   glPopMatrix();
}

void glDrawString(int X,int Y,int Theta,char *String,int Len,int UTF,int Flip) {

   glPushAttrib(GL_LIST_BIT|GL_CURRENT_BIT|GL_ENABLE_BIT|GL_POLYGON_BIT|GL_TRANSFORM_BIT);

   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   glEnable(GL_BLEND);
   glEnable(GL_TEXTURE_2D);
   glDisable(GL_CULL_FACE);
   glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
   glEnable(GL_ALPHA_TEST);
   glAlphaFunc(GL_NOTEQUAL,0);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();

   glTranslatef(X,Y,0.0);
   glRotatef(Theta,0.0,0.0,1.0);

   if (!Flip)
      glScalef(1.0f,-1.0f,1.0);

   if (UTF) {
      glCallLists(Len/2,GL_UNSIGNED_SHORT,String);
   } else {
      glCallLists(Len,GL_UNSIGNED_BYTE,String);
   }
   glPopMatrix();
   glPopAttrib();
}

/*----------------------------------------------------------------------------
 * Nom      : <glXFontFill>
 * Creation : Mars 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Generer une texture OpenGL pour un caracteres.
 *
 * Parametres :
 *   <dpy>    : Display
 *   <win>    : Window
 *   <gc>     : Graphic context
 *   <width>  : Texture width
 *   <Height> : Texture height
 *   <x0>     : X char coordinate within texture
 *   <y0>     : Y char coordinate within texture
 *   <c>      : Char
 *   <bitmap> : Texture to render to
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static void glXFontFill(Display *dpy,Window win,Pixmap pix,GC gc,unsigned int width,unsigned int height,int x0,int y0,unsigned int c,GLubyte *bitmap) {

   XImage       *image=NULL;
   unsigned int  x,y;
   XChar2b       char2b;

   XSetForeground(dpy,gc,0);
   XFillRectangle(dpy,pix,gc,0,0,width,height);
   XSetForeground(dpy,gc,1);

   char2b.byte1 = (c >> 8) & 0xff;
   char2b.byte2 = (c & 0xff);

   XDrawString16(dpy,pix,gc,x0,y0,&char2b,1);
   image = XGetImage(dpy,pix,0,0,width,height,1,XYPixmap);

   if (image) {
      /* Fill the bitmap (X11 and OpenGL are upside down wrt each other).  */
      for (y=0;y<height;y++)
         for (x=0;x<width;x++)
            if (XGetPixel(image,x,y))
               bitmap[width*y+x]=0xff;
      XDestroyImage(image);
   }
}

static XCharStruct *isvalid(XFontStruct * fs, unsigned int which) {

   unsigned int rows, pages;
   unsigned int byte1 = 0, byte2 = 0;
   int i, valid = 1;

   rows = fs->max_byte1 - fs->min_byte1 + 1;
   pages = fs->max_char_or_byte2 - fs->min_char_or_byte2 + 1;

   if (rows == 1) {
      /* "linear" fonts */
      if ((fs->min_char_or_byte2 > which) || (fs->max_char_or_byte2 < which))
         valid = 0;
   } else {
      /* "matrix" fonts */
      byte2 = which & 0xff;
      byte1 = which >> 8;
      if ((fs->min_char_or_byte2 > byte2) ||
          (fs->max_char_or_byte2 < byte2) ||
          (fs->min_byte1 > byte1) || (fs->max_byte1 < byte1))
         valid = 0;
   }

   if (valid) {
      if (fs->per_char) {
         if (rows == 1) {
            /* "linear" fonts */
            return (fs->per_char + (which - fs->min_char_or_byte2));
         } else {
            /* "matrix" fonts */
            i = ((byte1 - fs->min_byte1) * pages) + (byte2 - fs->min_char_or_byte2);
            return (fs->per_char + i);
         }
      }  else {
         return (&fs->min_bounds);
      }
   }
   return (NULL);
}

/*----------------------------------------------------------------------------
 * Nom      : <glXFontFree>
 * Creation : Mars 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Liberer les resources GL associes a une police.
 *
 * Parametres   :
 *   <glfont>   : Structure glFont
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void glXFontFree(T_glFont *glfont) {

   glDeleteLists(glfont->list,glfont->nchar);
   glDeleteTextures(glfont->nchar,glfont->tex);
   free(glfont->tex);
}

/*----------------------------------------------------------------------------
 * Nom      : <glXFontTexture>
 * Creation : Mars 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Creer les displays liste et textures pour une police de caracteres.
 *
 * Parametres   :
 *   <Font>     : Police de caracteres
 *   <first>    : Debut de la display list
 *   <count>    : nombre de caracteres
 *   <listbase> : Texture width
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void glXFontTexture(Font font,int first,int count,int listbase,int *tex) {

   Display      *dpy;
   Window        win;
   Pixmap        pixmap;
   GC            gc;
   XGCValues     values;
   unsigned long valuemask;
   XFontStruct  *fs;
   GLint         swapbytes, lsbfirst, rowlength;
   GLint         skiprows, skippixels, alignment;
   unsigned int  max_bm_width, max_bm_height;
   GLubyte      *bm;
   int           i;

   dpy=glXGetCurrentDisplay();
   if (!dpy)
      return;   /* I guess glXMakeCurrent wasn't called */
   win = RootWindow(dpy,DefaultScreen(dpy));

   fs = XQueryFont(dpy,font);
   if (!fs) {
      fprintf(stderr,"(ERROR) glXFontTexture: Couldn't get font structure information");
      return;
   }

   /* Allocate a texture that can fit all characters.  */
   max_bm_width  = glTextureFit(fs->max_bounds.rbearing-fs->min_bounds.lbearing,0.0);
   max_bm_height = glTextureFit(fs->max_bounds.ascent+fs->max_bounds.descent,0.0);

   bm = (GLubyte*)malloc(max_bm_width*max_bm_height*sizeof(GLubyte));
   if (!bm) {
      XFreeFontInfo(NULL, fs, 1);
      fprintf(stderr,"(ERROR) glXFontTexture: Couldn't allocate bitmap in glXUseXFont()");
      return;
   }

   /* create X Resources */
   pixmap = XCreatePixmap(dpy,win,max_bm_width,max_bm_height,1);
   values.foreground = BlackPixel(dpy, DefaultScreen(dpy));
   values.background = WhitePixel(dpy, DefaultScreen(dpy));
   values.font = fs->fid;
   valuemask = GCForeground | GCBackground | GCFont;
   gc = XCreateGC(dpy, pixmap, valuemask, &values);

   /* Save the current packing mode for bitmaps.  */
   glGetIntegerv(GL_UNPACK_SWAP_BYTES,&swapbytes);
   glGetIntegerv(GL_UNPACK_LSB_FIRST,&lsbfirst);
   glGetIntegerv(GL_UNPACK_ROW_LENGTH,&rowlength);
   glGetIntegerv(GL_UNPACK_SKIP_ROWS,&skiprows);
   glGetIntegerv(GL_UNPACK_SKIP_PIXELS,&skippixels);
   glGetIntegerv(GL_UNPACK_ALIGNMENT,&alignment);

   /* Enforce a standard packing mode which is compatible with */
   glPixelStorei(GL_UNPACK_SWAP_BYTES,GL_FALSE);
   glPixelStorei(GL_UNPACK_LSB_FIRST,GL_FALSE);
   glPixelStorei(GL_UNPACK_ROW_LENGTH,0);
   glPixelStorei(GL_UNPACK_SKIP_ROWS,0);
   glPixelStorei(GL_UNPACK_SKIP_PIXELS,0);
   glPixelStorei(GL_UNPACK_ALIGNMENT,1);

   for (i = 0; i < count; i++) {
      unsigned int width, height;
      XCharStruct *ch;
      unsigned int c = first + i;
      int          list = listbase + i;
      int          valid;
      float        rx,ry;

      /* check on index validity and get the bounds */
      ch = isvalid(fs,c);
      if (!ch) {
         ch = &fs->max_bounds;
         valid = 0;
      } else {
         valid = 1;
      }

      width =ch->rbearing-ch->lbearing;
      height=ch->ascent+ch->descent;

      glNewList(list, GL_COMPILE);

      if (valid && width>0 && height>0) {

         memset(bm,'\0',max_bm_width*max_bm_height);
         glXFontFill(dpy,win,pixmap,gc,max_bm_width,max_bm_height,-ch->lbearing,ch->ascent,c,bm);
         glBindTexture(GL_TEXTURE_2D,tex[i]);
         glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
         glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
         glTexImage2D(GL_TEXTURE_2D,0,GL_ALPHA,max_bm_width,max_bm_height,0,GL_ALPHA,GL_UNSIGNED_BYTE,bm);
         glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);

         rx=(float)width/max_bm_width;
         ry=(float)height/max_bm_height;
         glTranslatef(ch->lbearing,-ch->ascent,0);
         glBegin(GL_QUADS);
         glTexCoord2f(0.0f,0.0f);glVertex2f(0.0f,0.0f);
         glTexCoord2f(0.0f,ry);glVertex2f(0.0f,height);
         glTexCoord2f(rx,ry);glVertex2f(width,height);
         glTexCoord2f(rx,0.0f);glVertex2f(width,0.0f);
         glEnd();
         glTranslatef(0,ch->ascent,0);
      }
      glTranslatef(ch->width-ch->lbearing,0,0);
      glEndList();
   }

   free(bm);
   XFreePixmap(dpy,pixmap);
   XFreeFontInfo(NULL,fs,1);

   /* Restore saved packing modes.  */
   glPixelStorei(GL_UNPACK_SWAP_BYTES,swapbytes);
   glPixelStorei(GL_UNPACK_LSB_FIRST,lsbfirst);
   glPixelStorei(GL_UNPACK_ROW_LENGTH,rowlength);
   glPixelStorei(GL_UNPACK_SKIP_ROWS,skiprows);
   glPixelStorei(GL_UNPACK_SKIP_PIXELS,skippixels);
   glPixelStorei(GL_UNPACK_ALIGNMENT,alignment);
}
