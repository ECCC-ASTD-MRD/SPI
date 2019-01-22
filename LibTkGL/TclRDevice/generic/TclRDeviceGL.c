// Normal includes
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// R includes
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>
#include <R_ext/Parse.h>
#include <R_ext/Rdynload.h>

// Tcl includes
#include <tcl.h>
#include <tk.h>
#include <tkInt.h>
#include <tkFont.h>

// Driver includes
#include "TclRDeviceGL.h"
#include "tkCanvRDevice.h"

//#define DBGPRINTF(...) printf(__VA_ARGS__);
#define DBGPRINTF(...)

#define MM2INCH     0.0393701

typedef struct TCtx {
    Tcl_Obj         *PS;                // PostScript buffer (MUST be first to use the PS functionality)
    Tk_Window       TkWin;              // TkWindow (MUST be second for PS functionality)
    Display         *Display;           // Connection to the X server
    GLuint          FBuf,RBuf;          // Framebuffer and renderbuffer for offscreen rendering
    GLUtesselator   *Tess;              // Tesselator to draw polygons
    double          *TessBuf;           // Pointer to the tesselator extra point buffer
    void            *Item;              // RDeviceItem (needs to be passed to signal a redraw)
    void            *Buf;               // Buffer used for the tesselator and the image
    size_t          BufSize;            // Size of the above buffer
    int             W,H;                // Width and Height of the device
    int             PxW,PxH;            // Width and height of the underlying buffer
    int             FontSize;           // Font Size currently in use
    int             FontFace;           // Font face currently in use
    Tcl_Obj         *FontFamily;        // Font family currently in use
    Tk_Font         TkFont;             // Current font
    int             Alias;              // Whether we use anti-aliasing or not
    double          InPxX,InPxY;        // Inch per Pixel conversion factor in X and Y
    GLint           VP[4];              // Viewport to restore after drawing
} TCtx;

static union {char bytes[2]; uint16_t ushort;} BYTEORDER = {.ushort=0x1122};
#define IS_BIG_ENDIAN (BYTEORDER.bytes[0]==0x11)

static int TclRDeviceGL_ResizeFramebuffer(TCtx *restrict Ctx,int W,int H,int Alias);
static void TclRDeviceGL_GCColor(TCtx *restrict Ctx,rcolor RCol);


// Canvas item sync functions


/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Destroy>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Signals a kill from the tk side (when the canvas item is destroyed)
 *
 * Parametres   :
 *  <GE>        : Graphics Engine description pointer
 *
 * Retour       :
 *
 * Remarque     : This function should only be triggered by the rdevice canvas item
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void TclRDeviceGL_Destroy(void* GE) {
    if( GE ) {
        killDevice(ndevNumber(((pGEDevDesc)GE)->dev));
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Redraw>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Force the device to replay the display list
 *
 * Parametres   :
 *  <GE>        : Graphics Engine description pointer
 *
 * Retour       :
 *
 * Remarque     : This function should only be triggered by the rdevice canvas item
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void TclRDeviceGL_Redraw(void *GE) {
    if( GE ) {
        // Replay the display list
        GEplayDisplayList(GE);
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Resize>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Resize the device
 *
 * Parametres   :
 *  <GE>        : Graphics Engine description pointer
 *  <W>         : New width
 *  <H>         : New height
 *
 * Retour       :
 *
 * Remarque     : This function should only be called by the rdevice canvas item
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void TclRDeviceGL_Resize(void *GE,int W,int H) {
    if( GE ) {
        pDevDesc dev = ((pGEDevDesc)GE)->dev;
        TCtx *ctx = (TCtx*)dev->deviceSpecific;

        // Free and create a new pixmap if we need a bigger one in one or both dimension(s)
        if( !TclRDeviceGL_ResizeFramebuffer(ctx,W,H,-1) ) {
            // Let's hope this never happens
            TclRDeviceGL_Destroy(GE);
            return;
        }

        // Only resize if we changed dimensions
        if( ctx->W!=W || ctx->H!=H ) {
            // Update the context
            ctx->W = W;
            ctx->H = H;

            // Update the device
            dev->size(NULL,NULL,NULL,NULL,dev);

            // Redraw
            TclRDeviceGL_Redraw(GE);
        }
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_GetFramebuffer>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Return the framebuffer associated with the canvas item's opaque pointer
 *
 * Parametres   :
 *  <GE>        : Graphics Engine description pointer
 *
 * Retour       : The framebuffer associated with the given handle
 *
 * Remarque     : This function should only be called by the rdevice canvas item
 *
 *---------------------------------------------------------------------------------------------------------------
*/
GLuint TclRDeviceGL_GetFramebuffer(void* GE) {
    if( GE ) {
        TCtx *ctx = (TCtx*)((pGEDevDesc)GE)->dev->deviceSpecific;
        return ctx->FBuf;
    }
    return None;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_SetFont>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Force a font change from the widget. Useful to change the font family without going through R.
 *
 * Parametres   :
 *  <GE>        : Graphics Engine description pointer
 *  <Font>      : The new font to use
 *
 * Retour       :
 *
 * Remarque     : This function should only be called by the rdevice canvas item
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void TclRDeviceGL_SetFont(void *GE,Tk_Font Font) {
    if( GE && Font ) {
        TCtx *ctx = (TCtx*)((pGEDevDesc)GE)->dev->deviceSpecific;

        if( ctx->TkFont != Font ) {
            TkFont *font = (TkFont*)Font;   /*This is an ugly hack, but how else am I suppose to get the necessary font specs?*/

            if( ctx->FontFamily ) {
                Tcl_DecrRefCount(ctx->FontFamily);
            }

            // Note that we do NOT need to free the old font, because that is already taken care of by the associated tk widget
            ctx->FontSize   = font->fa.size;
            ctx->FontFace   = (font->fa.slant==TK_FS_ITALIC)<<1|(font->fa.weight==TK_FW_BOLD);
            ctx->FontFamily = Tcl_NewStringObj(strncasecmp(font->fa.family,"itc ",4)?font->fa.family:font->fa.family+4,-1); Tcl_IncrRefCount(ctx->FontFamily);
            ctx->TkFont     = Font;
        }
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_GetFramebuffer>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Set whether we use anti-aliasing or not
 *
 * Parametres   :
 *  <GE>        : Graphics Engine description pointer
 *  <Alias>     : 0 to disable anti-aliasing, any other value to enable it
 *
 * Retour       :
 *
 * Remarque     : This function should only be called by the rdevice canvas item
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void TclRDeviceGL_SetAlias(void* GE,int Alias) {
    if( GE ) {
        TCtx *ctx = (TCtx*)((pGEDevDesc)GE)->dev->deviceSpecific;

        if( !TclRDeviceGL_ResizeFramebuffer(ctx,ctx->W,ctx->H,Alias!=0) ) {
            // Let's hope this never happens
            TclRDeviceGL_Destroy(GE);
            return;
        }
    }
}

// Helper functions


/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_BufCheck>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Make sure the buffer has space for at least Size bytes of data
 *
 * Parametres   :
 *  <Ctx>       : The device context
 *  <Size>      : The minimum size of the buffer
 *
 * Retour       : The buffer address (NULL if error)
 *
 * Remarque     :
 *---------------------------------------------------------------------------------------------------------------
*/
static void* TclRDeviceGL_BufCheck(TCtx *restrict Ctx,size_t Size) {
    if( Ctx->BufSize < Size ) {
        free(Ctx->Buf);
        Ctx->Buf = malloc(Size);
        Ctx->BufSize = Ctx->Buf ? Size : 0;
    }

    return Ctx->Buf;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_CtxFree>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Free the context
 *
 * Parametres   :
 *  <Ctx>       : The context to free
 *
 * Retour       :
 *
 * Remarque     :
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_CtxFree(TCtx *Ctx) {
    if( Ctx ) {
        // Free OpenGL resources
        if( Ctx->FBuf )
            glDeleteFramebuffers(1,&Ctx->FBuf);
        if( Ctx->RBuf )
            glDeleteRenderbuffers(1,&Ctx->RBuf);
        if( Ctx->Tess )
            gluDeleteTess(Ctx->Tess);

        // Free Tcl/Tk resources
        // We do NOT need to free the font, because that will be taken care of by the associated tk widget
        //if( Ctx->TkFont )
        //    Tk_FreeFont(Ctx->TkFont);
        if( Ctx->FontFamily )
            Tcl_DecrRefCount(Ctx->FontFamily);

        // Free other resources
        if( Ctx->Buf )
            free(Ctx->Buf);

        // Free the context
        free(Ctx);
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_ResizeFramebuffer>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Make sure we have a big enough framebuffer and create a bigger one if needed
 *
 * Parametres   :
 *  <Ctx>       : Device context
 *  <W>         : Desired width
 *  <H>         : Desired height
 *  <Alias>     : Whether we want anti-aliasing or not (-1 to use current config)
 *
 * Retour       : 1 if OK, 0 in case of error
 *
 * Remarque     :
 *---------------------------------------------------------------------------------------------------------------
*/
static int TclRDeviceGL_ResizeFramebuffer(TCtx *restrict Ctx,int W,int H,int Alias) {
    if( Alias < 0 )
        Alias = Ctx->Alias;

    // Check if we need to do something
    if( !Ctx->FBuf || !Ctx->RBuf || Ctx->PxW<W || Ctx->PxH<H || Ctx->Alias!=Alias ) {
        // Free the old framebuffer object
        if( Ctx->FBuf ) {
            glDeleteFramebuffers(1,&Ctx->FBuf);
            Ctx->FBuf = 0;
        }

        // Free the renderbuffer
        if( Ctx->RBuf ) {
            glDeleteRenderbuffers(1,&Ctx->RBuf);
            Ctx->RBuf = 0;
        }

        // Create the framebuffer for offline rendering
        glGenFramebuffers(1,&Ctx->FBuf);
        glBindFramebuffer(GL_FRAMEBUFFER,Ctx->FBuf);

        // Create the renderbuffer
        glGenRenderbuffers(1,&Ctx->RBuf);
        glBindRenderbuffer(GL_RENDERBUFFER,Ctx->RBuf);
        if( Alias ) {
            //printf("Max samples : %d\n",GL_MAX_INTEGER_SAMPLES);
            glRenderbufferStorageMultisample(GL_RENDERBUFFER,4,GL_RGBA8,W,H);
            glEnable(GL_MULTISAMPLE);
        } else {
            glRenderbufferStorage(GL_RENDERBUFFER,GL_RGBA8,W,H);
        }

        // Link the renderbuffer to the framebuffer
        glFramebufferRenderbuffer(GL_FRAMEBUFFER,GL_COLOR_ATTACHMENT0,GL_RENDERBUFFER,Ctx->RBuf);

        Ctx->PxW = W;
        Ctx->PxH = H;
        Ctx->Alias = Alias;

        // Check for errors
        return glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE;
    }

    return 1;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_TessCombine>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Combine callback for the tesselation of polygons
 *
 * Parametres   :
 *  <Coords>    : Coordinates of the new point
 *  <DataIn>    : Vertex data
 *  <Weight>    : Weigths of the vertices
 *  <DataOut>   : [OUT] The newly created vertex. Must be freed later on.
 *  <Ctx>       : Device context
 *
 * Retour       :
 *
 * Remarque     : Since this device works in a 2D world without color gradients, our combine version is very simple
 *
 * From online GL doc :
 *
 *  The combine callback is called to create a new vertex when the tessellation detects an intersection or wishes
 *  to merge features. The function takes four arguments: an array of three elements each of type GLdouble, an 
 *  array of four pointers, an array of four elements each of type GLfloat, and a pointer to a pointer.
 *  
 *  The prototype is: void combine( GLdouble coords[3], void *vertex_data[4], GLfloat weight[4], void **outData );
 *
 *  The vertex is defined as a linear combination of up to four existing vertices, stored in vertex_data.
 *  The coefficients of the linear combination are given by weight; these weights always add up to 1. All vertex 
 *  pointers are valid even when some of the weights are 0. coords gives the location of the new vertex.
 *
 *  The user must allocate another vertex, interpolate parameters using vertex_data and weight, and return the
 *  new vertex pointer in outData. This handle is supplied during rendering callbacks. The user is responsible for
 *  freeing the memory some time after gluTessEndPolygon is called.
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_TessCombine(GLdouble Coords[3],void *restrict DataIn[4],GLfloat Weight[4],void **DataOut,TCtx *restrict Ctx) {
    Ctx->TessBuf[0] = Coords[0];
    Ctx->TessBuf[1] = Coords[1];
    *DataOut = Ctx->TessBuf;
    Ctx->TessBuf += 2;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_LineJoinCap>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Handle the line join and line ending as it is not handled by OpenGL itself
 *
 * Parametres   :
 *  <N>         : Number of points
 *  <X>         : X coordinate of points
 *  <Y>         : Y coordinates of points
 *  <GEC>       : R graphical engine context
 *  <Loop>      : True if the last point connects to the first point (i.e for a polygon), false else.
 *
 * Retour       : 1 if OK, 0 in case of error
 *
 * Remarque     :
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_LineJoinCap(int N,double *X,double *Y,const pGEcontext restrict GEC,int Loop) {
    double pts[16],ax,ay,bx,by,vx,vy,sc,lwd=GEC->lwd*0.5;
    int i,j,n,loops;

    if( N<2 || GEC->lwd<=1 || (GEC->lty!=LTY_BLANK && GEC->lty!=LTY_SOLID) )
        return;

    loops = X[0]==X[N-1] && Y[0]==Y[N-1];

    if( !Loop && !loops ) {
        switch( GEC->lend ) {
            case GE_ROUND_CAP:
                glMatrixMode(GL_MODELVIEW);
                glPushMatrix();
                glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

                glLoadIdentity();
                glTranslated(X[0],Y[0],0.0);
                glScaled(lwd,lwd,0.0);
                glDrawCircle(128,GL_POLYGON);

                glLoadIdentity();
                glTranslated(X[N-1],Y[N-1],0.0);
                glScaled(lwd,lwd,0.0);
                glDrawCircle(128,GL_POLYGON);

                glPopMatrix();
                break;
            case GE_BUTT_CAP:
                // This is pretty much the default already
                break;
            case GE_SQUARE_CAP:
                // Line-width-unitary vector in the direction of the line
                ax = X[0] - X[1];
                ay = Y[0] - Y[1];
                sc = lwd/hypot(ax,ay);
                ax *= sc;
                ay *= sc;

                pts[0]=X[0]-ay;     pts[1]=Y[0]+ax;
                pts[2]=pts[0]+ax;   pts[3]=pts[1]+ay;
                pts[6]=X[0]+ay;     pts[7]=Y[0]-ax;
                pts[4]=pts[6]+ax;   pts[5]=pts[7]+ay;

                ax = X[N-1] - X[N-2];
                ay = Y[N-1] - Y[N-2];
                sc = lwd/hypot(ax,ay);
                ax *= sc;
                ay *= sc;

                pts[8]=X[N-1]-ay;   pts[9]=Y[N-1]+ax;
                pts[10]=pts[8]+ax;  pts[11]=pts[9]+ay;
                pts[14]=X[N-1]+ay;  pts[15]=Y[N-1]-ax;
                pts[12]=pts[14]+ax; pts[13]=pts[15]+ay;

                glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
                glEnableClientState(GL_VERTEX_ARRAY);

                glVertexPointer(2,GL_DOUBLE,0,pts);
                glDrawArrays(GL_TRIANGLE_FAN,0,4);
                glDrawArrays(GL_TRIANGLE_FAN,4,4);

                glDisableClientState(GL_VERTEX_ARRAY);
                break;
        }
    }

    if( N<3 )
        return;

    switch( GEC->ljoin ) {
        case GE_MITRE_JOIN:
            // Mitre will be left unimplemeted so far. Let's use round instead!
            //break;
        case GE_ROUND_JOIN:
            glMatrixMode(GL_MODELVIEW);
            glPushMatrix();
            glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

            n = loops||Loop ? N : N-1;
            for(i=Loop?0:1; i<n; ++i) {
                glLoadIdentity();
                glTranslated(X[i],Y[i],0.0);
                glScaled(lwd,lwd,0.0);
                glDrawCircle(128,GL_POLYGON);
            }

            glPopMatrix();
            break;
        case GE_BEVEL_JOIN:
            glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
            glEnableClientState(GL_VERTEX_ARRAY);

            // Handle the case where we need to loop (connect the first and last point) or the line loops (first==last)
            n = loops ? N-1 : N;
            if( Loop || loops ) {
                i = n-1;
                j = 0;
            } else {
                i = 1;
                j = 2;
            }
            // Line-width-unitary vector in the direction of the line
            ax = X[i] - X[i-1];
            ay = Y[i] - Y[i-1];
            sc = lwd/hypot(ax,ay);
            ax *= sc;
            ay *= sc;
            for(; j<n; i=j++,ax=-bx,ay=-by) {
                // Line-width-unitary vector in the direction of the line
                bx = X[i] - X[j];
                by = Y[i] - Y[j];
                sc = lwd/hypot(bx,by);
                bx *= sc;
                by *= sc;

                vx = ax + bx;
                vy = ay + by;

                sc = vx*ay-vy*ax;
                if( fabs(sc) <= 1.0 ) {
                    // Lines are almost parallel, no need for line join
                    continue;
                } else if( sc > 0.0 ) {
                    // The angle between the vector (vx,vy) is <90° with the normal (ay,-ax), keep that point
                    pts[0]=X[i]+ay;     pts[1]=Y[i]-ax;
                    pts[8]=X[i]-ay-ax;  pts[9]=Y[i]+ax-ay;
                } else {
                    // Keep the other point calculated from the normal (-ay,ax)
                    pts[0]=X[i]-ay;     pts[1]=Y[i]+ax;
                    pts[8]=X[i]+ay-ax;  pts[9]=Y[i]-ax-ay;
                }
                pts[10]=pts[0]-ax;  pts[11]=pts[1]-ay;

                sc = vx*by-vy*bx;
                if( fabs(sc) <= 1.0 ) {
                    // Lines are almost parallel, no need for line join
                    continue;
                } else if( sc > 0.0 ) {
                    // The angle between the vector (vx,vy) is <90° with the normal (ay,-ax), keep that point
                    pts[2]=X[i]+by;     pts[3]=Y[i]-bx;
                    pts[6]=X[i]-by-bx;  pts[7]=Y[i]+bx-by;
                } else {
                    // Keep the other point calculated from the normal (-ay,ax)
                    pts[2]=X[i]-by;     pts[3]=Y[i]+bx;
                    pts[6]=X[i]+by-bx;  pts[7]=Y[i]-bx-by;
                }
                pts[4]=pts[2]-bx;   pts[5]=pts[3]-by;

                glVertexPointer(2,GL_DOUBLE,0,pts);
                glDrawArrays(GL_TRIANGLE_FAN,0,6);
            }

            glDisableClientState(GL_VERTEX_ARRAY);
            break;
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_GCColor>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Set the color in the Graphic Context from an R color
 *
 * Parametres   :
 *  <Ctx>       : Device context
 *  <RCol>      : R Color
 *
 * Retour       :
 *
 * Remarque     :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_GCColor(TCtx *restrict Ctx,rcolor RCol) {
    DBGPRINTF("RCol=%u (%u,%u,%u,%u)\n",RCol,R_RED(RCol),R_GREEN(RCol),R_BLUE(RCol),R_ALPHA(RCol));

    // Enable blend if we have transparency
    if( !R_OPAQUE(RCol) ) {
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
    } else {
        glDisable(GL_BLEND);
    }

    // Set the color
    glColor4ub(R_RED(RCol),R_GREEN(RCol),R_BLUE(RCol),R_ALPHA(RCol));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_GCLine>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Set the line style in the Graphic Context from an R graphical engine context
 *
 * Parametres   :
 *  <Ctx>       : Device context
 *  <GEC>       : R graphical engine context
 *
 * Retour       :
 *
 * Remarque     :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_GCLine(TCtx *restrict Ctx,const pGEcontext restrict GEC) {
    DBGPRINTF("lwd(%f) lty(%d) lend(%d) ljoin(%d)\n",GEC->lwd,GEC->lty,GEC->lend,GEC->ljoin);

    if( GEC->lty==LTY_BLANK || GEC->lty==LTY_SOLID ) {
        glDisable(GL_LINE_STIPPLE);
    } else {
        unsigned int    fac,lty;
        unsigned short  pat;
        int             i,tot,val,maxb=sizeof(pat)*8;

        glEnable(GL_LINE_STIPPLE);

        // Get the pattern length
        for(i=0,tot=0,lty=GEC->lty; (lty&0xf)&&i<8; ++i,lty>>=4) {
            tot += lty&0xf;
        }

        // Get the scaling factor (no rounding, this could get ugly)
        fac = tot>maxb ? tot/maxb : 1;

        // Set the pattern (OpenGL's pattern start at bit 0, so we need to treat R's pattern in reverse order)
        for(i-=1,pat=0,tot=0; i>=0; --i) {
            // Scale the value (again, no rounding, let's just hope it works out well)
            val = ((GEC->lty>>4*i)&0xf)/fac;
            tot += val;

            // Odd means brush off (add 0s), even means brush on (add 1s)
            pat = i&1 ? pat<<val : pat<<val|~(~0u<<val);
        }

        // Replicate the pattern (let's hope it wraps ok)
        while( tot < maxb ) {
            pat |= pat<<tot;
            tot *= 2;
        }
        DBGPRINTF("pattern R(%x) GL(%u|%04hx)\n",GEC->lty,fac,pat);

        glLineStipple(fac,pat);
    }

    //TODO
    //switch( GEC->lend ) {
    //    case GE_ROUND_CAP:  capstyle=CapRound;      break;
    //    case GE_BUTT_CAP:   capstyle=CapButt;       break;
    //    case GE_SQUARE_CAP: capstyle=CapProjecting; break;
    //    //capstyle=CapNotLast;    break;
    //}

    //switch( GEC->ljoin ) {
    //    case GE_ROUND_JOIN: joinstyle=JoinRound;    break;
    //    case GE_MITRE_JOIN: joinstyle=JoinMiter;    break;
    //    case GE_BEVEL_JOIN: joinstyle=JoinBevel;    break;
    //}

    glLineWidth((float)GEC->lwd);
    glPointSize((float)GEC->lwd);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_GCFont>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Set the font Graphic Context from an R graphical engine context
 *
 * Parametres   :
 *  <Ctx>       : Device context
 *  <GEC>       : R graphical engine context
 *  <Dev>       : The device
 *
 * Retour       :
 *
 * Remarque     :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_GCFont(TCtx *restrict Ctx,const pGEcontext restrict GEC,pDevDesc Dev) {
    int fontsize = (int)round(GEC->ps*GEC->cex);
    int fontface = GEC->fontface-1;
    int famchange = (GEC->fontfamily[0]!='\0'&&strcmp(Tcl_GetString(Ctx->FontFamily),GEC->fontfamily));

    // Strings that will be used to query fonts
    static Tcl_Obj *str_family=NULL,*str_size,*str_weight,*str_slant,*str_bold,*str_normal,*str_roman,*str_italic;
    if( !str_family ) {
        Tcl_IncrRefCount( str_family    = Tcl_NewStringObj("-family",7) );
        Tcl_IncrRefCount( str_size      = Tcl_NewStringObj("-size",5) );
        Tcl_IncrRefCount( str_weight    = Tcl_NewStringObj("-weight",7) );
        Tcl_IncrRefCount( str_slant     = Tcl_NewStringObj("-slant",6) );
        Tcl_IncrRefCount( str_bold      = Tcl_NewStringObj("bold",4) );
        Tcl_IncrRefCount( str_normal    = Tcl_NewStringObj("normal",6) );
        Tcl_IncrRefCount( str_roman     = Tcl_NewStringObj("roman",5) );
        Tcl_IncrRefCount( str_italic    = Tcl_NewStringObj("italic",6) );
    }

    if( Ctx->FontSize!=fontsize || Ctx->FontFace!=fontface || famchange ) {
        Tcl_Obj *lst,*objs[8];
        Tk_Font font;

        DBGPRINTF("Font family(%s|%s) cex(%g) ps(%g) lineheight(%g) fontface(%d|%d) -- fontsize(%d|%d)\n",
                GEC->fontfamily,Tcl_GetString(Ctx->FontFamily),GEC->cex,GEC->ps,GEC->lineheight,fontface,Ctx->FontFace,fontsize,Ctx->FontSize);

        // Specify the family (Keep the same one if none is specified)
        objs[0] = str_family;
        objs[1] = famchange ? Tcl_NewStringObj(GEC->fontfamily,-1) : Ctx->FontFamily;

        // Specify the font size (in points) from the "cex" and "ps" params
        objs[2] = str_size;
        objs[3] = Tcl_NewIntObj(fontsize);

        // Add the style from the "fontface" param (originally 1=plain, 2=bold, 3=italic, 4=bold-italic, 5=plain symbol,
        // but we set it back to a zero-based value so the following code works)
        objs[4] = str_weight;
        objs[5] = fontface&1 ? str_bold : str_normal;
        objs[6] = str_slant;
        objs[7] = fontface&2 ? str_italic : str_roman;

        lst = Tcl_NewListObj(8,objs);

        // Get the font
        if( (font=Tk_AllocFontFromObj(NULL,Ctx->TkWin,lst)) ) {
            // It is NOT needed to free the font as it will be managed by the associated Tk widget
            //Tk_FreeFont(Ctx->TkFont);

            // Assign the new font
            Ctx->FontSize   = fontsize;
            Ctx->FontFace   = fontface;
            Ctx->TkFont     = font;

            if( famchange ) {
                Tcl_DecrRefCount(Ctx->FontFamily);
                Ctx->FontFamily = objs[1];
                Tcl_IncrRefCount(Ctx->FontFamily);
            }

            // Also update the character size in rasters
            Dev->cra[0] = 0.9*fontsize/(Ctx->InPxX*72.0);
            Dev->cra[1] = 1.2*fontsize/(Ctx->InPxY*72.0);

            glFontUse(Ctx->Display,font);

            // Update the font on the item (the old font will be freed there)
            RDeviceItem_SetFont(Ctx->Item,Ctx->TkFont);
        } else {
            fprintf(stderr,"%s: Could not get font for params \"%s\"\n",__func__,Tcl_GetStringFromObj(lst,NULL));
            Tcl_DecrRefCount(lst);
        }
    }
}


// Standard RDevice function implementations


//static void (*activate)(const pDevDesc );

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Circle>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Draw a circle
 *
 * Parametres   :
 *  <X>         : X position of the center of the circle to draw
 *  <Y>         : Y position of the center of the circle to draw
 *  <R>         : Radius of the circle to draw
 *  <GEC>       : R graphical engine context. To honor : col, fill, gamma, lty, lwd
 *  <Dev>       : The device on which to act
 *
 * Retour       :
 *
 * Remarque     : This is a handler called from the R device engine
 *
 * from R Doc   :
 *
 *  device_Circle should have the side-effect that a circle is drawn, centred at the given location, with
 *  the given radius. (If the device has non-square pixels, 'radius' should be interpreted in the units of
 *  the x direction.)
 *
 *  The border of the circle should be drawn in the given "col", and the circle should be filled with the
 *  given "fill" colour.
 *  If "col" is NA_INTEGER then no border should be drawn
 *  If "fill" is NA_INTEGER then the circle should not be filled.
 *
 *  R_GE_gcontext parameters that should be honoured (if possible): col, fill, gamma, lty, lwd
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_Circle(double X,double Y,double R,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    DBGPRINTF("Circle @[%.4f,%.4f] r=%.4f\n",X,Y,R);

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();
    glTranslated(X,Y,0.0);
    glScaled(R,R,0.0);

    // Check if we need to fill the circle
    if( GEC->fill != NA_INTEGER ) {
        TclRDeviceGL_GCColor(ctx,(rcolor)GEC->fill);

        glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
        glDrawCircle(128,GL_POLYGON);
    }

    // Check if we need to draw the borders
    if( GEC->col!=NA_INTEGER && (GEC->fill==NA_INTEGER||GEC->fill!=GEC->col) ) {
        TclRDeviceGL_GCLine(ctx,GEC);
        TclRDeviceGL_GCColor(ctx,(rcolor)GEC->col);

        glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
        glDrawCircle(128,GL_LINE_STRIP);
    }

    glPopMatrix();
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Clip>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Set the clipping region
 *
 * Parametres   :
 *  <X0>        : X position of the first clipping rectangle's corner
 *  <X1>        : X position of the second clipping rectangle's corner
 *  <Y0>        : Y position of the first clipping rectangle's corner
 *  <Y1>        : Y position of the second clipping rectangle's corner
 *  <Dev>       : The device on which to act
 *
 * Retour       :
 *
 * Remarque     : This is a handler called from the R device engine
 *
 * from R Doc   :
 *
 *  device_Clip is given the left, right, bottom, and top of a rectangle (in DEVICE coordinates).
 *  It should have the side-effect that subsequent output is clipped to the given rectangle.
 *  NOTE that R's graphics engine already clips to the extent of the device.
 *  NOTE also that this will probably only be called if the flag canClip is true. [ELO : NOT TRUE!!]
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_Clip(double X0,double X1,double Y0,double Y1,pDevDesc Dev) {
    DBGPRINTF("Clip to [%.4f,%.4f] [%.4f,%.4f]\n",X0,Y0,X1,Y1);

    Dev->clipLeft   = X0<=X1 ? floor(X0) : floor(X1);
    Dev->clipRight  = X0<=X1 ? floor(X1) : floor(X0);
    Dev->clipBottom = Y0<=Y1 ? floor(Y0) : floor(Y1);
    Dev->clipTop    = Y0<=Y1 ? floor(Y1) : floor(Y0);

    glEnable(GL_SCISSOR_TEST);
    glScissor((int)Dev->clipLeft,(int)Dev->clipBottom,(int)(Dev->clipRight-Dev->clipLeft),(int)(Dev->clipTop-Dev->clipBottom));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Close>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Free the device's resources
 *
 * Parametres   :
 *  <Dev>       : The device on which to act
 *
 * Retour       :
 *
 * Remarque     : This is a handler called from the R device engine
 *
 * from R Doc   :
 *
 *  device_Close is called when the device is killed. This function is responsible for destroying any
 *  device-specific resources that were created in device_Open and for FREEing the device-specific parameters
 *  structure.
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_Close(pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;
    DBGPRINTF("Freeing RDevice\n");

    // Detach this device from the associated item
    RDeviceItem_DetachDevice(ctx->Item);

    // Free the context
    TclRDeviceGL_CtxFree(ctx);
    Dev->deviceSpecific = NULL;
}

//static void (*deactivate)(pDevDesc );
//static Rboolean (*locator)(double *x,double *y,pDevDesc Dev);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Line>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Draw a line
 *
 * Parametres   :
 *  <X0>        : X position of the first point
 *  <Y0>        : Y position of the first point
 *  <X1>        : X position of the second point
 *  <Y1>        : Y position of the second point
 *  <GEC>       : R graphical engine context. To honor : col, gamma, lty, lwd
 *  <Dev>       : The device on which to act
 *
 * Retour       :
 *
 * Remarque     : This is a handler called from the R device engine
 *
 * from R Doc   :
 *
 *  device_Line should have the side-effect that a single line is drawn (from x0,y0 to x1,y1)
 *
 *  R_GE_gcontext parameters that should be honoured (if possible): col, gamma, lty, lwd
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_Line(double X0,double Y0,double X1,double Y1,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    DBGPRINTF("Line [%.4f,%.4f] -> [%.4f,%.4f]\n",X0,Y0,X1,Y1);
    TclRDeviceGL_GCLine(ctx,GEC);
    TclRDeviceGL_GCColor(ctx,(rcolor)GEC->col);

    glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
    glBegin(GL_LINES);
        glVertex2d(X0,Y0);
        glVertex2d(X1,Y1);
    glEnd();
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_MetricInfo>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Return metric infos
 *
 * Parametres   :
 *  <C>         : Character for the metrics (see note)
 *  <GEC>       : R graphical engine context. To honor : font, cex, ps
 *  <Ascent>    : [OUT] Amount in pixels that the tallest letter sticks up above the baseline, plus any extra
 *                blank space added by the designer of the font
 *  <Descent    : [OUT] Amount in pixels that any letter sticks below the baseline, plus any extra
 *                blank space added by the designer of the font
 *  <Width>     : [OUT] Width of the char
 *  <Dev>       : The device on which to act
 *
 * Retour       :
 *
 * Remarque     : This is a handler called from the R device engine
 *
 * from R Doc   :
 *
 *  device_MetricInfo should return height, depth, and width information for the given character in DEVICE units.
 *
 *  Note: in an 8-bit locale, c is 'char'. In an mbcslocale, it is wchar_t, and at least some of code assumes
 *  that is UCS-2 (Windows, true) or UCS-4.
 *
 *  This is used for formatting mathematical expressions and for exact centering of text (see GText)
 *  If the device cannot provide metric information then it MUST return 0.0 for ascent, descent, and width.
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_MetricInfo(int C,const pGEcontext restrict GEC,double *Ascent,double *Descent,double *Width,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;
    Tk_FontMetrics fm;
    int width,n;
    char str[TCL_UTF_MAX];

    TclRDeviceGL_GCFont(ctx,GEC,Dev);

    // Get the ascent and descent
    Tk_GetFontMetrics(ctx->TkFont,&fm);

    // Get the width
    if( C < 0 ) {
        // We have a UCS-2 string, which is what Tcl uses for its UniChar, so just convert it back to utf-8 so we can get its width
        n = Tcl_UniCharToUtf(-C,str);
    } else {
        n = 1;
        str[0] = (char)C;
    }
    width = Tk_TextWidth(ctx->TkFont,str,n);
    DBGPRINTF("Font metrics queried ascent=%d descent=%d linespace=%d width=%d (%.*s)\n",fm.ascent,fm.descent,fm.linespace,width,n,str);

    *Ascent = fm.ascent;
    *Descent = fm.descent;
    *Width = width;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Mode>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Changes the mode of the device
 *
 * Parametres   :
 *  <Mode>      : 1 if start drawing, 0 if stops drawing (and a possible 2 which might exist)
 *  <Dev>       : The device on which to act
 *
 * Retour       :
 *
 * Remarque     : This is a handler called from the R device engine
 *
 * from R Doc   :
 *
 *  device_Mode is called whenever the graphics engine starts drawing (mode=1) or stops drawing (mode=0)
 *  GMode (in graphics.c) also says that mode = 2 (graphical input on) exists.
 *
 *  The device is not required to do anything
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_Mode(int Mode,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    DBGPRINTF("Mode set to %d\n",Mode);

    switch( Mode ) {
        case 0: // Device stopped drawing
            // Restore matrix states
            glMatrixMode(GL_PROJECTION);
            glPopMatrix();

            glMatrixMode(GL_MODELVIEW);
            glPopMatrix();

            // Restore viewport
            glViewport(ctx->VP[0],ctx->VP[1],ctx->VP[2],ctx->VP[3]);

            // Set back the rendering target to the screen
            glBindFramebuffer(GL_FRAMEBUFFER,0);
            glDrawBuffer(GL_BACK);

            // Turn off some modes we might have enabled
            glDisable(GL_LINE_STIPPLE);
            glDisable(GL_BLEND);
            glDisable(GL_SCISSOR_TEST);
            
            // Signal a refresh
            RDeviceItem_SignalRedraw(ctx->Item);

            break;
        case 1: // Device starts drawing
            // Save viewport state
            glGetIntegerv(GL_VIEWPORT,ctx->VP);

            // Set the rendering target
            glBindFramebuffer(GL_FRAMEBUFFER,ctx->FBuf);
            glDrawBuffer(GL_COLOR_ATTACHMENT0);

            // Save and reset matrix states
            glMatrixMode(GL_MODELVIEW);
            glPushMatrix();
            glLoadIdentity();

            glMatrixMode(GL_PROJECTION);
            glPushMatrix();
            glLoadIdentity();

            // Set the viewport and projection
            glViewport(0,0,ctx->W,ctx->H);
            glOrtho(0.,ctx->W,0.,ctx->H,-1.,1.);

            // Set the clipping back
            TclRDeviceGL_Clip(Dev->clipLeft,Dev->clipRight,Dev->clipBottom,Dev->clipTop,Dev);

            // Make sure we use whatever we have in our context
            glFontUse(ctx->Display,ctx->TkFont);

            break;
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_NewPage>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Clear the device
 *
 * Parametres   :
 *  <GEC>       : R graphical engine context. To honor : fill
 *  <Dev>       : The device on which to act
 *
 * Retour       :
 *
 * Remarque     : This is a handler called from the R device engine
 *
 * from R Doc   :
 *
 *  device_NewPage is called whenever a new plot requires a new page.
 *  A new page might mean just clearing the device (e.g., X11) or moving to a new page (e.g., postscript)
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_NewPage(const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx    *ctx = (TCtx*)Dev->deviceSpecific;
    rcolor  rcol = (rcolor)GEC->fill;

    DBGPRINTF("CLEAR\n");

    // Set the rendering target to our framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER,ctx->FBuf);
    glDrawBuffer(GL_COLOR_ATTACHMENT0);

    // Reset clipping
    TclRDeviceGL_Clip(Dev->left,Dev->right,Dev->bottom,Dev->top,Dev);

    // Reset background
    glClearColor(R_RED(rcol)/255.f,R_GREEN(rcol)/255.f,R_BLUE(rcol)/255.f,R_ALPHA(rcol)/255.f);
    glClear(GL_COLOR_BUFFER_BIT);

    // Set back the rendering target to the screen
    glBindFramebuffer(GL_FRAMEBUFFER,0);
    glDrawBuffer(GL_BACK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Polygon>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Draw a polygon
 *
 * Parametres   :
 *  <N>         : Number of points in the polygon
 *  <X>         : X coords of the polygon
 *  <Y>         : Y coords of the polygon
 *  <GEC>       : R graphical engine context. To honor : col, fill, gamma, lty, lwd
 *  <Dev>       : The device on which to act
 *
 * Retour       :
 *
 * Remarque     : This is a handler called from the R device engine
 *
 * from R Doc   :
 *
 *  device_Polygon should have the side-effect that a polygon is drawn using the given x and y values
 *  the polygon border should be drawn in the "col" colour and filled with the "fill" colour.
 *  If "col" is NA_INTEGER don't draw the border
 *  If "fill" is NA_INTEGER don't fill the polygon
 *
 *  R_GE_gcontext parameters that should be honoured (if possible): col, fill, gamma, lty, lwd
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_Polygon(int N,double *X,double *Y,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx    *ctx = (TCtx*)Dev->deviceSpecific;

    DBGPRINTF("Polygon (%d)\n",N);

    if( N >= 3 ) {
        int i;

        //for(i=0; i<N; ++i)
        //    DBGPRINTF("    %.2f %.2f\n",X[i],Y[i]);

        // Check if we need to fill the polygon
        if( GEC->fill != NA_INTEGER ) {
            TclRDeviceGL_GCColor(ctx,(rcolor)GEC->fill);

            // Reserve 3*N for the 3D points (needed by the tesselator) and 2*N for the extra points added by
            // the tesselation. It is assumed that there won't be more than N extra points.
            if( TclRDeviceGL_BufCheck(ctx,5*N*sizeof(double)) ) {
                double *buf;

                // Position where the extra points will be added
                ctx->TessBuf = ctx->Buf + 3*N*sizeof(double);

                // Init the tesselation
                glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
                gluTessBeginPolygon(ctx->Tess,ctx);
                gluTessBeginContour(ctx->Tess);

                // Add the points to be tesselated
                for(i=0,buf=ctx->Buf; i<N; ++i,buf+=3) {
                    buf[0] = X[i];
                    buf[1] = Y[i];
                    buf[2] = 0.0;
                    gluTessVertex(ctx->Tess,buf,buf);
                }

                // End the tesselation
                gluTessEndContour(ctx->Tess);
                gluTessEndPolygon(ctx->Tess);
            }
        }
        // Check if we need to draw the borders
        if( GEC->col!=NA_INTEGER && (GEC->fill==NA_INTEGER||GEC->fill!=GEC->col) ) {
            TclRDeviceGL_GCLine(ctx,GEC);
            TclRDeviceGL_GCColor(ctx,(rcolor)GEC->col);

            glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
            glBegin(GL_LINE_LOOP);
            for(i=0; i<N; ++i) {
                glVertex2d(X[i],Y[i]);
            }
            glEnd();

            TclRDeviceGL_LineJoinCap(N,X,Y,GEC,1);
        }
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Polyline>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Draw a polyline
 *
 * Parametres   :
 *  <N>         : Number of points in the polyline
 *  <X>         : X coords of the polyline
 *  <Y>         : Y coords of the polyline
 *  <GEC>       : R graphical engine context. To honor : col, gamma, lty, lwd
 *  <Dev>       : The device on which to act
 *
 * Retour       :
 *
 * Remarque     : This is a handler called from the R device engine
 *
 * from R Doc   :
 *
 *  device_Polyline should have the side-effect that a series of line segments are drawn using the given x
 *  and y values.
 *
 *  R_GE_gcontext parameters that should be honoured (if possible): col, gamma, lty, lwd
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_Polyline(int N,double *X,double *Y,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx    *ctx = (TCtx*)Dev->deviceSpecific;

    DBGPRINTF("Polyline (%d)\n",N);

    if( N ) {
        int i;

        // Draw the lines
        TclRDeviceGL_GCLine(ctx,GEC);
        TclRDeviceGL_GCColor(ctx,(rcolor)GEC->col);

        glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
        glBegin(GL_LINE_STRIP);
        for(i=0; i<N; ++i) {
            glVertex2d(X[i],Y[i]);
        }
        glEnd();

        TclRDeviceGL_LineJoinCap(N,X,Y,GEC,0);
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Rect>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Draw a rectangle
 *
 * Parametres   :
 *  <X0>        : X position of the first point
 *  <Y0>        : Y position of the first point
 *  <X1>        : X position of the second point
 *  <Y1>        : Y position of the second point
 *  <GEC>       : R graphical engine context. To honor : col, fill, gamma, lty, lwd
 *  <Dev>       : The device on which to act
 *
 * Retour       :
 *
 * Remarque     : This is a handler called from the R device engine
 *
 * from R Doc   :
 *
 *  device_Rect should have the side-effect that a rectangle is drawn with the given locations for its
 *  opposite corners. The border of the rectangle should be in the given "col" colour and the rectangle
 *  should be filled with the given "fill" colour.
 *  If "col" is NA_INTEGER then no border should be drawn
 *  If "fill" is NA_INTEGER then the rectangle should not be filled.
 *
 *  R_GE_gcontext parameters that should be honoured (if possible): ?? [ELO probably col, fill, gamma, lty, lwd]
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_Rect(double X0,double Y0,double X1,double Y1,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx    *ctx=(TCtx*)Dev->deviceSpecific;
    
    DBGPRINTF("Rect [%.4f,%.4f] -> [%.4f,%.4f]\n",X0,Y0,X1,Y1);

    // Check if we need to fill the rectangle
    if( GEC->fill != NA_INTEGER ) {
        TclRDeviceGL_GCColor(ctx,(rcolor)GEC->fill);

        glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
        glBegin(GL_QUADS);
            glVertex2d(X0,Y0);
            glVertex2d(X1,Y0);
            glVertex2d(X1,Y1);
            glVertex2d(X0,Y1);
        glEnd();
    }
    // Check if we need to draw the borders
    if( GEC->col!=NA_INTEGER && (GEC->fill==NA_INTEGER||GEC->fill!=GEC->col) ) {
        double x[] = {X0,X1,X1,X0};
        double y[] = {Y0,Y0,Y1,Y1};

        TclRDeviceGL_GCLine(ctx,GEC);
        TclRDeviceGL_GCColor(ctx,(rcolor)GEC->col);

        glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
        glBegin(GL_QUADS);
            glVertex2d(X0,Y0);
            glVertex2d(X1,Y0);
            glVertex2d(X1,Y1);
            glVertex2d(X0,Y1);
        glEnd();

        TclRDeviceGL_LineJoinCap(4,x,y,GEC,1);
    }
}

//static void (*path)(double *x,double *y,int npoly,int *nper,Rboolean winding,const pGEcontext restrict GEC,pDevDesc Dev);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Raster>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Draw a raster
 *
 * Parametres   :
 *  <Raster>    : Image data BY ROW, every four bytes giving on R colour (ABGR)
 *  <W>         : Width of the raster (size in X)
 *  <H>         : Height of the raster (size in Y)
 *  <X>         : X position of the bottom-left corner
 *  <Y>         : Y position of the bottom-left corner
 *  <Width>     : Width we want the raster to have on the device (see Interp)
 *  <Height>    : Height we want the raster to have on the device (see Interp)
 *  <Rot>       : Rotation angle (degree) with positive rotation anticlockwise from the positive x-axis
 *  <Interp>    : Whether to apply linear interpolation to the image
 *  <GEC>       : R graphical engine context. To honor : ???
 *  <Dev>       : The device on which to act
 *
 * Retour       :
 *
 * Remarque     : This is a handler called from the R device engine
 *
 * from R Doc   :
 *
 *  device_Raster should draw a raster image justified at the given location, size, and rotation
 *  (not all devices may be able to rotate?)
 *
 *  'raster' gives the image data BY ROW, with every four bytes giving one R colour (ABGR).
 *  'x and 'y' give the bottom-left corner.
 *
 *  'rot' is in degrees (as per device_Text), with positive rotation anticlockwise from the positive x-axis.
 *
 *  R_GE_gcontext parameters that should be honoured (if possible): ??
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_Raster(unsigned int *Raster,int W,int H,double X,double Y,double Width,double Height,double Rot,Rboolean Interp,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx            *ctx=(TCtx*)Dev->deviceSpecific;
    int             imgW,imgH,bufs,idx,rotW=0,rotH=0;
    double          angle = Rot*DEG2RAD;
    unsigned int    *data;
    float           zoomX,zoomY;

    DBGPRINTF("Raster [%.4f,%.4f]+[%.4f,%.4f] (Ori: %dx%d) Interp=%d Rot=%.4f\n",X,Y,Width,Height,W,H,Interp,Rot);

    // Get the width/height and coords of the image
    imgW    = (int)Width;
    imgH    = (int)Height;
    bufs    = imgW*imgH;

    // Adjust values based on transformation
    if( Rot != 0.0 ) {
        double rotX,rotY;

        // Calculate the BBOX around the rotated image
        R_GE_rasterRotatedSize(imgW,imgH,angle,&rotW,&rotH);

        // We'll need twice the buffer size since we need some manipulations to take place later on
        bufs = rotW*rotH*2;

        // Adjust the position of the bottom left corner
        R_GE_rasterRotatedOffset(imgW,imgH,angle,1,&rotX,&rotY);

        X -= (rotW-imgW)*0.5 + rotX;
        Y -= (rotH-imgH)*0.5 + rotY;
    }

    // Make sure we have a big enough memory buffer
    if( !TclRDeviceGL_BufCheck(ctx,bufs*sizeof(*Raster)) ) {
        fprintf(stderr,"%s: Could not create image buffer (%dx%dx4), raster won't be drawn\n",__func__,imgW,imgH);
        return;
    }
    
    // Scale the image to the required size
    if( Interp ) {
        R_GE_rasterInterpolate(Raster,W,H,ctx->Buf,imgW,imgH);
    } else {
        R_GE_rasterScale(Raster,W,H,ctx->Buf,imgW,imgH);
    }

    // Make the image rotation
    if( Rot != 0.0 ) {
        data = (unsigned int*)((char*)ctx->Buf + rotW*rotH*sizeof(*Raster));

        R_GE_rasterResizeForRotation(ctx->Buf,imgW,imgH,data,rotW,rotH,GEC);
        R_GE_rasterRotate(data,rotW,rotH,angle,ctx->Buf,GEC,FALSE);

        imgW = rotW;
        imgH = rotH;
    }

    // R gives ABGR (msb->lsb) byte order, but on a little-endian system, that gives RGBA when read byte by byte, which is perfect for OpenGL
    // However, on a big-endian system, bytes have to be swapped
    if( IS_BIG_ENDIAN ) {
        unsigned char *bytes,swap;

        // We are on a big-endian system, swap the bytes (ABGR -> RGBA)
        for(idx=imgW*imgH,bytes=ctx->Buf; --idx; bytes+=4) {
            swap=bytes[0]; bytes[0]=bytes[3]; bytes[3]=swap;
            swap=bytes[1]; bytes[1]=bytes[2]; bytes[2]=swap;
        }
    }

    // The raster pos can't be out of the clipping area, otherwise the whole raster won't be shown.
    // Here, we are basically finding a corner inside the clipping area and reversing the picture accordingly
    if( X < 0.0 ) {
        unsigned int swap;
        int i,j;

        X += imgW;
        zoomX = -1.0f;

        // Flip the image in X
        for(j=imgH,data=ctx->Buf; j; --j) {
            for(i=imgW-1; i>0; i-=2,++data) {
                swap = *data;
                *data = data[i];
                data[i] = swap;
            }
            data += (imgW>>1) + (imgW&1);
        }
    } else {
        zoomX = 1.0f;
    }
    // Note that the Y direction needs to be reversed in the first place, so we only need to
    if( Y < 0.0 ) {
        Y += imgH;
        zoomY = -1.0f;
    } else {
        unsigned int swap;
        int i,j;

        zoomY = 1.0f;

        // Flip the image in Y
        for(j=imgH-1,data=ctx->Buf; j>0; j-=2) {
            idx = j*imgW;
            for(i=imgW; i; --i,++data) {
                swap = *data;
                *data = data[idx];
                data[idx] = swap;
            }
        }
    }

    // Draw the image
    glRasterPos2d(X,Y);
    glPixelZoom(zoomX,zoomY);
    glDrawPixels(imgW,imgH,GL_RGBA,GL_UNSIGNED_BYTE,ctx->Buf);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Cap>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Create an R raster from the device pixmap
 *
 * Parametres   :
 *  <Dev>       : The device on which to act
 *
 * Retour       : An R raster (matrix)
 *
 * Remarque     : This is a handler called from the R device engine
 *
 * from R Doc   :
 *
 *  device_Cap should return an integer matrix (R colors) representing the current contents of the device display.
 *
 *  The result is expected to be ROW FIRST.
 *
 *  This will only make sense for raster devices and can probably only be implemented for screen devices.
 *---------------------------------------------------------------------------------------------------------------
 */
static SEXP TclRDeviceGL_Cap(pDevDesc Dev) {
    TCtx            *ctx=(TCtx*)Dev->deviceSpecific;
    unsigned int    *rdata;
    SEXP            raster=R_NilValue,rdim;
    int             size=ctx->W*ctx->H;

    // Allocate the memory in R for the raster
    PROTECT( raster=allocVector(INTSXP,size) );
    rdata=(unsigned int*)INTEGER(raster);

    // Get the bytes
    glReadPixels(0,0,ctx->W,ctx->H,GL_BGRA,GL_UNSIGNED_BYTE,rdata);

    // R wants ABGR (msb->lsb) byte order, but on a little-endian system, that gives RGBA when read byte by byte, which is perfect for OpenGL
    // However, on a big-endian system, bytes have to be swapped
    if( IS_BIG_ENDIAN ) {
        unsigned char *bytes,swap;

        // We are on a big-endian system, swap the bytes (ABGR -> RGBA)
        for(bytes=(unsigned char*)rdata; --size; bytes+=4) {
            swap=bytes[0]; bytes[0]=bytes[3]; bytes[3]=swap;
            swap=bytes[1]; bytes[1]=bytes[2]; bytes[2]=swap;
        }
    }

    // Add the dimensions (nrow,ncol) as attribute (create an R matrix)
    PROTECT( rdim=allocVector(INTSXP,2) );
    INTEGER(rdim)[0] = ctx->H;
    INTEGER(rdim)[1] = ctx->W;
    Rf_setAttrib(raster,R_DimSymbol,rdim);

    // Free everything
    UNPROTECT(2);

    return raster;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Size>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Get/Set the size of the device
 *
 * Parametres   :
 *  <Left>      : X coord of the leftmost corner
 *  <Right>     : X coord of the rightmost corner
 *  <Bottom>    : Y coord of the bottommost corner
 *  <Top>       : Y coord of the topmost corner
 *  <Dev>       : The device on which to act
 *
 * Retour       :
 *
 * Remarque     : This is a handler called from the R device engine. Note that the "write" part is not used by R.
 *
 * from R Doc   :
 *
 *  device_Size is called whenever the device is resized. The function returns (left, right, bottom, and top) for the
 *  new device size. This is not usually called directly by the graphics engine because the detection of device resizes
 *  (e.g., a window resize) are usually detected by device-specific code.
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_Size(double *Left,double *Right,double *Bottom,double *Top,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    if( Left ) {
        *Left   = 0.0;
        *Right  = ctx->W;
        *Bottom = ctx->H;
        *Top    = 0.0;
    } else {
        Dev->left   = 0.;
        Dev->right  = ctx->W;
        Dev->bottom = 0.;
        Dev->top    = ctx->H;
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_StrWidth>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Return the size of the text in device units (pixels)
 *
 * Parametres   :
 *  <Str>       : String we want the screen size of
 *  <GEC>       : R graphical engine context. To honor : font, cex, ps
 *  <Dev>       : The device on which to act
 *
 * Retour       :
 *
 * Remarque     : This is a handler called from the R device engine
 *
 * from R Doc   :
 *
 *  device_StrWidth should return the width of the given string in DEVICE units.
 *
 *  R_GE_gcontext parameters that should be honoured (if possible): font, cex, ps
 *---------------------------------------------------------------------------------------------------------------
*/
static double TclRDeviceGL_StrWidth(const char *Str,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    TclRDeviceGL_GCFont(ctx,GEC,Dev);

    DBGPRINTF("StrWidth of (%s)(%d) is %d\n",Str,(int)strlen(Str),Tk_TextWidth(ctx->TkFont,Str,strlen(Str)));
    return Tk_TextWidth(ctx->TkFont,Str,strlen(Str));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Text>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Draw text
 *
 * Parametres   :
 *  <X>         : X coords of the text
 *  <Y>         : Y coords of the text
 *  <Str>       : String we want the to draw
 *  <Rot>       : Rotation angle (degrees)
 *  <HAdj>      : Horizontal adjustment (ignored)
 *  <GEC>       : R graphical engine context. To honor : font, cex, ps, col, gamma
 *  <Dev>       : The device on which to act
 *
 * Retour       :
 *
 * Remarque     : This is a handler called from the R device engine
 *
 * from R Doc   :
 *
 *  device_Text should have the side-effect that the given text is drawn at the given location.
 *  The text should be rotated according to rot (degrees)
 *
 *  R_GE_gcontext parameters that should be honoured (if possible): font, cex, ps, col, gamma
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceGL_Text(double X,double Y,const char *Str,double Rot,double HAdj,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    DBGPRINTF("Text @[%.4f,%.4f] rotated[%.2f] hadj(%.4f) : (%s)\n",X,Y,Rot,HAdj,Str);
    TclRDeviceGL_GCColor(ctx,(rcolor)GEC->col);
    TclRDeviceGL_GCFont(ctx,GEC,Dev);
    glDrawString((int)X,(int)Y,(int)round(Rot),(char*)Str,strlen(Str),0,0);
}
static void TclRDeviceGL_TextUTF8(double X,double Y,const char *Str,double Rot,double HAdj,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;
    Tcl_DString dstr;

    DBGPRINTF("TextUTF @[%.4f,%.4f] rotated[%.2f] hadj(%.4f) : (%s)\n",X,Y,Rot,HAdj,Str);

    // Get the unichar representation
    Tcl_DStringInit(&dstr);
    Tcl_UtfToUniCharDString(Str,strlen(Str),&dstr);

    // Draw the string
    TclRDeviceGL_GCColor(ctx,(rcolor)GEC->col);
    TclRDeviceGL_GCFont(ctx,GEC,Dev);
    glDrawString((int)X,(int)Y,(int)round(Rot),Tcl_DStringValue(&dstr),Tcl_DStringLength(&dstr),1,0);

    Tcl_DStringFree(&dstr);
}

//static void (*onExit)(pDevDesc Dev);
//static SEXP (*getEvent)(SEXP,const char *);
//static Rboolean (*newFrameConfirm)(pDevDesc Dev);

//static double (*strWidthUTF8)(const char *str,const pGEcontext restrict GEC,pDevDesc Dev);
//static void (*eventHelper)(pDevDesc Dev,int code);
//static int (*holdflush)(pDevDesc Dev,int level);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_NewDev>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Create a new RDevice structure
 *
 * Parametres   :
 *  <Ctx>       : The new device's context
 *
 * Retour       : A new RDevice initialized
 *
 * Remarque     :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static DevDesc* TclRDeviceGL_NewDev(TCtx *Ctx) {
    pDevDesc dev = calloc(1,sizeof(*dev));

    if( dev ) {
        Tk_FontMetrics fm;
        Tk_GetFontMetrics(Ctx->TkFont,&fm);

        // Device physical parameters
        dev->left       = 0.;
        dev->right      = Ctx->W;
        dev->bottom     = 0.;
        dev->top        = Ctx->H;
        dev->clipLeft   = 0.;
        dev->clipRight  = Ctx->W;
        dev->clipBottom = 0.;
        dev->clipTop    = Ctx->H;
        dev->xCharOffset= 0.4900;       // ┌╴ Character addressing offsets
        dev->yCharOffset= 0.3333;       // │  used to plot a single plotting character
        dev->yLineBias  = 0.1;          // └╴ so that it is exactly over the plotting point
        dev->ipr[0]     = Ctx->InPxX;   /* Inches per raster; [0]=x, [1]=y */
        dev->ipr[1]     = Ctx->InPxY;
        dev->cra[0]     = 0.9*Ctx->FontSize/(Ctx->InPxX*72.0);       /* Character size in rasters; [0]=x, [1]=y */
        dev->cra[1]     = 1.2*Ctx->FontSize/(Ctx->InPxY*72.0);
        dev->gamma      = 1.;

        // Device capabilities
        dev->canClip            = TRUE;
        dev->canChangeGamma     = FALSE;
        dev->canHAdj            = 0;
        dev->canGenMouseDown    = FALSE;
        dev->canGenMouseMove    = FALSE;
        dev->canGenMouseUp      = FALSE;
        dev->canGenKeybd        = FALSE;
        dev->haveTransparency   = 2;        /* 1 = no, 2 = yes */
        dev->haveTransparentBg  = 2;        /* 1 = no, 2 = fully, 3 = semi */
        dev->haveRaster         = 2;        /* 1 = no, 2 = yes, 3 = except for missing values */
        dev->haveCapture        = 2;        /* 1 = no, 2 = yes */
        dev->haveLocator        = 1;        /* 1 = no, 2 = yes */
        dev->hasTextUTF8        = TRUE;
        dev->wantSymbolUTF8     = TRUE;
        dev->useRotatedTextInContour = FALSE;

        // Initial settings
        dev->startps    = Ctx->FontSize;
        dev->startcol   = R_RGBA(0,0,0,255);
        dev->startfill  = R_RGBA(255,255,255,255);
        dev->startlty   = LTY_SOLID;
        dev->startfont  = Ctx->FontFace;
        dev->startgamma = 1.;

        // Device specific
        dev->deviceSpecific = (void*)Ctx;

        // Display list
        dev->displayListOn  = TRUE;

        // Device functions
        dev->activate       = NULL;
        dev->circle         = (void*)TclRDeviceGL_Circle;
        dev->clip           = (void*)TclRDeviceGL_Clip; // Apparently called even if canClip is FALSE
        dev->close          = (void*)TclRDeviceGL_Close;
        dev->deactivate     = NULL;
        dev->locator        = NULL;
        dev->line           = (void*)TclRDeviceGL_Line;
        dev->metricInfo     = (void*)TclRDeviceGL_MetricInfo;
        dev->mode           = (void*)TclRDeviceGL_Mode;
        dev->newPage        = (void*)TclRDeviceGL_NewPage;
        dev->polygon        = (void*)TclRDeviceGL_Polygon;
        dev->polyline       = (void*)TclRDeviceGL_Polyline;
        dev->rect           = (void*)TclRDeviceGL_Rect;
        dev->path           = NULL;
        dev->raster         = (void*)TclRDeviceGL_Raster;
        dev->cap            = (void*)TclRDeviceGL_Cap;
        dev->size           = (void*)TclRDeviceGL_Size;
        dev->strWidth       = (void*)TclRDeviceGL_StrWidth;
        dev->text           = (void*)TclRDeviceGL_Text;
        dev->onExit         = NULL;
        dev->getEvent       = NULL;
        dev->newFrameConfirm= NULL;
        dev->textUTF8       = (void*)TclRDeviceGL_TextUTF8;
        dev->strWidthUTF8   = (void*)TclRDeviceGL_StrWidth;

        dev->eventHelper    = NULL;
        dev->holdflush      = NULL;
    }

    return dev;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceGL_Init>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Create a new RDevice and register it in the R world as the active device
 *
 * Parametres   :
 *  <Interp>    : The tcl interp to put error messages in case of error
 *  <Item>      : A reference to the canvas item for callbacks. Volontarily made as an opaque pointer.
 *  <TkWin>     : Tk Window associated to the canvas
 *  <W>         : Width of the device to create
 *  <H>         : Height of the device to create
 *
 * Retour       : A new RDevice initialized
 *
 * Remarque     : This is called by the Tk canvas item to create its associated RDevice in the R world
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void* TclRDeviceGL_Init(Tcl_Interp *Interp,void *Item,Tk_Window TkWin,Tk_Font Font,int W,int H) {
    pDevDesc    dev = NULL;
    pGEDevDesc  ge = NULL;
    TCtx        *ctx = NULL;
    TkFont      *font = (TkFont*)Font;   /*This is an ugly hack, but how else am I suppose to get the necessary font specs?*/
    int         screen = Tk_ScreenNumber(TkWin);

    // Make sure we have a slot for the device
    if( R_CheckDeviceAvailableBool() == FALSE ) {
        Tcl_AppendResult(Interp,"No device available",NULL);
        goto err;
    }

    // Allocate memory for context
    if( !(ctx=calloc(1,sizeof(*ctx))) ) {
        Tcl_AppendResult(Interp,"Could not allocate TclR Device context",NULL);
        goto err;
    }

    // Init the context
    ctx->PS         = NULL;
    ctx->Item       = Item;
    ctx->W          = W;
    ctx->H          = H;
    ctx->PxW        = 0;
    ctx->PxH        = 0;
    ctx->Display    = Tk_Display(TkWin);
    ctx->TkWin      = TkWin;
    ctx->FBuf       = 0;
    ctx->RBuf       = 0;
    ctx->Tess       = NULL;
    ctx->Buf        = NULL;
    ctx->BufSize    = 0;
    ctx->FontSize   = font->fa.size;
    ctx->FontFace   = (font->fa.slant==TK_FS_ITALIC)<<1|(font->fa.weight==TK_FW_BOLD);
    ctx->FontFamily = Tcl_NewStringObj(strncasecmp(font->fa.family,"itc ",4)?font->fa.family:font->fa.family+4,-1); Tcl_IncrRefCount(ctx->FontFamily);
    ctx->TkFont     = Font;
    ctx->Alias      = 1;
    ctx->InPxX      = ((double)(DisplayWidthMM(ctx->Display,screen))/(double)(DisplayWidth(ctx->Display,screen))) * MM2INCH;
    ctx->InPxY      = ((double)(DisplayHeightMM(ctx->Display,screen))/(double)(DisplayHeight(ctx->Display,screen))) * MM2INCH;

    // Allocate the framebuffer
    if( !TclRDeviceGL_ResizeFramebuffer(ctx,W,H,-1) ) {
        Tcl_AppendResult(Interp,"Could not create GL framebuffer",NULL);
        goto err;
    }

    // Go back to screen rendering
    glBindFramebuffer(GL_FRAMEBUFFER,0);

    // Allocate the tesselator
    if( !(ctx->Tess=gluNewTess()) ) {
        Tcl_AppendResult(Interp,"Unable to create TclR Device",NULL);
        goto err;
    }

    // Init the tesselator
    gluTessCallback(ctx->Tess,GLU_TESS_BEGIN,(_GLUfuncptr)glBegin);
    gluTessCallback(ctx->Tess,GLU_TESS_VERTEX,(_GLUfuncptr)glVertex2dv);
    gluTessCallback(ctx->Tess,GLU_TESS_END,(_GLUfuncptr)glEnd);
    gluTessCallback(ctx->Tess,GLU_TESS_COMBINE_DATA,(_GLUfuncptr)TclRDeviceGL_TessCombine);

    gluTessProperty(ctx->Tess,GLU_TESS_BOUNDARY_ONLY,GL_FALSE);
    gluTessProperty(ctx->Tess,GLU_TESS_WINDING_RULE,GLU_TESS_WINDING_ODD);

    // Allocate a new device
    if( !(dev=TclRDeviceGL_NewDev(ctx)) ) {
        Tcl_AppendResult(Interp,"Unable to create TclR Device",NULL);
        goto err;
    }

    // Create the associated graphic engine
    if( !(ge=GEcreateDevDesc(dev)) ) {
        Tcl_AppendResult(Interp,"Unable to create graphic engine for TclR Device",NULL);
        goto err;
    }

    // Initialise the graphic engine
    GEaddDevice2(ge,"TclRDevice");

    return (void*)ge;
err:
    // An error occured, free all the resources
    TclRDeviceGL_CtxFree(ctx);
    free(dev);
    return NULL;
}
