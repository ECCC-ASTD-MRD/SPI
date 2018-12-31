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
#include "TclRDeviceX.h"
#include "tkCanvRDevice.h"

//#define XDBGPRINTF(...) printf(__VA_ARGS__);
#define XDBGPRINTF(...)

#define MM2INCH     0.0393701

typedef struct TCtx {
    Tcl_Obj     *PS;                // PostScript buffer (MUST be first to use the PS functionality)
    Tk_Window   TkWin;              // TkWindow (MUST be second for PS functionality)
    Display     *Display;           // Connection to the X server
    Pixmap      Pixmap;             // Pixmap where we will draw all the R primitives
    GC          GC;                 // X Graphic context
    XColor      *Col;               // Color currently in use
    XImage      *Img;               // Image used to show rasters
    void        *Item;              // RDeviceItem (needs to be passed to signal a redraw)
    int         W,H;                // Width and Height of the device
    int         PxW,PxH;            // Width and height of the underlying pixmap
    int         ImgSize;            // True size of the image buffer
    int         FontSize;           // Font Size currently in use
    int         FontFace;           // Font face currently in use
    Tcl_Obj     *FontFamily;        // Font family currently in use
    Tk_Font     TkFont;             // Current font
    double      InPxX,InPxY;        // Inch per Pixel conversion factor in X and Y
} TCtx;

// Point buffer

static __thread XPoint *XPOINT_BUF=NULL;
static __thread size_t  XPOINT_N=0;

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <ToXPoint>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Put the R given coords into an XPoint buffer
 *
 * Parametres   :
 *  <Ctx>       : Device context
 *  <N>         : Number of coordinates
 *  <X>         : Coordinates in X
 *  <Y>         : Coordinates in Y
 *
 * Retour       : The XPoint buffer
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static XPoint* ToXPoint(TCtx *restrict Ctx,int N,double *X,double *Y) {
    // Make sure the buffer has enough space
    if( XPOINT_N < N ) {
        if( XPOINT_BUF )
            free(XPOINT_BUF);
        XPOINT_BUF = malloc(N*sizeof(*XPOINT_BUF));
        XPOINT_N = N;
    }

    if( XPOINT_BUF ) {
        int i;

        // Convert the coodinates into the X world
        for(i=0; i<N; ++i) {
            XDBGPRINTF("\t[%d] [%.4f,%.4f]\n",i,X[i],Y[i]);
            XPOINT_BUF[i].x = (short)X[i];
            XPOINT_BUF[i].y = (short)(Ctx->H - (int)Y[i]);
        }
    }

    return XPOINT_BUF;
}



// Canvas item sync functions


/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Destroy>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
void TclRDeviceX_Destroy(void* GE) {
    if( GE ) {
        killDevice(ndevNumber(((pGEDevDesc)GE)->dev));
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Redraw>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
void TclRDeviceX_Redraw(void *GE) {
    if( GE ) {
        // Replay the display list
        GEplayDisplayList(GE);
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Resize>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
void TclRDeviceX_Resize(void *GE,int W,int H) {
    if( GE ) {
        pDevDesc dev = ((pGEDevDesc)GE)->dev;
        TCtx *ctx = (TCtx*)dev->deviceSpecific;

        // Free and create a new pixmap if we need a bigger one in one or both dimension(s)
        if( ctx->PxW<W || ctx->PxH<H ) {
            if( ctx->Pixmap != None )
                Tk_FreePixmap(ctx->Display,ctx->Pixmap);
            if( (ctx->Pixmap=Tk_GetPixmap(ctx->Display,Tk_WindowId(ctx->TkWin),W,H,Tk_Depth(ctx->TkWin))) == None ) {
                // Let's hope this never happens
                TclRDeviceX_Destroy(GE);
                return;
            }
            ctx->PxW = W;
            ctx->PxH = H;
        }

        // Only resize if we changed dimensions
        if( ctx->W!=W || ctx->H!=H ) {
            // Update the context
            ctx->W = W;
            ctx->H = H;

            // Update the device
            dev->size(NULL,NULL,NULL,NULL,dev);

            // Redraw
            TclRDeviceX_Redraw(GE);
        }
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_SetFont>
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
void TclRDeviceX_SetFont(void *GE,Tk_Font Font) {
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
 * Nom          : <TclRDeviceX_GetPixmap>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Return the pixmap associated with the canvas item's opaque pointer
 *
 * Parametres   :
 *  <GE>        : Graphics Engine description pointer
 *
 * Retour       : The pixmap associated with the given handle
 *
 * Remarque     : This function should only be called by the rdevice canvas item
 *
 *---------------------------------------------------------------------------------------------------------------
*/
Pixmap TclRDeviceX_GetPixmap(void* GE) {
    if( GE ) {
        TCtx *ctx = (TCtx*)((pGEDevDesc)GE)->dev->deviceSpecific;
        return ctx->Pixmap;
    }
    return None;
}


// Helper functions


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
static void TclRDeviceX_CtxFree(TCtx *Ctx) {
    if( Ctx ) {
        // Free the X resources
        if( Ctx->Pixmap != None )
            Tk_FreePixmap(Ctx->Display,Ctx->Pixmap);
        if( Ctx->GC != None )
            XFreeGC(Ctx->Display,Ctx->GC);
        if( Ctx->Col )
            Tk_FreeColor(Ctx->Col);
        if( Ctx->Img )
            XDestroyImage(Ctx->Img);

        // Free Tcl/Tk resources
        // We do NOT need to free the font, because that will be taken care of by the associated tk widget
        //if( Ctx->TkFont )
        //    Tk_FreeFont(Ctx->TkFont);
        if( Ctx->FontFamily )
            Tcl_DecrRefCount(Ctx->FontFamily);

        // Free the context
        free(Ctx);
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_GCColor>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_GCColor(TCtx *restrict Ctx,rcolor RCol) {
    XDBGPRINTF("RCol=%u (%u,%u,%u)\n",RCol,R_RED(RCol),R_GREEN(RCol),R_BLUE(RCol));

    // Set the XColor structure
    XColor col;
    col.red     = R_RED(RCol)<<8;//|0x77;
    col.green   = R_GREEN(RCol)<<8;//|0x77;
    col.blue    = R_BLUE(RCol)<<8;//|0x77;

    // Check if we need to change the color
    if( !Ctx->Col || Ctx->Col->red!=col.red || Ctx->Col->green!=col.green || Ctx->Col->blue!=col.blue ) {
        // Get the closest value to that color
        XColor* tkcol = Tk_GetColorByValue(Ctx->TkWin,&col);

        // Free the previous color that was in use up until now
        if( Ctx->Col )
            Tk_FreeColor(Ctx->Col);
        Ctx->Col = tkcol;

        // Change the foreground value of our GC
        XGCValues xgc;
        xgc.foreground = tkcol->pixel;
        XChangeGC(Ctx->Display,Ctx->GC,GCForeground,&xgc);
        XDBGPRINTF("Foreground changed to %lu\n",xgc.foreground);
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_GCLine>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_GCLine(TCtx *restrict Ctx,const pGEcontext restrict GEC) {
    int lstyle=LineSolid,capstyle=CapRound,joinstyle=JoinRound;

    XDBGPRINTF("lwd(%f) lty(%d) lend(%d) ljoin(%d)\n",GEC->lwd,GEC->lty,GEC->lend,GEC->ljoin);

    switch( GEC->lty ) {
        case LTY_SOLID:     lstyle=LineSolid;       break;
        case LTY_DASHED:    lstyle=LineOnOffDash;   break;
        case LTY_TWODASH:   lstyle=LineDoubleDash;  break;
    }

    switch( GEC->lend ) {
        case GE_ROUND_CAP:  capstyle=CapRound;      break;
        case GE_BUTT_CAP:   capstyle=CapButt;       break;
        case GE_SQUARE_CAP: capstyle=CapProjecting; break;
        //capstyle=CapNotLast;    break;
    }

    switch( GEC->ljoin ) {
        case GE_ROUND_JOIN: joinstyle=JoinRound;    break;
        case GE_MITRE_JOIN: joinstyle=JoinMiter;    break;
        case GE_BEVEL_JOIN: joinstyle=JoinBevel;    break;
    }

    XSetLineAttributes(Ctx->Display,Ctx->GC,(unsigned int)round(GEC->lwd),lstyle,capstyle,joinstyle);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_GCFont>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_GCFont(TCtx *restrict Ctx,const pGEcontext restrict GEC,pDevDesc Dev) {
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

        XDBGPRINTF("Font family(%s|%s) cex(%g) ps(%g) lineheight(%g) fontface(%d|%d) -- fontsize(%d|%d)\n",
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

            XSetFont(Ctx->Display,Ctx->GC,Tk_FontId(font));

            // Update the font on the item (the old font will be freed there)
            RDeviceItem_SetFont(Ctx->Item,Ctx->TkFont);
        } else {
            fprintf(stderr,"%s: Could not get font for params \"%s\"\n",__func__,Tcl_GetStringFromObj(lst,NULL));
        }
    }
}


// Standard RDevice function implementations


//static void (*activate)(const pDevDesc );

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Circle>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_Circle(double X,double Y,double R,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;
    int r=(int)round(R),x=(int)X-r,y=ctx->H-(int)Y-r,d=r*2;

    XDBGPRINTF("Circle @[%.4f,%.4f] r=%.4f\n",X,Y,R);
    // Check if we need to fill the circle
    if( GEC->fill != NA_INTEGER ) {
        TclRDeviceX_GCColor(ctx,(rcolor)GEC->fill);
        XFillArc(ctx->Display,ctx->Pixmap,ctx->GC,x,y,d,d,0,360*64);
    }
    // Check if we need to draw the borders
    if( GEC->col!=NA_INTEGER && (GEC->fill==NA_INTEGER||GEC->fill!=GEC->col) ) {
        TclRDeviceX_GCLine(ctx,GEC);
        TclRDeviceX_GCColor(ctx,(rcolor)GEC->col);
        XDrawArc(ctx->Display,ctx->Pixmap,ctx->GC,x,y,d,d,0,360*64);
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Clip>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_Clip(double X0,double X1,double Y0,double Y1,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;
    XRectangle clip;

    clip.width = (unsigned short)abs((int)X1-(int)X0)-1;
    clip.height = (unsigned short)abs((int)Y1-(int)Y0)-1;

    clip.x = (short)(X0<=X1 ? X0 : X1) + 1;
    clip.y = ctx->H - (short)(Y0<=Y1 ? Y0 : Y1) - clip.height;

    XDBGPRINTF("Clip to [%.4f,%.4f] [%.4f,%.4f]\n",X0,Y0,X1,Y1);
    XSetClipRectangles(ctx->Display,ctx->GC,0,0,&clip,1,Unsorted);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Close>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_Close(pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;
    XDBGPRINTF("Freeing RDevice\n");

    // Detach this device from the associated item
    RDeviceItem_DetachDevice(ctx->Item);

    // Free the xpoint buffer
    if( XPOINT_BUF ) {
        free(XPOINT_BUF);
        XPOINT_BUF = NULL;
        XPOINT_N = 0;
    }

    // Free the context
    TclRDeviceX_CtxFree(ctx);
    Dev->deviceSpecific = NULL;
}

//static void (*deactivate)(pDevDesc );
//static Rboolean (*locator)(double *x,double *y,pDevDesc Dev);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Line>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_Line(double X0,double Y0,double X1,double Y1,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    XDBGPRINTF("Line [%.4f,%.4f] -> [%.4f,%.4f]\n",X0,Y0,X1,Y1);
    TclRDeviceX_GCLine(ctx,GEC);
    TclRDeviceX_GCColor(ctx,(rcolor)GEC->col);
    XDrawLine(ctx->Display,ctx->Pixmap,ctx->GC,(int)X0,ctx->H-(int)Y0,(int)X1,ctx->H-(int)Y1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_MetricInfo>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
 *  device_MetricInfo should return height, depth, and width information for the given character in DEVICEi units.
 *
 *  Note: in an 8-bit locale, c is 'char'. In an mbcslocale, it is wchar_t, and at least some of code assumes
 *  that is UCS-2 (Windows, true) or UCS-4.
 *
 *  This is used for formatting mathematical expressions and for exact centering of text (see GText)
 *  If the device cannot provide metric information then it MUST return 0.0 for ascent, descent, and width.
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDeviceX_MetricInfo(int C,const pGEcontext restrict GEC,double *Ascent,double *Descent,double *Width,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;
    Tk_FontMetrics fm;
    int width,n;
    char str[TCL_UTF_MAX];

    TclRDeviceX_GCFont(ctx,GEC,Dev);

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
    XDBGPRINTF("Font metrics queried ascent=%d descent=%d linespace=%d width=%d (%.*s)\n",fm.ascent,fm.descent,fm.linespace,width,n,str);

    *Ascent = fm.ascent;
    *Descent = fm.descent;
    *Width = width;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Mode>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_Mode(int Mode,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    XDBGPRINTF("Mode set to %d\n",Mode);
    // Device stopped drawing, signal a refresh
    if( Mode == 0 ) {
        RDeviceItem_SignalRedraw(ctx->Item);
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_NewPage>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_NewPage(const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    XDBGPRINTF("CLEAR\n");

    // Reset clipping
    XSetClipMask(ctx->Display,ctx->GC,None);

    // Reset background
    TclRDeviceX_GCColor(ctx,(rcolor)GEC->fill);
    XFillRectangle(ctx->Display,ctx->Pixmap,ctx->GC,0,0,ctx->W,ctx->H);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Polygon>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_Polygon(int N,double *X,double *Y,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx    *ctx = (TCtx*)Dev->deviceSpecific;
    XPoint  *xp;

    XDBGPRINTF("Polygon (%d)\n",N);

    if( N && (xp=ToXPoint(ctx,N,X,Y)) ) {
        // Check if we need to fill the polygon
        if( GEC->fill != NA_INTEGER ) {
            TclRDeviceX_GCColor(ctx,(rcolor)GEC->fill);
            XFillPolygon(ctx->Display,ctx->Pixmap,ctx->GC,xp,N,Convex,CoordModeOrigin);
        }
        // Check if we need to draw the borders
        if( GEC->col!=NA_INTEGER && (GEC->fill==NA_INTEGER||GEC->fill!=GEC->col) ) {
            TclRDeviceX_GCLine(ctx,GEC);
            TclRDeviceX_GCColor(ctx,(rcolor)GEC->col);
            XDrawLines(ctx->Display,ctx->Pixmap,ctx->GC,xp,N,CoordModeOrigin);
        }
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Polyline>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_Polyline(int N,double *X,double *Y,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx    *ctx = (TCtx*)Dev->deviceSpecific;
    XPoint  *xp;

    XDBGPRINTF("Polyline (%d)\n",N);

    if( N && (xp=ToXPoint(ctx,N,X,Y)) ) {
        // Draw the lines
        TclRDeviceX_GCLine(ctx,GEC);
        TclRDeviceX_GCColor(ctx,(rcolor)GEC->col);
        XDrawLines(ctx->Display,ctx->Pixmap,ctx->GC,xp,N,CoordModeOrigin);
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Rect>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_Rect(double X0,double Y0,double X1,double Y1,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx    *ctx=(TCtx*)Dev->deviceSpecific;
    int     x,y,w,h;

    if( X1 < X0 ) {
        w = (int)X0 - (int)X1;
        x = (int)X1;
    } else {
        w = (int)X1 - (int)X0;
        x = (int)X0;
    }

    if( Y1 < Y0 ) {
        h = (int)Y0 - (int)Y1;
        y = (int)Y0;
    } else {
        h = (int)Y1 - (int)Y0;
        y = (int)Y1;
    }
    y = ctx->H - y;

    XDBGPRINTF("Rect [%.4f,%.4f] -> [%.4f,%.4f]\n",X0,Y0,X1,Y1);

    // Check if we need to fill the rectangle
    if( GEC->fill != NA_INTEGER ) {
        TclRDeviceX_GCColor(ctx,(rcolor)GEC->fill);
        XFillRectangle(ctx->Display,ctx->Pixmap,ctx->GC,x,y,w,h);
    }
    // Check if we need to draw the borders
    if( GEC->col!=NA_INTEGER && (GEC->fill==NA_INTEGER||GEC->fill!=GEC->col) ) {
        TclRDeviceX_GCLine(ctx,GEC);
        TclRDeviceX_GCColor(ctx,(rcolor)GEC->col);
        XDrawRectangle(ctx->Display,ctx->Pixmap,ctx->GC,x,y,w,h);
    }
}

//static void (*path)(double *x,double *y,int npoly,int *nper,Rboolean winding,const pGEcontext restrict GEC,pDevDesc Dev);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Raster>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_Raster(unsigned int *Raster,int W,int H,double X,double Y,double Width,double Height,double Rot,Rboolean Interp,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx    *ctx=(TCtx*)Dev->deviceSpecific;
    int     x,y,imgW,imgH,bufs,idx,rotW=0,rotH=0;
    double  angle = Rot*DEG2RAD;
    char    *data,swap;

    XDBGPRINTF("Raster [%.4f,%.4f]+[%.4f,%.4f] (Ori: %dx%d) Interp=%d Rot=%.4f\n",X,Y,Width,Height,W,H,Interp,Rot);

    // Make sure we have an image
    if( !ctx->Img ) {
        int depth;
        Visual *visual = Tk_GetVisual(NULL,ctx->TkWin,"default",&depth,NULL);

        if( depth < 24 ) {
            fprintf(stderr,"%s: Depth of %d unsupported, raster won't be drawn\n",__func__,depth);
            return;
        }

        // Initialize an XImage
        if( !(ctx->Img=XCreateImage(ctx->Display,visual,depth,ZPixmap,0,NULL,0,0,32,0)) ) {
            fprintf(stderr,"%s: Could not create XImage, raster won't be drawn\n",__func__);
            return;
        }
        ctx->ImgSize = 0;
    }

    // Get the width/height and coords of the image
    x       = (int)X;
    y       = (int)Y;
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

        x = (int)(X-(rotW-imgW)*0.5-rotX);
        y = (int)(Y-(rotH-imgH)*0.5-rotY);
    }

    // Make sure we have a big enough memory buffer
    if( ctx->ImgSize < bufs ) {
        // Free previously allocated memory
        if( ctx->Img->data )
            free(ctx->Img->data);

        // Allocate the data for the image (note : the data will be freed by XDestroyImage)
        ctx->ImgSize = bufs;
        if( !(ctx->Img->data=malloc(bufs*sizeof(*Raster))) ) {
            ctx->ImgSize = 0;
            fprintf(stderr,"%s: Could not create XImage buffer (%dx%dx4), raster won't be drawn\n",__func__,imgW,imgH);
            return;
        }
    }

    // Scale the image to the required size
    if( Interp ) {
        R_GE_rasterInterpolate(Raster,W,H,(unsigned int*)ctx->Img->data,imgW,imgH);
    } else {
        R_GE_rasterScale(Raster,W,H,(unsigned int*)ctx->Img->data,imgW,imgH);
    }

    // Make the image rotation
    if( Rot != 0.0 ) {
        data = ctx->Img->data + rotW*rotH*sizeof(*Raster);

        R_GE_rasterResizeForRotation((unsigned int*)ctx->Img->data,imgW,imgH,(unsigned int*)data,rotW,rotH,GEC);
        R_GE_rasterRotate((unsigned int*)data,rotW,rotH,angle,(unsigned int*)ctx->Img->data,GEC,FALSE);

        imgW = rotW;
        imgH = rotH;
    }

    // Set the image bytes in ARGB (msb->lsb) order (R passes it as ABGR (msb->lsb))
    if( ctx->Img->byte_order == LSBFirst ) {
        for(idx=imgW*imgH,data=ctx->Img->data; idx; --idx,data+=4) {
            // Bytes are RGBA, we need BGRA
            swap    = data[0];
            data[0] = data[2];
            data[2] = swap;
        }
    } else {
        for(idx=imgW*imgH,data=ctx->Img->data; idx; --idx,data+=4) {
            // Bytes are ABGR, we need ARGB
            swap    = data[1];
            data[1] = data[3];
            data[3] = swap;
        }
    }

    // Set the image dimensions
    ctx->Img->width             = imgW;
    ctx->Img->height            = imgH;
    ctx->Img->bytes_per_line    = imgW*ctx->Img->bitmap_unit/8;

    // Draw the image
    XPutImage(ctx->Display,ctx->Pixmap,ctx->GC,ctx->Img,0,0,x,ctx->H-y-imgH,imgW,imgH);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Cap>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static SEXP TclRDeviceX_Cap(pDevDesc Dev) {
    TCtx    *ctx=(TCtx*)Dev->deviceSpecific;
    SEXP    raster=R_NilValue;
    XImage  *img;

    if( (img=XGetImage(ctx->Display,ctx->Pixmap,0,0,ctx->W,ctx->H,AllPlanes,ZPixmap) ) ) {
        char    *data,*rdata;
        int     size=img->width*img->height;
        SEXP    rdim;

        // Allocate the memory in R for the raster
        PROTECT( raster=allocVector(INTSXP,size) );

        // Set the image bytes in ABGR (msb->lsb) order (Xlib passes it as ARGB (msb->lsb))
        if( img->byte_order == LSBFirst ) {
            for(rdata=(char*)INTEGER(raster),data=img->data; size; --size,data+=4,rdata+=4) {
                // Bytes are BGRA, we need RGBA
                rdata[0] = data[2];
                rdata[1] = data[1];
                rdata[2] = data[0];
                rdata[3] = data[3];
            }
        } else {
            for(rdata=(char*)INTEGER(raster),data=img->data; size; --size,data+=4,rdata+=4) {
                // Bytes are ARGB, we need ABGR
                rdata[0] = data[0];
                rdata[1] = data[3];
                rdata[2] = data[2];
                rdata[3] = data[1];
            }
        }

        // Add the dimensions (nrow,ncol) as attribute (create an R matrix)
        PROTECT( rdim=allocVector(INTSXP,2) );
        INTEGER(rdim)[0] = img->height;
        INTEGER(rdim)[1] = img->width;
        Rf_setAttrib(raster,R_DimSymbol,rdim);

        // Free everything
        UNPROTECT(2);
        XDestroyImage(img);
    } else {
        fprintf(stderr,"%s: Could not get XImage for raster creation\n",__func__);
    }

    return raster;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Size>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_Size(double *Left,double *Right,double *Bottom,double *Top,pDevDesc Dev) {
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
 * Nom          : <TclRDeviceX_StrWidth>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static double TclRDeviceX_StrWidth(const char *Str,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    TclRDeviceX_GCFont(ctx,GEC,Dev);
    XDBGPRINTF("StrWidth of (%s)(%d) is %d\n",Str,(int)strlen(Str),Tk_TextWidth(ctx->TkFont,Str,strlen(Str)));
    return Tk_TextWidth(ctx->TkFont,Str,strlen(Str));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Text>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static void TclRDeviceX_Text(double X,double Y,const char *Str,double Rot,double HAdj,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    XDBGPRINTF("Text @[%.4f,%.4f] rotated[%.2f] hadj(%.4f) : (%s)\n",X,Y,Rot,HAdj,Str);
    TclRDeviceX_GCColor(ctx,(rcolor)GEC->col);
    TclRDeviceX_GCFont(ctx,GEC,Dev);
    TkDrawAngledChars(ctx->Display,ctx->Pixmap,ctx->GC,ctx->TkFont,Str,strlen(Str),(int)X,ctx->H-(int)Y,Rot);
}

//static void (*onExit)(pDevDesc Dev);
//static SEXP (*getEvent)(SEXP,const char *);
//static Rboolean (*newFrameConfirm)(pDevDesc Dev);
//static void (*textUTF8)(double x,double y,const char *str,double rot,double hadj,const pGEcontext restrict GEC,pDevDesc Dev);
//static double (*strWidthUTF8)(const char *str,const pGEcontext restrict GEC,pDevDesc Dev);
//static void (*eventHelper)(pDevDesc Dev,int code);
//static int (*holdflush)(pDevDesc Dev,int level);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_NewDev>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
static DevDesc* TclRDeviceX_NewDev(TCtx *Ctx) {
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
        dev->xCharOffset= 0.4900;
        dev->yCharOffset= 0.3333;
        dev->yLineBias  = 0.1;
        dev->ipr[0]     = 1.0/72.0; /* Inches per raster; [0]=x, [1]=y */
        dev->ipr[1]     = 1.0/72.0;
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
        dev->haveTransparency   = 1;        /* 1 = no, 2 = yes */
        dev->haveTransparentBg  = 1;        /* 1 = no, 2 = fully, 3 = semi */
        dev->haveRaster         = 2;        /* 1 = no, 2 = yes, 3 = except for missing values */
        dev->haveCapture        = 2;        /* 1 = no, 2 = yes */
        dev->haveLocator        = 1;        /* 1 = no, 2 = yes */
        dev->hasTextUTF8        = TRUE;
        dev->wantSymbolUTF8     = TRUE;
        dev->useRotatedTextInContour = FALSE;

        // Initial settings
        dev->startps    = Ctx->FontSize;
        dev->startcol   = R_RGB(0,0,0);
        dev->startfill  = R_TRANWHITE;
        dev->startlty   = LTY_SOLID;
        dev->startfont  = Ctx->FontFace;
        dev->startgamma = 1.;

        // Device specific
        dev->deviceSpecific = (void*)Ctx;

        // Display list
        dev->displayListOn  = TRUE;

        // Device functions
        dev->activate       = NULL;
        dev->circle         = (void*)TclRDeviceX_Circle;
        dev->clip           = (void*)TclRDeviceX_Clip; // Apparently called even if canClip is FALSE
        dev->close          = (void*)TclRDeviceX_Close;
        dev->deactivate     = NULL;
        dev->locator        = NULL;
        dev->line           = (void*)TclRDeviceX_Line;
        dev->metricInfo     = (void*)TclRDeviceX_MetricInfo;
        dev->mode           = (void*)TclRDeviceX_Mode;
        dev->newPage        = (void*)TclRDeviceX_NewPage;
        dev->polygon        = (void*)TclRDeviceX_Polygon;
        dev->polyline       = (void*)TclRDeviceX_Polyline;
        dev->rect           = (void*)TclRDeviceX_Rect;
        dev->path           = NULL;
        dev->raster         = (void*)TclRDeviceX_Raster;
        dev->cap            = (void*)TclRDeviceX_Cap;
        dev->size           = (void*)TclRDeviceX_Size;
        dev->strWidth       = (void*)TclRDeviceX_StrWidth;
        dev->text           = (void*)TclRDeviceX_Text;
        dev->onExit         = NULL;
        dev->textUTF8       = (void*)TclRDeviceX_Text;
        dev->strWidthUTF8   = (void*)TclRDeviceX_StrWidth;

        dev->eventHelper    = NULL;
        dev->holdflush      = NULL;
    }

    return dev;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDeviceX_Init>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
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
void* TclRDeviceX_Init(Tcl_Interp *Interp,void *Item,Tk_Window TkWin,Tk_Font Font,int W,int H) {
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
    ctx->PxW        = W;
    ctx->PxH        = H;
    ctx->Display    = Tk_Display(TkWin);
    ctx->TkWin      = TkWin;
    ctx->Pixmap     = None;
    ctx->GC         = None;
    ctx->Col        = NULL;
    ctx->Img        = NULL;
    ctx->ImgSize    = 0;
    ctx->FontSize   = font->fa.size;
    ctx->FontFace   = (font->fa.slant==TK_FS_ITALIC)<<1|(font->fa.weight==TK_FW_BOLD);
    ctx->FontFamily = Tcl_NewStringObj(strncasecmp(font->fa.family,"itc ",4)?font->fa.family:font->fa.family+4,-1); Tcl_IncrRefCount(ctx->FontFamily);
    ctx->FontFace   = font->fa.weight==TK_FW_NORMAL ? font->fa.slant==TK_FS_ROMAN?1:3 : font->fa.slant==TK_FS_ROMAN?2:4;  /*This has to be the acme of ugly hacks*/
    ctx->TkFont     = Font;
    ctx->InPxX      = ((double)(DisplayWidthMM(ctx->Display,screen))/(double)(DisplayWidth(ctx->Display,screen))) * MM2INCH;
    ctx->InPxY      = ((double)(DisplayHeightMM(ctx->Display,screen))/(double)(DisplayHeight(ctx->Display,screen))) * MM2INCH;
    if( (ctx->Pixmap=Tk_GetPixmap(ctx->Display,Tk_WindowId(TkWin),W,H,Tk_Depth(TkWin))) == None ) {
        Tcl_AppendResult(Interp,"Could not create pixmap",NULL);
        goto err;
    }
    if( (ctx->GC=XCreateGC(ctx->Display,ctx->Pixmap,0x0,NULL)) == None ) {
        Tcl_AppendResult(Interp,"Could not create GC",NULL);
        goto err;
    }

    // Clear the pixmap
    TclRDeviceX_GCColor(ctx,R_TRANWHITE);
    XFillRectangle(ctx->Display,ctx->Pixmap,ctx->GC,0,0,W,H);

    // Allocate a new device
    if( !(dev=TclRDeviceX_NewDev(ctx)) ) {
        Tcl_AppendResult(Interp,"Unable to create TclR Device",NULL);
        goto err;
    }

    // Create the associated graphic engine
    //gsetVar(install(".Device"),mkString("TclRDevice"),R_NilValue);
    if( !(ge=GEcreateDevDesc(dev)) ) {
        Tcl_AppendResult(Interp,"Unable to create graphic engine for TclR Device",NULL);
        goto err;
    }

    // Initialise the graphic engine
    GEaddDevice(ge);
    GEinitDisplayList(ge);

    return (void*)ge;
err:
    // An error occured, free all the resources
    TclRDeviceX_CtxFree(ctx);
    free(dev);
    return NULL;
}
