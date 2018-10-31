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

// Driver includes
#include "TclRDeviceX.h"
#include "tkCanvRDevice.h"

#define MM2INCH 0.0393701

typedef struct TCtx {
    void        *Item;      // RDeviceItem (needs to be passed to signal a redraw)
    int         W,H;        // Width and Height of the pixmap
    Display     *Display;   // Connection to the X server
    Tk_Window   TkWin;      // TkWindow
    Pixmap      Pixmap;     // Pixmap where we will draw all the R primitives
    GC          GC;         // X Graphic context
    XColor      *Col;       // Color currently in use
} TCtx;

// Point buffer

static __thread XPoint *XPOINT_BUF=NULL;
static __thread size_t  XPOINT_N=0;

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
            DBGPRINTF("\t[%d] [%.4f,%.4f]\n",i,X[i],Y[i]);
            XPOINT_BUF[i].x = (short)round(X[i]);
            XPOINT_BUF[i].y = (short)(Ctx->H - (int)round(Y[i]));
        }
    }

    return XPOINT_BUF;
}

// Canvas item sync functions

// Signals a kill from the tk side (when the canvas item is destroyed)
void TclRDeviceX_Destroy(void* GE) {
    if( GE ) {
        killDevice(ndevNumber(((pGEDevDesc)GE)->dev));
    }
}

// Redraw the scene for the device
void TclRDeviceX_Redraw(void *GE) {
    if( GE ) {
        // Replay the display list
        GEplayDisplayList(GE);
    }
}

void TclRDeviceX_Resize(void *GE,int W,int H) {
    if( GE ) {
        pDevDesc dev = ((pGEDevDesc)GE)->dev;
        TCtx *ctx = (TCtx*)dev->deviceSpecific;

        // Free and create a new pixmap if we need a bigger one in one or both dimension(s)
        if( ctx->W<W || ctx->H<H ) {
            if( ctx->Pixmap != None )
                Tk_FreePixmap(ctx->Display,ctx->Pixmap);
            if( (ctx->Pixmap=Tk_GetPixmap(ctx->Display,Tk_WindowId(ctx->TkWin),W,H,Tk_Depth(ctx->TkWin))) == None ) {
                // Let's hope this never happens
                TclRDeviceX_Destroy(GE);
                return;
            }
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

Pixmap TclRDeviceX_GetPixmap(void* GE) {
    if( GE ) {
        TCtx *ctx = (TCtx*)((pGEDevDesc)GE)->dev->deviceSpecific;
        return ctx->Pixmap;
    }
    return None;
}

// Signals a redraw to Tk
static void TclRDeviceX_MarkDirty(pDevDesc Dev) {
    RDeviceItem_SignalRedraw(((TCtx*)Dev->deviceSpecific)->Item);
}

// Helper functions

void TclRDeviceX_GCColor(TCtx *restrict Ctx,rcolor RCol) {
    // Set the XColor structure
    XColor col;
    col.red     = R_RED(RCol)<<8;//|0x77;
    col.green   = R_GREEN(RCol)<<8;//|0x77;
    col.blue    = R_BLUE(RCol)<<8;//|0x77;
    DBGPRINTF("RCol=%u (%u,%u,%u)\n",RCol,R_RED(RCol),R_GREEN(RCol),R_BLUE(RCol));

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
        DBGPRINTF("Foreground changed to %lu\n",xgc.foreground);
    }
}

void TclRDeviceX_GCLine(TCtx *restrict Ctx,const pGEcontext restrict GEC) {
    int lstyle=LineSolid,capstyle=CapRound,joinstyle=JoinRound;

    DBGPRINTF("lwd(%f) lty(%d) lend(%d) ljoin(%d)\n",GEC->lwd,GEC->lty,GEC->lend,GEC->ljoin);
    switch( GEC->lty ) {
        case 1: lstyle=LineSolid;       break;
        case 2: lstyle=LineOnOffDash;   break;
        case 6: lstyle=LineDoubleDash;  break;
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

/* Standard RDevice function implementations */

//static void (*activate)(const pDevDesc );
static void TclRDeviceX_Circle(double X,double Y,double R,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;
    int r=(int)round(R),x=(int)round(X)-r,y=ctx->H-(int)round(Y)+r,d=r*2;

    DBGPRINTF("Circle @[%.4f,%.4f] r=%.4f\n",X,Y,R);
    // Check if we need to fill the circle
    if( GEC->fill != NA_INTEGER ) {
        TclRDeviceX_GCColor(ctx,(rcolor)GEC->fill);
        XFillArc(ctx->Display,ctx->Pixmap,ctx->GC,x,y,d,d,0,360*60);
    }
    // Check if we need to draw the borders
    if( GEC->col!=NA_INTEGER && (GEC->fill==NA_INTEGER||GEC->fill!=GEC->col) ) {
        TclRDeviceX_GCLine(ctx,GEC);
        TclRDeviceX_GCColor(ctx,(rcolor)GEC->col);
        XDrawArc(ctx->Display,ctx->Pixmap,ctx->GC,x,y,d,d,0,360*60);
    }
}
static void TclRDeviceX_Clip(double X0,double X1,double Y0,double Y1,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;
    XRectangle clip;

    clip.x = (short)(X0<=X1 ? X0 : X1);
    clip.y = (short)(Y0<=Y1 ? Y0 : Y1);

    clip.width = (unsigned short)fabs(X1-X0);
    clip.height = (unsigned short)fabs(Y1-Y0);

    DBGPRINTF("Clip to [%.4f,%.4f] [%.4f,%.4f]\n",X0,Y0,X1,Y1);
    XSetClipRectangles(ctx->Display,ctx->GC,0,0,&clip,1,Unsorted);
}
static void TclRDeviceX_Free(pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;
    DBGPRINTF("Freeing RDevice\n");

    // Detach this device from the associated item
    RDeviceItem_DetachDevice(ctx->Item);

    // Free the X resources
    if( ctx->Pixmap != None )
        Tk_FreePixmap(ctx->Display,ctx->Pixmap);
    if( ctx->GC != None )
        XFreeGC(ctx->Display,ctx->GC);
    if( ctx->Col )
        Tk_FreeColor(ctx->Col);

    ctx->Display    = NULL;
    ctx->TkWin      = NULL;
    ctx->Pixmap     = None;
    ctx->GC         = None;
    ctx->Col        = NULL;

    // Free the xpoint buffer
    if( XPOINT_BUF ) {
        free(XPOINT_BUF);
        XPOINT_BUF = NULL;
        XPOINT_N = 0;
    }
}
//static void (*deactivate)(pDevDesc );
//static Rboolean (*locator)(double *x,double *y,pDevDesc Dev);
static void TclRDeviceX_Line(double X0,double Y0,double X1,double Y1,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    DBGPRINTF("Line [%.4f,%.4f] -> [%.4f,%.4f]\n",X0,Y0,X1,Y1);
    TclRDeviceX_GCLine(ctx,GEC);
    TclRDeviceX_GCColor(ctx,(rcolor)GEC->col);
    XDrawLine(ctx->Display,ctx->Pixmap,ctx->GC,(int)round(X0),ctx->H-(int)round(Y0),(int)round(X1),ctx->H-(int)round(Y1));

}
static void TclRDeviceX_MetricInfo(int C,const pGEcontext restrict GEC,double *Ascent,double *Descent,double *Width,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;
    Tk_FontMetrics fm;

    Tk_GetFontMetrics(RDeviceItem_GetFont(ctx->Item),&fm);
    DBGPRINTF("Font metrics queried ascent=%d descent=%d width=%d\n",fm.ascent,fm.descent,fm.linespace);

    *Ascent = fm.ascent;
    *Descent = fm.descent;
    *Width = fm.linespace;
}
static void TclRDeviceX_Mode(int Mode,pDevDesc Dev) {
    DBGPRINTF("Mode set to %d\n",Mode);
    // Device stopped drawing, signal a refresh
    if( Mode == 0 ) {
        TclRDeviceX_MarkDirty(Dev);
    }
}
static void TclRDeviceX_NewPage(const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    DBGPRINTF("CLEAR\n");
    // Reset clipping
    XSetClipMask(ctx->Display,ctx->GC,None);

    // Reset background
    TclRDeviceX_GCColor(ctx,R_TRANWHITE);
    XFillRectangle(ctx->Display,ctx->Pixmap,ctx->GC,0,0,ctx->W,ctx->H);
}
static void TclRDeviceX_Polygon(int N,double *X,double *Y,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx    *ctx = (TCtx*)Dev->deviceSpecific;
    XPoint  *xp;

    DBGPRINTF("Polygon (%d)\n",N);
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
static void TclRDeviceX_Polyline(int N,double *X,double *Y,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx    *ctx = (TCtx*)Dev->deviceSpecific;
    XPoint  *xp;

    DBGPRINTF("Polyline (%d)\n",N);
    if( N && (xp=ToXPoint(ctx,N,X,Y)) ) {
        // Draw the lines
        TclRDeviceX_GCLine(ctx,GEC);
        TclRDeviceX_GCColor(ctx,(rcolor)GEC->col);
        XDrawLines(ctx->Display,ctx->Pixmap,ctx->GC,xp,N,CoordModeOrigin);
    }
}
static void TclRDeviceX_Rect(double X0,double Y0,double X1,double Y1,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx       = (TCtx*)Dev->deviceSpecific;
    int             x=(int)round(X0),y=ctx->H-(int)round(Y0);
    unsigned int    w=(unsigned int)round(X1-X0),h=(unsigned int)round(Y1-Y0);

    DBGPRINTF("Rect [%.4f,%.4f] -> [%.4f,%.4f]\n",X0,Y0,X1,Y1);
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
//static void (*raster)(unsigned int *raster,int w,int h,double x,double y,double width,double height,double rot,Rboolean interpolate,const pGEcontext restrict GEC,pDevDesc Dev);
//static SEXP (*cap)(pDevDesc Dev);
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
static double TclRDeviceX_StrWidth(const char *Str,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    DBGPRINTF("StrWidth of (%s)(%d) is %d\n",Str,(int)strlen(Str),Tk_TextWidth(RDeviceItem_GetFont(ctx->Item),Str,strlen(Str)));
    return Tk_TextWidth(RDeviceItem_GetFont(ctx->Item),Str,strlen(Str));
}
static void TclRDeviceX_Text(double X,double Y,const char *Str,double Rot,double HAdj,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    DBGPRINTF("Text @[%.4f,%.4f] rotated[%.2f] hadj(%.4f) : (%s)\n",X,Y,Rot,HAdj,Str);
    //Tk_DrawChars(ctx->Display,ctx->Pixmap,ctx->GC,RDeviceItem_GetFont(ctx->Item),Str,strlen(Str),(int)round(X),ctx->H-(int)round(Y));
    TkDrawAngledChars(ctx->Display,ctx->Pixmap,ctx->GC,RDeviceItem_GetFont(ctx->Item),Str,strlen(Str),(int)round(X),ctx->H-(int)round(Y),Rot);
}
//static void (*onExit)(pDevDesc Dev);
//static SEXP (*getEvent)(SEXP,const char *);
//static Rboolean (*newFrameConfirm)(pDevDesc Dev);
//static void (*textUTF8)(double x,double y,const char *str,double rot,double hadj,const pGEcontext restrict GEC,pDevDesc Dev);
//static double (*strWidthUTF8)(const char *str,const pGEcontext restrict GEC,pDevDesc Dev);
//static void (*eventHelper)(pDevDesc Dev,int code);
//static int (*holdflush)(pDevDesc Dev,int level);

static DevDesc* TclRDeviceX_NewDev(TCtx *Ctx) {
    pDevDesc dev = calloc(1,sizeof(*dev));

    if( dev ) {
        //int screen;
        //double pxw,pxh;

        //screen = Tk_ScreenNumber(Ctx->TkWin);
        //pxw = ((double)(DisplayWidthMM(Ctx->Display,screen))/(double)(DisplayWidth(Ctx->Display,screen))) * MM2INCH;
        //pxh = ((double)(DisplayHeightMM(Ctx->Display,screen))/(double)(DisplayHeight(Ctx->Display,screen))) * MM2INCH;

        Tk_FontMetrics fm;
        Tk_GetFontMetrics(RDeviceItem_GetFont(Ctx->Item),&fm);
        DBGPRINTF("font : ascent=%d descent=%d width=%d\n",fm.ascent,fm.descent,fm.linespace);

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
        dev->cra[0]     = 10;       /* Character size in rasters; [0]=x, [1]=y */
        dev->cra[1]     = 10;
        //dev->gamma      = 1.;

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
        dev->haveRaster         = 1;        /* 1 = no, 2 = yes, 3 = except for missing values */
        dev->haveCapture        = 1;        /* 1 = no, 2 = yes */
        dev->haveLocator        = 1;        /* 1 = no, 2 = yes */
        dev->hasTextUTF8        = TRUE;
        dev->wantSymbolUTF8     = TRUE;
        dev->useRotatedTextInContour = FALSE;

        // Initial settings
        dev->startps    = fm.linespace;
        dev->startcol   = R_RGB(0,0,0);
        dev->startfill  = R_TRANWHITE;
        dev->startlty   = LTY_SOLID;
        dev->startfont  = 1;
        dev->startgamma = 1;

        // Device specific
        dev->deviceSpecific = (void*)Ctx;

        // Display list
        dev->displayListOn  = TRUE;

        // Device functions
        dev->activate       = NULL;
        dev->circle         = (void*)TclRDeviceX_Circle;
        dev->clip           = (void*)TclRDeviceX_Clip; // Apparently called even if canClip is FALSE
        dev->close          = (void*)TclRDeviceX_Free;
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
        dev->raster         = NULL;
        dev->cap            = NULL;
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

void* TclRDeviceX_Init(Tcl_Interp *Interp,void *Item,Tk_Window TkWin,int W,int H) {
    pDevDesc dev = NULL;
    pGEDevDesc ge = NULL;
    TCtx *ctx = NULL;

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
    ctx->Item       = Item;
    ctx->W          = W;
    ctx->H          = H;
    ctx->Display    = Tk_Display(TkWin);
    ctx->TkWin      = TkWin;
    ctx->Pixmap     = None;
    ctx->GC         = None;
    ctx->Col        = NULL;
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
    if( ctx ) {
        if( ctx->Pixmap != None )
            Tk_FreePixmap(ctx->Display,ctx->Pixmap);
        if( ctx->GC != None )
            XFreeGC(ctx->Display,ctx->GC);
        free(ctx);
    }
    free(dev);
    return NULL;
}
