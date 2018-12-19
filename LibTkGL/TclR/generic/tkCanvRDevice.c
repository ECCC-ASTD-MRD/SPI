/*
 * tkCanvRDevice.c --
 *
 *    This file implements RDevice items for canvas widgets.
 *
 */

#include "tk.h"
#include "tkInt.h"
#include "tkCanvas.h"
#include "TclRDeviceX.h"
#include "TclRDeviceGL.h"
#include "TclRDevice2PS.h"
#include "tkCanvRDevice.h"
#include "glStuff.h"
#include <math.h>

// For dynamic linking
#include <dlfcn.h>

//#define DBGPRINTF(...) printf(__VA_ARGS__)
#define DBGPRINTF(...)

#define IS_GLDEV(x) (((Tk_Item*)x)->typePtr == &tkglRDeviceType)
#define IS_XDEV(x)  (((Tk_Item*)x)->typePtr == &tkRDeviceType)

/*
 * The structure below defines the record for each RDevice item.
 */


typedef struct RDeviceItem  {
    Tk_Item     Header;     // Generic stuff that's the same for all types. MUST BE FIRST IN STRUCTURE
    void        *RDev;      // R Device (Volontarily opaque type)
    Tk_Canvas   Canv;       // Canvas containing the item
    Tk_Font     Font;       // Font for any text in the device

    void        (*destroy)(void *RDev);                                         // Destroy function for the device
    void        (*redraw)(void *RDev);                                          // Redraw function of the device
    void        (*resize)(void *RDev,int W,int H);                              // Resize function of the device
    void        (*tkredraw)(Tk_Canvas canvas,int x1,int y1,int x2,int y2);      // Signals to Tk that a redraw is needed
} RDeviceItem;

/*
 * Information used for parsing configuration specs:
 */

static const Tk_CustomOption tagsOption = {
    Tk_CanvasTagsParseProc, Tk_CanvasTagsPrintProc, NULL
};

static const Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_CUSTOM,  "-tags",    NULL,NULL,NULL,             0,                              TK_CONFIG_NULL_OK,  &tagsOption},
    {TK_CONFIG_FONT,    "-font",    NULL,NULL,"TkDefaultFont",  Tk_Offset(RDeviceItem,Font),    0,                  NULL},
    {TK_CONFIG_END,     NULL,       NULL,NULL,NULL,             0,                              0,                  NULL}
};

/*
 * Prototypes for functions defined in this file:
 */

static int      RDeviceCoords(Tcl_Interp *Interp,Tk_Canvas Canv,Tk_Item *ItemPtr,int Objc,Tcl_Obj *const Objv[]);
static int      RDeviceAreaOverlap(Tk_Canvas Canv,Tk_Item *ItemPtr,double *Rect);
static double   RDeviceDistToPoint(Tk_Canvas Canv,Tk_Item *ItemPtr,double *CoordPtr);
static int      RDeviceToPostscript(Tcl_Interp *Interp,Tk_Canvas Canv,Tk_Item *ItemPtr,int Prepass);
static int      RDeviceConfigure(Tcl_Interp *Interp,Tk_Canvas Canv,Tk_Item *ItemPtr,int Objc,Tcl_Obj *const Objv[],int Flags);
static int      TkcCreateRDevice(Tcl_Interp *Interp,Tk_Canvas Canv,struct Tk_Item *ItemPtr,int Objc,Tcl_Obj *const Objv[]);
static void     RDeviceDelete(Tk_Canvas Canv,Tk_Item *ItemPtr,Display *Display);
static void     RDeviceDisplay(Tk_Canvas Canv,Tk_Item *ItemPtr,Display *Display,Drawable D,int X,int Y,int Width,int Height);
static void     RDeviceDisplayGL(Tk_Canvas Canv,Tk_Item *ItemPtr,Display *Display,Drawable D,int X,int Y,int Width,int Height);
static void     RDeviceScale(Tk_Canvas Canv,Tk_Item *ItemPtr,double OriginX,double OriginY,double ScaleX,double ScaleY);
static void     RDeviceTranslate(Tk_Canvas Canv,Tk_Item *ItemPtr,double DeltaX,double DeltaY);

/*
 * The structures below defines the bitmap item type in terms of functions
 * that can be invoked by generic item code.
 */

Tk_ItemType tkRDeviceType = {
    "rdevice",              /* name */
    sizeof(RDeviceItem),    /* itemSize */
    TkcCreateRDevice,       /* createProc */
    configSpecs,            /* configSpecs */
    RDeviceConfigure,       /* configureProc */
    RDeviceCoords,          /* coordProc */
    RDeviceDelete,          /* deleteProc */
    RDeviceDisplay,         /* displayProc */
    TK_CONFIG_OBJS,         /* flags */
    RDeviceDistToPoint,     /* pointProc */
    RDeviceAreaOverlap,     /* areaProc */
    RDeviceToPostscript,    /* postscriptProc */
    RDeviceScale,           /* scaleProc */
    RDeviceTranslate,       /* translateProc */
    NULL,                   /* indexProc */
    NULL,                   /* icursorProc */
    NULL,                   /* selectionProc */
    NULL,                   /* insertProc */
    NULL,                   /* dTextProc */
    NULL,                   /* nextPtr */
    NULL, 0, NULL, NULL
};


Tk_ItemType tkglRDeviceType = {
    "rdevice",              /* name */
    sizeof(RDeviceItem),    /* itemSize */
    TkcCreateRDevice,       /* createProc */
    configSpecs,            /* configSpecs */
    RDeviceConfigure,       /* configureProc */
    RDeviceCoords,          /* coordProc */
    RDeviceDelete,          /* deleteProc */
    RDeviceDisplayGL,       /* displayProc */
    TK_CONFIG_OBJS,         /* flags */
    RDeviceDistToPoint,     /* pointProc */
    RDeviceAreaOverlap,     /* areaProc */
    RDeviceToPostscript,    /* postscriptProc */
    RDeviceScale,           /* scaleProc */
    RDeviceTranslate,       /* translateProc */
    NULL,                   /* indexProc */
    NULL,                   /* icursorProc */
    NULL,                   /* selectionProc */
    NULL,                   /* insertProc */
    NULL,                   /* dTextProc */
    NULL,                   /* nextPtr */
    NULL, 0, NULL, NULL
};

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TkcCreateRDevice>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : This function is invoked to create a new rdevice item in a canvas
 *
 * Parametres   :
 *  <Interp>    : Interpreter for error reporting
 *  <Canv>      : Canvas to hold new item
 *  <ItemPtr>   : Record to hold new item; header has been initialized by caller
 *  <Objc>      : Number of arguments in objv
 *  <Objv>      : Arguments describing rectangle
 *
 * Retour       : A standard Tcl return value. If an error occurred in creating the
 *                item, then an error message is left in the interp's result; in this
 *                case itemPtr is left uninitialized, so it can be safely freed by the
 *                caller.
 *
 * Side effects : A new rdevice item is created.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int TkcCreateRDevice(Tcl_Interp *Interp,Tk_Canvas Canv,Tk_Item *ItemPtr,int Objc,Tcl_Obj *const Objv[]) {
    RDeviceItem *rdv = (RDeviceItem*)ItemPtr;
    Tk_Window   win = Tk_CanvasTkwin(Canv);
    const char  *arg;
    int         i;

    if (Objc == 0) {
        Tcl_Panic("canvas did not pass any coords");
    }

    // Initialize item's record.
    rdv->Canv   = Canv;
    rdv->RDev   = NULL;
    rdv->Font   = NULL;
    if( IS_XDEV(ItemPtr) ) {
        rdv->destroy    = TclRDeviceX_Destroy;
        rdv->redraw     = TclRDeviceX_Redraw;
        rdv->resize     = TclRDeviceX_Resize;
        rdv->tkredraw   = Tk_CanvasEventuallyRedraw;
    } else if( IS_GLDEV(ItemPtr) ) {
        rdv->destroy    = TclRDeviceGL_Destroy;
        rdv->redraw     = TclRDeviceGL_Redraw;
        rdv->resize     = TclRDeviceGL_Resize;
        rdv->tkredraw   = dlsym(NULL,"Tk_glCanvasEventuallyRedraw");
    } else {
        rdv->destroy    = NULL;
        rdv->redraw     = NULL;
        rdv->resize     = NULL;
        rdv->tkredraw   = NULL;
    }

    // Check where configure argument starts
    for(i=0; i<Objc; ++i) {
        arg = Tcl_GetString(Objv[i]);
        if ((arg[0] == '-') && (arg[1] >= 'a') && (arg[1] <= 'z')) {
            break;
        }
    }
    // Process coords
    if( !i || RDeviceCoords(Interp,Canv,ItemPtr,i,Objv)!=TCL_OK ) {
        goto error;
    }

    // Process rest of arguments
    if( RDeviceConfigure(Interp,Canv,ItemPtr,Objc-i,Objv+i,0) != TCL_OK ) {
        goto error;
    }

    // Make sure the XWindow exists (needed for the RDevice) instead of having to call "update idletasks" in the tcl code
    Tk_MakeWindowExist(win);

    // Create the device in R
    if( IS_XDEV(ItemPtr) ) {
        if( !(rdv->RDev=TclRDeviceX_Init(Interp,(void*)rdv,win,rdv->Font,rdv->Header.x2-rdv->Header.x1,rdv->Header.y2-rdv->Header.y1)) ) {
            goto error;
        }
    } else if( IS_GLDEV(ItemPtr) ) {
        if( !(rdv->RDev=TclRDeviceGL_Init(Interp,(void*)rdv,win,rdv->Font,rdv->Header.x2-rdv->Header.x1,rdv->Header.y2-rdv->Header.y1)) ) {
            goto error;
        }
    }

    return TCL_OK;
error:
    RDeviceDelete(Canv,ItemPtr,Tk_Display(win));
    return TCL_ERROR;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <RDeviceCoords>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Process the "coords" widget command on rdevice items.
 *
 * Parametres   :
 *  <Interp>    : Interpreter for error reporting
 *  <Canv>      : Canvas containing item
 *  <ItemPtr>   : Item to process
 *  <Objc>      : Number of arguments in Objv
 *  <Objv>      : Arguments describing rectangle
 *
 * Retour       : A standard Tcl return value.
 *
 * Side effects : The coordinates for the given item may be changed.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int RDeviceCoords(Tcl_Interp *Interp,Tk_Canvas Canv,Tk_Item *ItemPtr,int Objc,Tcl_Obj *const Objv[]) {
    RDeviceItem *rdv = (RDeviceItem*)ItemPtr;
    double d;
    int i,c[4];

    switch( Objc ) {
        case 0:
            {
                // We need to return the coordinates of the item
                Tcl_Obj *obj[4];
                obj[0] = Tcl_NewDoubleObj(rdv->Header.x1);
                obj[1] = Tcl_NewDoubleObj(rdv->Header.y1);
                obj[2] = Tcl_NewDoubleObj(rdv->Header.x2);
                obj[3] = Tcl_NewDoubleObj(rdv->Header.y2);

                Tcl_SetObjResult(Interp,Tcl_NewListObj(4,obj));
                return TCL_OK;
            }
        case 1:
            // We should have a list of exactly four coords
            if( Tcl_ListObjGetElements(Interp,Objv[0],&Objc,(Tcl_Obj***)&Objv)!=TCL_OK ) {
                return TCL_ERROR;
            } else if( Objc != 4 ) {
                Tcl_SetObjResult(Interp,Tcl_ObjPrintf("wrong # coordinates: expected 4, got %d", Objc));
                Tcl_SetErrorCode(Interp,"TK","CANVAS","COORDS","RDEVICE",NULL);
                return TCL_ERROR;
            }
            break;
        case 4:
            // Our two coords should be in the actual Objv
            break;
        default:
            Tcl_SetObjResult(Interp,Tcl_ObjPrintf("wrong # coordinates: expected 0 or 4, got %d", Objc));
            Tcl_SetErrorCode(Interp,"TK","CANVAS","COORDS","RDEVICE",NULL);
            return TCL_ERROR;
    }

    // Get the coordinates from the tcl objects
    for(i=0; i<4; ++i) {
        if( Tk_CanvasGetCoordFromObj(Interp,Canv,Objv[i],&d) != TCL_OK )
            return TCL_ERROR;
        c[i] = (int)(d+(d>=0.0?0.5:-0.5));
    }

    rdv->Header.x1 = c[0]<=c[2] ? c[0] : c[2];
    rdv->Header.y1 = c[1]<=c[3] ? c[1] : c[3];
    rdv->Header.x2 = c[2]>=c[0] ? c[2] : c[0];
    rdv->Header.y2 = c[3]>=c[1] ? c[3] : c[1];;

    // Update the device since we resized it
    DBGPRINTF("Resize to %d %d\n",rdv->Header.x2-rdv->Header.x1,rdv->Header.y2-rdv->Header.y1);
    if( rdv->resize )
        rdv->resize(rdv->RDev,rdv->Header.x2-rdv->Header.x1,rdv->Header.y2-rdv->Header.y1);

    return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <RDeviceConfigure>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Invoked to configure various aspects of an rdevice
 *
 * Parametres   :
 *  <Interp>    : Interpreter for error reporting
 *  <Canv>      : Canvas containing rdevice
 *  <ItemPtr>   : rdevice to configure
 *  <Objc>      : Number of arguments in Objv
 *  <Objv>      : Arguments describing changes
 *  <Flags>     : Flags to pass to Tk_ConfigureWidget
 *
 * Retour       : A standard Tcl return value. If an error occurred
 *                then an error message is left in the interp's result
 *
 * Side effects : Configuration information may be changed for the item
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int RDeviceConfigure(Tcl_Interp *Interp,Tk_Canvas Canv,Tk_Item *ItemPtr,int Objc,Tcl_Obj *const Objv[],int Flags) {
    RDeviceItem *rdv = (RDeviceItem *)ItemPtr;

    // Process the option and set the internal structure consequently
    if (TCL_OK != Tk_ConfigureWidget(Interp,Tk_CanvasTkwin(Canv),configSpecs,Objc,(const char**)Objv,(char*)rdv,Flags|TK_CONFIG_OBJS)) {
        return TCL_ERROR;
    }

    return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <RDeviceDelete>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Clean up the data structure associated with an rdevice
 *
 * Parametres   :
 *  <Canv>      : Canvas containing rdevice
 *  <ItemPtr>   : rdevice to delete
 *  <Display>   : Display containing window for canvas
 *
 * Retour       :
 *
 * Side effects : Resources associated with itemPtr are released.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static void RDeviceDelete(Tk_Canvas Canv,Tk_Item *ItemPtr,Display *Display) {
    RDeviceItem *rdv = (RDeviceItem*)ItemPtr;

    // Free the device
    if( rdv->destroy )
        rdv->destroy(rdv->RDev);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <RDeviceDisplay>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Copy the pixmap curated by the RDevice into the displayed canvas's drawable
 *
 * Parametres   :
 *  <Canv>      : Canvas containing rdevice
 *  <ItemPtr>   : rdevice to display
 *  <Display>   : Display on which to draw item
 *  <Drawable>  : Pixmap or window in which to draw item
 *  <X>         : Describes region of canvas that must be redisplayed
 *  <Y>         : Describes region of canvas that must be redisplayed
 *  <Width>     : Describes region of canvas that must be redisplayed
 *  <Height>    : Describes region of canvas that must be redisplayed
 *
 * Retour       :
 *
 * Side effects : ItemPtr is drawn in Drawable using the transformation information in Canvas.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static void RDeviceDisplay(Tk_Canvas Canv,Tk_Item *ItemPtr,Display *Display,Drawable Drawable,int X,int Y,int Width,int Height) {
    RDeviceItem     *rdv = (RDeviceItem*)ItemPtr;
    unsigned int    w,h;
    int             x,y;
    short           drawX,drawY;
    Pixmap          pixmap;

    // If the area being displayed doesn't cover the whole pixmap, then only
    // redisplay the part of the pixmap that needs redisplay.
    if( (pixmap=TclRDeviceX_GetPixmap(rdv->RDev)) != None ) {
        // Find the x coordinate and the width of the region to copy relative to our pixmap coordinates
        if( X > rdv->Header.x1 ) {
            x = X - rdv->Header.x1;
            w = rdv->Header.x2 - X;
        } else {
            x = 0;
            w = (X+Width)<rdv->Header.x2 ? X+Width-rdv->Header.x1 : rdv->Header.x2-rdv->Header.x1;
        }
        // Find the y coordinate and the height of the region to copy relative to our pixmap coordinates
        if( Y > rdv->Header.y1 ) {
            y = Y - rdv->Header.y1;
            h = rdv->Header.y2 - Y;
        } else {
            y = 0;
            h = (Y+Height)<rdv->Header.y2 ? Y+Height-rdv->Header.y1 : rdv->Header.y2-rdv->Header.y1;
        }

        // Get those coordinates in the portion of the canvas actually shown
        Tk_CanvasDrawableCoords(Canv,rdv->Header.x1+x,rdv->Header.y1+y,&drawX,&drawY);
        DBGPRINTF("Drawable coords : %hd %hd\n",drawX,drawY);
        DBGPRINTF("x(%d) y(%d) w(%u) h(%u)\n",x,y,w,h);

        // Copy our pixmap (filled by the RDevice) to the canvas
        XCopyArea(Display,pixmap,Drawable,Canvas(Canv)->pixmapGC,x,y,w,h,drawX,drawY);
    }
}

static void RDeviceDisplayGL(Tk_Canvas Canv,Tk_Item *ItemPtr,Display *Display,Drawable Drawable,int X,int Y,int Width,int Height) {
    RDeviceItem     *rdv = (RDeviceItem*)ItemPtr;
    int             x,y,w,h;
    short           drawX,drawY;
    GLuint          fbuf;

    // Find the x coordinate and the width of the region to copy relative to our framebuffer coordinates
    x = X>rdv->Header.x1 ? X-rdv->Header.x1 : 0;
    w = (X+Width<rdv->Header.x2?X+Width:rdv->Header.x2) - rdv->Header.x1 - x;
    // Find the y coordinate and the height of the region to copy relative to our framebuffer coordinates
    y = Y+Height>rdv->Header.y2 ? 0 : rdv->Header.y2-(Y+Height);
    h = rdv->Header.y2 - (Y>rdv->Header.y1?Y:rdv->Header.y1) - y;

    if( w<=0 || h<=0 )
        return;

    // Get those coordinates in the portion of the canvas actually shown
    Tk_CanvasDrawableCoords(Canv,rdv->Header.x1+x,rdv->Header.y1+h,&drawX,&drawY);
    DBGPRINTF("Drawable coords : %hd %hd (Canvas dim : %d %d)\n",drawX,drawY,Tk_Width(Canv),Tk_Height(Canv));
    DBGPRINTF("x(%d) y(%d) w(%d) h(%d)\n",x,y,w,h);

    // Blit (copy) the RDevice's framebuffer into the screen framebuffer
    if( (fbuf=TclRDeviceGL_GetFramebuffer(rdv->RDev)) ) {
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER,0);
        glBindFramebuffer(GL_READ_FRAMEBUFFER,fbuf);
        glReadBuffer(GL_COLOR_ATTACHMENT0);
        glDrawBuffer(GL_BACK);

        drawY = (short)Tk_Height(Canv)-drawY;
        glBlitFramebuffer(x,y,w,h,drawX,drawY,drawX+w,drawY+h,GL_COLOR_BUFFER_BIT,GL_NEAREST);

        glBindFramebuffer(GL_READ_FRAMEBUFFER,0);
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <RDeviceDistToPoint>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Computes the distance from a given point to the item
 *
 * Parametres   :
 *  <Canv>      : Canvas containing rdevice
 *  <ItemPtr>   : rdevice to display
 *  <P>         : Point (x,y)
 *
 * Retour       : 0 if the point is inside the item, the distance from the point to the item else.
 *
 * Side effects :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static double RDeviceDistToPoint(Tk_Canvas Canv,Tk_Item *ItemPtr,double *P) {
    RDeviceItem *rdv = (RDeviceItem*)ItemPtr;

    return hypot(
        fmin(rdv->Header.x2,P[0])-fmax(rdv->Header.x1,P[0]),
        fmin(rdv->Header.y2,P[1])-fmax(rdv->Header.y1,P[1])
    );
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <RDeviceAreaOverlap>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Determines whether an item lies entirely inside, entirely outside, or overlapping a given rectangle.
 *
 * Parametres   :
 *  <Canv>      : Canvas containing rdevice
 *  <ItemPtr>   : rdevice to display
 *  <Rect>      : Rectangle of the area to check (x0,y0,x1,y1)
 *
 * Retour       : -1 if item is entirely outside, 0 if overlaps and 1 if entirely outside
 *
 * Side effects :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int RDeviceAreaOverlap(Tk_Canvas Canv,Tk_Item *ItemPtr,double *Rect) {
    RDeviceItem *rdv = (RDeviceItem*)ItemPtr;

    if( Rect[0]>=rdv->Header.x2 || Rect[2]<=rdv->Header.x1 || Rect[1]>=rdv->Header.y2 || Rect[3]<=rdv->Header.y1 ) {
        return -1;
    }
    if( Rect[0]<=rdv->Header.x1 && Rect[2]>=rdv->Header.x2 && Rect[1]<=rdv->Header.y1 && Rect[3]>=rdv->Header.y2 ) {
        return 1;
    }
    return 0;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <RDeviceScale>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Resize the RDevice
 *
 * Parametres   :
 *  <Canv>      : Canvas containing rdevice
 *  <ItemPtr>   : Item to resize
 *  <OriginX>   : Origin in X about which to scale item
 *  <OriginY>   : Origin in Y about which to scale item
 *  <ScaleX>    : Amount to scale in X direction
 *  <ScaleY>    : Amount to scale in Y direction
 *
 * Retour       :
 *
 * Side effects : ItemPtr is drawn in Drawable using the transformation information in Canvas.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static void RDeviceScale(Tk_Canvas Canv,Tk_Item *ItemPtr,double OriginX,double OriginY,double ScaleX,double ScaleY) {
    RDeviceItem *rdv = (RDeviceItem*)ItemPtr;

    rdv->Header.x1 = (int)round(OriginX + ScaleX*(rdv->Header.x1-OriginX));
    rdv->Header.y1 = (int)round(OriginY + ScaleY*(rdv->Header.y1-OriginY));
    rdv->Header.x2 = (int)round(OriginX + ScaleX*(rdv->Header.x2-OriginX));
    rdv->Header.y2 = (int)round(OriginY + ScaleY*(rdv->Header.y2-OriginY));

    // Update the device since we resized it
    DBGPRINTF("Scaled to %d %d\n",rdv->Header.x2-rdv->Header.x1,rdv->Header.y2-rdv->Header.y1);
    if( rdv->resize )
        rdv->resize(rdv->RDev,rdv->Header.x2-rdv->Header.x1,rdv->Header.y2-rdv->Header.y1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <RDeviceDisplay>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Move the item by the given amount
 *
 * Parametres   :
 *  <Canv>      : Canvas containing item
 *  <ItemPtr>   : Item to move
 *  <DX>        : Amount by which Item must be moved in X
 *  <DY>        : Amount by which Item must be moved in Y
 *
 * Retour       :
 *
 * Side effects : The position of the item is offset by (DX,DY)
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static void RDeviceTranslate(Tk_Canvas Canv,Tk_Item *ItemPtr,double DX, double DY) {
    RDeviceItem *rdv = (RDeviceItem*)ItemPtr;

    rdv->Header.x1 += DX;
    rdv->Header.y1 += DY;
    rdv->Header.x2 += DX;
    rdv->Header.y2 += DY;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <RDeviceToPostscript>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Generate Postscript for rdevice item
 *
 * Parametres   :
 *  <Interp>    : Interp to leave error messages into if needed
 *  <Canv>      : Canvas containing item
 *  <ItemPtr>   : Item to generate postscript for
 *  <Prepass>   : 1 if this is a prepass to collect font info, 0 if postscript is being created
 *
 * Retour       : TCL_OK if OK, TCL_ERROR else
 *
 * Side effects :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int RDeviceToPostscript(Tcl_Interp *Interp,Tk_Canvas Canv,Tk_Item *ItemPtr,int prepass) {
    RDeviceItem *rdv    = (RDeviceItem*)ItemPtr;
    Tcl_Obj     *psObj  = Tcl_NewObj();

    // TODO translate to the rdevice's position in the canvas
    TclRDevice2PS_Dev2PS(Interp,rdv->RDev,psObj);
    Tcl_AppendObjToObj(Tcl_GetObjResult(Interp), psObj);

    Tcl_DecrRefCount(psObj);

    return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <RDeviceItem_SignalRedraw>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Called by the device to signal changes in the device-curated pixmap
 *
 * Parametres   :
 *  <Item>      : Item that needs to be updated
 *
 * Retour       :
 *
 * Side effects : A redraw  request will be signalled to the canvas containing the item
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void RDeviceItem_SignalRedraw(void *Item) {
    RDeviceItem *rdv = (RDeviceItem*)Item;

    if( rdv->tkredraw )
        rdv->tkredraw(rdv->Canv,rdv->Header.x1,rdv->Header.y1,rdv->Header.x2,rdv->Header.y2);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <RDeviceItem_DetachDevice>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Called by the device when the device is closed by R.
 *
 * Parametres   :
 *  <Item>      : Item that needs to be updated
 *
 * Retour       :
 *
 * Side effects : Resets the RDev to NULL
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void RDeviceItem_DetachDevice(void *Item) {
    ((RDeviceItem*)Item)->RDev = NULL;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <RDeviceItem_SetFont>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Called by the device when a font change happens in R
 *
 * Parametres   :
 *  <Item>      : Item want to update the font of
 *
 * Retour       :
 *
 * Side effects : Updates the font in the item
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void RDeviceItem_SetFont(void *Item,Tk_Font Font) {
    ((RDeviceItem*)Item)->Font = Font;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <RDeviceItem_Register>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Called externally to register the rdevice type so that it can be created by a canvas
 *
 * Parametres   :
 *
 * Retour       :
 *
 * Side effects : Creates a new canvas item type
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void RDeviceItem_Register() {
    void (*createItemType)(Tk_ItemType *Type);

    // Register to a Tk canvas
    Tk_CreateItemType(&tkRDeviceType);

    // Register to our glCanvas (if package is loaded)
    if( (createItemType=dlsym(NULL,"Tk_glCreateItemType")) ) {
        createItemType(&tkglRDeviceType);
    }
}
