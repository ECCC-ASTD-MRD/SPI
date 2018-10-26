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
#include <math.h>

/*
 * The structure below defines the record for each RDevice item.
 */

typedef struct RDeviceItem  {
    Tk_Item     Header;     // Generic stuff that's the same for all types. MUST BE FIRST IN STRUCTURE
    void        *RDev;      // R Device (Volontarily opaque type)
    Tk_Canvas   Canv;       // Canvas containing the item
    GC          GC;         // X Graphic Context
} RDeviceItem;

/*
 * Information used for parsing configuration specs:
 */

static const Tk_CustomOption tagsOption = {
    Tk_CanvasTagsParseProc, Tk_CanvasTagsPrintProc, NULL
};

static const Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_CUSTOM,  "-tags",    NULL,NULL,NULL,     0,  TK_CONFIG_NULL_OK,  &tagsOption},
    {TK_CONFIG_END,     NULL,       NULL,NULL,NULL,     0,  0,                  NULL}
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
    rdv->GC = None;
    rdv->Canv = Canv;
    rdv->RDev = NULL;

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

    // Make sure the XWindow exists (needed for the RDevice) instead of having to call "update ideltasks" in the tcl code
    Tk_MakeWindowExist(win);

    // Create the device in R
    if( !(rdv->RDev=TclRDeviceX_Init(Interp,(void*)rdv,win,rdv->Header.x2-rdv->Header.x1,rdv->Header.y2-rdv->Header.y1)) ) {
        goto error;
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

    rdv->Header.x1 = c[0];
    rdv->Header.y1 = c[1];
    rdv->Header.x2 = c[2];
    rdv->Header.y2 = c[3];

    //if( x<0.0 || y<0.0 || w<0.0 || h<0.0 ) {
    //    Tcl_SetObjResult(Interp,Tcl_ObjPrintf("invalid coordinates: negative coordinates obtained"));
    //    Tcl_SetErrorCode(Interp,"TK","CANVAS","COORDS","RDEVICE",NULL);
    //    return TCL_ERROR;
    //}

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
 * Nom          : <RDeviceConfigure>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Clean up the data structure associated with an rdevice
 *
 * Parametres   :
 *  <Canv>      : Canvas containing rdevice
 *  <ItemPtr>   : rdevice to delete
 *  <Display>   : Display containing window for canvas
 *  <Objv>      : Arguments describing changes
 *  <Flags>     : Flags to pass to Tk_ConfigureWidget
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
    TclRDeviceX_Destroy(rdv->RDev);
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
        printf("Drawable coords : %hd %hd\n",drawX,drawY);
        printf("x(%d) y(%d) w(%u) h(%u)\n",x,y,w,h);

        // Copy the selected area from our pixmap (filled by the RDevice) to the canvas
        // Note that we need to set the mask's origin to the start of our pixmap if we want to draw a background correctly
        //XSetClipOrigin(Display,rdv->GC,drawX-x,drawY-y);
        XCopyArea(Display,pixmap,Drawable,Canvas(Canv)->pixmapGC,x,y,w,h,drawX,drawY);
        //XSetClipOrigin(Display,rdv->gc,0,0);
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

/*
 *--------------------------------------------------------------
 *
 * ScaleBitmap --
 *
 *    This function is invoked to rescale a bitmap item in a canvas. It is
 *    one of the standard item functions for bitmap items, and is invoked by
 *    the generic canvas code.
 *
 * Results:
 *    None.
 *
 * Side effects:
 *    The item referred to by itemPtr is rescaled so that the following
 *    transformation is applied to all point coordinates:
 *        x' = originX + scaleX*(x-originX)
 *        y' = originY + scaleY*(y-originY)
 *
 *--------------------------------------------------------------
 */

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

    //TODO Resize the rdevice
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
    // TODO
    //BitmapItem *bmapPtr = (BitmapItem *) itemPtr;
    //double x, y;
    //int width, height, rowsAtOnce, rowsThisTime;
    //int curRow;
    //XColor *fgColor;
    //XColor *bgColor;
    //Pixmap bitmap;
    //Tk_State state = itemPtr->state;
    //Tcl_Obj *psObj;
    //Tcl_InterpState interpState;

    //if (state == TK_STATE_NULL) {
    //    state = Canvas(canvas)->canvas_state;
    //}
    //fgColor = bmapPtr->fgColor;
    //bgColor = bmapPtr->bgColor;
    //bitmap = bmapPtr->bitmap;
    //if (Canvas(canvas)->currentItemPtr == itemPtr) {
    //    if (bmapPtr->activeFgColor!=NULL) {
    //        fgColor = bmapPtr->activeFgColor;
    //    }
    //    if (bmapPtr->activeBgColor!=NULL) {
    //        bgColor = bmapPtr->activeBgColor;
    //    }
    //    if (bmapPtr->activeBitmap!=None) {
    //        bitmap = bmapPtr->activeBitmap;
    //    }
    //} else if (state == TK_STATE_DISABLED) {
    //    if (bmapPtr->disabledFgColor!=NULL) {
    //        fgColor = bmapPtr->disabledFgColor;
    //    }
    //    if (bmapPtr->disabledBgColor!=NULL) {
    //        bgColor = bmapPtr->disabledBgColor;
    //    }
    //    if (bmapPtr->disabledBitmap!=None) {
    //        bitmap = bmapPtr->disabledBitmap;
    //    }
    //}

    //if (bitmap == None) {
    //    return TCL_OK;
    //}

    ///*
    // * Compute the coordinates of the lower-left corner of the bitmap, taking
    // * into account the anchor position for the bitmp.
    // */

    //x = bmapPtr->x;
    //y = Tk_CanvasPsY(canvas, bmapPtr->y);
    //Tk_SizeOfBitmap(Tk_Display(Tk_CanvasTkwin(canvas)), bitmap,
    //        &width, &height);
    //switch (bmapPtr->anchor) {
    //    case TK_ANCHOR_NW:               y -= height;        break;
    //    case TK_ANCHOR_N:       x -= width/2.0; y -= height;        break;
    //    case TK_ANCHOR_NE:       x -= width;       y -= height;        break;
    //    case TK_ANCHOR_E:       x -= width;       y -= height/2.0;    break;
    //    case TK_ANCHOR_SE:       x -= width;                break;
    //    case TK_ANCHOR_S:       x -= width/2.0;            break;
    //    case TK_ANCHOR_SW:                        break;
    //    case TK_ANCHOR_W:               y -= height/2.0;    break;
    //    case TK_ANCHOR_CENTER: x -= width/2.0; y -= height/2.0;    break;
    //}

    ///*
    // * Make our working space.
    // */

    //psObj = Tcl_NewObj();
    //interpState = Tcl_SaveInterpState(interp, TCL_OK);

    ///*
    // * Color the background, if there is one.
    // */

    //if (bgColor != NULL) {
    //    Tcl_AppendPrintfToObj(psObj,
    //            "%.15g %.15g moveto %d 0 rlineto 0 %d rlineto "
    //            "%d 0 rlineto closepath\n",
    //            x, y, width, height, -width);

    //    Tcl_ResetResult(interp);
    //    if (Tk_CanvasPsColor(interp, canvas, bgColor) != TCL_OK) {
    //        goto error;
    //    }
    //    Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));

    //    Tcl_AppendToObj(psObj, "fill\n", -1);
    //}

    ///*
    // * Draw the bitmap, if there is a foreground color. If the bitmap is very
    // * large, then chop it up into multiple bitmaps, each consisting of one or
    // * more rows. This is needed because Postscript can't handle single
    // * strings longer than 64 KBytes long.
    // */

    //if (fgColor != NULL) {
    //    Tcl_ResetResult(interp);
    //    if (Tk_CanvasPsColor(interp, canvas, fgColor) != TCL_OK) {
    //        goto error;
    //    }
    //    Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));

    //    if (width > 60000) {
    //        Tcl_SetObjResult(interp, Tcl_NewStringObj(
    //                    "can't generate Postscript for bitmaps more than 60000"
    //                    " pixels wide", -1));
    //        Tcl_SetErrorCode(interp, "TK", "CANVAS", "PS", "MEMLIMIT", NULL);
    //        goto error;
    //    }

    //    rowsAtOnce = 60000/width;
    //    if (rowsAtOnce < 1) {
    //        rowsAtOnce = 1;
    //    }

    //    Tcl_AppendPrintfToObj(psObj, "%.15g %.15g translate\n", x, y+height);

    //    for (curRow = 0; curRow < height; curRow += rowsAtOnce) {
    //        rowsThisTime = rowsAtOnce;
    //        if (rowsThisTime > (height - curRow)) {
    //            rowsThisTime = height - curRow;
    //        }

    //        Tcl_AppendPrintfToObj(psObj,
    //                "0 -%.15g translate\n%d %d true matrix {\n",
    //                (double) rowsThisTime, width, rowsThisTime);

    //        Tcl_ResetResult(interp);
    //        if (Tk_CanvasPsBitmap(interp, canvas, bitmap,
    //                    0, curRow, width, rowsThisTime) != TCL_OK) {
    //            goto error;
    //        }
    //        Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));

    //        Tcl_AppendToObj(psObj, "\n} imagemask\n", -1);
    //    }
    //}

    ///*
    // * Plug the accumulated postscript back into the result.
    // */

    //(void) Tcl_RestoreInterpState(interp, interpState);
    //Tcl_AppendObjToObj(Tcl_GetObjResult(interp), psObj);
    //Tcl_DecrRefCount(psObj);
    return TCL_OK;

//error:
    //Tcl_DiscardInterpState(interpState);
    //Tcl_DecrRefCount(psObj);
    //return TCL_ERROR;
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
    Tk_CanvasEventuallyRedraw(rdv->Canv,rdv->Header.x1,rdv->Header.y1,rdv->Header.x2,rdv->Header.y2);
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
 * Nom          : <RDeviceItem_Register>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Called externally to register the rdevice type so that it can be created by a canvas
 *
 * Parametres   :
 *
 * Retour       :
 *
 * Side effects : Resets the RDev to NULL
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void RDeviceItem_Register() {
    Tk_CreateItemType(&tkRDeviceType);
}
