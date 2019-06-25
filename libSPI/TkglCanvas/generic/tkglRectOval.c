/*
 * tkglRectOval.c --
 *
 *	This file implements rectangle and oval items for canvas
 *	widgets.
 *
 * Copyright (c) 1991-1994 The Regents of the University of California.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: tkRectOval.c,v 1.7 2000/04/19 22:20:45 ericm Exp $
 */

#include "tkglCanvas.h"

/* The structure below defines the record for each rectangle/oval item. */

typedef struct glRectOvalItem  {
   Tk_Item header;       /* Generic stuff that's the same for all types.  MUST BE FIRST IN STRUCTURE. */
   Tk_Outline outline;   /* Outline structure */
   double bbox[4];       /* Coordinates of bounding box for rectangle
                          * or oval (x1, y1, x2, y2).  Item includes
                          * x1 and x2 but not y1 and y2. */
   Tk_TSOffset tsoffset;
   XColor *fillColor;                   /* Color for filling rectangle/oval. */
   XColor *activeFillColor;             /* Color for filling rectangle/oval if state is active. */
   XColor *disabledFillColor;           /* Color for filling rectangle/oval if state is disabled. */

   T_glBitmap *fillStipple;             /* Stipple bitmap for filling item. */
   T_glBitmap *activeFillStipple;       /* Stipple bitmap for filling item if state is active. */
   T_glBitmap *disabledFillStipple;     /* Stipple bitmap for filling item if state is disabled. */
   int alpha;                           /* Transparency factor */
} glRectOvalItem;

/* Information used for parsing configuration specs */

static const Tk_CustomOption bitmapOption = { glBitmapParseProc,glBitmapPrintProc,NULL };
static const Tk_CustomOption stateOption  = { TkStateParseProc,TkStatePrintProc,INT2PTR(2) };
static const Tk_CustomOption tagsOption   = { Tk_CanvasTagsParseProc,Tk_CanvasTagsPrintProc,NULL };
static const Tk_CustomOption dashOption   = { TkCanvasDashParseProc,TkCanvasDashPrintProc,NULL };
static const Tk_CustomOption offsetOption = { TkOffsetParseProc,TkOffsetPrintProc,INT2PTR(TK_OFFSET_RELATIVE) };
static const Tk_CustomOption pixelOption  = { TkPixelParseProc,TkPixelPrintProc,NULL };

static const Tk_ConfigSpec configSpecs[] = {
   { TK_CONFIG_CUSTOM, "-activedash", NULL, NULL, NULL, Tk_Offset(glRectOvalItem,outline.activeDash), TK_CONFIG_NULL_OK,&dashOption },
   { TK_CONFIG_COLOR, "-activefill", NULL, NULL, NULL, Tk_Offset(glRectOvalItem, activeFillColor), TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-activeoutline", NULL, NULL, NULL, Tk_Offset(glRectOvalItem, outline.activeColor), TK_CONFIG_NULL_OK },
   { TK_CONFIG_BITMAP, "-activeoutlinestipple", NULL, NULL, NULL, Tk_Offset(glRectOvalItem, outline.activeStipple), TK_CONFIG_NULL_OK },
   { TK_CONFIG_CUSTOM, "-activestipple", NULL, NULL, NULL, Tk_Offset(glRectOvalItem,activeFillStipple), TK_CONFIG_NULL_OK,&bitmapOption },
   { TK_CONFIG_CUSTOM, "-activewidth", NULL, NULL, "0.0", Tk_Offset(glRectOvalItem, outline.activeWidth), TK_CONFIG_DONT_SET_DEFAULT, &pixelOption },
   { TK_CONFIG_CUSTOM, "-dash", NULL, NULL, NULL, Tk_Offset(glRectOvalItem, outline.dash), TK_CONFIG_NULL_OK, &dashOption },
   { TK_CONFIG_PIXELS, "-dashoffset", NULL, NULL, "0", Tk_Offset(glRectOvalItem, outline.offset), TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_CUSTOM, "-disableddash", NULL, NULL, NULL, Tk_Offset(glRectOvalItem, outline.disabledDash), TK_CONFIG_NULL_OK, &dashOption },
   { TK_CONFIG_COLOR, "-disabledfill", NULL, NULL, NULL, Tk_Offset(glRectOvalItem, disabledFillColor), TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR, "-disabledoutline", NULL, NULL, NULL, Tk_Offset(glRectOvalItem, outline.disabledColor), TK_CONFIG_NULL_OK },
   { TK_CONFIG_BITMAP, "-disabledoutlinestipple", NULL, NULL, NULL, Tk_Offset(glRectOvalItem, outline.disabledStipple), TK_CONFIG_NULL_OK },
   { TK_CONFIG_CUSTOM, "-disabledstipple", NULL, NULL, NULL, Tk_Offset(glRectOvalItem,disabledFillStipple),TK_CONFIG_NULL_OK,&bitmapOption },
   { TK_CONFIG_PIXELS, "-disabledwidth", NULL, NULL, "0.0", Tk_Offset(glRectOvalItem, outline.disabledWidth), TK_CONFIG_DONT_SET_DEFAULT, &pixelOption },
   { TK_CONFIG_COLOR, "-fill", NULL, NULL, NULL, Tk_Offset(glRectOvalItem, fillColor), TK_CONFIG_NULL_OK },
   { TK_CONFIG_CUSTOM, "-offset", NULL, NULL, "0,0", Tk_Offset(glRectOvalItem, tsoffset), TK_CONFIG_DONT_SET_DEFAULT, &offsetOption },
   { TK_CONFIG_COLOR, "-outline", NULL, NULL, "black", Tk_Offset(glRectOvalItem, outline.color), TK_CONFIG_NULL_OK },
   { TK_CONFIG_CUSTOM, "-outlineoffset", NULL, NULL, "0,0", Tk_Offset(glRectOvalItem, outline.tsoffset), TK_CONFIG_DONT_SET_DEFAULT, &offsetOption },
   { TK_CONFIG_BITMAP, "-outlinestipple", NULL, NULL, NULL, Tk_Offset(glRectOvalItem, outline.stipple), TK_CONFIG_NULL_OK },
   { TK_CONFIG_CUSTOM, "-state", NULL, NULL, NULL, Tk_Offset(Tk_Item, state),TK_CONFIG_NULL_OK, &stateOption },
   { TK_CONFIG_CUSTOM,"-stipple", NULL, NULL, NULL, Tk_Offset(glRectOvalItem,fillStipple), TK_CONFIG_NULL_OK, &bitmapOption },
   { TK_CONFIG_CUSTOM, "-tags", NULL, NULL, NULL, 0, TK_CONFIG_NULL_OK, &tagsOption },
   { TK_CONFIG_CUSTOM, "-width", NULL, NULL, "1.0", Tk_Offset(glRectOvalItem, outline.width), TK_CONFIG_DONT_SET_DEFAULT, &pixelOption },
   { TK_CONFIG_INT, "-transparency", NULL, NULL, "100", Tk_Offset(glRectOvalItem,alpha), TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0 }
};

/* Prototypes for procedures defined in this file */

static void   glComputeRectOvalBbox (Tk_Canvas canvas,glRectOvalItem *rectOvalPtr);
static int    glConfigureRectOval (Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int argc,Tcl_Obj *const objv[],int flags);
static int    glCreateRectOval (Tcl_Interp *interp,Tk_Canvas canvas,struct Tk_Item *itemPtr,int argc,Tcl_Obj *const objv[]);
static void   glDeleteRectOval (Tk_Canvas canvas,Tk_Item *itemPtr,Display *display);
static void   glDisplayRectOval (Tk_Canvas canvas,Tk_Item *itemPtr,Display *display,Drawable dst,int x,int y,int width,int height);
static int    glOvalToArea (Tk_Canvas canvas,Tk_Item *itemPtr,double *areaPtr);
static double glOvalToPoint (Tk_Canvas canvas,Tk_Item *itemPtr,double *pointPtr);
static int    glRectOvalCoords (Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int argc,Tcl_Obj *const objv[]);
static int    glRectOvalToPostscript (Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int prepass);
static int    glRectToArea (Tk_Canvas canvas,Tk_Item *itemPtr,double *areaPtr);
static double glRectToPoint (Tk_Canvas canvas,Tk_Item *itemPtr,double *pointPtr);
static void   glScaleRectOval (Tk_Canvas canvas,Tk_Item *itemPtr,double originX,double originY,double scaleX,double scaleY);
static void   glTranslateRectOval (Tk_Canvas canvas,Tk_Item *itemPtr,double deltaX,double deltaY);

/*
 * The structures below defines the rectangle and oval item types
 * by means of procedures that can be invoked by generic item code.
 */

Tk_ItemType tkglRectangleType = {
   "rectangle",                 /* name */
   sizeof(glRectOvalItem),      /* itemSize */
   glCreateRectOval,            /* createProc */
   configSpecs,                 /* configSpecs */
   glConfigureRectOval,         /* configureProc */
   glRectOvalCoords,            /* coordProc */
   glDeleteRectOval,            /* deleteProc */
   glDisplayRectOval,           /* displayProc */
   TK_CONFIG_OBJS,              /* flags */
   glRectToPoint,               /* pointProc */
   glRectToArea,                /* areaProc */
   glRectOvalToPostscript,      /* postscriptProc */
   glScaleRectOval,             /* scaleProc */
   glTranslateRectOval,         /* translateProc */
   NULL,                        /* indexProc */
   NULL,                        /* icursorProc */
   NULL,                        /* selectionProc */
   NULL,                        /* insertProc */
   NULL,                        /* dTextProc */
   NULL,                        /* nextPtr */
};

Tk_ItemType tkglOvalType = {
   "oval",                      /* name */
   sizeof(glRectOvalItem),      /* itemSize */
   glCreateRectOval,            /* createProc */
   configSpecs,                 /* configSpecs */
   glConfigureRectOval,         /* configureProc */
   glRectOvalCoords,            /* coordProc */
   glDeleteRectOval,            /* deleteProc */
   glDisplayRectOval,           /* displayProc */
   TK_CONFIG_OBJS,              /* flags */
   glOvalToPoint,               /* pointProc */
   glOvalToArea,                /* areaProc */
   glRectOvalToPostscript,      /* postscriptProc */
   glScaleRectOval,             /* scaleProc */
   glTranslateRectOval,         /* translateProc */
   NULL,                        /* indexProc */
   NULL,                        /* cursorProc */
   NULL,                        /* selectionProc */
   NULL,                        /* insertProc */
   NULL,                        /* dTextProc */
   NULL,                        /* nextPtr */
};

/*
 *--------------------------------------------------------------
 *
 * glCreateRectOval --
 *
 *	This procedure is invoked to create a new rectangle
 *	or oval item in a canvas.
 *
 * Results:
 *	A standard Tcl return value.  If an error occurred in
 *	creating the item, then an error message is left in
 *	the interp's result;  in this case itemPtr is left uninitialized,
 *	so it can be safely freed by the caller.
 *
 * Side effects:
 *	A new rectangle or oval item is created.
 *
 *--------------------------------------------------------------
 */

static int glCreateRectOval(
   Tcl_Interp *interp,    /* For error reporting. */
   Tk_Canvas canvas,      /* Canvas to hold new item. */
   Tk_Item *itemPtr,      /* Record to hold new item; header has been * initialized by caller. */
   int objc,              /* Number of arguments in objv. */
   Tcl_Obj *const objv[]) /* Arguments describing rectangle. */
{
   glRectOvalItem *rectOvalPtr = (glRectOvalItem *) itemPtr;
   int i;

   if (objc == 0) {
      Tcl_Panic("canvas did not pass any coords");
   }

   /* Carry out initialization that is needed in order to clean up after errors
   during the the remainder of this procedure. */

   Tk_CreateOutline(&(rectOvalPtr->outline));
   rectOvalPtr->outline.gc          = None;
   rectOvalPtr->tsoffset.flags      = 0;
   rectOvalPtr->tsoffset.xoffset    = 0;
   rectOvalPtr->tsoffset.yoffset    = 0;
   rectOvalPtr->fillColor           = NULL;
   rectOvalPtr->activeFillColor     = NULL;
   rectOvalPtr->disabledFillColor   = NULL;
   rectOvalPtr->fillStipple         = None;
   rectOvalPtr->activeFillStipple       = None;
   rectOvalPtr->disabledFillStipple     = None;
   rectOvalPtr->alpha               = 100;

   /* Process the arguments to fill in the item record. */

   for (i = 1; i < objc; i++) {
      const char *arg = Tcl_GetString(objv[i]);

      if ((arg[0] == '-') && (arg[1] >= 'a') && (arg[1] <= 'z')) {
         break;
      }
   }

   if ((glRectOvalCoords(interp,canvas,itemPtr,i,objv) == TCL_OK)) {
      if (glConfigureRectOval(interp,canvas,itemPtr,objc-i,objv+i,0) == TCL_OK) {
         return TCL_OK;
      }
   }

   glDeleteRectOval(canvas,itemPtr,Tk_Display(Tk_CanvasTkwin(canvas)));
   return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * glRectOvalCoords --
 *
 *	This procedure is invoked to process the "coords" widget
 *	command on rectangles and ovals.  See the user documentation
 *	for details on what it does.
 *
 * Results:
 *	Returns TCL_OK or TCL_ERROR, and sets the interp's result.
 *
 * Side effects:
 *	The coordinates for the given item may be changed.
 *
 *--------------------------------------------------------------
 */

static int glRectOvalCoords(
   Tcl_Interp *interp,    /* Used for error reporting. */
   Tk_Canvas canvas,      /* Canvas containing item. */
   Tk_Item *itemPtr,      /* Item whose coordinates are to be read or modified. */
   int objc,              /* Number of coordinates supplied in objv. */
   Tcl_Obj *const objv[]) /* Array of coordinates: x1,y1,x2,y2,... */
{
   glRectOvalItem *rectOvalPtr = (glRectOvalItem *) itemPtr;

   /*
   * If no coordinates, return the current coordinates (i.e. bounding box).
   */

   if (objc == 0) {
      Tcl_Obj *bbox[4];

      bbox[0] = Tcl_NewDoubleObj(rectOvalPtr->bbox[0]);
      bbox[1] = Tcl_NewDoubleObj(rectOvalPtr->bbox[1]);
      bbox[2] = Tcl_NewDoubleObj(rectOvalPtr->bbox[2]);
      bbox[3] = Tcl_NewDoubleObj(rectOvalPtr->bbox[3]);
      Tcl_SetObjResult(interp, Tcl_NewListObj(4, bbox));
      return TCL_OK;
   }

   /*
   * If one "coordinate", treat as list of coordinates.
   */

   if (objc == 1) {
      if (Tcl_ListObjGetElements(interp, objv[0], &objc,
            (Tcl_Obj ***) &objv) != TCL_OK) {
         return TCL_ERROR;
      }
   }

   /*
   * Better have four coordinates now. Spit out an error message otherwise.
   */

   if (objc != 4) {
      Tcl_SetObjResult(interp, Tcl_ObjPrintf(
         "wrong # coordinates: expected 0 or 4, got %d", objc));
      Tcl_SetErrorCode(interp, "TK", "CANVAS", "COORDS",
         (rectOvalPtr->header.typePtr == &tkglRectangleType
            ? "RECTANGLE" : "OVAL"), NULL);
      return TCL_ERROR;
   }

   /*
   * Parse the coordinates and update our bounding box.
   */

   if ((Tk_CanvasGetCoordFromObj(interp, canvas, objv[0],
      &rectOvalPtr->bbox[0]) != TCL_OK)
      || (Tk_CanvasGetCoordFromObj(interp, canvas, objv[1],
      &rectOvalPtr->bbox[1]) != TCL_OK)
      || (Tk_CanvasGetCoordFromObj(interp, canvas, objv[2],
      &rectOvalPtr->bbox[2]) != TCL_OK)
      || (Tk_CanvasGetCoordFromObj(interp, canvas, objv[3],
      &rectOvalPtr->bbox[3]) != TCL_OK)) {
      return TCL_ERROR;
   }
   glComputeRectOvalBbox(canvas, rectOvalPtr);
   return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * glConfigureRectOval --
 *
 *	This procedure is invoked to configure various aspects
 *	of a rectangle or oval item, such as its border and
 *	background colors.
 *
 * Results:
 *	A standard Tcl result code.  If an error occurs, then
 *	an error message is left in the interp's result.
 *
 * Side effects:
 *	Configuration information, such as colors and stipple
 *	patterns, may be set for itemPtr.
 *
 *--------------------------------------------------------------
 */

static int glConfigureRectOval(
   Tcl_Interp *interp,    /* Used for error reporting. */
   Tk_Canvas canvas,      /* Canvas containing itemPtr. */
   Tk_Item *itemPtr,      /* Rectangle item to reconfigure. */
   int objc,              /* Number of elements in objv. */
   Tcl_Obj *const objv[], /* Arguments describing things to configure. */
   int flags)             /* Flags to pass to Tk_ConfigureWidget. */
{
   glRectOvalItem *rectOvalPtr = (glRectOvalItem *) itemPtr;
   Tk_Window tkwin;
   Tk_TSOffset *tsoffset;
   Tk_State state;

   tkwin = Tk_CanvasTkwin(canvas);

   if (Tk_ConfigureWidget(interp, tkwin, configSpecs, objc, (const char **)objv,(char *) rectOvalPtr, flags|TK_CONFIG_OBJS) != TCL_OK)
      return TCL_ERROR;

   state = itemPtr->state;  /* Get item state */

   /* A few of the options require additional processing, such as graphics contexts. */

   if (rectOvalPtr->outline.activeWidth > rectOvalPtr->outline.width ||
      rectOvalPtr->outline.activeDash.number != 0 ||
      rectOvalPtr->outline.activeColor != NULL ||
      rectOvalPtr->outline.activeStipple != None ||
      rectOvalPtr->activeFillColor != NULL ||
      rectOvalPtr->activeFillStipple != NULL) {
      itemPtr->redraw_flags |= TK_ITEM_STATE_DEPENDANT;
   } else {
      itemPtr->redraw_flags &= ~TK_ITEM_STATE_DEPENDANT;
   }

   tsoffset = &rectOvalPtr->outline.tsoffset;
      flags = tsoffset->flags;
   if (flags & TK_OFFSET_LEFT) {
      tsoffset->xoffset = (int) (rectOvalPtr->bbox[0] + 0.5);
   } else if (flags & TK_OFFSET_CENTER) {
      tsoffset->xoffset = (int) ((rectOvalPtr->bbox[0]+rectOvalPtr->bbox[2]+1)/2);
   } else if (flags & TK_OFFSET_RIGHT) {
      tsoffset->xoffset = (int) (rectOvalPtr->bbox[2] + 0.5);
   }
   if (flags & TK_OFFSET_TOP) {
      tsoffset->yoffset = (int) (rectOvalPtr->bbox[1] + 0.5);
   } else if (flags & TK_OFFSET_MIDDLE) {
      tsoffset->yoffset = (int) ((rectOvalPtr->bbox[1]+rectOvalPtr->bbox[3]+1)/2);
   } else if (flags & TK_OFFSET_BOTTOM) {
      tsoffset->yoffset = (int) (rectOvalPtr->bbox[2] + 0.5);
   }

   if(state == TK_STATE_NULL) {
      state = Canvas(canvas)->canvas_state;
   }
   if (state==TK_STATE_HIDDEN) {
      glComputeRectOvalBbox(canvas, rectOvalPtr);
      return TCL_OK;
   }

   tsoffset = &rectOvalPtr->tsoffset;
   flags = tsoffset->flags;
   if (flags & TK_OFFSET_LEFT) {
      tsoffset->xoffset = (int) (rectOvalPtr->bbox[0] + 0.5);
   } else if (flags & TK_OFFSET_CENTER) {
      tsoffset->xoffset = (int) ((rectOvalPtr->bbox[0]+rectOvalPtr->bbox[2]+1)/2);
   } else if (flags & TK_OFFSET_RIGHT) {
      tsoffset->xoffset = (int) (rectOvalPtr->bbox[2] + 0.5);
   }
   if (flags & TK_OFFSET_TOP) {
      tsoffset->yoffset = (int) (rectOvalPtr->bbox[1] + 0.5);
   } else if (flags & TK_OFFSET_MIDDLE) {
      tsoffset->yoffset = (int) ((rectOvalPtr->bbox[1]+rectOvalPtr->bbox[3]+1)/2);
   } else if (flags & TK_OFFSET_BOTTOM) {
      tsoffset->yoffset = (int) (rectOvalPtr->bbox[3] + 0.5);
   }

   rectOvalPtr->alpha=rectOvalPtr->alpha<0?0:rectOvalPtr->alpha>100?100:rectOvalPtr->alpha;

   glComputeRectOvalBbox(canvas, rectOvalPtr);

   return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * glDeleteRectOval --
 *
 *	This procedure is called to clean up the data structure
 *	associated with a rectangle or oval item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources associated with itemPtr are released.
 *
 *--------------------------------------------------------------
 */

static void glDeleteRectOval(
   Tk_Canvas canvas,      /* Info about overall widget. */
   Tk_Item *itemPtr,      /* Item that is being deleted. */
   Display *display)      /* Display containing window for canvas. */
{
   glRectOvalItem *rectOvalPtr = (glRectOvalItem *) itemPtr;

   Tk_DeleteOutline(display, &(rectOvalPtr->outline));
   if (rectOvalPtr->fillColor != NULL) {
      Tk_FreeColor(rectOvalPtr->fillColor);
   }
   if (rectOvalPtr->activeFillColor != NULL) {
      Tk_FreeColor(rectOvalPtr->activeFillColor);
   }
   if (rectOvalPtr->disabledFillColor != NULL) {
      Tk_FreeColor(rectOvalPtr->disabledFillColor);
   }
}

/*
 *--------------------------------------------------------------
 *
 * glComputeRectOvalBbox --
 *
 *	This procedure is invoked to compute the bounding box of
 *	all the pixels that may be drawn as part of a rectangle
 *	or oval.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The fields x1, y1, x2, and y2 are updated in the header
 *	for itemPtr.
 *
 *--------------------------------------------------------------
 */

static void glComputeRectOvalBbox(
   Tk_Canvas canvas,            /* Canvas that contains item. */
   glRectOvalItem *rectOvalPtr)   /* Item whose bbox is to be recomputed. */
{
    int bloat, tmp;
    double dtmp, width;
    Tk_State state = rectOvalPtr->header.state;

    if (state == TK_STATE_NULL) {
	state = Canvas(canvas)->canvas_state;
    }

    width = rectOvalPtr->outline.width;
    if (state == TK_STATE_HIDDEN) {
	rectOvalPtr->header.x1 = rectOvalPtr->header.y1 =
	rectOvalPtr->header.x2 = rectOvalPtr->header.y2 = -1;
	return;
    }
    if (Canvas(canvas)->currentItemPtr == (Tk_Item *) rectOvalPtr) {
	if (rectOvalPtr->outline.activeWidth>width) {
	    width = rectOvalPtr->outline.activeWidth;
	}
    } else if (state == TK_STATE_DISABLED) {
	if (rectOvalPtr->outline.disabledWidth>0) {
	    width = rectOvalPtr->outline.disabledWidth;
	}
    }

    /*
     * Make sure that the first coordinates are the lowest ones.
     */

    if (rectOvalPtr->bbox[1] > rectOvalPtr->bbox[3]) {
	double tmpY = rectOvalPtr->bbox[3];

	rectOvalPtr->bbox[3] = rectOvalPtr->bbox[1];
	rectOvalPtr->bbox[1] = tmpY;
    }
    if (rectOvalPtr->bbox[0] > rectOvalPtr->bbox[2]) {
	double tmpX = rectOvalPtr->bbox[2];

	rectOvalPtr->bbox[2] = rectOvalPtr->bbox[0];
	rectOvalPtr->bbox[0] = tmpX;
    }

    if (rectOvalPtr->outline.gc == None) {
	/*
	 * The Win32 switch was added for 8.3 to solve a problem with ovals
	 * leaving traces on bottom and right of 1 pixel. This may not be the
	 * correct place to solve it, but it works.
	 */

#ifdef __WIN32__
	bloat = 1;
#else
	bloat = 0;
#endif /* __WIN32__ */
    } else {
#ifdef MAC_OSX_TK
	/*
	 * Mac OS X CoreGraphics needs correct rounding here otherwise it will
	 * draw outside the bounding box. Probably correct on other platforms
	 * as well?
	 */

	bloat = (int) (width+1.5)/2;
#else
	bloat = (int) (width+1)/2;
#endif /* MAC_OSX_TK */
    }

    /*
     * Special note: the rectangle is always drawn at least 1x1 in size, so
     * round up the upper coordinates to be at least 1 unit greater than the
     * lower ones.
     */

    tmp = (int) ((rectOvalPtr->bbox[0] >= 0) ? rectOvalPtr->bbox[0] + .5
	    : rectOvalPtr->bbox[0] - .5);
    rectOvalPtr->header.x1 = tmp - bloat;
    tmp = (int) ((rectOvalPtr->bbox[1] >= 0) ? rectOvalPtr->bbox[1] + .5
	    : rectOvalPtr->bbox[1] - .5);
    rectOvalPtr->header.y1 = tmp - bloat;
    dtmp = rectOvalPtr->bbox[2];
    if (dtmp < (rectOvalPtr->bbox[0] + 1)) {
	dtmp = rectOvalPtr->bbox[0] + 1;
    }
    tmp = (int) ((dtmp >= 0) ? dtmp + .5 : dtmp - .5);
    rectOvalPtr->header.x2 = tmp + bloat;
    dtmp = rectOvalPtr->bbox[3];
    if (dtmp < (rectOvalPtr->bbox[1] + 1)) {
	dtmp = rectOvalPtr->bbox[1] + 1;
    }
    tmp = (int) ((dtmp >= 0) ? dtmp + .5 : dtmp - .5);
    rectOvalPtr->header.y2 = tmp + bloat;
}
/*
 *--------------------------------------------------------------
 *
 * glDisplayRectOval --
 *
 *	This procedure is invoked to draw a rectangle or oval
 *	item in a given drawable.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	ItemPtr is drawn in drawable using the transformation
 *	information in canvas.
 *
 *--------------------------------------------------------------
 */

static void glDisplayRectOval(
   Tk_Canvas canvas,      /* Canvas that contains item. */
   Tk_Item *itemPtr,      /* Item to be displayed. */
   Display *display,      /* Display on which to draw item. */
   Drawable drawable,     /* Pixmap or window in which to draw item. */
   int x, int y, int width, int height) /* Describes region of canvas that must be redisplayed (not used). */
{
   glRectOvalItem *rectOvalPtr = (glRectOvalItem *) itemPtr;
   short x1, y1, x2, y2;              /* Rectangle coordinates */
   double h,w;
   T_glBitmap *stipple;            /* Stippling bitmap */
   XColor *color;
   Tk_State state = itemPtr->state;   /* Current item's state */

    /*
     * Compute the screen coordinates of the bounding box for the item.
     * Make sure that the bbox is at least one pixel large, since some
     * X servers will die if it isn't.
     */

   Tk_CanvasDrawableCoords(canvas,rectOvalPtr->bbox[0],rectOvalPtr->bbox[1],&x1,&y1);
   Tk_CanvasDrawableCoords(canvas,rectOvalPtr->bbox[2],rectOvalPtr->bbox[3],&x2,&y2);

   if (x2 <= x1)
      x2 = x1+1;
   if (y2 <= y1)
      y2 = y1+1;

   h=(y2-y1)/2.0;
   w=(x2-x1)/2.0;
   
   if (state == TK_STATE_NULL)
      state = Canvas(canvas)->canvas_state;

   stipple = rectOvalPtr->fillStipple;   /* Get the bitmap for stippling */
   color = rectOvalPtr->fillColor;

   if (Canvas(canvas)->currentItemPtr == (Tk_Item *)rectOvalPtr) {
      if (rectOvalPtr->activeFillColor!=NULL) {
         color = rectOvalPtr->activeFillColor;
      }
      if (rectOvalPtr->activeFillStipple!=None)
         stipple = rectOvalPtr->activeFillStipple;
   } else if (state==TK_STATE_DISABLED) {
      if (rectOvalPtr->disabledFillColor!=NULL) {
         color = rectOvalPtr->disabledFillColor;
      }
      if (rectOvalPtr->disabledFillStipple!=None)
         stipple = rectOvalPtr->disabledFillStipple;
   }

   if(rectOvalPtr->alpha<100) {
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
   }

   glTranslated(-Canvas(canvas)->xOrigin,-Canvas(canvas)->yOrigin,0.0);

   if (color) {
      if (stipple) {
         glEnable(GL_POLYGON_STIPPLE);
         glPolygonStipple(stipple->Data);
      }

      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glColor4us(color->red,color->green,color->blue,rectOvalPtr->alpha*655);

      if (rectOvalPtr->header.typePtr == &tkglRectangleType) {
         glBegin(GL_QUADS);
            glVertex2f(x1,y2);
            glVertex2f(x2,y2);
            glVertex2f(x2,y1);
            glVertex2f(x1,y1);
         glEnd();

      } else {

         /* Translate & Scale */
         glPushMatrix();
         glTranslated(w+x1,h+y1,0.0);
         glScaled(w,h,0.0);
         glDrawCircle(128,GL_POLYGON);
         glPopMatrix();
      }
      glDisable(GL_POLYGON_STIPPLE);
   }

   /* Draw the outline of the rectangle */
   if (rectOvalPtr->outline.color && rectOvalPtr->outline.width>0) {

      glDash(&rectOvalPtr->outline.dash);

      glLineWidth((float)rectOvalPtr->outline.width);
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
      glColor4us(rectOvalPtr->outline.color->red,rectOvalPtr->outline.color->green,rectOvalPtr->outline.color->blue,rectOvalPtr->alpha*655);

      if (rectOvalPtr->header.typePtr == &tkglRectangleType) {
         glBegin(GL_QUADS);
            glVertex2f(x1,y2);
            glVertex2f(x2,y2);
            glVertex2f(x2,y1);
            glVertex2f(x1,y1);
         glEnd();

      } else {

         /* Translate & Scale */
         glPushMatrix();
         glTranslated(w+x1,h+y1,0.0);
         glScaled(w,h,0.0);
         glDrawCircle(128,GL_LINE_STRIP);
         glPopMatrix();
      }
      glDisable(GL_LINE_STIPPLE);
   }

   glTranslated(Canvas(canvas)->xOrigin,Canvas(canvas)->yOrigin,0.0);
   glDisable(GL_BLEND);
}

/*
 *--------------------------------------------------------------
 *
 * glRectToPoint --
 *
 *	Computes the distance from a given point to a given
 *	rectangle, in canvas units.
 *
 * Results:
 *	The return value is 0 if the point whose x and y coordinates
 *	are coordPtr[0] and coordPtr[1] is inside the rectangle.  If the
 *	point isn't inside the rectangle then the return value is the
 *	distance from the point to the rectangle.  If itemPtr is filled,
 *	then anywhere in the interior is considered "inside"; if
 *	itemPtr isn't filled, then "inside" means only the area
 *	occupied by the outline.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static double glRectToPoint(
   Tk_Canvas canvas,      /* Canvas containing item. */
   Tk_Item *itemPtr,      /* Item to check against point. */
   double *pointPtr)      /* Pointer to x and y coordinates. */
{
   glRectOvalItem *rectPtr = (glRectOvalItem *) itemPtr;
   double xDiff, yDiff, x1, y1, x2, y2, inc, tmp;
   double width;
   Tk_State state = itemPtr->state;

   if(state == TK_STATE_NULL) {
      state = Canvas(canvas)->canvas_state;
   }

   width = rectPtr->outline.width;
   if (Canvas(canvas)->currentItemPtr == itemPtr) {
      if (rectPtr->outline.activeWidth>width) {
         width = rectPtr->outline.activeWidth;
      }
   } else if (state==TK_STATE_DISABLED) {
      if (rectPtr->outline.disabledWidth>0) {
         width = rectPtr->outline.disabledWidth;
      }
   }

   /*
   * Generate a new larger rectangle that includes the border
   * width, if there is one.
   */

   x1 = rectPtr->bbox[0];
   y1 = rectPtr->bbox[1];
   x2 = rectPtr->bbox[2];
   y2 = rectPtr->bbox[3];
   inc = width/2.0;
   x1 -= inc;
   y1 -= inc;
   x2 += inc;
   y2 += inc;

   /*
   * If the point is inside the rectangle, handle specially:
   * distance is 0 if rectangle is filled, otherwise compute
   * distance to nearest edge of rectangle and subtract width
   * of edge.
   */

   if ((pointPtr[0] >= x1) && (pointPtr[0] < x2)
      && (pointPtr[1] >= y1) && (pointPtr[1] < y2)) {

         /*I consider a non fill rectangle as a rectangle so return in*/
      return 0.0;

      if ((rectPtr->fillColor) || !(rectPtr->outline.color)) {
         return 0.0;
      }
      xDiff = pointPtr[0] - x1;
      tmp = x2 - pointPtr[0];
      if (tmp < xDiff) {
         xDiff = tmp;
      }
      yDiff = pointPtr[1] - y1;
      tmp = y2 - pointPtr[1];
      if (tmp < yDiff) {
         yDiff = tmp;
      }
      if (yDiff < xDiff) {
         xDiff = yDiff;
      }
      xDiff -= width;
      if (xDiff < 0.0) {
         return 0.0;
      }
      return xDiff;
   }

   /*
   * Point is outside rectangle.
   */

   if (pointPtr[0] < x1) {
      xDiff = x1 - pointPtr[0];
   } else if (pointPtr[0] > x2)  {
      xDiff = pointPtr[0] - x2;
   } else {
      xDiff = 0;
   }

   if (pointPtr[1] < y1) {
      yDiff = y1 - pointPtr[1];
   } else if (pointPtr[1] > y2)  {
      yDiff = pointPtr[1] - y2;
   } else {
      yDiff = 0;
   }

   return hypot(xDiff, yDiff);
}

/*
 *--------------------------------------------------------------
 *
 * glOvalToPoint --
 *
 *	Computes the distance from a given point to a given
 *	oval, in canvas units.
 *
 * Results:
 *	The return value is 0 if the point whose x and y coordinates
 *	are coordPtr[0] and coordPtr[1] is inside the oval.  If the
 *	point isn't inside the oval then the return value is the
 *	distance from the point to the oval.  If itemPtr is filled,
 *	then anywhere in the interior is considered "inside"; if
 *	itemPtr isn't filled, then "inside" means only the area
 *	occupied by the outline.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static double glOvalToPoint(
   Tk_Canvas canvas,      /* Canvas containing item. */
   Tk_Item *itemPtr,      /* Item to check against point. */
   double *pointPtr)      /* Pointer to x and y coordinates. */
{
   glRectOvalItem *ovalPtr = (glRectOvalItem *) itemPtr;
   double width;
   int filled=1;
   Tk_State state = itemPtr->state;

   if(state == TK_STATE_NULL) {
      state = Canvas(canvas)->canvas_state;
   }

   width = (double) ovalPtr->outline.width;
   if (Canvas(canvas)->currentItemPtr == itemPtr) {
      if (ovalPtr->outline.activeWidth>width) {
         width = (double) ovalPtr->outline.activeWidth;
      }
   } else if (state==TK_STATE_DISABLED) {
      if (ovalPtr->outline.disabledWidth>0) {
         width = (double) ovalPtr->outline.disabledWidth;
      }
   }

   return TkOvalToPoint(ovalPtr->bbox, width, filled, pointPtr);
}

/*
 *--------------------------------------------------------------
 *
 * glRectToArea --
 *
 *	This procedure is called to determine whether an item
 *	lies entirely inside, entirely outside, or overlapping
 *	a given rectangle.
 *
 * Results:
 *	-1 is returned if the item is entirely outside the area
 *	given by rectPtr, 0 if it overlaps, and 1 if it is entirely
 *	inside the given area.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static int glRectToArea(
   Tk_Canvas canvas,      /* Canvas containing item. */
   Tk_Item *itemPtr,      /* Item to check against rectangle. */
   double *areaPtr)       /* Pointer to array of four coordinates (x1, y1, x2, y2) describing rectangular area. */
{
   glRectOvalItem *rectPtr = (glRectOvalItem *) itemPtr;
   double halfWidth;
   double width;
   Tk_State state = itemPtr->state;

   if(state == TK_STATE_NULL) {
      state = Canvas(canvas)->canvas_state;
   }

   width = rectPtr->outline.width;
   if (Canvas(canvas)->currentItemPtr == itemPtr) {
      if (rectPtr->outline.activeWidth>width) {
         width = rectPtr->outline.activeWidth;
      }
   } else if (state==TK_STATE_DISABLED) {
      if (rectPtr->outline.disabledWidth>0) {
         width = rectPtr->outline.disabledWidth;
      }
   }

   halfWidth = width/2.0;

   if ((areaPtr[2] <= (rectPtr->bbox[0] - halfWidth))
      || (areaPtr[0] >= (rectPtr->bbox[2] + halfWidth))
      || (areaPtr[3] <= (rectPtr->bbox[1] - halfWidth))
      || (areaPtr[1] >= (rectPtr->bbox[3] + halfWidth))) {
      return -1;
   }
   if (!(rectPtr->fillColor) && (rectPtr->outline.color)
      && (areaPtr[0] >= (rectPtr->bbox[0] + halfWidth))
      && (areaPtr[1] >= (rectPtr->bbox[1] + halfWidth))
      && (areaPtr[2] <= (rectPtr->bbox[2] - halfWidth))
      && (areaPtr[3] <= (rectPtr->bbox[3] - halfWidth))) {
      return -1;
   }
   if ((areaPtr[0] <= (rectPtr->bbox[0] - halfWidth))
      && (areaPtr[1] <= (rectPtr->bbox[1] - halfWidth))
      && (areaPtr[2] >= (rectPtr->bbox[2] + halfWidth))
      && (areaPtr[3] >= (rectPtr->bbox[3] + halfWidth))) {
      return 1;
   }
   return 0;
}

/*
 *--------------------------------------------------------------
 *
 * glOvalToArea --
 *
 *	This procedure is called to determine whether an item
 *	lies entirely inside, entirely outside, or overlapping
 *	a given rectangular area.
 *
 * Results:
 *	-1 is returned if the item is entirely outside the area
 *	given by rectPtr, 0 if it overlaps, and 1 if it is entirely
 *	inside the given area.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static int glOvalToArea(
   Tk_Canvas canvas,      /* Canvas containing item. */
   Tk_Item *itemPtr,      /* Item to check against oval. */
   double *areaPtr)       /* Pointer to array of four coordinates (x1, y1, x2, y2) describing rectangular area. */
{
   glRectOvalItem *ovalPtr = (glRectOvalItem *) itemPtr;
   double oval[4], halfWidth;
   int result;
   double width;
   Tk_State state = itemPtr->state;

   if(state == TK_STATE_NULL) {
      state = Canvas(canvas)->canvas_state;
   }

   width = ovalPtr->outline.width;
   if (Canvas(canvas)->currentItemPtr == itemPtr) {
      if (ovalPtr->outline.activeWidth>width) {
         width = ovalPtr->outline.activeWidth;
      }
   } else if (state==TK_STATE_DISABLED) {
      if (ovalPtr->outline.disabledWidth>0) {
         width = ovalPtr->outline.disabledWidth;
      }
   }

   /*
   * Expand the oval to include the width of the outline, if any.
   */

   halfWidth = width/2.0;
   oval[0] = ovalPtr->bbox[0] - halfWidth;
   oval[1] = ovalPtr->bbox[1] - halfWidth;
   oval[2] = ovalPtr->bbox[2] + halfWidth;
   oval[3] = ovalPtr->bbox[3] + halfWidth;

   result = TkOvalToArea(oval, areaPtr);

   /*
   * If the rectangle appears to overlap the oval and the oval
   * isn't filled, do one more check to see if perhaps all four
   * of the rectangle's corners are totally inside the oval's
   * unfilled center, in which case we should return "outside".
   */

   if ((result == 0) && (ovalPtr->outline.color)
      && !(ovalPtr->fillColor)) {
      double centerX, centerY, height;
      double xDelta1, yDelta1, xDelta2, yDelta2;

      centerX = (ovalPtr->bbox[0] + ovalPtr->bbox[2])/2.0;
      centerY = (ovalPtr->bbox[1] + ovalPtr->bbox[3])/2.0;
      width = (ovalPtr->bbox[2] - ovalPtr->bbox[0])/2.0 - halfWidth;
      height = (ovalPtr->bbox[3] - ovalPtr->bbox[1])/2.0 - halfWidth;
      xDelta1 = (areaPtr[0] - centerX)/width;
      xDelta1 *= xDelta1;
      yDelta1 = (areaPtr[1] - centerY)/height;
      yDelta1 *= yDelta1;
      xDelta2 = (areaPtr[2] - centerX)/width;
      xDelta2 *= xDelta2;
      yDelta2 = (areaPtr[3] - centerY)/height;
      yDelta2 *= yDelta2;
      if (((xDelta1 + yDelta1) < 1.0)
         && ((xDelta1 + yDelta2) < 1.0)
         && ((xDelta2 + yDelta1) < 1.0)
         && ((xDelta2 + yDelta2) < 1.0)) {
         return -1;
      }
   }
   return result;
}

/*
 *--------------------------------------------------------------
 *
 * glScaleRectOval --
 *
 *	This procedure is invoked to rescale a rectangle or oval
 *	item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The rectangle or oval referred to by itemPtr is rescaled
 *	so that the following transformation is applied to all
 *	point coordinates:
 *		x' = originX + scaleX*(x-originX)
 *		y' = originY + scaleY*(y-originY)
 *
 *--------------------------------------------------------------
 */

static void glScaleRectOval(
   Tk_Canvas canvas,      /* Canvas containing rectangle. */
   Tk_Item *itemPtr,      /* Rectangle to be scaled. */
   double originX, double originY, /* Origin about which to scale rect. */
   double scaleX,         /* Amount to scale in X direction. */
   double scaleY)         /* Amount to scale in Y direction. */
{
   glRectOvalItem *rectOvalPtr = (glRectOvalItem *) itemPtr;

   rectOvalPtr->bbox[0] = originX + scaleX*(rectOvalPtr->bbox[0] - originX);
   rectOvalPtr->bbox[1] = originY + scaleY*(rectOvalPtr->bbox[1] - originY);
   rectOvalPtr->bbox[2] = originX + scaleX*(rectOvalPtr->bbox[2] - originX);
   rectOvalPtr->bbox[3] = originY + scaleY*(rectOvalPtr->bbox[3] - originY);
   glComputeRectOvalBbox(canvas, rectOvalPtr);
}

/*
 *--------------------------------------------------------------
 *
 * glTranslateRectOval --
 *
 *	This procedure is called to move a rectangle or oval by a
 *	given amount.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The position of the rectangle or oval is offset by
 *	(xDelta, yDelta), and the bounding box is updated in the
 *	generic part of the item structure.
 *
 *--------------------------------------------------------------
 */

static void glTranslateRectOval(
   Tk_Canvas canvas,      /* Canvas containing item. */
   Tk_Item *itemPtr,      /* Item that is being moved. */
   double deltaX, double deltaY)  /* Amount by which item is to be moved. */
{
   glRectOvalItem *rectOvalPtr = (glRectOvalItem *) itemPtr;

   rectOvalPtr->bbox[0] += deltaX;
   rectOvalPtr->bbox[1] += deltaY;
   rectOvalPtr->bbox[2] += deltaX;
   rectOvalPtr->bbox[3] += deltaY;
   glComputeRectOvalBbox(canvas, rectOvalPtr);
}

/*
 *--------------------------------------------------------------
 *
 * glRectOvalToPostscript --
 *
 *	This procedure is called to generate Postscript for
 *	rectangle and oval items.
 *
 * Results:
 *	The return value is a standard Tcl result.  If an error
 *	occurs in generating Postscript then an error message is
 *	left in the interp's result, replacing whatever used to be there.
 *	If no error occurs, then Postscript for the rectangle is
 *	appended to the result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static int glRectOvalToPostscript(
   Tcl_Interp *interp,    /* Interpreter for error reporting. */
   Tk_Canvas canvas,      /* Information about overall canvas. */
   Tk_Item *itemPtr,      /* Item for which Postscript is wanted. */
   int prepass)           /* 1 means this is a prepass to collect font information; 0 means final Postscript is being created. */
{
   Tcl_Obj *pathObj, *psObj;
   glRectOvalItem *rectOvalPtr = (glRectOvalItem *) itemPtr;
   double y1, y2;
   XColor *color;
   XColor *fillColor;
   T_glBitmap *fillStipple;
   Tk_State state = itemPtr->state;
   Tcl_InterpState interpState;

   y1 = Tk_CanvasPsY(canvas, rectOvalPtr->bbox[1]);
   y2 = Tk_CanvasPsY(canvas, rectOvalPtr->bbox[3]);

   /*
   * Generate a string that creates a path for the rectangle or oval. This
   * is the only part of the function's code that is type-specific.
   */

   if (rectOvalPtr->header.typePtr == &tkglRectangleType) {
      pathObj = Tcl_ObjPrintf(
         "%.15g %.15g moveto "
         "%.15g 0 rlineto "
         "0 %.15g rlineto "
         "%.15g 0 rlineto "
         "closepath\n",
         rectOvalPtr->bbox[0], y1,
         rectOvalPtr->bbox[2]-rectOvalPtr->bbox[0],
         y2-y1,
         rectOvalPtr->bbox[0]-rectOvalPtr->bbox[2]);
   } else {
      pathObj = Tcl_ObjPrintf(
         "matrix currentmatrix\n"
         "%.15g %.15g translate "
         "%.15g %.15g scale "
         "1 0 moveto 0 0 1 0 360 arc\n"
         "setmatrix\n",
         (rectOvalPtr->bbox[0] + rectOvalPtr->bbox[2])/2, (y1 + y2)/2,
         (rectOvalPtr->bbox[2] - rectOvalPtr->bbox[0])/2, (y1 - y2)/2);
   }

   if (state == TK_STATE_NULL) {
      state = Canvas(canvas)->canvas_state;
   }
   color = rectOvalPtr->outline.color;
   fillColor = rectOvalPtr->fillColor;
   fillStipple = rectOvalPtr->fillStipple;
   if (Canvas(canvas)->currentItemPtr == itemPtr) {
      if (rectOvalPtr->outline.activeColor!=NULL) {
         color = rectOvalPtr->outline.activeColor;
      }
      if (rectOvalPtr->activeFillColor!=NULL) {
         fillColor = rectOvalPtr->activeFillColor;
      }
      if (rectOvalPtr->activeFillStipple!=None) {
         fillStipple = rectOvalPtr->activeFillStipple;
      }
   } else if (state == TK_STATE_DISABLED) {
      if (rectOvalPtr->outline.disabledColor!=NULL) {
         color = rectOvalPtr->outline.disabledColor;
      }
      if (rectOvalPtr->disabledFillColor!=NULL) {
         fillColor = rectOvalPtr->disabledFillColor;
      }
      if (rectOvalPtr->disabledFillStipple!=None) {
         fillStipple = rectOvalPtr->disabledFillStipple;
      }
   }

   /*
   * Make our working space.
   */

   psObj = Tcl_NewObj();
   interpState = Tcl_SaveInterpState(interp, TCL_OK);

   /*
   * First draw the filled area of the rectangle.
   */

   if (fillColor != NULL) {
      Tcl_AppendObjToObj(psObj, pathObj);

      Tcl_ResetResult(interp);
      if (Tk_CanvasPsColor(interp, canvas, fillColor) != TCL_OK) {
         goto error;
      }
      Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));

      if (fillStipple != None) {
         Tcl_AppendToObj(psObj, "clip ", -1);

         Tcl_ResetResult(interp);
         if (glPostscriptStipple(interp,psObj,fillStipple) != TCL_OK) {
            goto error;
         }
         Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));
         if (color != NULL) {
            Tcl_AppendToObj(psObj, "grestore gsave\n", -1);
         }
      } else {
         Tcl_AppendToObj(psObj, "fill\n", -1);
      }
   }

   /*
   * Now draw the outline, if there is one.
   */

   if (color != NULL && rectOvalPtr->outline.width>0) {
      Tcl_AppendObjToObj(psObj, pathObj);
      Tcl_AppendToObj(psObj, "0 setlinejoin 2 setlinecap\n", -1);

      Tcl_ResetResult(interp);
      if (Tk_CanvasPsOutline(canvas, itemPtr,
         &rectOvalPtr->outline)!= TCL_OK) {
         goto error;
      }
      Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));
   }

   /*
   * Plug the accumulated postscript back into the result.
   */

   (void) Tcl_RestoreInterpState(interp, interpState);
   Tcl_AppendObjToObj(Tcl_GetObjResult(interp), psObj);
   Tcl_DecrRefCount(psObj);
   Tcl_DecrRefCount(pathObj);
   return TCL_OK;

   error:
      Tcl_DiscardInterpState(interpState);
      Tcl_DecrRefCount(psObj);
      Tcl_DecrRefCount(pathObj);
      return TCL_ERROR;
}