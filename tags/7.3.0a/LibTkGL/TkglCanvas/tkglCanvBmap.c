/*
 * tkCanvBmap.c --
 *
 *	This file implements bitmap items for canvas widgets.
 *
 * Copyright (c) 1992-1994 The Regents of the University of California.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: tkCanvBmap.c,v 1.4 1999/12/14 06:52:25 hobbs Exp $
 */

#include <stdio.h>
#include "tkInt.h"
#include "tkPort.h"
#include "tkglCanvas.h"

/* The structure below defines the record for each bitmap item */

typedef struct glBitmapItem  {
    Tk_Item header;           /* Generic stuff that's the same for all
                               * types.  MUST BE FIRST IN STRUCTURE */
    double x, y;              /* Coordinates of positioning point for bitmap */
    Tk_Anchor anchor;         /* Where to anchor bitmap relative to x,y */
    XColor *fgColor;          /* Foreground color to use for bitmap */
    XColor *activeFgColor;    /* Foreground color to use for bitmap */
    XColor *disabledFgColor;  /* Foreground color to use for bitmap */
    XColor *bgColor;          /* Background color to use for bitmap */
    XColor *activeBgColor;    /* Background color to use for bitmap */
    XColor *disabledBgColor;  /* Background color to use for bitmap */
    T_glBitmap *bitmap;             /* Bitmap to display in window */
    T_glBitmap *activeBitmap;       /* Bitmap to display in window */
    T_glBitmap *disabledBitmap;     /* Bitmap to display in window */
    int    alpha;                   /* Transparency */
} glBitmapItem;

/* Information used for parsing configuration specs: */

static Tk_CustomOption stateOption  = { (Tk_OptionParseProc*)TkStateParseProc,TkStatePrintProc,(ClientData)2 };
static Tk_CustomOption tagsOption   = { (Tk_OptionParseProc*)Tk_CanvasTagsParseProc,Tk_CanvasTagsPrintProc,(ClientData)NULL };
static Tk_CustomOption bitmapOption = { (Tk_OptionParseProc*)glBitmapParseProc,glBitmapPrintProc,(ClientData)NULL };

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_COLOR, "-activebackground", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glBitmapItem, activeBgColor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-activebitmap", (char *) NULL, (char *) NULL,
	(char *) NULL,Tk_Offset(glBitmapItem,activeBitmap),TK_CONFIG_NULL_OK,&bitmapOption},
    {TK_CONFIG_COLOR, "-activeforeground", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glBitmapItem, activeFgColor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_ANCHOR, "-anchor", (char *) NULL, (char *) NULL,
	"center", Tk_Offset(glBitmapItem, anchor), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_COLOR, "-background", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glBitmapItem, bgColor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-bitmap", (char *) NULL, (char *) NULL,
	(char *) NULL,Tk_Offset(glBitmapItem,bitmap),TK_CONFIG_NULL_OK,&bitmapOption},
    {TK_CONFIG_COLOR, "-disabledbackground", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glBitmapItem, disabledBgColor),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-disabledbitmap", (char *) NULL, (char *) NULL,
	(char *) NULL,Tk_Offset(glBitmapItem,disabledBitmap),TK_CONFIG_NULL_OK,&bitmapOption},
    {TK_CONFIG_COLOR, "-disabledforeground", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glBitmapItem, disabledFgColor),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_COLOR, "-foreground", (char *) NULL, (char *) NULL,
	"black", Tk_Offset(glBitmapItem, fgColor), 0},
    {TK_CONFIG_CUSTOM, "-state", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(Tk_Item, state), TK_CONFIG_NULL_OK,
	&stateOption},
     {TK_CONFIG_INT, "-transparency", (char *) NULL, (char *) NULL,
	"100", Tk_Offset(glBitmapItem, alpha), TK_CONFIG_DONT_SET_DEFAULT},
   {TK_CONFIG_CUSTOM, "-tags", (char *) NULL, (char *) NULL,
	(char *) NULL, 0, TK_CONFIG_NULL_OK, &tagsOption},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
	(char *) NULL, 0, 0}
};

/* Prototypes for procedures defined in this file */

static int    glBitmapCoords _ANSI_ARGS_((Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int argc,Tcl_Obj *CONST argv[]));
static int    glBitmapToArea _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,double *rectPtr));
static double glBitmapToPoint _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,double *coordPtr));
static int    glBitmapToPostscript _ANSI_ARGS_((Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int prepass));
static void   glComputeBitmapBbox _ANSI_ARGS_((Tk_Canvas canvas,glBitmapItem *bmapPtr));
static int    glConfigureBitmap _ANSI_ARGS_((Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int argc,Tcl_Obj *CONST argv[],int flags));
static int    glCreateBitmap _ANSI_ARGS_((Tcl_Interp *interp,Tk_Canvas canvas,struct Tk_Item *itemPtr,int argc,Tcl_Obj *CONST argv[]));
static void   glDeleteBitmap _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,Display *display));
static void   glDisplayBitmap _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,Display *display,Drawable dst,int x,int y,int width,int height));
static void   glScaleBitmap _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,double originX,double originY, double scaleX,double scaleY));
static void   glTranslateBitmap _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,double deltaX,double deltaY));

/*
 * The structures below defines the bitmap item type in terms of
 * procedures that can be invoked by generic item code.
 */

Tk_ItemType tkglBitmapType = {
    "bitmap",				/* name */
    sizeof(glBitmapItem),		/* itemSize */
    glCreateBitmap,			/* createProc */
    configSpecs,			/* configSpecs */
    glConfigureBitmap,			/* configureProc */
    glBitmapCoords,			/* coordProc */
    glDeleteBitmap,			/* deleteProc */
    glDisplayBitmap,			/* displayProc */
    TK_CONFIG_OBJS,			/* flags */
    glBitmapToPoint,			/* pointProc */
    glBitmapToArea,			/* areaProc */
    glBitmapToPostscript,		/* postscriptProc */
    glScaleBitmap,			/* scaleProc */
    glTranslateBitmap,			/* translateProc */
    (Tk_ItemIndexProc *) NULL,		/* indexProc */
    (Tk_ItemCursorProc *) NULL,		/* icursorProc */
    (Tk_ItemSelectionProc *) NULL,	/* selectionProc */
    (Tk_ItemInsertProc *) NULL,		/* insertProc */
    (Tk_ItemDCharsProc *) NULL,		/* dTextProc */
    (Tk_ItemType *) NULL,		/* nextPtr */
};

/*
 *--------------------------------------------------------------
 *
 * glCreateBitmap --
 *
 *	This procedure is invoked to create a new bitmap
 *	item in a canvas.
 *
 * Results:
 *	A standard Tcl return value.  If an error occurred in
 *	creating the item, then an error message is left in
 *	the interp's result;  in this case itemPtr is left uninitialized,
 *	so it can be safely freed by the caller.
 *
 * Side effects:
 *	A new bitmap item is created.
 *
 *--------------------------------------------------------------
 */

static int glCreateBitmap(interp, canvas, itemPtr, argc, argv)
    Tcl_Interp *interp;			/* Interpreter for error reporting. */
    Tk_Canvas canvas;			/* Canvas to hold new item. */
    Tk_Item *itemPtr;			/* Record to hold new item;  header
					 * has been initialized by caller. */
    int argc;				/* Number of arguments in argv. */
    Tcl_Obj *CONST argv[];		/* Arguments describing rectangle. */
{
   glBitmapItem *bmapPtr = (glBitmapItem*)itemPtr;
   int i;

   if (argc==1) {
      i = 1;
   } else {
      char *arg = Tcl_GetStringFromObj(argv[1], NULL);
      if (((argc>1) && (arg[0] == '-') && (arg[1] >= 'a') && (arg[1] <= 'z'))) {
         i = 1;
      } else {
         i = 2;
      }
   }

   if (argc < i) {
      Tcl_AppendResult(interp,"wrong # args: should be \"",Tk_PathName(Tk_CanvasTkwin(canvas))," create ",
         itemPtr->typePtr->name," x y ?options?\"",(char*)NULL);
      return TCL_ERROR;
   }

    /* Initialize item's record */

   bmapPtr->anchor          = TK_ANCHOR_CENTER;
   bmapPtr->fgColor         = NULL;
   bmapPtr->activeFgColor   = NULL;
   bmapPtr->disabledFgColor = NULL;
   bmapPtr->bgColor         = NULL;
   bmapPtr->activeBgColor   = NULL;
   bmapPtr->disabledBgColor = NULL;
   bmapPtr->bitmap          = NULL;
   bmapPtr->activeBitmap    = NULL;
   bmapPtr->disabledBitmap  = NULL;
   bmapPtr->alpha           = 100;

    /* Process the arguments to fill in the item record */

   if ((glBitmapCoords(interp,canvas,itemPtr,i,argv) == TCL_OK)) {
      if (glConfigureBitmap(interp,canvas,itemPtr,argc-i,argv+i,0) == TCL_OK) {
         return TCL_OK;
      }
   }

    glDeleteBitmap(canvas, itemPtr, Tk_Display(Tk_CanvasTkwin(canvas)));
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * glBitmapCoords --
 *
 *	This procedure is invoked to process the "coords" widget
 *	command on bitmap items.  See the user documentation for
 *	details on what it does.
 *
 * Results:
 *	Returns TCL_OK or TCL_ERROR, and sets the interp's result.
 *
 * Side effects:
 *	The coordinates for the given item may be changed.
 *
 *--------------------------------------------------------------
 */

static int glBitmapCoords(interp, canvas, itemPtr, argc, argv)
    Tcl_Interp *interp;			/* Used for error reporting. */
    Tk_Canvas canvas;			/* Canvas containing item. */
    Tk_Item *itemPtr;			/* Item whose coordinates are to be
					 * read or modified. */
    int argc;				/* Number of coordinates supplied in
					 * argv. */
    Tcl_Obj *CONST argv[];		/* Array of coordinates: x1, y1,
					 * x2, y2, ... */
{
    glBitmapItem *bmapPtr = (glBitmapItem *) itemPtr;

    if (argc == 0) {
	Tcl_Obj *obj = Tcl_NewObj();
	Tcl_Obj *subobj = Tcl_NewDoubleObj(bmapPtr->x);
	Tcl_ListObjAppendElement(interp, obj, subobj);
	subobj = Tcl_NewDoubleObj(bmapPtr->y);
	Tcl_ListObjAppendElement(interp, obj, subobj);
	Tcl_SetObjResult(interp, obj);
    } else if (argc <3) {
	if (argc==1) {
	    if (Tcl_ListObjGetElements(interp, argv[0], &argc,
		    (Tcl_Obj ***) &argv) != TCL_OK) {
		return TCL_ERROR;
	    } else if (argc != 2) {
		char buf[64 + TCL_INTEGER_SPACE];

		sprintf(buf, "wrong # coordinates: expected 2, got %d", argc);
		Tcl_SetResult(interp, buf, TCL_VOLATILE);
		return TCL_ERROR;
	    }
	}
	if ((Tk_CanvasGetCoordFromObj(interp, canvas, argv[0], &bmapPtr->x) != TCL_OK)
		|| (Tk_CanvasGetCoordFromObj(interp, canvas, argv[1], &bmapPtr->y)
 		    != TCL_OK)) {
	    return TCL_ERROR;
	}
	glComputeBitmapBbox(canvas, bmapPtr);
    } else {
	char buf[64 + TCL_INTEGER_SPACE];

	sprintf(buf, "wrong # coordinates: expected 0 or 2, got %d", argc);
	Tcl_SetResult(interp, buf, TCL_VOLATILE);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * glConfigureBitmap --
 *
 *	This procedure is invoked to configure various aspects
 *	of a bitmap item, such as its anchor position.
 *
 * Results:
 *	A standard Tcl result code.  If an error occurs, then
 *	an error message is left in the interp's result.
 *
 * Side effects:
 *	Configuration information may be set for itemPtr.
 *
 *--------------------------------------------------------------
 */

static int glConfigureBitmap(interp, canvas, itemPtr, argc, argv, flags)
    Tcl_Interp *interp;		/* Used for error reporting. */
    Tk_Canvas canvas;		/* Canvas containing itemPtr. */
    Tk_Item *itemPtr;		/* Bitmap item to reconfigure. */
    int argc;			/* Number of elements in argv.  */
    Tcl_Obj *CONST argv[];	/* Arguments describing things to configure. */
    int flags;			/* Flags to pass to Tk_ConfigureWidget. */
{
    glBitmapItem *bmapPtr = (glBitmapItem *) itemPtr;
    Tk_State state;

    if (Tk_ConfigureWidget(interp, Tk_CanvasTkwin(canvas), configSpecs, argc, (char **) argv,
	    (char *) bmapPtr, flags|TK_CONFIG_OBJS) != TCL_OK) {
	return TCL_ERROR;
    }

    /*
     * A few of the options require additional processing, such as those
     * that determine the graphics context.
     */

    state = itemPtr->state;

    if (bmapPtr->activeFgColor!=NULL ||
	    bmapPtr->activeBgColor!=NULL ||
	    bmapPtr->activeBitmap!=NULL) {
	itemPtr->redraw_flags |= TK_ITEM_STATE_DEPENDANT;
    } else {
	itemPtr->redraw_flags &= ~TK_ITEM_STATE_DEPENDANT;
    }

    bmapPtr->alpha=bmapPtr->alpha<0?0:bmapPtr->alpha>100?100:bmapPtr->alpha;

    glComputeBitmapBbox(canvas, bmapPtr);

    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * glDeleteBitmap --
 *
 *	This procedure is called to clean up the data structure
 *	associated with a bitmap item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources associated with itemPtr are released.
 *
 *--------------------------------------------------------------
 */

static void glDeleteBitmap(canvas, itemPtr, display)
    Tk_Canvas canvas;			/* Info about overall canvas widget. */
    Tk_Item *itemPtr;			/* Item that is being deleted. */
    Display *display;			/* Display containing window for
					 * canvas. */
{
    glBitmapItem *bmapPtr = (glBitmapItem *) itemPtr;

    if (bmapPtr->fgColor != NULL) {
	Tk_FreeColor(bmapPtr->fgColor);
    }
    if (bmapPtr->activeFgColor != NULL) {
	Tk_FreeColor(bmapPtr->activeFgColor);
    }
    if (bmapPtr->disabledFgColor != NULL) {
	Tk_FreeColor(bmapPtr->disabledFgColor);
    }
    if (bmapPtr->bgColor != NULL) {
	Tk_FreeColor(bmapPtr->bgColor);
    }
    if (bmapPtr->activeBgColor != NULL) {
	Tk_FreeColor(bmapPtr->activeBgColor);
    }
    if (bmapPtr->disabledBgColor != NULL) {
	Tk_FreeColor(bmapPtr->disabledBgColor);
    }
}

/*
 *--------------------------------------------------------------
 *
 * glComputeBitmapBbox --
 *
 *	This procedure is invoked to compute the bounding box of
 *	all the pixels that may be drawn as part of a bitmap item.
 *	This procedure is where the child bitmap's placement is
 *	computed.
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

static void glComputeBitmapBbox(canvas, bmapPtr)
    Tk_Canvas canvas;			/* Canvas that contains item. */
    glBitmapItem *bmapPtr;		/* Item whose bbox is to be
					 * recomputed. */
{
    int x, y;
    T_glBitmap *bitmap;
    Tk_State state = bmapPtr->header.state;

    if(state == TK_STATE_NULL) {
       state=((TkCanvas *)canvas)->canvas_state;
    }
    bitmap = bmapPtr->bitmap;

    if (((TkCanvas *)canvas)->currentItemPtr == (Tk_Item *)bmapPtr) {
       if (bmapPtr->activeBitmap) {
	  bitmap = bmapPtr->activeBitmap;
       }
    } else if (state==TK_STATE_DISABLED) {
       if (bmapPtr->disabledBitmap) {
          bitmap = bmapPtr->disabledBitmap;
       }
    }

    x = (int) (bmapPtr->x + ((bmapPtr->x >= 0) ? 0.5 : - 0.5));
    y = (int) (bmapPtr->y + ((bmapPtr->y >= 0) ? 0.5 : - 0.5));

    if (state==TK_STATE_HIDDEN || !bitmap) {
	bmapPtr->header.x1 = bmapPtr->header.x2 = x;
	bmapPtr->header.y1 = bmapPtr->header.y2 = y;
	return;
    }

    /*
     * Compute location and size of bitmap, using anchor information.
     */

    switch (bmapPtr->anchor) {
	case TK_ANCHOR_N:
	    x -= bitmap->Width/2;
	    break;
	case TK_ANCHOR_NE:
	    x -= bitmap->Width;
	    break;
	case TK_ANCHOR_E:
	    x -= bitmap->Width;
	    y -= bitmap->Height/2;
	    break;
	case TK_ANCHOR_SE:
	    x -= bitmap->Width;
	    y -= bitmap->Height;
	    break;
	case TK_ANCHOR_S:
	    x -= bitmap->Width/2;
	    y -= bitmap->Height;
	    break;
	case TK_ANCHOR_SW:
	    y -= bitmap->Height;
	    break;
	case TK_ANCHOR_W:
	    y -= bitmap->Height/2;
	    break;
	case TK_ANCHOR_NW:
	    break;
	case TK_ANCHOR_CENTER:
	    x -= bitmap->Width/2;
	    y -= bitmap->Height/2;
	    break;
    }

    /*
     * Store the information in the item header.
     */

    bmapPtr->header.x1 = x;
    bmapPtr->header.y1 = y;
    bmapPtr->header.x2 = x + bitmap->Width;
    bmapPtr->header.y2 = y + bitmap->Height;
}

/*
 *--------------------------------------------------------------
 *
 * glDisplayBitmap --
 *
 *	This procedure is invoked to draw a bitmap item in a given
 *	drawable.
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

static void
glDisplayBitmap(canvas, itemPtr, display, drawable, x, y, width, height)
    Tk_Canvas canvas;			/* Canvas that contains item. */
    Tk_Item *itemPtr;			/* Item to be displayed. */
    Display *display;			/* Display on which to draw item. */
    Drawable drawable;			/* Pixmap or window in which to draw item. */
    int x, y, width, height;		/* Describes region of canvas that
					 * must be redisplayed (not used). */
{
   glBitmapItem *bmapPtr=(glBitmapItem*)itemPtr;
   XColor *fgColor;
   XColor *bgColor;
   T_glBitmap *bitmap;
   Tk_State state = itemPtr->state;

   if (state == TK_STATE_NULL) {
      state = ((TkCanvas *)canvas)->canvas_state;
   }

   fgColor = bmapPtr->fgColor;
   bgColor = bmapPtr->bgColor;
   bitmap  = bmapPtr->bitmap;

   /* Get the Bitmap dimensions and color by considering the state*/
   if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {

      if (bmapPtr->activeFgColor!=NULL)
         fgColor = bmapPtr->activeFgColor;

      if (bmapPtr->activeBgColor!=NULL)
         bgColor = bmapPtr->activeBgColor;

      if (bmapPtr->activeBitmap) {
         bitmap = bmapPtr->activeBitmap;
      }

   } else if (state==TK_STATE_DISABLED) {

      if (bmapPtr->disabledFgColor!=NULL)
         fgColor = bmapPtr->disabledFgColor;

      if (bmapPtr->disabledBgColor!=NULL)
         bgColor = bmapPtr->disabledBgColor;

      if (bmapPtr->disabledBitmap) {
         bitmap = bmapPtr->disabledBitmap;
      }
   }

   if (bitmap) {
      if (bmapPtr->alpha<100) {
         glEnable(GL_BLEND);
         glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
      }

      glPixelStorei(GL_UNPACK_LSB_FIRST,True);
      glPixelStorei(GL_UNPACK_ALIGNMENT,1);

      if (bgColor) {
         glColor4us(bgColor->red,bgColor->green,bgColor->blue,bmapPtr->alpha*655);
         glPolygonMode(GL_FRONT,GL_FILL);
         glBegin(GL_QUADS);
            glVertex2i(bmapPtr->header.x1,bmapPtr->header.y1);
            glVertex2i(bmapPtr->header.x1,bmapPtr->header.y2);
            glVertex2i(bmapPtr->header.x2,bmapPtr->header.y2);
            glVertex2i(bmapPtr->header.x2,bmapPtr->header.y1);
         glEnd();
      }

      glColor4us(fgColor->red,fgColor->green,fgColor->blue,bmapPtr->alpha*655);
      trRasterPos2i((bmapPtr->header.x1-((TkCanvas *)canvas)->xOrigin),-(bmapPtr->header.y1-((TkCanvas *)canvas)->yOrigin)-bitmap->Height);
      glBitmap(bitmap->Width,bitmap->Height,0.0,0.0,0.0,0.0,(GLubyte *)bitmap->Data);
      glPixelStorei(GL_UNPACK_LSB_FIRST,False);
      glPixelStorei(GL_UNPACK_ALIGNMENT,1);
      glDisable(GL_BLEND);
   }
}

/*
 *--------------------------------------------------------------
 *
 * glBitmapToPoint --
 *
 *	Computes the distance from a given point to a given
 *	rectangle, in canvas units.
 *
 * Results:
 *	The return value is 0 if the point whose x and y coordinates
 *	are coordPtr[0] and coordPtr[1] is inside the bitmap.  If the
 *	point isn't inside the bitmap then the return value is the
 *	distance from the point to the bitmap.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static double glBitmapToPoint(canvas, itemPtr, coordPtr)
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item to check against point. */
    double *coordPtr;		/* Pointer to x and y coordinates. */
{
    glBitmapItem *bmapPtr = (glBitmapItem *) itemPtr;
    double x1, x2, y1, y2, xDiff, yDiff;

    x1 = bmapPtr->header.x1;
    y1 = bmapPtr->header.y1;
    x2 = bmapPtr->header.x2;
    y2 = bmapPtr->header.y2;

    /*
     * Point is outside rectangle.
     */

    if (coordPtr[0] < x1) {
	xDiff = x1 - coordPtr[0];
    } else if (coordPtr[0] > x2)  {
	xDiff = coordPtr[0] - x2;
    } else {
	xDiff = 0;
    }

    if (coordPtr[1] < y1) {
	yDiff = y1 - coordPtr[1];
    } else if (coordPtr[1] > y2)  {
	yDiff = coordPtr[1] - y2;
    } else {
	yDiff = 0;
    }

    return hypot(xDiff, yDiff);
}

/*
 *--------------------------------------------------------------
 *
 * glBitmapToArea --
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

static int glBitmapToArea(canvas, itemPtr, rectPtr)
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item to check against rectangle. */
    double *rectPtr;		/* Pointer to array of four coordinates
				 * (x1, y1, x2, y2) describing rectangular
				 * area.  */
{
    glBitmapItem *bmapPtr = (glBitmapItem *) itemPtr;

    if ((rectPtr[2] <= bmapPtr->header.x1)
	    || (rectPtr[0] >= bmapPtr->header.x2)
	    || (rectPtr[3] <= bmapPtr->header.y1)
	    || (rectPtr[1] >= bmapPtr->header.y2)) {
	return -1;
    }
    if ((rectPtr[0] <= bmapPtr->header.x1)
	    && (rectPtr[1] <= bmapPtr->header.y1)
	    && (rectPtr[2] >= bmapPtr->header.x2)
	    && (rectPtr[3] >= bmapPtr->header.y2)) {
	return 1;
    }
    return 0;
}

/*
 *--------------------------------------------------------------
 *
 * glScaleBitmap --
 *
 *	This procedure is invoked to rescale a bitmap item in a
 *	canvas.  It is one of the standard item procedures for
 *	bitmap items, and is invoked by the generic canvas code.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The item referred to by itemPtr is rescaled so that the
 *	following transformation is applied to all point coordinates:
 *		x' = originX + scaleX*(x-originX)
 *		y' = originY + scaleY*(y-originY)
 *
 *--------------------------------------------------------------
 */

static void glScaleBitmap(canvas, itemPtr, originX, originY, scaleX, scaleY)
    Tk_Canvas canvas;			/* Canvas containing rectangle. */
    Tk_Item *itemPtr;			/* Rectangle to be scaled. */
    double originX, originY;		/* Origin about which to scale item. */
    double scaleX;			/* Amount to scale in X direction. */
    double scaleY;			/* Amount to scale in Y direction. */
{
    glBitmapItem *bmapPtr = (glBitmapItem *) itemPtr;

    bmapPtr->x = originX + scaleX*(bmapPtr->x - originX);
    bmapPtr->y = originY + scaleY*(bmapPtr->y - originY);
    glComputeBitmapBbox(canvas, bmapPtr);
}

/*
 *--------------------------------------------------------------
 *
 * glTranslateBitmap --
 *
 *	This procedure is called to move an item by a given amount.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The position of the item is offset by (xDelta, yDelta), and
 *	the bounding box is updated in the generic part of the item
 *	structure.
 *
 *--------------------------------------------------------------
 */

static void glTranslateBitmap(canvas, itemPtr, deltaX, deltaY)
    Tk_Canvas canvas;			/* Canvas containing item. */
    Tk_Item *itemPtr;			/* Item that is being moved. */
    double deltaX, deltaY;		/* Amount by which item is to be
					 * moved. */
{
    glBitmapItem *bmapPtr = (glBitmapItem *) itemPtr;

    bmapPtr->x += deltaX;
    bmapPtr->y += deltaY;
    glComputeBitmapBbox(canvas, bmapPtr);
}

/*
 *--------------------------------------------------------------
 *
 * glBitmapToPostscript --
 *
 *	This procedure is called to generate Postscript for
 *	bitmap items.
 *
 * Results:
 *	The return value is a standard Tcl result.  If an error
 *	occurs in generating Postscript then an error message is
 *	left in the interp's result, replacing whatever used to be there.
 *	If no error occurs, then Postscript for the item is appended
 *	to the result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static int glBitmapToPostscript(interp, canvas, itemPtr, prepass)
    Tcl_Interp *interp;			/* Leave Postscript or error message
					 * here. */
    Tk_Canvas canvas;			/* Information about overall canvas. */
    Tk_Item *itemPtr;			/* Item for which Postscript is
					 * wanted. */
    int prepass;			/* 1 means this is a prepass to
					 * collect font information;  0 means
					 * final Postscript is being created. */
{
    glBitmapItem *bmapPtr = (glBitmapItem *) itemPtr;
    double x, y;
    int width, height;
    char buffer[100 + TCL_DOUBLE_SPACE * 2 + TCL_INTEGER_SPACE * 4];

    if (!bmapPtr->bitmap) {
	return TCL_OK;
    }

    if (prepass) {
	return TCL_OK;
    }

    /*
     * Compute the coordinates of the lower-left corner of the bitmap,
     * taking into account the anchor position for the bitmp.
     */

    x = bmapPtr->x;
    y = Tk_CanvasPsY(canvas, bmapPtr->y);
    width=bmapPtr->bitmap->Width;
    height=bmapPtr->bitmap->Height;
    switch (bmapPtr->anchor) {
	case TK_ANCHOR_NW:			y -= height;		break;
	case TK_ANCHOR_N:	x -= width/2.0; y -= height;		break;
	case TK_ANCHOR_NE:	x -= width;	y -= height;		break;
	case TK_ANCHOR_E:	x -= width;	y -= height/2.0;	break;
	case TK_ANCHOR_SE:	x -= width;				break;
	case TK_ANCHOR_S:	x -= width/2.0;				break;
	case TK_ANCHOR_SW:						break;
	case TK_ANCHOR_W:			y -= height/2.0;	break;
	case TK_ANCHOR_CENTER:	x -= width/2.0; y -= height/2.0;	break;
    }

    /*Color the background, if there is one */

    if (bmapPtr->bgColor) {
	sprintf(buffer,"%.15g %.15g moveto %d 0 rlineto 0 %d rlineto %d %s\n",
		x, y,width, height,-width,"0 rlineto closepath");
	Tcl_AppendResult(interp,buffer,(char*)NULL);
	if (Tk_CanvasPsColor(interp,canvas,bmapPtr->bgColor) != TCL_OK) {
	    return TCL_ERROR;
	}
	Tcl_AppendResult(interp,"fill\n",(char*)NULL);
    }

    /*
     * Draw the bitmap, if there is a foreground color.  If the bitmap
     * is very large, then chop it up into multiple bitmaps, each
     * consisting of one or more rows.  This is needed because Postscript
     * can't handle single strings longer than 64 KBytes long.
     */

    if (bmapPtr->fgColor) {
	if (Tk_CanvasPsColor(interp,canvas,bmapPtr->fgColor) != TCL_OK) {
	    return TCL_ERROR;
	}
	sprintf(buffer,"%.15g %.15g translate %d %d true matrix {\n",x,y,width,height);
	Tcl_AppendResult(interp,buffer,(char*)NULL);
	if (glPostscriptBitmap(interp,bmapPtr->bitmap,width,height) != TCL_OK) {
	   return TCL_ERROR;
	}
        Tcl_AppendResult(interp, "\n} imagemask\n", (char *) NULL);
    }
    return TCL_OK;
}
