/*
 * tkglCanvArc.c --
 *
 *	This file implements arc items for canvas widgets.
 *
 * Copyright (c) 1992-1994 The Regents of the University of California.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: tkCanvArc.c,v 1.8 2000/02/01 11:41:09 hobbs Exp $
 */

#include <stdio.h>
#include "tkPort.h"
#include "tkInt.h"
#include "tkglCanvas.h"

/* The structure below defines the record for each arc item */

typedef enum { PIESLICE_STYLE, CHORD_STYLE, ARC_STYLE } Style;

typedef struct glArcItem  {
    Tk_Item header;		/* Generic stuff that's the same for all
				 * types.  MUST BE FIRST IN STRUCTURE. */
    Tk_Outline outline;		/* Outline structure */
    double bbox[4];		/* Coordinates (x1, y1, x2, y2) of bounding
				 * box for oval of which arc is a piece. */
    double start;		/* Angle at which arc begins, in degrees
				 * between 0 and 360. */
    double extent;		/* Extent of arc (angular distance from
				 * start to end of arc) in degrees between
				 * -360 and 360. */
    double *outlinePtr;		/* Points to (x,y) coordinates for points
				 * that define one or two closed polygons
				 * representing the portion of the outline
				 * that isn't part of the arc (the V-shape
				 * for a pie slice or a line-like segment
				 * for a chord).  Malloc'ed. */
    int numOutlinePoints;	/* Number of points at outlinePtr.  Zero
				 * means no space allocated. */
    Tk_TSOffset tsoffset;
    XColor *fillColor;		/* Color for filling arc (used for drawing
				 * outline too when style is "arc").  NULL
				 * means don't fill arc. */
    XColor *activeFillColor;	/* Color for filling arc (used for drawing
				 * outline too when style is "arc" and state
				 * is "active").  NULL means use fillColor. */
    XColor *disabledFillColor;	/* Color for filling arc (used for drawing
				 * outline too when style is "arc" and state
				 * is "disabled". NULL means use fillColor */
    Style style;		/* How to draw arc: arc, chord, or pieslice. */
    double center1[2];		/* Coordinates of center of arc outline at
				 * start (see ComputeArcOutline). */
    double center2[2];		/* Coordinates of center of arc outline at
				 * start+extent (see ComputeArcOutline). */

   T_glBitmap *fillStipple;          /* Stipple bitmap for filling item. */
   T_glBitmap *activeStipple;        /* Stipple bitmap for filling item if state is active. */
   T_glBitmap *disabledStipple;      /* Stipple bitmap for filling item if state is disabled. */
   int alpha;                        /* Transparency factor */
} glArcItem;

/*
 * The definitions below define the sizes of the polygons used to
 * display outline information for various styles of arcs:
 */

#define CHORD_OUTLINE_PTS	7
#define PIE_OUTLINE1_PTS	6
#define PIE_OUTLINE2_PTS	7

/* Information used for parsing configuration specs */

static int   StyleParseProc _ANSI_ARGS_((ClientData clientData,Tcl_Interp *interp,Tk_Window tkwin,CONST char *value,char *widgRec,int offset));
static char *StylePrintProc _ANSI_ARGS_((ClientData clientData,Tk_Window tkwin,char *widgRec,int offset,Tcl_FreeProc **freeProcPtr));

static Tk_CustomOption bitmapOption = { (Tk_OptionParseProc*)glBitmapParseProc,glBitmapPrintProc,(ClientData)NULL };
static Tk_CustomOption stateOption  = { (Tk_OptionParseProc*)TkStateParseProc,TkStatePrintProc,(ClientData)2 };
static Tk_CustomOption styleOption  = { (Tk_OptionParseProc*)StyleParseProc,StylePrintProc,(ClientData)NULL };
static Tk_CustomOption tagsOption   = { (Tk_OptionParseProc*)Tk_CanvasTagsParseProc,Tk_CanvasTagsPrintProc,(ClientData)NULL };
static Tk_CustomOption dashOption   = { (Tk_OptionParseProc*)TkCanvasDashParseProc,TkCanvasDashPrintProc,(ClientData)NULL };
static Tk_CustomOption offsetOption = { (Tk_OptionParseProc*)TkOffsetParseProc,TkOffsetPrintProc,(ClientData)(TK_OFFSET_RELATIVE) };
static Tk_CustomOption pixelOption  = { (Tk_OptionParseProc*)TkPixelParseProc,TkPixelPrintProc,(ClientData)NULL };

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_CUSTOM, "-activedash", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glArcItem, outline.activeDash),
	TK_CONFIG_NULL_OK, &dashOption},
    {TK_CONFIG_COLOR, "-activefill", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glArcItem, activeFillColor),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_COLOR, "-activeoutline", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glArcItem, outline.activeColor),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_BITMAP, "-activeoutlinestipple", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glArcItem, outline.activeStipple),
	TK_CONFIG_NULL_OK},
   { TK_CONFIG_CUSTOM,"-activestipple",(char*)NULL,(char*)NULL,(char*)NULL,
      Tk_Offset(glArcItem,activeStipple),TK_CONFIG_NULL_OK,&bitmapOption },
    {TK_CONFIG_CUSTOM, "-activewidth", (char *) NULL, (char *) NULL,
	"0.0", Tk_Offset(glArcItem, outline.activeWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &pixelOption},
    {TK_CONFIG_CUSTOM, "-dash", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glArcItem, outline.dash),
	TK_CONFIG_NULL_OK, &dashOption},
    {TK_CONFIG_PIXELS, "-dashoffset", (char *) NULL, (char *) NULL,
	"0", Tk_Offset(glArcItem, outline.offset), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-disableddash", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glArcItem, outline.disabledDash),
	TK_CONFIG_NULL_OK, &dashOption},
    {TK_CONFIG_COLOR, "-disabledfill", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glArcItem, disabledFillColor),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_COLOR, "-disabledoutline", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glArcItem, outline.disabledColor),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_BITMAP, "-disabledoutlinestipple", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glArcItem, outline.disabledStipple),
	TK_CONFIG_NULL_OK},
   { TK_CONFIG_CUSTOM,"-disabledstipple",(char*)NULL,(char*)NULL,(char*)NULL,
      Tk_Offset(glArcItem,disabledStipple),TK_CONFIG_NULL_OK,&bitmapOption },
    {TK_CONFIG_CUSTOM, "-disabledwidth", (char *) NULL, (char *) NULL,
	"0.0", Tk_Offset(glArcItem, outline.disabledWidth),
	TK_CONFIG_DONT_SET_DEFAULT, &pixelOption},
    {TK_CONFIG_DOUBLE, "-extent", (char *) NULL, (char *) NULL,
	"90", Tk_Offset(glArcItem, extent), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_COLOR, "-fill", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glArcItem, fillColor), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-offset", (char *) NULL, (char *) NULL,
	"0,0", Tk_Offset(glArcItem, tsoffset),
	TK_CONFIG_DONT_SET_DEFAULT, &offsetOption},
    {TK_CONFIG_COLOR, "-outline", (char *) NULL, (char *) NULL,
	"black", Tk_Offset(glArcItem, outline.color), TK_CONFIG_NULL_OK},
    {TK_CONFIG_CUSTOM, "-outlineoffset", (char *) NULL, (char *) NULL,
	"0,0", Tk_Offset(glArcItem, outline.tsoffset),
	TK_CONFIG_DONT_SET_DEFAULT, &offsetOption},
    {TK_CONFIG_BITMAP, "-outlinestipple", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glArcItem, outline.stipple),
	TK_CONFIG_NULL_OK},
    {TK_CONFIG_DOUBLE, "-start", (char *) NULL, (char *) NULL,
	"0", Tk_Offset(glArcItem, start), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-state", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(Tk_Item, state), TK_CONFIG_NULL_OK,
	&stateOption},
   { TK_CONFIG_CUSTOM,"-stipple",(char*)NULL,(char*)NULL,(char*)NULL,
      Tk_Offset(glArcItem,fillStipple),TK_CONFIG_NULL_OK,&bitmapOption },
    {TK_CONFIG_CUSTOM, "-style", (char *) NULL, (char *) NULL,
	(char *) NULL, Tk_Offset(glArcItem, style), TK_CONFIG_DONT_SET_DEFAULT,
	&styleOption},
    {TK_CONFIG_CUSTOM, "-tags", (char *) NULL, (char *) NULL,
	(char *) NULL, 0, TK_CONFIG_NULL_OK, &tagsOption},
    {TK_CONFIG_CUSTOM, "-width", (char *) NULL, (char *) NULL,
	"1.0", Tk_Offset(glArcItem, outline.width), TK_CONFIG_DONT_SET_DEFAULT,
	&pixelOption},
    {TK_CONFIG_INT, "-transparency", (char *) NULL, (char *) NULL,
	"100", Tk_Offset(glArcItem,alpha), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
	(char *) NULL, 0, 0}
};

/* Prototypes for procedures defined in this file */

static void   glComputeArcBbox _ANSI_ARGS_((Tk_Canvas canvas,glArcItem *arcPtr));
static int    glConfigureArc _ANSI_ARGS_((Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int argc,Tcl_Obj *CONST argv[],int flags));
static int    glCreateArc _ANSI_ARGS_((Tcl_Interp *interp,Tk_Canvas canvas,struct Tk_Item *itemPtr,int argc,Tcl_Obj *CONST argv[]));
static void   glDeleteArc _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,Display *display));
static void   glDisplayArc _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,Display *display,Drawable dst,int x,int y,int width,int height));
static int    glArcCoords _ANSI_ARGS_((Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int argc,Tcl_Obj *CONST argv[]));
static int    glArcToArea _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,double *rectPtr));
static double glArcToPoint _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,double *coordPtr));
static int    glArcToPostscript _ANSI_ARGS_((Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int prepass));
static void   glScaleArc _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,double originX,double originY,double scaleX,double scaleY));
static void   glTranslateArc _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,double deltaX,double deltaY));
static int    AngleInRange _ANSI_ARGS_((double x,double y,double start,double extent));
static void   glComputeArcOutline _ANSI_ARGS_((Tk_Canvas canvas,glArcItem *arcPtr));
static int    HorizLineToArc _ANSI_ARGS_((double x1,double x2,double y,double rx,double ry,double start,double extent));
static int    VertLineToArc _ANSI_ARGS_((double x,double y1,double y2,double rx,double ry,double start,double extent));

/*
 * The structures below defines the arc item types by means of procedures
 * that can be invoked by generic item code.
 */

Tk_ItemType tkglArcType = {
    "arc",				/* name */
    sizeof(glArcItem),			/* itemSize */
    glCreateArc,			/* createProc */
    configSpecs,			/* configSpecs */
    glConfigureArc,			/* configureProc */
    glArcCoords,			/* coordProc */
    glDeleteArc,			/* deleteProc */
    glDisplayArc,			/* displayProc */
    TK_CONFIG_OBJS,			/* flags */
    glArcToPoint,			/* pointProc */
    glArcToArea,			/* areaProc */
    glArcToPostscript,			/* postscriptProc */
    glScaleArc,				/* scaleProc */
    glTranslateArc,			/* translateProc */
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
 * glCreateArc --
 *
 *	This procedure is invoked to create a new arc item in
 *	a canvas.
 *
 * Results:
 *	A standard Tcl return value.  If an error occurred in
 *	creating the item, then an error message is left in
 *	the interp's result;  in this case itemPtr is
 *	left uninitialized, so it can be safely freed by the
 *	caller.
 *
 * Side effects:
 *	A new arc item is created.
 *
 *--------------------------------------------------------------
 */

static int glCreateArc(interp, canvas, itemPtr, argc, argv)
    Tcl_Interp *interp;			/* Interpreter for error reporting. */
    Tk_Canvas canvas;			/* Canvas to hold new item. */
    Tk_Item *itemPtr;			/* Record to hold new item;  header
					 * has been initialized by caller. */
    int argc;				/* Number of arguments in argv. */
    Tcl_Obj *CONST argv[];		/* Arguments describing arc. */
{
   glArcItem *arcPtr = (glArcItem *) itemPtr;
   int i;

   if (argc==1) {
      i = 1;
   } else {
      char *arg = Tcl_GetStringFromObj(argv[1], NULL);
      if ((argc>1) && (arg[0] == '-') && (arg[1] >= 'a') && (arg[1] <= 'z')) {
         i = 1;
      } else {
         i = 4;
      }
   }

   if (argc < i) {
      Tcl_AppendResult(interp,"wrong # args: should be \"",Tk_PathName(Tk_CanvasTkwin(canvas))," create ",
         itemPtr->typePtr->name," x1 y1 x2 y2 ?options?\"",(char*)NULL);
      return TCL_ERROR;
   }

   /*
    * Carry out initialization that is needed in order to clean
    * up after errors during the the remainder of this procedure.
    */

   Tk_CreateOutline(&(arcPtr->outline));
   arcPtr->start               = 0;
   arcPtr->extent              = 90;
   arcPtr->outlinePtr          = NULL;
   arcPtr->numOutlinePoints    = 0;
   arcPtr->tsoffset.flags      = 0;
   arcPtr->tsoffset.xoffset    = 0;
   arcPtr->tsoffset.yoffset    = 0;
   arcPtr->fillColor           = NULL;
   arcPtr->activeFillColor     = NULL;
   arcPtr->disabledFillColor   = NULL;

   arcPtr->fillStipple         = NULL;
   arcPtr->activeStipple       = NULL;
   arcPtr->disabledStipple     = NULL;
   arcPtr->style               = PIESLICE_STYLE;
   arcPtr->alpha               = 100;

   /* Process the arguments to fill in the item record */

   if ((glArcCoords(interp,canvas,itemPtr,i,argv) == TCL_OK)) {
      if (glConfigureArc(interp,canvas,itemPtr,argc-4,argv+4,0) == TCL_OK) {
         return TCL_OK;
      }
   }

   glDeleteArc(canvas,itemPtr,Tk_Display(Tk_CanvasTkwin(canvas)));
   return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * glArcCoords --
 *
 *	This procedure is invoked to process the "coords" widget
 *	command on arcs.  See the user documentation for details
 *	on what it does.
 *
 * Results:
 *	Returns TCL_OK or TCL_ERROR, and sets the interp's result.
 *
 * Side effects:
 *	The coordinates for the given item may be changed.
 *
 *--------------------------------------------------------------
 */

static int glArcCoords(interp, canvas, itemPtr, argc, argv)
    Tcl_Interp *interp;			/* Used for error reporting. */
    Tk_Canvas canvas;			/* Canvas containing item. */
    Tk_Item *itemPtr;			/* Item whose coordinates are to be
					 * read or modified. */
    int argc;				/* Number of coordinates supplied in
					 * argv. */
    Tcl_Obj *CONST argv[];		/* Array of coordinates: x1, y1,
					 * x2, y2, ... */
{
    glArcItem *arcPtr = (glArcItem *) itemPtr;

    if (argc == 0) {
	Tcl_Obj *obj = Tcl_NewObj();
	Tcl_Obj *subobj = Tcl_NewDoubleObj(arcPtr->bbox[0]);
	Tcl_ListObjAppendElement(interp, obj, subobj);
	subobj = Tcl_NewDoubleObj(arcPtr->bbox[1]);
	Tcl_ListObjAppendElement(interp, obj, subobj);
	subobj = Tcl_NewDoubleObj(arcPtr->bbox[2]);
	Tcl_ListObjAppendElement(interp, obj, subobj);
	subobj = Tcl_NewDoubleObj(arcPtr->bbox[3]);
	Tcl_ListObjAppendElement(interp, obj, subobj);
	Tcl_SetObjResult(interp, obj);
    } else if ((argc == 1)||(argc == 4)) {
	if (argc==1) {
	    if (Tcl_ListObjGetElements(interp, argv[0], &argc,
		    (Tcl_Obj ***) &argv) != TCL_OK) {
		return TCL_ERROR;
	    } else if (argc != 4) {
		char buf[64 + TCL_INTEGER_SPACE];

		sprintf(buf, "wrong # coordinates: expected 4, got %d", argc);
		Tcl_SetResult(interp, buf, TCL_VOLATILE);
		return TCL_ERROR;
	    }
	}
	if ((Tk_CanvasGetCoordFromObj(interp, canvas, argv[0],
 		    &arcPtr->bbox[0]) != TCL_OK)
		|| (Tk_CanvasGetCoordFromObj(interp, canvas, argv[1],
		    &arcPtr->bbox[1]) != TCL_OK)
		|| (Tk_CanvasGetCoordFromObj(interp, canvas, argv[2],
			&arcPtr->bbox[2]) != TCL_OK)
		|| (Tk_CanvasGetCoordFromObj(interp, canvas, argv[3],
			&arcPtr->bbox[3]) != TCL_OK)) {
	    return TCL_ERROR;
	}
	glComputeArcBbox(canvas, arcPtr);
    } else {
	char buf[64 + TCL_INTEGER_SPACE];

	sprintf(buf, "wrong # coordinates: expected 0 or 4, got %d", argc);
	Tcl_SetResult(interp, buf, TCL_VOLATILE);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * glConfigureArc --
 *
 *	This procedure is invoked to configure various aspects
 *	of a arc item, such as its outline and fill colors.
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

static int glConfigureArc(interp, canvas, itemPtr, argc, argv, flags)
    Tcl_Interp *interp;		/* Used for error reporting. */
    Tk_Canvas canvas;		/* Canvas containing itemPtr. */
    Tk_Item *itemPtr;		/* Arc item to reconfigure. */
    int argc;			/* Number of elements in argv.  */
    Tcl_Obj *CONST argv[];	/* Arguments describing things to configure. */
    int flags;			/* Flags to pass to Tk_ConfigureWidget. */
{
    glArcItem *arcPtr = (glArcItem *) itemPtr;
    int i;
    Tk_Window tkwin;
    Tk_TSOffset *tsoffset;
    Tk_State state;

    tkwin = Tk_CanvasTkwin(canvas);
    if (Tk_ConfigureWidget(interp, tkwin, configSpecs, argc, (char **) argv,
	    (char *) arcPtr, flags|TK_CONFIG_OBJS) != TCL_OK) {
	return TCL_ERROR;
    }

    state = itemPtr->state;

    /*
     * A few of the options require additional processing, such as
     * style and graphics contexts.
     */

    if (arcPtr->outline.activeWidth > arcPtr->outline.width ||
	    arcPtr->outline.activeDash.number != 0 ||
	    arcPtr->outline.activeColor != NULL ||
	    arcPtr->outline.activeStipple != None ||
	    arcPtr->activeFillColor != NULL ||
	    arcPtr->activeStipple != NULL) {
	itemPtr->redraw_flags |= TK_ITEM_STATE_DEPENDANT;
    } else {
	itemPtr->redraw_flags &= ~TK_ITEM_STATE_DEPENDANT;
    }

    tsoffset = &arcPtr->outline.tsoffset;
    flags = tsoffset->flags;
    if (flags & TK_OFFSET_LEFT) {
	tsoffset->xoffset = (int) (arcPtr->bbox[0] + 0.5);
    } else if (flags & TK_OFFSET_CENTER) {
	tsoffset->xoffset = (int) ((arcPtr->bbox[0]+arcPtr->bbox[2]+1)/2);
    } else if (flags & TK_OFFSET_RIGHT) {
	tsoffset->xoffset = (int) (arcPtr->bbox[2] + 0.5);
    }
    if (flags & TK_OFFSET_TOP) {
	tsoffset->yoffset = (int) (arcPtr->bbox[1] + 0.5);
    } else if (flags & TK_OFFSET_MIDDLE) {
	tsoffset->yoffset = (int) ((arcPtr->bbox[1]+arcPtr->bbox[3]+1)/2);
    } else if (flags & TK_OFFSET_BOTTOM) {
	tsoffset->yoffset = (int) (arcPtr->bbox[2] + 0.5);
    }

    i = (int) (arcPtr->start/360.0);
    arcPtr->start -= i*360.0;
    if (arcPtr->start < 0) {
	arcPtr->start += 360.0;
    }
    i = (int) (arcPtr->extent/360.0);
    arcPtr->extent -= i*360.0;

    if(state == TK_STATE_NULL) {
	state = ((TkCanvas *)canvas)->canvas_state;
    }
    if (state==TK_STATE_HIDDEN) {
	glComputeArcBbox(canvas, arcPtr);
	return TCL_OK;
    }

    tsoffset = &arcPtr->tsoffset;
    flags = tsoffset->flags;
    if (flags & TK_OFFSET_LEFT) {
	tsoffset->xoffset = (int) (arcPtr->bbox[0] + 0.5);
    } else if (flags & TK_OFFSET_CENTER) {
	tsoffset->xoffset = (int) ((arcPtr->bbox[0]+arcPtr->bbox[2]+1)/2);
    } else if (flags & TK_OFFSET_RIGHT) {
	tsoffset->xoffset = (int) (arcPtr->bbox[2] + 0.5);
    }
    if (flags & TK_OFFSET_TOP) {
	tsoffset->yoffset = (int) (arcPtr->bbox[1] + 0.5);
    } else if (flags & TK_OFFSET_MIDDLE) {
	tsoffset->yoffset = (int) ((arcPtr->bbox[1]+arcPtr->bbox[3]+1)/2);
    } else if (flags & TK_OFFSET_BOTTOM) {
	tsoffset->yoffset = (int) (arcPtr->bbox[3] + 0.5);
    }

    arcPtr->alpha=arcPtr->alpha<0?0:arcPtr->alpha>100?100:arcPtr->alpha;

    glComputeArcBbox(canvas, arcPtr);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * glDeleteArc --
 *
 *	This procedure is called to clean up the data structure
 *	associated with a arc item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources associated with itemPtr are released.
 *
 *--------------------------------------------------------------
 */

static void glDeleteArc(canvas, itemPtr, display)
    Tk_Canvas canvas;			/* Info about overall canvas. */
    Tk_Item *itemPtr;			/* Item that is being deleted. */
    Display *display;			/* Display containing window for
					 * canvas. */
{
    glArcItem *arcPtr = (glArcItem *) itemPtr;

   Tk_DeleteOutline(display, &(arcPtr->outline));
   if (arcPtr->numOutlinePoints != 0) {
      ckfree((char *) arcPtr->outlinePtr);
   }
   if (arcPtr->fillColor != NULL) {
      Tk_FreeColor(arcPtr->fillColor);
   }
   if (arcPtr->activeFillColor != NULL) {
      Tk_FreeColor(arcPtr->activeFillColor);
   }
   if (arcPtr->disabledFillColor != NULL) {
      Tk_FreeColor(arcPtr->disabledFillColor);
   }
}

/*
 *--------------------------------------------------------------
 *
 * glComputeArcBbox --
 *
 *	This procedure is invoked to compute the bounding box of
 *	all the pixels that may be drawn as part of an arc.
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

static void glComputeArcBbox(canvas, arcPtr)
    Tk_Canvas canvas;			/* Canvas that contains item. */
    glArcItem *arcPtr;			/* Item whose bbox is to be
					 * recomputed. */
{
    double tmp, center[2], point[2];
    double width;
    Tk_State state = arcPtr->header.state;

    if(state == TK_STATE_NULL) {
	state = ((TkCanvas *)canvas)->canvas_state;
    }

    width = arcPtr->outline.width;
    if (width < 1.0) {
	width = 1.0;
    }
    if (state==TK_STATE_HIDDEN) {
	arcPtr->header.x1 = arcPtr->header.x2 =
	arcPtr->header.y1 = arcPtr->header.y2 = -1;
	return;
    } else if (((TkCanvas *)canvas)->currentItemPtr == (Tk_Item *) arcPtr) {
	if (arcPtr->outline.activeWidth>width) {
	    width = arcPtr->outline.activeWidth;
	}
    } else if (state==TK_STATE_DISABLED) {
	if (arcPtr->outline.disabledWidth>0) {
	    width = arcPtr->outline.disabledWidth;
	}
    }

    /*
     * Make sure that the first coordinates are the lowest ones.
     */

    if (arcPtr->bbox[1] > arcPtr->bbox[3]) {
	double tmp;
	tmp = arcPtr->bbox[3];
	arcPtr->bbox[3] = arcPtr->bbox[1];
	arcPtr->bbox[1] = tmp;
    }
    if (arcPtr->bbox[0] > arcPtr->bbox[2]) {
	double tmp;
	tmp = arcPtr->bbox[2];
	arcPtr->bbox[2] = arcPtr->bbox[0];
	arcPtr->bbox[0] = tmp;
    }

    glComputeArcOutline(canvas,arcPtr);

    /*
     * To compute the bounding box, start with the the bbox formed
     * by the two endpoints of the arc.  Then add in the center of
     * the arc's oval (if relevant) and the 3-o'clock, 6-o'clock,
     * 9-o'clock, and 12-o'clock positions, if they are relevant.
     */

    arcPtr->header.x1 = arcPtr->header.x2 = (int) arcPtr->center1[0];
    arcPtr->header.y1 = arcPtr->header.y2 = (int) arcPtr->center1[1];
    TkIncludePoint((Tk_Item *) arcPtr, arcPtr->center2);
    center[0] = (arcPtr->bbox[0] + arcPtr->bbox[2])/2;
    center[1] = (arcPtr->bbox[1] + arcPtr->bbox[3])/2;
    if (arcPtr->style == PIESLICE_STYLE) {
	TkIncludePoint((Tk_Item *) arcPtr, center);
    }

    tmp = -arcPtr->start;
    if (tmp < 0) {
	tmp += 360.0;
    }
    if ((tmp < arcPtr->extent) || ((tmp-360) > arcPtr->extent)) {
	point[0] = arcPtr->bbox[2];
	point[1] = center[1];
	TkIncludePoint((Tk_Item *) arcPtr, point);
    }
    tmp = 90.0 - arcPtr->start;
    if (tmp < 0) {
	tmp += 360.0;
    }
    if ((tmp < arcPtr->extent) || ((tmp-360) > arcPtr->extent)) {
	point[0] = center[0];
	point[1] = arcPtr->bbox[1];
	TkIncludePoint((Tk_Item *) arcPtr, point);
    }
    tmp = 180.0 - arcPtr->start;
    if (tmp < 0) {
	tmp += 360.0;
    }
    if ((tmp < arcPtr->extent) || ((tmp-360) > arcPtr->extent)) {
	point[0] = arcPtr->bbox[0];
	point[1] = center[1];
	TkIncludePoint((Tk_Item *) arcPtr, point);
    }
    tmp = 270.0 - arcPtr->start;
    if (tmp < 0) {
	tmp += 360.0;
    }
    if ((tmp < arcPtr->extent) || ((tmp-360) > arcPtr->extent)) {
	point[0] = center[0];
	point[1] = arcPtr->bbox[3];
	TkIncludePoint((Tk_Item *) arcPtr, point);
    }

    /*
     * Lastly, expand by the width of the arc (if the arc's outline is
     * being drawn) and add one extra pixel just for safety.
     */

    if (arcPtr->outline.gc == None) {
	tmp = 1;
    } else {
	tmp = (int) ((width + 1.0)/2.0 + 1);
    }
    arcPtr->header.x1 -= (int) tmp;
    arcPtr->header.y1 -= (int) tmp;
    arcPtr->header.x2 += (int) tmp;
    arcPtr->header.y2 += (int) tmp;
}

/*
 *--------------------------------------------------------------
 *
 * glDisplayArc --
 *
 *	This procedure is invoked to draw an arc item in a given
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

static void glDisplayArc(canvas, itemPtr, display, drawable, x, y, width, height)
    Tk_Canvas canvas;			/* Canvas that contains item. */
    Tk_Item *itemPtr;			/* Item to be displayed. */
    Display *display;			/* Display on which to draw item. */
    Drawable drawable;			/* Pixmap or window in which to draw
					 * item. */
    int x, y, width, height;		/* Describes region of canvas that
					 * must be redisplayed (not used). */
{
    glArcItem *arcPtr = (glArcItem *) itemPtr;
    short x1, y1, x2, y2;
    int dashnumber;
    double lineWidth;
    Tk_State state = itemPtr->state;
    T_glBitmap *stipple;

    if(state == TK_STATE_NULL) {
	state = ((TkCanvas *)canvas)->canvas_state;
    }
    lineWidth = arcPtr->outline.width;
    if (lineWidth < 1.0) {
	lineWidth = 1.0;
    }
    dashnumber = arcPtr->outline.dash.number;
    stipple = arcPtr->fillStipple;
    if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
	if (arcPtr->outline.activeWidth>lineWidth) {
	    lineWidth = arcPtr->outline.activeWidth;
	}
	if (arcPtr->outline.activeDash.number != 0) {
	    dashnumber = arcPtr->outline.activeDash.number;
	}
	if (arcPtr->activeStipple) {
	    stipple = arcPtr->activeStipple;
	}
    } else if (state==TK_STATE_DISABLED) {
	if (arcPtr->outline.disabledWidth > 0) {
	    lineWidth = arcPtr->outline.disabledWidth;
	}
	if (arcPtr->outline.disabledDash.number != 0) {
	    dashnumber = arcPtr->outline.disabledDash.number;
	}
	if (arcPtr->disabledStipple) {
	    stipple = arcPtr->disabledStipple;
	}
    }

   /*
    * Compute the screen coordinates of the bounding box for the item,
    * plus integer values for the angles.
    */

   Tk_CanvasDrawableCoords(canvas,arcPtr->bbox[0],arcPtr->bbox[1],&x1,&y1);
   Tk_CanvasDrawableCoords(canvas,arcPtr->bbox[2],arcPtr->bbox[3],&x2,&y2);

   if (x2 <= x1)
      x2 = x1+1;
   if (y2 <= y1)
      y2 = y1+1;

   if (arcPtr->extent!=0) {
      if(arcPtr->alpha<100) {
         glEnable(GL_BLEND);
         glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
      }

      glPushMatrix();
      glTranslated(-((TkCanvas *)canvas)->xOrigin,-((TkCanvas *)canvas)->yOrigin,0.0);
      glTranslated((x2-x1)/2.0+x1,(y2-y1)/2.0+y1,0.0);
      glScaled((x2-x1)/2.0,-(y2-y1)/2.0,0.0);

      if (arcPtr->fillColor && arcPtr->style!=ARC_STYLE) {
         if (stipple) {
            glEnable(GL_POLYGON_STIPPLE);
            glPolygonStipple(stipple->Data);
         }

         glPolygonMode(GL_FRONT,GL_FILL);
         glColor4us(arcPtr->fillColor->red,arcPtr->fillColor->green,arcPtr->fillColor->blue,arcPtr->alpha*655);
         glDrawArc(arcPtr->start,arcPtr->extent,64,GL_POLYGON,arcPtr->style);
         glDisable(GL_POLYGON_STIPPLE);
      }

      if (arcPtr->outline.color && arcPtr->outline.width>0) {
         glDash(&arcPtr->outline.dash);
         glLineWidth(arcPtr->outline.width);
         glColor4us(arcPtr->outline.color->red,arcPtr->outline.color->green,arcPtr->outline.color->blue,arcPtr->alpha*655);
         glDrawArc(arcPtr->start,arcPtr->extent,64,GL_LINE_STRIP,arcPtr->style);
         glDisable(GL_LINE_STIPPLE);
      }
      glPopMatrix();
      glDisable(GL_BLEND);
   }
}

/*
 *--------------------------------------------------------------
 *
 * glArcToPoint --
 *
 *	Computes the distance from a given point to a given
 *	arc, in canvas units.
 *
 * Results:
 *	The return value is 0 if the point whose x and y coordinates
 *	are coordPtr[0] and coordPtr[1] is inside the arc.  If the
 *	point isn't inside the arc then the return value is the
 *	distance from the point to the arc.  If itemPtr is filled,
 *	then anywhere in the interior is considered "inside"; if
 *	itemPtr isn't filled, then "inside" means only the area
 *	occupied by the outline.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static double glArcToPoint(canvas, itemPtr, pointPtr)
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item to check against point. */
    double *pointPtr;		/* Pointer to x and y coordinates. */
{
    glArcItem *arcPtr = (glArcItem *) itemPtr;
    double vertex[2], pointAngle, diff, dist, newDist;
    double poly[8], polyDist, width, t1, t2;
    int filled, angleInRange;
    Tk_State state = itemPtr->state;

    if(state == TK_STATE_NULL) {
	state = ((TkCanvas *)canvas)->canvas_state;
    }

    width = (double) arcPtr->outline.width;
    if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
	if (arcPtr->outline.activeWidth>width) {
	    width = (double) arcPtr->outline.activeWidth;
	}
    } else if (state == TK_STATE_DISABLED) {
	if (arcPtr->outline.disabledWidth>0) {
	    width = (double) arcPtr->outline.disabledWidth;
	}
    }

    /*
     * See if the point is within the angular range of the arc.
     * Remember, X angles are backwards from the way we'd normally
     * think of them.  Also, compensate for any eccentricity of
     * the oval.
     */

    vertex[0] = (arcPtr->bbox[0] + arcPtr->bbox[2])/2.0;
    vertex[1] = (arcPtr->bbox[1] + arcPtr->bbox[3])/2.0;
    t1 = arcPtr->bbox[3] - arcPtr->bbox[1];
    if (t1 != 0.0) {
	t1 = (pointPtr[1] - vertex[1]) / t1;
    }
    t2 = arcPtr->bbox[2] - arcPtr->bbox[0];
    if (t2 != 0.0) {
	t2 = (pointPtr[0] - vertex[0]) / t2;
    }
    if ((t1 == 0.0) && (t2 == 0.0)) {
	pointAngle = 0;
    } else {
	pointAngle = -atan2(t1, t2)*180/M_PI;
    }
    diff = pointAngle - arcPtr->start;
    diff -= ((int) (diff/360.0) * 360.0);
    if (diff < 0) {
	diff += 360.0;
    }
    angleInRange = (diff <= arcPtr->extent) ||
	    ((arcPtr->extent < 0) && ((diff - 360.0) >= arcPtr->extent));

    /*
     * Now perform different tests depending on what kind of arc
     * we're dealing with.
     */

    if (arcPtr->style == ARC_STYLE) {
	if (angleInRange) {
	    return TkOvalToPoint(arcPtr->bbox, width,
		    0, pointPtr);
	}
	dist = hypot(pointPtr[0] - arcPtr->center1[0],
		pointPtr[1] - arcPtr->center1[1]);
	newDist = hypot(pointPtr[0] - arcPtr->center2[0],
		pointPtr[1] - arcPtr->center2[1]);
	if (newDist < dist) {
	    return newDist;
	}
	return dist;
    }

    if (arcPtr->fillColor || !arcPtr->outline.color) {
	filled = 1;
    } else {
	filled = 0;
    }
    if (!arcPtr->outline.color) {
	width = 0.0;
    }

    if (arcPtr->style == PIESLICE_STYLE) {
	if (width > 1.0) {
	    dist = TkPolygonToPoint(arcPtr->outlinePtr, PIE_OUTLINE1_PTS,
		    pointPtr);
	    newDist = TkPolygonToPoint(arcPtr->outlinePtr + 2*PIE_OUTLINE1_PTS,
			PIE_OUTLINE2_PTS, pointPtr);
	} else {
	    dist = TkLineToPoint(vertex, arcPtr->center1, pointPtr);
	    newDist = TkLineToPoint(vertex, arcPtr->center2, pointPtr);
	}
	if (newDist < dist) {
	    dist = newDist;
	}
	if (angleInRange) {
	    newDist = TkOvalToPoint(arcPtr->bbox, width, filled, pointPtr);
	    if (newDist < dist) {
		dist = newDist;
	    }
	}
	return dist;
    }

    /*
     * This is a chord-style arc.  We have to deal specially with the
     * triangular piece that represents the difference between a
     * chord-style arc and a pie-slice arc (for small angles this piece
     * is excluded here where it would be included for pie slices;
     * for large angles the piece is included here but would be
     * excluded for pie slices).
     */

    if (width > 1.0) {
	dist = TkPolygonToPoint(arcPtr->outlinePtr, CHORD_OUTLINE_PTS,
		    pointPtr);
    } else {
	dist = TkLineToPoint(arcPtr->center1, arcPtr->center2, pointPtr);
    }
    poly[0] = poly[6] = vertex[0];
    poly[1] = poly[7] = vertex[1];
    poly[2] = arcPtr->center1[0];
    poly[3] = arcPtr->center1[1];
    poly[4] = arcPtr->center2[0];
    poly[5] = arcPtr->center2[1];
    polyDist = TkPolygonToPoint(poly, 4, pointPtr);
    if (angleInRange) {
	if ((arcPtr->extent < -180.0) || (arcPtr->extent > 180.0)
		|| (polyDist > 0.0)) {
	    newDist = TkOvalToPoint(arcPtr->bbox, width, filled, pointPtr);
	    if (newDist < dist) {
		dist = newDist;
	    }
	}
    } else {
	if ((arcPtr->extent < -180.0) || (arcPtr->extent > 180.0)) {
	    if (filled && (polyDist < dist)) {
		dist = polyDist;
	    }
	}
    }
    return dist;
}

/*
 *--------------------------------------------------------------
 *
 * glArcToArea --
 *
 *	This procedure is called to determine whether an item
 *	lies entirely inside, entirely outside, or overlapping
 *	a given area.
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

static int glArcToArea(canvas, itemPtr, rectPtr)
    Tk_Canvas canvas;		/* Canvas containing item. */
    Tk_Item *itemPtr;		/* Item to check against arc. */
    double *rectPtr;		/* Pointer to array of four coordinates
				 * (x1, y1, x2, y2) describing rectangular
				 * area.  */
{
    glArcItem *arcPtr = (glArcItem *) itemPtr;
    double rx, ry;		/* Radii for transformed oval:  these define
				 * an oval centered at the origin. */
    double tRect[4];		/* Transformed version of x1, y1, x2, y2,
				 * for coord. system where arc is centered
				 * on the origin. */
    double center[2], width, angle, tmp;
    double points[20], *pointPtr;
    int numPoints, filled;
    int inside;			/* Non-zero means every test so far suggests
				 * that arc is inside rectangle.  0 means
				 * every test so far shows arc to be outside
				 * of rectangle. */
    int newInside;
    Tk_State state = itemPtr->state;

    if(state == TK_STATE_NULL) {
	state = ((TkCanvas *)canvas)->canvas_state;
    }
    width = (double) arcPtr->outline.width;
    if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
	if (arcPtr->outline.activeWidth>width) {
	    width = (double) arcPtr->outline.activeWidth;
	}
    } else if (state==TK_STATE_DISABLED) {
	if (arcPtr->outline.disabledWidth>0) {
	    width = (double) arcPtr->outline.disabledWidth;
	}
    }

    if (arcPtr->fillColor || !arcPtr->outline.color) {
	filled = 1;
    } else {
	filled = 0;
    }
    if (!arcPtr->outline.color) {
	width = 0.0;
    }

    /*
     * Transform both the arc and the rectangle so that the arc's oval
     * is centered on the origin.
     */

    center[0] = (arcPtr->bbox[0] + arcPtr->bbox[2])/2.0;
    center[1] = (arcPtr->bbox[1] + arcPtr->bbox[3])/2.0;
    tRect[0] = rectPtr[0] - center[0];
    tRect[1] = rectPtr[1] - center[1];
    tRect[2] = rectPtr[2] - center[0];
    tRect[3] = rectPtr[3] - center[1];
    rx = arcPtr->bbox[2] - center[0] + width/2.0;
    ry = arcPtr->bbox[3] - center[1] + width/2.0;

    /*
     * Find the extreme points of the arc and see whether these are all
     * inside the rectangle (in which case we're done), partly in and
     * partly out (in which case we're done), or all outside (in which
     * case we have more work to do).  The extreme points include the
     * following, which are checked in order:
     *
     * 1. The outside points of the arc, corresponding to start and
     *	  extent.
     * 2. The center of the arc (but only in pie-slice mode).
     * 3. The 12, 3, 6, and 9-o'clock positions (but only if the arc
     *    includes those angles).
     */

    pointPtr = points;
    angle = -arcPtr->start*(M_PI/180.0);
    pointPtr[0] = rx*cos(angle);
    pointPtr[1] = ry*sin(angle);
    angle += -arcPtr->extent*(M_PI/180.0);
    pointPtr[2] = rx*cos(angle);
    pointPtr[3] = ry*sin(angle);
    numPoints = 2;
    pointPtr += 4;

    if ((arcPtr->style == PIESLICE_STYLE) && (arcPtr->extent < 180.0)) {
	pointPtr[0] = 0.0;
	pointPtr[1] = 0.0;
	numPoints++;
	pointPtr += 2;
    }

    tmp = -arcPtr->start;
    if (tmp < 0) {
	tmp += 360.0;
    }
    if ((tmp < arcPtr->extent) || ((tmp-360) > arcPtr->extent)) {
	pointPtr[0] = rx;
	pointPtr[1] = 0.0;
	numPoints++;
	pointPtr += 2;
    }
    tmp = 90.0 - arcPtr->start;
    if (tmp < 0) {
	tmp += 360.0;
    }
    if ((tmp < arcPtr->extent) || ((tmp-360) > arcPtr->extent)) {
	pointPtr[0] = 0.0;
	pointPtr[1] = -ry;
	numPoints++;
	pointPtr += 2;
    }
    tmp = 180.0 - arcPtr->start;
    if (tmp < 0) {
	tmp += 360.0;
    }
    if ((tmp < arcPtr->extent) || ((tmp-360) > arcPtr->extent)) {
	pointPtr[0] = -rx;
	pointPtr[1] = 0.0;
	numPoints++;
	pointPtr += 2;
    }
    tmp = 270.0 - arcPtr->start;
    if (tmp < 0) {
	tmp += 360.0;
    }
    if ((tmp < arcPtr->extent) || ((tmp-360) > arcPtr->extent)) {
	pointPtr[0] = 0.0;
	pointPtr[1] = ry;
	numPoints++;
    }

    /*
     * Now that we've located the extreme points, loop through them all
     * to see which are inside the rectangle.
     */

    inside = (points[0] > tRect[0]) && (points[0] < tRect[2])
	    && (points[1] > tRect[1]) && (points[1] < tRect[3]);
    for (pointPtr = points+2; numPoints > 1; pointPtr += 2, numPoints--) {
	newInside = (pointPtr[0] > tRect[0]) && (pointPtr[0] < tRect[2])
		&& (pointPtr[1] > tRect[1]) && (pointPtr[1] < tRect[3]);
	if (newInside != inside) {
	    return 0;
	}
    }

    if (inside) {
	return 1;
    }

    /*
     * So far, oval appears to be outside rectangle, but can't yet tell
     * for sure.  Next, test each of the four sides of the rectangle
     * against the bounding region for the arc.  If any intersections
     * are found, then return "overlapping".  First, test against the
     * polygon(s) forming the sides of a chord or pie-slice.
     */

    if (arcPtr->style == PIESLICE_STYLE) {
	if (width >= 1.0) {
	    if (TkPolygonToArea(arcPtr->outlinePtr, PIE_OUTLINE1_PTS,
		    rectPtr) != -1)  {
		return 0;
	    }
	    if (TkPolygonToArea(arcPtr->outlinePtr + 2*PIE_OUTLINE1_PTS,
		    PIE_OUTLINE2_PTS, rectPtr) != -1) {
		return 0;
	    }
	} else {
	    if ((TkLineToArea(center, arcPtr->center1, rectPtr) != -1) ||
		    (TkLineToArea(center, arcPtr->center2, rectPtr) != -1)) {
		return 0;
	    }
	}
    } else if (arcPtr->style == CHORD_STYLE) {
	if (width >= 1.0) {
	    if (TkPolygonToArea(arcPtr->outlinePtr, CHORD_OUTLINE_PTS,
		    rectPtr) != -1) {
		return 0;
	    }
	} else {
	    if (TkLineToArea(arcPtr->center1, arcPtr->center2,
		    rectPtr) != -1) {
		return 0;
	    }
	}
    }

    /*
     * Next check for overlap between each of the four sides and the
     * outer perimiter of the arc.  If the arc isn't filled, then also
     * check the inner perimeter of the arc.
     */

    if (HorizLineToArc(tRect[0], tRect[2], tRect[1], rx, ry, arcPtr->start,
		arcPtr->extent)
	    || HorizLineToArc(tRect[0], tRect[2], tRect[3], rx, ry,
		arcPtr->start, arcPtr->extent)
	    || VertLineToArc(tRect[0], tRect[1], tRect[3], rx, ry,
		arcPtr->start, arcPtr->extent)
	    || VertLineToArc(tRect[2], tRect[1], tRect[3], rx, ry,
		arcPtr->start, arcPtr->extent)) {
	return 0;
    }
    if ((width > 1.0) && !filled) {
	rx -= width;
	ry -= width;
	if (HorizLineToArc(tRect[0], tRect[2], tRect[1], rx, ry, arcPtr->start,
		    arcPtr->extent)
		|| HorizLineToArc(tRect[0], tRect[2], tRect[3], rx, ry,
		    arcPtr->start, arcPtr->extent)
		|| VertLineToArc(tRect[0], tRect[1], tRect[3], rx, ry,
		    arcPtr->start, arcPtr->extent)
		|| VertLineToArc(tRect[2], tRect[1], tRect[3], rx, ry,
		    arcPtr->start, arcPtr->extent)) {
	    return 0;
	}
    }

    /*
     * The arc still appears to be totally disjoint from the rectangle,
     * but it's also possible that the rectangle is totally inside the arc.
     * Do one last check, which is to check one point of the rectangle
     * to see if it's inside the arc.  If it is, we've got overlap.  If
     * it isn't, the arc's really outside the rectangle.
     */

    if (glArcToPoint(canvas, itemPtr, rectPtr) == 0.0) {
	return 0;
    }
    return -1;
}

/*
 *--------------------------------------------------------------
 *
 * glScaleArc --
 *
 *	This procedure is invoked to rescale an arc item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The arc referred to by itemPtr is rescaled so that the
 *	following transformation is applied to all point
 *	coordinates:
 *		x' = originX + scaleX*(x-originX)
 *		y' = originY + scaleY*(y-originY)
 *
 *--------------------------------------------------------------
 */

static void glScaleArc(canvas, itemPtr, originX, originY, scaleX, scaleY)
    Tk_Canvas canvas;			/* Canvas containing arc. */
    Tk_Item *itemPtr;			/* Arc to be scaled. */
    double originX, originY;		/* Origin about which to scale rect. */
    double scaleX;			/* Amount to scale in X direction. */
    double scaleY;			/* Amount to scale in Y direction. */
{
    glArcItem *arcPtr = (glArcItem *) itemPtr;

    arcPtr->bbox[0] = originX + scaleX*(arcPtr->bbox[0] - originX);
    arcPtr->bbox[1] = originY + scaleY*(arcPtr->bbox[1] - originY);
    arcPtr->bbox[2] = originX + scaleX*(arcPtr->bbox[2] - originX);
    arcPtr->bbox[3] = originY + scaleY*(arcPtr->bbox[3] - originY);
    glComputeArcBbox(canvas, arcPtr);
}

/*
 *--------------------------------------------------------------
 *
 * glTranslateArc --
 *
 *	This procedure is called to move an arc by a given amount.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The position of the arc is offset by (xDelta, yDelta), and
 *	the bounding box is updated in the generic part of the item
 *	structure.
 *
 *--------------------------------------------------------------
 */

static void glTranslateArc(canvas, itemPtr, deltaX, deltaY)
    Tk_Canvas canvas;			/* Canvas containing item. */
    Tk_Item *itemPtr;			/* Item that is being moved. */
    double deltaX, deltaY;		/* Amount by which item is to be
					 * moved. */
{
    glArcItem *arcPtr = (glArcItem *) itemPtr;

    arcPtr->bbox[0] += deltaX;
    arcPtr->bbox[1] += deltaY;
    arcPtr->bbox[2] += deltaX;
    arcPtr->bbox[3] += deltaY;
    glComputeArcBbox(canvas, arcPtr);
}

/*
 *--------------------------------------------------------------
 *
 * glComputeArcOutline --
 *
 *	This procedure creates a polygon describing everything in
 *	the outline for an arc except what's in the curved part.
 *	For a "pie slice" arc this is a V-shaped chunk, and for
 *	a "chord" arc this is a linear chunk (with cutaway corners).
 *	For "arc" arcs, this stuff isn't relevant.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The information at arcPtr->outlinePtr gets modified, and
 *	storage for arcPtr->outlinePtr may be allocated or freed.
 *
 *--------------------------------------------------------------
 */

static void glComputeArcOutline(canvas,arcPtr)
    Tk_Canvas canvas;			/* Information about overall canvas. */
    glArcItem *arcPtr;			/* Information about arc. */
{
    double sin1, cos1, sin2, cos2, angle, width, halfWidth;
    double boxWidth, boxHeight;
    double vertex[2], corner1[2], corner2[2];
    double *outlinePtr;
    Tk_State state = arcPtr->header.state;


    /*
     * Make sure that the outlinePtr array is large enough to hold
     * either a chord or pie-slice outline.
     */

    if (arcPtr->numOutlinePoints == 0) {
	arcPtr->outlinePtr = (double *) ckalloc((unsigned)
		(26 * sizeof(double)));
	arcPtr->numOutlinePoints = 22;
    }
    outlinePtr = arcPtr->outlinePtr;

    if(state == TK_STATE_NULL) {
	state = ((TkCanvas *)canvas)->canvas_state;
    }

    /*
     * First compute the two points that lie at the centers of
     * the ends of the curved arc segment, which are marked with
     * X's in the figure below:
     *
     *
     *				  * * *
     *			      *          *
     *			   *      * *      *
     *			 *    *         *    *
     *			*   *             *   *
     *			 X *               * X
     *
     * The code is tricky because the arc can be ovular in shape.
     * It computes the position for a unit circle, and then
     * scales to fit the shape of the arc's bounding box.
     *
     * Also, watch out because angles go counter-clockwise like you
     * might expect, but the y-coordinate system is inverted.  To
     * handle this, just negate the angles in all the computations.
     */

    boxWidth = arcPtr->bbox[2] - arcPtr->bbox[0];
    boxHeight = arcPtr->bbox[3] - arcPtr->bbox[1];
    angle = -arcPtr->start*M_PI/180.0;
    sin1 = sin(angle);
    cos1 = cos(angle);
    angle -= arcPtr->extent*M_PI/180.0;
    sin2 = sin(angle);
    cos2 = cos(angle);
    vertex[0] = (arcPtr->bbox[0] + arcPtr->bbox[2])/2.0;
    vertex[1] = (arcPtr->bbox[1] + arcPtr->bbox[3])/2.0;
    arcPtr->center1[0] = vertex[0] + cos1*boxWidth/2.0;
    arcPtr->center1[1] = vertex[1] + sin1*boxHeight/2.0;
    arcPtr->center2[0] = vertex[0] + cos2*boxWidth/2.0;
    arcPtr->center2[1] = vertex[1] + sin2*boxHeight/2.0;

    /*
     * Next compute the "outermost corners" of the arc, which are
     * marked with X's in the figure below:
     *
     *				  * * *
     *			      *          *
     *			   *      * *      *
     *			 *    *         *    *
     *			X   *             *   X
     *			   *               *
     *
     * The code below is tricky because it has to handle eccentricity
     * in the shape of the oval.  The key in the code below is to
     * realize that the slope of the line from arcPtr->center1 to corner1
     * is (boxWidth*sin1)/(boxHeight*cos1), and similarly for arcPtr->center2
     * and corner2.  These formulas can be computed from the formula for
     * the oval.
     */

    width = arcPtr->outline.width;
    if (((TkCanvas *)canvas)->currentItemPtr == (Tk_Item *) arcPtr) {
	if (arcPtr->outline.activeWidth>arcPtr->outline.width) {
	    width = arcPtr->outline.activeWidth;
	}
    } else if (state==TK_STATE_DISABLED) {
	if (arcPtr->outline.disabledWidth>arcPtr->outline.width) {
	    width = arcPtr->outline.disabledWidth;
	}
    }
    halfWidth = width/2.0;

    if (((boxWidth*sin1) == 0.0) && ((boxHeight*cos1) == 0.0)) {
	angle = 0.0;
    } else {
	angle = atan2(boxWidth*sin1, boxHeight*cos1);
    }
    corner1[0] = arcPtr->center1[0] + cos(angle)*halfWidth;
    corner1[1] = arcPtr->center1[1] + sin(angle)*halfWidth;
    if (((boxWidth*sin2) == 0.0) && ((boxHeight*cos2) == 0.0)) {
	angle = 0.0;
    } else {
	angle = atan2(boxWidth*sin2, boxHeight*cos2);
    }
    corner2[0] = arcPtr->center2[0] + cos(angle)*halfWidth;
    corner2[1] = arcPtr->center2[1] + sin(angle)*halfWidth;

    /*
     * For a chord outline, generate a six-sided polygon with three
     * points for each end of the chord.  The first and third points
     * for each end are butt points generated on either side of the
     * center point.  The second point is the corner point.
     */

    if (arcPtr->style == CHORD_STYLE) {
	outlinePtr[0] = outlinePtr[12] = corner1[0];
	outlinePtr[1] = outlinePtr[13] = corner1[1];
	TkGetButtPoints(arcPtr->center2, arcPtr->center1,
		width, 0, outlinePtr+10, outlinePtr+2);
	outlinePtr[4] = arcPtr->center2[0] + outlinePtr[2]
		- arcPtr->center1[0];
	outlinePtr[5] = arcPtr->center2[1] + outlinePtr[3]
		- arcPtr->center1[1];
	outlinePtr[6] = corner2[0];
	outlinePtr[7] = corner2[1];
	outlinePtr[8] = arcPtr->center2[0] + outlinePtr[10]
		- arcPtr->center1[0];
	outlinePtr[9] = arcPtr->center2[1] + outlinePtr[11]
		- arcPtr->center1[1];
    } else if (arcPtr->style == PIESLICE_STYLE) {
	/*
	 * For pie slices, generate two polygons, one for each side
	 * of the pie slice.  The first arm has a shape like this,
	 * where the center of the oval is X, arcPtr->center1 is at Y, and
	 * corner1 is at Z:
	 *
	 *	 _____________________
	 *	|		      \
	 *	|		       \
	 *	X		     Y  Z
	 *	|		       /
	 *	|_____________________/
	 *
	 */

	TkGetButtPoints(arcPtr->center1, vertex, width, 0,
		outlinePtr, outlinePtr+2);
	outlinePtr[4] = arcPtr->center1[0] + outlinePtr[2] - vertex[0];
	outlinePtr[5] = arcPtr->center1[1] + outlinePtr[3] - vertex[1];
	outlinePtr[6] = corner1[0];
	outlinePtr[7] = corner1[1];
	outlinePtr[8] = arcPtr->center1[0] + outlinePtr[0] - vertex[0];
	outlinePtr[9] = arcPtr->center1[1] + outlinePtr[1] - vertex[1];
	outlinePtr[10] = outlinePtr[0];
	outlinePtr[11] = outlinePtr[1];

	/*
	 * The second arm has a shape like this:
	 *
	 *
	 *	   ______________________
	 *	  /			  \
	 *	 /			   \
	 *	Z  Y			X  /
	 *	 \			  /
	 *	  \______________________/
	 *
	 * Similar to above X is the center of the oval/circle, Y is
	 * arcPtr->center2, and Z is corner2.  The extra jog out to the left
	 * of X is needed in or to produce a butted joint with the
	 * first arm;  the corner to the right of X is one of the
	 * first two points of the first arm, depending on extent.
	 */

	TkGetButtPoints(arcPtr->center2, vertex, width, 0,
		outlinePtr+12, outlinePtr+16);
	if ((arcPtr->extent > 180) ||
		((arcPtr->extent < 0) && (arcPtr->extent > -180))) {
	    outlinePtr[14] = outlinePtr[0];
	    outlinePtr[15] = outlinePtr[1];
	} else {
	    outlinePtr[14] = outlinePtr[2];
	    outlinePtr[15] = outlinePtr[3];
	}
	outlinePtr[18] = arcPtr->center2[0] + outlinePtr[16] - vertex[0];
	outlinePtr[19] = arcPtr->center2[1] + outlinePtr[17] - vertex[1];
	outlinePtr[20] = corner2[0];
	outlinePtr[21] = corner2[1];
	outlinePtr[22] = arcPtr->center2[0] + outlinePtr[12] - vertex[0];
	outlinePtr[23] = arcPtr->center2[1] + outlinePtr[13] - vertex[1];
	outlinePtr[24] = outlinePtr[12];
	outlinePtr[25] = outlinePtr[13];
    }
}

/*
 *--------------------------------------------------------------
 *
 * HorizLineToArc --
 *
 *	Determines whether a horizontal line segment intersects
 *	a given arc.
 *
 * Results:
 *	The return value is 1 if the given line intersects the
 *	infinitely-thin arc section defined by rx, ry, start,
 *	and extent, and 0 otherwise.  Only the perimeter of the
 *	arc is checked: interior areas (e.g. pie-slice or chord)
 *	are not checked.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static int HorizLineToArc(x1, x2, y, rx, ry, start, extent)
    double x1, x2;		/* X-coords of endpoints of line segment.
				 * X1 must be <= x2. */
    double y;			/* Y-coordinate of line segment. */
    double rx, ry;		/* These x- and y-radii define an oval
				 * centered at the origin. */
    double start, extent;	/* Angles that define extent of arc, in
				 * the standard fashion for this module. */
{
    double tmp;
    double tx, ty;		/* Coordinates of intersection point in
				 * transformed coordinate system. */
    double x;

    /*
     * Compute the x-coordinate of one possible intersection point
     * between the arc and the line.  Use a transformed coordinate
     * system where the oval is a unit circle centered at the origin.
     * Then scale back to get actual x-coordinate.
     */

    ty = y/ry;
    tmp = 1 - ty*ty;
    if (tmp < 0) {
	return 0;
    }
    tx = sqrt(tmp);
    x = tx*rx;

    /*
     * Test both intersection points.
     */

    if ((x >= x1) && (x <= x2) && AngleInRange(tx, ty, start, extent)) {
	return 1;
    }
    if ((-x >= x1) && (-x <= x2) && AngleInRange(-tx, ty, start, extent)) {
	return 1;
    }
    return 0;
}

/*
 *--------------------------------------------------------------
 *
 * VertLineToArc --
 *
 *	Determines whether a vertical line segment intersects
 *	a given arc.
 *
 * Results:
 *	The return value is 1 if the given line intersects the
 *	infinitely-thin arc section defined by rx, ry, start,
 *	and extent, and 0 otherwise.  Only the perimeter of the
 *	arc is checked: interior areas (e.g. pie-slice or chord)
 *	are not checked.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static int VertLineToArc(x, y1, y2, rx, ry, start, extent)
    double x;			/* X-coordinate of line segment. */
    double y1, y2;		/* Y-coords of endpoints of line segment.
				 * Y1 must be <= y2. */
    double rx, ry;		/* These x- and y-radii define an oval
				 * centered at the origin. */
    double start, extent;	/* Angles that define extent of arc, in
				 * the standard fashion for this module. */
{
    double tmp;
    double tx, ty;		/* Coordinates of intersection point in
				 * transformed coordinate system. */
    double y;

    /*
     * Compute the y-coordinate of one possible intersection point
     * between the arc and the line.  Use a transformed coordinate
     * system where the oval is a unit circle centered at the origin.
     * Then scale back to get actual y-coordinate.
     */

    tx = x/rx;
    tmp = 1 - tx*tx;
    if (tmp < 0) {
	return 0;
    }
    ty = sqrt(tmp);
    y = ty*ry;

    /*
     * Test both intersection points.
     */

    if ((y > y1) && (y < y2) && AngleInRange(tx, ty, start, extent)) {
	return 1;
    }
    if ((-y > y1) && (-y < y2) && AngleInRange(tx, -ty, start, extent)) {
	return 1;
    }
    return 0;
}

/*
 *--------------------------------------------------------------
 *
 * AngleInRange --
 *
 *	Determine whether the angle from the origin to a given
 *	point is within a given range.
 *
 * Results:
 *	The return value is 1 if the angle from (0,0) to (x,y)
 *	is in the range given by start and extent, where angles
 *	are interpreted in the standard way for ovals (meaning
 *	backwards from normal interpretation).  Otherwise the
 *	return value is 0.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static int AngleInRange(x, y, start, extent)
    double x, y;		/* Coordinate of point;  angle measured
				 * from origin to here, relative to x-axis. */
    double start;		/* First angle, degrees, >=0, <=360. */
    double extent;		/* Size of arc in degrees >=-360, <=360. */
{
    double diff;

    if ((x == 0.0) && (y == 0.0)) {
	return 1;
    }
    diff = -atan2(y, x);
    diff = diff*(180.0/M_PI) - start;
    while (diff > 360.0) {
	diff -= 360.0;
    }
    while (diff < 0.0) {
	diff += 360.0;
    }
    if (extent >= 0) {
	return diff <= extent;
    }
    return (diff-360.0) >= extent;
}

/*
 *--------------------------------------------------------------
 *
 * glArcToPostscript --
 *
 *	This procedure is called to generate Postscript for
 *	arc items.
 *
 * Results:
 *	The return value is a standard Tcl result.  If an error
 *	occurs in generating Postscript then an error message is
 *	left in the interp's result, replacing whatever used
 *	to be there.  If no error occurs, then Postscript for the
 *	item is appended to the result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static int glArcToPostscript(interp, canvas, itemPtr, prepass)
    Tcl_Interp *interp;			/* Leave Postscript or error message
					 * here. */
    Tk_Canvas canvas;			/* Information about overall canvas. */
    Tk_Item *itemPtr;			/* Item for which Postscript is
					 * wanted. */
    int prepass;			/* 1 means this is a prepass to
					 * collect font information;  0 means
					 * final Postscript is being created. */
{
    glArcItem *arcPtr = (glArcItem *) itemPtr;
    char buffer[400];
    double y1, y2, ang1, ang2;
    XColor *color;
    Pixmap stipple;
    XColor *fillColor;
    T_glBitmap *fillStipple;
    Tk_State state = itemPtr->state;

    y1 = Tk_CanvasPsY(canvas, arcPtr->bbox[1]);
    y2 = Tk_CanvasPsY(canvas, arcPtr->bbox[3]);
    ang1 = arcPtr->start;
    ang2 = ang1 + arcPtr->extent;
    if (ang2 < ang1) {
	ang1 = ang2;
	ang2 = arcPtr->start;
    }

    if(state == TK_STATE_NULL) {
	state = ((TkCanvas *)canvas)->canvas_state;
    }
    color = arcPtr->outline.color;
    stipple = arcPtr->outline.stipple;
    fillColor = arcPtr->fillColor;
    fillStipple = arcPtr->fillStipple;
    if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
	if (arcPtr->outline.activeColor!=NULL) {
	    color = arcPtr->outline.activeColor;
	}
	if (arcPtr->outline.activeStipple!=None) {
	    stipple = arcPtr->outline.activeStipple;
	}
	if (arcPtr->activeFillColor!=NULL) {
	    fillColor = arcPtr->activeFillColor;
	}
	if (arcPtr->activeStipple) {
	    fillStipple = arcPtr->activeStipple;
	}
    } else if (state==TK_STATE_DISABLED) {
	if (arcPtr->outline.disabledColor!=NULL) {
	    color = arcPtr->outline.disabledColor;
	}
	if (arcPtr->outline.disabledStipple!=None) {
	    stipple = arcPtr->outline.disabledStipple;
	}
	if (arcPtr->disabledFillColor!=NULL) {
	    fillColor = arcPtr->disabledFillColor;
	}
	if (arcPtr->disabledStipple) {
	    fillStipple = arcPtr->disabledStipple;
	}
    }

    /*
     * If the arc is filled, output Postscript for the interior region
     * of the arc.
     */

    if (arcPtr->fillColor) {
	sprintf(buffer, "matrix currentmatrix\n%.15g %.15g translate %.15g %.15g scale\n",
		(arcPtr->bbox[0] + arcPtr->bbox[2])/2, (y1 + y2)/2,
		(arcPtr->bbox[2] - arcPtr->bbox[0])/2, (y1 - y2)/2);
	Tcl_AppendResult(interp, buffer, (char *) NULL);
	if (arcPtr->style == CHORD_STYLE) {
	    sprintf(buffer, "0 0 1 %.15g %.15g arc closepath\nsetmatrix\n",
		    ang1, ang2);
	} else {
	    sprintf(buffer,
		    "0 0 moveto 0 0 1 %.15g %.15g arc closepath\nsetmatrix\n",
		    ang1, ang2);
	}
	Tcl_AppendResult(interp, buffer, (char *) NULL);
	if (Tk_CanvasPsColor(interp, canvas, fillColor) != TCL_OK) {
	    return TCL_ERROR;
	};
	if (fillStipple != None) {
	    Tcl_AppendResult(interp, "clip ", (char *) NULL);
	    if (glPostscriptStipple(interp,fillStipple)!= TCL_OK) {
		return TCL_ERROR;
	    }
	    if (arcPtr->outline.color) {
		Tcl_AppendResult(interp, "grestore gsave\n", (char *) NULL);
	    }
	} else {
	    Tcl_AppendResult(interp, "fill\n", (char *) NULL);
	}
    }

    /* If there's an outline for the arc, draw it */

    if (arcPtr->outline.color) {
	sprintf(buffer, "matrix currentmatrix\n%.15g %.15g translate %.15g %.15g scale\n",
		(arcPtr->bbox[0] + arcPtr->bbox[2])/2, (y1 + y2)/2,
		(arcPtr->bbox[2] - arcPtr->bbox[0])/2, (y1 - y2)/2);
	Tcl_AppendResult(interp, buffer, (char *) NULL);
	sprintf(buffer, "0 0 1 %.15g %.15g", ang1, ang2);
	Tcl_AppendResult(interp, buffer,
		" arc\nsetmatrix\n0 setlinecap\n", (char *) NULL);
	if (Tk_CanvasPsOutline(canvas, itemPtr,
		&(arcPtr->outline)) != TCL_OK) {
	    return TCL_ERROR;
	}
	if (arcPtr->style != ARC_STYLE) {
	    Tcl_AppendResult(interp, "grestore gsave\n", (char *) NULL);
	    if (arcPtr->style == CHORD_STYLE) {
		Tk_CanvasPsPath(interp, canvas, arcPtr->outlinePtr,
			CHORD_OUTLINE_PTS);
	    } else {
		Tk_CanvasPsPath(interp, canvas, arcPtr->outlinePtr,
			PIE_OUTLINE1_PTS);
		if (Tk_CanvasPsColor(interp, canvas, color)
			!= TCL_OK) {
		    return TCL_ERROR;
		}
		if (stipple != None) {
		    Tcl_AppendResult(interp, "clip ", (char *) NULL);
		    if (Tk_CanvasPsStipple(interp, canvas,
			    stipple) != TCL_OK) {
			return TCL_ERROR;
		    }
		} else {
		    Tcl_AppendResult(interp, "fill\n", (char *) NULL);
		}
		Tcl_AppendResult(interp, "grestore gsave\n", (char *) NULL);
		Tk_CanvasPsPath(interp, canvas,
			arcPtr->outlinePtr + 2*PIE_OUTLINE1_PTS,
			PIE_OUTLINE2_PTS);
	    }
	    if (Tk_CanvasPsColor(interp, canvas, color)
		    != TCL_OK) {
		return TCL_ERROR;
	    }
	    if (stipple != None) {
		Tcl_AppendResult(interp, "clip ", (char *) NULL);
		if (Tk_CanvasPsStipple(interp, canvas,
			stipple) != TCL_OK) {
		    return TCL_ERROR;
		}
	    } else {
		Tcl_AppendResult(interp, "fill\n", (char *) NULL);
	    }
	}
    }

    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * StyleParseProc --
 *
 *	This procedure is invoked during option processing to handle
 *	the "-style" option.
 *
 * Results:
 *	A standard Tcl return value.
 *
 * Side effects:
 *	The state for a given item gets replaced by the state
 *	indicated in the value argument.
 *
 *--------------------------------------------------------------
 */

static int StyleParseProc(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;		/* some flags.*/
    Tcl_Interp *interp;			/* Used for reporting errors. */
    Tk_Window tkwin;			/* Window containing canvas widget. */
    CONST char *value;			/* Value of option. */
    char *widgRec;			/* Pointer to record for item. */
    int offset;				/* Offset into item. */
{
    int c;
    size_t length;

    register Style *stylePtr = (Style *) (widgRec + offset);

    if(value == NULL || *value == 0) {
	*stylePtr = PIESLICE_STYLE;
	return TCL_OK;
    }

    c = value[0];
    length = strlen(value);

    if ((c == 'a') && (strncmp(value, "arc", length) == 0)) {
	*stylePtr = ARC_STYLE;
	return TCL_OK;
    }
    if ((c == 'c') && (strncmp(value, "chord", length) == 0)) {
	*stylePtr = CHORD_STYLE;
	return TCL_OK;
    }
    if ((c == 'p') && (strncmp(value, "pieslice", length) == 0)) {
	*stylePtr = PIESLICE_STYLE;
	return TCL_OK;
    }

    Tcl_AppendResult(interp, "bad -style option \"",
	    value, "\": must be arc, chord, or pieslice",
	    (char *) NULL);
    *stylePtr = PIESLICE_STYLE;
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * StylePrintProc --
 *
 *	This procedure is invoked by the Tk configuration code
 *	to produce a printable string for the "-style"
 *	configuration option.
 *
 * Results:
 *	The return value is a string describing the state for
 *	the item referred to by "widgRec".  In addition, *freeProcPtr
 *	is filled in with the address of a procedure to call to free
 *	the result string when it's no longer needed (or NULL to
 *	indicate that the string doesn't need to be freed).
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static char *StylePrintProc(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;		/* Ignored. */
    Tk_Window tkwin;			/* Ignored. */
    char *widgRec;			/* Pointer to record for item. */
    int offset;				/* Offset into item. */
    Tcl_FreeProc **freeProcPtr;		/* Pointer to variable to fill in with
					 * information about how to reclaim
					 * storage for return string. */
{
    register Style *stylePtr = (Style *) (widgRec + offset);

    if (*stylePtr==ARC_STYLE) {
	return "arc";
    } else if (*stylePtr==CHORD_STYLE) {
	return "chord";
    } else {
	return "pieslice";
    }
}
