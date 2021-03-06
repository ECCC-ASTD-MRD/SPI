/*
 * tkglCanvLine.c --
 *
 * This file implements line items for canvas widgets.
 *
 * Copyright (c) 1991-1994 The Regents of the University of California.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 * Copyright (c) 1998-1999 by Scriptics Corporation.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: tkCanvLine.c,v 1.7.2.1 2001/04/04 07:57:16 hobbs Exp $
 */

#include "tkglCanvas.h"

/* The structure below defines the record for each line item */

typedef enum { ARROWS_NONE,ARROWS_FIRST,ARROWS_LAST,ARROWS_BOTH } Arrows;

typedef struct glLineItem  {
    Tk_Item header;     /* Generic stuff that's the same for all
             * types.  MUST BE FIRST IN STRUCTURE. */
    Tk_Outline outline;    /* Outline structure */
    Tk_Canvas canvas;      /* Canvas containing item.  Needed for
             * parsing arrow shapes. */
    int numPoints;      /* Number of points in line (always >= 0). */
    double *coordPtr;      /* Pointer to malloc-ed array containing
             * x- and y-coords of all points in line.
             * X-coords are even-valued indices, y-coords
             * are corresponding odd-valued indices. If
             * the line has arrowheads then the first
             * and last points have been adjusted to refer
             * to the necks of the arrowheads rather than
             * their tips.  The actual endpoints are
             * stored in the *firstArrowPtr and
             * *lastArrowPtr, if they exist. */
    int capStyle;    /* Cap style for line. */
    int joinStyle;      /* Join style for line. */
    Arrows arrow;    /* Indicates whether or not to draw arrowheads:
             * "none", "first", "last", or "both". */
    float arrowShapeA;     /* Distance from tip of arrowhead to center. */
    float arrowShapeB;     /* Distance from tip of arrowhead to trailing
             * point, measured along shaft. */
    float arrowShapeC;     /* Distance of trailing points from outside
             * edge of shaft. */
    double *firstArrowPtr; /* Points to array of PTS_IN_ARROW points
             * describing polygon for arrowhead at first
             * point in line.  First point of arrowhead
             * is tip.  Malloc'ed.  NULL means no arrowhead
             * at first point. */
    double *lastArrowPtr;  /* Points to polygon for arrowhead at last
             * point in line (PTS_IN_ARROW points, first
             * of which is tip).  Malloc'ed.  NULL means
             * no arrowhead at last point. */
    Tk_SmoothMethod *smooth;  /* Non-zero means draw line smoothed (i.e.
             * with Bezier splines). */
    int splineSteps;    /* Number of steps in each spline segment. */
    int alpha;                  /* Transparency factor */
    int numSmooth;      /* Number of vertex in line (always >= 0). */
    double *SmoothPtr;     /* Pointer to malloc-ed array containing*/
} glLineItem;

/* Number of points in an arrowHead */

#define PTS_IN_ARROW 6

/* Prototypes for procedures defined in this file */

static int    ArrowheadPostscript(Tcl_Interp *interp,Tk_Canvas canvas,glLineItem *linePtr,double *arrowPtr,Tcl_Obj *psObj);
static int    ConfigureArrows(Tk_Canvas canvas,glLineItem *linePtr);
static int    ArrowParseProc(ClientData clientData,Tcl_Interp *interp,Tk_Window tkwin,const char *value,char *recordPtr,int offset);
static const char*  ArrowPrintProc(ClientData clientData,Tk_Window tkwin,char *recordPtr,int offset,Tcl_FreeProc **freeProcPtr);
static int    ParseArrowShape(ClientData clientData,Tcl_Interp *interp,Tk_Window tkwin,const char *value,char *recordPtr,int offset);
static const char*  PrintArrowShape(ClientData clientData,Tk_Window tkwin,char *recordPtr,int offset,Tcl_FreeProc **freeProcPtr);

static void   glComputeLine(Tk_Canvas canvas,glLineItem *linePtr);
static void   glComputeLineBbox(Tk_Canvas canvas,glLineItem *linePtr);
static int    glConfigureLine(Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int objc,Tcl_Obj *const objv[],int flags);
static int    glCreateLine(Tcl_Interp *interp,Tk_Canvas canvas,struct Tk_Item *itemPtr,int objc,Tcl_Obj *const objv[]);
static void   glDeleteLine(Tk_Canvas canvas,Tk_Item *itemPtr,Display *display);
static void   glDisplayLine(Tk_Canvas canvas,Tk_Item *itemPtr,Display *display,Drawable dst,int x,int y,int width,int height);
static int    glGetLineIndex(Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,Tcl_Obj *obj,int *indexPtr);
static int    glLineCoords(Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int objc,Tcl_Obj *const objv[]);
static void   glLineDeleteCoords(Tk_Canvas canvas,Tk_Item *itemPtr,int first,int last);
static void   glLineInsert(Tk_Canvas canvas,Tk_Item *itemPtr,int beforeThis,Tcl_Obj *obj);
static int    glLineToArea(Tk_Canvas canvas,Tk_Item *itemPtr,double *rectPtr);
static double glLineToPoint(Tk_Canvas canvas,Tk_Item *itemPtr,double *coordPtr);
static int    glLineToPostscript(Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int prepass);
static void   glScaleLine(Tk_Canvas canvas,Tk_Item *itemPtr,double originX,double originY,double scaleX,double scaleY);
static void   glTranslateLine(Tk_Canvas canvas,Tk_Item *itemPtr,double deltaX,double deltaY);

/*
 * Information used for parsing configuration specs.  If you change any
 * of the default strings, be sure to change the corresponding default
 * values in CreateLine.
 */

static const Tk_CustomOption arrowShapeOption = { ParseArrowShape,PrintArrowShape,NULL };
static const Tk_CustomOption arrowOption      = { ArrowParseProc,ArrowPrintProc,NULL };
static const Tk_CustomOption smoothOption     = { TkSmoothParseProc,TkSmoothPrintProc,NULL };
static const Tk_CustomOption stateOption      = { TkStateParseProc,TkStatePrintProc,INT2PTR(2) };
static const Tk_CustomOption tagsOption       = { Tk_CanvasTagsParseProc,Tk_CanvasTagsPrintProc,NULL };
static const Tk_CustomOption dashOption       = { TkCanvasDashParseProc,TkCanvasDashPrintProc,NULL };
static const Tk_CustomOption offsetOption     = { TkOffsetParseProc,TkOffsetPrintProc,INT2PTR(TK_OFFSET_RELATIVE|TK_OFFSET_INDEX) };
static const Tk_CustomOption pixelOption      = { TkPixelParseProc,TkPixelPrintProc,NULL };

static const Tk_ConfigSpec configSpecs[] = {
   { TK_CONFIG_CUSTOM, "-activedash", NULL, NULL, NULL, Tk_Offset(glLineItem, outline.activeDash), TK_CONFIG_NULL_OK, &dashOption },
   { TK_CONFIG_COLOR, "-activefill", NULL, NULL, NULL, Tk_Offset(glLineItem, outline.activeColor), TK_CONFIG_NULL_OK },
   { TK_CONFIG_BITMAP, "-activestipple", NULL, NULL, NULL, Tk_Offset(glLineItem, outline.activeStipple), TK_CONFIG_NULL_OK },
   { TK_CONFIG_CUSTOM, "-activewidth", NULL, NULL, "0.0", Tk_Offset(glLineItem, outline.activeWidth), TK_CONFIG_DONT_SET_DEFAULT, &pixelOption },
   { TK_CONFIG_CUSTOM, "-arrow", NULL, NULL, "none", Tk_Offset(glLineItem, arrow), TK_CONFIG_DONT_SET_DEFAULT, &arrowOption },
   { TK_CONFIG_CUSTOM, "-arrowshape", NULL, NULL, "8 10 3", Tk_Offset(glLineItem, arrowShapeA), TK_CONFIG_DONT_SET_DEFAULT, &arrowShapeOption },
   { TK_CONFIG_CAP_STYLE, "-capstyle", NULL, NULL, "butt", Tk_Offset(glLineItem, capStyle), TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_COLOR, "-fill", NULL, NULL, "black", Tk_Offset(glLineItem, outline.color), TK_CONFIG_NULL_OK },
   { TK_CONFIG_CUSTOM, "-dash", NULL, NULL, NULL, Tk_Offset(glLineItem, outline.dash), TK_CONFIG_NULL_OK, &dashOption },
   { TK_CONFIG_PIXELS, "-dashoffset", NULL, NULL, "0", Tk_Offset(glLineItem, outline.offset), TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_CUSTOM, "-disableddash", NULL, NULL,  NULL, Tk_Offset(glLineItem, outline.disabledDash), TK_CONFIG_NULL_OK, &dashOption },
   { TK_CONFIG_COLOR, "-disabledfill", NULL, NULL, NULL, Tk_Offset(glLineItem, outline.disabledColor), TK_CONFIG_NULL_OK },
   { TK_CONFIG_BITMAP, "-disabledstipple", NULL, NULL, NULL, Tk_Offset(glLineItem, outline.disabledStipple), TK_CONFIG_NULL_OK },
   { TK_CONFIG_CUSTOM, "-disabledwidth", NULL, NULL, "0.0", Tk_Offset(glLineItem, outline.disabledWidth), TK_CONFIG_DONT_SET_DEFAULT, &pixelOption },
   { TK_CONFIG_JOIN_STYLE, "-joinstyle", NULL, NULL, "round", Tk_Offset(glLineItem, joinStyle), TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_CUSTOM, "-offset", NULL, NULL, "0,0", Tk_Offset(glLineItem, outline.tsoffset), TK_CONFIG_DONT_SET_DEFAULT, &offsetOption },
   { TK_CONFIG_CUSTOM, "-smooth", NULL, NULL, "0", Tk_Offset(glLineItem, smooth), TK_CONFIG_DONT_SET_DEFAULT, &smoothOption },
   { TK_CONFIG_INT, "-splinesteps", NULL, NULL, "12", Tk_Offset(glLineItem, splineSteps), TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_CUSTOM, "-state", NULL, NULL, NULL, Tk_Offset(Tk_Item, state), TK_CONFIG_NULL_OK, &stateOption },
   { TK_CONFIG_BITMAP, "-stipple", NULL, NULL, NULL, Tk_Offset(glLineItem, outline.stipple), TK_CONFIG_NULL_OK },
   { TK_CONFIG_CUSTOM, "-tags", NULL, NULL, NULL, 0, TK_CONFIG_NULL_OK, &tagsOption },
   { TK_CONFIG_CUSTOM, "-width", NULL, NULL, "1.0", Tk_Offset(glLineItem, outline.width), TK_CONFIG_DONT_SET_DEFAULT, &pixelOption },
   { TK_CONFIG_INT, "-transparency", NULL, NULL, "100", Tk_Offset(glLineItem, alpha), TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0 }
};

/*
 * The structures below defines the line item type by means
 * of procedures that can be invoked by generic item code.
 */

Tk_ItemType tkglLineType = {
    "line",                /* name */
    sizeof(glLineItem),    /* itemSize */
    glCreateLine,          /* createProc */
    configSpecs,           /* configSpecs */
    glConfigureLine,       /* configureProc */
    glLineCoords,          /* coordProc */
    glDeleteLine,          /* deleteProc */
    glDisplayLine,         /* displayProc */
    TK_CONFIG_OBJS,        /* flags */
    glLineToPoint,         /* pointProc */
    glLineToArea,          /* areaProc */
    glLineToPostscript,    /* postscriptProc */
    glScaleLine,           /* scaleProc */
    glTranslateLine,       /* translateProc */
    glGetLineIndex,        /* indexProc */
    NULL,                  /* icursorProc */
    NULL,                  /* selectionProc */
    glLineInsert,          /* insertProc */
    glLineDeleteCoords,    /* dTextProc */
    NULL,                  /* nextPtr */
};

/*
 *--------------------------------------------------------------
 *
 * CreateLine --
 *
 * This procedure is invoked to create a new line item in
 * a canvas.
 *
 * Results:
 * A standard Tcl return value.  If an error occurred in
 * creating the item, then an error message is left in
 * the interp's result;  in this case itemPtr is left uninitialized,
 * so it can be safely freed by the caller.
 *
 * Side effects:
 * A new line item is created.
 *
 *--------------------------------------------------------------
 */

static int glCreateLine(
   Tcl_Interp *interp,    /* Interpreter for error reporting. */
   Tk_Canvas canvas,      /* Canvas to hold new item. */
   Tk_Item *itemPtr,      /* Record to hold new item; header has been initialized by caller. */
   int objc,        /* Number of arguments in objv. */
   Tcl_Obj *const objv[]) /* Arguments describing line. */
{
   glLineItem *linePtr = (glLineItem*)itemPtr;
   int i;

   if (objc == 0) {
      Tcl_Panic("canvas did not pass any coords");
   }


   /*
   * Carry out initialization that is needed to set defaults and to
   * allow proper cleanup after errors during the the remainder of
   * this procedure.
   */

   Tk_CreateOutline(&(linePtr->outline));
   linePtr->canvas        = canvas;
   linePtr->numPoints     = 0;
   linePtr->coordPtr      = NULL;
   linePtr->capStyle      = CapButt;
   linePtr->joinStyle     = JoinRound;
   linePtr->arrow         = ARROWS_NONE;
   linePtr->arrowShapeA   = (float)8.0;
   linePtr->arrowShapeB   = (float)10.0;
   linePtr->arrowShapeC   = (float)3.0;
   linePtr->firstArrowPtr = NULL;
   linePtr->lastArrowPtr  = NULL;
   linePtr->smooth        = NULL;
   linePtr->splineSteps   = 12;
   linePtr->alpha         = 100;
   linePtr->numSmooth     = 0;
   linePtr->SmoothPtr     = NULL;

   /*
   * Count the number of points and then parse them into a point
   * array.  Leading arguments are assumed to be points if they
   * start with a digit or a minus sign followed by a digit.
   */

   for (i = 1; i < objc; i++) {
      const char *arg = Tcl_GetString(objv[i]);

      if ((arg[0] == '-') && (arg[1] >= 'a') && (arg[1] <= 'z')) {
         break;
      }
   }

   if (i && (glLineCoords(interp,canvas,itemPtr,i,objv) == TCL_OK)) {
      if (glConfigureLine(interp,canvas,itemPtr,objc-i,objv+i,0) == TCL_OK) {
         return TCL_OK;
      }
   }

   glDeleteLine(canvas, itemPtr, Tk_Display(Tk_CanvasTkwin(canvas)));
   return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * glLineCoords --
 *
 * This procedure is invoked to process the "coords" widget
 * command on lines.  See the user documentation for details
 * on what it does.
 *
 * Results:
 * Returns TCL_OK or TCL_ERROR, and sets the interp's result.
 *
 * Side effects:
 * The coordinates for the given item may be changed.
 *
 *--------------------------------------------------------------
 */

static int glLineCoords(
   Tcl_Interp *interp,    /* Used for error reporting. */
   Tk_Canvas canvas,      /* Canvas containing item. */
   Tk_Item *itemPtr,      /* Item whose coordinates are to be read or * modified. */
   int objc,        /* Number of coordinates supplied in objv. */
   Tcl_Obj *const objv[]) /* Array of coordinates: x1, y1, x2, y2, ... */
{
   glLineItem *linePtr = (glLineItem *) itemPtr;
   int i, numPoints;
   double *coordPtr;

   if (objc == 0) {
      int numCoords;
      Tcl_Obj *subobj, *obj = Tcl_NewObj();

      numCoords = 2*linePtr->numPoints;
      if (linePtr->firstArrowPtr != NULL) {
         coordPtr = linePtr->firstArrowPtr;
      } else {
         coordPtr = linePtr->coordPtr;
      }
      for (i = 0; i < numCoords; i++, coordPtr++) {
         if (i == 2) {
            coordPtr = linePtr->coordPtr+2;
         }
         if ((linePtr->lastArrowPtr != NULL) && (i == (numCoords-2))) {
            coordPtr = linePtr->lastArrowPtr;
         }
         subobj = Tcl_NewDoubleObj(*coordPtr);
         Tcl_ListObjAppendElement(interp, obj, subobj);
      }
      Tcl_SetObjResult(interp, obj);
      return TCL_OK;
   }
   if (objc == 1) {
      if (Tcl_ListObjGetElements(interp, objv[0], &objc,
         (Tcl_Obj ***) &objv) != TCL_OK) {
         return TCL_ERROR;
      }
   }

   /* Give internal/smoothed coordinates */
   if (objc==1 && strcasecmp(Tcl_GetString(objv[0]),"smooth")==0) {
      Tcl_Obj *subobj, *obj = Tcl_NewObj();

      if (linePtr->smooth) {
         for (i = 0; i < 2*linePtr->numSmooth; i++) {
            subobj = Tcl_NewDoubleObj(linePtr->SmoothPtr[i]);
            Tcl_ListObjAppendElement(interp, obj, subobj);
         }
      }
      Tcl_SetObjResult(interp, obj);
      return TCL_OK;     
   }
   
   if (objc & 1) {
      Tcl_SetObjResult(interp, Tcl_ObjPrintf(
         "wrong # coordinates: expected an even number, got %d",
         objc));
      Tcl_SetErrorCode(interp, "TK", "CANVAS", "COORDS", "LINE", NULL);
      return TCL_ERROR;
   } else if (objc < 4) {
      Tcl_SetObjResult(interp, Tcl_ObjPrintf(
         "wrong # coordinates: expected at least 4, got %d", objc));
      Tcl_SetErrorCode(interp, "TK", "CANVAS", "COORDS", "LINE", NULL);
      return TCL_ERROR;
   }

   numPoints = objc/2;
   if (linePtr->numPoints != numPoints) {
      coordPtr = ckalloc(sizeof(double) * objc);
      if (linePtr->coordPtr != NULL) {
         ckfree(linePtr->coordPtr);
      }
      linePtr->coordPtr = coordPtr;
      linePtr->numPoints = numPoints;
   }
   coordPtr = linePtr->coordPtr;
   for (i = 0; i < objc ; i++) {
      if (Tk_CanvasGetCoordFromObj(interp, canvas, objv[i],
         coordPtr++) != TCL_OK) {
         return TCL_ERROR;
      }
   }

   /*
   * Update arrowheads by throwing away any existing arrow-head information
   * and calling ConfigureArrows to recompute it.
   */

   if (linePtr->firstArrowPtr != NULL) {
      ckfree(linePtr->firstArrowPtr);
      linePtr->firstArrowPtr = NULL;
   }
   if (linePtr->lastArrowPtr != NULL) {
      ckfree(linePtr->lastArrowPtr);
      linePtr->lastArrowPtr = NULL;
   }
   if (linePtr->arrow != ARROWS_NONE) {
      ConfigureArrows(canvas, linePtr);
   }
   glComputeLineBbox(canvas, linePtr);
   return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * glConfigureLine --
 *
 * This procedure is invoked to configure various aspects
 * of a line item such as its background color.
 *
 * Results:
 * A standard Tcl result code.  If an error occurs, then
 * an error message is left in the interp's result.
 *
 * Side effects:
 * Configuration information, such as colors and stipple
 * patterns, may be set for itemPtr.
 *
 *--------------------------------------------------------------
 */

static int glConfigureLine(
   Tcl_Interp *interp,    /* Used for error reporting. */
   Tk_Canvas canvas,      /* Canvas containing itemPtr. */
   Tk_Item *itemPtr,      /* Line item to reconfigure. */
   int objc,        /* Number of elements in objv. */
   Tcl_Obj *const objv[], /* Arguments describing things to configure. */
   int flags)       /* Flags to pass to Tk_ConfigureWidget. */
{
   glLineItem *linePtr = (glLineItem *) itemPtr;
   Tk_Window tkwin;
   Tk_State state;

   tkwin = Tk_CanvasTkwin(canvas);
   if (Tk_ConfigureWidget(interp, tkwin, configSpecs, objc, (const char **) objv,
      (char *) linePtr, flags|TK_CONFIG_OBJS) != TCL_OK) {
      return TCL_ERROR;
   }

   state=itemPtr->state;

   if(state==TK_STATE_NULL) {
      state =((TkCanvas *)canvas)->canvas_state;
   }

   if (linePtr->outline.activeWidth > linePtr->outline.width ||
         linePtr->outline.activeDash.number != 0 ||
         linePtr->outline.activeColor != NULL ||
         linePtr->outline.activeStipple != None) {
      itemPtr->redraw_flags |= TK_ITEM_STATE_DEPENDANT;
   } else {
      itemPtr->redraw_flags &= ~TK_ITEM_STATE_DEPENDANT;
   }

   if ((!linePtr->numPoints) || (state==TK_STATE_HIDDEN)) {
      glComputeLineBbox(canvas, linePtr);
      return TCL_OK;
   }

   /*
   * Setup arrowheads, if needed.  If arrowheads are turned off,
   * restore the line's endpoints (they were shortened when the
   * arrowheads were added).
   */

   if ((linePtr->firstArrowPtr != NULL) && (linePtr->arrow != ARROWS_FIRST)
         && (linePtr->arrow != ARROWS_BOTH)) {
      linePtr->coordPtr[0] = linePtr->firstArrowPtr[0];
      linePtr->coordPtr[1] = linePtr->firstArrowPtr[1];
      ckfree((char *) linePtr->firstArrowPtr);
      linePtr->firstArrowPtr = NULL;
   }
   if ((linePtr->lastArrowPtr != NULL) && (linePtr->arrow != ARROWS_LAST)
         && (linePtr->arrow != ARROWS_BOTH)) {
      int i;

      i = 2*(linePtr->numPoints-1);
      linePtr->coordPtr[i] = linePtr->lastArrowPtr[0];
      linePtr->coordPtr[i+1] = linePtr->lastArrowPtr[1];
      ckfree((char *) linePtr->lastArrowPtr);
      linePtr->lastArrowPtr = NULL;
   }
   if (linePtr->arrow != ARROWS_NONE) {
      ConfigureArrows(canvas, linePtr);
   }

   linePtr->alpha=linePtr->alpha<0?0:linePtr->alpha>100?100:linePtr->alpha;
   linePtr->splineSteps=linePtr->splineSteps<1?1:linePtr->splineSteps>100?100:linePtr->splineSteps;

   /* Recompute bounding box for line */

   glComputeLineBbox(canvas, linePtr);

   return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * glDeleteLine --
 *
 * This procedure is called to clean up the data structure
 * associated with a line item.
 *
 * Results:
 * None.
 *
 * Side effects:
 * Resources associated with itemPtr are released.
 *
 *--------------------------------------------------------------
 */

static void glDeleteLine(
   Tk_Canvas canvas,      /* Info about overall canvas widget. */
   Tk_Item *itemPtr,      /* Item that is being deleted. */
   Display *display)      /* Display containing window for canvas. */
{
   glLineItem *linePtr = (glLineItem *) itemPtr;

   Tk_DeleteOutline(display,&(linePtr->outline));
   if (linePtr->coordPtr)
      ckfree((char *)linePtr->coordPtr);

   if (linePtr->SmoothPtr)
      ckfree((char *)linePtr->SmoothPtr);

   if (linePtr->firstArrowPtr)
      ckfree((char *)linePtr->firstArrowPtr);

   if (linePtr->lastArrowPtr)
      ckfree((char *)linePtr->lastArrowPtr);
}

/*
 *--------------------------------------------------------------
 *
 * glComputeLine --
 *
 *
 *--------------------------------------------------------------
 */
static void glComputeLine(Tk_Canvas canvas,glLineItem *linePtr) {

   if (linePtr->SmoothPtr) {
      ckfree((char*)linePtr->SmoothPtr);
      linePtr->SmoothPtr=NULL;
      linePtr->numSmooth=0;
   }
   if (linePtr->smooth && linePtr->numPoints>2) {
      linePtr->numSmooth=linePtr->smooth->coordProc(canvas,(double *)NULL,linePtr->numPoints,linePtr->splineSteps,(XPoint*)NULL,(double*)NULL);
      linePtr->SmoothPtr=(double*)ckalloc((unsigned)(2*linePtr->numSmooth*sizeof(double)));
      linePtr->numSmooth=linePtr->smooth->coordProc(canvas,linePtr->coordPtr,linePtr->numPoints,linePtr->splineSteps,(XPoint*)NULL,linePtr->SmoothPtr);
   }
}

/*
//  *--------------------------------------------------------------
 *
 * glComputeLineBbox --
 *
 * This procedure is invoked to compute the bounding box of
 * all the pixels that may be drawn as part of a line.
 *
 * Results:
 * None.
 *
 * Side effects:
 * The fields x1, y1, x2, and y2 are updated in the header
 * for itemPtr.
 *
 *--------------------------------------------------------------
 */

static void
glComputeLineBbox(
   Tk_Canvas canvas,      /* Canvas that contains item. */
   glLineItem *linePtr)     /* Item whose bbos is to be recomputed. */
{
   double *coordPtr;
   int i, intWidth;
   double width;
   Tk_State state = linePtr->header.state;
   Tk_TSOffset *tsoffset;

   glComputeLine(canvas,linePtr);

   if(state == TK_STATE_NULL) {
      state = ((TkCanvas *)canvas)->canvas_state;
   }

   if (!(linePtr->numPoints) || (state==TK_STATE_HIDDEN)) {
      linePtr->header.x1 = -1;
      linePtr->header.x2 = -1;
      linePtr->header.y1 = -1;
      linePtr->header.y2 = -1;
      return;
   }

   width = linePtr->outline.width;
   if (((TkCanvas *)canvas)->currentItemPtr == (Tk_Item *)linePtr) {
      if (linePtr->outline.activeWidth>width) {
         width = linePtr->outline.activeWidth;
      }
   } else if (state==TK_STATE_DISABLED) {
      if (linePtr->outline.disabledWidth>0) {
         width = linePtr->outline.disabledWidth;
      }
   }

   coordPtr = linePtr->coordPtr;
   linePtr->header.x1 = linePtr->header.x2 = (int) *coordPtr;
   linePtr->header.y1 = linePtr->header.y2 = (int) coordPtr[1];

   /*
   * Compute the bounding box of all the points in the line,
   * then expand in all directions by the line's width to take
   * care of butting or rounded corners and projecting or
   * rounded caps.  This expansion is an overestimate (worst-case
   * is square root of two over two) but it's simple.  Don't do
   * anything special for curves.  This causes an additional
   * overestimate in the bounding box, but is faster.
   */

   for (i = 1, coordPtr = linePtr->coordPtr+2; i < linePtr->numPoints;
         i++, coordPtr += 2) {
      TkIncludePoint((Tk_Item *) linePtr, coordPtr);
   }
   width = linePtr->outline.width;
   if (width < 1.0) {
      width = 1.0;
   }
   if (linePtr->arrow != ARROWS_NONE) {
      if (linePtr->arrow != ARROWS_LAST) {
         TkIncludePoint((Tk_Item *) linePtr, linePtr->firstArrowPtr);
      }
      if (linePtr->arrow != ARROWS_FIRST) {
         TkIncludePoint((Tk_Item *) linePtr, linePtr->lastArrowPtr);
      }
   }

   tsoffset = &linePtr->outline.tsoffset;
   if (tsoffset->flags & TK_OFFSET_INDEX) {
      double *coordPtr = linePtr->coordPtr + (tsoffset->flags & ~TK_OFFSET_INDEX);
      if (tsoffset->flags <= 0) {
         coordPtr = linePtr->coordPtr;
         if ((linePtr->arrow == ARROWS_FIRST) || (linePtr->arrow == ARROWS_BOTH)) {
         coordPtr = linePtr->firstArrowPtr;
         }
      }
      if (tsoffset->flags > (linePtr->numPoints * 2)) {
         coordPtr = linePtr->coordPtr + (linePtr->numPoints * 2);
         if ((linePtr->arrow == ARROWS_LAST) || (linePtr->arrow == ARROWS_BOTH)) {
         coordPtr = linePtr->lastArrowPtr;
         }
      }
      tsoffset->xoffset = (int) (coordPtr[0] + 0.5);
      tsoffset->yoffset = (int) (coordPtr[1] + 0.5);
   } else {
      if (tsoffset->flags & TK_OFFSET_LEFT) {
         tsoffset->xoffset = linePtr->header.x1;
      } else if (tsoffset->flags & TK_OFFSET_CENTER) {
         tsoffset->xoffset = (linePtr->header.x1 + linePtr->header.x2)/2;
      } else if (tsoffset->flags & TK_OFFSET_RIGHT) {
         tsoffset->xoffset = linePtr->header.x2;
      }
      if (tsoffset->flags & TK_OFFSET_TOP) {
         tsoffset->yoffset = linePtr->header.y1;
      } else if (tsoffset->flags & TK_OFFSET_MIDDLE) {
         tsoffset->yoffset = (linePtr->header.y1 + linePtr->header.y2)/2;
      } else if (tsoffset->flags & TK_OFFSET_BOTTOM) {
         tsoffset->yoffset = linePtr->header.y2;
      }
   }

   intWidth = (int) (width + 0.5);
   linePtr->header.x1 -= intWidth;
   linePtr->header.x2 += intWidth;
   linePtr->header.y1 -= intWidth;
   linePtr->header.y2 += intWidth;

   if (linePtr->numPoints==1) {
      linePtr->header.x1 -= 1;
      linePtr->header.x2 += 1;
      linePtr->header.y1 -= 1;
      linePtr->header.y2 += 1;
      return;
   }

   /*
   * For mitered lines, make a second pass through all the points.
   * Compute the locations of the two miter vertex points and add
   * those into the bounding box.
   */

   if (linePtr->joinStyle == JoinMiter) {
      for (i = linePtr->numPoints, coordPtr = linePtr->coordPtr; i >= 3;
         i--, coordPtr += 2) {
         double miter[4];
         int j;

         if (TkGetMiterPoints(coordPtr, coordPtr+2, coordPtr+4,
            width, miter, miter+2)) {
         for (j = 0; j < 4; j += 2) {
            TkIncludePoint((Tk_Item *) linePtr, miter+j);
         }
         }
      }
   }

   /*
   * Add in the sizes of arrowheads, if any.
   */
   if (linePtr->arrow != ARROWS_NONE) {
      if (linePtr->arrow != ARROWS_LAST) {
         for (i = 0, coordPtr = linePtr->firstArrowPtr; i < PTS_IN_ARROW;
            i++, coordPtr += 2) {
         TkIncludePoint((Tk_Item *) linePtr, coordPtr);
         }
      }
      if (linePtr->arrow != ARROWS_FIRST) {
         for (i = 0, coordPtr = linePtr->lastArrowPtr; i < PTS_IN_ARROW;
            i++, coordPtr += 2) {
         TkIncludePoint((Tk_Item *) linePtr, coordPtr);
         }
      }
   }

   /*
   * Add one more pixel of fudge factor just to be safe (e.g.
   * X may round differently than we do).
   */

   linePtr->header.x1 -= 1;
   linePtr->header.x2 += 1;
   linePtr->header.y1 -= 1;
   linePtr->header.y2 += 1;
}

/*----------------------------------------------------------------------------
 * Nom      : <glDisplayLine>
 * Creation : Aout 2002- J.P. Gauthier & David Dube
 *
 * But      :  Dessiner une ligne OpenGL dans le canvas associe.
 *
 *
 * Parametres :
 *  <canvas>    : Canvas dans lequel on dessine la ligne
 *  <itemPtr>   : Pointeur sur l'objet Tk(ligne) a dessine.
 *  <display>   : Non utilise en mode OpenGL
 *  <drawable>  : Non utilise en mode OpenGL
 *  <x>         : Non utilise.
 *  <y>         : Non utilise.
 *  <width>     :
 *  <height>    :
 *
 * Retour: Un entier indiquant si le programme s'est termine normalement ou non.
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
static void glDisplayLine(Tk_Canvas canvas,Tk_Item *itemPtr,Display *display,Drawable drawable,int x,int y,int width,int height) {

   glLineItem *linePtr=(glLineItem*)itemPtr;

   double   linewidth;
   XColor  *color;
   Pixmap   stipple;
   Tk_State state=itemPtr->state;

  /* If the line as no graphic context or has no points, nothing to draw */
   if (!linePtr->numPoints)
      return;

   /* Change the current state of the object */
   if (state == TK_STATE_NULL)
      state = ((TkCanvas *)canvas)->canvas_state;

   linewidth = linePtr->outline.width;
   color = linePtr->outline.color;
   stipple = linePtr->outline.stipple;

   if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
      if (linePtr->outline.activeWidth>linewidth) {
         linewidth = linePtr->outline.activeWidth;
      }
      if (linePtr->outline.activeColor!=NULL) {
         color = linePtr->outline.activeColor;
      }
      if (linePtr->outline.activeStipple!=None) {
         stipple = linePtr->outline.activeStipple;
          }
   } else if (state==TK_STATE_DISABLED) {
      if (linePtr->outline.disabledWidth>0) {
         linewidth = linePtr->outline.disabledWidth;
      }
      if (linePtr->outline.disabledColor!=NULL) {
         color = linePtr->outline.disabledColor;
      }
      if (linePtr->outline.disabledStipple!=None) {
         stipple = linePtr->outline.disabledStipple;
      }
   }

   if (!color || linewidth==0.0 || linePtr->numPoints<1 || linePtr->coordPtr==NULL || linePtr->alpha==0.0) {
      return;
   }

   glDash(&linePtr->outline.dash);

   if (linePtr->alpha<100) {
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
   }

   if (linePtr->SmoothPtr)
      glEnable(GL_LINE_SMOOTH);

   glColor4us(color->red,color->green,color->blue,linePtr->alpha*655);
   glLineWidth((float)linewidth);
   glPointSize((float)linewidth);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   glTranslated(-((TkCanvas *)canvas)->xOrigin,-((TkCanvas *)canvas)->yOrigin,0.0);
   glEnableClientState(GL_VERTEX_ARRAY);

   if (linePtr->numPoints>1) {
      if (linePtr->numSmooth) {
         glVertexPointer(2,GL_DOUBLE,0,linePtr->SmoothPtr);
         glDrawArrays(GL_LINE_STRIP,0,linePtr->numSmooth);
      } else {
         glVertexPointer(2,GL_DOUBLE,0,linePtr->coordPtr);
         glDrawArrays(GL_LINE_STRIP,0,linePtr->numPoints);
      }
   } else {
      glVertexPointer(2,GL_DOUBLE,0,linePtr->coordPtr);
      glDrawArrays(GL_POINT,0,1);
   }

   /* Display arrowheads, if they are wanted. */

   /* First arrow */
   if (linePtr->firstArrowPtr) {
      glVertexPointer(2,GL_DOUBLE,0,linePtr->firstArrowPtr);
      glDrawArrays(GL_POLYGON,0,PTS_IN_ARROW);
   }
   /* Last Arrow */
   if (linePtr->lastArrowPtr) {
      glVertexPointer(2,GL_DOUBLE,0,linePtr->lastArrowPtr);
      glDrawArrays(GL_POLYGON,0,PTS_IN_ARROW);
   }

   glTranslated(((TkCanvas *)canvas)->xOrigin,((TkCanvas *)canvas)->yOrigin,0.0);
   glDisable(GL_LINE_STIPPLE);
   glDisableClientState(GL_VERTEX_ARRAY);
   glDisable(GL_BLEND);
   if (!GLRender->GLAlias)
      glDisable(GL_LINE_SMOOTH);
}

/*
 *--------------------------------------------------------------
 *
 * glLineInsert --
 *
 * Insert coords into a line item at a given index.
 *
 * Results:
 * None.
 *
 * Side effects:
 * The coords in the given item is modified.
 *
 *--------------------------------------------------------------
 */

static void glLineInsert(
   Tk_Canvas canvas,      /* Canvas containing text item. */
   Tk_Item *itemPtr,      /* Line item to be modified. */
   int beforeThis,     /* Index before which new coordinates are to be inserted. */
   Tcl_Obj *obj)    /* New coordinates to be inserted. */
{
   glLineItem *linePtr = (glLineItem *) itemPtr;
   int length, objc, i;
   double *newCoordPtr, *coordPtr;
   Tk_State state = itemPtr->state;
   Tcl_Obj **objv;

   if (state == TK_STATE_NULL) {
      state = Canvas(canvas)->canvas_state;
   }

   if (!obj || (Tcl_ListObjGetElements(NULL, obj, &objc, &objv) != TCL_OK)
         || !objc || objc&1) {
      return;
      }
   length = 2*linePtr->numPoints;
   if (beforeThis < 0) {
      beforeThis = 0;
   }
   if (beforeThis > length) {
      beforeThis = length;
   }
   if (linePtr->firstArrowPtr != NULL) {
      linePtr->coordPtr[0] = linePtr->firstArrowPtr[0];
      linePtr->coordPtr[1] = linePtr->firstArrowPtr[1];
   }
   if (linePtr->lastArrowPtr != NULL) {
      linePtr->coordPtr[length-2] = linePtr->lastArrowPtr[0];
      linePtr->coordPtr[length-1] = linePtr->lastArrowPtr[1];
   }
   newCoordPtr = ckalloc(sizeof(double) * (length + objc));
   for (i=0; i<beforeThis; i++) {
      newCoordPtr[i] = linePtr->coordPtr[i];
   }
   for (i=0; i<objc; i++) {
      if (Tcl_GetDoubleFromObj(NULL, objv[i],
         &newCoordPtr[i + beforeThis]) != TCL_OK) {
         Tcl_ResetResult(Canvas(canvas)->interp);
         ckfree(newCoordPtr);
         return;
      }
   }

   for (i=beforeThis; i<length; i++) {
      newCoordPtr[i+objc] = linePtr->coordPtr[i];
   }
   if (linePtr->coordPtr) {
      ckfree(linePtr->coordPtr);
   }
   linePtr->coordPtr = newCoordPtr;
   length += objc ;
   linePtr->numPoints = length / 2;

   if ((length > 3) && (state != TK_STATE_HIDDEN)) {
      /*
      * This is some optimizing code that will result that only the part of
      * the polygon that changed (and the objects that are overlapping with
      * that part) need to be redrawn. A special flag is set that instructs
      * the general canvas code not to redraw the whole object. If this
      * flag is not set, the canvas will do the redrawing, otherwise I have
      * to do it here.
      */

      itemPtr->redraw_flags |= TK_ITEM_DONT_REDRAW;

      if (beforeThis > 0) {
         beforeThis -= 2;
         objc += 2;
      }
      if (beforeThis+objc < length) {
         objc += 2;
      }
      if (linePtr->smooth) {
         if (beforeThis > 0) {
         beforeThis -= 2;
         objc += 2;
         }
         if (beforeThis+objc+2 < length) {
         objc += 2;
         }
      }
      itemPtr->x1 = itemPtr->x2 = (int) linePtr->coordPtr[beforeThis];
      itemPtr->y1 = itemPtr->y2 = (int) linePtr->coordPtr[beforeThis+1];
      if ((linePtr->firstArrowPtr != NULL) && (beforeThis < 1)) {
         /*
         * Include old first arrow.
         */

         for (i = 0, coordPtr = linePtr->firstArrowPtr; i < PTS_IN_ARROW;
               i++, coordPtr += 2) {
            TkIncludePoint(itemPtr, coordPtr);
         }
      }
      if ((linePtr->lastArrowPtr != NULL) && (beforeThis+objc >= length)) {
         /*
         * Include old last arrow.
         */

         for (i = 0, coordPtr = linePtr->lastArrowPtr; i < PTS_IN_ARROW;
               i++, coordPtr += 2) {
            TkIncludePoint(itemPtr, coordPtr);
         }
      }
      coordPtr = linePtr->coordPtr + beforeThis + 2;
      for (i=2; i<objc; i+=2) {
         TkIncludePoint(itemPtr, coordPtr);
         coordPtr += 2;
      }
   }
   if (linePtr->firstArrowPtr != NULL) {
      ckfree(linePtr->firstArrowPtr);
      linePtr->firstArrowPtr = NULL;
   }
   if (linePtr->lastArrowPtr != NULL) {
     ckfree(linePtr->lastArrowPtr);
      linePtr->lastArrowPtr = NULL;
   }
   if (linePtr->arrow != ARROWS_NONE) {
      ConfigureArrows(canvas, linePtr);
   }

   if (itemPtr->redraw_flags & TK_ITEM_DONT_REDRAW) {
      double width;
      int intWidth;

      if ((linePtr->firstArrowPtr != NULL) && (beforeThis > 2)) {
         /*
         * Include new first arrow.
         */

         for (i = 0, coordPtr = linePtr->firstArrowPtr; i < PTS_IN_ARROW;
               i++, coordPtr += 2) {
            TkIncludePoint(itemPtr, coordPtr);
         }
      }
      if ((linePtr->lastArrowPtr != NULL) && (beforeThis+objc < length-2)) {
         /*
         * Include new right arrow.
         */

         for (i = 0, coordPtr = linePtr->lastArrowPtr; i < PTS_IN_ARROW;
               i++, coordPtr += 2) {
            TkIncludePoint(itemPtr, coordPtr);
         }
      }
      width = linePtr->outline.width;
      if (Canvas(canvas)->currentItemPtr == itemPtr) {
         if (linePtr->outline.activeWidth > width) {
            width = linePtr->outline.activeWidth;
         }
      } else if (state == TK_STATE_DISABLED) {
         if (linePtr->outline.disabledWidth > 0) {
            width = linePtr->outline.disabledWidth;
         }
      }
      intWidth = (int) (width + 0.5);
      if (intWidth < 1) {
         intWidth = 1;
      }
      itemPtr->x1 -= intWidth;
      itemPtr->y1 -= intWidth;
      itemPtr->x2 += intWidth;
      itemPtr->y2 += intWidth;
      Tk_CanvasEventuallyRedraw(canvas, itemPtr->x1, itemPtr->y1,
         itemPtr->x2, itemPtr->y2);
   }

   glComputeLineBbox(canvas, linePtr);
}

/*
 *--------------------------------------------------------------
 *
 * glLineDeleteCoords --
 *
 * Delete one or more coordinates from a line item.
 *
 * Results:
 * None.
 *
 * Side effects:
 * Characters between "first" and "last", inclusive, get
 * deleted from itemPtr.
 *
 *--------------------------------------------------------------
 */

static void glLineDeleteCoords(
   Tk_Canvas canvas,      /* Canvas containing itemPtr. */
   Tk_Item *itemPtr,      /* Item in which to delete characters. */
   int first,       /* Index of first character to delete. */
   int last)        /* Index of last character to delete. */
{
   glLineItem *linePtr = (glLineItem *) itemPtr;
   int count, i, first1, last1;
   int length = 2*linePtr->numPoints;
   double *coordPtr;
   Tk_State state = itemPtr->state;

   if (state == TK_STATE_NULL) {
      state = Canvas(canvas)->canvas_state;
   }

   first &= -2;
   last &= -2;

   if (first < 0) {
      first = 0;
   }
   if (last >= length) {
      last = length-2;
   }
   if (first > last) {
      return;
   }
   if (linePtr->firstArrowPtr != NULL) {
      linePtr->coordPtr[0] = linePtr->firstArrowPtr[0];
      linePtr->coordPtr[1] = linePtr->firstArrowPtr[1];
   }
   if (linePtr->lastArrowPtr != NULL) {
      linePtr->coordPtr[length-2] = linePtr->lastArrowPtr[0];
      linePtr->coordPtr[length-1] = linePtr->lastArrowPtr[1];
   }
   first1 = first;
   last1 = last;
   if (first1 > 0) {
      first1 -= 2;
   }
   if (last1 < length-2) {
      last1 += 2;
   }
   if (linePtr->smooth) {
      if (first1 > 0) {
         first1 -= 2;
      }
      if (last1 < length-2) {
         last1 += 2;
      }
   }

   if ((first1 >= 2) || (last1 < length-2)) {
      /*
      * This is some optimizing code that will result that only the part of
      * the line that changed (and the objects that are overlapping with
      * that part) need to be redrawn. A special flag is set that instructs
      * the general canvas code not to redraw the whole object. If this
      * flag is set, the redrawing has to be done here, otherwise the
      * general Canvas code will take care of it.
      */

      itemPtr->redraw_flags |= TK_ITEM_DONT_REDRAW;
      itemPtr->x1 = itemPtr->x2 = (int) linePtr->coordPtr[first1];
      itemPtr->y1 = itemPtr->y2 = (int) linePtr->coordPtr[first1+1];
      if ((linePtr->firstArrowPtr != NULL) && (first1 < 2)) {
         /*
         * Include old first arrow.
         */

         for (i = 0, coordPtr = linePtr->firstArrowPtr; i < PTS_IN_ARROW;
               i++, coordPtr += 2) {
            TkIncludePoint(itemPtr, coordPtr);
         }
      }
      if ((linePtr->lastArrowPtr != NULL) && (last1 >= length-2)) {
         /*
         * Include old last arrow.
         */

         for (i = 0, coordPtr = linePtr->lastArrowPtr; i < PTS_IN_ARROW;
               i++, coordPtr += 2) {
            TkIncludePoint(itemPtr, coordPtr);
         }
      }
      coordPtr = linePtr->coordPtr+first1+2;
      for (i=first1+2; i<=last1; i+=2) {
         TkIncludePoint(itemPtr, coordPtr);
         coordPtr += 2;
      }
   }

   count = last + 2 - first;
   for (i=last+2; i<length; i++) {
      linePtr->coordPtr[i-count] = linePtr->coordPtr[i];
   }
   linePtr->numPoints -= count/2;
   if (linePtr->firstArrowPtr != NULL) {
      ckfree(linePtr->firstArrowPtr);
      linePtr->firstArrowPtr = NULL;
   }
   if (linePtr->lastArrowPtr != NULL) {
      ckfree(linePtr->lastArrowPtr);
      linePtr->lastArrowPtr = NULL;
   }
   if (linePtr->arrow != ARROWS_NONE) {
      ConfigureArrows(canvas, linePtr);
   }
   if (itemPtr->redraw_flags & TK_ITEM_DONT_REDRAW) {
      double width;
      int intWidth;

      if ((linePtr->firstArrowPtr != NULL) && (first1 < 4)) {
         /*
         * Include new first arrow.
         */

         for (i = 0, coordPtr = linePtr->firstArrowPtr; i < PTS_IN_ARROW;
               i++, coordPtr += 2) {
            TkIncludePoint(itemPtr, coordPtr);
         }
      }
      if ((linePtr->lastArrowPtr != NULL) && (last1 > length-4)) {
         /*
         * Include new right arrow.
         */

         for (i = 0, coordPtr = linePtr->lastArrowPtr; i < PTS_IN_ARROW;
               i++, coordPtr += 2) {
            TkIncludePoint(itemPtr, coordPtr);
         }
      }
      width = linePtr->outline.width;
      if (Canvas(canvas)->currentItemPtr == itemPtr) {
         if (linePtr->outline.activeWidth > width) {
            width = linePtr->outline.activeWidth;
         }
      } else if (state == TK_STATE_DISABLED) {
         if (linePtr->outline.disabledWidth > 0) {
            width = linePtr->outline.disabledWidth;
         }
      }
      intWidth = (int) (width + 0.5);
      if (intWidth < 1) {
         intWidth = 1;
      }
      itemPtr->x1 -= intWidth;
      itemPtr->y1 -= intWidth;
      itemPtr->x2 += intWidth;
      itemPtr->y2 += intWidth;
      Tk_CanvasEventuallyRedraw(canvas, itemPtr->x1, itemPtr->y1,
         itemPtr->x2, itemPtr->y2);
   }
   glComputeLineBbox(canvas, linePtr);
}

/*
 *--------------------------------------------------------------
 *
 * glLineToPoint --
 *
 * Computes the distance from a given point to a given
 * line, in canvas units.
 *
 * Results:
 * The return value is 0 if the point whose x and y coordinates
 * are pointPtr[0] and pointPtr[1] is inside the line.  If the
 * point isn't inside the line then the return value is the
 * distance from the point to the line.
 *
 * Side effects:
 * None.
 *
 *--------------------------------------------------------------
 */

static double glLineToPoint(
   Tk_Canvas canvas,      /* Canvas containing item. */
   Tk_Item *itemPtr,      /* Item to check against point. */
   double *pointPtr)      /* Pointer to x and y coordinates. */
{
   glLineItem *linePtr=(glLineItem *)itemPtr;
   Tk_State    state = itemPtr->state;
   double     *coordPtr,*linePoints;
   double      poly[10];
   double      bestDist, dist, width;
   int         numPoints, count;
   int         changedMiterToBevel; /* Non-zero means that a mitered corner
                * had to be treated as beveled after all
                * because the angle was < 11 degrees. */

   bestDist = 1.0e36;

   /* Handle smoothed lines by generating an expanded set of points against which to do the check */

   if (state == TK_STATE_NULL) {
      state = ((TkCanvas *)canvas)->canvas_state;
   }

   width = linePtr->outline.width;
   if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
      if (linePtr->outline.activeWidth>width) {
         width = linePtr->outline.activeWidth;
      }
   } else if (state==TK_STATE_DISABLED) {
      if (linePtr->outline.disabledWidth>0) {
          width = linePtr->outline.disabledWidth;
      }
   }

   if ((linePtr->smooth) && (linePtr->numPoints > 2)) {
      numPoints  = linePtr->numSmooth;
      linePoints = linePtr->SmoothPtr;
   } else {
      numPoints  = linePtr->numPoints;
      linePoints = linePtr->coordPtr;
   }

   if (width < 1.0) {
      width = 1.0;
   }

   if (!numPoints || itemPtr->state==TK_STATE_HIDDEN) {
      return bestDist;
   } else if (numPoints == 1) {
      bestDist = hypot(linePoints[0]-pointPtr[0],linePoints[1]-pointPtr[1])-width/2.0;
      if (bestDist < 0)
          bestDist = 0;
      return bestDist;
   }

   /*
    * The overall idea is to iterate through all of the edges of
    * the line, computing a polygon for each edge and testing the
    * point against that polygon.  In addition, there are additional
    * tests to deal with rounded joints and caps.
    */

   changedMiterToBevel = 0;
   for (count=numPoints,coordPtr=linePoints; count>=2; count--,coordPtr += 2) {

       /*
        * If rounding is done around the first point then compute
        * the distance between the point and the point.
        */

      if (((linePtr->capStyle == CapRound) && (count == numPoints)) || ((linePtr->joinStyle == JoinRound) && (count != numPoints))) {
         dist = hypot(coordPtr[0]-pointPtr[0],coordPtr[1]-pointPtr[1])-width/2.0;
         if (dist <= 0.0) {
            bestDist = 0.0;
            goto done;
         } else if (dist < bestDist) {
            bestDist = dist;
    }
      }

      /*
       * Compute the polygonal shape corresponding to this edge,
       * consisting of two points for the first point of the edge
       * and two points for the last point of the edge.
       */

      if (count == numPoints) {
         TkGetButtPoints(coordPtr+2,coordPtr,width,linePtr->capStyle==CapProjecting,poly,poly+2);
      } else if ((linePtr->joinStyle == JoinMiter) && !changedMiterToBevel) {
         poly[0] = poly[6];
         poly[1] = poly[7];
         poly[2] = poly[4];
         poly[3] = poly[5];
      } else {
         TkGetButtPoints(coordPtr+2,coordPtr,width,0,poly,poly+2);

         /*
          * If this line uses beveled joints, then check the distance
          * to a polygon comprising the last two points of the previous
          * polygon and the first two from this polygon;  this checks
          * the wedges that fill the mitered joint.
          */

         if ((linePtr->joinStyle == JoinBevel) || changedMiterToBevel) {
            poly[8] = poly[0];
            poly[9] = poly[1];
            dist = TkPolygonToPoint(poly, 5, pointPtr);
            if (dist <= 0.0) {
               bestDist = 0.0;
               goto done;
            } else if (dist < bestDist) {
               bestDist = dist;
            }
            changedMiterToBevel = 0;
         }
      }

      if (count == 2) {
         TkGetButtPoints(coordPtr,coordPtr+2,width,linePtr->capStyle==CapProjecting,poly+4,poly+6);
      } else if (linePtr->joinStyle == JoinMiter) {
         if (TkGetMiterPoints(coordPtr,coordPtr+2,coordPtr+4,width,poly+4,poly+6) == 0) {
            changedMiterToBevel = 1;
            TkGetButtPoints(coordPtr,coordPtr+2,width,0,poly+4,poly+6);
         }
      } else {
         TkGetButtPoints(coordPtr,coordPtr+2,width,0,poly+4,poly+6);
      }
      poly[8] = poly[0];
      poly[9] = poly[1];
      dist = TkPolygonToPoint(poly, 5, pointPtr);
      if (dist <= 0.0) {
         bestDist = 0.0;
         goto done;
      } else if (dist < bestDist) {
        bestDist = dist;
      }
   }

   /*
    * If caps are rounded, check the distance to the cap around the
    * final end point of the line.
    */

   if (linePtr->capStyle == CapRound) {
      dist = hypot(coordPtr[0]-pointPtr[0],coordPtr[1]-pointPtr[1])-width/2.0;
      if (dist <= 0.0) {
         bestDist = 0.0;
         goto done;
      } else if (dist < bestDist) {
         bestDist = dist;
      }
   }

   /*
    * If there are arrowheads, check the distance to the arrowheads.
    */

   if (linePtr->arrow != ARROWS_NONE) {
      if (linePtr->arrow != ARROWS_LAST) {
         dist = TkPolygonToPoint(linePtr->firstArrowPtr,PTS_IN_ARROW,pointPtr);
         if (dist <= 0.0) {
            bestDist = 0.0;
            goto done;
         } else if (dist < bestDist) {
            bestDist = dist;
         }
      }
      if (linePtr->arrow != ARROWS_FIRST) {
         dist = TkPolygonToPoint(linePtr->lastArrowPtr,PTS_IN_ARROW,pointPtr);
         if (dist <= 0.0) {
            bestDist = 0.0;
            goto done;
         } else if (dist < bestDist) {
            bestDist = dist;
         }
      }
   }

   done:
   return bestDist;
}

/*
 *--------------------------------------------------------------
 *
 * glLineToArea --
 *
 * This procedure is called to determine whether an item
 * lies entirely inside, entirely outside, or overlapping
 * a given rectangular area.
 *
 * Results:
 * -1 is returned if the item is entirely outside the
 * area, 0 if it overlaps, and 1 if it is entirely
 * inside the given area.
 *
 * Side effects:
 * None.
 *
 *--------------------------------------------------------------
 */

static int glLineToArea(
   Tk_Canvas canvas,      /* Canvas containing item. */
   Tk_Item *itemPtr,      /* Item to check against line. */
   double *rectPtr)
{
   glLineItem *linePtr=(glLineItem*)itemPtr;
   double     *linePoints;
   int        numPoints,result;
   double     radius,width;
   Tk_State   state=itemPtr->state;

   if(state == TK_STATE_NULL) {
      state = ((TkCanvas *)canvas)->canvas_state;
   }
   width = linePtr->outline.width;
   if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
      if (linePtr->outline.activeWidth>width) {
         width = linePtr->outline.activeWidth;
      }
   } else if (state==TK_STATE_DISABLED) {
      if (linePtr->outline.disabledWidth>0) {
         width = linePtr->outline.disabledWidth;
      }
   }

   radius = (width+1.0)/2.0;

   if ((state==TK_STATE_HIDDEN) || !linePtr->numPoints) {
      return -1;
   } else if (linePtr->numPoints == 1) {
      double oval[4];
      oval[0] = linePtr->coordPtr[0]-radius;
      oval[1] = linePtr->coordPtr[1]-radius;
      oval[2] = linePtr->coordPtr[0]+radius;
      oval[3] = linePtr->coordPtr[1]+radius;
      return TkOvalToArea(oval, rectPtr);
   }

   /* Handle smoothed lines by generating an expanded set of points against which to do the check */

   if ((linePtr->smooth) && (linePtr->numPoints > 2)) {
      numPoints  = linePtr->numSmooth;
      linePoints = linePtr->SmoothPtr;
   } else {
      numPoints  = linePtr->numPoints;
      linePoints = linePtr->coordPtr;
   }

   /* Check the segments of the line */

   if (width < 1.0) {
      width = 1.0;
   }

   result=TkThickPolyLineToArea(linePoints,numPoints,width,linePtr->capStyle,linePtr->joinStyle,rectPtr);
   if (result == 0) {
      goto done;
   }

   /* Check arrowheads, if any */

   if (linePtr->arrow != ARROWS_NONE) {
      if (linePtr->arrow != ARROWS_LAST) {
         if (TkPolygonToArea(linePtr->firstArrowPtr,PTS_IN_ARROW,rectPtr) != result) {
            result = 0;
            goto done;
         }
      }
      if (linePtr->arrow != ARROWS_FIRST) {
         if (TkPolygonToArea(linePtr->lastArrowPtr,PTS_IN_ARROW,rectPtr) != result) {
            result = 0;
            goto done;
         }
      }
   }

   done:
   return result;
}

/*
 *--------------------------------------------------------------
 *
 * glScaleLine --
 *
 * This procedure is invoked to rescale a line item.
 *
 * Results:
 * None.
 *
 * Side effects:
 * The line referred to by itemPtr is rescaled so that the
 * following transformation is applied to all point
 * coordinates:
 *    x' = originX + scaleX*(x-originX)
 *    y' = originY + scaleY*(y-originY)
 *
 *--------------------------------------------------------------
 */

static void glScaleLine(
    Tk_Canvas canvas,      /* Canvas containing line. */
    Tk_Item *itemPtr,      /* Line to be scaled. */
    double originX, double originY,  /* Origin about which to scale rect. */
    double scaleX,      /* Amount to scale in X direction. */
    double scaleY)      /* Amount to scale in Y direction. */
{
   glLineItem *linePtr = (glLineItem *) itemPtr;
   double *coordPtr;
   int i;

   /*
   * Delete any arrowheads before scaling all the points (so that
   * the end-points of the line get restored).
   */

   if (linePtr->firstArrowPtr != NULL) {
      linePtr->coordPtr[0] = linePtr->firstArrowPtr[0];
      linePtr->coordPtr[1] = linePtr->firstArrowPtr[1];
      ckfree((char *) linePtr->firstArrowPtr);
      linePtr->firstArrowPtr = NULL;
   }
   if (linePtr->lastArrowPtr != NULL) {
      int i;

      i = 2*(linePtr->numPoints-1);
      linePtr->coordPtr[i] = linePtr->lastArrowPtr[0];
      linePtr->coordPtr[i+1] = linePtr->lastArrowPtr[1];
      ckfree((char *) linePtr->lastArrowPtr);
      linePtr->lastArrowPtr = NULL;
   }
   for (i = 0, coordPtr = linePtr->coordPtr; i < linePtr->numPoints;
         i++, coordPtr += 2) {
      coordPtr[0] = originX + scaleX*(*coordPtr - originX);
      coordPtr[1] = originY + scaleY*(coordPtr[1] - originY);
   }
   if (linePtr->arrow != ARROWS_NONE) {
      ConfigureArrows(canvas, linePtr);
   }
   glComputeLineBbox(canvas, linePtr);
}

/*
 *--------------------------------------------------------------
 *
 * glGetLineIndex --
 *
 * Parse an index into a line item and return either its value
 * or an error.
 *
 * Results:
 * A standard Tcl result.  If all went well, then *indexPtr is
 * filled in with the index (into itemPtr) corresponding to
 * string.  Otherwise an error message is left in
 * interp->result.
 *
 * Side effects:
 * None.
 *
 *--------------------------------------------------------------
 */

static int glGetLineIndex(
   Tcl_Interp *interp,    /* Used for error reporting. */
   Tk_Canvas canvas,      /* Canvas containing item. */
   Tk_Item *itemPtr,      /* Item for which the index is being * specified. */
   Tcl_Obj *obj,    /* Specification of a particular coord in * itemPtr's line. */
   int *indexPtr)      /* Where to store converted index. */
{
   glLineItem *linePtr = (glLineItem *) itemPtr;
   int length;
   const char *string = Tcl_GetStringFromObj(obj, &length);

   if (string[0] == 'e') {
      if (strncmp(string, "end", (unsigned) length) == 0) {
         *indexPtr = 2*linePtr->numPoints;
      } else {
         goto badIndex;
      }
   } else if (string[0] == '@') {
      int i;
      double x, y, bestDist, dist, *coordPtr;
      char *end;
      const char *p;

      p = string+1;
      x = strtod(p, &end);
      if ((end == p) || (*end != ',')) {
         goto badIndex;
      }
      p = end+1;
      y = strtod(p, &end);
      if ((end == p) || (*end != 0)) {
         goto badIndex;
      }
      bestDist = 1.0e36;
      coordPtr = linePtr->coordPtr;
      *indexPtr = 0;
      for (i=0; i<linePtr->numPoints; i++) {
         dist = hypot(coordPtr[0] - x, coordPtr[1] - y);
         if (dist < bestDist) {
            bestDist = dist;
            *indexPtr = 2*i;
         }
         coordPtr += 2;
      }
   } else {
      if (Tcl_GetIntFromObj(interp, obj, indexPtr) != TCL_OK) {
         goto badIndex;
      }
      *indexPtr &= -2;  /* If index is odd, make it even. */
      if (*indexPtr < 0){
         *indexPtr = 0;
      } else if (*indexPtr > (2*linePtr->numPoints)) {
         *indexPtr = (2*linePtr->numPoints);
      }
   }
   return TCL_OK;

   /*
   * Some of the paths here leave messages in interp->result, so we have to
   * clear it out before storing our own message.
   */

   badIndex:

   Tcl_ResetResult(interp);
   Tcl_SetObjResult(interp, Tcl_ObjPrintf("bad index \"%s\"", string));
   Tcl_SetErrorCode(interp, "TK", "CANVAS", "ITEM_INDEX", "LINE", NULL);
   return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * glTranslateLine --
 *
 * This procedure is called to move a line by a given amount.
 *
 * Results:
 * None.
 *
 * Side effects:
 * The position of the line is offset by (xDelta, yDelta), and
 * the bounding box is updated in the generic part of the item
 * structure.
 *
 *--------------------------------------------------------------
 */

static void glTranslateLine(
    Tk_Canvas canvas,         /* Canvas containing item. */
    Tk_Item *itemPtr,         /* Item that is being moved. */
    double deltaX, double deltaY)    /* Amount by which item is to be moved. */
{
   glLineItem *linePtr = (glLineItem *) itemPtr;
   double *coordPtr;
   int i;

   for (i = 0, coordPtr = linePtr->coordPtr; i < linePtr->numPoints;
         i++, coordPtr += 2) {
      coordPtr[0] += deltaX;
      coordPtr[1] += deltaY;
   }
   if (linePtr->firstArrowPtr != NULL) {
      for (i = 0, coordPtr = linePtr->firstArrowPtr; i < PTS_IN_ARROW;
         i++, coordPtr += 2) {
         coordPtr[0] += deltaX;
         coordPtr[1] += deltaY;
      }
   }
   if (linePtr->lastArrowPtr != NULL) {
      for (i = 0, coordPtr = linePtr->lastArrowPtr; i < PTS_IN_ARROW;
         i++, coordPtr += 2) {
         coordPtr[0] += deltaX;
         coordPtr[1] += deltaY;
      }
   }
   glComputeLineBbox(canvas, linePtr);
}

/*
 *--------------------------------------------------------------
 *
 * ParseArrowShape --
 *
 * This procedure is called back during option parsing to
 * parse arrow shape information.
 *
 * Results:
 * The return value is a standard Tcl result:  TCL_OK means
 * that the arrow shape information was parsed ok, and
 * TCL_ERROR means it couldn't be parsed.
 *
 * Side effects:
 * Arrow information in recordPtr is updated.
 *
 *--------------------------------------------------------------
 */

static int
ParseArrowShape(
    ClientData clientData, /* Not used. */
    Tcl_Interp *interp,    /* Used for error reporting. */
    Tk_Window tkwin,    /* Not used. */
    const char *value,     /* Textual specification of arrow shape. */
    char *recordPtr,    /* Pointer to item record in which to store
             * arrow information. */
    int offset)         /* Offset of shape information in widget
             * record. */
{
    glLineItem *linePtr = (glLineItem *) recordPtr;
    double a, b, c;
    int argc;
    const char **argv = NULL;

    if (offset != Tk_Offset(glLineItem, arrowShapeA)) {
   Tcl_Panic("ParseArrowShape received bogus offset");
    }

    if (Tcl_SplitList(interp, (char *) value, &argc, &argv) != TCL_OK) {
   goto syntaxError;
    } else if (argc != 3) {
   goto syntaxError;
    }
    if ((Tk_CanvasGetCoord(interp, linePtr->canvas, argv[0], &a) != TCL_OK)
       || (Tk_CanvasGetCoord(interp, linePtr->canvas, argv[1], &b)
      != TCL_OK)
       || (Tk_CanvasGetCoord(interp, linePtr->canvas, argv[2], &c)
      != TCL_OK)) {
   goto syntaxError;
    }

    linePtr->arrowShapeA = (float) a;
    linePtr->arrowShapeB = (float) b;
    linePtr->arrowShapeC = (float) c;
    ckfree(argv);
    return TCL_OK;

  syntaxError:
    Tcl_ResetResult(interp);
    Tcl_SetObjResult(interp, Tcl_ObjPrintf(
       "bad arrow shape \"%s\": must be list with three numbers",
       value));
    Tcl_SetErrorCode(interp, "TK", "CANVAS", "ARROW_SHAPE", NULL);
    if (argv != NULL) {
   ckfree(argv);
    }
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * PrintArrowShape --
 *
 * This procedure is a callback invoked by the configuration
 * code to return a printable value describing an arrow shape.
 *
 * Results:
 * None.
 *
 * Side effects:
 * None.
 *
 *--------------------------------------------------------------
 */

static const char *
PrintArrowShape(
    ClientData clientData, /* Not used. */
    Tk_Window tkwin,    /* Window associated with linePtr's widget. */
    char *recordPtr,    /* Pointer to item record containing current
             * shape information. */
    int offset,         /* Offset of arrow information in record. */
    Tcl_FreeProc **freeProcPtr)  /* Store address of function to call to free
             * string here. */
{
    glLineItem *linePtr = (glLineItem *) recordPtr;
    char *buffer = ckalloc(120);

    sprintf(buffer, "%.5g %.5g %.5g", linePtr->arrowShapeA,
       linePtr->arrowShapeB, linePtr->arrowShapeC);
    *freeProcPtr = TCL_DYNAMIC;
    return buffer;
}

/*
 *--------------------------------------------------------------
 *
 * ArrowParseProc --
 *
 * This procedure is invoked during option processing to handle
 * the "-arrow" option.
 *
 * Results:
 * A standard Tcl return value.
 *
 * Side effects:
 * The arrow for a given item gets replaced by the arrow
 * indicated in the value argument.
 *
 *--------------------------------------------------------------
 */

static int
ArrowParseProc(
    ClientData clientData, /* some flags.*/
    Tcl_Interp *interp,    /* Used for reporting errors. */
    Tk_Window tkwin,    /* Window containing canvas widget. */
    const char *value,     /* Value of option. */
    char *widgRec,      /* Pointer to record for item. */
    int offset)         /* Offset into item. */
{
    int c;
    size_t length;

    register Arrows *arrowPtr = (Arrows *) (widgRec + offset);

    if (value == NULL || *value == 0) {
   *arrowPtr = ARROWS_NONE;
   return TCL_OK;
    }

    c = value[0];
    length = strlen(value);

    if ((c == 'n') && (strncmp(value, "none", length) == 0)) {
   *arrowPtr = ARROWS_NONE;
   return TCL_OK;
    }
    if ((c == 'f') && (strncmp(value, "first", length) == 0)) {
   *arrowPtr = ARROWS_FIRST;
   return TCL_OK;
    }
    if ((c == 'l') && (strncmp(value, "last", length) == 0)) {
   *arrowPtr = ARROWS_LAST;
   return TCL_OK;
    }
    if ((c == 'b') && (strncmp(value, "both", length) == 0)) {
   *arrowPtr = ARROWS_BOTH;
   return TCL_OK;
    }

    Tcl_SetObjResult(interp, Tcl_ObjPrintf(
       "bad arrow spec \"%s\": must be none, first, last, or both",
       value));
    Tcl_SetErrorCode(interp, "TK", "CANVAS", "ARROW", NULL);
    *arrowPtr = ARROWS_NONE;
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * ArrowPrintProc --
 *
 * This procedure is invoked by the Tk configuration code
 * to produce a printable string for the "-arrow"
 * configuration option.
 *
 * Results:
 * The return value is a string describing the arrows for
 * the item referred to by "widgRec".  In addition, *freeProcPtr
 * is filled in with the address of a procedure to call to free
 * the result string when it's no longer needed (or NULL to
 * indicate that the string doesn't need to be freed).
 *
 * Side effects:
 * None.
 *
 *--------------------------------------------------------------
 */

static const char *
ArrowPrintProc(
    ClientData clientData, /* Ignored. */
    Tk_Window tkwin,    /* Window containing canvas widget. */
    char *widgRec,      /* Pointer to record for item. */
    int offset,         /* Offset into item. */
    Tcl_FreeProc **freeProcPtr)  /* Pointer to variable to fill in with
             * information about how to reclaim storage
             * for return string. */
{
    register Arrows *arrowPtr = (Arrows *) (widgRec + offset);

    switch (*arrowPtr) {
    case ARROWS_FIRST:
   return "first";
    case ARROWS_LAST:
   return "last";
    case ARROWS_BOTH:
   return "both";
    default:
   return "none";
    }
}

/*
 *--------------------------------------------------------------
 *
 * ConfigureArrows --
 *
 * If arrowheads have been requested for a line, this
 * procedure makes arrangements for the arrowheads.
 *
 * Results:
 * Always returns TCL_OK.
 *
 * Side effects:
 * Information in linePtr is set up for one or two arrowheads.
 * the firstArrowPtr and lastArrowPtr polygons are allocated
 * and initialized, if need be, and the end points of the line
 * are adjusted so that a thick line doesn't stick out past
 * the arrowheads.
 *
 *--------------------------------------------------------------
 */

static int
ConfigureArrows(
    Tk_Canvas canvas,      /* Canvas in which arrows will be displayed
             * (interp and tkwin fields are needed). */
    glLineItem *linePtr)     /* Item to configure for arrows. */
{
    double *poly, *coordPtr;
    double dx, dy, length, sinTheta, cosTheta, temp;
    double fracHeight;     /* Line width as fraction of arrowhead
             * width. */
    double backup;      /* Distance to backup end points so the line
             * ends in the middle of the arrowhead. */
    double vertX, vertY;   /* Position of arrowhead vertex. */
    double shapeA, shapeB, shapeC;
            /* Adjusted coordinates (see explanation
             * below). */
    double width;
    Tk_State state = linePtr->header.state;

    if (linePtr->numPoints < 2) {
   return TCL_OK;
    }

    if (state == TK_STATE_NULL) {
   state = Canvas(canvas)->canvas_state;
    }

    width = linePtr->outline.width;
    if (Canvas(canvas)->currentItemPtr == (Tk_Item *)linePtr) {
   if (linePtr->outline.activeWidth > width) {
       width = linePtr->outline.activeWidth;
   }
    } else if (state == TK_STATE_DISABLED) {
   if (linePtr->outline.disabledWidth > 0) {
       width = linePtr->outline.disabledWidth;
   }
    }

    /*
     * The code below makes a tiny increase in the shape parameters for the
     * line. This is a bit of a hack, but it seems to result in displays that
     * more closely approximate the specified parameters. Without the
     * adjustment, the arrows come out smaller than expected.
     */

    shapeA = linePtr->arrowShapeA + 0.001;
    shapeB = linePtr->arrowShapeB + 0.001;
    shapeC = linePtr->arrowShapeC + width/2.0 + 0.001;

    /*
     * If there's an arrowhead on the first point of the line, compute its
     * polygon and adjust the first point of the line so that the line doesn't
     * stick out past the leading edge of the arrowhead.
     */

    fracHeight = (width/2.0)/shapeC;
    backup = fracHeight*shapeB + shapeA*(1.0 - fracHeight)/2.0;
    if (linePtr->arrow != ARROWS_LAST) {
   poly = linePtr->firstArrowPtr;
   if (poly == NULL) {
       poly = ckalloc(2 * PTS_IN_ARROW * sizeof(double));
       poly[0] = poly[10] = linePtr->coordPtr[0];
       poly[1] = poly[11] = linePtr->coordPtr[1];
       linePtr->firstArrowPtr = poly;
   }
   dx = poly[0] - linePtr->coordPtr[2];
   dy = poly[1] - linePtr->coordPtr[3];
   length = hypot(dx, dy);
   if (length == 0) {
       sinTheta = cosTheta = 0.0;
   } else {
       sinTheta = dy/length;
       cosTheta = dx/length;
   }
   vertX = poly[0] - shapeA*cosTheta;
   vertY = poly[1] - shapeA*sinTheta;
   temp = shapeC*sinTheta;
   poly[2] = poly[0] - shapeB*cosTheta + temp;
   poly[8] = poly[2] - 2*temp;
   temp = shapeC*cosTheta;
   poly[3] = poly[1] - shapeB*sinTheta - temp;
   poly[9] = poly[3] + 2*temp;
   poly[4] = poly[2]*fracHeight + vertX*(1.0-fracHeight);
   poly[5] = poly[3]*fracHeight + vertY*(1.0-fracHeight);
   poly[6] = poly[8]*fracHeight + vertX*(1.0-fracHeight);
   poly[7] = poly[9]*fracHeight + vertY*(1.0-fracHeight);

   /*
    * Polygon done. Now move the first point towards the second so that
    * the corners at the end of the line are inside the arrowhead.
    */

   linePtr->coordPtr[0] = poly[0] - backup*cosTheta;
   linePtr->coordPtr[1] = poly[1] - backup*sinTheta;
    }

    /*
     * Similar arrowhead calculation for the last point of the line.
     */

    if (linePtr->arrow != ARROWS_FIRST) {
   coordPtr = linePtr->coordPtr + 2*(linePtr->numPoints-2);
   poly = linePtr->lastArrowPtr;
   if (poly == NULL) {
       poly = ckalloc(2 * PTS_IN_ARROW * sizeof(double));
       poly[0] = poly[10] = coordPtr[2];
       poly[1] = poly[11] = coordPtr[3];
       linePtr->lastArrowPtr = poly;
   }
   dx = poly[0] - coordPtr[0];
   dy = poly[1] - coordPtr[1];
   length = hypot(dx, dy);
   if (length == 0) {
       sinTheta = cosTheta = 0.0;
   } else {
       sinTheta = dy/length;
       cosTheta = dx/length;
   }
   vertX = poly[0] - shapeA*cosTheta;
   vertY = poly[1] - shapeA*sinTheta;
   temp = shapeC * sinTheta;
   poly[2] = poly[0] - shapeB*cosTheta + temp;
   poly[8] = poly[2] - 2*temp;
   temp = shapeC * cosTheta;
   poly[3] = poly[1] - shapeB*sinTheta - temp;
   poly[9] = poly[3] + 2*temp;
   poly[4] = poly[2]*fracHeight + vertX*(1.0-fracHeight);
   poly[5] = poly[3]*fracHeight + vertY*(1.0-fracHeight);
   poly[6] = poly[8]*fracHeight + vertX*(1.0-fracHeight);
   poly[7] = poly[9]*fracHeight + vertY*(1.0-fracHeight);
   coordPtr[2] = poly[0] - backup*cosTheta;
   coordPtr[3] = poly[1] - backup*sinTheta;
    }

    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * glLineToPostscript --
 *
 * This procedure is called to generate Postscript for
 * line items.
 *
 * Results:
 * The return value is a standard Tcl result.  If an error
 * occurs in generating Postscript then an error message is
 * left in the interp's result, replacing whatever used
 * to be there.  If no error occurs, then Postscript for the
 * item is appended to the result.
 *
 * Side effects:
 * None.
 *
 *--------------------------------------------------------------
 */

static int glLineToPostscript(interp, canvas, itemPtr, prepass)
    Tcl_Interp *interp;       /* Leave Postscript or error message
                * here. */
    Tk_Canvas canvas;         /* Information about overall canvas. */
    Tk_Item *itemPtr;         /* Item for which Postscript is
                * wanted. */
    int prepass;        /* 1 means this is a prepass to
                * collect font information;  0 means
                * final Postscript is being created. */
{
   glLineItem *linePtr=(glLineItem*)itemPtr;
   int        style;
   double      width;
   XColor     *color;
   Pixmap      stipple;
   Tk_State    state=itemPtr->state;
   Tcl_Obj *psObj;
   Tcl_InterpState interpState;

   if (state == TK_STATE_NULL) {
      state = ((TkCanvas *)canvas)->canvas_state;
   }

   width = linePtr->outline.width;
   color = linePtr->outline.color;
   stipple = linePtr->outline.stipple;

   if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
      if (linePtr->outline.activeWidth>width) {
         width = linePtr->outline.activeWidth;
      }
      if (linePtr->outline.activeColor!=NULL) {
         color = linePtr->outline.activeColor;
      }
      if (linePtr->outline.activeStipple!=None) {
         stipple = linePtr->outline.activeStipple;
         }
   } else if (state==TK_STATE_DISABLED) {
      if (linePtr->outline.disabledWidth>0) {
         width = linePtr->outline.disabledWidth;
      }
      if (linePtr->outline.disabledColor!=NULL) {
         color = linePtr->outline.disabledColor;
      }
      if (linePtr->outline.disabledStipple!=None) {
         stipple = linePtr->outline.disabledStipple;
      }
   }

   if (color == NULL || linePtr->numPoints<1 || linePtr->coordPtr==NULL || linePtr->outline.width<=0) {
      return TCL_OK;
   }

   /*
   * Make our working space.
   */

   psObj = Tcl_NewObj();
   interpState = Tcl_SaveInterpState(interp, TCL_OK);

   /*
   * Check if we're just doing a "pixel".
   */

   if (linePtr->numPoints == 1) {
      Tcl_AppendToObj(psObj, "matrix currentmatrix\n", -1);
      Tcl_AppendPrintfToObj(psObj, "%.15g %.15g translate %.15g %.15g",
         linePtr->coordPtr[0], Tk_CanvasPsY(canvas, linePtr->coordPtr[1]),
         width/2.0, width/2.0);
      Tcl_AppendToObj(psObj,
         " scale 1 0 moveto 0 0 1 0 360 arc\nsetmatrix\n", -1);

      Tcl_ResetResult(interp);
      if (Tk_CanvasPsColor(interp, canvas, color) != TCL_OK) {
         goto error;
      }
      Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));

      if (stipple != None) {
         Tcl_AppendToObj(psObj, "clip ", -1);
         Tcl_ResetResult(interp);
         if (Tk_CanvasPsStipple(interp, canvas, stipple) != TCL_OK) {
         goto error;
         }
         Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));
      } else {
         Tcl_AppendToObj(psObj, "fill\n", -1);
      }
      goto done;
   }

   /*
   * Generate a path for the line's center-line (do this differently
   * for straight lines and smoothed lines).
   */

   if ((!linePtr->smooth) || (linePtr->numPoints < 3)) {
      Tk_CanvasPsPath(interp, canvas, linePtr->coordPtr, linePtr->numPoints);
   } else {
      if ((stipple == None) && linePtr->smooth->postscriptProc) {
         linePtr->smooth->postscriptProc(interp,canvas,linePtr->coordPtr,linePtr->numPoints,linePtr->splineSteps);
      } else {
         /*
         * Special hack: Postscript printers don't appear to be able
         * to turn a path drawn with "curveto"s into a clipping path
         * without exceeding resource limits, so TkMakeBezierPostscript
         * won't work for stippled curves.  Instead, generate all of
         * the intermediate points here and output them into the
         * Postscript file with "lineto"s instead.
         */
         Tk_CanvasPsPath(interp, canvas,linePtr->SmoothPtr,linePtr->numSmooth);
      }
   }
   Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));

   /*
   * Set other line-drawing parameters and stroke out the line.
   */

   if (linePtr->capStyle == CapRound) {
      style = 1;
   } else if (linePtr->capStyle == CapProjecting) {
      style = 2;
   } else {
      style = 0;
   }
   Tcl_AppendPrintfToObj(psObj, "%d setlinecap\n", style);
   if (linePtr->joinStyle == JoinRound) {
      style = 1;
   } else if (linePtr->joinStyle == JoinBevel) {
      style = 2;
   } else {
      style = 0;
   }
   Tcl_AppendPrintfToObj(psObj, "%d setlinejoin\n", style);

   Tcl_ResetResult(interp);
   if (Tk_CanvasPsOutline(canvas, itemPtr, &linePtr->outline) != TCL_OK) {
      goto error;
   }
   Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));

   /*
   * Output polygons for the arrowheads, if there are any.
   */

   if (linePtr->firstArrowPtr != NULL) {
      if (stipple != None) {
         Tcl_AppendToObj(psObj, "grestore gsave\n", -1);
      }
      if (ArrowheadPostscript(interp, canvas, linePtr,
         linePtr->firstArrowPtr, psObj) != TCL_OK) {
         goto error;
      }
   }
   if (linePtr->lastArrowPtr != NULL) {
      if (stipple != None) {
         Tcl_AppendToObj(psObj, "grestore gsave\n", -1);
      }
      if (ArrowheadPostscript(interp, canvas, linePtr,
         linePtr->lastArrowPtr, psObj) != TCL_OK) {
         goto error;
      }
   }

   /*
   * Plug the accumulated postscript back into the result.
   */

   done:
   (void) Tcl_RestoreInterpState(interp, interpState);
   Tcl_AppendObjToObj(Tcl_GetObjResult(interp), psObj);
   Tcl_DecrRefCount(psObj);
   return TCL_OK;

   error:
   Tcl_DiscardInterpState(interpState);
   Tcl_DecrRefCount(psObj);
   return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * ArrowheadPostscript --
 *
 * This procedure is called to generate Postscript for
 * an arrowhead for a line item.
 *
 * Results:
 * The return value is a standard Tcl result.  If an error
 * occurs in generating Postscript then an error message is
 * left in the interp's result, replacing whatever used
 * to be there.  If no error occurs, then Postscript for the
 * arrowhead is appended to the result.
 *
 * Side effects:
 * None.
 *
 *--------------------------------------------------------------
 */

static int
ArrowheadPostscript(
    Tcl_Interp *interp,    /* Leave error message here; non-error results
             * will be discarded by caller. */
    Tk_Canvas canvas,      /* Information about overall canvas. */
    glLineItem *linePtr,     /* Line item for which Postscript is being
             * generated. */
    double *arrowPtr,      /* Pointer to first of five points describing
             * arrowhead polygon. */
    Tcl_Obj *psObj)     /* Append postscript to this object. */
{
    Pixmap stipple;
    Tk_State state = linePtr->header.state;

    if (state == TK_STATE_NULL) {
   state = Canvas(canvas)->canvas_state;
    }

    stipple = linePtr->outline.stipple;
    if (Canvas(canvas)->currentItemPtr == (Tk_Item *) linePtr) {
   if (linePtr->outline.activeStipple!=None) {
       stipple = linePtr->outline.activeStipple;
   }
    } else if (state == TK_STATE_DISABLED) {
   if (linePtr->outline.activeStipple!=None) {
       stipple = linePtr->outline.disabledStipple;
   }
    }

    Tcl_ResetResult(interp);
    Tk_CanvasPsPath(interp, canvas, arrowPtr, PTS_IN_ARROW);
    Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));

    if (stipple != None) {
   Tcl_AppendToObj(psObj, "clip ", -1);

   Tcl_ResetResult(interp);
   if (Tk_CanvasPsStipple(interp, canvas, stipple) != TCL_OK) {
       return TCL_ERROR;
   }
   Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));
    } else {
   Tcl_AppendToObj(psObj, "fill\n", -1);
    }
    return TCL_OK;
}