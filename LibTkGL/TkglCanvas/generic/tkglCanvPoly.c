/*
 * tkglCanvPoly.c --
 *
 *	This file implements polygon items for canvas widgets.
 *
 * Copyright (c) 1991-1994 The Regents of the University of California.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 * Copyright (c) 1998-2000 Ajuba Solutions.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: tkCanvPoly.c,v 1.5.2.1 2000/08/05 23:53:11 hobbs Exp $
 */

#include "tkglCanvas.h"

/* The structure below defines the record for each polygon item */

typedef struct glPolygonItem  {
   Tk_Item header;       /* Generic stuff that's the same for all types.  MUST BE FIRST IN STRUCTURE. */
   Tk_Outline outline;   /* Outline structure */
   int numPoints;        /* Number of points in polygon. Polygon is always closed. */
   int pointsAllocated;  /* Number of points for which space is allocated at *coordPtr. */
   double *coordPtr;     /* Pointer to malloc-ed array containing
                          * x- and y-coords of all points in polygon.
                          * X-coords are even-valued indices, y-coords
                          * are corresponding odd-valued indices. */
   int joinStyle;        /* Join style for outline */
   Tk_TSOffset tsoffset;
   XColor *fillColor;                  /* Foreground color for polygon. */
   XColor *activeFillColor;            /* Foreground color for polygon if state is active. */
   XColor *disabledFillColor;          /* Foreground color for polygon if state is disabled. */

   Tk_SmoothMethod *smooth;     /* Non-zero means draw shape smoothed (i.e. with Bezier splines). */
   int numSmooth;               /* Number of vertex in line (always >= 0). */
   double *SmoothPtr;           /* Pointer to malloc-ed array containing*/
   int splineSteps;             /* Number of steps in each spline segment. */
   int autoClosed;     /* Zero means the given polygon was closed, one means that we auto closed it. */

   T_glBitmap *fillStipple;          /* Stipple bitmap for filling item. */
   T_glBitmap *activeFillStipple;        /* Stipple bitmap for filling item if state is active. */
   T_glBitmap *disabledFillStipple;      /* Stipple bitmap for filling item if state is disabled. */
   int  alpha;                       /* alpha value of the polygon & stipple bitmap */
} glPolygonItem;

/* Information used for parsing configuration specs: */

static const Tk_CustomOption bitmapOption = { glBitmapParseProc,glBitmapPrintProc,NULL };
static const Tk_CustomOption smoothOption = { TkSmoothParseProc,TkSmoothPrintProc,NULL };
static const Tk_CustomOption stateOption  = { TkStateParseProc,TkStatePrintProc,INT2PTR(2) };
static const Tk_CustomOption tagsOption   = { Tk_CanvasTagsParseProc,Tk_CanvasTagsPrintProc,NULL };
static const Tk_CustomOption dashOption   = { TkCanvasDashParseProc,TkCanvasDashPrintProc,NULL };
static const Tk_CustomOption offsetOption = { TkOffsetParseProc,TkOffsetPrintProc,INT2PTR(TK_OFFSET_RELATIVE|TK_OFFSET_INDEX) };
static const Tk_CustomOption pixelOption  = { TkPixelParseProc,TkPixelPrintProc,NULL };

static const Tk_ConfigSpec configSpecs[] = {
   { TK_CONFIG_CUSTOM,"-activedash", NULL, NULL, NULL, Tk_Offset(glPolygonItem,outline.activeDash),TK_CONFIG_NULL_OK,&dashOption },
   { TK_CONFIG_COLOR,"-activefill", NULL, NULL, NULL, Tk_Offset(glPolygonItem,activeFillColor),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-activeoutline", NULL, NULL, NULL, Tk_Offset(glPolygonItem,outline.activeColor),TK_CONFIG_NULL_OK },
   { TK_CONFIG_BITMAP,"-activeoutlinestipple", NULL, NULL, NULL, Tk_Offset(glPolygonItem,outline.activeStipple),TK_CONFIG_NULL_OK },
   { TK_CONFIG_CUSTOM,"-activestipple", NULL, NULL, NULL, Tk_Offset(glPolygonItem,activeFillStipple),TK_CONFIG_NULL_OK,&bitmapOption },
   { TK_CONFIG_CUSTOM,"-activewidth", NULL, NULL,"0.0", Tk_Offset(glPolygonItem, outline.activeWidth),TK_CONFIG_DONT_SET_DEFAULT, &pixelOption },
   { TK_CONFIG_CUSTOM,"-dash", NULL, NULL, NULL, Tk_Offset(glPolygonItem,outline.dash),TK_CONFIG_NULL_OK,&dashOption },
   { TK_CONFIG_PIXELS,"-dashoffset", NULL, NULL,"0", Tk_Offset(glPolygonItem,outline.offset),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_CUSTOM,"-disableddash", NULL, NULL, NULL, Tk_Offset(glPolygonItem,outline.disabledDash),TK_CONFIG_NULL_OK,&dashOption },
   { TK_CONFIG_COLOR,"-disabledfill", NULL, NULL, NULL, Tk_Offset(glPolygonItem,disabledFillColor),TK_CONFIG_NULL_OK },
   { TK_CONFIG_COLOR,"-disabledoutline", NULL, NULL, NULL, Tk_Offset(glPolygonItem,outline.disabledColor),TK_CONFIG_NULL_OK },
   { TK_CONFIG_BITMAP,"-disabledoutlinestipple", NULL, NULL, NULL, Tk_Offset(glPolygonItem,outline.disabledStipple),TK_CONFIG_NULL_OK },
   { TK_CONFIG_CUSTOM,"-disabledstipple", NULL, NULL, NULL, Tk_Offset(glPolygonItem,disabledFillStipple),TK_CONFIG_NULL_OK,&bitmapOption },
   { TK_CONFIG_CUSTOM,"-disabledwidth", NULL, NULL,"0.0", Tk_Offset(glPolygonItem, outline.disabledWidth),TK_CONFIG_DONT_SET_DEFAULT,&pixelOption },
   { TK_CONFIG_COLOR,"-fill", NULL, NULL,"black", Tk_Offset(glPolygonItem,fillColor),TK_CONFIG_NULL_OK },
   { TK_CONFIG_JOIN_STYLE, "-joinstyle", NULL, NULL,"round", Tk_Offset(glPolygonItem,joinStyle),TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_CUSTOM,"-offset", NULL, NULL,"0,0", Tk_Offset(glPolygonItem,tsoffset),TK_CONFIG_NULL_OK,&offsetOption },
   { TK_CONFIG_COLOR,"-outline", NULL, NULL, NULL, Tk_Offset(glPolygonItem,outline.color),TK_CONFIG_NULL_OK },
   { TK_CONFIG_CUSTOM,"-outlineoffset", NULL, NULL,"0,0", Tk_Offset(glPolygonItem, outline.tsoffset),TK_CONFIG_NULL_OK,&offsetOption },
   { TK_CONFIG_BITMAP,"-outlinestipple", NULL, NULL, NULL, Tk_Offset(glPolygonItem,outline.stipple),TK_CONFIG_NULL_OK },
   { TK_CONFIG_CUSTOM,"-smooth", NULL, NULL,"0", Tk_Offset(glPolygonItem,smooth),TK_CONFIG_DONT_SET_DEFAULT,&smoothOption },
   { TK_CONFIG_INT,"-splinesteps", NULL, NULL,"12", Tk_Offset(glPolygonItem, splineSteps), TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_CUSTOM,"-state", NULL, NULL, NULL, Tk_Offset(Tk_Item,state),TK_CONFIG_NULL_OK,&stateOption },
   { TK_CONFIG_CUSTOM,"-stipple", NULL, NULL, NULL, Tk_Offset(glPolygonItem,fillStipple),TK_CONFIG_NULL_OK,&bitmapOption },
   { TK_CONFIG_INT, "-transparency", NULL, NULL,"100", Tk_Offset(glPolygonItem, alpha), TK_CONFIG_DONT_SET_DEFAULT },
   { TK_CONFIG_CUSTOM,"-tags", NULL, NULL, NULL, 0,TK_CONFIG_NULL_OK, &tagsOption },
   { TK_CONFIG_CUSTOM,"-width", NULL, NULL,"1.0", Tk_Offset(glPolygonItem,outline.width),TK_CONFIG_DONT_SET_DEFAULT,&pixelOption },
   { TK_CONFIG_END, NULL, NULL, NULL, NULL,0,0 }
};

/* Prototypes for procedures defined in this file */

static void   glComputePolygonBbox(Tk_Canvas canvas,glPolygonItem *polyPtr);
static int    glConfigurePolygon(Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int objc,Tcl_Obj *const objv[], int flags);
static int    glCreatePolygon(Tcl_Interp *interp,Tk_Canvas canvas,struct Tk_Item *itemPtr,int objc,Tcl_Obj *const objv[]);
static void   glDeletePolygon(Tk_Canvas canvas,Tk_Item *itemPtr,Display *display);
static void   glDisplayPolygon(Tk_Canvas canvas,Tk_Item *itemPtr,Display *display,Drawable dst,int x,int y,int width,int height);
static int    glGetPolygonIndex(Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,Tcl_Obj *obj,int *indexPtr);
static int    glPolygonCoords(Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int objc,Tcl_Obj *const objv[]);
static void   glPolygonDeleteCoords(Tk_Canvas canvas,Tk_Item *itemPtr,int first,int last);
static void   glPolygonInsert(Tk_Canvas canvas,Tk_Item *itemPtr,int beforeThis,Tcl_Obj *obj);
static int    glPolygonToArea(Tk_Canvas canvas,Tk_Item *itemPtr,double *rectPtr);
static double glPolygonToPoint(Tk_Canvas canvas,Tk_Item *itemPtr,double *pointPtr);
static int    glPolygonToPostscript(Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr, int prepass);
static void   glScalePolygon(Tk_Canvas canvas,Tk_Item *itemPtr,double originX,double originY,double scaleX,double scaleY);
static void   glTranslatePolygon(Tk_Canvas canvas,Tk_Item *itemPtr,double deltaX,double deltaY);

/*
 * The structures below defines the polygon item type by means
 * of procedures that can be invoked by generic item code.
 */

Tk_ItemType tkglPolygonType = {
    "polygon",                                /* name */
    sizeof(glPolygonItem),                    /* itemSize */
    glCreatePolygon,                          /* createProc */
    configSpecs,                              /* configSpecs */
    glConfigurePolygon,                       /* configureProc */
    glPolygonCoords,                          /* coordProc */
    glDeletePolygon,                          /* deleteProc */
    glDisplayPolygon,                         /* displayProc */
    TK_CONFIG_OBJS | TK_MOVABLE_POINTS,       /* flags */
    glPolygonToPoint,                         /* pointProc */
    glPolygonToArea,                          /* areaProc */
    glPolygonToPostscript,                    /* postscriptProc */
    glScalePolygon,                           /* scaleProc */
    glTranslatePolygon,                       /* translateProc */
    glGetPolygonIndex,                        /* indexProc */
    NULL,                                     /* icursorProc */
    NULL,                                     /* selectionProc */
    glPolygonInsert,                          /* insertProc */
    glPolygonDeleteCoords,                    /* dTextProc */
    NULL,                                     /* nextPtr */
};

/*
 *--------------------------------------------------------------
 *
 * glCreatePolygon --
 *
 *	This procedure is invoked to create a new polygon item in
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
 *	A new polygon item is created.
 *
 *--------------------------------------------------------------
 */

static int glCreatePolygon(
   Tcl_Interp *interp,    /* Interpreter for error reporting. */
   Tk_Canvas canvas,      /* Canvas to hold new item. */
   Tk_Item *itemPtr,      /* Record to hold new item; header has been initialized by caller. */
   int objc,              /* Number of arguments in objv. */
   Tcl_Obj *const objv[]) /* Arguments describing polygon. */
{
   glPolygonItem *polyPtr = (glPolygonItem *) itemPtr;
   int i;

   if (objc == 0) {
      Tcl_Panic("canvas did not pass any coords");
   }

    /*
    * Carry out initialization that is needed in order to clean
    * up after errors during the the remainder of this procedure.
    */

   Tk_CreateOutline(&(polyPtr->outline));
   polyPtr->numPoints           = 0;
   polyPtr->pointsAllocated     = 0;
   polyPtr->coordPtr            = NULL;
   polyPtr->joinStyle           = JoinRound;
   polyPtr->tsoffset.flags      = 0;
   polyPtr->tsoffset.xoffset    = 0;
   polyPtr->tsoffset.yoffset    = 0;
   polyPtr->fillColor           = NULL;
   polyPtr->activeFillColor     = NULL;
   polyPtr->disabledFillColor   = NULL;
   polyPtr->smooth              = NULL;
   polyPtr->numSmooth           = 0;
   polyPtr->SmoothPtr           = NULL;
   polyPtr->fillStipple         = NULL;
   polyPtr->activeFillStipple   = NULL;
   polyPtr->disabledFillStipple = NULL;
   polyPtr->splineSteps         = 12;
   polyPtr->autoClosed          = 0;
   polyPtr->alpha               = 100;

   /*
   * Count the number of points and then parse them into a point
   * array.  Leading arguments are assumed to be points if they
   * start with a digit or a minus sign followed by a digit.
   */

   for (i = 0; i < objc; i++) {
      const char *arg = Tcl_GetString(objv[i]);

      if ((arg[0] == '-') && (arg[1] >= 'a') && (arg[1] <= 'z')) {
         break;
      }
   }

   /* Process the arguments to fill in the item record */

   if (i && glPolygonCoords(interp,canvas,itemPtr,i,objv) == TCL_OK) {
      if (glConfigurePolygon(interp,canvas,itemPtr,objc-i,objv+i,0) == TCL_OK)
         return TCL_OK;
   }

   glDeletePolygon(canvas, itemPtr, Tk_Display(Tk_CanvasTkwin(canvas)));
   return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * glPolygonCoords --
 *
 *	This procedure is invoked to process the "coords" widget
 *	command on polygons.  See the user documentation for details
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

static int glPolygonCoords(
   Tcl_Interp *interp,    /* Used for error reporting. */
   Tk_Canvas canvas,      /* Canvas containing item. */
   Tk_Item *itemPtr,      /* Item whose coordinates are to be read or modified. */
   int objc,        /* Number of coordinates supplied in objv. */
   Tcl_Obj *const objv[]) /* Array of coordinates: x1, y1, x2, y2, ... */
{

   glPolygonItem *polyPtr = (glPolygonItem *) itemPtr;
   int i, numPoints;

   if (objc == 0) {
      /*
      * Print the coords used to create the polygon. If we auto closed the
      * polygon then we don't report the last point.
      */

      Tcl_Obj *subobj, *obj = Tcl_NewObj();

      for (i = 0; i < 2*(polyPtr->numPoints - polyPtr->autoClosed); i++) {
         subobj = Tcl_NewDoubleObj(polyPtr->coordPtr[i]);
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

      if (polyPtr->smooth) {
         for (i = 0; i < 2*polyPtr->numSmooth; i++) {
            subobj = Tcl_NewDoubleObj(polyPtr->SmoothPtr[i]);
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
      Tcl_SetErrorCode(interp, "TK", "CANVAS", "COORDS", "POLYGON", NULL);
      return TCL_ERROR;
   }

   numPoints = objc/2;
   if (polyPtr->pointsAllocated <= numPoints) {
      if (polyPtr->coordPtr != NULL) {
         ckfree(polyPtr->coordPtr);
      }

      /*
      * One extra point gets allocated here, because we always add
      * another point to close the polygon.
      */

      polyPtr->coordPtr = ckalloc(sizeof(double) * (objc+2));
      polyPtr->pointsAllocated = numPoints+1;
   }
   for (i = objc-1; i >= 0; i--) {
      if (Tk_CanvasGetCoordFromObj(interp, canvas, objv[i],
         &polyPtr->coordPtr[i]) != TCL_OK) {
         return TCL_ERROR;
      }
   }
   polyPtr->numPoints = numPoints;
   polyPtr->autoClosed = 0;

   /*
   * Close the polygon if it isn't already closed.
   */

   if (objc>2 && ((polyPtr->coordPtr[objc-2] != polyPtr->coordPtr[0])
         || (polyPtr->coordPtr[objc-1] != polyPtr->coordPtr[1]))) {
      polyPtr->autoClosed = 1;
      polyPtr->numPoints++;
      polyPtr->coordPtr[objc] = polyPtr->coordPtr[0];
      polyPtr->coordPtr[objc+1] = polyPtr->coordPtr[1];
   }

   glComputePolygonBbox(canvas, polyPtr);
   return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * glConfigurePolygon --
 *
 *	This procedure is invoked to configure various aspects
 *	of a polygon item such as its background color.
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

static int glConfigurePolygon(
   Tcl_Interp *interp,    /* Interpreter for error reporting. */
   Tk_Canvas canvas,      /* Canvas containing itemPtr. */
   Tk_Item *itemPtr,      /* Polygon item to reconfigure. */
   int objc,              /* Number of elements in objv.  */
   Tcl_Obj *const objv[], /* Arguments describing things to configure. */
   int flags)             /* Flags to pass to Tk_ConfigureWidget. */
{

   glPolygonItem *polyPtr = (glPolygonItem *) itemPtr;
   Tk_Window tkwin;
   Tk_State state;

   tkwin = Tk_CanvasTkwin(canvas);
   if (Tk_ConfigureWidget(interp, tkwin, configSpecs, objc, (const char **) objv,
      (char *) polyPtr, flags|TK_CONFIG_OBJS) != TCL_OK) {
      return TCL_ERROR;
   }

   /*
   * A few of the options require additional processing, such as graphics
   * contexts.
   */

   state = itemPtr->state;

   if (polyPtr->outline.activeWidth > polyPtr->outline.width ||
      polyPtr->outline.activeDash.number != 0 ||
      polyPtr->outline.activeColor != NULL ||
      polyPtr->outline.activeStipple != None ||
      polyPtr->activeFillColor != NULL ||
      polyPtr->activeFillStipple != NULL) {
      itemPtr->redraw_flags |= TK_ITEM_STATE_DEPENDANT;
   } else {
      itemPtr->redraw_flags &= ~TK_ITEM_STATE_DEPENDANT;
   }
   if (state == TK_STATE_NULL) {
      state = Canvas(canvas)->canvas_state;
   }
   if (state == TK_STATE_HIDDEN) {
      glComputePolygonBbox(canvas, polyPtr);
      return TCL_OK;
   }

   /* Keep spline parameters within reasonable limits. */
   polyPtr->alpha=polyPtr->alpha<0?0:polyPtr->alpha>100?100:polyPtr->alpha;
   polyPtr->splineSteps=polyPtr->splineSteps<1?1:polyPtr->splineSteps>100?100:polyPtr->splineSteps;

   glComputePolygonBbox(canvas, polyPtr);
   return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * glDeletePolygon --
 *
 *	This procedure is called to clean up the data structure
 *	associated with a polygon item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources associated with itemPtr are released.
 *
 *--------------------------------------------------------------
 */

static void glDeletePolygon(
   Tk_Canvas canvas,      /* Info about overall canvas widget. */
   Tk_Item *itemPtr,      /* Item that is being deleted. */
   Display *display)      /* Display containing window for canvas. */
{
   glPolygonItem *polyPtr = (glPolygonItem *) itemPtr;

   Tk_DeleteOutline(display,&(polyPtr->outline));
   if (polyPtr->coordPtr != NULL) {
      ckfree(polyPtr->coordPtr);
   }

   if (polyPtr->fillColor != NULL) {
      Tk_FreeColor(polyPtr->fillColor);
   }
   if (polyPtr->activeFillColor != NULL) {
      Tk_FreeColor(polyPtr->activeFillColor);
   }
   if (polyPtr->disabledFillColor != NULL) {
      Tk_FreeColor(polyPtr->disabledFillColor);
   }
}

/*
 *--------------------------------------------------------------
 *
 * glComputePolygon --
 *
 *
 *--------------------------------------------------------------
 */
static void glComputePolygon(Tk_Canvas canvas,glPolygonItem *polyPtr) {

   if (polyPtr->SmoothPtr) {
      ckfree((char*)polyPtr->SmoothPtr);
      polyPtr->SmoothPtr=NULL;
      polyPtr->numSmooth=0;
   }

   if (polyPtr->smooth && polyPtr->numPoints>2) {
      polyPtr->numSmooth=polyPtr->smooth->coordProc(canvas,NULL,polyPtr->numPoints,polyPtr->splineSteps,NULL,NULL);
      polyPtr->SmoothPtr=(double*)ckalloc((unsigned)(2*polyPtr->numSmooth*sizeof(double)));
      polyPtr->numSmooth=polyPtr->smooth->coordProc(canvas,polyPtr->coordPtr,polyPtr->numPoints,polyPtr->splineSteps,(XPoint*)NULL,polyPtr->SmoothPtr);
   }
}
/*
 *--------------------------------------------------------------
 *
 * glComputePolygonBbox --
 *
 *	This procedure is invoked to compute the bounding box of
 *	all the pixels that may be drawn as part of a polygon.
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

static void glComputePolygonBbox(
   Tk_Canvas canvas,      /* Canvas that contains item. */
   glPolygonItem *polyPtr)  /* Item whose bbox is to be recomputed. */
{
   double *coordPtr;
   int i;
   double width;
   Tk_State state = polyPtr->header.state;
   Tk_TSOffset *tsoffset;

   glComputePolygon(canvas,polyPtr);

   if (state == TK_STATE_NULL) {
      state = Canvas(canvas)->canvas_state;
   }
   width = polyPtr->outline.width;
   if (polyPtr->coordPtr == NULL || (polyPtr->numPoints < 1)
      || (state == TK_STATE_HIDDEN)) {
      polyPtr->header.x1 = polyPtr->header.x2 =
      polyPtr->header.y1 = polyPtr->header.y2 = -1;
      return;
   }
   if (Canvas(canvas)->currentItemPtr == (Tk_Item *) polyPtr) {
      if (polyPtr->outline.activeWidth > width) {
         width = polyPtr->outline.activeWidth;
      }
   } else if (state == TK_STATE_DISABLED) {
      if (polyPtr->outline.disabledWidth > 0.0) {
         width = polyPtr->outline.disabledWidth;
      }
   }

   coordPtr = polyPtr->coordPtr;
   polyPtr->header.x1 = polyPtr->header.x2 = (int) *coordPtr;
   polyPtr->header.y1 = polyPtr->header.y2 = (int) coordPtr[1];

   /*
   * Compute the bounding box of all the points in the polygon, then expand
   * in all directions by the outline's width to take care of butting or
   * rounded corners and projecting or rounded caps. This expansion is an
   * overestimate (worst-case is square root of two over two) but it's
   * simple. Don't do anything special for curves. This causes an additional
   * overestimate in the bounding box, but is faster.
   */

   for (i = 1, coordPtr = polyPtr->coordPtr+2; i < polyPtr->numPoints-1;
      i++, coordPtr += 2) {
      TkIncludePoint((Tk_Item *) polyPtr, coordPtr);
   }

   tsoffset = &polyPtr->tsoffset;
   if (tsoffset->flags & TK_OFFSET_INDEX) {
      int index = tsoffset->flags & ~TK_OFFSET_INDEX;

      if (tsoffset->flags == INT_MAX) {
         index = (polyPtr->numPoints - polyPtr->autoClosed) * 2;
         if (index < 0) {
         index = 0;
         }
      }
      index %= (polyPtr->numPoints - polyPtr->autoClosed) * 2;
      if (index < 0) {
         index += (polyPtr->numPoints - polyPtr->autoClosed) * 2;
      }
      tsoffset->xoffset = (int) (polyPtr->coordPtr[index] + 0.5);
      tsoffset->yoffset = (int) (polyPtr->coordPtr[index+1] + 0.5);
   } else {
      if (tsoffset->flags & TK_OFFSET_LEFT) {
         tsoffset->xoffset = polyPtr->header.x1;
      } else if (tsoffset->flags & TK_OFFSET_CENTER) {
         tsoffset->xoffset = (polyPtr->header.x1 + polyPtr->header.x2)/2;
      } else if (tsoffset->flags & TK_OFFSET_RIGHT) {
         tsoffset->xoffset = polyPtr->header.x2;
      }
      if (tsoffset->flags & TK_OFFSET_TOP) {
         tsoffset->yoffset = polyPtr->header.y1;
      } else if (tsoffset->flags & TK_OFFSET_MIDDLE) {
         tsoffset->yoffset = (polyPtr->header.y1 + polyPtr->header.y2)/2;
      } else if (tsoffset->flags & TK_OFFSET_BOTTOM) {
         tsoffset->yoffset = polyPtr->header.y2;
      }
   }

   tsoffset = &polyPtr->outline.tsoffset;
   if (tsoffset) {
      if (tsoffset->flags & TK_OFFSET_INDEX) {
         int index = tsoffset->flags & ~TK_OFFSET_INDEX;

         if (tsoffset->flags == INT_MAX) {
            index = (polyPtr->numPoints - 1) * 2;
         }
         index %= (polyPtr->numPoints - 1) * 2;
         if (index < 0) {
            index += (polyPtr->numPoints - 1) * 2;
         }
         tsoffset->xoffset = (int) (polyPtr->coordPtr[index] + 0.5);
         tsoffset->yoffset = (int) (polyPtr->coordPtr[index+1] + 0.5);
      } else {
         if (tsoffset->flags & TK_OFFSET_LEFT) {
            tsoffset->xoffset = polyPtr->header.x1;
         } else if (tsoffset->flags & TK_OFFSET_CENTER) {
            tsoffset->xoffset =
               (polyPtr->header.x1 + polyPtr->header.x2) / 2;
         } else if (tsoffset->flags & TK_OFFSET_RIGHT) {
            tsoffset->xoffset = polyPtr->header.x2;
         }
         if (tsoffset->flags & TK_OFFSET_TOP) {
            tsoffset->yoffset = polyPtr->header.y1;
         } else if (tsoffset->flags & TK_OFFSET_MIDDLE) {
            tsoffset->yoffset =
               (polyPtr->header.y1 + polyPtr->header.y2) / 2;
         } else if (tsoffset->flags & TK_OFFSET_BOTTOM) {
            tsoffset->yoffset = polyPtr->header.y2;
         }
      }
   }

   i = (int) ((width+1.5) / 2.0);
   polyPtr->header.x1 -= i;
   polyPtr->header.x2 += i;
   polyPtr->header.y1 -= i;
   polyPtr->header.y2 += i;

   /*
   * For mitered lines, make a second pass through all the points.
   * Compute the locations of the two miter vertex points and add those
   * into the bounding box.
   */

   if (polyPtr->joinStyle == JoinMiter) {
      double miter[4];
      int j;

      coordPtr = polyPtr->coordPtr;
      if (polyPtr->numPoints > 3) {
         if (TkGetMiterPoints(coordPtr+2*(polyPtr->numPoints-2),
            coordPtr, coordPtr+2, width, miter, miter+2)) {
            for (j = 0; j < 4; j += 2) {
            TkIncludePoint((Tk_Item *) polyPtr, miter+j);
            }
         }
      }
      for (i = polyPtr->numPoints ; i >= 3; i--, coordPtr += 2) {
         if (TkGetMiterPoints(coordPtr, coordPtr+2, coordPtr+4, width,
            miter, miter+2)) {
            for (j = 0; j < 4; j += 2) {
            TkIncludePoint((Tk_Item *) polyPtr, miter+j);
            }
         }
      }
   }

   /*
   * Add one more pixel of fudge factor just to be safe (e.g. X may round
   * differently than we do).
   */

   polyPtr->header.x1 -= 1;
   polyPtr->header.x2 += 1;
   polyPtr->header.y1 -= 1;
   polyPtr->header.y2 += 1;
}

/*
 *--------------------------------------------------------------
 *
 * glDisplayPolygon --
 *
 *	This procedure is invoked to draw a polygon item in a given
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

static void glDisplayPolygon(
   Tk_Canvas canvas,      /* Canvas that contains item. */
   Tk_Item *itemPtr,      /* Item to be displayed. */
   Display *display,      /* Display on which to draw item. */
   Drawable drawable,     /* Pixmap or window in which to draw item. */
   int x, int y, int width, int height)  /* Describes region of canvas that must be redisplayed (not used). */
{

   glPolygonItem *polyPtr = (glPolygonItem *) itemPtr;
   Tk_State state = itemPtr->state;
   T_glBitmap *stipple = polyPtr->fillStipple;
   XColor *color = polyPtr->fillColor;;
   double polygonwidth = polyPtr->outline.width;
   int i,numPoints;
   double* coordPtr;                /* Pointer on the real coords of a polygon */

   double *ttt;

   /* Verify some basic requirements about a polygon item */
   if (polyPtr->numPoints < 3)
         return;

   if (state == TK_STATE_NULL)
      state = ((TkCanvas *)canvas)->canvas_state;

   /* If the polygon is currently the current item */
   if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {

      if (polyPtr->activeFillColor != NULL) {
         color = polyPtr->activeFillColor;
      }
      if (polyPtr->outline.activeWidth>polygonwidth)
         polygonwidth = polyPtr->outline.activeWidth;

      if (polyPtr->activeFillStipple)
         stipple = polyPtr->activeFillStipple;

   } else if (state==TK_STATE_DISABLED) {

      if (polyPtr->disabledFillColor != NULL) {
         color = polyPtr->disabledFillColor;
      }
      if (polyPtr->outline.disabledWidth>0.0)
         polygonwidth = polyPtr->outline.disabledWidth;

      if (polyPtr->disabledFillStipple)
         stipple = polyPtr->disabledFillStipple;
   }


   /* Draw the polygon according to the number of points available. */
   if (polyPtr->SmoothPtr) {
      numPoints=polyPtr->numSmooth;
      coordPtr=polyPtr->SmoothPtr;
   } else {
      numPoints=polyPtr->numPoints;
      coordPtr=polyPtr->coordPtr;
   }

   /* Transparency */
   if(polyPtr->alpha<100) {
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
   }
   glTranslated(-((TkCanvas *)canvas)->xOrigin,-((TkCanvas *)canvas)->yOrigin,0.0);

   /* Fill the polygon */
   if (polyPtr->fillColor) {
      if (stipple) {
         glEnable(GL_POLYGON_STIPPLE);
         glPolygonStipple(stipple->Data);
      }

      /* We have to reallocate for the 3rd coordinate */
      ttt=(double*)malloc(3*sizeof(double)*numPoints);
      for(i=0;i<numPoints;i++) {
         ttt[i*3]=coordPtr[i*2];
         ttt[i*3+1]=coordPtr[i*2+1];
         ttt[i*3+2]=0.0;
      }

      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glColor4us(color->red,color->green,color->blue,polyPtr->alpha*655);

      gluTessBeginPolygon(GLRender->GLTess,NULL);
      gluTessBeginContour(GLRender->GLTess);

      /* Begin the tesselation */
      for(i=0;i<numPoints;i++)
        gluTessVertex(GLRender->GLTess,&ttt[i*3],&ttt[i*3]);

      gluTessEndContour(GLRender->GLTess);
      gluTessEndPolygon(GLRender->GLTess);

      free(ttt);
      glDisable(GL_POLYGON_STIPPLE);
   }

   /* Draw the outline */
   if(polyPtr->outline.color!=None && polygonwidth>0) {

      glDash(&polyPtr->outline.dash);
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
      glLineWidth((float)polygonwidth);
      glColor4us(polyPtr->outline.color->red,polyPtr->outline.color->green,polyPtr->outline.color->blue,polyPtr->alpha*655);
      glEnableClientState(GL_VERTEX_ARRAY);
      glVertexPointer(2,GL_DOUBLE,0,coordPtr);
      glDrawArrays(GL_POLYGON,0,numPoints);
      glDisableClientState(GL_VERTEX_ARRAY);
      glDisable(GL_LINE_STIPPLE);
  }

   glTranslated(((TkCanvas *)canvas)->xOrigin,((TkCanvas *)canvas)->yOrigin,0.0);
   glDisable(GL_BLEND);
}

/*
 *--------------------------------------------------------------
 *
 * glPolygonInsert --
 *
 *	Insert coords into a polugon item at a given index.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The coords in the given item is modified.
 *
 *--------------------------------------------------------------
 */

static void glPolygonInsert(
   Tk_Canvas canvas,      /* Canvas containing text item. */
   Tk_Item *itemPtr,      /* Line item to be modified. */
   int beforeThis,     /* Index before which new coordinates are to be inserted. */
   Tcl_Obj *obj)    /* New coordinates to be inserted. */
{
   glPolygonItem *polyPtr = (glPolygonItem *) itemPtr;
   int length, objc, i;
   Tcl_Obj **objv;
   double *newCoordPtr;
   Tk_State state = itemPtr->state;

   if (state == TK_STATE_NULL) {
      state = Canvas(canvas)->canvas_state;
   }

   if (!obj || (Tcl_ListObjGetElements(NULL, obj, &objc, &objv) != TCL_OK)
      || !objc || objc&1) {
      return;
   }
   length = 2*(polyPtr->numPoints - polyPtr->autoClosed);
   while (beforeThis > length) {
      beforeThis -= length;
   }
   while (beforeThis < 0) {
      beforeThis += length;
   }
   newCoordPtr = ckalloc(sizeof(double) * (length + 2 + objc));
   for (i=0; i<beforeThis; i++) {
      newCoordPtr[i] = polyPtr->coordPtr[i];
   }
   for (i=0; i<objc; i++) {
      if (Tcl_GetDoubleFromObj(NULL, objv[i],
         &newCoordPtr[i+beforeThis]) != TCL_OK){
         ckfree(newCoordPtr);
         return;
      }
   }

   for (i=beforeThis; i<length; i++) {
      newCoordPtr[i+objc] = polyPtr->coordPtr[i];
   }
   if (polyPtr->coordPtr) {
      ckfree(polyPtr->coordPtr);
   }
   length += objc;
   polyPtr->coordPtr = newCoordPtr;
   polyPtr->numPoints = (length/2) + polyPtr->autoClosed;

   /*
   * Close the polygon if it isn't already closed, or remove autoclosing if
   * the user's coordinates are now closed.
   */

   if (polyPtr->autoClosed) {
      if ((newCoordPtr[length-2] == newCoordPtr[0])
         && (newCoordPtr[length-1] == newCoordPtr[1])) {
         polyPtr->autoClosed = 0;
         polyPtr->numPoints--;
      }
   } else {
      if ((newCoordPtr[length-2] != newCoordPtr[0])
         || (newCoordPtr[length-1] != newCoordPtr[1])) {
         polyPtr->autoClosed = 1;
         polyPtr->numPoints++;
      }
   }

   newCoordPtr[length] = newCoordPtr[0];
   newCoordPtr[length+1] = newCoordPtr[1];
   if ((length-objc > 3) && (state != TK_STATE_HIDDEN)) {
      /*
      * This is some optimizing code that will result that only the part of
      * the polygon that changed (and the objects that are overlapping with
      * that part) need to be redrawn. A special flag is set that instructs
      * the general canvas code not to redraw the whole object. If this
      * flag is not set, the canvas will do the redrawing, otherwise I have
      * to do it here.
      */

      double width;
      int j;

      itemPtr->redraw_flags |= TK_ITEM_DONT_REDRAW;

      /*
      * The header elements that normally are used for the bounding box,
      * are now used to calculate the bounding box for only the part that
      * has to be redrawn. That doesn't matter, because afterwards the
      * bounding box has to be re-calculated anyway.
      */

      itemPtr->x1 = itemPtr->x2 = (int) polyPtr->coordPtr[beforeThis];
      itemPtr->y1 = itemPtr->y2 = (int) polyPtr->coordPtr[beforeThis+1];
      beforeThis -= 2;
      objc += 4;
      if (polyPtr->smooth) {
         beforeThis -= 2;
         objc += 4;
      }

      /*
      * Be careful; beforeThis could now be negative
      */

      for (i=beforeThis; i<beforeThis+objc; i+=2) {
         j = i;
         if (j < 0) {
            j += length;
         } else if (j >= length) {
            j -= length;
         }
         TkIncludePoint(itemPtr, polyPtr->coordPtr+j);
      }
      width = polyPtr->outline.width;
      if (Canvas(canvas)->currentItemPtr == itemPtr) {
         if (polyPtr->outline.activeWidth > width) {
         width = polyPtr->outline.activeWidth;
         }
      } else if (state == TK_STATE_DISABLED) {
         if (polyPtr->outline.disabledWidth > 0.0) {
         width = polyPtr->outline.disabledWidth;
         }
      }
      itemPtr->x1 -= (int) width;
      itemPtr->y1 -= (int) width;
      itemPtr->x2 += (int) width;
      itemPtr->y2 += (int) width;
      Tk_CanvasEventuallyRedraw(canvas,
         itemPtr->x1, itemPtr->y1, itemPtr->x2, itemPtr->y2);
   }

   glComputePolygonBbox(canvas, polyPtr);
}

/*
 *--------------------------------------------------------------
 *
 * glPolygonDeleteCoords --
 *
 *	Delete one or more coordinates from a polygon item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Characters between "first" and "last", inclusive, get
 *	deleted from itemPtr.
 *
 *--------------------------------------------------------------
 */

static void glPolygonDeleteCoords(
   Tk_Canvas canvas,      /* Canvas containing itemPtr. */
   Tk_Item *itemPtr,      /* Item in which to delete characters. */
   int first,             /* Index of first character to delete. */
   int last)              /* Index of last character to delete. */
{
   glPolygonItem *polyPtr = (glPolygonItem *) itemPtr;
   int count, i;
   int length = 2*(polyPtr->numPoints - polyPtr->autoClosed);

   while (first >= length) {
      first -= length;
   }
   while (first < 0) {
      first += length;
   }
   while (last >= length) {
      last -= length;
   }
   while (last < 0) {
      last += length;
   }

   first &= -2;
   last &= -2;

   count = last + 2 - first;
   if (count <= 0) {
      count += length;
   }

   if (count >= length) {
      polyPtr->numPoints = 0;
      if (polyPtr->coordPtr != NULL) {
         ckfree(polyPtr->coordPtr);
         polyPtr->coordPtr = NULL;
      }
      glComputePolygonBbox(canvas, polyPtr);
      return;
   }

   if (last >= first) {
      for (i=last+2; i<length; i++) {
         polyPtr->coordPtr[i-count] = polyPtr->coordPtr[i];
      }
   } else {
      for (i=last; i<=first; i++) {
         polyPtr->coordPtr[i-last] = polyPtr->coordPtr[i];
      }
   }
   polyPtr->coordPtr[length-count] = polyPtr->coordPtr[0];
   polyPtr->coordPtr[length-count+1] = polyPtr->coordPtr[1];
   polyPtr->numPoints -= count/2;
   glComputePolygonBbox(canvas, polyPtr);
}

/*
 *--------------------------------------------------------------
 *
 * glPolygonToPoint --
 *
 *	Computes the distance from a given point to a given
 *	polygon, in canvas units.
 *
 * Results:
 *	The return value is 0 if the point whose x and y coordinates
 *	are pointPtr[0] and pointPtr[1] is inside the polygon.  If the
 *	point isn't inside the polygon then the return value is the
 *	distance from the point to the polygon.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static double glPolygonToPoint(
   Tk_Canvas canvas,      /* Canvas containing item. */
   Tk_Item *itemPtr,      /* Item to check against point. */
   double *pointPtr)      /* Pointer to x and y coordinates. */
{
   glPolygonItem *polyPtr = (glPolygonItem *) itemPtr;
   double *coordPtr, *polyPoints;
   double poly[10];
   double radius;
   double bestDist, dist;
   int numPoints, count;
   int changedMiterToBevel;	/* Non-zero means that a mitered corner
            * had to be treated as beveled after all
            * because the angle was < 11 degrees. */
   double width;
   Tk_State state = itemPtr->state;

   bestDist = 1.0e36;

   if(state == TK_STATE_NULL) {
      state = ((TkCanvas *)canvas)->canvas_state;
   }
   width = polyPtr->outline.width;
   if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
      if (polyPtr->outline.activeWidth>width) {
         width = polyPtr->outline.activeWidth;
      }
   } else if (state==TK_STATE_DISABLED) {
      if (polyPtr->outline.disabledWidth>0.0) {
         width = polyPtr->outline.disabledWidth;
      }
   }
   radius = width/2.0;

   /* Handle smoothed polygons by generating an expanded set of points against which to do the check */

   if ((polyPtr->smooth) && (polyPtr->numPoints>2)) {
      numPoints  = polyPtr->numSmooth;
      polyPoints = polyPtr->SmoothPtr;
   } else {
      numPoints = polyPtr->numPoints;
      polyPoints = polyPtr->coordPtr;
   }

   bestDist = TkPolygonToPoint(polyPoints, numPoints, pointPtr);
   if (bestDist<=0.0) {
      goto donepoint;
   }
   if ((polyPtr->outline.gc != None) && (polyPtr->joinStyle == JoinRound)) {
      dist = bestDist - radius;
      if (dist <= 0.0) {
         bestDist = 0.0;
         goto donepoint;
      } else {
         bestDist = dist;
      }
   }

   if ((polyPtr->outline.gc == None) || (width <= 1)) goto donepoint;

   /*
   * The overall idea is to iterate through all of the edges of
   * the line, computing a polygon for each edge and testing the
   * point against that polygon.  In addition, there are additional
   * tests to deal with rounded joints and caps.
   */

   changedMiterToBevel = 0;
   for (count = numPoints, coordPtr = polyPoints; count >= 2;
      count--, coordPtr += 2) {

      /*
      * If rounding is done around the first point then compute
      * the distance between the point and the point.
      */

      if (polyPtr->joinStyle == JoinRound) {
         dist = hypot(coordPtr[0] - pointPtr[0], coordPtr[1] - pointPtr[1])
            - radius;
         if (dist <= 0.0) {
            bestDist = 0.0;
            goto donepoint;
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
         TkGetButtPoints(coordPtr+2, coordPtr, (double) width,
            0, poly, poly+2);
      } else if ((polyPtr->joinStyle == JoinMiter) && !changedMiterToBevel) {
         poly[0] = poly[6];
         poly[1] = poly[7];
         poly[2] = poly[4];
         poly[3] = poly[5];
      } else {
         TkGetButtPoints(coordPtr+2, coordPtr, (double) width, 0,
            poly, poly+2);

         /*
         * If this line uses beveled joints, then check the distance
         * to a polygon comprising the last two points of the previous
         * polygon and the first two from this polygon;  this checks
         * the wedges that fill the mitered joint.
         */

         if ((polyPtr->joinStyle == JoinBevel) || changedMiterToBevel) {
         poly[8] = poly[0];
         poly[9] = poly[1];
         dist = TkPolygonToPoint(poly, 5, pointPtr);
         if (dist <= 0.0) {
            bestDist = 0.0;
            goto donepoint;
         } else if (dist < bestDist) {
            bestDist = dist;
         }
         changedMiterToBevel = 0;
         }
      }
      if (count == 2) {
         TkGetButtPoints(coordPtr, coordPtr+2, (double) width,
            0, poly+4, poly+6);
      } else if (polyPtr->joinStyle == JoinMiter) {
         if (TkGetMiterPoints(coordPtr, coordPtr+2, coordPtr+4,
            (double) width, poly+4, poly+6) == 0) {
         changedMiterToBevel = 1;
         TkGetButtPoints(coordPtr, coordPtr+2, (double) width,
            0, poly+4, poly+6);
         }
      } else {
         TkGetButtPoints(coordPtr, coordPtr+2, (double) width, 0,
            poly+4, poly+6);
      }
      poly[8] = poly[0];
      poly[9] = poly[1];
      dist = TkPolygonToPoint(poly, 5, pointPtr);
      if (dist <= 0.0) {
         bestDist = 0.0;
         goto donepoint;
      } else if (dist < bestDist) {
         bestDist = dist;
      }
   }

   donepoint:
   return bestDist;
}

/*
 *--------------------------------------------------------------
 *
 * glPolygonToArea --
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

static int glPolygonToArea(
    Tk_Canvas canvas,      /* Canvas containing item. */
    Tk_Item *itemPtr,      /* Item to check against polygon. */
    double *rectPtr)    /* Pointer to array of four coordinates (x1,y1,x2,y2) describing rectangular area. */
{
   glPolygonItem *polyPtr = (glPolygonItem *) itemPtr;
   double *coordPtr;
   double *polyPoints, poly[10];
   double radius;
   int numPoints, count;
   int changedMiterToBevel;	/* Non-zero means that a mitered corner
            * had to be treated as beveled after all
            * because the angle was < 11 degrees. */
   int inside;			/* Tentative guess about what to return,
            * based on all points seen so far:  one
            * means everything seen so far was
            * inside the area;  -1 means everything
            * was outside the area.  0 means overlap
            * has been found. */
   double width;
   Tk_State state = itemPtr->state;

   if(state == TK_STATE_NULL) {
      state = ((TkCanvas *)canvas)->canvas_state;
   }

   width = polyPtr->outline.width;
   if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
      if (polyPtr->outline.activeWidth>width) {
         width = polyPtr->outline.activeWidth;
      }
   } else if (state==TK_STATE_DISABLED) {
      if (polyPtr->outline.disabledWidth>0.0) {
         width = polyPtr->outline.disabledWidth;
      }
   }

   radius = width/2.0;
   inside = -1;

   if ((state==TK_STATE_HIDDEN) || polyPtr->numPoints<2) {
      return -1;
   } else if (polyPtr->numPoints <3) {
      double oval[4];
      oval[0] = polyPtr->coordPtr[0]-radius;
      oval[1] = polyPtr->coordPtr[1]-radius;
      oval[2] = polyPtr->coordPtr[0]+radius;
      oval[3] = polyPtr->coordPtr[1]+radius;
      return TkOvalToArea(oval, rectPtr);
   }

   /* Handle smoothed polygons by generating an expanded set of points against which to do the check */

   if ((polyPtr->smooth) && (polyPtr->numPoints>2)) {
      numPoints  = polyPtr->numSmooth;
         polyPoints = polyPtr->SmoothPtr;
      } else {
      numPoints = polyPtr->numPoints;
      polyPoints = polyPtr->coordPtr;
   }

   /*
    * Simple test to see if we are in the polygon. Polygons are different
    * from othe canvas items in that they register points being inside even
    * if it isn't filled.
    */

   inside = TkPolygonToArea(polyPoints, numPoints, rectPtr);
   if (inside==0) {
      goto donearea;
   }

   if (polyPtr->outline.gc == None) {
      goto donearea ;
   }

   /*
   * Iterate through all of the edges of the line, computing a polygon
   * for each edge and testing the area against that polygon.  In
   * addition, there are additional tests to deal with rounded joints
   * and caps.
   */

   changedMiterToBevel = 0;
   for (count = numPoints, coordPtr = polyPoints; count >= 2;
      count--, coordPtr += 2) {

      /*
      * If rounding is done around the first point of the edge
      * then test a circular region around the point with the
      * area.
      */

      if (polyPtr->joinStyle == JoinRound) {
         poly[0] = coordPtr[0] - radius;
         poly[1] = coordPtr[1] - radius;
         poly[2] = coordPtr[0] + radius;
         poly[3] = coordPtr[1] + radius;
         if (TkOvalToArea(poly, rectPtr) != inside) {
            inside = 0;
            goto donearea;
         }
      }

      /*
      * Compute the polygonal shape corresponding to this edge,
      * consisting of two points for the first point of the edge
      * and two points for the last point of the edge.
      */

      if (count == numPoints) {
         TkGetButtPoints(coordPtr+2, coordPtr, width,
            0, poly, poly+2);
      } else if ((polyPtr->joinStyle == JoinMiter) && !changedMiterToBevel) {
         poly[0] = poly[6];
         poly[1] = poly[7];
         poly[2] = poly[4];
         poly[3] = poly[5];
      } else {
         TkGetButtPoints(coordPtr+2, coordPtr, width, 0,
            poly, poly+2);

         /*
         * If the last joint was beveled, then also check a
         * polygon comprising the last two points of the previous
         * polygon and the first two from this polygon;  this checks
         * the wedges that fill the beveled joint.
         */

         if ((polyPtr->joinStyle == JoinBevel) || changedMiterToBevel) {
         poly[8] = poly[0];
         poly[9] = poly[1];
         if (TkPolygonToArea(poly, 5, rectPtr) != inside) {
            inside = 0;
            goto donearea;
         }
         changedMiterToBevel = 0;
         }
      }
      if (count == 2) {
         TkGetButtPoints(coordPtr, coordPtr+2, width,
            0, poly+4, poly+6);
      } else if (polyPtr->joinStyle == JoinMiter) {
         if (TkGetMiterPoints(coordPtr, coordPtr+2, coordPtr+4,
            width, poly+4, poly+6) == 0) {
         changedMiterToBevel = 1;
         TkGetButtPoints(coordPtr, coordPtr+2, width,
            0, poly+4, poly+6);
         }
      } else {
         TkGetButtPoints(coordPtr, coordPtr+2, width, 0,
            poly+4, poly+6);
      }
      poly[8] = poly[0];
      poly[9] = poly[1];
      if (TkPolygonToArea(poly, 5, rectPtr) != inside) {
         inside = 0;
         goto donearea;
      }
   }

   donearea:
   return inside;
}

/*
 *--------------------------------------------------------------
 *
 * glScalePolygon --
 *
 *	This procedure is invoked to rescale a polygon item.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The polygon referred to by itemPtr is rescaled so that the
 *	following transformation is applied to all point
 *	coordinates:
 *		x' = originX + scaleX*(x-originX)
 *		y' = originY + scaleY*(y-originY)
 *
 *--------------------------------------------------------------
 */

static void glScalePolygon(
   Tk_Canvas canvas,      /* Canvas containing polygon. */
   Tk_Item *itemPtr,      /* Polygon to be scaled. */
   double originX, double originY,  /* Origin about which to scale rect. */
   double scaleX,      /* Amount to scale in X direction. */
   double scaleY)      /* Amount to scale in Y direction. */
{
   glPolygonItem *polyPtr = (glPolygonItem *) itemPtr;
   double *coordPtr;
   int i;

   for (i = 0, coordPtr = polyPtr->coordPtr; i < polyPtr->numPoints;
      i++, coordPtr += 2) {
      *coordPtr = originX + scaleX*(*coordPtr - originX);
      coordPtr[1] = originY + scaleY*(coordPtr[1] - originY);
   }
   glComputePolygonBbox(canvas, polyPtr);
}

/*
 *--------------------------------------------------------------
 *
 * glGetPolygonIndex --
 *
 *	Parse an index into a polygon item and return either its value
 *	or an error.
 *
 * Results:
 *	A standard Tcl result.  If all went well, then *indexPtr is
 *	filled in with the index (into itemPtr) corresponding to
 *	string.  Otherwise an error message is left in
 *	interp->result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static int glGetPolygonIndex(
    Tcl_Interp *interp,    /* Used for error reporting. */
    Tk_Canvas canvas,      /* Canvas containing item. */
    Tk_Item *itemPtr,      /* Item for which the index is being * specified. */
    Tcl_Obj *obj,    /* Specification of a particular coord in * itemPtr's line. */
    int *indexPtr)      /* Where to store converted index. */
{
    glPolygonItem *polyPtr = (glPolygonItem *) itemPtr;
    size_t length;
    char *string = Tcl_GetStringFromObj(obj, (int *) &length);

   if (string[0] == 'e') {
      if (strncmp(string, "end", (unsigned)length) != 0) {
         goto badIndex;
      }
      *indexPtr = 2*(polyPtr->numPoints - polyPtr->autoClosed);
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
      coordPtr = polyPtr->coordPtr;
      *indexPtr = 0;
      for (i=0; i<polyPtr->numPoints-1; i++) {
         dist = hypot(coordPtr[0] - x, coordPtr[1] - y);
         if (dist < bestDist) {
         bestDist = dist;
         *indexPtr = 2*i;
         }
         coordPtr += 2;
      }
   } else {
      int count = 2*(polyPtr->numPoints - polyPtr->autoClosed);

      if (Tcl_GetIntFromObj(interp, obj, indexPtr) != TCL_OK) {
         goto badIndex;
      }
      *indexPtr &= -2; /* if odd, make it even */
      if (!count) {
         *indexPtr = 0;
      } else if (*indexPtr > 0) {
         *indexPtr = ((*indexPtr - 2) % count) + 2;
      } else {
         *indexPtr = -((-(*indexPtr)) % count);
      }
   }
   return TCL_OK;

   /*
   * Some of the paths here leave messages in interp->result, so we have to
   * clear it out before storing our own message.
   */

   badIndex:

   Tcl_SetObjResult(interp, Tcl_ObjPrintf("bad index \"%s\"", string));
   Tcl_SetErrorCode(interp, "TK", "CANVAS", "ITEM_INDEX", "POLY", NULL);
   return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * glTranslatePolygon --
 *
 *	This procedure is called to move a polygon by a given
 *	amount.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The position of the polygon is offset by (xDelta, yDelta),
 *	and the bounding box is updated in the generic part of the
 *	item structure.
 *
 *--------------------------------------------------------------
 */

static void glTranslatePolygon(
    Tk_Canvas canvas,      /* Canvas containing item. */
    Tk_Item *itemPtr,      /* Item that is being moved. */
    double deltaX, double deltaY) /* Amount by which item is to be moved. */
{
   glPolygonItem *polyPtr = (glPolygonItem *) itemPtr;
   double *coordPtr;
   int i;

   for (i = 0, coordPtr = polyPtr->coordPtr; i < polyPtr->numPoints;
      i++, coordPtr += 2) {
      *coordPtr += deltaX;
      coordPtr[1] += deltaY;
   }
   glComputePolygonBbox(canvas, polyPtr);
}

/*
 *--------------------------------------------------------------
 *
 * glPolygonToPostscript --
 *
 *	This procedure is called to generate Postscript for
 *	polygon items.
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

static int glPolygonToPostscript(interp, canvas, itemPtr, prepass)
    Tcl_Interp *interp;			/* Leave Postscript or error message
					 * here. */
    Tk_Canvas canvas;			/* Information about overall canvas. */
    Tk_Item *itemPtr;			/* Item for which Postscript is
					 * wanted. */
    int prepass;			/* 1 means this is a prepass to
					 * collect font information;  0 means
					 * final Postscript is being created. */
{
   glPolygonItem *polyPtr = (glPolygonItem *) itemPtr;
   int style;
   XColor *color;
   XColor *fillColor;
   Pixmap stipple;
   T_glBitmap *fillStipple;
   Tk_State state = itemPtr->state;
   double width;
   Tcl_Obj *psObj;
   Tcl_InterpState interpState;

   if (polyPtr->numPoints < 2 || polyPtr->coordPtr == NULL) {
      return TCL_OK;
   }

   if (state == TK_STATE_NULL) {
      state = Canvas(canvas)->canvas_state;
   }
   width = polyPtr->outline.width;
   color = polyPtr->outline.color;
   stipple = None;
   fillColor = polyPtr->fillColor;
   fillStipple = polyPtr->fillStipple;
   if (Canvas(canvas)->currentItemPtr == itemPtr) {
      if (polyPtr->outline.activeWidth > width) {
         width = polyPtr->outline.activeWidth;
      }
      if (polyPtr->outline.activeColor != NULL) {
         color = polyPtr->outline.activeColor;
      }
      if (polyPtr->outline.activeStipple != None) {
         stipple = polyPtr->outline.activeStipple;
      }
      if (polyPtr->activeFillColor != NULL) {
         fillColor = polyPtr->activeFillColor;
      }
      if (polyPtr->activeFillStipple != None) {
         fillStipple = polyPtr->activeFillStipple;
      }
   } else if (state == TK_STATE_DISABLED) {
      if (polyPtr->outline.disabledWidth > 0.0) {
         width = polyPtr->outline.disabledWidth;
      }
      if (polyPtr->outline.disabledColor != NULL) {
         color = polyPtr->outline.disabledColor;
      }
      if (polyPtr->outline.disabledStipple != None) {
         stipple = polyPtr->outline.disabledStipple;
      }
      if (polyPtr->disabledFillColor != NULL) {
         fillColor = polyPtr->disabledFillColor;
      }
      if (polyPtr->disabledFillStipple != None) {
         fillStipple = polyPtr->disabledFillStipple;
      }
   }

   /*
   * Make our working space.
   */

   psObj = Tcl_NewObj();
   interpState = Tcl_SaveInterpState(interp, TCL_OK);

   if (polyPtr->numPoints == 2) {
      if (color == NULL) {
         goto done;
      }

      /*
      * Create a point by using a small circle. (Printer pixels are too
      * tiny to be used directly...)
      */

      Tcl_AppendPrintfToObj(psObj,
         "matrix currentmatrix\n"   /* save state */
         "%.15g %.15g translate "   /* go to drawing location */
         "%.15g %.15g scale "    /* scale the drawing */
         "1 0 moveto "        /* correct for origin */
         "0 0 1 0 360 arc\n"     /* make the circle */
         "setmatrix\n",       /* restore state */
         polyPtr->coordPtr[0],
         Tk_CanvasPsY(canvas, polyPtr->coordPtr[1]),
         width/2.0, width/2.0);

      /*
      * Color it in.
      */

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
   * Fill the area of the polygon.
   */

   if (fillColor != NULL && polyPtr->numPoints > 3) {
      Tcl_ResetResult(interp);
      if (!polyPtr->smooth || !polyPtr->smooth->postscriptProc) {
         Tk_CanvasPsPath(interp, canvas, polyPtr->coordPtr,
            polyPtr->numPoints);
      } else {
         polyPtr->smooth->postscriptProc(interp, canvas, polyPtr->coordPtr,
            polyPtr->numPoints, polyPtr->splineSteps);
      }
      if (Tk_CanvasPsColor(interp, canvas, fillColor) != TCL_OK) {
         goto error;
      }
      Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));

      if (fillStipple != None) {
         Tcl_AppendToObj(psObj, "eoclip ", -1);

         Tcl_ResetResult(interp);
         if (glPostscriptStipple(interp,psObj,fillStipple)!= TCL_OK) {
            goto error;
         }

         if (color != NULL) {
            Tcl_AppendToObj(psObj, "grestore gsave\n", -1);
         }
      } else {
         Tcl_AppendToObj(psObj, "eofill\n", -1);
      }
   }

   /*
   * Now draw the outline, if there is one.
   */

   if (color != NULL) {
      Tcl_ResetResult(interp);
      if (!polyPtr->smooth || !polyPtr->smooth->postscriptProc) {
         Tk_CanvasPsPath(interp, canvas, polyPtr->coordPtr,
            polyPtr->numPoints);
      } else {
         polyPtr->smooth->postscriptProc(interp, canvas, polyPtr->coordPtr,
            polyPtr->numPoints, polyPtr->splineSteps);
      }
      Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));

      if (polyPtr->joinStyle == JoinRound) {
         style = 1;
      } else if (polyPtr->joinStyle == JoinBevel) {
         style = 2;
      } else {
         style = 0;
      }
      Tcl_AppendPrintfToObj(psObj, "%d setlinejoin 1 setlinecap\n", style);

      Tcl_ResetResult(interp);
      if (Tk_CanvasPsOutline(canvas, itemPtr, &polyPtr->outline) != TCL_OK){
         goto error;
      }
      Tcl_AppendObjToObj(psObj, Tcl_GetObjResult(interp));
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
