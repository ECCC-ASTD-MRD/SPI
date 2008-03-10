/*
 * tkglCanvText.c --
 *
 * This file implements text items for canvas widgets.
 *
 * Copyright (c) 1991-1994 The Regents of the University of California.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id: tkCanvText.c,v 1.8 1999/12/21 23:55:10 hobbs Exp $
 */

#include <stdio.h>
#include "tkglCanvText.h"

/* Information used for parsing configuration specs */

static Tk_CustomOption stateOption  = { (Tk_OptionParseProc*)TkStateParseProc,TkStatePrintProc,(ClientData)2 };
static Tk_CustomOption tagsOption   = { (Tk_OptionParseProc*)Tk_CanvasTagsParseProc,Tk_CanvasTagsPrintProc,(ClientData)NULL };
static Tk_CustomOption offsetOption = { (Tk_OptionParseProc*)TkOffsetParseProc,TkOffsetPrintProc,(ClientData)(TK_OFFSET_RELATIVE) };
static Tk_CustomOption bitmapOption = { (Tk_OptionParseProc*)glBitmapParseProc,glBitmapPrintProc,(ClientData)NULL };

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_COLOR, "-activefill", (char *) NULL, (char *) NULL,
   (char *) NULL, Tk_Offset(glTextItem, activeColor), TK_CONFIG_NULL_OK},

   { TK_CONFIG_CUSTOM,"-activestipple",(char*)NULL,(char*)NULL,(char*)NULL,
      Tk_Offset(glTextItem,activeStipple),TK_CONFIG_NULL_OK,&bitmapOption },

    {TK_CONFIG_ANCHOR, "-anchor", (char *) NULL, (char *) NULL,
   "center", Tk_Offset(glTextItem, anchor),TK_CONFIG_DONT_SET_DEFAULT},
   {TK_CONFIG_INT, "-angle", (char *) NULL, (char *) NULL,
   "0", Tk_Offset(glTextItem, angle), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_COLOR, "-disabledfill", (char *) NULL, (char *) NULL,
   (char *) NULL, Tk_Offset(glTextItem, disabledColor), TK_CONFIG_NULL_OK},

   { TK_CONFIG_CUSTOM,"-disabledstipple",(char*)NULL,(char*)NULL,(char*)NULL,
      Tk_Offset(glTextItem,disabledStipple),TK_CONFIG_NULL_OK,&bitmapOption },

    {TK_CONFIG_COLOR, "-fill", (char *) NULL, (char *) NULL,
   "black", Tk_Offset(glTextItem, color), TK_CONFIG_NULL_OK},
    {TK_CONFIG_FONT, "-font", (char *) NULL, (char *) NULL,
   DEF_CANVTEXT_FONT, Tk_Offset(glTextItem, tkfont), 0},
    {TK_CONFIG_JUSTIFY, "-justify", (char *) NULL, (char *) NULL,
   "left", Tk_Offset(glTextItem, justify),
   TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_CUSTOM, "-offset", (char *) NULL, (char *) NULL,
   "0,0", Tk_Offset(glTextItem, tsoffset),
   TK_CONFIG_DONT_SET_DEFAULT, &offsetOption},

   { TK_CONFIG_CUSTOM,"-state",(char*)NULL,(char*)NULL,(char*)NULL,
       Tk_Offset(Tk_Item,state),TK_CONFIG_NULL_OK,&stateOption },

   { TK_CONFIG_CUSTOM,"-stipple",(char*)NULL,(char*)NULL,(char*)NULL,
      Tk_Offset(glTextItem,stipple),TK_CONFIG_NULL_OK,&bitmapOption },

    {TK_CONFIG_CUSTOM, "-tags", (char *) NULL, (char *) NULL,
   (char *) NULL, 0, TK_CONFIG_NULL_OK, &tagsOption},
    {TK_CONFIG_STRING, "-text", (char *) NULL, (char *) NULL,
   "", Tk_Offset(glTextItem, text), 0},
   {TK_CONFIG_INT, "-transparency", (char *) NULL, (char *) NULL,
   "100", Tk_Offset(glTextItem, alpha), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_PIXELS, "-width", (char *) NULL, (char *) NULL,
   "0", Tk_Offset(glTextItem, width), TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
   (char *) NULL, 0, 0}
};

/* Prototypes for procedures defined in this file */

static int    glConfigureText _ANSI_ARGS_((Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int argc,Tcl_Obj *CONST argv[],int flags));
static int    glCreateText _ANSI_ARGS_((Tcl_Interp *interp,Tk_Canvas canvas,struct Tk_Item *itemPtr,int argc,Tcl_Obj *CONST argv[]));
static void   glDeleteText _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,Display *display));
static void   glScaleText _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,double originX,double originY,double scaleX,double scaleY));
static int    glTextCoords _ANSI_ARGS_((Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int argc,Tcl_Obj *CONST argv[]));
static int    glTextToArea _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,double *rectPtr));
static double glTextToPoint _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr, double *pointPtr));
static int    glTextToPostscript _ANSI_ARGS_((Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,int prepass));
static void   glTranslateText _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,double deltaX,double deltaY));

void glComputeTextBbox _ANSI_ARGS_((Tk_Canvas canvas,glTextItem *textPtr));
void glDisplayText _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,Display *display,Drawable dst,int x,int y,int width,int height));

static int    GetSelText _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,int offset,char *buffer,int maxBytes));
static int    GetTextIndex _ANSI_ARGS_((Tcl_Interp *interp,Tk_Canvas canvas,Tk_Item *itemPtr,Tcl_Obj *obj,int *indexPtr));
static void   SetTextCursor _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,int index));
static void   TextDeleteChars _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,int first,int last));
static void   TextInsert _ANSI_ARGS_((Tk_Canvas canvas,Tk_Item *itemPtr,int beforeThis,char *string));

/*
 * The structures below defines the rectangle and oval item types
 * by means of procedures that can be invoked by generic item code.
 */

Tk_ItemType tkglTextType = {
    "text",                 /* name */
    sizeof(glTextItem),     /* itemSize */
    glCreateText,             /* createProc */
    configSpecs,            /* configSpecs */
    glConfigureText,          /* configureProc */
    glTextCoords,             /* coordProc */
    glDeleteText,             /* deleteProc */
    glDisplayText,        /* displayProc */
    TK_CONFIG_OBJS,         /* flags */
    glTextToPoint,            /* pointProc */
    glTextToArea,             /* areaProc */
    glTextToPostscript,       /* postscriptProc */
    glScaleText,              /* scaleProc */
    glTranslateText,          /* translateProc */
    (Tk_ItemIndexProc *) GetTextIndex,   /* indexProc */
    SetTextCursor,                       /* icursorProc */
    GetSelText,                          /* selectionProc */
    TextInsert,                          /* insertProc */
    TextDeleteChars,                     /* dTextProc */
    (Tk_ItemType *) NULL,                /* nextPtr */
};

/*
 *--------------------------------------------------------------
 *
 * glCreateText --
 *
 * This procedure is invoked to create a new text item
 * in a canvas.
 *
 * Results:
 * A standard Tcl return value.  If an error occurred in
 * creating the item then an error message is left in
 * the interp's result;  in this case itemPtr is left uninitialized
 * so it can be safely freed by the caller.
 *
 * Side effects:
 * A new text item is created.
 *
 *--------------------------------------------------------------
 */

static int glCreateText(interp, canvas, itemPtr, argc, argv)
   Tcl_Interp *interp;         /* Interpreter for error reporting. */
   Tk_Canvas canvas;           /* Canvas to hold new item. */
   Tk_Item *itemPtr;           /* Record to hold new item; header has been initialized by caller. */
   int argc;                   /* Number of arguments in argv. */
   Tcl_Obj *CONST argv[];      /* Arguments describing rectangle. */
{

   glTextItem *textPtr = (glTextItem *) itemPtr;
   int i;

   if (argc==1) {
      i = 1;
   } else {
      char *arg = Tcl_GetStringFromObj(argv[1], NULL);
      if ((argc>1) && (arg[0] == '-') && (arg[1] >= 'a') && (arg[1] <= 'z')) {
         i = 1;
      } else {
         i = 2;
      }
   }

   if (argc < i) {
      Tcl_AppendResult(interp, "wrong # args: should be \"",
      Tk_PathName(Tk_CanvasTkwin(canvas)), " create ",
      itemPtr->typePtr->name, " x y ?options?\"", (char *) NULL);
      return TCL_ERROR;
   }

   /* Carry out initialization that is needed in order to clean up after errors during the the remainder of this procedure. */

   textPtr->textInfoPtr = Tk_CanvasGetTextInfo(canvas);
   textPtr->insertPos           = 0;
   textPtr->anchor           = TK_ANCHOR_CENTER;
   textPtr->tsoffset.flags      = 0;
   textPtr->tsoffset.xoffset    = 0;
   textPtr->tsoffset.yoffset    = 0;
   textPtr->color         = NULL;
   textPtr->activeColor         = NULL;
   textPtr->disabledColor       = NULL;
   textPtr->tkfont           = NULL;
   textPtr->justify          = TK_JUSTIFY_LEFT;
   textPtr->text          = NULL;
   textPtr->width         = 0;
   textPtr->numChars         = 0;
   textPtr->numBytes         = 0;
   textPtr->textLayout          = NULL;
   textPtr->leftEdge         = 0;
   textPtr->rightEdge           = 0;
   textPtr->dx             = 0;
   textPtr->dy             = 0;

   textPtr->alpha               = 100;
   textPtr->angle               = 0;
   textPtr->stipple          = NULL;
   textPtr->activeStipple       = NULL;
   textPtr->disabledStipple     = NULL;

   /* Process the arguments to fill in the item record. */

   if ((glTextCoords(interp,canvas,itemPtr,i,argv) == TCL_OK)) {
      if (glConfigureText(interp, canvas, itemPtr, argc-i, argv+i, 0) == TCL_OK) {
         return TCL_OK;
      }
   }

   glDeleteText(canvas, itemPtr, Tk_Display(Tk_CanvasTkwin(canvas)));
   return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * glTextCoords --
 *
 * This procedure is invoked to process the "coords" widget
 * command on text items.  See the user documentation for
 * details on what it does.
 *
 * Results:
 * Returns TCL_OK or TCL_ERROR, and sets the interp's result.
 *
 * Side effects:
 * The coordinates for the given item may be changed.
 *
 *--------------------------------------------------------------
 */

static int glTextCoords(interp, canvas, itemPtr, argc, argv)
    Tcl_Interp *interp;     /* Used for error reporting. */
    Tk_Canvas canvas;       /* Canvas containing item. */
    Tk_Item *itemPtr;       /* Item whose coordinates are to be read or modified. */
    int argc;               /* Number of coordinates supplied in argv. */
    Tcl_Obj *CONST argv[];  /* Array of coordinates: x1, y1, x2, y2, ... */
{
    glTextItem *textPtr = (glTextItem *) itemPtr;

    if (argc == 0) {
   Tcl_Obj *obj = Tcl_NewObj();
   Tcl_Obj *subobj = Tcl_NewDoubleObj(textPtr->x);
   Tcl_ListObjAppendElement(interp, obj, subobj);
   subobj = Tcl_NewDoubleObj(textPtr->y);
   Tcl_ListObjAppendElement(interp, obj, subobj);
   Tcl_SetObjResult(interp, obj);
    } else if (argc < 3) {
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
   if ((Tk_CanvasGetCoordFromObj(interp, canvas, argv[0], &textPtr->x) != TCL_OK)
      || (Tk_CanvasGetCoordFromObj(interp, canvas, argv[1],
          &textPtr->y) != TCL_OK)) {
       return TCL_ERROR;
   }
   glComputeTextBbox(canvas, textPtr);
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
 * glConfigureText --
 *
 * This procedure is invoked to configure various aspects
 * of a text item, such as its border and background colors.
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

static int glConfigureText(interp, canvas, itemPtr, argc, argv, flags)
    Tcl_Interp *interp;    /* Interpreter for error reporting. */
    Tk_Canvas canvas;      /* Canvas containing itemPtr. */
    Tk_Item *itemPtr;      /* Rectangle item to reconfigure. */
    int argc;              /* Number of elements in argv.  */
    Tcl_Obj *CONST argv[]; /* Arguments describing things to configure. */
    int flags;             /* Flags to pass to Tk_ConfigureWidget. */
{
    glTextItem *textPtr = (glTextItem *) itemPtr;
    Tk_Window tkwin;
    Tk_CanvasTextInfo *textInfoPtr = textPtr->textInfoPtr;
    Tk_State state;

    tkwin = Tk_CanvasTkwin(canvas);
    if (Tk_ConfigureWidget(interp, tkwin, configSpecs, argc, (char **) argv,
       (char *) textPtr, flags|TK_CONFIG_OBJS) != TCL_OK) {
   return TCL_ERROR;
    }

   /* A few of the options require additional processing, such as graphics contexts. */

   state = itemPtr->state;

   if (textPtr->activeColor || textPtr->activeStipple) {
      itemPtr->redraw_flags |= TK_ITEM_STATE_DEPENDANT;
   } else {
      itemPtr->redraw_flags &= ~TK_ITEM_STATE_DEPENDANT;
   }

   if(state == TK_STATE_NULL) {
      state = ((TkCanvas *)canvas)->canvas_state;
   }

    /*
     * If the text was changed, move the selection and insertion indices
     * to keep them inside the item.
     */

    textPtr->numBytes = (int)strlen(textPtr->text);
    textPtr->numChars = Tcl_NumUtfChars(textPtr->text, textPtr->numBytes);
    if (textInfoPtr->selItemPtr == itemPtr) {

   if (textInfoPtr->selectFirst >= textPtr->numChars) {
       textInfoPtr->selItemPtr = NULL;
   } else {
       if (textInfoPtr->selectLast >= textPtr->numChars) {
      textInfoPtr->selectLast = textPtr->numChars - 1;
       }
       if ((textInfoPtr->anchorItemPtr == itemPtr)
          && (textInfoPtr->selectAnchor >= textPtr->numChars)) {
      textInfoPtr->selectAnchor = textPtr->numChars - 1;
         }
      }
   }
   if (textPtr->insertPos >= textPtr->numChars) {
      textPtr->insertPos = textPtr->numChars;
   }

   textPtr->alpha=textPtr->alpha<0?0:textPtr->alpha>100?100:textPtr->alpha;
   glComputeTextBbox(canvas, textPtr);
   return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * glDeleteText --
 *
 * This procedure is called to clean up the data structure
 * associated with a text item.
 *
 * Results:
 * None.
 *
 * Side effects:
 * Resources associated with itemPtr are released.
 *
 *--------------------------------------------------------------
 */

static void glDeleteText(canvas, itemPtr, display)
    Tk_Canvas canvas;      /* Info about overall canvas widget. */
    Tk_Item *itemPtr;      /* Item that is being deleted. */
    Display *display;      /* Display containing window for canvas. */
{
    glTextItem *textPtr = (glTextItem *) itemPtr;

   if (textPtr->color != NULL) {
      Tk_FreeColor(textPtr->color);
   }
   if (textPtr->activeColor != NULL) {
      Tk_FreeColor(textPtr->activeColor);
   }
   if (textPtr->disabledColor != NULL) {
      Tk_FreeColor(textPtr->disabledColor);
   }
   Tk_FreeFont(textPtr->tkfont);
   if (textPtr->text != NULL) {
      ckfree(textPtr->text);
   }

   Tk_FreeTextLayout(textPtr->textLayout);
}

/*
 *--------------------------------------------------------------
 *
 * glComputeTextBbox --
 *
 * This procedure is invoked to compute the bounding box of
 * all the pixels that may be drawn as part of a text item.
 * In addition, it recomputes all of the geometry information
 * used to display a text item or check for mouse hits.
 *
 * Results:
 * None.
 *
 * Side effects:
 * The fields x1, y1, x2, and y2 are updated in the header
 * for itemPtr, and the linePtr structure is regenerated
 * for itemPtr.
 *
 *--------------------------------------------------------------
 */
static void glTextRotateCoord(double Th,double Xr,double Yr,double *X,double *Y) {

   double len,dx,dy;

   dx=*X-Xr;
   dy=*Y-Yr;

   Th*=0.017453292519943295474371680598;
   Th+=atan2(dy,dx);

   len=sqrt(dx*dx+dy*dy);
   *X=Xr+cos(Th)*len;
   *Y=Yr+sin(Th)*len;
}

void glComputeTextBbox(canvas, textPtr)
    Tk_Canvas canvas;      /* Canvas that contains item. */
    glTextItem *textPtr;      /* Item whose bbox is to be recomputed. */
{
    Tk_CanvasTextInfo *textInfoPtr;
    int leftX, topY, width, height, fudge;
    double minx,miny,maxx,maxy,dx,dy;
    Tk_State state = textPtr->header.state;

    if(state == TK_STATE_NULL) {
   state = ((TkCanvas *)canvas)->canvas_state;
    }

    Tk_FreeTextLayout(textPtr->textLayout);
    textPtr->textLayout = Tk_ComputeTextLayout(textPtr->tkfont,
       textPtr->text, textPtr->numChars, textPtr->width,
       textPtr->justify, 0, &width, &height);

    if (state == TK_STATE_HIDDEN || textPtr->color == NULL) {
       width = height = 0;
    }

    /*
     * Use overall geometry information to compute the top-left corner
     * of the bounding box for the text item.
     */

    leftX = (int) (textPtr->x + 0.5);
    topY = (int) (textPtr->y + 0.5);
    switch (textPtr->anchor) {
   case TK_ANCHOR_NW:
   case TK_ANCHOR_N:
   case TK_ANCHOR_NE:
       break;

   case TK_ANCHOR_W:
   case TK_ANCHOR_CENTER:
   case TK_ANCHOR_E:
       topY -= height / 2;
       break;

   case TK_ANCHOR_SW:
   case TK_ANCHOR_S:
   case TK_ANCHOR_SE:
       topY -= height;
       break;
    }
    switch (textPtr->anchor) {
   case TK_ANCHOR_NW:
   case TK_ANCHOR_W:
   case TK_ANCHOR_SW:
       break;

   case TK_ANCHOR_N:
   case TK_ANCHOR_CENTER:
   case TK_ANCHOR_S:
       leftX -= width / 2;
       break;

   case TK_ANCHOR_NE:
   case TK_ANCHOR_E:
   case TK_ANCHOR_SE:
       leftX -= width;
       break;
    }

    textPtr->leftEdge  = leftX;
    textPtr->rightEdge = leftX + width;
    /*
     * Last of all, update the bounding box for the item.  The item's
     * bounding box includes the bounding box of all its lines, plus
     * an extra fudge factor for the cursor border (which could
     * potentially be quite large).
     */

    textInfoPtr = textPtr->textInfoPtr;
    fudge = (textInfoPtr->insertWidth + 1) / 2;
    if (textInfoPtr->selBorderWidth > fudge) {
       fudge = textInfoPtr->selBorderWidth;
    }

    textPtr->header.x1=leftX - fudge;
    textPtr->header.y1=topY;
    textPtr->header.x2 = leftX + width + fudge;
    textPtr->header.y2 = topY + height;

    textPtr->dx=textPtr->header.x1;
    textPtr->dy=textPtr->header.y1;

    minx=miny=1e32;
    maxx=maxy=-1e32;

    dx=textPtr->header.x1;
    dy=textPtr->header.y1;
    glTextRotateCoord(textPtr->angle,textPtr->x,textPtr->y,&dx,&dy);
    minx=minx<dx?minx:dx;  miny=miny<dy?miny:dy;
    maxx=maxx>dx?maxx:dx;  maxy=maxy>dy?maxy:dy;

    dx=textPtr->header.x1;
    dy=textPtr->header.y2;
    glTextRotateCoord(textPtr->angle,textPtr->x,textPtr->y,&dx,&dy);
    minx=minx<dx?minx:dx;  miny=miny<dy?miny:dy;
    maxx=maxx>dx?maxx:dx;  maxy=maxy>dy?maxy:dy;

    dx=textPtr->header.x2;
    dy=textPtr->header.y1;
    glTextRotateCoord(textPtr->angle,textPtr->x,textPtr->y,&dx,&dy);
    minx=minx<dx?minx:dx;  miny=miny<dy?miny:dy;
    maxx=maxx>dx?maxx:dx;  maxy=maxy>dy?maxy:dy;

    dx=textPtr->header.x2;
    dy=textPtr->header.y2;
    glTextRotateCoord(textPtr->angle,textPtr->x,textPtr->y,&dx,&dy);
    minx=minx<dx?minx:dx;  miny=miny<dy?miny:dy;
    maxx=maxx>dx?maxx:dx;  maxy=maxy>dy?maxy:dy;

    textPtr->header.x1=minx; textPtr->header.y1=miny;
    textPtr->header.x2=maxx; textPtr->header.y2=maxy;
}

/*
 *--------------------------------------------------------------
 *
 * glDisplayTextLayout --
 *
 * This procedure is invoked to draw a text layout item in
 * opengl
 *
 * Results:
 * None.
 *
 * Side effects:
 * ItemPtr is drawn in drawable using the transformation
 * information in canvas.
 *
 *--------------------------------------------------------------
 */

void glDisplayTextLayout(Tk_TextLayout layout,int angle,int x,int y,int firstChar,int lastChar) {

   TextLayout *layoutPtr;
   int i, numDisplayChars, drawX;
   CONST char *firstByte;
   CONST char *lastByte;
   LayoutChunk *chunkPtr;

   Tcl_DString runString;

   layoutPtr=(TextLayout*)layout;
   if (!layoutPtr)
      return;

   if (lastChar<0) {
      lastChar=100000000;
   }

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();

   glTranslatef(x,y,0.0);
   if (angle)
      glRotatef(angle,0.0,0.0,1.0);

   chunkPtr=layoutPtr->chunks;
   for (i=0;i<layoutPtr->numChunks;i++) {
      numDisplayChars=chunkPtr->numDisplayChars;
      if ((numDisplayChars>0) && (firstChar<numDisplayChars)) {
         if (firstChar<=0) {
            drawX=0;
            firstChar=0;
            firstByte=chunkPtr->start;
         } else {
            firstByte=Tcl_UtfAtIndex(chunkPtr->start,firstChar);
            Tk_MeasureChars(layoutPtr->tkfont,chunkPtr->start,firstByte-chunkPtr->start,-1,0,&drawX);
         }
         if (lastChar<numDisplayChars) {
            numDisplayChars=lastChar;
         }
         lastByte=Tcl_UtfAtIndex(chunkPtr->start,numDisplayChars);

         Tcl_DStringInit(&runString);
         Tcl_UtfToUniCharDString(firstByte,lastByte-firstByte,&runString);

         glDrawString(chunkPtr->x+drawX,chunkPtr->y,0,Tcl_DStringValue(&runString),Tcl_DStringLength(&runString),1,1);
         Tcl_DStringFree(&runString);
      }
      firstChar-=chunkPtr->numChars;
      lastChar -=chunkPtr->numChars;
      if (lastChar<=0) {
         break;
      }
      chunkPtr++;
   }

   glPopMatrix();
}

/*
 *--------------------------------------------------------------
 *
 * glDisplayText --
 *
 * This procedure is invoked to draw a text item in a given
 * drawable.
 *
 * Results:
 * None.
 *
 * Side effects:
 * ItemPtr is drawn in drawable using the transformation
 * information in canvas.
 *
 *--------------------------------------------------------------
 */

void glDisplayText(canvas, itemPtr, display, drawable, x, y, width, height)
    Tk_Canvas canvas;          /* Canvas that contains item. */
    Tk_Item *itemPtr;          /* Item to be displayed. */
    Display *display;          /* Display on which to draw item. */
    Drawable drawable;         /* Pixmap or window in which to draw item. */
    int x, y, width, height;   /* Describes region of canvas that must be redisplayed (not used). */
{
   glTextItem *textPtr;
   Tk_CanvasTextInfo *textInfoPtr;
   int selFirstChar, selLastChar;
   T_glBitmap *stipple;
   Tk_State state = itemPtr->state;

   textPtr = (glTextItem *) itemPtr;
   textInfoPtr = textPtr->textInfoPtr;

   if(state == TK_STATE_NULL) {
      state = ((TkCanvas *)canvas)->canvas_state;
   }
   stipple = textPtr->stipple;

   if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
      if (textPtr->activeStipple) {
         stipple=textPtr->activeStipple;
      }
   } else if (state==TK_STATE_DISABLED) {
      if (textPtr->disabledStipple) {
         stipple=textPtr->disabledStipple;
      }
   }

   if (textPtr->alpha<100) {
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
   }
   glPolygonMode(GL_FRONT,GL_FILL);
   glColor4ub(128,128,128,textPtr->alpha*128);

   selFirstChar = -1;
   selLastChar  = 0;

   if (textInfoPtr->selItemPtr==itemPtr) {
      selFirstChar = textInfoPtr->selectFirst;
      selLastChar  = textInfoPtr->selectLast;

      if (selLastChar>textPtr->numChars) {
         selLastChar=textPtr->numChars-1;
      }

      if ((selFirstChar >= 0) && (selFirstChar <= selLastChar)) {
         int xFirst,yFirst,hFirst;
         int xLast,yLast;

         /* Draw a special background under the selection */

         Tk_CharBbox(textPtr->textLayout,selFirstChar,&xFirst,&yFirst,NULL,&hFirst);
         Tk_CharBbox(textPtr->textLayout,selLastChar,&xLast,&yLast,NULL,NULL);

         /* If the selection spans the end of this line, then display
          * selection background all the way to the end of the line.
          * However, for the last line we only want to display up to the
          * last character, not the end of the line.
          */

         glMatrixMode(GL_MODELVIEW);
         glPushMatrix();

         glTranslatef(textPtr->dx-((TkCanvas *)canvas)->xOrigin,textPtr->dy-((TkCanvas *)canvas)->yOrigin,0.0);
         glRotatef(textPtr->angle,0.0,0.0,1.0);
         glBegin(GL_QUADS);
         x = xFirst;
         height = hFirst;
         for (y=yFirst;y<=yLast;y+= height) {
            if (y==yLast) {
               width = xLast - x;
            } else {
               width = textPtr->rightEdge - textPtr->leftEdge - x;
            }
            glVertex2d(x,y);
            glVertex2d(x,y+height);
            glVertex2d(x+width,y+height);
            glVertex2d(x+width,y);
            x=0;
         }
         glEnd();
         glPopMatrix();
      }
   }

   /*
    * If the insertion point should be displayed, then draw a special
    * background for the cursor before drawing the text.
    */

    if (!textPtr->color) {
       return;
    }

    glColor4us(textPtr->color->red,textPtr->color->green,textPtr->color->blue,textPtr->alpha*655);

   if ((textInfoPtr->focusItemPtr == itemPtr) && (textInfoPtr->gotFocus)) {
      if (Tk_CharBbox(textPtr->textLayout,textPtr->insertPos,&x,&y,NULL,&height)) {

         if (textInfoPtr->cursorOn) {
            glMatrixMode(GL_MODELVIEW);
            glPushMatrix();

            glTranslatef(textPtr->dx-((TkCanvas *)canvas)->xOrigin,textPtr->dy-((TkCanvas *)canvas)->yOrigin,0.0);
            glRotatef(textPtr->angle,0.0,0.0,1.0);
            glLineWidth(2.0);
            glBegin(GL_LINES);
            glVertex2d(x,y);
            glVertex2d(x,y+height);
            glEnd();
            glPopMatrix();
         }
      }
    }

   /*
    * Display the text in two pieces: draw the entire text item, then
    * draw the selected text on top of it.  The selected text then
    * will only need to be drawn if it has different attributes (such
    * as foreground color) than regular text.
    */

   if (stipple) {
      glEnable(GL_POLYGON_STIPPLE);
      glPolygonStipple(stipple->Data);

      glClearStencil(0x0);
      glClear(GL_STENCIL_BUFFER_BIT);
      glStencilMask(0x1);
      glStencilFunc(GL_ALWAYS,0x1,0x1);
      glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);
      glEnable(GL_STENCIL_TEST);
      glColorMask(0,0,0,0);

      glBegin(GL_QUADS);
         glVertex2d(textPtr->header.x1,textPtr->header.y1);
         glVertex2d(textPtr->header.x1,textPtr->header.y2);
         glVertex2d(textPtr->header.x2,textPtr->header.y2);
         glVertex2d(textPtr->header.x2,textPtr->header.y1);
      glEnd();

      glStencilFunc(GL_EQUAL,0x1,0x1);
      glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
      glColorMask(1,1,1,1);
      glDisable(GL_POLYGON_STIPPLE);
   }

   glFontUse(display,textPtr->tkfont);
   glDisplayTextLayout(textPtr->textLayout,textPtr->angle,textPtr->dx-((TkCanvas *)canvas)->xOrigin,textPtr->dy-((TkCanvas *)canvas)->yOrigin,0,-1);

   /* Draw the selected text*/
   if (selFirstChar >= 0) {
      glDisplayTextLayout(textPtr->textLayout,textPtr->angle,textPtr->dx-((TkCanvas *)canvas)->xOrigin,textPtr->dy-((TkCanvas *)canvas)->yOrigin,selFirstChar,selLastChar + 1);
   }

   glDisable(GL_BLEND);
   glDisable(GL_STENCIL_TEST);
}

/*
 *--------------------------------------------------------------
 *
 * TextInsert --
 *
 * Insert characters into a text item at a given position.
 *
 * Results:
 * None.
 *
 * Side effects:
 * The text in the given item is modified.  The cursor and
 * selection positions are also modified to reflect the
 * insertion.
 *
 *--------------------------------------------------------------
 */

static void TextInsert(canvas, itemPtr, index, string)
    Tk_Canvas canvas;      /* Canvas containing text item. */
    Tk_Item *itemPtr;      /* Text item to be modified. */
    int index;       /* Character index before which string is
             * to be inserted. */
    char *string;    /* New characters to be inserted. */
{
    glTextItem *textPtr = (glTextItem *) itemPtr;
    int byteIndex, byteCount, charsAdded;
    char *new, *text;
    Tk_CanvasTextInfo *textInfoPtr = textPtr->textInfoPtr;

    string = Tcl_GetStringFromObj((Tcl_Obj *) string, &byteCount);

    text = textPtr->text;

    if (index < 0) {
   index = 0;
    }
    if (index > textPtr->numChars) {
   index = textPtr->numChars;
    }
    byteIndex = Tcl_UtfAtIndex(text, index) - text;
    byteCount = (int)strlen(string);
    if (byteCount == 0) {
   return;
    }

    new = (char *) ckalloc((unsigned) textPtr->numBytes + byteCount + 1);
    memcpy(new, text, (size_t) byteIndex);
    strcpy(new + byteIndex, string);
    strcpy(new + byteIndex + byteCount, text + byteIndex);

    ckfree(text);
    textPtr->text = new;
    charsAdded = Tcl_NumUtfChars(string, byteCount);
    textPtr->numChars += charsAdded;
    textPtr->numBytes += byteCount;

    /*
     * Inserting characters invalidates indices such as those for the
     * selection and cursor.  Update the indices appropriately.
     */

    if (textInfoPtr->selItemPtr == itemPtr) {
   if (textInfoPtr->selectFirst >= index) {
       textInfoPtr->selectFirst += charsAdded;
   }
   if (textInfoPtr->selectLast >= index) {
       textInfoPtr->selectLast += charsAdded;
   }
   if ((textInfoPtr->anchorItemPtr == itemPtr)
      && (textInfoPtr->selectAnchor >= index)) {
       textInfoPtr->selectAnchor += charsAdded;
   }
    }
    if (textPtr->insertPos >= index) {
   textPtr->insertPos += charsAdded;
    }
    glComputeTextBbox(canvas, textPtr);
}

/*
 *--------------------------------------------------------------
 *
 * TextDeleteChars --
 *
 * Delete one or more characters from a text item.
 *
 * Results:
 * None.
 *
 * Side effects:
 * Characters between "first" and "last", inclusive, get
 * deleted from itemPtr, and things like the selection
 * position get updated.
 *
 *--------------------------------------------------------------
 */

static void TextDeleteChars(canvas, itemPtr, first, last)
    Tk_Canvas canvas;      /* Canvas containing itemPtr. */
    Tk_Item *itemPtr;      /* Item in which to delete characters. */
    int first;       /* Character index of first character to
             * delete. */
    int last;        /* Character index of last character to
             * delete (inclusive). */
{
    glTextItem *textPtr = (glTextItem *) itemPtr;
    int byteIndex, byteCount, charsRemoved;
    char *new, *text;
    Tk_CanvasTextInfo *textInfoPtr = textPtr->textInfoPtr;

    text = textPtr->text;
    if (first < 0) {
   first = 0;
    }
    if (last >= textPtr->numChars) {
   last = textPtr->numChars - 1;
    }
    if (first > last) {
   return;
    }
    charsRemoved = last + 1 - first;

    byteIndex = Tcl_UtfAtIndex(text, first) - text;
    byteCount = Tcl_UtfAtIndex(text + byteIndex, charsRemoved)
   - (text + byteIndex);

    new = (char *) ckalloc((unsigned) (textPtr->numBytes + 1 - byteCount));
    memcpy(new, text, (size_t) byteIndex);
    strcpy(new + byteIndex, text + byteIndex + byteCount);

    ckfree(text);
    textPtr->text = new;
    textPtr->numChars -= charsRemoved;
    textPtr->numBytes -= byteCount;

    /*
     * Update indexes for the selection and cursor to reflect the
     * renumbering of the remaining characters.
     */

    if (textInfoPtr->selItemPtr == itemPtr) {
   if (textInfoPtr->selectFirst > first) {
       textInfoPtr->selectFirst -= charsRemoved;
       if (textInfoPtr->selectFirst < first) {
      textInfoPtr->selectFirst = first;
       }
   }
   if (textInfoPtr->selectLast >= first) {
       textInfoPtr->selectLast -= charsRemoved;
       if (textInfoPtr->selectLast < first - 1) {
      textInfoPtr->selectLast = first - 1;
       }
   }
   if (textInfoPtr->selectFirst > textInfoPtr->selectLast) {
       textInfoPtr->selItemPtr = NULL;
   }
   if ((textInfoPtr->anchorItemPtr == itemPtr)
      && (textInfoPtr->selectAnchor > first)) {
       textInfoPtr->selectAnchor -= charsRemoved;
       if (textInfoPtr->selectAnchor < first) {
      textInfoPtr->selectAnchor = first;
       }
   }
    }
    if (textPtr->insertPos > first) {
   textPtr->insertPos -= charsRemoved;
   if (textPtr->insertPos < first) {
       textPtr->insertPos = first;
   }
    }
    glComputeTextBbox(canvas, textPtr);
    return;
}

/*
 *--------------------------------------------------------------
 *
 * glTextToPoint --
 *
 * Computes the distance from a given point to a given
 * text item, in canvas units.
 *
 * Results:
 * The return value is 0 if the point whose x and y coordinates
 * are pointPtr[0] and pointPtr[1] is inside the text item.  If
 * the point isn't inside the text item then the return value
 * is the distance from the point to the text item.
 *
 * Side effects:
 * None.
 *
 *--------------------------------------------------------------
 */
static double glTextToPoint(canvas, itemPtr, pointPtr)
    Tk_Canvas canvas;      /* Canvas containing itemPtr. */
    Tk_Item *itemPtr;      /* Item to check against point. */
    double *pointPtr;      /* Pointer to x and y coordinates. */
{
    glTextItem *textPtr;
    Tk_State state = itemPtr->state;
    double value;

    if (state == TK_STATE_NULL) {
       state = ((TkCanvas *)canvas)->canvas_state;
    }
    textPtr = (glTextItem *) itemPtr;

   glTextRotateCoord(-textPtr->angle,textPtr->dx,textPtr->dy,&pointPtr[0],&pointPtr[1]);
   pointPtr[0]-=textPtr->dx;
   pointPtr[1]-=textPtr->dy;

    value =  (double) Tk_DistanceToTextLayout(textPtr->textLayout,pointPtr[0],pointPtr[1]);

    if ((state == TK_STATE_HIDDEN) || (textPtr->color == NULL) ||
       (textPtr->text == NULL) || (*textPtr->text == 0)) {
   value = 1.0e36;
    }
    return value;
}

/*
 *--------------------------------------------------------------
 *
 * glTextToArea --
 *
 * This procedure is called to determine whether an item
 * lies entirely inside, entirely outside, or overlapping
 * a given rectangle.
 *
 * Results:
 * -1 is returned if the item is entirely outside the area
 * given by rectPtr, 0 if it overlaps, and 1 if it is entirely
 * inside the given area.
 *
 * Side effects:
 * None.
 *
 *--------------------------------------------------------------
 */

static int glTextToArea(canvas, itemPtr, rectPtr)
    Tk_Canvas canvas;      /* Canvas containing itemPtr. */
    Tk_Item *itemPtr;      /* Item to check against rectangle. */
    double *rectPtr;    /* Pointer to array of four coordinates
             * (x1, y1, x2, y2) describing rectangular
             * area.  */
{
    glTextItem *textPtr;
    Tk_State state = itemPtr->state;

    if (state == TK_STATE_NULL) {
   state = ((TkCanvas *)canvas)->canvas_state;
    }

    textPtr = (glTextItem *) itemPtr;

   glTextRotateCoord(-textPtr->angle,textPtr->dx,textPtr->dy,&rectPtr[0],&rectPtr[1]);
   glTextRotateCoord(-textPtr->angle,textPtr->dx,textPtr->dy,&rectPtr[2],&rectPtr[3]);

    return Tk_IntersectTextLayout(textPtr->textLayout,
       (int) (rectPtr[0] + 0.5) - textPtr->dx,
       (int) (rectPtr[1] + 0.5) - textPtr->dy,
       (int) (rectPtr[2] - rectPtr[0] + 0.5),
       (int) (rectPtr[3] - rectPtr[1] + 0.5));
}

/*
 *--------------------------------------------------------------
 *
 * glScaleText --
 *
 * This procedure is invoked to rescale a text item.
 *
 * Results:
 * None.
 *
 * Side effects:
 * Scales the position of the text, but not the size
 * of the font for the text.
 *
 *--------------------------------------------------------------
 */

static void glScaleText(canvas, itemPtr, originX, originY, scaleX, scaleY)
    Tk_Canvas canvas;      /* Canvas containing rectangle. */
    Tk_Item *itemPtr;      /* Rectangle to be scaled. */
    double originX, originY;  /* Origin about which to scale rect. */
    double scaleX;      /* Amount to scale in X direction. */
    double scaleY;      /* Amount to scale in Y direction. */
{
    glTextItem *textPtr = (glTextItem *) itemPtr;

    textPtr->x = originX + scaleX*(textPtr->x - originX);
    textPtr->y = originY + scaleY*(textPtr->y - originY);
    glComputeTextBbox(canvas, textPtr);
    return;
}

/*
 *--------------------------------------------------------------
 *
 * glTranslateText --
 *
 * This procedure is called to move a text item by a
 * given amount.
 *
 * Results:
 * None.
 *
 * Side effects:
 * The position of the text item is offset by (xDelta, yDelta),
 * and the bounding box is updated in the generic part of the
 * item structure.
 *
 *--------------------------------------------------------------
 */

static void glTranslateText(canvas, itemPtr, deltaX, deltaY)
    Tk_Canvas canvas;      /* Canvas containing item. */
    Tk_Item *itemPtr;      /* Item that is being moved. */
    double deltaX, deltaY; /* Amount by which item is to be moved. */
{
    glTextItem *textPtr = (glTextItem *) itemPtr;

    textPtr->x += deltaX;
    textPtr->y += deltaY;
    glComputeTextBbox(canvas, textPtr);
}

/*
 *--------------------------------------------------------------
 *
 * GetTextIndex --
 *
 * Parse an index into a text item and return either its value
 * or an error.
 *
 * Results:
 * A standard Tcl result.  If all went well, then *indexPtr is
 * filled in with the index (into itemPtr) corresponding to
 * string.  Otherwise an error message is left in
 * the interp's result.
 *
 * Side effects:
 * None.
 *
 *--------------------------------------------------------------
 */

static int GetTextIndex(interp, canvas, itemPtr, obj, indexPtr)
    Tcl_Interp *interp;    /* Used for error reporting. */
    Tk_Canvas canvas;      /* Canvas containing item. */
    Tk_Item *itemPtr;      /* Item for which the index is being
             * specified. */
    Tcl_Obj *obj;    /* Specification of a particular character
             * in itemPtr's text. */
    int *indexPtr;      /* Where to store converted character
             * index. */
{
    glTextItem *textPtr = (glTextItem *) itemPtr;
    size_t length;
    int c;
    TkCanvas *canvasPtr = (TkCanvas *) canvas;
    Tk_CanvasTextInfo *textInfoPtr = textPtr->textInfoPtr;
    char *string = Tcl_GetStringFromObj(obj, (int *) &length);

    c = string[0];
    length = strlen(string);

    if ((c == 'e') && (strncmp(string, "end", length) == 0)) {
   *indexPtr = textPtr->numChars;
    } else if ((c == 'i') && (strncmp(string, "insert", length) == 0)) {
   *indexPtr = textPtr->insertPos;
    } else if ((c == 's') && (strncmp(string, "sel.first", length) == 0)
       && (length >= 5)) {
   if (textInfoPtr->selItemPtr != itemPtr) {
       Tcl_SetResult(interp, "selection isn't in item", TCL_STATIC);
       return TCL_ERROR;
   }
   *indexPtr = textInfoPtr->selectFirst;
    } else if ((c == 's') && (strncmp(string, "sel.last", length) == 0)
       && (length >= 5)) {
   if (textInfoPtr->selItemPtr != itemPtr) {
       Tcl_SetResult(interp, "selection isn't in item", TCL_STATIC);
       return TCL_ERROR;
   }
   *indexPtr = textInfoPtr->selectLast;
    } else if (c == '@') {
   double x, y;
   double tmp;
   char *end, *p;

   p = string+1;
   tmp = strtod(p, &end);
   if ((end == p) || (*end != ',')) {
       goto badIndex;
   }
   x = (int) ((tmp < 0) ? tmp - 0.5 : tmp + 0.5);
   p = end+1;
   tmp = strtod(p, &end);
   if ((end == p) || (*end != 0)) {
       goto badIndex;
   }
   y = (int) ((tmp < 0) ? tmp - 0.5 : tmp + 0.5);

    x+=canvasPtr->scrollX1;
    y+=canvasPtr->scrollY1;

   glTextRotateCoord(-textPtr->angle,textPtr->dx,textPtr->dy,&x,&y);
   x-=textPtr->dx;
   y-=textPtr->dy;

   *indexPtr = Tk_PointToChar(textPtr->textLayout,x,y);

    } else if (Tcl_GetIntFromObj((Tcl_Interp *)NULL, obj, indexPtr) == TCL_OK) {
   if (*indexPtr < 0){
       *indexPtr = 0;
   } else if (*indexPtr > textPtr->numChars) {
       *indexPtr = textPtr->numChars;
   }
    } else {
   /*
    * Some of the paths here leave messages in the interp's result,
    * so we have to clear it out before storing our own message.
    */

   badIndex:
   Tcl_SetResult(interp, (char *) NULL, TCL_STATIC);
   Tcl_AppendResult(interp, "bad index \"", string, "\"",
      (char *) NULL);
   return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * SetTextCursor --
 *
 * Set the position of the insertion cursor in this item.
 *
 * Results:
 * None.
 *
 * Side effects:
 * The cursor position will change.
 *
 *--------------------------------------------------------------
 */

static void SetTextCursor(canvas, itemPtr, index)
    Tk_Canvas canvas;      /* Record describing canvas widget. */
    Tk_Item *itemPtr;      /* Text item in which cursor position is to
             * be set. */
    int index;       /* Character index of character just before
             * which cursor is to be positioned. */
{
    glTextItem *textPtr = (glTextItem *) itemPtr;

    if (index < 0) {
   textPtr->insertPos = 0;
    } else  if (index > textPtr->numChars) {
   textPtr->insertPos = textPtr->numChars;
    } else {
   textPtr->insertPos = index;
    }
}

/*
 *--------------------------------------------------------------
 *
 * GetSelText --
 *
 * This procedure is invoked to return the selected portion
 * of a text item.  It is only called when this item has
 * the selection.
 *
 * Results:
 * The return value is the number of non-NULL bytes stored
 * at buffer.  Buffer is filled (or partially filled) with a
 * NULL-terminated string containing part or all of the selection,
 * as given by offset and maxBytes.
 *
 * Side effects:
 * None.
 *
 *--------------------------------------------------------------
 */

static int GetSelText(canvas, itemPtr, offset, buffer, maxBytes)
    Tk_Canvas canvas;      /* Canvas containing selection. */
    Tk_Item *itemPtr;      /* Text item containing selection. */
    int offset;         /* Byte offset within selection of first
             * character to be returned. */
    char *buffer;    /* Location in which to place selection. */
    int maxBytes;    /* Maximum number of bytes to place at
             * buffer, not including terminating NULL
             * character. */
{
    glTextItem *textPtr = (glTextItem *) itemPtr;
    int byteCount;
    char *text, *selStart, *selEnd;
    Tk_CanvasTextInfo *textInfoPtr = textPtr->textInfoPtr;

    if ((textInfoPtr->selectFirst < 0) ||
       (textInfoPtr->selectFirst > textInfoPtr->selectLast)) {
   return 0;
    }
    text = textPtr->text;
    selStart = Tcl_UtfAtIndex(text, textInfoPtr->selectFirst);
    selEnd = Tcl_UtfAtIndex(selStart,
       textInfoPtr->selectLast + 1 - textInfoPtr->selectFirst);
    byteCount = selEnd - selStart - offset;
    if (byteCount > maxBytes) {
   byteCount = maxBytes;
    }
    if (byteCount <= 0) {
   return 0;
    }
    memcpy(buffer, selStart + offset, (size_t) byteCount);
    buffer[byteCount] = '\0';
    return byteCount;
}

/*
 *--------------------------------------------------------------
 *
 * glTextToPostscript --
 *
 * This procedure is called to generate Postscript for
 * text items.
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

int glPostscripTextLayout(Tcl_Interp *Interp,Tk_Canvas canvas,Tk_TextLayout layout,XColor *color,T_glBitmap *stipple,int angle,int x,int y,Tk_Anchor anchor,Tk_Justify justify) {

   int    dx,dy;
   char   buf[64];
   char   *dj;
   TextLayout *layoutPtr;
   Tk_FontMetrics tkm;

   layoutPtr=(TextLayout*)layout;
   Tk_GetFontMetrics(layoutPtr->tkfont,&tkm);

   if (layoutPtr->tkfont) {
      if (Tk_CanvasPsFont(Interp,canvas,layoutPtr->tkfont)!=TCL_OK) {
         return(TCL_ERROR);
      }
   }

   if (color) {
      if (Tk_CanvasPsColor(Interp,canvas,color) != TCL_OK) {
         return(TCL_ERROR);
      }
   }

   if (stipple) {
      Tcl_AppendResult(Interp,"/StippleText {\n    ",(char*)NULL);
      if (glPostscriptStipple(Interp,stipple)!= TCL_OK) {
         return(TCL_ERROR);
      }
      Tcl_AppendResult(Interp,"} bind def\n",(char*)NULL);
   }

   sprintf(buf,"gsave %.15g %.15g %d [\n",(double)x,Tk_CanvasPsY(canvas,y),-angle);
   Tcl_AppendResult(Interp,buf,(char*)NULL);
   Tk_TextLayoutToPostscript(Interp,layout);

   dx=dy=0;
   dj=NULL;

   switch (anchor) {
      case TK_ANCHOR_NW:     dx=0; dy=0; break;
      case TK_ANCHOR_N:      dx=1; dy=0; break;
      case TK_ANCHOR_NE:     dx=2; dy=0; break;
      case TK_ANCHOR_E:      dx=2; dy=1; break;
      case TK_ANCHOR_SE:     dx=2; dy=2; break;
      case TK_ANCHOR_S:      dx=1; dy=2; break;
      case TK_ANCHOR_SW:     dx=0; dy=2; break;
      case TK_ANCHOR_W:      dx=0; dy=1; break;
      case TK_ANCHOR_CENTER: dx=1; dy=1; break;
   }

   switch (justify) {
      case TK_JUSTIFY_LEFT:   dj="0";break;
      case TK_JUSTIFY_CENTER: dj="0.5";break;
      case TK_JUSTIFY_RIGHT:  dj="1";break;
   }

   sprintf(buf,"] %d %g %g %s %s DrawRotatedText grestore\n",tkm.linespace,dx/-2.0,dy/2.0,dj,((stipple==NULL)?"false":"true"));
   Tcl_AppendResult(Interp,buf,(char*)NULL);

   return(TCL_OK);
}

static int glTextToPostscript(interp, canvas, itemPtr, prepass)
    Tcl_Interp *interp;    /* Leave Postscript or error message here. */
    Tk_Canvas canvas;      /* Information about overall canvas. */
    Tk_Item *itemPtr;      /* Item for which Postscript is wanted. */
    int prepass;     /* 1 means this is a prepass to collect
             * font information; 0 means final Postscript
             * is being created. */
{
    glTextItem *textPtr = (glTextItem *) itemPtr;
    XColor *color;
    T_glBitmap *stipple;
    Tk_State state = itemPtr->state;

    if(state == TK_STATE_NULL) {
   state = ((TkCanvas *)canvas)->canvas_state;
    }
    color = textPtr->color;
    stipple = textPtr->stipple;
    if (state == TK_STATE_HIDDEN || textPtr->color == NULL ||
       textPtr->text == NULL || *textPtr->text == 0) {
   return TCL_OK;
    } else if (((TkCanvas *)canvas)->currentItemPtr == itemPtr) {
   if (textPtr->activeColor) {
       color = textPtr->activeColor;
   }
   if (textPtr->activeStipple) {
       stipple = textPtr->activeStipple;
   }
    } else if (state==TK_STATE_DISABLED) {
   if (textPtr->disabledColor) {
       color = textPtr->disabledColor;
   }
   if (textPtr->disabledStipple) {
       stipple = textPtr->disabledStipple;
   }
    }

   return(glPostscripTextLayout(interp,canvas,textPtr->textLayout,color,stipple,textPtr->angle,textPtr->x,textPtr->y,textPtr->anchor,textPtr->justify));
}
