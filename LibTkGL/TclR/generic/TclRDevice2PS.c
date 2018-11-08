// Normal includes
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// R includes
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>

// Driver includes
#include "TclRDevice2PS.h"

#define PSDBGPRINTF(...) printf(__VA_ARGS__);
//#define PSDBGPRINTF(...)

typedef struct TCtx {
    Tcl_Obj     *PS;                // PostScript buffer (MUST be first)
    Tk_Window   TkWin;              // TkWindow (MUST be second for PS functionality)
} TCtx;


// Helper functions


/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_GCColor>
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
static void TclRDevice2PS_GCColor(TCtx *restrict Ctx,rcolor RCol) {
    PSDBGPRINTF("RCol=%u (%u,%u,%u)\n",RCol,R_RED(RCol),R_GREEN(RCol),R_BLUE(RCol));

    Tcl_AppendPrintfToObj(Ctx->PS,"%f %f %f setrgbcolor\n",R_RED(RCol)/255.0,R_GREEN(RCol)/255.0,R_BLUE(RCol)/255.0);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_GCLine>
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
static void TclRDevice2PS_GCLine(TCtx *restrict Ctx,const pGEcontext restrict GEC) {
    int capstyle=1,joinstyle=1;

    PSDBGPRINTF("lwd(%f) lty(%d) lend(%d) ljoin(%d)\n",GEC->lwd,GEC->lty,GEC->lend,GEC->ljoin);

    if( GEC->lty==LTY_BLANK || GEC->lty==LTY_SOLID ) {
        Tcl_AppendToObj(Ctx->PS,"[] 0 setdash\n",13);
    } else {
        int i,val;

        Tcl_AppendToObj(Ctx->PS,"[",1);
        for(i=0; i<2*sizeof(GEC->lty); ++i) {
            val = (GEC->lty>>4*i)&0xf;
            if( !val )
                break;
            Tcl_AppendPrintfToObj(Ctx->PS,"%d ",val);
        }
        Tcl_AppendToObj(Ctx->PS,"] 0 setdash\n",12);
    }

    switch( GEC->lend ) {
        case GE_ROUND_CAP:  capstyle=1; break;
        case GE_BUTT_CAP:   capstyle=0; break;
        case GE_SQUARE_CAP: capstyle=2; break;
    }

    switch( GEC->ljoin ) {
        case GE_ROUND_JOIN: joinstyle=1;    break;
        case GE_MITRE_JOIN: joinstyle=0;    break;
        case GE_BEVEL_JOIN: joinstyle=2;    break;
    }

    Tcl_AppendPrintfToObj(Ctx->PS,"%u %d %d rd_lstyle\n",(unsigned int)round(GEC->lwd),capstyle,joinstyle);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_GCFont>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Set the font Graphic Context from an R graphical engine context
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
static void TclRDevice2PS_GCFont(TCtx *restrict Ctx,const pGEcontext restrict GEC) {
    Tcl_Obj     *objs[8],*lst;
    int         nobj=0;
    Tk_Font     font;
    int         fontsize = (int)round(GEC->ps*GEC->cex);


    PSDBGPRINTF("Font family(%s) cex(%g) ps(%g) lineheight(%g) fontface(%d) -- fontsize(%d)\n",
            GEC->fontfamily,GEC->cex,GEC->ps,GEC->lineheight,GEC->fontface,fontsize);

    // Specify the family (Keep the same one if none is specified)
    objs[nobj++] = Tcl_NewStringObj("-family",7);
    if( GEC->fontfamily[0] != '\0' ) {
        objs[nobj++] = Tcl_NewStringObj(GEC->fontfamily,-1);
    } else {
        objs[nobj++] = Tcl_NewStringObj("Helvetica",-1);
    }

    // Add the font size (in points) from the "cex" and "ps" params
    objs[nobj++] = Tcl_NewStringObj("-size",5);
    objs[nobj++] = Tcl_ObjPrintf("%d",fontsize);

    // Add the style from the "fontface" param (1=plain, 2=bold, 3=italic, 4=bold-italic)
    objs[nobj++] = Tcl_NewStringObj("-weight",7);
    switch( GEC->fontface ) {
        case 1:
        case 3: objs[nobj++] = Tcl_NewStringObj("normal",6); break;
        case 2:
        case 4: objs[nobj++] = Tcl_NewStringObj("bold",4);   break;
    }

    objs[nobj++] = Tcl_NewStringObj("-slant",6);
    switch( GEC->fontface ) {
        case 1:
        case 2: objs[nobj++] = Tcl_NewStringObj("roman",5);  break;
        case 3:
        case 4: objs[nobj++] = Tcl_NewStringObj("italic",6); break;
    }

    // Make a list out of the arguments
    lst = Tcl_NewListObj(nobj,objs);

    // Get the font
    if( (font=Tk_AllocFontFromObj(NULL,Ctx->TkWin,lst)) ) {
        Tk_FontMetrics  fm;
        Tcl_DString     dstr;

        // Get the PS name of the font
        Tcl_DStringInit(&dstr);
        Tk_PostscriptFontName(font,&dstr);
        Tcl_AppendPrintfToObj(Ctx->PS,"/%s findfont %d scalefont ISOEncode setfont\n",Tcl_DStringValue(&dstr),fontsize);
        Tcl_DStringFree(&dstr);

        // Get the metrics of the font
        Tk_GetFontMetrics(font,&fm);
        Tcl_AppendPrintfToObj(Ctx->PS,"/rd_linespace %d def\n",fm.linespace);

        // Free the font
        Tk_FreeFont(font);
    } else {
        fprintf(stderr,"%s: Could not get font for params \"%s\"\n",__func__,Tcl_GetStringFromObj(lst,NULL));
    }
}


// Standard RDevice function implementations


/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_Circle>
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
static void TclRDevice2PS_Circle(double X,double Y,double R,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    PSDBGPRINTF("Circle @[%.4f,%.4f] r=%.4f\n",X,Y,R);

    // Check if we need to fill the circle
    if( GEC->fill != NA_INTEGER ) {
        TclRDevice2PS_GCColor(ctx,(rcolor)GEC->fill);
        Tcl_AppendPrintfToObj(ctx->PS,"%f %f %f rd_circfill\n",X,Y,R);
    }
    // Check if we need to draw the borders
    if( GEC->col!=NA_INTEGER && (GEC->fill==NA_INTEGER||GEC->fill!=GEC->col) ) {
        TclRDevice2PS_GCLine(ctx,GEC);
        TclRDevice2PS_GCColor(ctx,(rcolor)GEC->col);
        Tcl_AppendPrintfToObj(ctx->PS,"%f %f %f rd_circstroke\n",X,Y,R);
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_Clip>
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
static void TclRDevice2PS_Clip(double X0,double X1,double Y0,double Y1,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    Tcl_AppendPrintfToObj(ctx->PS,"initclip %f %f %f %f rectclip\n",X0,Y0,X1-X0,Y1-Y0);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_Line>
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
static void TclRDevice2PS_Line(double X0,double Y0,double X1,double Y1,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    PSDBGPRINTF("Line [%.4f,%.4f] -> [%.4f,%.4f]\n",X0,Y0,X1,Y1);
    TclRDevice2PS_GCLine(ctx,GEC);
    TclRDevice2PS_GCColor(ctx,(rcolor)GEC->col);
    Tcl_AppendPrintfToObj(ctx->PS,"%f %f %f %f rd_line\n",X0,Y0,X1,Y1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_Mode>
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
static void TclRDevice2PS_Mode(int Mode,pDevDesc Dev) {
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_NewPage>
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
static void TclRDevice2PS_NewPage(const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    PSDBGPRINTF("CLEAR\n");

    // Reset clipping
    Tcl_AppendToObj(ctx->PS,"initclip\n",9);

    // Reset background
    TclRDevice2PS_GCColor(ctx,(rcolor)GEC->fill);
    Tcl_AppendPrintfToObj(ctx->PS,"0 0 %f %f rectfill\n",fabs(Dev->right-Dev->left),fabs(Dev->top-Dev->bottom));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_Polygon>
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
static void TclRDevice2PS_Polygon(int N,double *X,double *Y,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx    *ctx    = (TCtx*)Dev->deviceSpecific;

    PSDBGPRINTF("Polygon (%d)\n",N);

    if( N>0 ) {
        int hasFill = GEC->fill != NA_INTEGER;
        int hasBd   = GEC->col!=NA_INTEGER && (GEC->fill==NA_INTEGER||GEC->fill!=GEC->col);
        int i;

        // Make the path
        for(i=0; i<N; ++i) {
            // Make sure line is not too long
            if( i && (i&~255)==i )
                Tcl_AppendToObj(ctx->PS,"\n",1);

            Tcl_AppendPrintfToObj(ctx->PS,"%f %f ",X[i],Y[i]);
        }
        Tcl_AppendPrintfToObj(ctx->PS,"%d rd_path\n",N);

        // Save it if we need to use the path twice
        if( hasFill && hasBd )
            Tcl_AppendToObj(ctx->PS,"gsave\n",6);

        // Check if we need to fill the polygon
        if( hasFill ) {
            TclRDevice2PS_GCColor(ctx,(rcolor)GEC->fill);
            Tcl_AppendToObj(ctx->PS,"fill\n",5);
        }

        // Restore the path if we need to use the path twice
        if( hasFill && hasBd )
            Tcl_AppendToObj(ctx->PS,"grestore\n",9);

        // Check if we need to draw the borders
        if( hasBd ) {
            TclRDevice2PS_GCLine(ctx,GEC);
            TclRDevice2PS_GCColor(ctx,(rcolor)GEC->col);
            Tcl_AppendToObj(ctx->PS,"stroke\n",7);
        }
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_Polyline>
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
static void TclRDevice2PS_Polyline(int N,double *X,double *Y,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx    *ctx = (TCtx*)Dev->deviceSpecific;

    PSDBGPRINTF("Polyline (%d)\n",N);

    if( N > 0 ) {
        int i;

        // Make the path
        for(i=0; i<N; ++i) {
            // Make sure line is not too long
            if( i && (i&~255)==i )
                Tcl_AppendToObj(ctx->PS,"\n",1);

            Tcl_AppendPrintfToObj(ctx->PS,"%f %f ",X[i],Y[i]);
        }
        Tcl_AppendPrintfToObj(ctx->PS,"%d rd_path\n",N);

        // Draw the path
        TclRDevice2PS_GCLine(ctx,GEC);
        TclRDevice2PS_GCColor(ctx,(rcolor)GEC->col);
        Tcl_AppendToObj(ctx->PS,"stroke\n",7);
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_Rect>
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
static void TclRDevice2PS_Rect(double X0,double Y0,double X1,double Y1,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx *ctx = (TCtx*)Dev->deviceSpecific;

    PSDBGPRINTF("Rect [%.4f,%.4f] -> [%.4f,%.4f]\n",X0,Y0,X1,Y1);

    // Check if we need to fill the rectangle
    if( GEC->fill != NA_INTEGER ) {
        TclRDevice2PS_GCColor(ctx,(rcolor)GEC->fill);
        Tcl_AppendPrintfToObj(ctx->PS,"%f %f %f %f rectfill\n",X0,Y0,X1-X0,Y1-Y0);
    }
    // Check if we need to draw the borders
    if( GEC->col!=NA_INTEGER && (GEC->fill==NA_INTEGER||GEC->fill!=GEC->col) ) {
        TclRDevice2PS_GCLine(ctx,GEC);
        TclRDevice2PS_GCColor(ctx,(rcolor)GEC->col);
        Tcl_AppendPrintfToObj(ctx->PS,"%f %f %f %f rectstroke\n",X0,Y0,X1-X0,Y1-Y0);
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_Raster>
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
static void TclRDevice2PS_Raster(unsigned int *Raster,int W,int H,double X,double Y,double Width,double Height,double Rot,Rboolean Interp,const pGEcontext restrict GEC,pDevDesc Dev) {
    fprintf(stderr,"%s: raster export to postscript is not supported yet\n",__func__);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_Text>
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
static void TclRDevice2PS_Text(double X,double Y,const char *Str,double Rot,double HAdj,const pGEcontext restrict GEC,pDevDesc Dev) {
    TCtx        *ctx = (TCtx*)Dev->deviceSpecific;
    Tcl_UniChar u8ch;
    const char  *pstr=Str;
    char        c;

    PSDBGPRINTF("Text @[%.4f,%.4f] rotated[%.2f] hadj(%.4f) : (%s)\n",X,Y,Rot,HAdj,Str);
    TclRDevice2PS_GCColor(ctx,(rcolor)GEC->col);
    TclRDevice2PS_GCFont(ctx,GEC);

    Tcl_AppendPrintfToObj(ctx->PS,"%f %f %f (",Rot,X,Y);
    // We need to remove unicode characters (not supported by PS as far as I know, although the base Latin1 (from UTF8) is defined in the default canvas-outputted PS)
    pstr += Tcl_UtfToUniChar(pstr,&u8ch);
    while( u8ch ) {
        // Escape normal control characters and reserved PS characters
        if( u8ch=='(' || u8ch==')' || u8ch=='\\' || u8ch<0x20 ) {
            Tcl_AppendPrintfToObj(ctx->PS,"\\%03o",u8ch);
        } else if( u8ch <= 0x7f ) {
            // Normal ascii range
            c = (char)u8ch;
            Tcl_AppendToObj(ctx->PS,&c,1);
        } else if( u8ch>=0xa0 && u8ch<=0xff ) {
            // This range seems to be included in the canvas-outputted encoding in PostScript
            // I expect the range to change between locale, but in the worst case we'd have encoding problems, so we might as well allow it
            Tcl_AppendPrintfToObj(ctx->PS,"\\%03o",u8ch);
        }
        pstr += Tcl_UtfToUniChar(pstr,&u8ch);
    }
    Tcl_AppendToObj(ctx->PS,") rd_text\n",-1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_ToPostScript>
 * Creation     : Novembre 2017 - E. Legault-Ouellet
 *
 * But          : Return the PostScript instructions that would draw the current content of the device
 *
 * Parametres   :
 *  <GE>        : Graphics Engine description pointer
 *
 * Retour       :
 *
 * Remarque     : This function should only be called by the rdevice canvas item
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclRDevice2PS_Run(void *GE,Tcl_Obj *PS) {
    TCtx *ctx = (TCtx*)((pGEDevDesc)GE)->dev->deviceSpecific;

    Tcl_AppendToObj(PS,"\n%RDEVICE BEGIN\n",-1);

    // Install the PostScript procedures
    Tcl_AppendToObj(PS,"/rd_circfill { 0 360 arc fill } def\n",-1);
    Tcl_AppendToObj(PS,"/rd_circstroke { 0 360 arc stroke } def\n",-1);
    Tcl_AppendToObj(PS,"/rd_lstyle { setlinejoin setlinecap setlinewidth } def\n",-1);
    Tcl_AppendToObj(PS,"/rd_path { 1 dict begin /npts exch 1 sub def newpath moveto npts { lineto } repeat end } def\n",-1);
    Tcl_AppendToObj(PS,"/rd_line { newpath moveto lineto stroke } def\n",-1);
    Tcl_AppendToObj(PS,"/rd_text { 4 dict begin /txt exch def /rot exch def /y exch def /x exch def save x y rot [[txt]] rd_linespace 0 1 0 false DrawText restore end } def\n",-1);

    // Replay the display list
    ctx->PS = PS;
    GEplayDisplayList(GE);
    ctx->PS = NULL;

    Tcl_AppendToObj(PS,"%RDEVICE END\n",-1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclRDevice2PS_Dev2PS>
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
int TclRDevice2PS_Dev2PS(Tcl_Interp *Interp,void *GE,Tcl_Obj *PS) {
    pDevDesc dev,devbckp;   
    
    if( GE && PS ) {
        // Make a backup of the current device
        if( !(devbckp=calloc(1,sizeof(*devbckp))) ) {
            Tcl_AppendResult(Interp,"Could not allocate memory for device backup",NULL);
            return TCL_ERROR;
        }
        dev = ((pGEDevDesc)GE)->dev;
        memcpy(devbckp,dev,sizeof(*devbckp));

        // Device functions override
        dev->activate       = NULL;
        dev->circle         = (void*)TclRDevice2PS_Circle;
        dev->clip           = (void*)TclRDevice2PS_Clip; // Apparently called even if canClip is FALSE
        // dev->close : no override as it would have to close the underlying device. It shouldn't get called anyway.
        dev->deactivate     = NULL;
        dev->locator        = NULL;
        dev->line           = (void*)TclRDevice2PS_Line;
        // dev->metricInfo : we'll keep the same measures as for the original device
        dev->mode           = (void*)TclRDevice2PS_Mode;
        dev->newPage        = (void*)TclRDevice2PS_NewPage;
        dev->polygon        = (void*)TclRDevice2PS_Polygon;
        dev->polyline       = (void*)TclRDevice2PS_Polyline;
        dev->rect           = (void*)TclRDevice2PS_Rect;
        dev->path           = NULL;
        dev->raster         = (void*)TclRDevice2PS_Raster;
        dev->cap            = NULL; // Makes no sense in this context anyway
        // dev->size : No override as it would be pointless. Besides, this is mostly called by the device itself, and we won't resize in here, promise!
        // dev->strWidth : we'll keep the same measures as for the original device
        dev->text           = (void*)TclRDevice2PS_Text;
        dev->onExit         = NULL;
        dev->textUTF8       = (void*)TclRDevice2PS_Text;
        // dev->strWidthUTF8 : we'll keep the same measures as for the original device
        dev->eventHelper    = NULL;
        dev->holdflush      = NULL;

        // Make the postscript rendering
        TclRDevice2PS_Run(GE,PS);

        // Restore the device's functions
        dev->activate       = devbckp->activate;
        dev->circle         = devbckp->circle;
        dev->clip           = devbckp->clip;
        dev->close          = devbckp->close;
        dev->deactivate     = devbckp->deactivate;
        dev->locator        = devbckp->locator;
        dev->line           = devbckp->line;
        dev->metricInfo     = devbckp->metricInfo;
        dev->mode           = devbckp->mode;
        dev->newPage        = devbckp->newPage;
        dev->polygon        = devbckp->polygon;
        dev->polyline       = devbckp->polyline;
        dev->rect           = devbckp->rect;
        dev->path           = devbckp->path;
        dev->raster         = devbckp->raster;
        dev->cap            = devbckp->cap;
        dev->size           = devbckp->size;
        dev->strWidth       = devbckp->strWidth;
        dev->text           = devbckp->text;
        dev->onExit         = devbckp->onExit;
        dev->textUTF8       = devbckp->textUTF8;
        dev->strWidthUTF8   = devbckp->strWidthUTF8;
        dev->eventHelper    = devbckp->eventHelper;
        dev->holdflush      = devbckp->holdflush;

        free(devbckp);
    }

    return TCL_OK;
}
