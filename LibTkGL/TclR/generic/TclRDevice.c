// Normal includes
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// R includes
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>
#include <R_ext/Parse.h>
#include <R_ext/Rdynload.h>
//#include <R_ext/GraphicsDevice.h>

// Tcl includes
#include <tcl.h>

void TclRDevice_OhBoy() {
    printf("This function should NOT be called...\n");
}

//void (*activate)(const pDevDesc );
void TclRDevice_Circle(double X,double Y,double R,const pGEcontext *GEC,pDevDesc Dev) {
    printf("Circle @[%.4f,%.4f] r=%.4f\n",X,Y,R);
}
void TclRDevice_Clip(double X0,double X1,double Y0,double Y1,pDevDesc Dev) {
    printf("Clip to [%.4f,%.4f] [%.4f,%.4f]\n",X0,Y0,X1,Y1);
}
void TclRDevice_Free(pDevDesc Dev) {
}
//void (*deactivate)(pDevDesc );
//Rboolean (*locator)(double *x,double *y,pDevDesc Dev);
void TclRDevice_Line(double X0,double Y0,double X1,double Y1,const pGEcontext *GEC,pDevDesc Dev) {
    printf("Line [%.4f,%.4f] -> [%.4f,%.4f]\n",X0,Y0,X1,Y1);
}
void TclRDevice_MetricInfo(int C,const pGEcontext *GEC,double *Ascent,double *Descent,double *Width,pDevDesc Dev) {
    *Ascent = 0.0;
    *Descent = 0.0;
    *Width = 0.0;
}
//void (*mode)(int mode,pDevDesc Dev);
void TclRDevice_Clear(const pGEcontext *GEC,pDevDesc Dev) {
    printf("CLEAR\n");
}
void TclRDevice_Polygon(int N,double *X,double *Y,const pGEcontext *GEC,pDevDesc Dev) {
    int i;
    printf("Polygon (%d)\n",N);
    for(i=0; i<N; ++i)
        printf("\t[%d] [%.4f,%.4f]\n",i,X[i],Y[i]);
}
void TclRDevice_Polyline(int N,double *X,double *Y,const pGEcontext *GEC,pDevDesc Dev) {
    int i;
    printf("Polyline (%d)\n",N);
    for(i=0; i<N; ++i)
        printf("\t[%d] [%.4f,%.4f]\n",i,X[i],Y[i]);
}
void TclRDevice_Rect(double X0,double Y0,double X1,double Y1,const pGEcontext *GEC,pDevDesc Dev) {
    printf("Rect [%.4f,%.4f] -> [%.4f,%.4f]\n",X0,Y0,X1,Y1);
}
//void (*path)(double *x,double *y,int npoly,int *nper,Rboolean winding,const pGEcontext *GEC,pDevDesc Dev);
//void (*raster)(unsigned int *raster,int w,int h,double x,double y,double width,double height,double rot,Rboolean interpolate,const pGEcontext *GEC,pDevDesc Dev);
//SEXP (*cap)(pDevDesc Dev);
//void (*size)(double *left,double *right,double *bottom,double *top,pDevDesc Dev);
double TclRDevice_StrWidth(const char *Str,const pGEcontext *GEC,pDevDesc Dev) {
    printf("StrWidth of (%s)\n",Str);
    return strlen(Str)*5.0;
}
void TclRDevice_Text(double X,double Y,const char *Str,double Rot,double HAdj,const pGEcontext *GEC,pDevDesc Dev) {
    printf("Text @[%.4f,%.4f] rotated[%.2f] hadj(%.4f) : (%s)\n",X,Y,Rot,HAdj,Str);
}
//void (*onExit)(pDevDesc Dev);
//SEXP (*getEvent)(SEXP,const char *);
//Rboolean (*newFrameConfirm)(pDevDesc Dev);
//void (*textUTF8)(double x,double y,const char *str,double rot,double hadj,const pGEcontext *GEC,pDevDesc Dev);
//double (*strWidthUTF8)(const char *str,const pGEcontext *GEC,pDevDesc Dev);
//void (*eventHelper)(pDevDesc Dev,int code);
//int (*holdflush)(pDevDesc Dev,int level);

static DevDesc* TclRDevice_New() {
    pDevDesc dev = calloc(1,sizeof(*dev));
    if( dev ) {
        // Device physical parameters
        dev->left       = 0.;
        dev->right      = 1000.;
        dev->bottom     = 0.;
        dev->top        = 1000.;
        //dev->clipLeft   = 0.;
        //dev->clipRight  = 1000.;
        //dev->clipBottom = 0.;
        //dev->clipTop    = 1000.;
        dev->xCharOffset= 0.4900;
        dev->yCharOffset= 0.3333;
        dev->yLineBias  = 0.1;
        dev->ipr[0]     = 1.0/72.0;
        dev->ipr[1]     = 1.0/72.0;
        dev->cra[0]     = 10;
        dev->cra[1]     = 10;
        //dev->gamma      = 1.;

        // Device capabilities
        dev->canClip            = FALSE;
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
        dev->hasTextUTF8        = FALSE;
        dev->wantSymbolUTF8     = FALSE;
        dev->useRotatedTextInContour = FALSE;

        // Initial settings
        dev->startps    = 10.;
        dev->startcol   = R_RGB(0,0,0);
        dev->startfill  = R_TRANWHITE;
        dev->startlty   = LTY_SOLID;
        dev->startfont  = 1;
        dev->startgamma = 1;

        // Device specific
        dev->deviceSpecific = NULL;

        // Display list
        dev->displayListOn  = TRUE;

        // Device functions
        dev->activate       = NULL;
        dev->circle         = (void*)TclRDevice_Circle;
        dev->clip           = (void*)TclRDevice_Clip; // Apparently called even if canClip is TRUE
        dev->close          = (void*)TclRDevice_Free;
        dev->deactivate     = NULL;
        dev->locator        = NULL;
        dev->line           = (void*)TclRDevice_Line;
        dev->metricInfo     = (void*)TclRDevice_MetricInfo;
        dev->mode           = NULL;
        dev->newPage        = (void*)TclRDevice_Clear;
        dev->polygon        = (void*)TclRDevice_Polygon;
        dev->polyline       = (void*)TclRDevice_Polyline;
        dev->rect           = (void*)TclRDevice_Rect;
        dev->path           = NULL;
        dev->raster         = NULL;
        dev->cap            = NULL;
        dev->size           = NULL;
        dev->strWidth       = (void*)TclRDevice_StrWidth;
        dev->text           = (void*)TclRDevice_Text;
        dev->onExit         = NULL;
        dev->textUTF8       = NULL;
        dev->strWidthUTF8   = NULL;

        dev->eventHelper    = NULL;
        dev->holdflush      = NULL;
    }

    return dev;
}

SEXP TclRDevice_Init() {
    pDevDesc dev = NULL;
    pGEDevDesc ge = NULL;

    // Make sure we have a slot for the device
    R_CheckDeviceAvailable();

    // Allocate a new device
    if( !(dev=TclRDevice_New()) ) {
        error("unable to start TclR Device");
        //return R_NilValue;
    }

    // Create the associated graphic engine
    //gsetVar(install(".Device"),mkString("TclRDevice"),R_NilValue);
    if( !(ge=GEcreateDevDesc(dev)) ) {
        free(dev);
        error("unable to start TclR Device");
    }

    // Initialise the graphic engine
    GEaddDevice(ge);
    GEinitDisplayList(ge);

    return R_NilValue;
}

int TclRDevice_Install(Tcl_Interp *Interp) {
    SEXP rcmd,rexpr,res=R_NilValue;
    ParseStatus pstatus;
    int err;
    const char *install_cmd = "TclRDevice <- function() {.Call(\"TclRDevice_Init\")}";

    //----- Register the TclRDevice_Init function of the current DLL (shared object) as if it would have been if it was loaded by a module

    R_CallMethodDef cdef[] = {{"TclRDevice_Init",(DL_FUNC)&TclRDevice_Init,0},{NULL,NULL,-1}};
    R_registerRoutines(R_getEmbeddingDllInfo(),NULL,cdef,NULL,NULL);

    //----- Install an R function so that a call to TclRDevice() in R calls TclRDevice_Init here

    // Put the string in an R vector string
    PROTECT( rcmd=allocVector(STRSXP,1) );
    SET_STRING_ELT(rcmd,0,mkChar(install_cmd));

    // Parse the string
    // 1 is the max number of expr to parse; The null value indicates that this doesn't come from a source file (a filename would then have been given)
    PROTECT( rexpr=R_ParseVector(rcmd,1,&pstatus,R_NilValue) );
    if( pstatus != PARSE_OK ) {
        // Release our protected variables
        UNPROTECT(2);
        Tcl_AppendResult(Interp,"Could not install TclRDevice",NULL);
        return TCL_ERROR;
    }

    // Unprotect the rcmd string
    UNPROTECT(1);

    // Execute the statement and protect the new "res"
    PROTECT( res=R_tryEval(VECTOR_ELT(rexpr,0),R_GlobalEnv,&err) );
    if( err ) {
        Tcl_AppendResult(Interp,"Error while installing TclRDevice",NULL);
        UNPROTECT(2);
        return TCL_ERROR;
    }

    // Unprotect the rexpr
    // Should we also unprotect the result (the function) ? My guess for now is no...
    // Besides, it shouldn't be that bad of a memory loss anyway
    UNPROTECT(1);

    return TCL_OK;
}
