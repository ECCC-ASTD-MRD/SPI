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

// Driver includes
#include "TclRDeviceX.h"
#include "TclRDevicePrintf.h"
#include "tkCanvRDevice.h"

SEXP TclRDevice_InitFromR() {
    //pDevDesc dev = NULL;
    //pGEDevDesc ge = NULL;

    //// Make sure we have a slot for the device
    //R_CheckDeviceAvailable();

    //// Allocate a new device
    //if( !(dev=TclRDevicePrintf_NewDev(NULL)) ) {
    //    error("unable to start TclR Device");
    //    //return R_NilValue;
    //}

    //// Create the associated graphic engine
    ////gsetVar(install(".Device"),mkString("TclRDevice"),R_NilValue);
    //if( !(ge=GEcreateDevDesc(dev)) ) {
    //    free(dev);
    //    error("unable to start TclR Device");
    //}

    //// Initialise the graphic engine
    //GEaddDevice(ge);
    //GEinitDisplayList(ge);

    return R_NilValue;
}

//void* TclRDevice_InitFromTcl(Tcl_Interp *Interp,void *ClientData) {
//    pDevDesc dev = NULL;
//    pGEDevDesc ge = NULL;
//
//    // Make sure we have a slot for the device
//    if( R_CheckDeviceAvailableBool() == FALSE ) {
//        Tcl_AppendResult(Interp,"No device available",NULL);
//        return NULL;
//    }
//
//    // Allocate a new device
//    if( !(dev=TclRDeviceX_NewDev(ClientData)) ) {
//        Tcl_AppendResult(Interp,"Unable to create TclR Device",NULL);
//        return NULL;
//    }
//
//    // Create the associated graphic engine
//    //gsetVar(install(".Device"),mkString("TclRDevice"),R_NilValue);
//    if( !(ge=GEcreateDevDesc(dev)) ) {
//        free(dev);
//        Tcl_AppendResult(Interp,"Unable to create graphic engine for TclR Device",NULL);
//        return NULL;
//    }
//
//    // Initialise the graphic engine
//    GEaddDevice(ge);
//    GEinitDisplayList(ge);
//
//    return (void*)ge;
//}

int TclRDevice_Install(Tcl_Interp *Interp) {
    SEXP rcmd,rexpr,res=R_NilValue;
    ParseStatus pstatus;
    int err;
    const char *install_cmd = "TclRDevice <- function() {.Call(\"TclRDevice_Init\")}";

    //----- Install the canvas item

    RDeviceItem_Register();

    //----- Register the TclRDevice_Init function of the current DLL (shared object) as if it would have been if it was loaded by a module

    R_CallMethodDef cdef[] = {{"TclRDevice_Init",(DL_FUNC)&TclRDevice_InitFromR,0},{NULL,NULL,-1}};
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

