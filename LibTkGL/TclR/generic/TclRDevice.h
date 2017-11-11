#ifndef _TCLRDEVICE_H
#define _TCLRDEVICE_H

#include <Rinternals.h>

SEXP TclRDevice_Init();
int TclRDevice_Install(Tcl_Interp *Interp);

#endif //_TCLRDEVICE_H
