#ifndef TCLRDEVICE2PS_H
#define TCLRDEVICE2PS_H

#include <tcl.h>
#include <tk.h>

#define PSDBGPRINTF(...) printf(__VA_ARGS__);
//#define PSDBGPRINTF(...)

int TclRDevice2PS_Dev2PS(Tcl_Interp *Interp,void *GE,Tcl_Obj *PS);

#endif //TCLRDEVICE2PS_H
