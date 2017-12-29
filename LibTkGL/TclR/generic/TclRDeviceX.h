#ifndef TCLRDEVICEX_H
#define TCLRDEVICEX_H

#include <tcl.h>
#include <tk.h>

void* TclRDeviceX_Init(Tcl_Interp *Interp,void *Item,Tk_Window TkWin,int W,int H);
void TclRDeviceX_Destroy(void* GE);
Pixmap TclRDeviceX_GetPixmap(void* GE);

#endif //TCLRDEVICEX_H
