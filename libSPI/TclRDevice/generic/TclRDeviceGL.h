#ifndef TCLRDEVICEGL_H
#define TCLRDEVICEGL_H

#include <tcl.h>
#include <tk.h>
#include "glStuff.h"

void* TclRDeviceGL_Init(Tcl_Interp *Interp,void *Item,Tk_Window TkWin,Tk_Font Font,int W,int H);
void TclRDeviceGL_Destroy(void* GE);
void TclRDeviceGL_Redraw(void *GE);
void TclRDeviceGL_Resize(void *GE,int W,int H);
void TclRDeviceGL_SetFont(void *GE,Tk_Font Font);
void TclRDeviceGL_SetAlias(void *GE,int Alias);
int TclRDeviceGL_CopyBuffer(void *GE,int BufX,int BufY,int W,int H,int DrawX,int DrawY);

#endif //TCLRDEVICEGL_H
