#ifndef TKCANVRDEVICE_H
#define TKCANVRDEVICE_H

#include <tcl.h>
#include <tk.h>

void RDeviceItem_Register();
void RDeviceItem_SignalRedraw(void *Item);
void RDeviceItem_DetachDevice(void *Item);
void RDeviceItem_SetFont(void *Item,Tk_Font Font);

#endif // TKCANVRDEVICE_H
