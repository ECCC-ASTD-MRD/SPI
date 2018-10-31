#ifndef TKCANVRDEVICE_H
#define TKCANVRDEVICE_H

void RDeviceItem_Register();
void RDeviceItem_SignalRedraw(void *Item);
void RDeviceItem_DetachDevice(void *Item);
Tk_Font RDeviceItem_GetFont(void *Item);

#endif // TKCANVRDEVICE_H
