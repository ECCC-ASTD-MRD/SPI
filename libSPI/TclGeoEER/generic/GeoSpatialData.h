
#ifndef _GeoSpatialData_h
#define _GeoSpatialData_h

#include "tclUtils.h"
#include "tkCanvVP.h"
#include "Projection.h"

typedef void       (GeoSpatialData_Render) (Tcl_Interp *Interp,ClientData Data,ViewportItem *VP,Projection *Proj,unsigned int Mode,unsigned int Type);
typedef ClientData (GeoSpatialData_Get)    (char *Name);

typedef struct GeoSpatialDataType {
   ClientData               *Data;
   GeoSpatialData_Get       *Get;
   GeoSpatialData_Render    *Render;
} GeoSpatialDataType;

int                 GeoSpatialData_CreateType(Tcl_Interp *Interp,char *Name,GeoSpatialData_Get *Get,GeoSpatialData_Render *Render);
GeoSpatialDataType *GeoSpatialData_Find(char *Name,ClientData *Data);

#endif