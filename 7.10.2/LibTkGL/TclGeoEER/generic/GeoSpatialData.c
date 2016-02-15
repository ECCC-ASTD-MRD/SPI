
#include "GeoSpatialData.h"

static Tcl_HashTable GeoSpatialDataTypeTable;

int GeoSpatialData_CreateType(Tcl_Interp *Interp,char *Name,GeoSpatialData_Get *Get,GeoSpatialData_Render *Render) {

   GeoSpatialDataType *type;

   if (!(type=(GeoSpatialDataType*)TclY_HashPut(NULL,&GeoSpatialDataTypeTable,Name,sizeof(GeoSpatialDataType)))) {
      Tcl_AppendResult(Interp,"GeoData_CreateType: Unable ot create new GeoData type",(char*)NULL);
      return(TCL_ERROR);
   }

//   type->Name   = strdup(Name);
   type->Get    = Get;
   type->Render = Render;

   return(TCL_OK);
}

GeoSpatialDataType *GeoSpatialData_Find(char *Name,ClientData *Data) {

   GeoSpatialDataType   *type;
   Tcl_HashSearch        ptr;
   Tcl_HashEntry        *entry=NULL;

   entry=Tcl_FirstHashEntry(&GeoSpatialDataTypeTable,&ptr);

   while (entry) {
      type=Tcl_GetHashValue(entry);
      if ((*Data=type->Get(Name))) {
         return(type);
      }
      entry=Tcl_NextHashEntry(&ptr);
   }

   return(NULL);
}
