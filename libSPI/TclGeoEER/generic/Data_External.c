#include <tclData.h>
#include <Def.h>

int EXT_DataStat(Tcl_Interp *Interp,char *FieldID,char *TypeBuf,int BufSize,int *NI,int *NJ) {
   TData   *fstdfld;
   TDef    *def;
   const char* type = NULL;

   // Get the field
   if( !(fstdfld=Data_Get(FieldID)) ) {
      Tcl_AppendResult(Interp,"Not a valid field",NULL);
      return TCL_ERROR;
   }
   if( !(def=fstdfld->Def) ) {
      Tcl_AppendResult(Interp,"No valid data Def in the field",NULL);
      return TCL_ERROR;
   }

   // Calculate the dimensions
   *NI = def->Limits[0][1] - def->Limits[0][0] + 1;
   *NJ = def->Limits[1][1] - def->Limits[1][0] + 1;

   // Get the type
   switch(def->Type) {
      case TD_Unknown:
      case TD_Binary:   type = NULL;                  break;
      case TD_UByte:    type = "unsigned char";       break;
      case TD_Byte:     type = "char";                break;
      case TD_UInt16:   type = "unsigned short";      break;
      case TD_Int16:    type = "short";               break;
      case TD_UInt32:   type = "unsigned int";        break;
      case TD_Int32:    type = "int";                 break;
      case TD_UInt64:   type = "unsigned long long";  break;
      case TD_Int64:    type = "long long";           break;
      case TD_Float32:  type = "float";               break;
      case TD_Float64:  type = "double";              break;
      default:          type = NULL;                  break;
   }

   // Return the type
   if( type ) {
      sprintf(TypeBuf,"%.*s",BufSize-1,type);
      return TCL_OK;
   } else {
      Tcl_AppendResult(Interp,"Not a valid data type",NULL);
      return TCL_ERROR;
   }
}

#define EXT_DATACOPY(DEF,BUF) { \
   int i,j,ij; \
   for(j=DEF->Limits[1][0]; j<=DEF->Limits[1][1]; ++j) { \
      for(i=DEF->Limits[0][0],ij=j*DEF->NI+i; i<=DEF->Limits[0][1]; ++i,++ij) { \
         Def_Get(DEF,0,ij,*BUF++); \
      } \
   } \
}
int EXT_DataCopy(Tcl_Interp *Interp,char *FieldID,const char *Type,void *Buf,int BufSize,int *Size) {
   TData   *fstdfld;
   TDef    *def;
   int size;

   // Get the field
   if( !(fstdfld=Data_Get(FieldID)) ) {
      Tcl_AppendResult(Interp,"Not a valid field",NULL);
      return TCL_ERROR;
   }
   if( !(def=fstdfld->Def) ) {
      Tcl_AppendResult(Interp,"No valid data Def in the field",NULL);
      return TCL_ERROR;
   }

   // Get the size of the data to copy
   size = (def->Limits[0][1]-def->Limits[0][0]+1) * (def->Limits[1][1]-def->Limits[1][0]+1);
   if( Size )
      *Size = size;
   // Nothing to copy?
   if( !size )
      return TCL_OK;
   // Buffer too small?
   if( BufSize < size ) {
      Tcl_AppendResult(Interp,"Buffer is too small",NULL);
      return TCL_ERROR;
   }

   // Make the copy
   if(      !strcmp(Type,"float")               ) { float               *buf = Buf; EXT_DATACOPY(def,buf); }
   else if( !strcmp(Type,"double")              ) { double              *buf = Buf; EXT_DATACOPY(def,buf); }
   else if( !strcmp(Type,"char")                ) { char                *buf = Buf; EXT_DATACOPY(def,buf); }
   else if( !strcmp(Type,"short")               ) { short               *buf = Buf; EXT_DATACOPY(def,buf); }
   else if( !strcmp(Type,"int")                 ) { int                 *buf = Buf; EXT_DATACOPY(def,buf); }
   else if( !strcmp(Type,"long long")           ) { long long           *buf = Buf; EXT_DATACOPY(def,buf); }
   else if( !strcmp(Type,"unsigned char")       ) { unsigned char       *buf = Buf; EXT_DATACOPY(def,buf); }
   else if( !strcmp(Type,"unsigned short")      ) { unsigned short      *buf = Buf; EXT_DATACOPY(def,buf); }
   else if( !strcmp(Type,"unsigned int")        ) { unsigned int        *buf = Buf; EXT_DATACOPY(def,buf); }
   else if( !strcmp(Type,"unsigned long long")  ) { unsigned long long  *buf = Buf; EXT_DATACOPY(def,buf); }
   else {
      Tcl_AppendResult(Interp,"Unsupported type : [",Type,"]",NULL);
      return TCL_ERROR;
   }

   return TCL_OK;
}

