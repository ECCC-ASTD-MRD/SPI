#ifndef _tclMetObs_Sqlite_h
#define _tclMetObs_Sqlite_h
#include "tclMetObs.h"

int MetObs_LoadSQLite(Tcl_Interp *Interp, const char *database_filename, TMetObs *obs);

#endif // _tclMetObs_Sqlite_h
