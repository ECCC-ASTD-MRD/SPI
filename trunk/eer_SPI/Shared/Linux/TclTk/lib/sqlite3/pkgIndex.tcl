if {[catch {package require Tcl 8.2}]} return
package ifneeded sqlite3 3.3.17 [list load [file join $dir libtclsqlite3.so] tclsqlite3]
