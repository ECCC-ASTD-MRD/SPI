# Tcl package index file, version 1.1

if {$::tcl_version >= 8.3} {
    if {$::tcl_version == 8.3} {
	set provides {2.1.5}
    } else {
	set provides {2.5}
    }
    package ifneeded Thread $provides \
	    [list load [file join $dir libthread2.5.so] Thread]
}
