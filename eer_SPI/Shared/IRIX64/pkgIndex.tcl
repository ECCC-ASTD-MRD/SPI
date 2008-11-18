#
# Tcl package index file, version 1.1
#
if {[package vsatisfies [package provide Tcl] 8.4]} {
    package ifneeded TclData 7.2.4 [list load [file join $dir libTkViewport.so] TclData]
    package ifneeded TkViewport 7.2.4 [list load [file join $dir libTkViewport.so] TkViewport]
    package ifneeded TkglCanvas 8.4.12 [list load [file join $dir libTkglCanvas.so] glCanvas]
} else {
   puts stderr "Invalid Tcl version"
}
