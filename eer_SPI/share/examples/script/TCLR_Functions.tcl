#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"

package require TclR
package require Logger

Log::Start [info script] 0.1

#----- Switch for R traces

set filename [lindex $argv 0]
switch $filename {
    stdout  { set fd stdout }
    stderr  { set fd stderr }
    ""      { set fd [open /dev/null w] }
    default { set fd [open $filename w] }
}

#----- Test the configuration

R configure -resulttraces PRINT_ALL -commandtraces PRINT_ALL -tracechannel $fd

#----- Test tcl->R->tcl the standard way

Log::Print INFO "Testing tcl->R->tcl (standard)"

set d [dict create nums [list 1 2 3 4 5] strs [list bla1 bla2 bla3 bla4 bla5] attr.row.names [list row1 row2 row3 row4 row5] attr.class data.frame attr.custom myattrib]
R tcl2r $d df
R print df

if { [lindex [R type df] 0] != "VECSXP" } {
    Log::Print ERROR "Not the right type"
}

set df [R r2tcl df]

#----- Make sure we get back the same thing

if { [llength [dict keys $d]] != [llength [dict keys $df]] } {
    Log::Print ERROR "Not the same number of elements"
}

foreach key [dict keys $d] {
    if { ![dict exists $df $key] } {
        Log::Print ERROR "The key \"$key\" is missing from the result"
    } else {
        set v1 [dict get $d $key]
        set v2 [dict get $d $key]

        if { [llength $v1] != [llength $v2] } {
            Log::Print ERROR "The number of values for the key \"$key\" differ"
        } elseif { $v1 != $v2 } {
            Log::Print ERROR "The values for the key \"$key\" differ"
        }
    }
}

#----- Test tcl->R->tcl with data.frames

Log::Print INFO "Testing tcl->R->tcl (data.frame)"

set lst { {H1 H2 H3 H4 H5} {{r11 r12 r13 1 1.5} {r21 r22 r23 2 2.5} {r31 r32 r33 3 3.5}} }
 
R tcllst2rdf $lst rdf
set rdf [R rdf2tcllst rdf]
R exec {rdft<-c(typeof(rdf$H1),typeof(rdf$H2),typeof(rdf$H3),typeof(rdf$H4),typeof(rdf$H5))}
set rdft [R r2tcl rdft]

if { $rdft != [list character character character integer double] } {
    Log::Print ERROR "Wrong vector types"
}

if { [llength $lst]!=[llength $rdf] } {
    Log::Print ERROR "Wrong overall dimensions : [llength $lst]!=[llength $rdf]"
} elseif { [llength [lindex $lst 0]]!=[llength [lindex $rdf 0]] } {
    Log::Print ERROR "Wrong header dimensions : [llength [lindex $lst 0]]!=[llength [lindex $rdf 0]]"
} elseif { [llength [lindex $lst 1]]!=[llength [lindex $rdf 1]] } {
    Log::Print ERROR "Wrong row count : [llength [lindex $lst 1]]!=[llength [lindex $rdf 1]]"
}

foreach r1 [lindex $lst 1] r2 [lindex $lst 1] {
    if { [llength $r1] != [llength $r2] } {
        Log::Print ERROR "Wrong row dimension [llength $r1]!=[llength $r2]"
    } elseif { $r1 != $r2 } {
        Log::Print ERROR "The rows differ \[$r1\]!=\[$r2\]"
    }
}

#----- With a forced type data.frame

Log::Print INFO "Testing tcl->R->tcl (forced type data.frame)"
 
R tcllst2rdf $lst rdf {3 STRING 2 DOUBLE}
set rdf [R rdf2tcllst rdf]
R exec {rdft<-c(typeof(rdf$H1),typeof(rdf$H2),typeof(rdf$H3),typeof(rdf$H4),typeof(rdf$H5))}
set rdft [R r2tcl rdft]

if { $rdft != [list character character character double double] } {
    Log::Print ERROR "Wrong vector types"
}


if { [llength $lst]!=[llength $rdf] } {
    Log::Print ERROR "Wrong overall dimensions : [llength $lst]!=[llength $rdf]"
} elseif { [llength [lindex $lst 0]]!=[llength [lindex $rdf 0]] } {
    Log::Print ERROR "Wrong header dimensions : [llength [lindex $lst 0]]!=[llength [lindex $rdf 0]]"
} elseif { [llength [lindex $lst 1]]!=[llength [lindex $rdf 1]] } {
    Log::Print ERROR "Wrong row count : [llength [lindex $lst 1]]!=[llength [lindex $rdf 1]]"
}

foreach r1 [lindex $lst 1] r2 [lindex $lst 1] {
    if { [llength $r1] != [llength $r2] } {
        Log::Print ERROR "Wrong row dimension [llength $r1]!=[llength $r2]"
    } elseif { $r1 != $r2 } {
        Log::Print ERROR "The rows differ \[$r1\]!=\[$r2\]"
    }
}

#----- Use R to modify the data and get it back into tcl

Log::Print INFO "Testing tcl->R (modif)->tcl"

set ints [list 1 2 3 4 5 6 7 8 9]
set res  [list 11 12 13 14 15 16 17 18 19]
R tcl2r $ints rints
R exec {rints <- as.integer(rints+10)}
set rints [R r2tcl rints]

if { $rints != $res } {
    Log::Print ERROR "The new list of number doesn't match \[$rints\] != \[$res\]"
}

if { $fd ni {stdout stderr} } {
    close $fd
}

Log::End -1
