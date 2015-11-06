#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"

package require TclR
package require Logger

Log::Start [info script] 0.2

#----- Switch for R traces

set filename [lindex $argv 0]
switch $filename {
    stdout  { set fd stdout }
    stderr  { set fd stderr }
    ""      { set fd [open /dev/null w] }
    default { set fd [open $filename w] }
}

proc CheckDF { DF1 DF2 } {
    if { [llength $DF1]!=[llength $DF2] } {
        Log::Print ERROR "Wrong overall dimensions : [llength $DF1]!=[llength $DF2]"
    } elseif { [llength [lindex $DF1 0]]!=[llength [lindex $DF2 0]] } {
        Log::Print ERROR "Wrong header dimensions : [llength [lindex $DF1 0]]!=[llength [lindex $DF2 0]]"
    } elseif { [llength [lindex $DF1 1]]!=[llength [lindex $DF2 1]] } {
        Log::Print ERROR "Wrong row count : [llength [lindex $DF1 1]]!=[llength [lindex $DF2 1]]"
    }

    foreach r1 [lindex $DF1 1] r2 [lindex $DF2 1] {
        if { [llength $r1] != [llength $r2] } {
            Log::Print ERROR "Wrong row dimension [llength $r1]!=[llength $r2]"
        } elseif { $r1 != $r2 } {
            Log::Print ERROR "The rows differ \[$r1\]!=\[$r2\]"
        }
    }
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

CheckDF $lst $rdf

#----- With a forced type data.frame

Log::Print INFO "Testing tcl->R->tcl (forced type data.frame)"
 
R tcllst2rdf $lst rdf {3 STRING 2 DOUBLE}
set rdf [R rdf2tcllst rdf]
R exec {rdft<-c(typeof(rdf$H1),typeof(rdf$H2),typeof(rdf$H3),typeof(rdf$H4),typeof(rdf$H5))}
set rdft [R r2tcl rdft]

if { $rdft != [list character character character double double] } {
    Log::Print ERROR "Wrong vector types"
}

set lst { {H1 H2 H3 H4 H5} {{r11 r12 r13 1.0 1.5} {r21 r22 r23 2.0 2.5} {r31 r32 r33 3.0 3.5}} }
CheckDF $lst $rdf

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

#----- Test the -get option with the default

Log::Print INFO "Testing \[-get\] option"

if { [set res [R exec -get {as.integer(5+10)}]] != 15 } {
    Log::Print ERROR "The result doesn't match \[$res\] != \[15\]"
}

#----- Test the -get option with r2tcl

Log::Print INFO "Testing \[-get r2tcl\] option"

if { [set res [R exec -get r2tcl {as.integer(5+10)}]] != 15 } {
    Log::Print ERROR "The result doesn't match \[$res\] != \[15\]"
}

#----- Test the -get option with rdf2tcllst

Log::Print INFO "Testing \[-get rdf2tcllst\] option"

set lst { {H1 H2 H3 H4 H5} {{r11 r12 r13 1 1.5} {r21 r22 r23 2 2.5} {r31 r32 r33 3 3.5}} }
set rdf [R exec -get rdf2tcllst {data.frame(H1=c('r11','r21','r31'),H2=c('r12','r22','r32'),H3=c('r13','r23','r33'),H4=as.integer(c(1,2,3)),H5=c(1.5,2.5,3.5),stringsAsFactors=FALSE)}]

CheckDF $lst $rdf

#----- Test the fstd2r function

Log::Print INFO "Testing \[FSTD->R\] (fstd2r)"

package require TclData

fstdfile open FLE read DataIn/2005102612_012
fstdfield read FLD FLE 131073 ;# TT at 1.000 sg (12000) for T+18

R fstd2r FLD rfld
set rsum [R exec -get {sum(rfld)}]
set tsum [vexpr NIL ssum(FLD)]

if { $rsum != $tsum } {
    Log::Print ERROR "The result doesn't match \[$rsum\] != \[$tsum\]"
}

fstdfield free FLD
fstdfile close FLE

#----- Close any open channel

if { $fd ni {stdout stderr} } {
    close $fd
}

Log::End -1
