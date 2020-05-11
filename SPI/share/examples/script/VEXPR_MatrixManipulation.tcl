#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"

package require TclData
package require Logger

#set Log::Param(Level) DEBUG
Log::Start [info script] 0.1

proc TestCatch { IsErr args } {
    set err [catch {set res [{*}$args]} msg]
    if { $IsErr } {
        if { $err } {
            Log::Print INFO "==> TEST PASSED : command \[$args\] correctly returned an error : [lindex [split $msg \n] 0]"
            return 1
        } else {
            Log::Print ERROR "==> TEST FAILED : command \[$args\] should return an error and doesn't"
            return 0
        }
    } elseif { $err } {
        Log::Print ERROR "==> TEST FAILED : command \[$args\] returned an error : $msg"
        return 0
    }
    return 1
}

proc TestDim { Fld NI NJ NK } {
    if { [fstdfield define $Fld -NI]!=$NI || [fstdfield define $Fld -NJ]!=$NJ || [fstdfield define $Fld -NK]!=$NK } {
        Log::Print ERROR "==> TEST FAILED : Dimensions are [fstdfield define $Fld -NI]x[fstdfield define $Fld -NJ]x[fstdfield define $Fld -NK] and should be ${NI}x${NJ}x${NK}"
        return 0
    }
    return 1
}

proc TestVal { Tol Fld args } {
    set ni [fstdfield define $Fld -NI]
    set nj [fstdfield define $Fld -NJ]
    set nk [fstdfield define $Fld -NK]

    for {set k 0;set idx 0} {$k<$nk} {incr k} {
        for {set j 0} {$j<$nj} {incr j} {
            for {set i 0} {$i<$ni} {incr i;incr idx} {
                if { abs([vexpr - ${Fld}($i,$j,$k)]-[lindex $args $idx]) > $Tol } {
                    Log::Print ERROR "==> TEST FAILED : V\[$i,$j,$k\]=[vexpr - ${Fld}($i,$j,$k)] != [lindex $args $idx] with tol=$Tol"
                    return 0
                }
            }
        }
    }
    return 1
}

#----- Test seq
Log::Print INFO "Testing seq..."
fstdfield create CTX 1 1 1 Int32
TestCatch 1 vexpr SEQ CTX+seq(10,2,1)
TestCatch 1 vexpr SEQ CTX+seq(-2,-10,1)
TestCatch 1 vexpr SEQ CTX+seq(2,10,-1)
TestCatch 1 vexpr SEQ CTX+seq(2,10,0)
if { [TestCatch 0 vexpr SEQ CTX+seq(1,10,2)] && [TestDim SEQ 5 1 1] && [TestVal 0 SEQ 1 3 5 7 9] } {
    Log::Print INFO "==> TEST PASSED : sequence from 1 to 10 step 2"
}
if { [TestCatch 0 vexpr SEQ CTX+seq(-0.2,-1.2,-.2)] && [TestDim SEQ 6 1 1] && [TestVal 1e-7 SEQ -0.2 -0.4 -0.6 -0.8 -1.0 -1.2] } {
    Log::Print INFO "==> TEST PASSED : sequence from -0.2 to -1.2 step -.2"
}
#if { [TestCatch 0 vexpr SEQ CTX+seq(2,10,15)] && [TestDim SEQ 1 1 1] && [TestVal 0 SEQ 2] } {
#    Log::Print INFO "==> TEST PASSED : sequence from 2 to 10 step 15"
#}

#----- Test repeat
Log::Print INFO "Testing repeat..."
fstdfield create FLD 5 1 1 Int32
TestCatch 1 vexpr REP repeat(FLD,-3)
TestCatch 1 vexpr REP repeat(FLD,FLD)
fstdfield create FLD 5 1 1 Int32
set rep 3
set vals {1 2 3 4 5}
fstdfield define FLD -DATA [binary format n* $vals]
if { [TestCatch 0 vexpr REP repeat(FLD,${rep}.2)] && [TestDim REP 5 $rep 1] && [TestVal 0 REP {*}[lrepeat $rep {*}$vals]] } {
    Log::Print INFO "==> TEST PASSED : repeat a 1D field $rep times"
}
fstdfield create FLD 3 2 1 Float32
set rep 4
set vals {1.5 7.8 6.4 -25.6 -7.1 0.0}
fstdfield define FLD -DATA [binary format f* $vals]
if { [TestCatch 0 vexpr REP repeat(FLD,$rep)] && [TestDim REP 3 2 $rep] && [TestVal 1e-6 REP {*}[lrepeat $rep {*}$vals]] } {
    Log::Print INFO "==> TEST PASSED : repeat a 2D field $rep times"
}
fstdfield create FLD 3 2 3 Float64
set rep 7
set vals {1.5 7.8 6.4 -25.6 -7.1 0.0 1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 10.10 11.11 12.12}
fstdfield define FLD -DATA [binary format d* $vals]
if { [TestCatch 0 vexpr REP repeat(FLD,$rep)] && [TestDim REP 3 2 [expr 3*$rep]] && [TestVal 0 REP {*}[lrepeat $rep {*}$vals]] } {
    Log::Print INFO "==> TEST PASSED : repeat a 3D field $rep times"
}
fstdfield create FLD 1 2 3 Float64
set rep 5
set vals {1.5 7.8 6.4 -25.6 -7.1 0.0}
fstdfield define FLD -DATA [binary format d* $vals]
if { [TestCatch 0 vexpr REP repeat(FLD,$rep)] && [TestDim REP 2 3 $rep] && [TestVal 0 REP {*}[lrepeat $rep {*}$vals]] } {
    Log::Print INFO "==> TEST PASSED : repeat a 2D field $rep times"
}

#----- Test reshape
Log::Print INFO "Testing reshape..."
fstdfield create FLD 3 4 2 Int32
set vals {0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23}
fstdfield define FLD -DATA [binary format n* $vals]
TestCatch 1 vexpr SHP reshape(FLD,4,4,2)
TestCatch 1 vexpr SHP reshape(FLD,2,4,2)
if { [TestCatch 0 vexpr SHP reshape(FLD,3,2,4)] && [TestDim SHP 3 2 4] && [TestVal 0 SHP {*}$vals] } {
    Log::Print INFO "==> TEST PASSED : reshape a 3D field"
}
if { [TestCatch 0 vexpr SHP reshape(FLD,24,1,1)] && [TestDim SHP 24 1 1] && [TestVal 0 SHP {*}$vals] } {
    Log::Print INFO "==> TEST PASSED : reshape a 3D field into a 1D field"
}

Log::End -1
