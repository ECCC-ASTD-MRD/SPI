#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"

package require TclData
package require Logger

#set Log::Param(Level) DEBUG
Log::Start [info script] 0.1

#----- Setup fields

set fn  $env(CI_DATA_IN)/2005102612_012
set fld 131073 ;# TT at 1.000 sg (12000) for T+18

fstdfile open FLE read $fn
fstdfield read FLD FLE $fld

#----- Make the mask

vexpr RES ifelse(FLD>20.0, 0, 1)

#----- Calculate normal average

set avg [vexpr NIL savg(FLD)]
Log::Print DEBUG "W/O mask  : avg=$avg"

#----- Test with NaN

vexpr FLD ifelse(RES,FLD,NAN)
Log::Print DEBUG "No data is set to \[[fstdfield stats FLD -nodata]\]"

set nan_avg [vexpr NIL savg(FLD)]
Log::Print DEBUG "With NaN  : avg=$nan_avg"

#----- Test with -999 (ground temperature under -999 celcius is just silly and theoretically clamped at -273)

vexpr FLD ifelse(RES,FLD,-999)
fstdfield stats FLD -nodata -999
Log::Print DEBUG "No data is set to \[[fstdfield stats FLD -nodata]\]"

set na_avg [vexpr NIL savg(FLD)]
Log::Print DEBUG "With -999 : avg=$na_avg"

#----- Test if we achieve the same result

if { $na_avg==$avg } {
    Log::Print WARNING "The mask has not changed the average ==> Test is no good!"
}

if { $nan_avg!=$na_avg } {
    Log::Print ERROR "==> TEST FAILED : NaN -> number doesn't yield the same result as -999 when used as nodata"
} else {
    Log::Print INFO "==> TEST PASSED : NaN"
}

#----- Test with Inf

vexpr FLD ifelse(RES,FLD,INFINITY)

set inf_max [vexpr NIL smax(FLD)]
Log::Print DEBUG "Max with infinity: max=$inf_max"

if { $inf_max!="Inf" } {
    Log::Print ERROR "==> TEST FAILED : Inf -> Infinity is not big enough!"
} else {
    Log::Print INFO "==> TEST PASSED : Inf"
}

#----- Test with Inf

vexpr FLD ifelse(RES,FLD,-INFINITY)

set inf_min [vexpr NIL smin(FLD)]
Log::Print DEBUG "Min with infinity: minx=$inf_min"

if { $inf_min!="-Inf" } {
    Log::Print ERROR "==> TEST FAILED : -Inf -> Negative infinity is not small enough!"
} else {
    Log::Print INFO "==> TEST PASSED : -Inf"
}

#----- Cleanup fields

fstdfield free FLD
fstdfile close FLE

Log::End -1
