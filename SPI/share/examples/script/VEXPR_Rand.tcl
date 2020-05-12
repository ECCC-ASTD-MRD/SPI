#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"

package require TclData
package require Logger

#set Log::Param(Level) DEBUG
Log::Start [info script] 0.1

fstdfield create FLD 1 1 1 Float64

set r [vexpr - frand(FLD,1,100)]
set v 84.178584
if { abs($r-$v) <= 1e-6 } {
    Log::Print INFO "==> TEST PASSED : frand without seed initialization"
} else {
    Log::Print ERROR "==> TEST FAILED : frand without seed initisalization : expected $v but got $r"
}

vexpr - irand(12345)
set r [vexpr - frand(FLD,1,100)]
set v 18.661135
if { abs($r-$v) <= 1e-6 } {
    Log::Print INFO "==> TEST PASSED : frand with seed initialization"
} else {
    Log::Print ERROR "==> TEST FAILED : frand with seed initisalization : expected $v but got $r"
}

vexpr - irand(1)
vexpr - irand()
set r [vexpr - frand(FLD,1,100)]
set v 84.178584
if { abs($r-$v) > 1e-6 } {
    Log::Print INFO "==> TEST PASSED : frand with random seed initialization"
} else {
    Log::Print ERROR "==> TEST FAILED : frand with random seed initisalization : expected $v but got $r"
}

Log::End -1
