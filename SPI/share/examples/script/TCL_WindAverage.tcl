#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"

#----- Open file
set f [open [lindex $argv 0] r]

#----- Initialize counters
set x 0
set y 0
set v 0
set n 0

#----- Loop on lines
while { ![eof $f] } {

   gets $f line

   #----- Get wind info
   set dir ""
   set wind [lindex $line 3]
   
   scan $wind %03d%02d%2s dir spd unit

   #----- Value checking
   if { [string is integer -strict $dir] } {

      #----- Add to counters
      set x [expr $x+cos($dir*0.01745329)] 
      set y [expr $y+sin($dir*0.01745329)]
      set v [expr $v+$spd]
      incr n
   }
}

close $f

#----- good averaging
set d [expr round(atan2($y/$n,$x/$n)*57.29577)]
set v [expr round($v/$n)]

if ($d<0) {
   set d [expr $d+360]
}

puts "Average is: $v@$d"
