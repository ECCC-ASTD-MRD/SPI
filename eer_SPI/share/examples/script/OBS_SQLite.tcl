#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"

proc test { a b } {
    if { $a != $b } {
           puts "a=$a"
           puts "b=$b"
           puts "TEST FAILED"
           exit
       }
}
package require TclData

metobs table -readcmc
# This was found by having the C function MetObs_CMd print it's arguments
# I noticed that these calls were made when SPI started up.
# metobs table -readmaster B /home/binops/afsi/sio/env_ubuntu-14.04-amd64-64/afsisio/datafiles/constants/table_b_bufr_f
# metobs table -readmaster D /home/binops/afsi/sio/env_ubuntu-14.04-amd64-64/afsisio/datafiles/constants/table_d_bufr_f

set pos "Position 1"
metobs create NEWOBS
metobs define NEWOBS -ID $pos
metobs define NEWOBS -COORD $pos 35.95 -95.66 100
metobs define NEWOBS -ELEMENT $pos 24005 0 { 10 40 50 }
metobs define NEWOBS -ELEMENT $pos 24005 1 20
metobs define NEWOBS -ELEMENT $pos 5022 3 -95

set out_wv [metobs define NEWOBS -ELEMENT "Position 1" 24005]
set out_5022 [metobs define NEWOBS -ELEMENT "Position 1" 5022 ]
set expected_out_wv "{3 {}} {1 20.0} {0 {10.0 40.0 50.0}}"
set expected_out_5022 "{3 -95.0} {1 {}} {0 {}}"
test $out_wv $expected_out_wv
test $out_5022 $expected_out_5022
puts "Wind values :$out_wv"
puts "5022 value  :$out_5022"
puts "===== All tests passed for hand crafted TMetObs object ====="

set sql_obs SQLITEDATA
set sql_file /fs/cetus/fs2/ops/cmoe/afsr005/Data/SQLite/acars.sqlite
metobs create $sql_obs
metobs read $sql_obs $sql_file

set av_info [metobs define $sql_obs -INFO]
set nb_stations [metobs define $sql_obs -NB]
set av_dates [metobs define $sql_obs -DATE]
set av_elems [metobs define $sql_obs -ELEMENT]
set first_date [lindex [metobs define $sql_obs -DATE] 0]
set stations_for_date [llength [metobs define $sql_obs -STATION "" $first_date]]
set reports_for_date [llength [metobs define $sql_obs -REPORT "" $first_date]]

puts  "   Available info   : $av_info"
puts  "   Nb Stations      : $nb_stations"
puts  "   Available Dates  : $av_dates"
puts  "   Available Elems  : $av_elems"
puts  "   Stations for date: $stations_for_date"
puts  "   Reports for date : $reports_for_date"

#----- Parse the metobs data by elements
puts  "\n   Per stations:\n"
set idx 0
foreach id [metobs define $sql_obs -ID] {

    set coord [metobs define $sql_obs -COORD $id]
    set number [metobs define $sql_obs -NO $id]
    set dates [metobs define $sql_obs -DATE $id]
    set elems [metobs define $sql_obs -ELEMENT $id]

    puts  "   Station ($idx) $id\t: $number $coord"
    foreach date $dates  {
        puts  "      [clock format $date] :"
        foreach elem $elems {
            puts  "         $elem : [metobs define $sql_obs -ELEMENT $id $elem $date]"
        }
    }
}

#----- Parse the metobs data by reports
puts  "\n   Per report:\n"
set idx 0
foreach id [metobs define $sql_obs -ID] {

    set coord [metobs define $sql_obs -COORD $id]
    set number [metobs define $sql_obs -NO $id]
    set dates [metobs define $sql_obs -DATE $id]

    puts  "   Station ($idx) $id\t: $number $coord"
    foreach date $dates  {
        puts  "      [clock format $date] :"
        foreach report [metobs define $sql_obs -REPORT $id $date] {
            set codetype [metreport define $report -CODETYPE]
            set family [metreport define $report -FAMILY]
            set type [metreport define $report -TYPE]
            set stype [metreport define $report -STYPE]
            puts  "\n         CODE($codetype)  BFAM($family) TYP($type) STYP(stype)"

            foreach elem [metreport define $report -ELEMENT] {
                puts  "         $elem : [metreport define $report -ELEMENT $elem]"
            }
        }
    }
}
