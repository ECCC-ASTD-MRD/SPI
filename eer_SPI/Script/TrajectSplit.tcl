#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Package de post-traitement pour le modele Trajectoire
# Fichier  : TrajectSplit.tcl
# Creation : Avril 2002
#
# Description:
#    Separer une execution en plusieurs trajectoires.
#===============================================================================

proc WriteParcel { FI FO P } {
   variable Data

   if { "[lindex $P 3]"!="$Data(Name)" } {

      set Data(Name) [lindex $P 3]
      puts $FO " '\n $Data(Name)\n '"
      puts $FO $Data(Head0)
      puts $FO $Data(Head1)
      puts $FO "[format "%13i" $Data(Nb$Data(Name))]          Nombre de trajectoires"
      puts $FO $Data(Head3)
      puts $FO "[format "%13i" $Data(Nb)]"
      puts $FO $Data(Head5)
      puts $FO "   $Data(Date)"
   }

   for { set i 0 } { $i <$Data(Nb) } { incr i } {
      puts $FO [gets $FI]
   }
}

proc GetDate { } {

   set data [exec cat data_std_sim]

   set anal 0
   set prog 1e32

   foreach file $data {

      set lst [split [lindex $file 0] /]

      if { [lsearch -exact $lst trial] != -1 } {
         set anal 1
      } elseif { [lsearch -exact $lst prog] != -1 } {
         set d   [lindex $lst end]
         set sec [clock scan "[string range $d 4 5]/[string range $d 6 7]/[string range $d 0 3] [string range $d 8 9]:00" -gmt true]
         set h   [string trimleft [string range $d 12 end] 0]

         if { $h!="" } {
            set sec [expr $sec+$h*3600]
         }

         set date [clock format $sec -format "%Y%m%d%H" -gmt true]
         if { $date < $prog } {
            set prog $date
         }
      }
   }

   if { $prog!=1e32 && $anal } {
      return $prog
   } else {
      return 0
   }
}

set Dir  [lindex $argv 0]
set File [lindex $argv 1]

cd $Dir

#----- Recuperer les positions de departs

set en [open $Dir/entre r]

for { set i 0 } { $i < 6 } { incr i } {
   gets $en nb
}

set pos ""

for { set i 0 } { $i < [lindex $nb 0] } { incr i } {
   gets $en line

   if { [info exists Data(Nb[lindex $line 3])] } {
      incr Data(Nb[lindex $line 3])
   } else {
      set Data(Nb[lindex $line 3]) 1
   }
   lappend pos $line
}

close $en

#----- Produire le split

file rename -force $Dir/$File $Dir/$File.ori
set fi [open $Dir/$File.ori r]
set fo [open $Dir/$File w]

#----- Initialisations

set Data(Name) ""
set Data(Date) [GetDate]

#----- Recuperer l'entete originale

gets $fi
gets $fi
gets $fi

gets $fi Data(Head0)
gets $fi Data(Head1)
gets $fi Data(Head2)
gets $fi Data(Head3)
gets $fi Data(Nb)
gets $fi Data(Head5)

#----- Produire le fichier split

foreach p $pos {
   WriteParcel $fi $fo $p
}

close $fi
close $fo
