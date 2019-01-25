#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Serie temporelle de pseudo-concentrations
# Fichier  : AutoGraphTime.tcl
# Creation : Fevrier 2008 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Creer des series temporelles de pseudo-concentrations pours plusieurs sources.
#
# Arguments :
#
# Remarques :
#===============================================================================

namespace eval Macro::AutoGraphTime {
   variable Params

   set Param(Info)      { "Serie temporelle" "Time series" }
   set Param(InfoArgs)  { { "Fichier" "[Variable]" "[Lat]" "[Lon]" } { "File" "[Variable]" "[Lat]" "[Lon]" } }

   set Param(Paths)     ""
   set Param(Var)       "SEUD"
   set Param(Lat)       43.88
   set Param(Lon)       -78.70
   
   set Param(Colors)  { red red orange orange orange orange orange orange orange blue blue blue blue gray gray gray gray gray green green green green green green green gold gold pink pink pink pink }
   set Param(Icons)   { CIRCLE SQUARE CIRCLE SQUARE TRIANGLE VBAR HBAR LOZENGE + CIRCLE SQUARE TRIANGLE VBAR CIRCLE SQUARE TRIANGLE VBAR LOZENGE CIRCLE SQUARE TRIANGLE VBAR HBAR LOZENGE + CIRCLE SQUARE CIRCLE SQUARE TRIANGLE VBAR }
}

proc Macro::AutoGraphTime::Init { } {

   #----- Creer la page plutot que dans un layout
   SPI::PageNew True "" 1233x605

   #----- Creation du graph, des unite de l'echelle
   $Page::Data(Canvas) create graph -x 1 -y 1 -width 1000 -height 598 -anchor nw -xlegend 895 -ylegend -36 -fg black -fill #F0F0F0 -bg white -tags GRAPH -font XFont12 \

   graphaxis create AXISX
   graphaxis create AXISY

   graphaxis configure AXISX -font XFont12 -color black -gridcolor white -position LL -width 1 -type LINEAR -angle 45 -unit "Dates"
   graphaxis configure AXISY -font XFont12 -color black -gridcolor white -gridwidth 1 -position LL -width 1 -type LINEAR -highoffset 10 -unit "Modelled SRS \[m-3\]"
}

proc Macro::AutoGraphTime::Execute { } {
   variable Param
   variable Info
   
   #----- Initialize page and graph
   Macro::AutoGraphTime::Init

   #----- Loop on simulations
   foreach path $Param(Paths) {
   
      set dates {}
      set items {}
      set max   0
       
      #----- Loop on resul files
      foreach file [glob $path/results/*_???] {
      
         fstdfile open FSTDFILE read $file

         #----- Recuperer la description de l'experience
         Info::Decode ::Macro::AutoGraphTime::Info [Info::Read FSTDFILE]
         set date [clock format $Info(AccSecs) -format "%Y%m%d %H:%M" -gmt true]
        
         #----- Loop on stations and their display parameters
         foreach id $Info(Name) color $Param(Colors) icon $Param(Icons) {

            Macro::Doing "Processing station $id"
   
            #----- Creer le vecteur 2D si non-existant
            if { ![vector is DATA$id] } {
               vector create DATA$id
               vector dim    DATA$id { X Y }
            }
      
            #----- Extraire les donnees
            foreach fld [fstdfield find FSTDFILE -1 [string range $id 0 12] -1 -1 -1 "" $Param(Var)] {

               fstdfield read FLD FSTDFILE $fld
               set sec  [fstdstamp toseconds [fstdfield define FLD -DATEV]]
               set val  [fstdfield stats FLD -coordvalue $Param(Lat) $Param(Lon)]
         
               #----- Build data vectors
               vector append DATA$id [list $sec $val]
               if { "$id"=="[lindex $Info(Name) 0]" } {
                  lappend dates [clock format $sec -format "%d/%m %H:%M" -gmt True]
               }
            }

            #----- Creer l'item de graph pour cette localisation
            if { ![graphitem is ITEM$id] } {
               graphitem create ITEM$id
               graphitem configure ITEM$id -desc $id -xaxis AXISX -yaxis AXISY -xdata DATA$id.X -ydata DATA$id.Y -orient X \
                  -outline $color -fill "" -transparency 100 -width 1 -size 3 -value False \
                  -type LINE -font XFont12 -icon $icon -iconoutline $color -iconfill $color -bitmap "" -stipple "" -image ""            
               lappend items ITEM$id
            }
            
            #----- Recupere les limites pour l'axe Y
            set max [expr [vector stats DATA$id.Y -max]>$max?[vector stats DATA$id.Y -max]:$max]
         }
         
         fstdfile close FSTDFILE
      }
      
      #----- Update des axes, limites, libelle, ...
      set rng [expr $Info(Duration)*60/$Info(OutputTimeStepMin)-1]
      set t0  [lindex [vector get DATA$id.X] 0]
      graphaxis configure AXISX -max $t0 -min [expr $t0-$Info(Duration)*3600+1200] -intervals [lreverse [lrange [vector get DATA$id.X] 0 $rng]] -labels [lreverse [lrange $dates 0 $rng]]
      graphaxis configure AXISY -min 0 -max $max

      #----- Update du graph avec tles donnees et les titres
      $Page::Data(Canvas) itemconf GRAPH -item $items -title "Modelled SRS Time Series at Darlington NPP for Different FPS Stations\nfor a detection occuring on $date GMT" 
      update idletasks

      #----- En mode batch
      if { $SPI::Param(Batch) } {
         set file $path/products/[lindex [split [file tail $file] _] 0]
         
         #----- Save image
         PrintBox::Image $Page::Data(Frame) png $file
      
         #----- Save CSV data
         set f [open $file.csv w]
         puts $f "Date,[join $Info(Name) ,]"
         set d 0
         foreach date $dates {
            puts -nonewline $f "$date"
            foreach id $Info(Name) {
               puts -nonewline $f ",[vector get DATA$id.Y $d]"
            }
            puts -nonewline $f "\n"
            if { [incr d]>$rng } {
               break
            }
         }
         close $f
      }
   }
   
   if { $SPI::Param(Batch) } {
      SPI::Quit
   }
}

proc Macro::AutoGraphTime::Clean { } {
   variable Data

}

proc Macro::AutoGraphTime::Args { } {
   global argv argc
   variable Param

   set Param(Paths) [lindex $argv 0]
   
   #----- Parametres optionels
   if { $argc>1 } { set Param(Var)   [lindex $argv 1] }
   if { $argc>2 } { set Param(Lat)   [lindex $argv 2] }
   if { $argc>3 } { set Param(Lon)   [lindex $argv 3] }
}