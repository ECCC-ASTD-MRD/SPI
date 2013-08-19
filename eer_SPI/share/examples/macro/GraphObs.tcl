#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Serie temporelle d'observation d'O3 au stations
# Fichier  : GraphObs.tcl
# Creation : Fevrier 2008 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Creer des series temporelles d'observation d'O3 au stations.
#
# Arguments :
#
# Remarques :
#===============================================================================

namespace eval Macro::GraphObs {
   variable Params

   set Param(Info)      { "Serie temporelle d'observation d'O3" "Observed O3 time series at stations" }
   set Param(InfoArgs)  { { "Fichier" } { "File" } }

   set Param(Obs) /home/afsr/005/public_html/SPI/Script/DataIn/O3.20050302.obs
}

proc Macro::GraphObs::Init { } {

   SPI::PageNew True "" 800x600

   $Page::Data(Canvas) create graph -x 5 -y 5 -width 790 -height 590 -anchor nw -xlegend 5 -ylegend 5 -fg black -bg white -fill white -tags GRAPH -font XFont12 \
      -title "O3 Time series" -command toto

   #----- Creation des unite de l'echelle

   graphaxis create axisx
   graphaxis create axisy

   graphaxis configure axisx -font XFont12 -color black -gridcolor "" -position LL -width 1 -angle 45 -lowoffset 10
   graphaxis configure axisy -font XFont12 -color black -gridcolor "" -position LL -width 1 

   vector create DATA
   vector dim    DATA { X Y }

   graphitem create ITEM
   graphitem configure ITEM -xaxis axisx -yaxis axisy -xdata DATA.X -ydata DATA.Y -orient X -desc "" \
      -outline red -fill "" -transparency 100 -width 2 -size 2 -value False \
      -type LINE -font XFont12 -icon SQUARE -iconoutline red -iconfill black -bitmap "" -stipple "" -image ""
}


proc Macro::GraphObs::Execute { } {
   variable Param

   #----- Initialize page and graph
   Macro::GraphObs::Init

   #----- Load observations
   set obs [observation load $Param(Obs)]

   #----- Loop on stations
   for { set n 0 } { $n<[observation define [lindex $obs 0] -NB] } { incr n } {

      vector set DATA.X {}
      vector set DATA.Y {}
      set dates {}
      set id [observation define [lindex $obs 0] -ID $n]

      #----- loop on data
      foreach ob $obs {

         set sec [observation define $ob -DATE]
         set val [observation define $ob -DATA $n]

         #----- Missing value are set to 0
         if { $val=="-" } { set val 0 }

         #----- Build data vectors
         vector append DATA [list $sec $val]
         lappend dates [clock format $sec -format "%d/%m %H:%M" -gmt True]
      }

      #----- Update graph with data
      graphaxis configure axisx -type LINEAR -min [vector stats DATA.X -min] -max [vector stats DATA.X -max] -intervals [lsort -increasing [vector get DATA.X]] -labels $dates \
         -unit "Temps\n ttt"
      graphaxis configure axisy -type LINEAR -min 0 -max [vector stats DATA.Y -max] -unit Pbb -increment 10 -highoffset 10
      graphitem configure ITEM -desc $id

      puts stderr "$sec,$val = [toto -project $sec $val ITEM]"
      $Page::Data(Canvas) itemconf GRAPH -item ITEM
      update idletasks

      regsub -all "\[^a-zA-Z0-9\]" $id "" file
      PrintBox::Image $Page::Data(Frame) png $file
   }
}
