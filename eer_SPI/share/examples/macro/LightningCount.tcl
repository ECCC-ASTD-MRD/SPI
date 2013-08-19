namespace eval Macro::LightningCount { } {
   variable Param
   variable Data
   variable Error

   set Data(Secs) 3600
   set Data(Sec1) [clock seconds]
   set Data(Sec0) [expr $Data(Sec1)-$Data(Secs)]
   set Data(Path) /data/ade/rawdata/remote/foudre

   set Error(Something) { "Une erreur quelconque" "Some error" }

   set Param(Info)      { "Compte du nombre frappe de foudre" "Lightning strike counting" }
}

proc Macro::LightningCount::Grid { { Res 80000 } } {
   variable Data

   Macro::Doing "Creating grid"

   #----- Definier les parametre de la grille

   set nhem  1
   set dlat  45
   set dlon -101
   set dd60  1.0

   set xg3 $Res
   set NI [expr int(94*(80000.0/$Res))]
   set NJ [expr int(60*(80000.0/$Res))]

   #---- Calculer les parametres xg necessaires

   set xg4 [expr (270.0-$dlon+360.0)/360.0]
   set xg4 [expr ($xg4-floor($xg4))*360.0]

   set xy  [fstdgrid xyfll $dlat $dlon $dd60 $xg4 $nhem]

   set xg1 [expr ((($NI-1.0)/2.0) * $xg3 - [lindex $xy 0]) / $xg3 + 1.0]
   set xg2 [expr ((($NJ-1.0)/2.0) * $xg3 - [lindex $xy 1]) / $xg3 + 1.0]

   #----- Creer le champs sur une grille PS nord

   fstdfield create MACROLIGHTGRID $NI $NJ 1
   fstdfield define MACROLIGHTGRID -NOMVAR LTG -GRTYP N $xg1 $xg2 $xg3 $xg4 -DATEV [fstdstamp fromseconds $Data(Sec1)]
}

proc Macro::LightningCount::Count { } {
   variable Data

   #----- Process des donnees de foudres

   foreach file [lrange [lsort [glob $Data(Path)/*_]] end-1 end] {

      Macro::Doing "Processing file $file"
      set f [open $file r]
      while { ![eof $f] } {

         gets $f line

         set secs [clock scan "[string range $line 0 7] [string range $line 8 13]" -gmt true]

         if { $secs>=$Data(Sec0) && $secs<=$Data(Sec1)} {
            set lat [lindex $line 1]
            set lon [lindex $line 2]
            set val [lindex $line 3]
            fstdfield stats MACROLIGHTGRID -coordvalue $lat $lon [expr [fstdfield stats MACROLIGHTGRID -coordvalue $lat $lon]+1]
         }
      }
      close $f
   }
}

proc Macro::LightningCount::Execute { } {

   Macro::LightningCount::Grid 40000
   Macro::LightningCount::Count

   SPI::LayoutLoad $Page::Data(Frame) Lightning
#   SPI::IcoOpen /home/afsr/005/CanadaMajorCities.ico

   Lightning::Legend 1 MACROLIGHTGRID

   ProjCam::Set LightningCanada { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } {0.0 1.0 0.0} 2.51402674904 0 0 1 0 0 0 50.0378238644 -96.850928832
   ProjCam::Set LightningQuebec {0.0 0.0 1.0} {0.0 0.0 2.0} {0.0 1.0 0.0} 7.9447399635 0 0 1 0 0 0 51.762844933 -69.1849877379
   ProjCam::Set LightningAtlantique { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } {0.0 1.0 0.0} 10.4107348435 0 0 1 0 0 0 48.8931782044 -62.1823795818
   ProjCam::Set LightningPrairie { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } {0.0 1.0 0.0} 7.62110398435 0 0 1 0 0 0 55.1043098423 -105.253062512
   ProjCam::Set LightningBC { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } {0.0 1.0 0.0} 9.06307108237 0 0 1 0 0 0 54.2611494434 -123.740775765
   ProjCam::Set LightningOntario { 0.0 0.0 1.0 } { 0.0 0.0 2.0 } {0.0 1.0 0.0} 7.361501205 0 0 1 0 0 0 49.3265258703 -85.0018237284
   ProjCam::Set LightningYukon {0.0 0.0 1.0} {0.0 0.0 2.0} {0.0 1.0 0.0} 6.91629785046 0 0 1 0 0 0 66.1117533667 -121.270914247

   ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) LightningCanada

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) MACROLIGHTGRID

   #----- If in batch mode, print the map(s) and exit

   if { $SPI::Param(Batch) } {
      foreach cam { LightningCanada LightningQuebec LightningAtlantique LightningPrairie LightningBC LightningOntario LightningYukon } {
         ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) $cam
         PrintBox::Image $Page::Data(Frame) png $cam
      }
      SPI::Quit
   }
   Macro::Doing ""
}

proc Macro::LightningCount::Clean { } {
   variable Data

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP) MACROLIGHTGRID
   fstdfield free MACROLIGHTGRID
}









