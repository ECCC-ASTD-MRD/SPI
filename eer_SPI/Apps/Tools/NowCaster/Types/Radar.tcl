#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Outils de "NowCasting" pour SPI.
# Fichier  : Radar.tcl
# Creation : Avril 2006 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fonctions de manipulations des donnees radar pour l'outils
#    de NowCasting de SPI.
#
# Remarques :
#
#===============================================================================

namespace eval NowCaster::Radar { } {
   variable Param
   variable Data
   variable Msg
   variable Lbl
   variable Bubble

   set Param(Title)      { "Radar" "Radar" }
   set Param(Path)       /data/radar/rawdata

   set Data(Radars)      {}

   set Data(No)          -1
   set Data(Site)        ""
   set Data(Sites)       {}
   set Data(RangeActive) False
   set Data(Range)       {}
   set Data(Ranges)      { { 25 50 100 150 200 250 }
                           { 100 200 300 }
                           { 50 100 150 200 250 }
                           { 10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 190 200 210 220 230 240 250 } }

   set Data(Type)    ""
   set Data(Scan)    ""
   set Data(Sweep)   0
   set Data(Id)      ""
   set Data(Name)    ""
   set Data(Loc)     ""
   set Data(Lat)     ""
   set Data(Lon)     ""
   set Data(Ele)     ""
   set Data(Sec)     $NowCaster::Data(Sec)
   set Data(Secs)    {}
   set Data(Product) ""
   set Data(Noise)   ""
   set Data(Filter)  ""
   set Data(ZCal)    ""
   set Data(Nyquist) ""

   set Lbl(Type)    { "Type" "Type" }
   set Lbl(Scan)    { "Scan" "Scan" }
   set Lbl(Sweep)   { "Balayage" "Sweep" }
   set Lbl(Id)      { "Id" "Id" }
   set Lbl(Name)    { "Nom" "Name" }
   set Lbl(Loc)     { "Loc" "Loc" }
   set Lbl(Date)    { "Date" "Date" }
   set Lbl(Product) { "Produit" "Product" }
   set Lbl(Noise)   { "Bruit" "Noise" }
   set Lbl(Filter)  { "Filtre" "Filter" }
   set Lbl(ZCal)    { "ZCal" "ZCal" }
   set Lbl(Nyquist) { "Nyquist" "Nyquist" }

   set Msg(Read)    { "Lecture des données radar" "Reading radar data" }
   set Msg(Loc)     { "Impossible de trouver la localisation du radar" "Unable to find radar location" }
   set Msg(FileBad) { "Impossible d'ouvrir ce fichier de données radar" "Unable to open this radar file" }

   set Bubble(Mode)   { "Sélection interactive des radars" "Interactive selection of radars" }
   set Bubble(Params) { "Paramêtres d'affichage" "Display parameters" }
   set Bubble(Ranges) { "Cercle des distances" "Distance ranges" }
   set Bubble(Areas)  { "Affichage de la couverture des radars" "Toggle radar footprint display" }
   set Bubble(Add)    { "Ajouter un radar" "Add a radar" }
   set Bubble(Del)    { "Supprimer le radar courant" "Delete current radar" }
}

#----------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Window>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche l'interface de manipulation des donnees radar.
#
# Parametres :
#  <Frame>   : Frame de l'interface
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc NowCaster::Radar::Window { Frame } {
   global GDefs
   variable Data
   variable Param
   variable Lbl
   variable Bubble

   set Data(Frame) $Frame

   frame $Frame.head
      checkbutton $Frame.head.mode -variable Page::Data(ToolMode) -onvalue NowCaster::Radar -offvalue SPI \
         -image ARROW -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) \
         -command { SPI::ToolMode $Page::Data(ToolMode) Data True }
      button $Frame.head.params -image PARAMS -bd 0 -relief flat -overrelief raised -command { SPI::Params ;  TabFrame::Select .params.tab 1 }
      checkbutton $Frame.head.areas -image CIRCLE -relief sunken -bd 1 -overrelief raised -offrelief flat -anchor w  -selectcolor $GDefs(ColorLight)\
            -command { NowCaster::Radar::Areas } -variable Areas::Data(AllRADAR) -onvalue True -offvalue False -indicatoron False
      checkbutton $Frame.head.ranges -image RANGE -relief sunken -bd 1 -overrelief raised -offrelief flat -width 30 -anchor w  -selectcolor $GDefs(ColorLight)\
            -command { NowCaster::Radar::Range } -variable NowCaster::Radar::Data(RangeActive) -onvalue True -offvalue False -indicatoron False
      menubutton $Frame.head.ranges.down -image OPTIONS -relief flat -bd 0 -menu $Frame.head.ranges.down.menu
      place $Frame.head.ranges.down -relx 1.0 -rely 0.0 -anchor ne -relheight 1.0
      menubutton $Frame.head.add -image PLUS -bd 0 -relief flat -menu $Frame.head.add.list
      button $Frame.head.del -image DELETE -bd 1 -relief flat -overrelief raised \
         -command "catch { NowCaster::Radar::Delete \[$Frame.select.list get \[$Frame.select.list curselection\]\] }"
      pack $Frame.head.mode $Frame.head.areas $Frame.head.params -side left
      pack $Frame.head.ranges -padx 3 -side left
      pack $Frame.head.del $Frame.head.add -side right -padx 2
   pack $Frame.head -side top -fill x

   menu $Frame.head.ranges.down.menu -bd 1 -tearoff 0
   foreach range $Data(Ranges) {
      $Frame.head.ranges.down.menu add radiobutton -label $range -variable NowCaster::Radar::Data(Range) -value $range \
         -command { set NowCaster::Radar::Data(RangeActive) True; NowCaster::Radar::Range }
   }

   catch {  set Data(Radars) [lsort -increasing [concat [glob -nocomplain $Param(Path)/X*] [glob -nocomplain $Param(Path)/W*]]] }
   menu $Frame.head.add.list -bd 1 -tearoff 0
   set i 0
   foreach radar $Data(Radars) {
      if { [incr i] > 20 } {
         set i 0
         $Frame.head.add.list add command -label [file tail $radar] -columnbreak 1 -command "NowCaster::Radar::Add $radar"
      } else {
         $Frame.head.add.list add command -label [file tail $radar] -columnbreak 0 -command "NowCaster::Radar::Add $radar"
      }
   }
   $Frame.head.add.list add separator
   $Frame.head.add.list add command -label ... -command "NowCaster::Radar::Add \[FileBox::Create . \"\" Load \"\"\]"

   frame $Frame.select
      listbox $Frame.select.list -relief sunken -bd 1 -yscrollcommand "$Frame.select.scroll set" -selectmode single \
         -width 1 -height 1 -background white -selectbackground $GDefs(ColorHighLight) -selectforeground black \
         -listvariable NowCaster::Radar::Data(Sites)
      scrollbar $Frame.select.scroll -relief sunken -command "$Frame.select.list yview" -bd 1 -width 10
      pack $Frame.select.list -side left -fill both -expand true
      pack $Frame.select.scroll -side left -fill y
   pack $Frame.select -side top -fill both -expand true

   frame $Frame.param
      frame $Frame.param.scan
         label $Frame.param.scan.lbl -text [lindex $Lbl(Scan) $GDefs(Lang)] -width 8 -anchor w
         ComboBox::Create $Frame.param.scan.sel NowCaster::Radar::Data(Scan) noedit sorted nodouble -1 { } 12 6 { NowCaster::Radar::Read $NowCaster::Radar::Data(Site) $NowCaster::Radar::Data(Sec) $NowCaster::Radar::Data(Scan)}
         pack $Frame.param.scan.lbl -side left
         pack $Frame.param.scan.sel -side left -fill x -expand True
      pack $Frame.param.scan -side top -fill x

      frame $Frame.param.sweep
         label $Frame.param.sweep.lbl -text [lindex $Lbl(Sweep) $GDefs(Lang)] -width 8 -anchor w
         spinbox $Frame.param.sweep.sel -textvariable NowCaster::Radar::Data(Sweep) -width 3 -from 0 -to 0 -wrap 1 -bd 1 -command { NowCaster::Radar::Sweep $NowCaster::Radar::Data(Site) $NowCaster::Radar::Data(Sweep)} -bg $GDefs(ColorLight)
         entry $Frame.param.sweep.angle -textvariable NowCaster::Radar::Data(Angle) -bd 1 -width 9 -bg $GDefs(ColorLight)
         pack $Frame.param.sweep.lbl $Frame.param.sweep.sel $Frame.param.sweep.angle -side left
      pack $Frame.param.sweep -side top -fill x

      frame $Frame.param.type
         label $Frame.param.type.lbl -text [lindex $Lbl(Type) $GDefs(Lang)] -width 8 -anchor w
         entry $Frame.param.type.sel -textvariable NowCaster::Radar::Data(Type) -state disabled -bd 1
         pack $Frame.param.type.lbl $Frame.param.type.sel -side left
      pack $Frame.param.type -side top -fill x

      frame $Frame.param.id
         label $Frame.param.id.lbl -text [lindex $Lbl(Id) $GDefs(Lang)] -width 8 -anchor w
         entry $Frame.param.id.sel -textvariable NowCaster::Radar::Data(Id) -state disabled -bd 1
         pack $Frame.param.id.lbl $Frame.param.id.sel -side left
      pack $Frame.param.id -side top -fill x

      frame $Frame.param.name
         label $Frame.param.name.lbl -text [lindex $Lbl(Name) $GDefs(Lang)] -width 8 -anchor w
         entry $Frame.param.name.sel -textvariable NowCaster::Radar::Data(Name) -state disabled -bd 1
         pack $Frame.param.name.lbl $Frame.param.name.sel -side left
      pack $Frame.param.name -side top -fill x

      frame $Frame.param.loc
         label $Frame.param.loc.lbl -text [lindex $Lbl(Loc) $GDefs(Lang)] -width 8 -anchor w
         entry $Frame.param.loc.lat -textvariable NowCaster::Radar::Data(Lat) -bd 1 -width 7 -bg $GDefs(ColorLight)
         entry $Frame.param.loc.lon -textvariable NowCaster::Radar::Data(Lon) -bd 1 -width 7 -bg $GDefs(ColorLight)
         entry $Frame.param.loc.ele -textvariable NowCaster::Radar::Data(Ele) -bd 1 -width 5 -bg $GDefs(ColorLight)
         pack $Frame.param.loc.lbl $Frame.param.loc.lat $Frame.param.loc.lon $Frame.param.loc.ele -side left
      pack $Frame.param.loc -side top -fill x

      frame $Frame.param.date
         label $Frame.param.date.lbl -text [lindex $Lbl(Date) $GDefs(Lang)] -width 8 -anchor w
         entry $Frame.param.date.sel -textvariable NowCaster::Radar::Data(Date) -state disabled -bd 1
         pack $Frame.param.date.lbl $Frame.param.date.sel -side left
      pack $Frame.param.date -side top -fill x

      frame $Frame.param.prod
         label $Frame.param.prod.lbl -text [lindex $Lbl(Product) $GDefs(Lang)] -width 8 -anchor w
         entry $Frame.param.prod.sel -textvariable NowCaster::Radar::Data(Product) -state disabled -bd 1
         pack $Frame.param.prod.lbl $Frame.param.prod.sel -side left
      pack $Frame.param.prod -side top -fill x

      frame $Frame.param.noise
         label $Frame.param.noise.lbl -text [lindex $Lbl(Noise) $GDefs(Lang)] -width 8 -anchor w
         entry $Frame.param.noise.sel -textvariable NowCaster::Radar::Data(Noise) -state disabled -bd 1
         pack $Frame.param.noise.lbl $Frame.param.noise.sel -side left
      pack $Frame.param.noise -side top -fill x

      frame $Frame.param.filter
         label $Frame.param.filter.lbl -text [lindex $Lbl(Filter) $GDefs(Lang)] -width 8 -anchor w
         entry $Frame.param.filter.sel -textvariable NowCaster::Radar::Data(Filter) -state disabled -bd 1
         pack $Frame.param.filter.lbl $Frame.param.filter.sel -side left
      pack $Frame.param.filter -side top -fill x

      frame $Frame.param.zcal
         label $Frame.param.zcal.lbl -text [lindex $Lbl(ZCal) $GDefs(Lang)] -width 8 -anchor w
         entry $Frame.param.zcal.sel -textvariable NowCaster::Radar::Data(ZCal) -state disabled -bd 1
         pack $Frame.param.zcal.lbl $Frame.param.zcal.sel -side left
      pack $Frame.param.zcal -side top -fill x

      frame $Frame.param.nyquist
         label $Frame.param.nyquist.lbl -text [lindex $Lbl(Nyquist) $GDefs(Lang)] -width 8 -anchor w
         entry $Frame.param.nyquist.sel -textvariable NowCaster::Radar::Data(Nyquist) -state disabled -bd 1
         pack $Frame.param.nyquist.lbl $Frame.param.nyquist.sel -side left
      pack $Frame.param.nyquist -side top -fill x
   pack $Frame.param -side top -fill both -expand true -pady 2 -padx 2

   bind $Frame.param.loc.lat <Return> { NowCaster::Radar::Locate $NowCaster::Radar::Data(Site) $NowCaster::Radar::Data(Lat) $NowCaster::Radar::Data(Lon) $NowCaster::Radar::Data(Ele) }
   bind $Frame.param.loc.lon <Return> { NowCaster::Radar::Locate $NowCaster::Radar::Data(Site) $NowCaster::Radar::Data(Lat) $NowCaster::Radar::Data(Lon) $NowCaster::Radar::Data(Ele) }
   bind $Frame.param.loc.ele <Return> { NowCaster::Radar::Locate $NowCaster::Radar::Data(Site) $NowCaster::Radar::Data(Lat) $NowCaster::Radar::Data(Lon) $NowCaster::Radar::Data(Ele) }
   bind $Frame.select.list <ButtonRelease-1>  { NowCaster::Radar::ButtonSelect $NowCaster::Radar::Data(Frame).select.list }

  Bubble::Create $Frame.head.params [lindex $Bubble(Params) $GDefs(Lang)]
  Bubble::Create $Frame.head.ranges [lindex $Bubble(Ranges) $GDefs(Lang)]
  Bubble::Create $Frame.head.areas  [lindex $Bubble(Areas) $GDefs(Lang)]
  Bubble::Create $Frame.head.mode   [lindex $Bubble(Mode) $GDefs(Lang)]
  Bubble::Create $Frame.head.add    [lindex $Bubble(Add) $GDefs(Lang)]
  Bubble::Create $Frame.head.del    [lindex $Bubble(Del) $GDefs(Lang)]
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Cast>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Mise a jour des donnees pour la date specifiee.
#
# Parametres :
#   <Sec>    : Date en secondes (0 -> utilise la date globale)
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::Cast { Sec } {
   variable Data

   NowCaster::Radar::Now $Sec
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Now>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialisation de l'evenement de rafraichissement des donnees.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::Available { Site } {
   variable Data

   set secs {}
   foreach file [lsort -dictionary [glob -nocomplain $Data(Path$Site)/*]] {
      set date [lindex [split [file tail $file] :] 0]
      lappend secs [clock scan "[string range $date 0 7] [string range $date 8 11]" -gmt True]
   }

   set secs [lsort -decreasing -unique -integer $secs]
   if { ![info exists ::NowCaster::Radar::Data(Secs$Site)] || $secs!=$Data(Secs$Site) } {
      set Data(Secs$Site) $secs
      set Data(Secs) [lsort -decreasing -unique -integer [concat $Data(Secs) $Data(Secs$Site)]]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Nearest>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Recherche la date (en secondes) la plu proche de la date specifie.
#
# Parametres :
#   <Sec>    : Date en secondes
#   <Site>   : Site radar
#
# Retour    :
#   <sec>   : Date trouve en secondes (0 si aucune)
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::Nearest { Sec { Site "" } } {
   variable Data

   set sec 0
   foreach sec $Data(Secs$Site) {
      if { $Sec>=$sec } {
         break
      }
   }
   return $sec
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Now>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialisation de l'evenement de rafraichissement des donnees.
#
# Parametres :
#   <Sec>    : Date en secondes (0 -> utilise la date globale)
#   <Check>  :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::Now { Sec { Check False } } {
   global GDefs
   variable Msg
   variable Data

   set pages {}

   foreach site $Data(Sites) {

      #----- Get available date-times
      if  { $Check || ![info exists ::NowCaster::Radar::Data(Secs$site)] } {
         NowCaster::Radar::Available $site
      }
      set Data(Sec) [NowCaster::Radar::Nearest $Sec $site]

      set old [lsearch -inline -glob $Viewport::Data(Data$Viewport::Data(VP)) SCAN$site*]
      if { [llength $old] } {
         Viewport::AssignedTo [lindex $old 0] page vp
         Viewport::UnAssign $page $vp $old -1
      } else {
         set page $Page::Data(Frame)
         set vp   $Viewport::Data(VP)
      }
      lappend pages $page

      #------ If available and not already loaded
      if { ![radarscan is SCAN$site$Data(Sec)] } {

         set date [clock format $Data(Sec) -format "%Y%m%d%H%M" -gmt True]
         set file [glob -nocomplain $Data(Path$site)/$date~~CONVOL:URP:$site:RADAR:*]

         if { [file exist $file] } {
            if { [catch { set Data(Scans$site) [radarfile open RADAR${site}$Data(Sec) read $file]} ] } {
               Dialog::CreateError .nowcaster $Msg(FileBad) "\n\n$file"
               return
            }
            NowCaster::Radar::Read $site $Data(Sec)
         }
      }

      if { [radarscan is SCAN$site$Data(Sec)] } {
         Viewport::Assign $page $vp SCAN$site$Data(Sec) -1
      }
   }

   #----- Update the modified pages
   foreach page [lsort -unique $pages] {
      Viewport::UpdateData $page
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::PageUpdate>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour l'affichage de la Page et du Viewport dans lesquel
#            le radar est assignee.
#
# Parametres :
#   <Scan>   : Observation
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::PageUpdate { Scan } {

   if  { [radarscan is $Scan] } {
      Viewport::AssignedTo $Scan page

      Page::Update        $page
      Page::UpdateCommand $page
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Update>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les parametres d'affichage.
#
# Parametres :
#   <Obs>    : Observation
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::Update { { Site {} } } {
   variable Data

}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Add>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter un site radar a la liste courante.
#
# Parametres :
#   <Path>   : Path complet du fichier a lire
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::Add { Path } {
   global GDefs
   variable Msg
   variable Data

   if { $Path=="" } {
      return
   }

   #----- Read in the data

   if { [file isdirectory $Path] } {
      set site [file tail $Path]
      if { [lsearch -exact $Data(Sites) $site]==-1 } {
         set Data(Path$site) $Path
         lappend Data(Sites) $site

         NowCaster::Radar::Now $NowCaster::Data(Sec)
      }
   } else {
      foreach file $Path {
         set site [file tail $file]
         if { [lsearch -exact $Data(Sites) $site]==-1 } {
            if { [catch { set Data(Scans$site) [radarfile open RADAR${site}0 read $file] }] } {
              Dialog::CreateError .nowcaster $Msg(FileBad) "\n\n$file"
#               radarfile close RADAR${site}0
            } else {
               set Data(Path$site) $file
               lappend Data(Sites) $site
               Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) [NowCaster::Radar::Read $site 0]
            }
         }
      }
   }

   $Data(Frame).select.list selection clear 0 end
   $Data(Frame).select.list selection set end

   NowCaster::Radar::Update $site
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Delete>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer une observation a la liste courante.
#
# Parametres :
#   <Obs>    : Observation a supprimer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::Delete { Site } {
   variable Data

   if { [set idx [lsearch -exact $Data(Sites) $Site]]!=-1 } {
      set Data(Sites) [lreplace $Data(Sites) $idx $idx]
   }

   foreach sec $Data(Secs$Site) {
      if { [radarscan is SCAN$Site$sec] } {
         if { [Viewport::AssignedTo SCAN$Site$sec page vp] } {
            Viewport::UnAssign $page $vp SCAN$Site$sec
         }
         FSTD::UnRegister SCAN$Site$sec

         radarscan free SCAN$Site$sec
         radarfile close RADAR$Site$sec
      }
   }
   set Data(Site) ""

   unset Data(Secs$Site)
   unset Data(Scans$Site)
   unset Data(Path$Site)
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Read>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire un fichier de donnees.
#
# Parametres :
#   <Site>   : Radar
#   <Sec>    : Date (en secondes)
#   <Scan>   : Scan a lire
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::Read { Site Sec { Scan 0 } } {
   global GDefs
   variable Msg
   variable Data

   set NowCaster::Data(Job)  [lindex $Msg(Read) $GDefs(Lang)]
   update idletasks;

   if { ![string is integer $Scan] } {
      if { [set Scan [lsearch -exact $Data(Scans$Site) $Scan]]==-1 } {
         return
      }
   }

   radarscan read SCAN$Site$Sec RADAR$Site$Sec $Scan
   if { $Sec==0 } {
      set Data(Secs$Site) $Sec
   }
   set Data(Limits$Site) [radarscan stats SCAN$Site$Sec -limits]

   #--- Find location
   if { [set id [ogrlayer define RADAR -featureselect [list [list ID == $Site]]]]=="" } {
      Dialog::CreateError .nowcaster $Msg(Loc)
   } else {
      radarscan define SCAN$Site$Sec -LOCATION [ogrlayer define RADAR -feature $id LAT] [ogrlayer define RADAR -feature $id LON] [ogrlayer define RADAR -feature $id ELEV]
      ogrlayer define RADAR -featureselect { }
   }

   radarscan stats SCAN$Site$Sec -limits { 0 10000 0 10000 0 10 }

   NowCaster::Radar::Select $Site $Sec
   NowCaster::Radar::Range

   if { [llength $Data(Secs)] } {
      NowCaster::SetTimeScale [lindex $Data(Secs) end] [lindex $Data(Secs) 0]
   }

   FSTD::Register SCAN$Site$Sec

   set NowCaster::Data(Job)  ""

   return SCAN$Site$Sec
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Range>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher les cercles de distances.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::Range { } {
   variable Data

   set range $Data(Range)

   for { set r 0 } { $r<[llength $range] } { incr r } {
      lset range $r [expr [lindex $range $r]*1000]
   }

   foreach site $Data(Sites) {
      foreach sec $Data(Secs$site) {
         if { [radarscan is SCAN$site$sec] } {
            if { $Data(RangeActive) } {
               radarscan configure SCAN$site$sec -ranges $range -set 0
            } else {
               radarscan configure SCAN$site$sec -ranges {} -set 0
            }
         }
      }
   }

   Page::Update $Page::Data(Frame)
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Areas>
# Creation : Mai 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher la couverture des radars.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::Areas { } {
   variable Data

   Areas::Display RADAR
   Areas::DisplayId RADAR True
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Sweep>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer le sweep (angle) du site.
#
# Parametres :
#   <Site>   : Radar
#   <Sweep>  : Index du sweep
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::Sweep { Site { Sweep 0 } } {
   variable Data

   set scan ""

   foreach sec $Data(Secs$Site) {
      if { [radarscan is SCAN$Site$sec] } {
         set scan SCAN$Site$sec
         radarscan stats $scan -level $Sweep
      }
   }
   set Data(Angle) [lindex $Data(Sweeps) $Sweep]
   NowCaster::Radar::PageUpdate $scan
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Locate>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer la localisation du radar.
#
# Parametres :
#   <Site>   : Radar
#   <Lat>    : Latitude du radar
#   <Lon>    : Longitude du radar
#   <Ele>    : Elevation du radar
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::Locate { Site Lat Lon Ele } {
   variable Data

   set scan ""

   foreach sec $Data(Secs$Site) {
      if { [radarscan is SCAN$Site$sec] } {
         set scan SCAN$Site$sec
         radarscan define $scan -LOCATION $Lat $Lon $Ele
      }
   }
   NowCaster::Radar::PageUpdate $scan
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::ButtonSelect>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Selection d'un radar dans la liste deroulante des radars actifs.
#
# Parametres :
#   <List>   : Liste deroulante
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::ButtonSelect { List } {
   variable Data

   if { [set sel  [$List curselection]]!="" } {
      set site [$List get $sel]

      NowCaster::Radar::Select $site [NowCaster::Radar::Nearest $Data(Sec) $site]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Select>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Selection d'un radar et insertions de ses parametres dans les
#            variables courantes.
#
# Parametres :
#   <Site>   : Radar
#   <Sec>    : Date (en secondes)
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::Select { Site Sec } {
   global GDefs
   variable Data

   if { [info exists ::NowCaster::Radar::Data(Scans$Site)] } {
      ComboBox::DelAll  $Data(Frame).param.scan.sel

      ComboBox::AddList $Data(Frame).param.scan.sel $Data(Scans$Site)
      set Data(Sec)    $Sec

      set Data(Sweeps) [radarscan define SCAN$Site$Sec -SWEEPANGLE]
      set Data(Sweep)  [radarscan stats  SCAN$Site$Sec -level]
      set Data(Angle)  [lindex $Data(Sweeps) $Data(Sweep)]
      $Data(Frame).param.sweep.sel configure -from 0 -to [expr [llength $Data(Sweeps)] -1]

      set Data(Site)    $Site
      set Data(Type)    [radarscan define SCAN$Site$Sec -TYPE]
      set Data(Scan)    [radarscan define SCAN$Site$Sec -SCAN]
      set Data(Id)      [radarscan define SCAN$Site$Sec -SITEID]
      set Data(Name)    [radarscan define SCAN$Site$Sec -SITENAME]
      set Data(Loc)     [radarscan define SCAN$Site$Sec -LOCATION]
      set Data(Lat)     [lindex $Data(Loc) 0]
      set Data(Lon)     [lindex $Data(Loc) 1]
      set Data(Ele)     [lindex $Data(Loc) 2]
      set Data(Date)    [clock format [radarscan define SCAN$Site$Sec -DATE] -format "%Y%m%d %H:%M"]
      set Data(Product) [radarscan define SCAN$Site$Sec -PRODUCT]
      set Data(Noise)   [radarscan define SCAN$Site$Sec -NOISE]
      set Data(Filter)  [radarscan define SCAN$Site$Sec -FILTER]
      set Data(ZCal)    [radarscan define SCAN$Site$Sec -ZCAL]
      set Data(Nyquist) [radarscan define SCAN$Site$Sec -NYQUIST]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Radar::Draw...>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de manipulations de la selection sur la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Ces fonctions sont appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc NowCaster::Radar::DrawInit  { Frame VP } {
   global GDefs
   variable Data
   variable Param
   variable Lbl

   if { [set index [lindex [ogrlayer pick RADAR [list $Viewport::Map(LatCursor) $Viewport::Map(LonCursor) ] 0]]]!="" } {
      set radar [ogrlayer define RADAR -feature $index ID]
      NowCaster::Radar::Add $Param(Path)/$radar
   }
}

proc NowCaster::Radar::Draw      { Frame VP } {
}

proc NowCaster::Radar::DrawDone { Frame VP } {
}

proc NowCaster::Radar::MoveInit { Frame VP } {
}

proc NowCaster::Radar::Move { Frame VP } {
}

proc NowCaster::Radar::MoveDone { Frame VP } {
}
