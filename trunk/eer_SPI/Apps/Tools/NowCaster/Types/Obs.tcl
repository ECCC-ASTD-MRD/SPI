#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Outils de "NowCasting" pour SPI.
# Fichier  : Obs.tcl
# Version  : 1.0
# Creation : Avril 2006 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fonctions de manipulations des observations meteorologiques pour l'outils
#    de NowCasting de SPI.
#
# Fonctions:
#
#
# Remarques :
#
# Modification:
#
#   Nom         :
#   Date        :
#   Description :
#
#===============================================================================

namespace eval NowCaster::Obs { } {
   variable Data
   variable Lbl
   variable Error
   variable Msg
   variable Bubble
   variable Param

   set Param(Title)      { "Observation" "Observation" }

   set Param(PathsSurf)  { /data/ade/dbase/surface/drifter /data/ade/dbase/surface/metar /data/ade/dbase/surface/sa
                           /data/ade/dbase/surface/ozone /data/ade/dbase/surface/pm25 /data/ade/dbase/surface/synop
                           /data/ade/dbase/surface/shef }
   set Param(PathsUpper) { /data/ade/dbase/uprair/profiler /data/ade/dbase/uprair/radiosonde /data/ade/dbase/uprair/ads /data/ade/dbase/uprair/pirep /data/ade/dbase/uprair/airep }

   set Data(Frame)       ""

   set Data(ModelName)   ""
   set Data(Models)      { }
   set Data(Obs)         { }
   set Data(Topo)        ""
   set Data(CurrentObs)  NONE
   set Data(Spacing)     25
   set Data(Flat)        0
   set Data(Elems)       { }
   set Data(Item)        ""
   set Data(InfoObs)     ""
   set Data(InfoId)      ""
   set Data(Id)          ""
   set Data(Report)      False

   set Lbl(Var0)    { "Variable  " "Variable  " }
   set Lbl(Var1)    { "Direction " "Direction " }
   set Lbl(Yes)     { "Oui" "Yes" }
   set Lbl(No)      { "Non" "No" }
   set Lbl(Topo)    { "�l�vation " "Elevation " }
   set Lbl(Data)    { "Donn�es" "Data" }
   set Lbl(Model)   { "Mod�le de pointage" "Plotting model" }
   set Lbl(ModelN)  { "Mod�le" "Model" }
   set Lbl(Spacing) { "Espacement" "Spacing" }
   set Lbl(Flat)    { "Projet�" "Projected" }
   set Lbl(Info)    { "Information" "Information" }
   set Lbl(Pos)     { "Position" "Position" }
   set Lbl(Elem)    { "Elements" "Elements" }
   set Lbl(Report)  { "Rapports" "Reports" }
   set Lbl(Find)    { "Trouver une station" "Find station" }
   set Lbl(Surface) { "Surface" "Surface" }
   set Lbl(Upper)   { "Upper" "Upper" }

   set Msg(Read)   { "Lecture des donn�es d'observations" "Reading observation data" }
   set Msg(Exist)  { "Un model de ce nom existe d�ja, d�sirez vous le remplacer ?"
                     "A model by this name already exists, do you we to replace it ?" }
   set Msg(Del)    { "Voulez-vous vraiment supprimer de mod�le ?"
                     "do you really want to suppress this model ?" }

   set Error(File) { "Fichier d'observation invalide."
                     "Invalid observation file." }

   set Bubble(Find)       { "Rechercher un station et centrer la vue sur celle-ci" "Find a station and locate the viewport on it" }
   set Bubble(Mode)       { "Activer le mode de s�lection des observations\n\nBouton gauche: S�lection\nBouton centre: D�placer une localisation" "Activate observation selection mode\n\nLeft button  : Select location\nMiddle button: Move location" }
   set Bubble(Params)     { "Param�tres d'affiche" "Display parameters" }
   set Bubble(Add)        { "Ajouter une observation" "Add an observation" }
   set Bubble(Del)        { "Supprimer l'observation courante" "Delete current observation" }
   set Bubble(Model)      { "Nom du mod�le de pointage" "Plotting model name" }
   set Bubble(ModelSave)  { "Sauvegarde du mod�le de pointage courant" "Save the current plotting model" }
   set Bubble(ModelDel)   { "Supprimer le mod�le de pointage courant" "Delete the current plotting model" }
   set Bubble(ModelClear) { "Effacer le mod�le de pointage courant" "Clear the current plotting model" }
   set Bubble(Variable0)  { "Element � affiche\n(Composante de vitesse en configuration vectorielle)" "Element to display\n(Speed component in vectorial configuration)" }
   set Bubble(Variable1)  { "Element � afficher comme composante\ndirection en configuration vectorielle" "Direction component when\nin vectorial configuration" }
   set Bubble(Topo)       { "Variable definnisant l'�l�vation" "Variable definning the elevation" }
   set Bubble(Spacing)    { "Espacement entre les �l�ments de pointage" "Spacing between the plotted elements" }
   set Bubble(Grid)       { "S�lection du positionnement des �l�ment\nrelativement � la position centrale en blanc" "Element position selection relative\not the central location in white" }
}

#----------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::Window>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche l'interface de manipulation des observations.
#
# Parametres   :
#
# Retour:
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc NowCaster::Obs::Window { Frame } {
   global GDefs
   variable Data
   variable Lbl
   variable Bubble
   variable Param

   set Data(Frame) $Frame

   frame $Frame.head
      checkbutton $Frame.head.mode -variable Page::Data(ToolMode) -onvalue NowCaster::Obs -offvalue SPI \
         -image ARROW -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) \
         -command { SPI::ToolMode $Page::Data(ToolMode) Data True }
      button $Frame.head.params -image PARAMS -bd 0 -relief flat -overrelief raised -command { SPI::Params ;  TabFrame::Select .params.tab 2 }
      menubutton $Frame.head.add -image PLUS -bd 0 -relief flat -menu $Frame.head.add.list
      button $Frame.head.del -image DELETE -bd 1 -relief flat -overrelief raised \
         -command "catch { NowCaster::Obs::Delete \[$Frame.select.list get \[$Frame.select.list curselection\]\] }"
      pack $Frame.head.mode $Frame.head.params  -side left
      pack $Frame.head.del $Frame.head.add -side right -padx 2
   pack $Frame.head -side top -fill x

   menu $Frame.head.add.list -bd 1 -tearoff 0
   foreach path $Param(PathsSurf) {
      set type [file tail $path]
      $Frame.head.add.list add command -label $type -command "NowCaster::Obs::Add $path"
   }
   $Frame.head.add.list add separator
   foreach path $Param(PathsUpper) {
      set type [file tail $path]
      $Frame.head.add.list add command -label $type -command "NowCaster::Obs::Add $path"
   }
   $Frame.head.add.list add separator
   $Frame.head.add.list add command -label ... -command "NowCaster::Obs::Add  \[FileBox::Create . \"\" Load \"\"\]"

   frame $Frame.select
      listbox $Frame.select.list -relief sunken -bd 1 -yscrollcommand "$Frame.select.scroll set" -selectmode single \
         -width 1 -height 1 -background white -listvar nnn -selectbackground $GDefs(ColorHighLight) -selectforeground black \
         -listvariable NowCaster::Obs::Data(Obs) -exportselection false
      scrollbar $Frame.select.scroll -relief sunken -command "$Frame.select.list yview" -bd 1 -width 10
      pack $Frame.select.list -side left -fill both -expand true
      pack $Frame.select.scroll -side left -fill y
   pack $Frame.select -side top -fill both -expand true

   frame  $Frame.find
      labelframe $Frame.find.name -text [lindex $Lbl(Find) $GDefs(Lang)]
         entry $Frame.find.name.entry -textvariable NowCaster::Obs::Data(Id) -bg $GDefs(ColorLight) -bd 1
         button  $Frame.find.name.go -image FINGER -bd 0 -relief flat -overrelief raised \
            -command { NowCaster::Obs::Find $NowCaster::Obs::Data(CurrentObs) }
         pack $Frame.find.name.entry -side left -fill both -expand True -padx 2 -pady 2
         pack $Frame.find.name.go -side left -padx 2 -pady 2
         bind $Frame.find.name.entry <Return> { NowCaster::Obs::Find $NowCaster::Obs::Data(CurrentObs)}
      pack $Frame.find.name -side top -fill x -padx 5 -pady 5 -expand True

   frame  $Frame.model
      labelframe $Frame.model.name -text [lindex $Lbl(ModelN) $GDefs(Lang)]
         ComboBox::Create $Frame.model.name.model NowCaster::Obs::Data(ModelName) edit sorted nodouble -1 $NowCaster::Obs::Data(Models) 2 6 \
            "NowCaster::Obs::ModelSelect \$NowCaster::Obs::Data(ModelName)"
         button $Frame.model.name.new  -image PINNEW  -relief flat -bd 0 -overrelief raised \
            -command { NowCaster::Obs::ModelSelect \$NowCaster::Obs::Data(ModelName) { { 0 0 0 } } }
         button $Frame.model.name.save  -image PINSAVE  -relief flat -bd 0 -overrelief raised \
            -command { NowCaster::Obs::ModelAdd $NowCaster::Obs::Data(ModelName) [NowCaster::Obs::ModelParse] }
         button $Frame.model.name.del    -image PINDEL   -relief flat -bd 0 -overrelief raised \
            -command { NowCaster::Obs::ModelDel $NowCaster::Obs::Data(ModelName) }
         pack $Frame.model.name.model -side left -fill x -expand True -padx 2
         pack $Frame.model.name.new $Frame.model.name.save $Frame.model.name.del -side left
      pack $Frame.model.name -side top -fill x -padx 5 -expand True

      labelframe $Frame.model.elem -text [lindex $Lbl(Elem) $GDefs(Lang)]
         frame $Frame.model.elem.var0
            label $Frame.model.elem.var0.lbl -text [lindex $Lbl(Var0) $GDefs(Lang)] -width 11 -anchor w
            ComboBox::Create $Frame.model.elem.var0.sel NowCaster::Obs::Data(Var0) noedit sorted nodouble -1 { } 2 15 NowCaster::Obs::ModelApply
            pack $Frame.model.elem.var0.lbl -side left
            pack $Frame.model.elem.var0.sel -side left -fill x -expand True
         pack $Frame.model.elem.var0 -side top -fill x -padx 2 -expand True

         frame $Frame.model.elem.var1
            label $Frame.model.elem.var1.lbl -text [lindex $Lbl(Var1) $GDefs(Lang)] -width 11 -anchor w
            ComboBox::Create $Frame.model.elem.var1.sel NowCaster::Obs::Data(Var1) noedit sorted nodouble -1 { } 2 15 NowCaster::Obs::ModelApply
            pack $Frame.model.elem.var1.lbl -side left
            pack $Frame.model.elem.var1.sel -side left -fill x -expand True
         pack $Frame.model.elem.var1 -side top -fill x -padx 2 -pady 2 -expand True

         frame $Frame.model.elem.topo
            label $Frame.model.elem.topo.lbl -text [lindex $Lbl(Topo) $GDefs(Lang)] -width 11 -anchor w
            ComboBox::Create $Frame.model.elem.topo.sel NowCaster::Obs::Data(Topo) noedit sorted nodouble -1 { } 2 15 set NowCaster::Obs::Data(Topo\$NowCaster::Obs::Data(CurrentObs)) \$NowCaster::Obs::Data(Topo)\; NowCaster::Obs::ModelApply
            pack $Frame.model.elem.topo.lbl -side left
            pack $Frame.model.elem.topo.sel -side left -fill x -expand True
         pack $Frame.model.elem.topo -side top -fill x -padx 2 -pady 2 -expand True

         frame $Frame.model.elem.spc
            label $Frame.model.elem.spc.lbl -text [lindex $Lbl(Spacing) $GDefs(Lang)] -width 11 -anchor w
            label $Frame.model.elem.spc.txt -textvariable NowCaster::Obs::Data(Spacing) -width 4 -anchor w -relief sunken -bd 1 -bg $GDefs(ColorLight)
            scale $Frame.model.elem.spc.val -bd 1 -relief flat -width 15 -sliderlength 10 -from 0 -to 100 -variable NowCaster::Obs::Data(Spacing) \
               -orient horizontal -showvalue False -command { set NowCaster::Obs::Data(Spacing$NowCaster::Obs::Data(CurrentObs)) $NowCaster::Obs::Data(Spacing); NowCaster::Obs::ModelApply; catch }
            pack $Frame.model.elem.spc.lbl $Frame.model.elem.spc.txt -side left
            pack $Frame.model.elem.spc.val -side left -fill x -expand True
         pack  $Frame.model.elem.spc -side top -anchor w -padx 2 -fill x

         frame $Frame.model.elem.flat
            label $Frame.model.elem.flat.lbl -text [lindex $Lbl(Flat) $GDefs(Lang)] -width 11 -anchor w
            checkbutton $Frame.model.elem.flat.sel -variable NowCaster::Obs::Data(Flat) -relief sunken -bd 1 -overrelief raised -offrelief flat \
               -selectcolor $GDefs(ColorFrame) -onvalue 0 -offvalue 1 -indicatoron False \
               -command { set NowCaster::Obs::Data(Flat$NowCaster::Obs::Data(CurrentObs)) $NowCaster::Obs::Data(Flat); NowCaster::Obs::ModelApply }
            pack $Frame.model.elem.flat.lbl -side left
            pack $Frame.model.elem.flat.sel -side left -fill x -expand True
         pack $Frame.model.elem.flat -side top -fill x -padx 2 -pady 2 -expand True
      pack $Frame.model.elem -side top -padx 5 -pady 5 -fill x

         labelframe $Frame.model.items -text [lindex $Lbl(Pos) $GDefs(Lang)]
         frame $Frame.model.items.s -relief sunken -bd 1
         set n 0
         for { set x -2 } { $x <= 2 } { incr x  } {
            for { set y -2 } { $y <= 2 } { incr y } {
               set NowCaster::Obs::Data(Model.$x.$y) ""
               radiobutton $Frame.model.items.s.def$n -textvariable NowCaster::Obs::Data(Set.$x.$y) -relief raised -bd 1  -width 2 \
                  -variable NowCaster::Obs::Data(Item) -value .$x.$y -indicatoron False \
                  -command "set NowCaster::Obs::Data(Var0) \[lindex \$NowCaster::Obs::Data(Model.$x.$y) 0\]; set NowCaster::Obs::Data(Var1) \[lindex \$NowCaster::Obs::Data(Model.$x.$y) 1\]"
               grid $Frame.model.items.s.def$n -row [expr $y+2] -column [expr $x+2] -sticky nsew
               incr n
            }
         }
         $Frame.model.items.s.def12 configure -bg white

         set NowCaster::Obs::Data(Model.0.3) ""
         radiobutton $Frame.model.items.s.def$n -textvariable NowCaster::Obs::Data(Set.0.3) -relief raised -bd 1 -width 5 \
            -variable NowCaster::Obs::Data(Item) -value .0.3  -indicatoron False \
            -command "set NowCaster::Obs::Data(Var0) \[lindex \$NowCaster::Obs::Data(Model.0.3) 0\]; set NowCaster::Obs::Data(Var1) \[lindex \$NowCaster::Obs::Data(Model.0.3) 1\]"
         grid $Frame.model.items.s.def$n -row 5 -column 1 -columnspan 3 -sticky nsew

      pack $Frame.model.items.s -side top -padx 5 -pady 5
      pack $Frame.model.items -side top -padx 5 -pady 5 -fill x
   pack $Frame.find $Frame.model -side top -fill x

   bind $Frame.select.list <ButtonRelease-1>  "catch { NowCaster::Obs::ObsSelect \[$Frame.select.list get \[$Frame.select.list curselection\]\] }"

   Bubble::Create $Frame.head.mode   [lindex $Bubble(Mode) $GDefs(Lang)]
   Bubble::Create $Frame.head.params [lindex $Bubble(Params) $GDefs(Lang)]
   Bubble::Create $Frame.head.add    [lindex $Bubble(Add) $GDefs(Lang)]
   Bubble::Create $Frame.head.del    [lindex $Bubble(Del) $GDefs(Lang)]

   Bubble::Create $Frame.find        [lindex $Bubble(Find) $GDefs(Lang)]

   Bubble::Create $Frame.model.name.model [lindex $Bubble(Model) $GDefs(Lang)]
   Bubble::Create $Frame.model.name.save  [lindex $Bubble(ModelSave) $GDefs(Lang)]
   Bubble::Create $Frame.model.name.del   [lindex $Bubble(ModelDel) $GDefs(Lang)]
   Bubble::Create $Frame.model.name.new   [lindex $Bubble(ModelClear) $GDefs(Lang)]

   Bubble::Create $Frame.model.elem.var0.sel  [lindex $Bubble(Variable0) $GDefs(Lang)]
   Bubble::Create $Frame.model.elem.var1.sel  [lindex $Bubble(Variable1) $GDefs(Lang)]
   Bubble::Create $Frame.model.elem.topo.sel  [lindex $Bubble(Topo) $GDefs(Lang)]
   Bubble::Create $Frame.model.elem.spc.val   [lindex $Bubble(Spacing) $GDefs(Lang)]
   Bubble::Create $Frame.model.items          [lindex $Bubble(Grid) $GDefs(Lang)]

   NowCaster::Obs::ModelLoad
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::Cast>
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
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::Cast { Sec } {
   variable Data

   foreach obs $Data(Obs) {
      metobs define $obs -VALID $Sec False
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::Now>
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
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::Now { Sec { Check False } } {
   global GDefs
   variable Data

   foreach obs $Data(Obs) {
      if { [set file [lindex [lsort -dictionary [glob -nocomplain $Data(Path$obs)/*_]] end]]!="" } {
         file stat $file valid

         if { $valid(mtime)>$Data(Valid$obs) } {
            puts "NowCaster::Obs::Now: Change in $obs $file"
            NowCaster::Obs::Read $obs $file
         }
         metobs define $obs -VALID $Sec False
      }
   }

   if { $Data(InfoObs)!="" && [winfo exists .nowcasterinfo] } {
      NowCaster::Obs::Info $Data(InfoObs) $Data(InfoId)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::Read>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire un fichier de donnees.
#
# Parametres :
#   <Obs>    : Objet observation a modifier
#   <File>   : Fichier a lire
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ReadProcess { Obs } {
   variable Data

   Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) $Obs

   #----- Define default model
   set Data(Elems$Obs) [metobs define $Obs -ELEMENT]
   if { ![llength $Data(Model$Obs)] } {
      set Data(Model$Obs) [list [list 0 0 [lindex $Data(Elems$Obs) 0]]]
      NowCaster::Obs::ObsSelect $Obs
   }
   NowCaster::SetTimeScale [metobs define $Obs -DATE0] [metobs define $Obs -DATE1]
}

proc NowCaster::Obs::Read { Obs Files } {
   global GDefs
   variable Msg
   variable Data

   set NowCaster::Data(Job)  [lindex $Msg(Read) $GDefs(Lang)]
   update idletasks;

   #----- Create the Obs object

   if { ![metobs is $Obs] } {
      metobs create $Obs
      set Data(Valid$Obs) 0
      set Data(Model$Obs) {}
      set Data(Elems$Obs) {}
   }

   #----- Get the latest validity time

   file stat [lindex $Files 0] valid
   set Data(Valid$Obs) [expr $Data(Valid$Obs)<$valid(mtime)?$valid(mtime):$Data(Valid$Obs)]

   #----- Create the reading thread

   set id [thread::create {
      package require TkViewport

      proc NowCasterObsReader { Obs Files { Thread 0 } } {

         foreach file $Files {
            if { [catch { metobs read $Obs $file }] } {
               thread::send $Thread "Dialog::CreateError .nowcaster \"\[lindex \$NowCaster::Obs::Error(File) \$GDefs(Lang)\]\n\n$file\" \$GDefs(Lang)"
            } else {
               file stat $file valid
               if { $Thread!="0" } {
                  thread::send $Thread [list NowCaster::Obs::ReadProcess $Obs]
               }
            }
         }
         thread::release
      }
      thread::wait
    }]

   #----- Start the thread

   thread::send -async $id [list NowCasterObsReader $Obs $Files [thread::id]]

   set NowCaster::Data(Job)  ""
}

proc NowCaster::Obs::ExportSCIPUFF { Obs File Type } {

   if { [$File=="" } {
      return
   }

   set f [open w $File]

   if { $Type=="SURFACE" } {
      puts $f "SURFACE"
      puts $f "12"
      puts $f "ID      LAT     LON     YEAR    MONTH   DAY     HOUR    DIR     WSPD    T"
      puts $f "Q       P"
      puts $f "NONE    N       W                               HRS             M/S     C"
      puts $f "%       MBAR"
      puts $f "-999.00"

      foreach loc $Locations {
         foreach date [metobs define $Obs -DATE $loc] {
            set coord [metobs define $Obs -COORD $loc]
            set pre   [metobs define $Obs -ELEMENT $loc "PRESSURE" $date]
            set dir   [metobs define $Obs -ELEMENT $loc "WIND DIRECTION AT 10M" $date]
            set spd   [metobs define $Obs -ELEMENT $loc "WIND SPEED AT 10M" $date]
            set tmp   [metobs define $Obs -ELEMENT $loc "DRY BULB TEMPERATURE AT 2M" $date]
            set dew   [metobs define $Obs -ELEMENT $loc "DEW POINT TEMPERATURE AT 2M" $date]
            set hum   [metobs define $Obs -ELEMENT $loc "RELATIVE HUMIDITY" $date]
            puts $f [format "%5.5s %6.2f %7.2f %4.4d %s %s %6.3f %4d %6.2f %6.2f %8.2f" \
               $loc [lindex $coord 0] [lindex $coord 1] [clock format "%Y" $date] [clock format "%m" $date] [clock format "%m" $d] \
               $dir $spd [expr $tmp-273.16] $dew [expr $hum/100.0]]

         }
      }
   } else {
      puts $f "PROFILE"
      puts $f "13"
      puts $f "ID      LAT     LON     YEAR    MONTH   DAY     HOUR    Z       DIR     WSPD"
      puts $f "T       Q       PP"
      puts $f "NONE    N       W                               HRS     M               M/S"
      puts $f "C       %       MBAR"
      puts $f "-999.00"

      foreach loc $Locations {
         foreach date [metobs define $Obs -DATE $loc] {
            set coord [metobs define $Obs -COORD $loc]
            set hgts   [metobs define $Obs -ELEMENT $loc "GEOPOTENTIAL HEIGHT" $date]
            set pres   [metobs define $Obs -ELEMENT $loc "PRESSURE" $date]
            set dirs   [metobs define $Obs -ELEMENT $loc "WIND DIRECTION AT 10M" $date]
            set spds   [metobs define $Obs -ELEMENT $loc "WIND SPEED AT 10M" $date]
            set tmps   [metobs define $Obs -ELEMENT $loc "DRY BULB TEMPERATURE AT 2M" $date]
            set dews   [metobs define $Obs -ELEMENT $loc "DEW POINT TEMPERATURE AT 2M" $date]
            set hums   [metobs define $Obs -ELEMENT $loc "RELATIVE HUMIDITY" $date]

            foreach hgt $hgts pre $pres dir $dirs spd $spds tmp $tmps dew $dews hum $hums {
               if { $hgt=="" } { set hgt -999.00 }
               if { $pre=="" } { set pre -999.00 }
               if { $dir=="" } { set dir -999.00 }
               if { $spd=="" } { set spd -999.00 }
               if { $tmp=="" } { set tmp -999.00 }
               if { $dew=="" } { set dew -999.00 }
               if { $hum=="" } { set hum -999.00 }

               puts $f [format "%5.5s %6.2f %7.2f %4.4d %s %s %6.3f %4d %6.2f %6.2f %8.2f" \
                  $loc [lindex $coord 0] [lindex $coord 1] [clock format "%Y" $date] [clock format "%m" $date] [clock format "%m" $d] \
                  $dir $spd [expr $tmp-273.16] $dew [expr $hum/100.0]]
            }
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::Add>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter une observation a la liste courante.
#
# Parametres :
#   <Path>   : Path complet du fichier a lire
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::Add { Path } {
   variable Data

   if { $Path=="" } {
      return
   }

   set path [lindex $Path 0]

   #----- Figure out a unique number

   set file [file tail $path]

   set no [format "%02i" [llength [lsearch -all -glob [$Data(Frame).select.list get 0 end] $file*]]]
   set obs ${file}_$no
   lappend Data(Obs) $obs
   $Data(Frame).select.list selection clear 0 end
   $Data(Frame).select.list selection set end

   #----- Define default model

   set Data(Path$obs) $path
   set Data(ModelName$obs) ""
   set Data(Topo$obs)      ""
   set Data(Param$obs)     {}
   set Data(Spacing$obs)   $Data(Spacing)
   set Data(Flat$obs)      $Data(Flat)

   #----- Read in the data

   if { [file isdirectory $path] } {
      set files [lsort -decreasing -dictionary [glob -nocomplain $path/*_]]
      NowCaster::Obs::Read $obs [lrange $files 0 4]
   } else {
      foreach file $Path {
         NowCaster::Obs::Read $obs $file
      }
   }

   Page::Update        $Page::Data(Frame)
   Page::UpdateCommand $Page::Data(Frame)
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::Delete>
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
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::Delete { Obs } {
   variable Data

   if { [set idx [lsearch -exact $Data(Obs) $Obs]]!=-1 } {
      set Data(Obs) [lreplace $Data(Obs) $idx $idx]
   }

   if { [Viewport::AssignedTo $Obs page vp] } {
      Viewport::UnAssign $page $vp $Obs
   }
   Obs::UnRegister $Obs
   NowCaster::Obs::PageUpdate $Obs

   if { $Obs==$Data(InfoObs) } {
      set Data(InfoObs) ""
      set Data(InfoId)  ""
   }

   metobs free $Obs
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::PageUpdate>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour l'affichage de la Page et du Viewport dans lesquel
#            l'Obs est assignee.
#
# Parametres :
#   <Obs>    : Observation
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::PageUpdate { Obs } {

   Viewport::AssignedTo $Obs page vp
   Viewport::LinkDo $vp

   Page::Update        $page
   Page::UpdateCommand $page
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::Update>
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
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::Update { { Obs {} } } {
   variable Data

   if { ![llength $Obs] } {
      set Obs $Data(Obs)
   }

   foreach obs $Obs {
      metmodel define [metobs define $obs -MODEL] -items $Data(Model$obs) -spacing $Data(Spacing$obs) -flat  $Data(Flat$obs) -topography $Data(Topo$obs)
      metobs define $obs -VALID $NowCaster::Data(Sec) False -PERSISTANCE $NowCaster::Data(Persistance)

      foreach item $Data(Model$obs) {
         set var [lindex $item 2]
         if { ![dataspec is $var] } {
            dataspec create $var
            catch { dataspec configure $var -unit [lindex [metobs table -code [metobs table -desc $var]] end] }
            dataspec configure $var -desc $var -set 0
        }
        metmodel configure [metobs define $obs -MODEL] $var -dataspec $var
      }
   }
   Obs::ParamUpdate
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::ObsSelect>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer la selection d'une observation.
#
# Parametres :
#   <Obs>    : Observation
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ObsSelect { Obs } {
   variable Data

   if { [metobs is $Obs] } {
      set Data(CurrentObs) $Obs
      set Data(Elems)      $Data(Elems$Obs)
      set Data(ModelName)  $Data(ModelName$Obs)

      if { [winfo exists .nowcaster] } {
         ComboBox::DelAll  $Data(Frame).model.elem.var0.sel
         ComboBox::AddList $Data(Frame).model.elem.var0.sel $Data(Elems$Obs)
         ComboBox::Add     $Data(Frame).model.elem.var0.sel ""

         ComboBox::DelAll  $Data(Frame).model.elem.var1.sel
         ComboBox::AddList $Data(Frame).model.elem.var1.sel $Data(Elems$Obs)
         ComboBox::Add     $Data(Frame).model.elem.var1.sel ""

         ComboBox::DelAll  $Data(Frame).model.elem.topo.sel
         ComboBox::AddList $Data(Frame).model.elem.topo.sel $Data(Elems$Obs)
         ComboBox::Add     $Data(Frame).model.elem.topo.sel ""
      }
      NowCaster::Obs::ModelSelect $Obs $Data(Model$Obs)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::ModelApply>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Appliquer la selection de l'usager aux parametres du modele de pointage.
#
# Parametres :
#   <Model>  : Nom du modele
#   <List>   : Parametres du modele
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ModelApply { } {
   variable Data

   set Data(ModelName) ""

   if { $Data(Var0)=="" && $Data(Var1)=="" } {
      set Data(Model$Data(Item)) {}
      set Data(Set$Data(Item))   ""
   } else {
      set Data(Model$Data(Item)) [list $Data(Var0) $Data(Var1)]
      set Data(Set$Data(Item))   #
   }
   set Data(Model$Data(CurrentObs)) [NowCaster::Obs::ModelParse]

   if { [metobs is $Data(CurrentObs)] } {
      NowCaster::Obs::Update $Data(CurrentObs)
      NowCaster::Obs::PageUpdate $Data(CurrentObs)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::ModelSelect>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer la selection d'un modele de pointage.
#
# Parametres :
#   <Model>  : Nom du modele
#   <List>   : Parametres du modele
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ModelSelect { Model { List { } } } {
   variable Data

   if { [llength $List] } {
      set model $List
   } else {
      set model $Data(Models$Model)
   }

   #----- Reset

   for { set x -2 } { $x <= 2 } { incr x  } {
      for { set y -2 } { $y <= 2 } { incr y } {
         set Data(Model.$x.$y) {}
         set Data(Set.$x.$y)   ""
      }
   }
   set Data(Model.0.3) {}
   set Data(Set.0.3)   ""

   #----- Set

   foreach part $model {
      set dx [lindex $part 0]
      set dy [lindex $part 1]

      set Data(Model.$dx.$dy) [lrange $part 2 end]
      set Data(Item) .$dx.$dy
      set Data(Set.$dx.$dy) #
   }

   if { $Data(Item)!="" } {
      set Data(Var0) [lindex $Data(Model$Data(Item)) 0]
      set Data(Var1) [lindex $Data(Model$Data(Item)) 1]
   }

   set Data(ModelName$Data(CurrentObs)) $Data(ModelName)
   set Data(Model$Data(CurrentObs))     [NowCaster::Obs::ModelParse]
   if { [metobs is $Data(CurrentObs)] } {
      NowCaster::Obs::Update $Data(CurrentObs)
      NowCaster::Obs::PageUpdate $Data(CurrentObs)
   }

   if { [info exists ::NowCaster::Obs::Data(Param$Model)] } {
      foreach param $Data(Param$Model) {
         set var [lindex $param 2]
         if { ![dataspec is $var] } {
            dataspec create $var
         }
         eval $param
      }
      Obs::ParamGet
      Obs::ParamPut
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::ModelDel>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Suppression d'un modele de pointage.
#
# Parametres :
#   <Model>  : Nom du modele
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ModelDel { Model } {
   global GDefs
   variable Data
   variable Lbl
   variable Msg

   if { [set idx [lsearch -exact $Data(Models) $Model]]!=-1 } {
      set ok [Dialog::CreateDefault $Data(Frame) 200 "Info" "[lindex $Msg(Del) $GDefs(Lang)]\n\n\t$Model" info 1 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

      if { !$ok } {
         set Data(Models) [lreplace $Data(Models) $idx $idx]
      }
      NowCaster::Obs::ModelSave
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::ModelAdd>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajout d'un modele de pointage.
#
# Parametres :
#   <Model>  : Nom du modele
#   <Params> : Parametres du modele
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ModelAdd { Model Params } {
   global GDefs
   variable Data
   variable Lbl
   variable Msg

   if { $Model!= "" } {
      if { [set idx [lsearch -exact $Data(Models) $Model]]!=-1 } {
         set ok [Dialog::CreateDefault $Data(Frame) 200 "Info" [lindex $Msg(Exist) $GDefs(Lang)] info 1 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

         if { !$ok } {
            set Data(Models$Model) $Params
         }
      } else {
         lappend Data(Models) $Model
         set Data(Models$Model) $Params
      }
      NowCaster::Obs::ModelSave
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::ModelLoad>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire les modeles de pointage sauvegarde.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ModelLoad { } {
   global GDefs
   variable Data

   set Data(Models) {}

   if {[file exists $GDefs(DirEER)/eer_ObsModel] } {
      set f [open $GDefs(DirEER)/eer_ObsModel r]
      while { ![eof $f] } {
         gets $f line
         if { [string index $line 0] != "#" && [string length $line] > 0 } {
            set name [lindex $line 0]
            lappend Data(Models) $name
            set Data(Models$name) [lindex $line 1]
            set Data(Param$name) [lindex $line 2]

            #----- Convert description to code
            set m 0
            foreach item $Data(Models$name)  {
               set code0 [lindex $item 2]
               set code1 [lindex $item 3]
               lset Data(Models$name) $m 2 [set code0 [lindex [metobs table -desc $code0] 0]]
               if { $code1!="" } {
                  lset Data(Models$name) $m 3 [lindex [metobs table -desc $code1] 0]
               }
               lset Data(Param$name) $m 2 $code0
               incr m
            }
         }
      }
      close $f
   }

   ComboBox::DelAll  $Data(Frame).model.name.model
   ComboBox::AddList $Data(Frame).model.name.model $Data(Models)
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::ModelSave>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder la liste des modeles de pointage.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ModelSave { } {
   global GDefs
   variable Data

   if {[file exists $GDefs(DirEER)/eer_ObsModel] } {
      file rename -force $GDefs(DirEER)/eer_ObsModel $GDefs(DirEER)/eer_ObsModel.old
   }

   set f [open $GDefs(DirEER)/eer_ObsModel w]
   foreach model $Data(Models) {
      set mparam {}
      set msave $Data(Models$model)

      #----- Convert code to description
      set m 0
      foreach item $Data(Models$model) {
         set var0 [lindex $item 2]
         set var1 [lindex $item 3]
         lset msave $m 2 [set var0 [metobs table -code $var0]]
         if { $var1!="" } {
            lset msave $m 3 [set var1 [metobs table -code $var1]]
         }

         Obs::ParamGet [lindex $item 2]
         lappend mparam "dataspec configure $var0 -factor $Obs::Param(Factor) -value $Obs::Param(Order) $Obs::Param(Mantisse) -size $Obs::Param(Size)\
            -icon \"$Obs::Param(Icon)\" -color \"$Obs::Param(Color)\" -unit \"$Obs::Param(Unit)\" -rendercontour $Obs::Param(Contour)\
            -rendervector $Obs::Param(Vector) -rendertexture $Obs::Param(Texture) -rendervolume $Obs::Param(Volume)\
            -rendercoord $Obs::Param(Coord) -rendervalue $Obs::Param(Value) -renderlabel $Obs::Param(Label)\
            -min \"$Obs::Param(Min)\" -max \"$Obs::Param(Max)\" -intervals \"$Obs::Param(Intervals)\" -intervalmode $Obs::Param(IntervalMode) $Obs::Param(IntervalParam)"
         incr m
      }
      puts $f "\"$model\" { $msave } { $mparam }"
   }
   close $f

   ComboBox::DelAll  $Data(Frame).model.name.model
   ComboBox::AddList $Data(Frame).model.name.model $Data(Models)
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::ModelParse>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer la liste des parametres du modele selon la selection de l'usager.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ModelParse { } {
   variable Data

   set model {}
   for { set x -2 } { $x <= 2 } { incr x  } {
      for { set y -2 } { $y <= 2 } { incr y } {
         if { [llength $Data(Model.$x.$y)] } {
            set mx [lindex $Data(Model.$x.$y) 0]
            set my [lindex $Data(Model.$x.$y) 1]
            lappend model [list $x $y $mx $my]
         }
      }
   }
   if { [llength $Data(Model.0.3)] } {
      set mx [lindex $Data(Model.0.3) 0]
      set my [lindex $Data(Model.0.3) 1]
      lappend model [list 0 3 $mx $my]
   }
   return $model
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::Info>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer toutes les informations sur un station en particulier et
#            afficher le tout dans une fenetre d'edition.
#
# Parametres :
#   <Obs>    : Observation
#   <Id>     : Identificateur de la station
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::Info { Obs Id } {
   global GDefs
   variable Data
   variable Lbl

   if { [metobs is $Obs] } {
      set text [Text::Create .nowcasterinfo "[lindex $Lbl(Info) $GDefs(Lang)]: $Obs" "" 80 20]
      $text delete 0.0 end

      if { ![winfo exists .nowcasterinfo.cmd] } {
         frame .nowcasterinfo.cmd -relief raised -bd 1
            radiobutton .nowcasterinfo.cmd.on -text [lindex $Lbl(Report) $GDefs(Lang)] -variable NowCaster::Obs::Data(Report) -value True \
               -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False -command "NowCaster::Obs::Info $Obs $Id"
            radiobutton .nowcasterinfo.cmd.off -text [lindex $Lbl(Elem) $GDefs(Lang)] -variable NowCaster::Obs::Data(Report) -value False \
               -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False -command "NowCaster::Obs::Info $Obs $Id"
            pack .nowcasterinfo.cmd.on .nowcasterinfo.cmd.off  -side left -fill x -expand True
         pack .nowcasterinfo.cmd -side top -fill x
      }

      $text insert 0.0 "Station : $Id\n"

      #----- Per report
      if { $Data(Report) } {
         foreach date [metobs define $Obs -DATE $Id] {
            $text insert end "\n[clock format $date  -format "%Y%m%d %H:%M" -gmt true]\n"
            foreach report [metobs define $Obs -REPORT $Id $date] {
               $text insert end "---------------------------------------------------------------\n"
               foreach code [metreport define $report -CODE] desc [metreport define $report -DESC] unit [metreport define $report -UNIT]  value [metreport define $report -VALUE] {
                  $text insert end "[format %06i $code] [format %-43s $desc] ([format %-10s $unit]): $value\n"
               }
               $text insert end "\n"
#               metreport free $report
            }
         }
      } else {
         #----- Per element
         set elems [metobs define $Obs -ELEMENT $Id]

         foreach elem $elems {
            $text insert end  "\n[format %06i [metobs table -code $elem]] $elem ([metobs table -unit $elem])\n"
            foreach date [metobs define $Obs -DATE $Id] {
               $text insert end "[clock format $date  -format "%Y%m%d %H:%M" -gmt true] [metobs define $Obs -ELEMENT $Id $elem $date]\n"
            }
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::Find>
# Creation : Fevrier 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Centrer la projection sur la station specifie.
#
# Parametres :
#   <Obs>    : Observation
#   <Id>     : Identificateur de la station
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc NowCaster::Obs::Find { Obs { Id "" } } {
   variable Data

   if { $Id!="" } {
      set Data(Id) $Id
   }
   set Data(Id) [string toupper $Data(Id)]

   set coord [metobs define $Obs -COORD $Data(Id)]
   Viewport::GoTo $Page::Data(Frame) [lindex $coord 0] [lindex $coord 1]
   if { [winfo exists .nowcasterinfo] } {
      set Data(InfoObs) $Obs
      set Data(InfoId)  $Data(Id)
      NowCaster::Obs::Info $Data(InfoObs) $Data(InfoId)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::Draw...>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc NowCaster::Obs::DrawInit  { Frame VP } {
   global GDefs
   variable Data
   variable Lbl

   set Data(X) $Viewport::Map(X)
   set Data(Y) $Viewport::Map(Y)

   if { [llength [set picked [$VP -pick $Data(X) $Data(Y) { metobs }]]] } {
      set Data(InfoObs) [lindex $picked 1]
      set Data(InfoId)  [lindex [metobs define $Data(InfoObs) -ID] [lindex $picked 2]]
   } else {
      set Data(InfoId)  ""
      set Data(InfoObs) ""
   }
   NowCaster::Obs::Info $Data(InfoObs) $Data(InfoId)
}

proc NowCaster::Obs::Draw      { Frame VP } {
}

proc NowCaster::Obs::DrawDone { Frame VP } {
}

proc NowCaster::Obs::MoveInit { Frame VP } {
   variable Data

   set Data(X) $Viewport::Map(X)
   set Data(Y) $Viewport::Map(Y)

   if { [llength [set picked [$VP -pick $Data(X) $Data(Y) { metobs }]]] } {
      set Data(InfoObs) [lindex $picked 1]
      set Data(InfoId)  [lindex [metobs define $Data(InfoObs) -ID] [lindex $picked 2]]
      if { $Data(Flat$Data(InfoObs)) } {
         set Data(X)  $Viewport::Data(X$VP)
         set Data(Y)  $Viewport::Data(Y$VP)
         set Data(X0) 0
         set Data(Y0) 0
      } else {
         set xy [metobs define $Data(InfoObs) -PIXEL $Data(InfoId)]
         set Data(X0) [lindex $xy 0]
         set Data(Y0) [lindex $xy 1]
      }
   } else {
      set Data(InfoId)  ""
      set Data(InfoObs) ""
   }
}

proc NowCaster::Obs::Move { Frame VP } {
   variable Data

   if { $Data(InfoId)!="" && $Data(InfoObs)!="" } {
      set xy [metobs define $Data(InfoObs) -PIXEL $Data(InfoId)]
      metobs define $Data(InfoObs) -PIXEL $Data(InfoId) [expr $Data(X0)+($Viewport::Map(X)-$Data(X))] [expr $Data(Y0)+($Viewport::Map(Y)-$Data(Y))]
      Page::Update $Page::Data(Frame)
   }
}

proc NowCaster::Obs::MoveDone { Frame VP } {
}
