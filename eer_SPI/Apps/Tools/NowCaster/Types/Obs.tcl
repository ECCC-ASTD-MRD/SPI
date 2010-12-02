
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Outils de "NowCasting" pour SPI.
# Fichier  : Obs.tcl
# Creation : Avril 2006 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fonctions de manipulations des observations meteorologiques pour l'outils
#    de NowCasting de SPI.
#
# Remarques :
#
#===============================================================================

namespace eval NowCaster::Obs { } {
   variable Data
   variable Lbl
   variable Error
   variable Msg
   variable Bubble
   variable Param
   variable Tephi

   font create TEPHIFONT  -family arial  -size -10

   set Param(Title)      { "Observation" "Observation" }

   set Param(PathsSurf)  { }
   set Param(PathsUpper) { }

   set Data(Frame)       ""

   set Data(ModelName)   ""
   set Data(Models)      { }
   set Data(Obs)         { }
   set Data(Topo)        ""
   set Data(CurrentObs)  NONE
   set Data(Status)      -1
   set Data(Spacing)     25
   set Data(Flat)        0
   set Data(Elems)       { }
   set Data(Item)        ""
   set Data(InfoObs)     ""
   set Data(InfoTag)     ""
   set Data(InfoId)      ""
   set Data(InfoAll)     False
   set Data(Id)          ""
   set Data(Report)      False

   set Tephi(Dry)   True
   set Tephi(Wet)   True
   set Tephi(Dew)   True
   set Tephi(Wind)  True
   set Tephi(Info)  False

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
   set Lbl(Elem)    { "�lements" "Elements" }
   set Lbl(Report)  { "Rapports" "Reports" }
   set Lbl(Find)    { "Trouver une station" "Find station" }
   set Lbl(Surface) { "Surface" "Surface" }
   set Lbl(Upper)   { "Upper" "Upper" }
   set Lbl(Tephi)   { "Tephigramme" "Tephigram" }
   set Lbl(All)     { "Tous" "All" }
   set Lbl(Current) { "Courant" "Current" }
   set Lbl(Status)  { "Type" "Type" }

   set Msg(Read)   { "Lecture des donn�es d'observations" "Reading observation data" }
   set Msg(Exist)  { "Un model de ce nom existe d�ja, d�sirez vous le remplacer ?"
                     "A model by this name already exists, do you we to replace it ?" }
   set Msg(Del)    { "Voulez-vous vraiment supprimer de mod�le ?"
                     "do you really want to suppress this model ?" }

   set Error(File)  { "Fichier d'observation invalide." "Invalid observation file." }
   set Error(Elems) { "Il n'y a aucun element dans ce fichier." "No element foun in this file." }

   set Bubble(Find)       { "Rechercher un station et centrer la vue sur celle-ci" "Find a station and locate the viewport on it" }
   set Bubble(Mode)       { "Activer le mode de s�lection des observations\n\nBouton gauche: S�lection\nBouton centre: D�placer une localisation" "Activate observation selection mode\n\nLeft button  : Select location\nMiddle button: Move location" }
   set Bubble(Graph)      { "Activer les graphs dans la bulle d'information" "Enable graphs wirhin information bubble" }
   set Bubble(Params)     { "Param�tres d'affichage" "Display parameters" }
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
   set Bubble(Status)     { "S�lection du type/famille d'observation" "Select type/family of observation" }
}

#----------------------------------------------------------------------------
# Nom      : <NowCaster::Obs::Window>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche l'interface de manipulation des observations.
#
# Parametres :
#  <Frame>   : Frame de l'interface
#
# Retour:
#
# Remarques :
#
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
      checkbutton $Frame.head.graph -image BUBBLEGRAPH -relief sunken -bd 1 -overrelief raised -offrelief flat -anchor w -selectcolor $GDefs(ColorLight)\
         -variable Obs::Data(BubbleGraph) -onvalue True -offvalue False -indicatoron False
      button $Frame.head.params -image PARAMS -bd 0 -relief flat -overrelief raised -command { SPI::Params ;  TabFrame::Select .params.tab 2 }
      menubutton $Frame.head.add -image PLUS -bd 0 -relief flat -menu $Frame.head.add.list
      button $Frame.head.del -image DELETE -bd 1 -relief flat -overrelief raised \
         -command "catch { NowCaster::Obs::Delete \[$Frame.select.list get \[$Frame.select.list curselection\]\] }"
      pack $Frame.head.mode $Frame.head.graph $Frame.head.params  -side left
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
            ComboBox::Create $Frame.model.elem.topo.sel NowCaster::Obs::Data(Topo) noedit sorted nodouble -1 { } 2 15 set NowCaster::Obs::Data(Topo\$NowCaster::Obs::Data(CurrentObs)) \[lindex \$NowCaster::Obs::Data(Topo) 0\]\; NowCaster::Obs::ModelApply
            pack $Frame.model.elem.topo.lbl -side left
            pack $Frame.model.elem.topo.sel -side left -fill x -expand True
         pack $Frame.model.elem.topo -side top -fill x -padx 2 -pady 2 -expand True

         frame $Frame.model.elem.status
            label $Frame.model.elem.status.lbl -text [lindex $Lbl(Status) $GDefs(Lang)] -width 11 -anchor w
            spinbox $Frame.model.elem.status.sel -textvariable NowCaster::Obs::Data(Status) -from -1 -to 255 -wrap 1 -bd 1 -bg $GDefs(ColorLight) \
               -command { set NowCaster::Obs::Data(Status$NowCaster::Obs::Data(CurrentObs)) $NowCaster::Obs::Data(Status); NowCaster::Obs::ModelApply }
            pack $Frame.model.elem.status.lbl -side left
            pack $Frame.model.elem.status.sel -side left -fill x -expand True
         pack $Frame.model.elem.status -side top -fill x -padx 2 -pady 2 -expand True
         bind $Frame.model.elem.status.sel <Return>  { set NowCaster::Obs::Data(Status$NowCaster::Obs::Data(CurrentObs)) $NowCaster::Obs::Data(Status); NowCaster::Obs::ModelApply }

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
                  -command "set NowCaster::Obs::Data(Var0) \[NowCaster::Obs::VarSet \[lindex \$NowCaster::Obs::Data(Model.$x.$y) 0\]\]
                            set NowCaster::Obs::Data(Var1) \[NowCaster::Obs::VarSet \[lindex \$NowCaster::Obs::Data(Model.$x.$y) 1\]\]"
               grid $Frame.model.items.s.def$n -row [expr $y+2] -column [expr $x+2] -sticky nsew
               incr n
            }
         }
         $Frame.model.items.s.def12 configure -bg white

         set NowCaster::Obs::Data(Model.0.3) ""
         radiobutton $Frame.model.items.s.def$n -textvariable NowCaster::Obs::Data(Set.0.3) -relief raised -bd 1 -width 5 \
            -variable NowCaster::Obs::Data(Item) -value .0.3  -indicatoron False \
            -command { set NowCaster::Obs::Data(Var0) [NowCaster::Obs::VarSet [lindex $NowCaster::Obs::Data(Model.0.3) 0]
                       set NowCaster::Obs::Data(Var1) [NowCaster::Obs::VarSet [lindex $NowCaster::Obs::Data(Model.0.3) 1] }
         grid $Frame.model.items.s.def$n -row 5 -column 1 -columnspan 3 -sticky nsew

      pack $Frame.model.items.s -side top -padx 5 -pady 5
      pack $Frame.model.items -side top -padx 5 -pady 5 -fill x
   pack $Frame.find $Frame.model -side top -fill x

   bind $Frame.select.list <ButtonRelease-1>  "catch { NowCaster::Obs::ObsSelect \[$Frame.select.list get \[$Frame.select.list curselection\]\] }"

   Bubble::Create $Frame.head.mode   $Bubble(Mode)
   Bubble::Create $Frame.head.graph  $Bubble(Graph)
   Bubble::Create $Frame.head.params $Bubble(Params)
   Bubble::Create $Frame.head.add    $Bubble(Add)
   Bubble::Create $Frame.head.del    $Bubble(Del)
   Bubble::Create $Frame.find        $Bubble(Find)

   Bubble::Create $Frame.model.name.model $Bubble(Model)
   Bubble::Create $Frame.model.name.save  $Bubble(ModelSave)
   Bubble::Create $Frame.model.name.del   $Bubble(ModelDel)
   Bubble::Create $Frame.model.name.new   $Bubble(ModelClear)

   Bubble::Create $Frame.model.elem.var0.sel   $Bubble(Variable0)
   Bubble::Create $Frame.model.elem.var1.sel   $Bubble(Variable1)
   Bubble::Create $Frame.model.elem.topo.sel   $Bubble(Topo)
   Bubble::Create $Frame.model.elem.spc.val    $Bubble(Spacing)
   Bubble::Create $Frame.model.elem.status.sel $Bubble(Status)
   Bubble::Create $Frame.model.items           $Bubble(Grid)

   NowCaster::Obs::ModelLoad
}

proc NowCaster::Obs::VarSet { Var } {

   if { $Var!="" } {
      return "[lindex [metobs table -desc $Var] 0] $Var"
   } else {
      return $Var
   }
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
#   <Sec>    : Date en secondes (0 -> utilise la date globale)
#   <Check>  :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Obs::Now { Sec { Check False } } {
   global GDefs
   variable Data

   foreach obs $Data(Obs) {
      if { [set file [lindex [lsort -dictionary [glob -nocomplain $Data(Path$obs)/*_]] end]]!="" } {
         file stat $file valid

         if { $valid(mtime)>$Data(Valid$obs) } {
            Log::Print INFO "Change in $obs $file"
            NowCaster::Obs::Read $obs $file
         }
         metobs define $obs -VALID $Sec False
      }
   }

   if { $Data(InfoObs)!="" && [winfo exists .nowcasterinfo] } {
      NowCaster::Obs::Info $Data(InfoObs) $Data(InfoId) $Data(InfoTag) $Data(InfoAll)
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
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ReadProcess { Obs } {
   global GDefs
   variable Data
   variable Error

   Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) $Obs

   #----- Define default model
   if { ![llength [set Data(Elems$Obs) [metobs define $Obs -ELEMENT]]] } {
      Dialog::Error .nowcaster $Error(Elems)
   } else {
      if { ![llength $Data(Model$Obs)] } {
         set Data(Model$Obs) [list [list 0 0 [lindex $Data(Elems$Obs) 0]]]
         NowCaster::Obs::ObsSelect $Obs
      }
      NowCaster::SetTimeScale [metobs define $Obs -DATE0] [metobs define $Obs -DATE1]
   }
}

proc NowCaster::Obs::Read { Obs Files } {
   global GDefs
   variable Msg
   variable Data
   variable Error

   if { ![llength $Files] } {
      Dialog::Error .nowcaster $Error(File)
      return
   }

   set NowCaster::Data(Job) [lindex $Msg(Read) $GDefs(Lang)]
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
            thread::send -async $Thread "set NowCaster::Data(Job) \"\[lindex \$NowCaster::Obs::Msg(Read) \$GDefs(Lang)\] $file\""
            if { [catch { metobs read $Obs $file }] } {
               thread::send -async $Thread "Dialog::ErrorListing .nowcaster \$NowCaster::Obs::Error(File) \"$file\n\""
            } else {
               file stat $file valid
               if { $Thread!="0" } {
                  thread::send -async $Thread [list NowCaster::Obs::ReadProcess $Obs]
               }
            }
         }
         thread::send -async $Thread "set NowCaster::Data(Job) \"\""
         thread::release
      }
      thread::wait
    }]

   #----- Start the thread

   thread::send -async $id [list NowCasterObsReader $Obs $Files [thread::id]]
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
   set Data(Status$obs)    $Data(Status)

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
      set Data(InfoTag) ""
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
#-------------------------------------------------------------------------------

proc NowCaster::Obs::PageUpdate { Obs } {

   if { [Viewport::AssignedTo $Obs page vp] } {
      Viewport::LinkDo $vp

      Page::Update        $page
      Page::UpdateCommand $page
   }
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
#-------------------------------------------------------------------------------

proc NowCaster::Obs::Update { { Obs {} } } {
   variable Data

   if { ![llength $Obs] } {
      set Obs $Data(Obs)
   }

   foreach obs $Obs {
      set model [metobs define $obs -MODEL]
      metmodel define $model -items $Data(Model$obs) -spacing $Data(Spacing$obs) -flat $Data(Flat$obs) -topography $Data(Topo$obs)
      metobs define $obs -VALID $NowCaster::Data(Sec) False -PERSISTANCE $NowCaster::Data(Persistance) -STATUS $Data(Status$obs)

      foreach item $Data(Model$obs) {
         set desc [metobs table -desc [lindex $item 2]]
         set var  [lindex $desc 0]
         if { ![dataspec is $var] } {
            dataspec create $var
            dataspec configure $var -desc $var -unit [lindex $desc end] -set 0
         }
         metmodel configure $model [lindex $item 2] -dataspec $var
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
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ObsSelect { Obs } {
   variable Data

   if { [metobs is $Obs] } {
      set Data(CurrentObs) $Obs
      set Data(Elems)      $Data(Elems$Obs)
      set Data(ModelName)  $Data(ModelName$Obs)
      set Data(Status)     $Data(Status$Obs)

      if { [winfo exists .nowcaster] } {
         ComboBox::DelAll  $Data(Frame).model.elem.var0.sel
         ComboBox::DelAll  $Data(Frame).model.elem.var1.sel
         ComboBox::DelAll  $Data(Frame).model.elem.topo.sel
          foreach elem  $Data(Elems$Obs) {
            set info "[lindex [metobs table -desc $elem] 0] $elem"

            ComboBox::Add $Data(Frame).model.elem.var0.sel $info
            ComboBox::Add $Data(Frame).model.elem.var1.sel $info
            ComboBox::Add $Data(Frame).model.elem.topo.sel $info
         }
         ComboBox::Add $Data(Frame).model.elem.var0.sel ""
         ComboBox::Add $Data(Frame).model.elem.var1.sel ""
         ComboBox::Add $Data(Frame).model.elem.topo.sel ""
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
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ModelApply { } {
   variable Data

   set Data(ModelName) ""

   if { $Data(Var0)=="" && $Data(Var1)=="" } {
      set Data(Model$Data(Item)) {}
      set Data(Set$Data(Item))   ""
   } else {
      set Data(Model$Data(Item)) [list [lindex $Data(Var0) end] [lindex $Data(Var1) end]]
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
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ModelSelect { Model { List { } } } {
   variable Data

   if { ![metobs is $Data(CurrentObs)] } {
      return
   }

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
      set Data(Var0) [NowCaster::Obs::VarSet [lindex $Data(Model$Data(Item)) 0]]
      set Data(Var1) [NowCaster::Obs::VarSet [lindex $Data(Model$Data(Item)) 1]]

      if { [set topo [metmodel define [metobs define $Data(CurrentObs) -MODEL] -topography]]!="" } {
         set Data(Topo) "$topo [metobs table -code $topo]"
      } else {
         set Data(Topo) ""
      }
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
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ModelDel { Model } {
   global GDefs
   variable Data
   variable Lbl
   variable Msg

   if { [set idx [lsearch -exact $Data(Models) $Model]]!=-1 } {
      if { ![Dialog::Default $Data(Frame) 200 WARNING $Msg(Del) "\n\n\t$Model" 1 $Lbl(Yes) $Lbl(No)] } {
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
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ModelAdd { Model Params } {
   global GDefs
   variable Data
   variable Lbl
   variable Msg

   if { $Model!= "" } {
      if { [set idx [lsearch -exact $Data(Models) $Model]]!=-1 } {
         if { ![Dialog::Default $Data(Frame) 200 INFO $Msg(Exist) "" 1 $Lbl(Yes) $Lbl(No)] } {
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
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ModelLoad { } {
   global env
   variable Data

   set Data(Models) {}

   if {[file exists $env(HOME)/.spi/ObsModel] } {
      set f [open $env(HOME)/.spi/ObsModel r]
      while { ![eof $f] } {
         gets $f line
         if { [string index $line 0] != "#" && [string length $line] > 0 } {
            set name [lindex $line 0]
            lappend Data(Models) $name
            set Data(Models$name) [lindex $line 1]
            set Data(Param$name)  [lindex $line 2]
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
#-------------------------------------------------------------------------------

proc NowCaster::Obs::ModelSave { } {
   global env
   variable Data

   if {[file exists $env(HOME)/.spi/ObsModel] } {
      file rename -force $env(HOME)/.spi/ObsModel $env(HOME)/.spi/ObsModel.old
   }

   set f [open $env(HOME)/.spi/ObsModel w]
   foreach model $Data(Models) {
      set mparam {}
      set msave $Data(Models$model)

      #----- Convert code to description
      set m 0
      foreach item $Data(Models$model) {

         set desc [metobs table -desc [lindex $item 2]]
         set var  [lindex $desc 0]
         Obs::ParamGet $var
         lappend mparam "dataspec configure \"$var\" -factor $Obs::Param(Factor) -value $Obs::Param(Order) $Obs::Param(Mantisse) -size $Obs::Param(Size)\
            -icon \"$Obs::Param(Icon)\" -color \"$Obs::Param(Color)\" -unit \"$Obs::Param(Unit)\" -width $Obs::Param(Width)\
            -rendervector $Obs::Param(Vector) -rendertexture $Obs::Param(Texture) -rendervolume $Obs::Param(Volume)\
            -rendercoord $Obs::Param(Coord) -rendervalue $Obs::Param(Value) -renderlabel $Obs::Param(Label)\
            -min \"$Obs::Param(Min)\" -max \"$Obs::Param(Max)\" -intervals \"$Obs::Param(Intervals)\" -intervalmode $Obs::Param(IntervalMode) $Obs::Param(IntervalParam)"
         incr m
      }
#      puts $f "\"$model\" { $msave } [metmodel define $model -spacing] [metmodel define $model -flat] [metmodel define $model -topography] { $mparam }"
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
#   <Tag>    : Identificateur unique de la station
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Obs::InfoWindow { { Obs "" } } {
   global GDefs
   variable Lbl

   if { [winfo exists .nowcasterinfo]} {
      raise .nowcasterinfo
   } else {
      toplevel .nowcasterinfo
      wm transient .nowcasterinfo .
      wm geometry .nowcasterinfo 800x600

      TabFrame::Create .nowcasterinfo.tab 1 ""
      set tab [TabFrame::Add .nowcasterinfo.tab 1 [lindex $Lbl(Report) $GDefs(Lang)] True]
      frame ${tab}.bar -relief flat
         radiobutton ${tab}.bar.cur -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False \
            -text [lindex $Lbl(Current) $GDefs(Lang)] -variable NowCaster::Obs::Data(InfoAll) -value False -selectcolor $GDefs(ColorLight) \
            -command { NowCaster::Obs::Info $NowCaster::Obs::Data(InfoObs) $NowCaster::Obs::Data(InfoId) $NowCaster::Obs::Data(InfoTag) $NowCaster::Obs::Data(InfoAll) }
         radiobutton ${tab}.bar.all -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False \
            -text [lindex $Lbl(All) $GDefs(Lang)] -variable NowCaster::Obs::Data(InfoAll) -value True -selectcolor $GDefs(ColorLight) \
            -command { NowCaster::Obs::Info $NowCaster::Obs::Data(InfoObs) $NowCaster::Obs::Data(InfoId) $NowCaster::Obs::Data(InfoTag) $NowCaster::Obs::Data(InfoAll) }
         pack ${tab}.bar.cur ${tab}.bar.all -side left -pady 2 -ipady 2 -ipadx 2
     pack ${tab}.bar -side top -fill x -padx 5 -pady 5

      frame ${tab}.info -relief flat
         text ${tab}.info.text -relief sunken -bd 1 -yscrollcommand "${tab}.info.scrolly set"  -xscrollcommand "${tab}.info.scrollx set" \
           -width 1 -height 1 -bg $GDefs(ColorLight) -wrap none
         scrollbar ${tab}.info.scrolly -relief sunken -command "${tab}.info.text yview" -bd 1 -width 10
         scrollbar ${tab}.info.scrollx -relief sunken -command "${tab}.info.text xview" -bd 1 -width 10 -orient horizontal

         pack ${tab}.info.scrollx -side bottom -fill x -anchor s
         pack ${tab}.info.text -side left -expand true -fill both
         pack ${tab}.info.scrolly -side left -fill y
      pack ${tab}.info -side top -fill both -expand true -padx 5 -pady 5

      set tab [TabFrame::Add .nowcasterinfo.tab 1 [lindex $Lbl(Elem) $GDefs(Lang)] True]
      frame ${tab}.bar -relief flat
         radiobutton ${tab}.bar.cur -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False \
            -text [lindex $Lbl(Current) $GDefs(Lang)] -variable NowCaster::Obs::Data(InfoAll) -value False -selectcolor $GDefs(ColorLight) \
            -command { NowCaster::Obs::Info $NowCaster::Obs::Data(InfoObs) $NowCaster::Obs::Data(InfoId) $NowCaster::Obs::Data(InfoTag) $NowCaster::Obs::Data(InfoAll) }
         radiobutton ${tab}.bar.all -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False \
            -text [lindex $Lbl(All) $GDefs(Lang)] -variable NowCaster::Obs::Data(InfoAll) -value True -selectcolor $GDefs(ColorLight) \
            -command { NowCaster::Obs::Info $NowCaster::Obs::Data(InfoObs) $NowCaster::Obs::Data(InfoId) $NowCaster::Obs::Data(InfoTag) $NowCaster::Obs::Data(InfoAll) }
         pack ${tab}.bar.cur ${tab}.bar.all -side left -pady 2 -ipady 2 -ipadx 2
     pack ${tab}.bar -side top -fill x -padx 5 -pady 5
      frame ${tab}.info -relief flat
         text ${tab}.info.text -relief sunken -bd 1 -yscrollcommand "${tab}.info.scrolly set"  -xscrollcommand "${tab}.info.scrollx set" \
           -width 1 -height 1 -bg $GDefs(ColorLight) -wrap none
         scrollbar ${tab}.info.scrolly -relief sunken -command "${tab}.info.text yview" -bd 1 -width 10
         scrollbar ${tab}.info.scrollx -relief sunken -command "${tab}.info.text xview" -bd 1 -width 10 -orient horizontal

         pack ${tab}.info.scrollx -side bottom -fill x -anchor s
         pack ${tab}.info.text -side left -expand true -fill both
         pack ${tab}.info.scrolly -side left -fill y
      pack ${tab}.info -side top -fill both -expand true -padx 5 -pady 5

      set tab [TabFrame::Add .nowcasterinfo.tab 1 [lindex $Lbl(Tephi) $GDefs(Lang)] True]
      frame ${tab}.bar -relief flat
         checkbutton ${tab}.bar.dry -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False \
            -text "DRY " -variable NowCaster::Obs::Tephi(Dry) -selectcolor $GDefs(ColorLight) -image LINE -compound right \
            -command { NowCaster::Obs::Graph } -onvalue True -offvalue False
         checkbutton ${tab}.bar.wet -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False \
            -text "WET " -variable NowCaster::Obs::Tephi(Wet) -selectcolor $GDefs(ColorLight) -image DASH2 -compound right \
            -command { NowCaster::Obs::Graph } -onvalue True -offvalue False
         checkbutton ${tab}.bar.dew -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False \
            -text "DEW " -variable NowCaster::Obs::Tephi(Dew) -selectcolor $GDefs(ColorLight) -image DASH1 -compound right \
            -command { NowCaster::Obs::Graph } -onvalue True -offvalue False
         checkbutton ${tab}.bar.wind -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False \
            -text "Wind " -variable NowCaster::Obs::Tephi(Wind)  -selectcolor $GDefs(ColorLight) -image BARB -compound right \
            -command { NowCaster::Obs::Graph } -onvalue True -offvalue False
         menubutton ${tab}.bar.info -text "Info:" -relief flat -bd 0 -menu ${tab}.bar.info.menu
         menu ${tab}.bar.info.menu
         foreach info { "False" "pressure" "drybulb" "wetbulb" "dewpoint" "SPREAD" "HEIGHT" "RELATIVEHUMIDITY" } {
            ${tab}.bar.info.menu add command -label "$info" -command "${tab}.bar.info configure -text \"Info: $info\"; set NowCaster::Obs::Tephi(Info) $info; NowCaster::Obs::Graph"
         }
         pack ${tab}.bar.wet ${tab}.bar.dew ${tab}.bar.wind ${tab}.bar.info -side left -pady 2 -ipadx 2
      pack ${tab}.bar -side top -fill x -padx 5 -pady 5
      glcanvas ${tab}.glcanvas -width 0 -height 0 -bg white -relief sunken -bd 1 -highlightthickness 0
      pack ${tab}.glcanvas -side left -fill both -expand true -padx 5 -pady 5

      frame ${tab}.info -relief flat
         text ${tab}.info.text -relief sunken -bd 1 -yscrollcommand "${tab}.info.scrolly set" \
           -width 32 -height 1 -bg $GDefs(ColorLight) -wrap none
         scrollbar ${tab}.info.scrolly -relief sunken -command "${tab}.info.text yview" -bd 1 -width 10
         pack ${tab}.info.text -side left -expand true -fill both
         pack ${tab}.info.scrolly -side left -fill y
      pack ${tab}.info -side right -fill y -padx 5 -pady 5

      ${tab}.glcanvas create graph -x 0 -y 0 -width 1 -height 1 -anchor nw -xlegend 5 -ylegend 5 -command "gr" -legend True \
         -fg black -bg gray75 -fill white -tags "TEPHI" -font XFont12 -title "" -type TEPHI -tag TEPHI
      pack .nowcasterinfo.tab -side top -fill both -expand true -padx 5 -pady 5

      CVMagnifier::Create ${tab}.glcanvas

      bind ${tab}.glcanvas <Configure>       "update idletasks; ${tab}.glcanvas itemconfigure TEPHI -width \[winfo width ${tab}.glcanvas\] -height \[winfo height ${tab}.glcanvas\];"

      #----- Creation des unite de l'echelle

      if { ![graphaxis is TEPHIAXIST] } {
         graphaxis create TEPHIAXIST
         graphaxis create TEPHIAXISTH
         graphaxis create TEPHIAXISP
         graphaxis create TEPHIAXISMIX

         graphaxis configure TEPHIAXIST  -font TEPHIFONT -color black -gridcolor black -gridwidth 1 -position LL -width 1 -unit T \
            -min -70 -max 72 -increment 10 -intervals { -60 -50 -40 -30 -20 -10 0 10 20 30 40 50 60 70 } -highlightwidth 2 -highlight { 0 } -format INTEGER
         graphaxis configure TEPHIAXISP  -font TEPHIFONT -color black -gridcolor black -gridwidth 1 -position LL -width 1 -unit P \
            -min 1050 -max 10  -increment 50  -intervals { 1050 1000 950 900 850 800 750 700 650 600 550 500 450 400 350 300 250 200 150 100 50 40 30 20 } \
            -highlightwidth 2 -highlight { 1000 850 700 500 250 }
         graphaxis configure TEPHIAXISTH -font TEPHIFONT -color black -gridcolor black -gridwidth 1  -position LL -width 1 -unit TH \
            -min 220 -max 520 -increment 10 -format INTEGER -type LN -highlightwidth 2 -highlight { 300 }
         graphaxis configure TEPHIAXISMIX -font TEPHIFONT -color black -gridcolor gray75 -gridwidth 1 -position LL -width 2 -unit MIX \
            -min 0.02 -max 50 -increment 10 -format FIT \
            -intervals { 0.02 0.05 0.15 0.3 0.6 1.0 1.5 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 12.0 14.0 16.0 18.0 20.0 25.0 30.0 35.0 40.0 50.0 } \
            -labels { 0.02 0.05 0.15 0.3 0.6 1 1.5 2 3 4 5 6 7 8 9 10 12 14 16 18 20 25 30 35 40 50 }
      }

      TabFrame::Select .nowcasterinfo.tab 0
    }

    wm title .nowcasterinfo "[lindex $Lbl(Info) $GDefs(Lang)]: $Obs"
}

proc NowCaster::Obs::Info { Obs Id Tag { All False } } {
   global GDefs
   variable Data
   variable Lbl

   if { [metobs is $Obs] } {
      NowCaster::Obs::InfoWindow "$Obs (Station:$Id)"

      set datev [metobs define $Obs -VALID]

      if { $All } {
         set dates [metobs define $Obs -DATE $Tag]
      } else {
         set dates $datev
      }

      #----- Per report
      .nowcasterinfo.tab.frame0.info.text delete 0.0 end
      foreach date $dates {
         .nowcasterinfo.tab.frame0.info.text insert end "[clock format $date  -format "%Y%m%d %H:%M" -gmt true]\n"
         foreach report [metobs define $Obs -REPORT $Tag $date] {
            .nowcasterinfo.tab.frame0.info.text insert end "---------------------------------------------------------------\n"
            .nowcasterinfo.tab.frame0.info.text insert end "Status: [metreport define $report -STATUS]\n"
            foreach code [metreport define $report -CODE] desc [metreport define $report -DESC] unit [metreport define $report -UNIT]  value [metreport define $report -VALUE] {
               .nowcasterinfo.tab.frame0.info.text insert end "[format %06i $code] [format %-43s $desc] ([format %-10s $unit]): $value\n"
            }
#         metreport free $report
         }
         .nowcasterinfo.tab.frame0.info.text insert end "\n"
      }

      #----- Per element
      .nowcasterinfo.tab.frame1.info.text delete 0.0 end
      set elems [metobs define $Obs -ELEMENT $Tag]
      foreach elem $elems {
         set info [metobs table -desc $elem]
         .nowcasterinfo.tab.frame1.info.text insert end  "[format %06i $elem] [lindex $info 0] ([lindex $info 1])\n"
         .nowcasterinfo.tab.frame1.info.text insert end "---------------------------------------------------------------\n"
         foreach date $dates {
            if  { [set val [metobs define $Obs -ELEMENT $Tag $elem $date]]!="" } {
               .nowcasterinfo.tab.frame1.info.text insert end "[clock format $date  -format "%Y%m%d %H:%M" -gmt true] $val\n"
            }
         }
         .nowcasterinfo.tab.frame1.info.text insert end  "\n"
      }

      #----- Get Tephi temp profile
      vector free TEPHIPROF
      vector create TEPHIPROF
      vector dim TEPHIPROF { PRES DRY WET DEW  }
      vector stats TEPHIPROF -nodata -999.0

      foreach report [metobs define $Obs -REPORT $Tag $datev] {
         foreach pres [metreport define $report -ELEMENT 007004] temp [metreport define $report -ELEMENT { 012001 012101 }] wet [metreport define $report -ELEMENT 012102] dew [metreport define $report -ELEMENT { 012192 }] {
            if { $pres!=-999.0 && $pres!="" && $temp!=-999.0 } {
               catch { vector append TEPHIPROF  [list [expr $pres/100.0] [expr $temp-273.15] -999 [expr $dew!=-999.0?($temp-$dew-273.15):-999]] }
            }
         }
      }
      vector sort -unique TEPHIPROF PRES

      #----- Get Tephi wind profile
      vector free TEPHIWIND
      vector create TEPHIWIND
      vector dim TEPHIWIND { PRES SPD DIR  }
      foreach report [metobs define $Obs -REPORT $Tag $datev] {
         foreach pres [metreport define $report -ELEMENT 007004] spd [metreport define $report -ELEMENT 011002] dir [metreport define $report -ELEMENT 011001] {
            if { $pres!=-999.0 && $spd!=-999.0 && $dir!=-999.0 } {
               catch { vector append TEPHIWIND [list [expr $pres/100.0] [expr $spd*1.94384617179] $dir] }
            }
         }
      }
      vector sort -unique TEPHIWIND PRES

      #----- Display graph data
      if { ![graphitem is TEPHIITEM] } {
         graphitem create TEPHIITEM
      }
      graphitem configure TEPHIITEM -desc "Station $Id"
      NowCaster::Obs::Graph

      #----- Display text data
      .nowcasterinfo.tab.frame2.info.text delete 0.0 end
      .nowcasterinfo.tab.frame2.info.text insert end "Pres(mb) Dry(�C) Wet(�C) Dew(�C)\n"
      foreach pres [vector get TEPHIPROF.PRES] dry [vector get TEPHIPROF.DRY] wet [vector get TEPHIPROF.WET] dew [vector get TEPHIPROF.DEW] {
         .nowcasterinfo.tab.frame2.info.text insert end [format "%6.1f   " $pres]
         if { [catch { .nowcasterinfo.tab.frame2.info.text insert end [format "%5.1f   " $dry] } ] } {
            .nowcasterinfo.tab.frame2.info.text insert end "        "
         }
         if { [catch { .nowcasterinfo.tab.frame2.info.text insert end [format "%5.1f   " $dry] }] } {
            .nowcasterinfo.tab.frame2.info.text insert end "        "
         }
         if { [catch { .nowcasterinfo.tab.frame2.info.text insert end [format "%5.1f\n" $dew] }] } {
            .nowcasterinfo.tab.frame2.info.text insert end "\n"
         }
      }

      .nowcasterinfo.tab.frame2.info.text insert end "\nPres(mb) Speed(Kt) Dir(Deg)\n"
      foreach pres [vector get TEPHIWIND.PRES] spd [vector get TEPHIWIND.SPD] dir [vector get TEPHIWIND.DIR] {
         catch { .nowcasterinfo.tab.frame2.info.text insert end [format "%6.1f   %5.1f     %5.1f\n" $pres $spd $dir] }
      }
   }
}

proc NowCaster::Obs::Graph { } {
   variable Tephi

   graphitem configure TEPHIITEM -paxis TEPHIAXISP -taxis TEPHIAXIST -thaxis TEPHIAXISTH -mixaxis TEPHIAXISMIX \
      -pressure TEPHIPROF.PRES -drybulb TEPHIPROF.DRY -wetbulb [expr $Tephi(Wet)?"TEPHIPROF.WET":""] \
      -dewpoint [expr $Tephi(Dew)?"TEPHIPROF.DEW":""] -windpres TEPHIWIND.PRES \
      -speed [expr $Tephi(Wind)?"TEPHIWIND.SPD":""] -dir [expr $Tephi(Wind)?"TEPHIWIND.DIR":""] \
      -type LINE -width 2 -outline blue -value $Tephi(Info) -font XFont12 -anchor w -size 15

   .nowcasterinfo.tab.frame2.glcanvas itemconfigure TEPHI -item { TEPHIITEM }
   update idletasks
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
#-------------------------------------------------------------------------------

proc NowCaster::Obs::Find { Obs { Id "" } } {
   variable Data

   if { $Id!="" } {
      set Data(Id) $Id
   }
   set Data(Id) [string toupper $Data(Id)]

   if { [winfo exists .nowcasterinfo] } {
      set Data(InfoObs) $Obs
      set Data(InfoId)  $Data(Id)
      set Data(InfoTag) $Data(Id)
      NowCaster::Obs::Info $Data(InfoObs) $Data(InfoId) $Data(InfoTag) $Data(InfoAll)
   }

   set coord [metobs define $Obs -COORD $Data(Id)]
   Viewport::GoTo $Page::Data(Frame) [lindex $coord 0] [lindex $coord 1]
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
#-------------------------------------------------------------------------------

proc NowCaster::Obs::DrawInit  { Frame VP } {
   global GDefs
   variable Data
   variable Lbl

   set Data(X) $Viewport::Map(X)
   set Data(Y) $Viewport::Map(Y)

   if { [llength [set picked [$VP -pick $Data(X) $Data(Y) { metobs }]]] } {
      set Data(InfoObs) [lindex $picked 1]
      set Data(InfoTag) [lindex $picked 2]
      set Data(InfoId)  [metobs define $Data(InfoObs) -ID $Data(InfoTag)]
   } else {
      set Data(InfoId)  ""
      set Data(InfoTag) ""
      set Data(InfoObs) ""
   }
   NowCaster::Obs::Info $Data(InfoObs) $Data(InfoId) $Data(InfoTag) $Data(InfoAll)
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
      set Data(InfoTag) [lindex $picked 2]
      set Data(InfoId)  [metobs define $Data(InfoObs) -ID $Data(InfoTag)]
      if { $Data(Flat$Data(InfoObs)) } {
         set Data(X)  $Viewport::Data(X$VP)
         set Data(Y)  $Viewport::Data(Y$VP)
         set Data(X0) 0
         set Data(Y0) 0
      } else {
         set xy [metobs define $Data(InfoObs) -PIXEL $Data(InfoTag)]
         set Data(X0) [lindex $xy 0]
         set Data(Y0) [lindex $xy 1]
      }
   } else {
      set Data(InfoId)  ""
      set Data(InfoTag) ""
      set Data(InfoObs) ""
   }
}

proc NowCaster::Obs::Move { Frame VP } {
   variable Data

   if { $Data(InfoId)!="" && $Data(InfoObs)!="" } {
      set xy [metobs define $Data(InfoObs) -PIXEL $Data(InfoTag)]
      metobs define $Data(InfoObs) -PIXEL $Data(InfoTag) [expr $Data(X0)+($Viewport::Map(X)-$Data(X))] [expr $Data(Y0)+($Viewport::Map(Y)-$Data(Y))]
      Page::Update $Page::Data(Frame)
   }
}

proc NowCaster::Obs::MoveDone { Frame VP } {
}
