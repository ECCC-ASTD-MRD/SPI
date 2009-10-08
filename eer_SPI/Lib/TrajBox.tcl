#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : TrajBox.tcl
# Creation : Decembre 2000 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Permet d'afficher un selecteur de trajectoire.
#
# Fonctions:
#
#   TrajBox::Clear         { }
#   TrajBox::Close         { No }
#   TrajBox::CloseAll      { }
#   TrajBox::Create        { Parent Title { Geom "" } }
#   TrajBox::Exist         { No }
#   TrajBox::FileClose     { No }
#   TrajBox::FileOpen      { No File }
#   TrajBox::FileSelect    { No File FID }
#   TrajBox::Get           { }
#   TrajBox::GetContent    { { Id "" } }
#   TrajBox::GetFile       { args }
#   TrajBox::GetSelected   { args }
#   TrajBox::InfoCommand   { No Index }
#   TrajBox::Init          { No }
#   TrajBox::Insert        { No }
#   TrajBox::Raise         { }
#   TrajBox::Restrict      { No }
#   TrajBox::RestrictClear { No }
#   TrajBox::RestrictSet   { No Var Value }
#   TrajBox::Scroll        { No Incr }
#   TrajBox::Select        { }
#
# Remarques :
#
#===============================================================================

package provide TrajBox 1.2

catch { SPI::Splash "Loading Widget Package TrajBox 1.2" }

namespace eval TrajBox {
   variable Lbl
   variable Bubble
   variable Error
   variable Data
   variable Param

   set Param(Geom)     { 415x200+[winfo rootx .]+[winfo rooty .] }
   set Param(Title)    { "TrajBox" "TrajBox" }

   #----- Variables de suivit du contenu

   set Data(Current)  ""               ;#Numero de boite ayant le focus
   set Data(BoxList)  ""               ;#Liste des numeros de boite TrajBox

   #----- Textes et labels

   set Lbl(AllFiles)      { "Tous les fichiers" "Every files" }
   set Lbl(Bubble)        { "Bulles d'information" "Info bubbles" }
   set Lbl(SelectAll)     { "Selectionner tout" "Select all" }
   set Lbl(SelectClear)   { "Annuler la selection" "Clear selection" }
   set Lbl(Close)         { "Fermer fichier(s)" "Closefile(s)" }
   set Lbl(NewBox)        { "Nouvelle boite" "New box" }
   set Lbl(CloseBox)      { "Fermer cette boite" "Close box" }
   set Lbl(NoFiles)       { "Aucun fichier" "No files" }
   set Lbl(Open)          { "Ouvrir fichier" "Open file" }

   #----- Erreurs

   set Error(Empty)       { "Ce fichier ne contient pas de trajectoire."
                            "There is no trajectories in this file." }
   #----Bulles d'aides

   set Bubble(Files)      { "Selection des fichiers de trajectoires" "Trajectory file selection" }
   set Bubble(File)       { "Fichier(s) de trajectoire courant" "Current trajectory file(s)" }
   set Bubble(Nb)         { "Nombre de trajectoires affichees" "Number of displayed trajectories" }
   set Bubble(Refresh)    { "Mettre a jour l'affichage" "Update display" }
   set Bubble(Restrict)   { "Annuler les criteres de restrictions" "Clear the restriction criteria" }
   set Bubble(Select)     { "Criteres de restriction\nde selection des champs" "Field restriction criteria" }
}

#----------------------------------------------------------------------------
# Nom      : <TrajBox::Clear>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Deselectioner tout.
#
# Parametres   :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TrajBox::Clear { } {
   variable Data

   foreach box $Data(BoxList) {
      .trajbox$box.data.list selection clear 0 end
   }

   set Trajectory::Data(List) ""
}

#----------------------------------------------------------------------------
# Nom      : <TrajBox::Create>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une boite permettant de selectionner des trajectoires.
#
# Parametres   :
#   <Parent>   : Nom du widget
#   <Title>    : Titre des donnees
#
# Retour:
#   <No>       : Numero de la boite
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TrajBox::Create { Parent Title { Geom "" } } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data
   variable Param

   for { set no 0 } { $no < 20 } { incr no } {
      if { [lsearch -exact $Data(BoxList) $no] == -1 } {
         lappend Data(BoxList) $no
         break
      }
   }

   set id  .trajbox$no
   set spc Data$no

   TrajBox::Init $no

   toplevel     $id
   wm title     $id "[lindex $Param(Title) $GDefs(Lang)] ($Title)"
   wm transient $id .
   wm resizable $id False True
   wm maxsize   $id 415 10000
   wm minsize   $id 415 300
   wm protocol  $id WM_DELETE_WINDOW "TrajBox::Close $no"

   #----- Afficher la fenetre par dessus la derniere

   if { $Geom!="" } {
      wm geom   $id $Geom
   } else {
      if { [llength $Data(BoxList)] == 1 } {
         eval wm geom $id $Param(Geom)
      } else {
         set previd  .trajbox[lindex $Data(BoxList) [expr [llength $Data(BoxList)]-2]]
         wm geom   $id +[winfo rootx $previd]+[winfo rooty $previd]
      }
   }

   #----- Initialiser le resultat

   frame $id.header
      SelectBox::Create $id.header.id "IDENTIFICATION" \
         TrajBox::${spc}::Data(Id) "" 30 10 TrajBox::Restrict $no
      SelectBox::Create $id.header.level "LEVEL" \
         TrajBox::${spc}::Data(Level) "" 8 10 TrajBox::Restrict $no
      SelectBox::Create $id.header.mode "MODE" \
         TrajBox::${spc}::Data(Mode) "" 5 10 TrajBox::Restrict $no
      SelectBox::Create $id.header.date "DATE" \
         TrajBox::${spc}::Data(Date) "" 11 10 TrajBox::Restrict $no
      button $id.header.info -bitmap "@$GDefs(Dir)/Resources/Bitmap/CLEAR.xbm" -relief raised \
         -bd 1 -command "TrajBox::RestrictClear $no"
      pack $id.header.id $id.header.level $id.header.mode $id.header.date -side left -ipadx 3 -fill y
      pack $id.header.info -side left -fill both -expand true
   pack $id.header -side top -fill x

   #----- Creer la listbox concernant les champs.

   frame $id.data -relief raised -bd 1
      listbox $id.data.list -yscrollcommand [list $id.data.scrolly set] -xscrollcommand "$id.data.list xview 0;catch " \
         -height 5  -bd 1 -relief sunken -bg $GDefs(ColorLight) -exportselection 0 -selectmode extended -width 49
      scrollbar $id.data.scrolly -orient vertical -command [list $id.data.list yview] -bd 1 -width 10
      pack $id.data.list -side left -fill both -expand true
      pack $id.data.scrolly -side right -fill y
   pack $id.data -side top -fill both -expand true

   frame $id.info
      button $id.info.refresh -image DOCSEL -command "TrajBox::Select" -bd 1
      menubutton $id.info.file -menu $id.info.file.menu -bd 1 -relief raised -image OPEN
      menubutton $id.info.name -textvariable TrajBox::${spc}::Data(FileInfo) -anchor w -bg $GDefs(ColorLight) \
          -relief raised -bd 1 -width 20 -menu $id.info.name.list
      label $id.info.lbl -textvariable TrajBox::${spc}::Data(ShowRead) -bg $GDefs(ColorLight) \
          -relief raised -bd 1
      pack $id.info.refresh $id.info.file -side left -fill both
      pack $id.info.name -side left -fill both -expand true
      pack $id.info.lbl -side left -fill both -ipadx 2
   pack $id.info -side top -fill x

   menu $id.info.file.menu
      $id.info.file.menu add command -label "[lindex $Lbl(Open) $GDefs(Lang)] ..."  -image OPEN -compound left \
         -command "TrajBox::FileOpen $no \[FileBox::Create $id \"\" Load \[list \$FileBox::Type(TRAJ)\]\]"
      $id.info.file.menu add command -label [lindex $Lbl(Close) $GDefs(Lang)] -image FOLD -compound left \
         -command "TrajBox::FileClose $no"
      $id.info.file.menu add separator
      $id.info.file.menu add command -label [lindex $Lbl(NewBox) $GDefs(Lang)] -image FRAME0 -compound left \
         -command "TrajBox::Create $Parent Clone"
      $id.info.file.menu add command -label [lindex $Lbl(CloseBox) $GDefs(Lang)] -image FRAMEDEL -compound left \
         -command "TrajBox::Close $no"

   menu $id.info.name.list -bg $GDefs(ColorLight)
      $id.info.name.list add command -label [lindex $Lbl(AllFiles) $GDefs(Lang)] \
         -command "set TrajBox::${spc}::Data(FileCurrent) -1 ; set TrajBox::${spc}::Data(FileInfo) \"[lindex $Lbl(AllFiles) $GDefs(Lang)]\" ; TrajBox::Restrict $no"

   set TrajBox::${spc}::Data(FileInfo) [lindex $Lbl(NoFiles) $GDefs(Lang)]

   #----- Creer les bulles d'info

   ListboxBubble::Create $id.data.list 0 TrajBox::InfoCommand $no

   #----- Permettre la selection d'un champs dans la liste.

   bind $id.data.list <Double-ButtonRelease-1> "set TrajBox::Data(Current) $no ; TrajBox::Select"
   bind $id.data.list <ButtonRelease-1>        "set TrajBox::Data(Current) $no"
   bind $id.data.list <ButtonPress-3>          "set TrajBox::Data(Current) $no ; tk_popup .trajmenu %X %Y"

   bind $id <Key-Up>                           "TrajBox::Scroll $no -1"
   bind $id <Key-Down>                         "TrajBox::Scroll $no  1"

   Bubble::Create $id.info         [lindex $Bubble(Files)    $GDefs(Lang)]
   Bubble::Create $id.info.name    [lindex $Bubble(File)     $GDefs(Lang)]
   Bubble::Create $id.info.lbl     [lindex $Bubble(Nb)       $GDefs(Lang)]
   Bubble::Create $id.header       [lindex $Bubble(Select)   $GDefs(Lang)]
   Bubble::Create $id.header.info  [lindex $Bubble(Restrict) $GDefs(Lang)]
   Bubble::Create $id.info.refresh [lindex $Bubble(Refresh)  $GDefs(Lang)]

   #----- Creer le menu contextuel

   if { ![winfo exist .trajmenu] } {

      menu .trajmenu
         .trajmenu add command -label [lindex $Lbl(SelectClear) $GDefs(Lang)] \
            -command ".trajbox\$TrajBox::Data(Current).data.list selection clear 0 end"
         .trajmenu add command -label [lindex $Lbl(SelectAll) $GDefs(Lang)] \
            -command ".trajbox\$TrajBox::Data(Current).data.list selection set 0 end"
         .trajmenu add separator
         .trajmenu add checkbutton -label [lindex $Lbl(Bubble) $GDefs(Lang)] \
            -variable ListboxBubble::Data(State) -command "ListboxBubble::Activate"
   }
   set TrajBox::Data(Current) $no
   return $no
}

#----------------------------------------------------------------------------
# Nom      : <TrajBox::Exist>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Verifie que la boite specifie existe.
#
# Parametres   :
#   <No>       : Numero de la boite
#
# Retour:
#   <Exist>    : Existe ou non
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TrajBox::Exist { No } {

   return [winfo exist .trajbox$No]
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::FileClose>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Fermeture d'un fichier de trajectoire.
#
# Parametres :
#  <No>      : Numero de TrajBox
#
# Retour    :
#
# Remarque :
#    -On verifie la variable Data(FileCurrent) :
#       -1 : On ferme tout les fichiers
#        n : On ferme le fichier correspondant a cet index
#
#-------------------------------------------------------------------------------

proc TrajBox::FileClose { No } {
   variable Data
   variable Lbl
   global GDefs

   #----- On recupere les variable du module courant

   upvar #0 TrajBox::Data${No}::Data data

   #----- Determiner les fichiers a fermer

   if { $data(FileCurrent)=="-1" } {
      set closefile $data(FileList)
      set data(FileCurrent) ""
   } else {
      set closefile $data(FileCurrent)
      set data(FileCurrent) -1
   }

   foreach file $closefile {

      #----- Determiner l'index du fichier a fermer

      set idx [lsearch -exact $data(FileList) $file]

      #----- Fermer le fichier

      set data(FileList) [lreplace $data(FileList) $idx $idx]

      #----- Liberer les trajectoires

      foreach traj [lindex $data(TrajList) $idx] {
         set vp [lindex [trajectory stats $traj -tag] 1]
         if { $vp!="" && [Page::Registered All Viewport $vp]!=-1 } {
            set ido [lsearch -exact $Viewport::Data(Data$vp) $traj]
            set Viewport::Data(Data$vp) [lreplace $Viewport::Data(Data$vp) $ido $ido]
         }
         set i [lsearch -exact $Trajectory::Data(List) $traj]
         set Trajectory::Data(List) [lreplace $Trajectory::Data(List) $i $i]

         trajectory free $traj
      }

      set data(TrajList) [lreplace $data(TrajList) $idx $idx]

      .trajbox${No}.info.name.list delete [expr 1 + $idx]
   }

   if { [llength $data(FileList)] == 0 } {
      set data(FileInfo) [lindex $Lbl(NoFiles) $GDefs(Lang)]
   } elseif { [llength $data(FileList)] == 1 } {
      set data(FileInfo) "...[string range $data(FileList) end-46 end]"
      set data(FileCurrent) $data(FileList)
   } else {
      set data(FileInfo) [lindex $Lbl(AllFiles) $GDefs(Lang)]
      set data(FileCurrent) -1
   }
   TrajBox::Insert $No
   TrajBox::Select
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::FileOpen>
# Creation : Juillet 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Ouverture d'un fichier de trajectoires.
#
# Parametres :
#  <No>      : Numero de TrajBox
#  <File>    : Path du fichier
#
# Retour    :
#
# Remarque :
#     - Si No < 0 l'ouverture se fera dans la derniere boite
#
#-------------------------------------------------------------------------------

proc TrajBox::FileOpen { No File } {
   variable Data
   variable Lbl
   variable Error
   global GDefs

   if { $File == "" } {
      return
   }

   #----- Selectionner la derniere boite

   if { $No < 0 } {
      set No [lindex Data(BoxList) end]
   }

   #----- On recupere les variable du module courant

   upvar #0 TrajBox::Data${No}::Data data

   foreach file $File {

      #----- Si le fichier est deja ouvert

      if { [lsearch -exact $data(FileList) $file] != -1 } {
         continue
      }

      #----- Ouvrir le fichier

      set ltraj [trajectory load $file]

      if { ![llength $ltraj] } {
         Dialog::CreateError .trajbox$No $Error(Empty) "\n\n$file"
      } else {

         lappend data(FileList) $file
         lappend data(TrajList) $ltraj

         .trajbox$No.info.name.list add command -label "$file" \
            -command "TrajBox::FileSelect $No $file"

         if { [llength $data(FileList)] == 1 } {
            set data(FileInfo) "...[string range $file end-46 end]"
            set data(FileCurrent) $file
         } else {
           set data(FileInfo) [lindex $Lbl(AllFiles) $GDefs(Lang)]
           set data(FileCurrent) -1
         }
      }
   }
   TrajBox::Insert $No
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::Get>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la liste de toute les boites
#
# Parametres :
#
# Retour     :
#   <list>   : Liste des boites de champs.
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc TrajBox::Get { } {
   variable Data

   return $Data(BoxList)
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::GetContent>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la liste de toutes les trajectoires de toutes les boites.
#
# Parametres :
#   <Id>     : No de la boite (""=courante, -1=toute, 0...n=numero)
#
# Retour   :
#   <Traj> : Liste des parametres de tous les champs.
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc TrajBox::GetContent { { Id "" } } {
   variable Data

   #----- Boite courante

   if { $Id=="" } {
      set Id $TrajBox::Data(Current)
   }

   #----- Numero de boite

   if { $Id!=-1 } {
      upvar #0 TrajBox::Data${Id}::Data data
      return [join $data(TrajList)]
   }

   set trajs {}

   foreach box $Data(BoxList) {

      upvar #0 TrajBox::Data${box}::Data data

      if { $trajs != "" } {
         set trajs [list $trajs]
      }

      eval set trajs \[concat $trajs $data(TrajList)\]
   }
   return $trajs
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::GetFile>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la liste de tout les fichiers de toutes les boites.
#
# Parametres :
#   <args>   : No de la boite (""=courante, -1=toute, 0...n=numero)
#
# Retour     :
#   <Fields> : Liste des parametres de tous les champs.
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc TrajBox::GetFile { args } {
   variable Data

   #----- Boite courante

   if { $args=="" } {
      set args $TrajBox::Data(Current)
   }

   #----- Numero de boite

   if { $args!=-1 } {
      upvar #0 TrajBox::Data${args}::Data data
      return $data(FileList)
   }

   #----- Toute les boites

   set files {}

   foreach box $Data(BoxList) {

      upvar #0 TrajBox::Data${box}::Data data

      if { $files!="" } {
         set files [list $fields]
      }
      eval set files \[concat $fields $data(FileList)\]
   }
   return $files
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::GetSelected>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la liste de toutes les trajectoires selectionnees parmi
#            toutes les boites.
#
# Parametres :
#   <args>   : No de la boite (""=courante, -1=toute, 0...n=numero)
#
# Retour     :
#   <Trajs>  : Liste des parametres de toutes les trajectoires selectionnees
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc TrajBox::GetSelected { args } {
   variable Data

   set trajs {}

   #----- Boite courante

   if { $args=="" } {
      foreach idx [.trajbox$Data(Current).data.list curselection] {
         lappend trajs [.trajbox$Data(Current).data.list get $idx]
      }
      return $trajs
   }

   #----- Numero de boite

   if { $args!=-1 } {
      foreach idx [.trajbox${args}.data.list curselection] {
         lappend trajs [.trajbox${args}.data.list get $idx]
      }
      return $trajs
   }

   #----- Toute les boites

   foreach box $TrajBox::Data(BoxList) {
      foreach idx [.trajbox$box.data.list curselection] {
         lappend trajs [.trajbox$box.data.list get $idx]
      }
   }
   return $trajs
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::FileSelect>
# Creation : Juillet 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Fermeture d'un fichier standard.
#
# Parametres :
#  <No>      : Numero de TrajBox
#  <File>    : Path complet du fichier
#  <FID>     : Numero du fichier
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc TrajBox::FileSelect { No File } {

   #----- On recupere les variable du module courant

   upvar #0 TrajBox::Data${No}::Data data

   set data(FileCurrent) $File
   set data(FileInfo) "...[string range $File end-46 end]"
   TrajBox::Restrict $No
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::InfoCommand>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Execute la commande de recherche de l'information sur la trajectoire.
#
# Parametres :
#  <No>      : Numero de TrajBox
#  <Index>   : Index de l'item selectionne dans la liste.
#
# Remarques :
#    -Cette commande est appelee par le package ListboxBubble.
#
#-------------------------------------------------------------------------------

proc TrajBox::InfoCommand { No Index } {
   variable Data
   global GDefs

   set id   [lindex [.trajbox$No.data.list get $Index] end]
   set info [trajectory define $id -PARCEL 0]
   set lbl  "Coord  : [lindex $info 1] [lindex $info 2]"

   return "$lbl"
}

#----------------------------------------------------------------------------
# Nom      : <TrajBox::Init>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise les variable d'une nouvelle boite TrajBox.
#
# Parametres   :
#   <No>       : Numero de la boite
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TrajBox::Init { No } {

   namespace eval Data$No {
      variable Data
      variable Sel

      #----- Variables relatives champs

      set Data(Read)     0      ;#Nombre de trajectoire lu
      set Data(Show)     0      ;#Nombre de trajectoire affichees
      set Data(ShowRead) 0      ;#Nombre de trajectoires affichees sur le nombre de lu

      set Data(TrajList)    ""  ;#Liste des trajectoires
      set Data(FileCurrent) ""  ;#Fichier actif
      set Data(FileInfo)    ""  ;#Nom du fichier affiche
      set Data(FileList)    ""  ;#Liste des fichiers ouverts

      set Data(Id)     ""
      set Data(Level)  ""
      set Data(Mode)   ""
      set Data(Date)   ""
   }
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::Insert>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Insere les champs dans la liste.
#
# Parametres :
#  <No>      : Numero de TrajBox
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc TrajBox::Insert { No } {
   variable Data

   #----- On recupere les variable du module courant

   upvar #0 TrajBox::Data${No}::Data data

   .trajbox$No.data.list delete 0 end
   .trajbox$No config -cursor watch
   update idletasks

   set data(Read) 0
   set data(Id)     ""
   set data(Mode)   ""
   set data(Level)  ""
   set data(Date)   ""

   foreach file $data(TrajList) {
      foreach traj $file {
         #----- Extraire les informations

         set id [trajectory define $traj -ID]
         if { [string length $id] > 30 } {
            set id "[string range $id 0 27]..."
         }

         if { [trajectory define $traj -BACKWARD] } {
            set mode Retro
         } else {
            set mode Traj
         }
         set date [clock format [trajectory define $traj -DATE] -format "%Y%m%d%H%M" -gmt True]
         set level [format "%.2f" [trajectory define $traj -LEVEL]]

         #----- Garder la liste des champs de selection

         if { [lsearch -exact $data(Id) $id] == -1 } {
            lappend data(Id) $id
         }
         if { [lsearch -exact $data(Date) $date] == -1 } {
            lappend data(Date) $date
         }
         if { [lsearch -exact $data(Mode) $mode] == -1 } {
            lappend data(Mode) $mode
         }

         if { [lsearch -exact $data(Level) $level] == -1 } {
            lappend data(Level) $level
         }
         #----- Inserer les donnees dans la liste

         set line [format "%-30s %9.2f %-5s %12s %s" $id $level $mode $date $traj]
         .trajbox$No.data.list insert end $line
         incr data(Read)
      }
   }

   set data(Show)     "$data(Read)"
   set data(ShowRead) "$data(Show)/$data(Read)"

   #----- Update des listes de selections

   SelectBox::Insert .trajbox$No.header.id    $data(Id)
   SelectBox::Insert .trajbox$No.header.mode  $data(Mode)
   SelectBox::Insert .trajbox$No.header.level $data(Level)
   SelectBox::Insert .trajbox$No.header.date  $data(Date)

   .trajbox$No config -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::Close>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Ferme tout les fichiers ouvert et detruit la fenetre.
#
# Parametres :
#  <No>      : Numero de TrajBox
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc TrajBox::Close { No } {
   variable Data

   if { ![winfo exists .trajbox${No}] } {
      return
   }

   #----- Fermer tout les fichiers

   set TrajBox::Data${No}::Data(FileCurrent) -1
   TrajBox::FileClose $No

   #----- Annuler toute selection

   .trajbox${No}.data.list selection clear 0 end

   #----- Detruire l'interface

   SelectBox::Destroy .trajbox${No}.header.id
   SelectBox::Destroy .trajbox${No}.header.mode
   SelectBox::Destroy .trajbox${No}.header.date
   SelectBox::Destroy .trajbox${No}.header.level

   ListboxBubble::Destroy .trajbox${No}.data.list
   destroy .trajbox${No}

   #----- Cleanup des variables

   set idx [lsearch -exact $Data(BoxList) $No]
   set Data(BoxList) [lreplace $Data(BoxList) $idx $idx]
   namespace delete Data${No}
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::CloseAll>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Ferme toute les boites de champs.
#
# Parametres :
#  <No>      : Numero de TrajBox
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc TrajBox::CloseAll { } {
   variable Data

   foreach no $Data(BoxList) {
     TrajBox::Close $no
   }
}

#----------------------------------------------------------------------------
# Nom      : <TrajBox::Raise>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Reafficher toute les boites ouvertes.
#
# Parametres   :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TrajBox::Raise { } {
   variable Data

   foreach box $Data(BoxList) {
      wm deiconify .trajbox$box
   }
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::Restrict>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue une restriction dans les champs affichees.
#
# Parametres :
#  <No>      : Numero de TrajBox
#  <args>    : Valeur des parametres du SelectBox
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc TrajBox::Restrict { No args } {
   variable Data

   .trajbox${No} configure -cursor watch
   update idletasks

   #----- On recupere les variable du module courant

   upvar #0 TrajBox::Data${No}::Data data

   .trajbox$No.data.list delete 0 end

   set data(Show) 0
   if { $data(FileCurrent) == -1 } {
      set files $data(TrajList)
   } else {
      set files [list [lindex $data(TrajList) [lsearch -exact $data(FileList) $data(FileCurrent)]]]
   }

   foreach file $files {
      foreach traj $file {

         #----- Extraire les informations

         set id [trajectory define $traj -ID]
         if { [string length $id] > 30 } {
            set id "[string range $id 0 27]..."
         }

         if { [trajectory define $traj -BACKWARD] } {
            set mode Retro
         } else {
            set mode Traj
         }
         set date [clock format [trajectory define $traj -DATE] -format "%Y%m%d%H%M" -gmt True]
         set level [format "%.2f" [trajectory define $traj -LEVEL]]

         if { ($data(Id)    == "" || [lsearch -exact $data(Id) $id]       != -1) &&
              ($data(Level) == "" || [lsearch -exact $data(Level) $level] != -1) &&
              ($data(Mode)  == "" || [lsearch -exact $data(Mode) $mode]   != -1) &&
              ($data(Date)  == "" || [lsearch -exact $data(Date) $date]   != -1) } {

            #----- Inserer les donnees dans la liste

            set line [format "%-30s %9.2f %-5s %12s %s" $id $level $mode $date $traj]
            .trajbox$No.data.list insert end $line
            incr data(Show)
         }
      }
   }
   set data(ShowRead) "$data(Show)/$data(Read)"
   .trajbox${No} configure -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::RestrictClear>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Reinitialise les listes pour effacer les restrictions.
#
# Parametres :
#  <No>      : Numero de TrajBox
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc TrajBox::RestrictClear { No } {
   variable Sel

   SelectBox::Clear .trajbox$No.header.id 0
   SelectBox::Clear .trajbox$No.header.mode 0
   SelectBox::Clear .trajbox$No.header.date 1
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::RestrictSet>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue une restriction dans les champs affichees.
#
# Parametres :
#  <No>      : Numero de FieldBox
#  <Var>     : Variable a restreindre (Id Level Mode Date)
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc TrajBox::RestrictSet { No Var Value } {

   upvar #0 TrajBox::Data${No}::Data data

   set data($Var) $Value
   TrajBox::Restrict $No
}

#----------------------------------------------------------------------------
# Nom      : <TrajBox::Scroll>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer un scroll dans la liste et reafficher le tout
#
# Parametres   :
#   <No>       : Numero de boite
#   <Incr>     : Direction du scroll (1 ou -1)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc TrajBox::Scroll { No Incr } {

   set idx [lindex [.trajbox${No}.data.list curselection] 0]

   set TrajBox::Data(Current) $No

   if { $idx!="" } {

      set nidx [expr $idx+$Incr]

      if { $nidx>=0 && $nidx<[.trajbox${No}.data.list index end] } {
         .trajbox${No}.data.list selection clear $idx
         .trajbox${No}.data.list selection set $nidx
         .trajbox${No}.data.list see $nidx
      }
   }

   TrajBox::Select
}

#-------------------------------------------------------------------------------
# Nom      : <TrajBox::Select>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer la liste des parametres decrivant le champs et retourner ces
#            valeurs a l'application cliente.
#
# Parametres :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TrajBox::Select { } {
   variable Data

   if { ![llength $Data(BoxList)] } {
      return
   }

   if { $Page::Data(Canvas)!="" } {
      $Page::Data(Canvas) config -cursor watch
   }
   . config -cursor watch
   update idletasks

   #----- Obtenir la selection de l'usager

   set trajs ""

   foreach box $Data(BoxList) {

      .trajbox$box config -cursor watch

      #----- Recuperer toutes les trajectoires selectionnees

      set idxs [.trajbox$box.data.list curselection]

      foreach idx $idxs {
         set info [.trajbox$box.data.list get $idx]

         #----- On recupere l'id de la trajectoire

         set id   [lindex $info end]
         set lidx [lsearch $Trajectory::Data(List) $id]

         if { $lidx==-1 } {

            #----- Si la trajectoire n'etait pas deja selectionnee

            trajectory stats $id -tag "$Page::Data(Frame) $Viewport::Data(VP) $box"
            lappend Viewport::Data(Data$Viewport::Data(VP)) $id
            lappend trajs $id
         } else {
            set vp [lindex [trajectory stats $id -tag] 1]
            if { [Page::Registered All Viewport $vp]==-1 } {
               .trajbox$box.data.list selection clear $idx
            } else {
               lappend trajs $id
            }
         }
      }
      .trajbox$box config -cursor left_ptr
   }

   #----- Eliminer les trajectoires qui ne sont plus selectionne

   foreach traj $Trajectory::Data(List) {
      if { [lsearch -exact $trajs $traj]==-1 } {
         Viewport::AssignedTo $traj fr vp
         if { [Page::Registered All Viewport $vp]!=-1 } {
            Viewport::UnAssign $fr $vp $traj -1
         }
      }
   }

   #----- Initialiser les parametres globaux

   set Trajectory::Data(List) $trajs
   Trajectory:::ParamUpdate

   #----- Vider la liste de frame des animations

   Animator::EmptyPlayList

   foreach frame $Page::Data(Frames) {
      Viewport::UpdateData    $frame
      Page::Update            $frame
      Page::UpdateCommand     $frame
   }

   if { $Page::Data(Canvas)!="" } {
      $Page::Data(Canvas) config -cursor left_ptr
   }
   .  config -cursor left_ptr
}

