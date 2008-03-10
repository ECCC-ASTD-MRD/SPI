
#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : OsbBox.tcl
# Version  : 1.2
# Creation : Fevrier 2003 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Permet d'afficher un selecteur d'observations.
#
# Fonctions:
#
#   OsbBox::Clear         { }
#   ObsBox::Close         { No }
#   ObsBox::CloseAll      { }
#   ObsBox::Create        { Parent Title { Geom "" } }
#   ObsBox::Exist         { No }
#   ObsBox::FileClose     { No }
#   ObsBox::FileOpen      { No File }
#   ObsBox::FileSelect    { No File FID }
#   ObsBox::Get           { }
#   ObsBox::GetContent    { { Id "" } }
#   ObsBox::GetFile       { args }
#   ObsBox::GetSelected   { args }
#   ObsBox::InfoCommand   { No Index }
#   ObsBox::Init          { No }
#   ObsBox::Insert        { No }
#   ObsBox::PasteClick    { No Y }
#   ObsBox::PasteDeClick  { Y }
#   ObsBox::Raise         { }
#   ObsBox::Restrict      { No }
#   ObsBox::RestrictClear { No }
#   ObsBox::RestrictSet   { No Var Value }
#   ObsBox::Scroll        { No Incr }
#   ObsBox::Select        { }
#
# Remarques :
#
# Modification:
#
#   Nom         : J.P. Gauthier
#   Date        : Octobre 2003
#   Description : Ajout du scroll avec les fleches
#===============================================================================

package provide ObsBox 1.2

proc IdObsBox { Show } {

   if { $Show } {
      puts "(INFO) Loading Standard CMC/CMOE Widget Package ObsBox Version 1.2"
   }
}

namespace eval ObsBox {
   variable Lbl
   variable Bubble
   variable Error
   variable Data
   variable Param

   set Param(Geom)     { 300x200+[winfo rootx .]+[winfo rooty .] }
   set Param(Title)    { "ObsBox" "ObsBox" }

   #----- Variables de suivit du contenu

   set Data(Current)  ""               ;#Numero de boite ayant le focus
   set Data(BoxList)  ""               ;#Liste des numeros de boite ObsBox

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

   set Error(Empty)       { "Ce fichier ne contient pas d'observations."
                            "There is no observations in this file." }
   #----Bulles d'aides

   set Bubble(Files)      { "Selection des fichiers d'observations" "Observation file selection" }
   set Bubble(File)       { "Fichier(s) d'observation courant" "Current observation file(s)" }
   set Bubble(Nb)         { "Nombre d'observations affichees" "Number of displayed observations" }
   set Bubble(Refresh)    { "Mettre a jour l'affichage" "Update display" }
   set Bubble(Restrict)   { "Annuler les criteres de restrictions" "Clear the restriction criteria" }
   set Bubble(Select)     { "Criteres de restriction\nde selection des champs" "Field restriction criteria" }
}

#----------------------------------------------------------------------------
# Nom      : <ObsBox::Clear>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc ObsBox::Clear { } {
   variable Data

   foreach box $Data(BoxList) {
      .obsbox$box.data.list selection clear 0 end
   }

   set Obs::Data(List) ""
}

#----------------------------------------------------------------------------
# Nom      : <ObsBox::Create>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une boite permettant de selectionner des observations.
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc ObsBox::Create { Parent Title { Geom "" } } {
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

   set id  .obsbox$no
   set spc Data$no

   ObsBox::Init $no

   toplevel     $id
   wm title     $id "[lindex $Param(Title) $GDefs(Lang)] ($Title)"
   wm transient $id .
   wm resizable $id False True
   wm maxsize   $id 320 10000
   wm minsize   $id 320 300
   wm protocol  $id WM_DELETE_WINDOW "ObsBox::Close $no"

   #----- Afficher la fenetre par dessus la derniere

   if { $Geom!="" } {
      wm geom   $id $Geom
   } else {
      if { [llength $Data(BoxList)] == 1 } {
         eval wm geom $id $Param(Geom)
      } else {
         set previd  .obsbox[lindex $Data(BoxList) [expr [llength $Data(BoxList)]-2]]
         wm geom   $id +[winfo rootx $previd]+[winfo rooty $previd]
      }
   }

   #----- Initialiser le resultat

   frame $id.header
      SelectBox::Create $id.header.id "IDENTIFICATION" \
         ObsBox::${spc}::Data(Id) "" 20 10 ObsBox::Restrict $no
      SelectBox::Create $id.header.nb "NB" \
         ObsBox::${spc}::Data(Nb) "" 4 10 ObsBox::Restrict $no
      SelectBox::Create $id.header.hour "HR" \
         ObsBox::${spc}::Data(Hour) "" 2 10 ObsBox::Restrict $no
      SelectBox::Create $id.header.date "DATE" \
         ObsBox::${spc}::Data(Date) "" 11 10 ObsBox::Restrict $no
      button $id.header.info -bitmap "@$GDefs(Dir)/Resources/Bitmap/CLEAR.xbm" -relief raised \
         -bd 1 -command "ObsBox::RestrictClear $no"
      pack $id.header.id $id.header.nb $id.header.hour $id.header.date -side left -ipadx 3 -fill y
      pack $id.header.info -side left -fill both -expand true
   pack $id.header -side top -fill x

   #----- Creer la listbox concernant les champs.

   frame $id.data -relief raised -bd 1
      listbox $id.data.list -yscrollcommand [list $id.data.scrolly set] -xscrollcommand "$id.data.list xview 0;catch " \
         -height 5  -bd 1 -relief sunken -bg $GDefs(ColorLight) -exportselection 0 -selectmode extended -width 40
      scrollbar $id.data.scrolly -orient vertical -command [list $id.data.list yview] -bd 1 -width 10
      pack $id.data.list -side left -fill both -expand true
      pack $id.data.scrolly -side right -fill y
   pack $id.data -side top -fill both -expand true

   frame $id.info
      button $id.info.refresh -image DOCSEL -command "ObsBox::Select" -bd 1
      menubutton $id.info.file -menu $id.info.file.menu -bd 1 -relief raised -image OPEN
      menubutton $id.info.name -textvariable ObsBox::${spc}::Data(FileInfo) -anchor w -bg $GDefs(ColorLight) \
          -relief raised -bd 1 -width 20 -menu $id.info.name.list
      label $id.info.lbl -textvariable ObsBox::${spc}::Data(ShowRead) -bg $GDefs(ColorLight) \
          -relief raised -bd 1
      pack $id.info.refresh $id.info.file -side left -fill both
      pack $id.info.name -side left -fill both -expand true
      pack $id.info.lbl -side left -fill both -ipadx 2
   pack $id.info -side top -fill x

   menu $id.info.file.menu
      $id.info.file.menu add command -label "[lindex $Lbl(Open) $GDefs(Lang)] ..." -image OPEN -compound left \
         -command "ObsBox::FileOpen $no \[FileBox::Create $id \"\" Load \[list \$FileBox::Type(OBS)\]\]"
      $id.info.file.menu add command -label [lindex $Lbl(Close) $GDefs(Lang)] -image FOLD -compound left \
         -command "ObsBox::FileClose $no"
      $id.info.file.menu add separator
      $id.info.file.menu add command -label [lindex $Lbl(NewBox) $GDefs(Lang)] -image FRAME0 -compound left \
         -command "ObsBox::Create $Parent Clone"
      $id.info.file.menu add command -label [lindex $Lbl(CloseBox) $GDefs(Lang)] -image FRAMEDEL -compound left \
         -command "ObsBox::Close $no"

   menu $id.info.name.list -bg $GDefs(ColorLight)
      $id.info.name.list add command -label [lindex $Lbl(AllFiles) $GDefs(Lang)] \
         -command "set ObsBox::${spc}::Data(FileCurrent) -1 ; set ObsBox::${spc}::Data(FileInfo) \"[lindex $Lbl(AllFiles) $GDefs(Lang)]\" ; ObsBox::Restrict $no"

   set ObsBox::${spc}::Data(FileInfo) [lindex $Lbl(NoFiles) $GDefs(Lang)]

   #----- Creer les bulles d'info

#   ListboxBubble::Create $id.data.list 0 ObsBox::InfoCommand $no

   #----- Permettre la selection d'un champs dans la liste.

   bind $id.data.list <Double-ButtonRelease-1> "set ObsBox::Data(Current) $no ; ObsBox::Select"
   bind $id.data.list <ButtonRelease-1>        "set ObsBox::Data(Current) $no"
   bind $id.data.list <ButtonPress-3>          "set ObsBox::Data(Current) $no ; tk_popup .obsmenu %X %Y"

   bind $id.data.list <ButtonPress-2>          "set ObsBox::Data(Current) $no ; if { \[winfo exist .fieldcalc\] } { ObsBox::PasteClick $no %y }"
   bind $id.data.list <ButtonRelease-2>        "set ObsBox::Data(Current) $no ; if { \[winfo exist .fieldcalc\] } { ObsBox::PasteDeClick $no %y }"

   bind $id <Key-Up>                 "ObsBox::Scroll $no -1"
   bind $id <Key-Down>               "ObsBox::Scroll $no  1"

   Bubble::Create $id.info         [lindex $Bubble(Files)    $GDefs(Lang)]
   Bubble::Create $id.info.name    [lindex $Bubble(File)     $GDefs(Lang)]
   Bubble::Create $id.info.lbl     [lindex $Bubble(Nb)       $GDefs(Lang)]
   Bubble::Create $id.header       [lindex $Bubble(Select)   $GDefs(Lang)]
   Bubble::Create $id.header.info  [lindex $Bubble(Restrict) $GDefs(Lang)]
   Bubble::Create $id.info.refresh [lindex $Bubble(Refresh)  $GDefs(Lang)]

   #----- Creer le menu contextuel

   if { ![winfo exist .obsmenu] } {

      menu .obsmenu
         .obsmenu add command -label [lindex $Lbl(SelectClear) $GDefs(Lang)] \
            -command ".obsbox\$ObsBox::Data(Current).data.list selection clear 0 end"
         .obsmenu add command -label [lindex $Lbl(SelectAll) $GDefs(Lang)] \
            -command ".obsbox\$ObsBox::Data(Current).data.list selection set 0 end"
         .obsmenu add separator
         .obsmenu add checkbutton -label [lindex $Lbl(Bubble) $GDefs(Lang)] \
            -variable ListboxBubble::Data(State) -command "ListboxBubble::Activate"
   }
   set ObsBox::Data(Current) $no
   return $no
}

#----------------------------------------------------------------------------
# Nom      : <ObsBox::Exist>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc ObsBox::Exist { No } {

   return [winfo exist .obsbox$No]
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::FileClose>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Fermeture d'un fichier d'observations.
#
# Parametres :
#  <No>      : Numero de ObsBox
#
# Retour    :
#
# Remarque :
#    -On verifie la variable Data(FileCurrent) :
#       -1 : On ferme tout les fichiers
#        n : On ferme le fichier correspondant a cet index
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc ObsBox::FileClose { No } {
   variable Data
   variable Lbl
   global GDefs

   #----- On recupere les variable du module courant

   upvar #0 ObsBox::Data${No}::Data data

   #----- Determiner les fichiers a fermer

   if { $data(FileCurrent) == "-1" } {
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

      #----- Liberer les observations
      foreach obs [lindex $data(ObsList) $idx] {
         set vp [lindex [observation stats $obs -tag] 1]
        if { $vp!="" && [Page::Registered All Viewport $vp]!=-1 } {
            set ido [lsearch -exact $Viewport::Data(Data$vp) $obs]
            set Viewport::Data(Data$vp) [lreplace $Viewport::Data(Data$vp) $ido $ido]
         }
         set i [lsearch -exact $Obs::Data(List) $obs]
         set Obs::Data(List) [lreplace $Obs::Data(List) $i $i]

         observation free $obs
         if { [observation is COPY$obs] } {
            observation free COPY$obs
         }
      }
      set data(ObsList) [lreplace $data(ObsList) $idx $idx]

      .obsbox${No}.info.name.list delete [expr 1 + $idx]
   }

   if { [llength $data(FileList)] == 0 } {
      set data(FileInfo) [lindex $Lbl(NoFiles) $GDefs(Lang)]
   } elseif { [llength $data(FileList)] == 1 } {
      set data(FileInfo) "...[string range $data(FileList) end-25 end]"
      set data(FileCurrent) $data(FileList)
   } else {
      set data(FileInfo) [lindex $Lbl(AllFiles) $GDefs(Lang)]
      set data(FileCurrent) -1
   }
   ObsBox::Insert $No
   ObsBox::Select
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::FileOpen>
# Creation : Juillet 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Ouverture d'un fichier d'observations.
#
# Parametres :
#  <No>      : Numero de ObsBox
#  <File>    : Path du fichier
#
# Retour    :
#
# Remarque :
#     - Si No < 0 l'ouverture se fera dans la derniere boite
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc ObsBox::FileOpen { No File } {
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

   upvar #0 ObsBox::Data${No}::Data data

   foreach file $File {

      #----- Si le fichier est deja ouvert

      if { [lsearch -exact $data(FileList) $file] != -1 } {
         continue
      }

      #----- Ouvrir le fichier

      set lobs [observation load $file]

      if { ![llength $lobs] } {
         Dialog::CreateError .obsbox$No "[lindex $Error(Empty) $GDefs(Lang)]\n\n$file" $GDefs(Lang)
      } else {

         lappend data(FileList) $file
         lappend data(ObsList) $lobs

         .obsbox$No.info.name.list add command -label "$file" \
            -command "ObsBox::FileSelect $No $file"

         if { [llength $data(FileList)] == 1 } {
            set data(FileInfo) "...[string range $file end-25 end]"
            set data(FileCurrent) $file
         } else {
           set data(FileInfo) [lindex $Lbl(AllFiles) $GDefs(Lang)]
           set data(FileCurrent) -1
         }
      }
   }
   ObsBox::Insert $No
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::Get>
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
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc ObsBox::Get { } {
   variable Data

   return $Data(BoxList)
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::GetContent>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la liste de toutes les observations de toutes les boites.
#
# Parametres :
#   <Id>     : No de la boite (""=courante, -1=toute, 0...n=numero)
#
# Retour   :
#   <Obs> : Liste des parametres de tous les champs.
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc ObsBox::GetContent { { Id "" } } {
   variable Data

   #----- Boite courante

   if { $Id=="" } {
      set Id $ObsBox::Data(Current)
   }

   #----- Numero de boite

   if { $Id!=-1 } {
      upvar #0 ObsBox::Data${Id}::Data data
      return [join $data(ObsList)]
   }

   set obs {}

   foreach box $Data(BoxList) {

      upvar #0 ObsBox::Data${box}::Data data

      if { $obs != "" } {
         set obs [list $obs]
      }

      eval set obs \[concat $obs $data(ObsList)\]
   }
   return $obs
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::GetFile>
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
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc ObsBox::GetFile { args } {
   variable Data

   #----- Boite courante

   if { $args=="" } {
      set args $ObsBox::Data(Current)
   }

   #----- Numero de boite

   if { $args!=-1 } {
      upvar #0 ObsBox::Data${args}::Data data
      return $data(FileList)
   }

   #----- Toute les boites

   set files {}

   foreach box $Data(BoxList) {

      upvar #0 ObsBox::Data${box}::Data data

      if { $files!="" } {
         set files [list $fields]
      }
      eval set files \[concat $fields $data(FileList)\]
   }
   return $files
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::GetSelected>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la liste de toutes les observations selectionnees parmi
#            toutes les boites.
#
# Parametres :
#   <args>   : No de la boite (""=courante, -1=toute, 0...n=numero)
#
# Retour     :
#   <Obss>  : Liste des parametres de toutes les observations selectionnees
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc ObsBox::GetSelected { args } {
   variable Data

   set obs {}

   #----- Boite courante

   if { $args=="" } {
      foreach idx [.obsbox$Data(Current).data.list curselection] {
         lappend obs [.obsbox$Data(Current).data.list get $idx]
      }
      return $obs
   }

   #----- Numero de boite

   if { $args!=-1 } {
      foreach idx [.obsbox${args}.data.list curselection] {
         lappend obs [.obsbox${args}.data.list get $idx]
      }
      return $obs
   }

   #----- Toute les boites

   foreach box $ObsBox::Data(BoxList) {
      foreach idx [.obsbox$box.data.list curselection] {
         lappend obs [.obsbox$box.data.list get $idx]
      }
   }
   return $obs
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::FileSelect>
# Creation : Juillet 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Fermeture d'un fichier standard.
#
# Parametres :
#  <No>      : Numero de ObsBox
#  <File>    : Path complet du fichier
#  <FID>     : Numero du fichier
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

proc ObsBox::FileSelect { No File } {

   #----- On recupere les variable du module courant

   upvar #0 ObsBox::Data${No}::Data data

   set data(FileCurrent) $File
   set data(FileInfo) "...[string range $File end-25 end]"
   ObsBox::Restrict $No
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::InfoCommand>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Execute la commande de recherche de l'information sur l'observation.
#
# Parametres :
#  <No>      : Numero de ObsBox
#  <Index>   : Index de l'item selectionne dans la liste.
#
# Remarques :
#    -Cette commande est appelee par le package ListboxBubble.
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc ObsBox::InfoCommand { No Index } {
   variable Data
   global GDefs

#   set id   [lindex [.obsbox$No.data.list get $Index] end]
#   set info [trajectory define $id -PARCEL 0]
#   set lbl  "Coord  : [lindex $info 1] [lindex $info 2]"

#   return "$lbl"
}

#----------------------------------------------------------------------------
# Nom      : <ObsBox::Init>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise les variable d'une nouvelle boite ObsBox.
#
# Parametres   :
#   <No>       : Numero de la boite
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

proc ObsBox::Init { No } {

   namespace eval Data$No {
      variable Data
      variable Sel

      #----- Variables relatives champs

      set Data(Read)     0      ;#Nombre d'observations lu
      set Data(Show)     0      ;#Nombre d'observations affichees
      set Data(ShowRead) 0      ;#Nombre d'observations affichees sur le nombre de lu

      set Data(ObsList)     ""  ;#Liste des observations
      set Data(FileCurrent) ""  ;#Fichier actif
      set Data(FileInfo)    ""  ;#Nom du fichier affiche
      set Data(FileList)    ""  ;#Liste des fichiers ouverts

      set Data(Id)     ""
      set Data(Nb)     ""
      set Data(Hour)   ""
      set Data(Date)   ""
   }
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::Insert>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Insere les champs dans la liste.
#
# Parametres :
#  <No>      : Numero de ObsBox
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

proc ObsBox::Insert { No } {
   variable Data

   #----- On recupere les variable du module courant

   upvar #0 ObsBox::Data${No}::Data data

   .obsbox$No.data.list delete 0 end
   .obsbox$No config -cursor watch
   update idletasks

   set data(Read) 0
   set data(Id)   ""
   set data(Nb)   ""
   set data(Date) ""

   foreach file $data(ObsList) {
      foreach obs $file {
         #----- Extraire les informations

         set id [lindex [split $obs .] 0]
         if { [string length $id] > 20 } {
            set id "[string range $id 0 16]..."
         }

         set sec  [observation define $obs -DATE]
         set date [clock format $sec -format "%Y%m%d%H%M" -gmt true]
         set hour [clock format $sec -format "%H" -gmt true]
         set nb   [observation define $obs -NB]

         #----- Garder la liste des champs de selection

         if { [lsearch -exact $data(Id) $id] == -1 } {
            lappend data(Id) $id
         }
         if { [lsearch -exact $data(Date) $date] == -1 } {
            lappend data(Date) $date
         }
         if { [lsearch -exact $data(Nb) $nb] == -1 } {
            lappend data(Nb) $nb
         }
         if { [lsearch -exact $data(Hour) $hour] == -1 } {
            lappend data(Hour) $hour
         }

         #----- Inserer les donnees dans la liste

         set line [format "%-20s %6i %2s %12s %s" $id $nb $hour $date $obs]
         .obsbox$No.data.list insert end $line
         incr data(Read)
      }
   }

   set data(Show)     "$data(Read)"
   set data(ShowRead) "$data(Show)/$data(Read)"

   #----- Update des listes de selections

   SelectBox::Insert .obsbox$No.header.id   $data(Id)
   SelectBox::Insert .obsbox$No.header.nb   $data(Nb)
   SelectBox::Insert .obsbox$No.header.hour $data(Hour)
   SelectBox::Insert .obsbox$No.header.date $data(Date)

   .obsbox$No config -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::Close>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Ferme tout les fichiers ouvert et detruit la fenetre.
#
# Parametres :
#  <No>      : Numero de ObsBox
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

proc ObsBox::Close { No } {
   variable Data

   if { ![winfo exists .obsbox${No}] } {
      return
   }

   #----- Fermer tout les fichiers

   set ObsBox::Data${No}::Data(FileCurrent) -1
   ObsBox::FileClose $No
   #----- Annuler toute selection

   .obsbox${No}.data.list selection clear 0 end
   ObsBox::Select

   #----- Detruire l'interface

   SelectBox::Destroy .obsbox${No}.header.id
   SelectBox::Destroy .obsbox${No}.header.nb
   SelectBox::Destroy .obsbox${No}.header.hour
   SelectBox::Destroy .obsbox${No}.header.date

   ListboxBubble::Destroy .obsbox${No}.data.list
   destroy .obsbox${No}

   #----- Cleanup des variables

   set idx [lsearch -exact $Data(BoxList) $No]
   set Data(BoxList) [lreplace $Data(BoxList) $idx $idx]
   namespace delete Data${No}
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::CloseAll>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Ferme toute les boites de champs.
#
# Parametres :
#  <No>      : Numero de ObsBox
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -ObsBox::Close $no
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc ObsBox::CloseAll { } {
   variable Data

   foreach no $Data(BoxList) {
     ObsBox::Close $no
   }
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::PasteClick>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Selection de l'observation a etre utilise.
#
# Parametres :
#  <No>      : Numero de FieldBox
#  <Y>       : Coordonnee y du curseur
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc ObsBox::PasteClick { No Y } {
   variable Data

   #----- Obtenir la selection de l'usager

   set Data(Paste)  [.obsbox$No.data.list nearest $Y]
   .obsbox$No.data.list selection set $Data(Paste)
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::PasteDeClick>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Envoyer l'observation selectione a la calculatrice.
#
# Parametres :
#  <No>      : Numero de FieldBox
#  <Y>       : Coordonnee y du curseur
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc ObsBox::PasteDeClick { No Y } {
   variable Data

   #----- Obtenir la selection de l'usager

   set idx [.obsbox$No.data.list nearest $Y]
   .obsbox$No.data.list selection clear $Data(Paste)

   if { $idx==$Data(Paste) } {
      set data [.obsbox$No.data.list get $Data(Paste)]
      FieldCalc::Paste 0 [lindex $data end]
   }
}

#----------------------------------------------------------------------------
# Nom      : <ObsBox::Raise>
# Creation : Fevrier 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Reafficher toute les boites ouvertes.
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

proc ObsBox::Raise { } {
   variable Data

   foreach box $Data(BoxList) {
      wm deiconify .obsbox$box
   }
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::Restrict>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue une restriction dans les champs affichees.
#
# Parametres :
#  <No>      : Numero de ObsBox
#  <args>    : Valeur des parametres du SelectBox
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

proc ObsBox::Restrict { No args } {
   variable Data

   .obsbox${No} configure -cursor watch
   update idletasks

   #----- On recupere les variable du module courant

   upvar #0 ObsBox::Data${No}::Data data

   .obsbox$No.data.list delete 0 end

   set data(Show) 0
   if { $data(FileCurrent) == -1 } {
      set files $data(ObsList)
   } else {
      set files [list [lindex $data(ObsList) [lsearch -exact $data(FileList) $data(FileCurrent)]]]
   }

   foreach file $files {
      foreach obs $file {

         #----- Extraire les informations

         set id [lindex [split $obs .] 0]
         if { [string length $id] > 20 } {
            set id "[string range $id 0 16]..."
         }

         set sec  [observation define $obs -DATE]
         set date [clock format $sec -format "%Y%m%d%H%M" -gmt true]
         set hour [clock format $sec -format "%H" -gmt true]
         set nb    [observation define $obs -NB]

         if { ($data(Id)   == "" || [lsearch -exact $data(Id) $id]     != -1) &&
              ($data(Nb)   == "" || [lsearch -exact $data(Nb) $nb]     != -1) &&
              ($data(Hour) == "" || [lsearch -exact $data(Hour) $hour] != -1) &&
              ($data(Date) == "" || [lsearch -exact $data(Date) $date] != -1) } {

            #----- Inserer les donnees dans la liste

            set line [format "%-20s %6i %2s %12s %s" $id $nb $hour $date $obs]
            .obsbox$No.data.list insert end $line
            incr data(Show)
         }
      }
   }

   set data(ShowRead) "$data(Show)/$data(Read)"
   .obsbox${No} configure -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::RestrictClear>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Reinitialise les listes pour effacer les restrictions.
#
# Parametres :
#  <No>      : Numero de ObsBox
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

proc ObsBox::RestrictClear { No } {
   variable Sel

   SelectBox::Clear .obsbox$No.header.id   0
   SelectBox::Clear .obsbox$No.header.nb   0
   SelectBox::Clear .obsbox$No.header.hour 0
   SelectBox::Clear .obsbox$No.header.date 1
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::RestrictSet>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue une restriction dans les champs affichees.
#
# Parametres :
#  <No>      : Numero de FieldBox
#  <Var>     : Variable a restreindre (Id Nb Date)
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

proc ObsBox::RestrictSet { No Var Value } {

   upvar #0 ObsBox::Data${No}::Data data

   set data($Var) $Value
   ObsBox::Restrict $No
}

#----------------------------------------------------------------------------
# Nom      : <ObsBox::Scroll>
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc ObsBox::Scroll { No Incr } {

   set idx [lindex [.obsbox${No}.data.list curselection] 0]

   set ObsBox::Data(Current) $No

   if { $idx!="" } {

      set nidx [expr $idx+$Incr]

      if { $nidx>=0 && $nidx<[.obsbox${No}.data.list index end] } {
         .obsbox${No}.data.list selection clear $idx
         .obsbox${No}.data.list selection set $nidx
         .obsbox${No}.data.list see $nidx
      }
   }

   ObsBox::Select
}

#-------------------------------------------------------------------------------
# Nom      : <ObsBox::Select>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer la liste des parametres decrivant le champs et retourner ces
#            valeurs a l'application cliente.
#
# Parametres :
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc ObsBox::Select { } {
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

   set lobs ""

   foreach box $Data(BoxList) {

      .obsbox$box config -cursor watch

      #----- Recuperer toutes les observations selectionnees

      set idxs [.obsbox$box.data.list curselection]

      foreach idx $idxs {
         set info [.obsbox$box.data.list get $idx]

         #----- On recupere l'id de l'observations

         set id   [lindex $info end]
         set lidx [lsearch $Obs::Data(List) $id]

         if { $lidx==-1 } {
            #----- Si l'observation n'etait pas deja selectionnee

            observation stats $id -tag "$Page::Data(Frame) $Viewport::Data(VP) $box"
            lappend Viewport::Data(Data$Viewport::Data(VP)) $id
            lappend lobs $id
         } else {
            set vp [lindex [observation stats $id -tag] 1]
            if { [Page::Registered All Viewport $vp]==-1 } {
               .obsbox$box.data.list selection clear $idx
            } else {
               lappend lobs $id
            }
         }
      }
      .obsbox$box config -cursor left_ptr
   }
   #----- Eliminer les observations qui ne sont plus selectionne

   foreach obs $Obs::Data(List) {
      if { [lsearch -exact $lobs $obs]==-1 } {
         Viewport::AssignedTo $obs fr vp
         if { [Page::Registered All Viewport $vp]!=-1 } {
            Viewport::UnAssign $fr $vp $obs -1
         }
      }
   }

   #----- Initialiser les parametres globaux

   set Obs::Data(List) $lobs
   Obs::ParamUpdate

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
   . config -cursor left_ptr
}
