#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Interface de selection de fichier de destination ou de lecture.
# Fichier   : FileBox.tk
# Creation  : Juillet 1998 - J.P. Gauthier - CMC/CMOE
#
# Description: Permet avec une interface de selectionne un fichier pour
#              une application quelconque.
#
# Fonctions:
#   FileBox::GetContent    { Path }
#   FileBox::GetType       { }
#   FileBox::Create        { Parent Path Mode Type { File {} } }
#   FileBox::DeepLen       { Path { Total 0 } }
#   FileBox::Delete        { }
#   FileBox::FormatSize    { Val }
#   FileBox::MemAdd        { }
#   FileBox::MemDel        { }
#   FileBox::MemLoad       { }
#   FileBox::MemSave       { }
#   FileBox::New           { }
#   FileBox::Popup         { X Y YL }
#   FileBox::Select        { Ok }
#   FileBox::SelectList    { { Exec True } }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide FileBox 3.1

catch { SPI::Splash "Loading Widget Package FileBox 3.1" }

package require Dialog
package require ComboBox
package require WidgetDefs
package require Bubble

namespace eval FileBox {
   global env
   variable Data
   variable Type
   variable Bubble
   variable Lbl
   variable Title
   variable Error

   #----- Defnitions des constantes internes

   set Data(Spec)         ""
   set Data(Result)       ""
   set Data(Filename)     ""
   set Data(Type)         ""
   set Data(Mode)         ""
   set Data(DirList)      "$env(HOME)/.eer_ToolDefs/eer_FileBoxPath"
   set Data(All)          0
   set Data(Sort)         File
   set Data(Size)         0
   set Data(Nb)           0
   set Data(Path)         ""

   catch { set Data(Path) [pwd] }

   set Type(ALL)     {All {*}}
   set Type(FSTD)    {RPN Standard file {*.fstd *.fst *}}
   set Type(GRIB)    {Grib (Rev1-2) {*.grib *.grb *}}
   set Type(CDF)     {netCDF {*.cdf *.ncdf *.netcdf}}
   set Type(TRAJ)    {CMC Trajectory {*.points}}
   set Type(HYSPLIT) {HYSPLIT Trajectory {*tdump*}}
   set Type(OBS)     {EER Observation {*.obs}}
   set Type(ICO)     {SPI Icon {*.ico}}
   set Type(DWG)     {SPI Drawing {*.dwg}}
   set Type(TCL)     {SPI Script {*.tcl}}
   set Type(SPI)     {SPI Project file {*.spi}}
   set Type(TXT)     {Text files {*.txt}}
   set Type(PROJ)    {PROJ4 Cartographic Projection {*.prj *.proj *.proj4}}

   #----- Definitions des titres

   set Title(Save)       { "Enregistrer sous :" "Save as :" }
   set Title(Load)       { "Lire :" "Load :" }
   set Title(Path)       { "Répertoire :" "Path :" }

   #----- Definitionse des Labels

   set Lbl(Cancel)       { "Annuler" "Cancel" }
   set Lbl(Byte)         { "o" "b" }
   set Lbl(Dir)          { "Répertoire" "Directory" }
   set Lbl(File)         { "Fichier" "Filename" }
   set Lbl(Type)         { "Type" "Type" }
   set Lbl(Create)       { "Créer" "Create" }
   set Lbl(Del)          { "Effacer" "Delete" }
   set Lbl(File)         { "Fichier(s)" "File(s)" }
   set Lbl(Name)         { "Nom" "Name" }
   set Lbl(New)          { "Nouveau" "New" }
   set Lbl(No)           { "Non" "No" }
   set Lbl(Owner)        { "Propr" "Owner" }
   set Lbl(Size)         { "Espace" "Size" }
   set Lbl(Tout)         { "Fichiers caches" "Hidden files" }
   set Lbl(Yes)          { "Oui" "Yes" }

   #----- Definitions des Bulles

   set Bubble(Back)      { "Remonte dans l'arborescence"
                           "Go up into the tree" }

   set Bubble(Mem)       { "Liste des répertoires usuels"
                           "Usual directory list" }

   set Bubble(Del)       { "Suppression du répertoire de\nla liste des répertoires usuels"
                           "Delete the current directory\nfrom the usual directory list" }

   set Bubble(Add)       { "Sauvegarde du répertoire dans\nla liste des répertoires usuels"
                           "Save the current directory\ninto the usual directory list" }

   set Bubble(Pattern)   { "Spécification de la chaine de restriction" "Specify the restriction pattern" }

   #----- Definitions des Messages

   set Msg(MemAdd)       { "Le répertoire/fichier suivant a été sauvegardé"
                           "The following path/file has been saved" }
   set Msg(MemDel)       { "Le répertoire/fichier suivant a été supprimé de la liste"
                           "the followin path/file has been removed from list" }
   set Msg(Right)        { "Vous n'avez pas la permission d'accéder a ce répertoire:"
                           "You do not have the right to access this path:" }
   set Msg(Del)          { "Voulez-vous vraiment supprimer ces fichiers ?" "Do you really want to suppress these files ?" }
   set Msg(Overwrite)    { "Voulez-vous écraser ce fichier ?" "Do you want to overwrite this file ?" }

   #----- Definitions des Erreurs

   set Error(File)       { "Fichier invalide" "Invalid file" }
   set Error(Right)      { "Vous n'avez pas la permission d'ecriture" "You do not have writing permission" }
   set Error(DataPath)   { "La selection est un fichier et non un repertoire" \
                           "The selection is a file, not a directory" }
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::GetContent>
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Affiche le contenue du repertoire selectionne.
#
# Parametres  :
#   <Path>    : Path complet de la selection.
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::GetContent { { Path "" } } {
   global GDefs
   variable Lbl
   variable Msg
   variable Data

   if { $Path!="" } {
      set Data(Path) $Path
   }
   set Data(Path)      [file normalize $Data(Path)]
   set Data(Width)     [expr [winfo width .filebox.header.file]/[font measure [lindex [.filebox.header.file configure -font] 4] "m"]-2]

   if { $Data(Mode)!="Save" } {
      set Data(Filename) ""
   }

   .filebox.files.list configure -bg $GDefs(ColorLight)

   if { ![file isdirectory $Data(Path)] } {
      .filebox.files.list delete 0 end
      .filebox.files.list configure -bg $GDefs(ColorFrame)
      return
   } else {
      if { ![file readable $Data(Path)] } {
         Dialog::Info .filebox $Msg(Right) "\n\n\t$Data(Path)"
      }
   }

   .filebox config -cursor watch
   update idletasks

   #----- Initialiser le menu back

   .filebox.path.home.back delete 1 end
   .filebox.path.home.back add separator

   set list [file split $Data(Path)]
   set list [lrange $list 1 end-1]

   set path ""
   if { [llength $list] > 1 } {
      foreach part $list {
         append path "/$part"
         .filebox.path.home.back add command -label $path \
             -command "FileBox::GetContent \"$path\" "
      }
   }

   #----- Cleanup de la boite
   .filebox.files.list delete 0 end

   set Data(Size) 0.0
   set Data(Spec) [string trim $Data(Spec)]
   set Data(Nb) 0
   set lines ""

   set pattern $Data(Pattern)
   set dpattern *
   if { $Data(All) } {
      lappend pattern ".*"
      lappend dpattern ".*"
   }

   if { "$Data(Path)"!="/" && !$Data(All) } {
     .filebox.files.list insert end "../"
   }

   #----- Recuperer les repertoires
   eval set dirs \[lsort -dictionary \[glob -nocomplain -directory $Data(Path) -types d -tails  $dpattern\]\]
   foreach dir $dirs {
      if { [catch { file stat $Data(Path)/$dir info } ] } {
         continue
      }
      .filebox.files.list insert end [format "%-$Data(Width)s %8s %10i" ${dir}/ [file attributes $Data(Path)/$dir -owner] $info(size)]
      set Data(Size) [expr $Data(Size)+$info(size)]
      incr Data(Nb)
   }

   #----- Recuperer les fichiers
   eval set files \[glob -nocomplain -directory $Data(Path) -types f -tails $pattern\]
   foreach file $files {
      if { [catch { set size [file stat $Data(Path)/$file info] } ] } {
         continue
      }

      if { $Data(Spec)=="" || [string match -nocase $Data(Spec) $file] } {
         catch {
            lappend lines [format "%-$Data(Width)s %8s %10i" $file [file attributes $Data(Path)/$file -owner] $info(size)]
            set Data(Size) [expr $Data(Size)+$info(size)]
            incr Data(Nb)
         }
      }
   }

   switch $Data(Sort) {
      "File"  { set lines [lsort -unique -dictionary -index 0 $lines ] }
      "Owner" { set lines [lsort -dictionary -index 1 $lines ] }
      "Size"  { set lines [lsort -decreasing -integer -index end $lines ] }
   }
   eval .filebox.files.list insert end $lines

   #----- Formatter les infos

   set Data(Nb)   "$Data(Nb) [lindex $Lbl(File) $GDefs(Lang)]"
   set Data(Size) [FormatSize $Data(Size)]

   .filebox config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::GetType>
# Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recuperer le type de fichier selectionne.
#
# Parametres :
#
# Retour     :
#   <Type>   : Type de fichier
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::GetType { } {
   variable Data

   return $Data(Type)
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::SetPattern>
# Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE -
#
# But      : Assigner les extensions au type de fichier selectionne.
#
# Parametres :
#
# Retour     :
#   <Type>   : Type de fichier
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::SetPattern { } {
   variable Data

   set Data(Pattern) [lindex $Data(Type) end]

   if { $Data(Pattern) == "" } {
      set Data(Pattern) *
   }
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::Create>
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Creer la boite de dialogue Filebox.
#
# Parametres :
#   <Parent> : Fenetre parent.
#   <Path>   : Repertoire par defaut.
#   <Mode>   : Mode (Load, Save ou Path)
#   <Types>  : Type de fichiers
#   <File>   : Nom du fichier (Depend du Mode).
#
# Retour     :
#   <fichier>: Fichier selectionne.
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::Create { Parent Path Mode Types { File "" } } {
   global GDefs env
   variable Data
   variable Type
   variable Lbl
   variable Bubble
   variable Title

   $Parent config -cursor X_cursor
   update idletasks

   #----- Verifier le repertoire par defaut
   if { [file isdirectory $Path] } {
      set Data(Path) $Path
   }

   if { $Mode=="Load" || $Mode=="LoadPath"} {
      lappend Types $Type(ALL)
   }

   set Data(Types)    $Types
   set Data(Mode)     $Mode
   set Data(Type)     [lindex $Types 0]
   set Data(Filename) $File

   #----- build widget structure.

   toplevel .filebox -bg $GDefs(ColorLight)
   wm transient .filebox $Parent
   wm geo .filebox 530x350+[winfo rootx $Parent]+[winfo rooty $Parent]
   wm protocol .filebox WM_DELETE_WINDOW { }

   menu .fileboxpopup
      .fileboxpopup add cascade -label [lindex $Lbl(New) $GDefs(Lang)] -menu .fileboxpopup.new
      .fileboxpopup add command -label [lindex $Lbl(Del) $GDefs(Lang)] -command FileBox::Delete
   menu .fileboxpopup.new
      .fileboxpopup.new add command -label [lindex $Lbl(Dir) $GDefs(Lang)] -command FileBox::New

   frame .filebox.path -relief raised -bd 1
      label .filebox.path.label -text [lindex $Lbl(Dir) $GDefs(Lang)] -width 10 -anchor w
      ComboBox::Create .filebox.path.name FileBox::Data(Path) editclose sorted nodouble -1 {} 40 10 "FileBox::GetContent"
      button .filebox.path.save -image FOLDIN -bd 0 -command "FileBox::MemAdd"
      button .filebox.path.del -image FOLDOUT -bd 0 -command "FileBox::MemDel"
      menubutton .filebox.path.home -image FOLDUP -bd 0 -menu .filebox.path.home.back
      menu .filebox.path.home.back
      .filebox.path.home.back add command -label $env(HOME) -command { FileBox::GetContent $env(HOME) }
      .filebox.path.home.back add separator

      pack .filebox.path.label -side left -fill y -ipadx 2
      pack .filebox.path.name -side left -fill both -expand true
      pack .filebox.path.save .filebox.path.del  .filebox.path.home -side left -fill y -ipadx 2
   pack .filebox.path -side top -fill x

   frame .filebox.file -relief raised -bd 1
      label .filebox.file.label -text [lindex $Lbl(File) $GDefs(Lang)] -width 10 -anchor w
      entry .filebox.file.name -textvariable FileBox::Data(Filename) -background $GDefs(ColorLight) -relief sunken -bd 1
      entry .filebox.file.pattern -textvariable FileBox::Data(Spec) -background $GDefs(ColorLight) -relief sunken -bd 1 -width 9
      checkbutton .filebox.file.hid -variable FileBox::Data(All) -bd 0 -image FOLDHID \
         -indicatoron false -command "FileBox::GetContent"
      pack .filebox.file.label -side left -fill y -ipadx 2
      pack .filebox.file.name -side left -fill both -expand true
      pack .filebox.file.pattern .filebox.file.hid -side left -fill both
   pack .filebox.file -side top -fill x

   frame .filebox.type -relief raised -bd 1
      label .filebox.type.label -text [lindex $Lbl(Type) $GDefs(Lang)] -width 10 -anchor w
      ComboBox::Create .filebox.type.pattern FileBox::Data(Type) noedit unsorted nodouble -1 $FileBox::Data(Types) 40 5 \
         "FileBox::SetPattern; FileBox::GetContent"
      pack .filebox.type.label -side left -fill y -ipadx 2
      pack .filebox.type.pattern  -side left -fill both -expand true
   pack .filebox.type -side top -fill x

   frame .filebox.header
      radiobutton .filebox.header.file -text [lindex $Lbl(File) $GDefs(Lang)] -indicatoron false -bd 1 -width 1 \
          -value File -variable FileBox::Data(Sort) -command "FileBox::GetContent"
      radiobutton .filebox.header.size -text [lindex $Lbl(Size) $GDefs(Lang)] -indicatoron false -bd 1 -width 11 \
          -value Size -variable FileBox::Data(Sort) -command "FileBox::GetContent"
      radiobutton .filebox.header.uid -text [lindex $Lbl(Owner) $GDefs(Lang)] -indicatoron false -bd 1 -width 7 \
          -value Owner -variable FileBox::Data(Sort) -command "FileBox::GetContent"
      pack .filebox.header.file -side left -ipadx 2 -ipady 2 -fill x -expand true
      pack .filebox.header.uid .filebox.header.size -side left -ipadx 2 -ipady 2
   pack .filebox.header -side top -fill x

   frame .filebox.files
      scrollbar .filebox.files.vscroll -command ".filebox.files.list yview" -bd 1 -width 10
      listbox .filebox.files.list -relief sunken -yscrollcommand ".filebox.files.vscroll set" \
         -selectmode extended -exportselection 0 -background $GDefs(ColorLight) -bd 1
      grid .filebox.files.list -column 1 -row 1 -sticky nsew
      grid .filebox.files.vscroll -column 2 -row 1 -sticky ns
      grid columnconfigure .filebox.files 1 -weight 1
      grid rowconfigure .filebox.files 1 -weight 1
   pack .filebox.files -expand true -side top -fill both

   frame .filebox.bar -relief raised -bd 1
      label .filebox.bar.nbfile -textvariable FileBox::Data(Nb) -relief sunken -bd 1 -bg $GDefs(ColorLight) -width 15
      label .filebox.bar.size   -textvariable FileBox::Data(Size) -relief sunken -bd 1 -bg $GDefs(ColorLight) -width 11
      pack  .filebox.bar.size .filebox.bar.nbfile -side right -fill y
   pack .filebox.bar -side top -fill x

   frame .filebox.frame1
      button .filebox.frame1.ok -text "Ok" -command "FileBox::Select 1" -bd 1
      button .filebox.frame1.cancel -text [lindex $Lbl(Cancel) $GDefs(Lang)] -bd 1 -command "set FileBox::Data(Result) \"\""
      pack .filebox.frame1.ok .filebox.frame1.cancel -expand true -side left -fill both
   pack .filebox.frame1 -side top -fill x

   update idletasks

   #----- Creation des evenements

#   bind .filebox.path.name.select <Return>           { + FileBox::GetContent }
   bind .filebox.file.name        <Return>           " FileBox::Select 1 "
   bind .filebox.files.list       <ButtonRelease-1>  { FileBox::SelectList }
   bind .filebox.files.list       <Button-3>         { FileBox::Popup %X %Y %y}
   bind .filebox.type.pattern     <Return>           { FileBox::GetContent }
   bind .filebox.file.pattern     <Return>           { FileBox::GetContent }
   bind .filebox.files.list       <Configure>        { FileBox::GetContent }

   bind .filebox                  <Return>           " FileBox::SelectList; if { \$FileBox::Data(Filename)!=\"\" } { FileBox::Select 1 } "
   bind .filebox                  <Key-Up>           { FileBox::Scroll -1 }
   bind .filebox                  <Key-Down>         { FileBox::Scroll   1 }
   bind .filebox                  <Key-Escape>       { set FileBox::Data(Result) "" }

  #----- Creation des bulles

   Bubble::Create .filebox.path.home     [lindex $Bubble(Back) $GDefs(Lang)]
   Bubble::Create .filebox.path.save     [lindex $Bubble(Add)  $GDefs(Lang)]
   Bubble::Create .filebox.path.del      [lindex $Bubble(Del)  $GDefs(Lang)]
   Bubble::Create .filebox.path.name.box [lindex $Bubble(Mem)  $GDefs(Lang)]
   Bubble::Create .filebox.file.pattern  [lindex $Bubble(Pattern) $GDefs(Lang)]

   switch $Data(Mode) {

      "Load" {
         wm title .filebox [lindex $Title(Load) $GDefs(Lang)]
         .filebox.files.list configure -selectmode extended
      }
      "LoadPath" {
         wm title .filebox [lindex $Title(Load) $GDefs(Lang)]
         .filebox.files.list configure -selectmode extended
      }
      "Save" {
         wm title .filebox [lindex $Title(Save) $GDefs(Lang)]
      }
      "Path" {
         wm title .filebox [lindex $Title(Path) $GDefs(Lang)]
      }
      default {
         Log::Print ERROR "Mode has to be \"Load\" , \"Save\" or \"Path\""
         exit 0
      }
   }

   FileBox::MemLoad
   FileBox::SetPattern
   FileBox::GetContent

   #----- Attente de la selection du fichier

   set prevgrab [grab current]
   grab .filebox
   tkwait variable FileBox::Data(Result)
   catch { destroy .filebox .fileboxpopup }
   $Parent config -cursor left_ptr

   if { $prevgrab!="" } {
      grab $prevgrab
   }
   return $Data(Result)
}

proc FileBox::Scroll { Incr } {

   set idx [lindex [.filebox.files.list curselection] 0]

   if { $idx=="" } {
      set idx  0
      set nidx 0
   } else {
      set nidx [expr $idx+$Incr]
   }
   if { $nidx>=0 && $nidx<[.filebox.files.list index end] } {
      .filebox.files.list selection clear $idx
      .filebox.files.list selection set $nidx
      .filebox.files.list see $nidx

      FileBox::SelectList False
   }
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::DeepLen>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE -
#
# But      : Determiner le nombre de fichier recursivement.
#
# Parametres :
#   <Path>   : Path a parcourir
#   <Total>  : Decompte recursif
#
# Retour     :
#   <Total>  : Nombre de fichiers
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::DeepLen { Path { Total 1 } } {

   set files [glob -nocomplain $Path/*]
   incr Total [llength $files]

   foreach file [glob -nocomplain $Path/*] {
      if { [file isdirectory $file] } {
         set Total [FileBox::DeepLen $file $Total]
      }
   }
   return $Total
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::Delete>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Supprimer les fichiers selectionnes.
#
# Parametres :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::Delete { } {
   global   GDefs
   variable Data
   variable Lbl
   variable Msg

   set idxs [.filebox.files.list curselection]
   set files ""

   foreach idx $idxs {
      lappend files [string trim [string range [.filebox.files.list get $idx] 0 $Data(Width)]]
   }

   if { [Dialog::Default .filebox 200 WARNING $Msg(Del) "\n\n$files" 0 $Lbl(No) $Lbl(Yes)] } {
      foreach file $files {
         file delete -force $Data(Path)/$file
      }
      FileBox::GetContent
   }
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::FormatSize>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Formatter la grosseur en octets.
#
# Parametres  :
#   <Val>     : Valeur en Octet.
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::FormatSize { Val } {
   global GDefs
   variable Lbl

   set unit ""

   if { $Val > 1024 } {
      set Val [expr $Val/1024.0]
      set unit K

      if { $Val > 1024 } {
         set Val [expr $Val/1024.0]
         set unit M

         if { $Val > 1024 } {
            set Val [expr $Val/1024.0]
            set unit G
         }
      }
   }

   return "[format "%2.2f" $Val] $unit[lindex $Lbl(Byte) $GDefs(Lang)]"
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::MemAdd>
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ajoute le path en cours a la liste des path sauvegarde.
#
# Parametres :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::MemAdd { } {
   global GDefs
   variable Msg
   variable Data

   if { [file isdirectory $Data(Path)] } {
      set name $Data(Path)
   } else {
      set name [file dirname $Data(Path)]
   }

   if { [ComboBox::Add .filebox.path.name $name] != -1 } {
      FileBox::MemSave
      Dialog::Info .filebox $Msg(MemAdd) "\n\n\t$name"
   }
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::MemDel>
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Supprime le path selectionne de la liste des path sauvegarde.
#
# Parametres :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::MemDel { } {
   global GDefs
   variable Msg
   variable Data

   if { [ComboBox::Del .filebox.path.name $Data(Path)] != -1 } {
      FileBox::MemSave
      Dialog::Info .filebox $Msg(MemDel) "\n\n\t$Data(Path)"
   }
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::MemLoad>
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Lit la liste des path sauvegarde et l'insere dans le listbox.
#
# Parametres :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::MemLoad { } {
   variable Data

   set p $Data(Path)
   ComboBox::DelAll .filebox.path.name
   set maxlen 40

   if { ![catch { set file [open $Data(DirList) r] }]  } {

      while { ![eof $file] } {
         gets $file path
         set len [string length $path]
         if { [string length $path]>0 } {
            ComboBox::Add .filebox.path.name $path
         }
         if { $len>$maxlen } {
            set maxlen [incr len 2]
         }
      }
      close $file
   }
   set Data(Path) $p
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::MemSave>
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Sauvegarde le liste des path.
#
# Parametres :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::MemSave { } {
   variable Data

   .filebox configure -cursor watch

   set file [open $Data(DirList) w]

   foreach path [ComboBox::List .filebox.path.name] {
      puts $file $path
   }
   close $file

   .filebox configure -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::New>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Creer un nouveau fichier/repertoire
#
# Parametres :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::New { } {
   global   GDefs
   variable Data
   variable Lbl
   variable Msg

   toplevel .fileboxnew
   wm title .fileboxnew [lindex $Lbl(New) $GDefs(Lang)]
   wm transient .fileboxnew .filebox
   wm geom .fileboxnew +[expr [winfo rootx .filebox]+50]+[expr [winfo rooty .filebox]+50]
   wm protocol .fileboxnew WM_DELETE_WINDOW { }
   wm resizable .fileboxnew 0 0

   set Data(New) ""

   frame .fileboxnew.spec -relief raised -bd 1
      label .fileboxnew.spec.lbl -text [lindex $Lbl(Name) $GDefs(Lang)] -anchor w
      entry .fileboxnew.spec.ent -textvariable FileBox::Data(New) -relief sunken -bd 1 -width 20 -bg $GDefs(ColorLight)
      pack .fileboxnew.spec.lbl .fileboxnew.spec.ent -side left

   frame .fileboxnew.com
      button .fileboxnew.com.ok -text [lindex $Lbl(Create) $GDefs(Lang)] \
         -relief raised -bd 1 -command { file mkdir $FileBox::Data(Path)/$FileBox::Data(New) ; destroy .fileboxnew ; FileBox::GetContent }
      button .fileboxnew.com.cancel -text [lindex $Lbl(Cancel) $GDefs(Lang)]\
         -relief raised -bd 1 -command { destroy .fileboxnew }
      pack .fileboxnew.com.ok .fileboxnew.com.cancel -side left -fill x -expand true
   pack .fileboxnew.spec .fileboxnew.com -side top -fill x -expand true

   grab .fileboxnew
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::Popup>
# Creation : Novembre 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Afficher le menu contextuel et initialiser la selection
#
# Parametres :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::Popup { X Y YL } {

   set idxs [.filebox.files.list curselection]

   if { ![llength $idxs] } {
      .filebox.files.list selection set [.filebox.files.list nearest $YL]
   }
   tk_popup .fileboxpopup $X $Y
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::Select>
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Retour apres la selection effectuee.
#
# Parametres   :
#   <Ok>       : L'appel provient du bouton "Ok" ou de la cle "Enter"
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::Filename { File } {
   variable Data

   if { [file extension $File]!=[set ext [file extension [lindex $Data(Pattern) 0]]] } {
      return $File$ext
   } else {
      return $File
   }
}

proc FileBox::Select { Ok } {
   global   GDefs
   variable Error
   variable Data
   variable Lbl
   variable Msg

   switch $Data(Mode) {
      "LoadPath" {
          set result ""

          if { $Data(Filename)!="" } {
             foreach file $Data(Filename) {
                set file "$Data(Path)/$file"
                if { ![file isfile $file] } {
                   Dialog::Error .filebox $Error(File) "\n\n\t$file"
                   return
                } else {
                   lappend result $file
                }
             }
             set Data(Result) $result
          } elseif { [file isdirectory $Data(Path)] } {
             set Data(Result) $Data(Path)
          }
       }
      "Load" {
          set result ""

          foreach file $Data(Filename) {
             set file "$Data(Path)/$file"
             if { ![file isfile $file] } {
                Dialog::Error .filebox $Error(File) "\n\n\t$file"
                return
             } else {
                lappend result $file
             }
          }
          set Data(Result) $result
       }
      "Save" {
         if { [string trim $Data(Filename)]!="" } {
            if { [file isfile $Data(Path)/$Data(Filename)] } {
              if { [Dialog::Default .filebox 200 WARNING $Msg(Overwrite) "\n\n$Data(Path)/$Data(Filename)" 0 $Lbl(No) $Lbl(Yes)] } {
                  set Data(Result) [FileBox::Filename $Data(Path)/$Data(Filename)]
               }

             #----- Si le path est bon

             } else {
                if { [file writable [file dirname $Data(Path)/$Data(Filename)]] } {
                   set Data(Result) [FileBox::Filename $Data(Path)/$Data(Filename)]
                } else {
                   Dialog::Error .filebox $Error(Right)
                }
             }
          }
       }
      "Path" {
          if { [file isdirectory $Data(Path)] } {
             set Data(Result) $Data(Path)
          } else {
             FileBox::GetContent $Data(Path)/$Data(Filename)
          }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <FileBox::SelectList>
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Selection du nom dans le listbox.
#
# Parametres :
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc FileBox::SelectList { { Exec True } } {
   global GDefs
   variable Lbl
   variable Data

   #----- Extraire le nom selectionne
   set idx [.filebox.files.list curselection]

   if { [llength $idx]==0 } {
      return
   }

   set files ""
   set size  0.0

   #----- Get the total selection file size
   foreach id $idx {
      set line [.filebox.files.list get $id]
      lappend files "[string trim [lindex $line 0]]"
      catch { set size [expr $size+[lindex $line end]] }
   }

   if { [file isdirectory [lindex $Data(Path)/$files 0]] } {
      if { $Exec } {
         FileBox::GetContent $Data(Path)/[lindex $files 0]
      } else {
         set Data(Filename) ""
      }
   } else {
      set Data(Size)     [FormatSize $size]
      set Data(Nb)       "[llength $idx] [lindex $Lbl(File) $GDefs(Lang)]"
      set Data(Filename) $files
   }
}
