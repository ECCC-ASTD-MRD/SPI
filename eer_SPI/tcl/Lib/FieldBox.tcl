#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : FieldBox.tcl
# Creation : Juin 1999 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Permet d'afficher un selecteur de champs de fichier standard.
#
# Fonctions:
#
#   FieldBox::Clear         { }
#   FieldBox::Close         { No }
#   FieldBox::CloseAll      { }
#   FieldBox::Create        { Parent Title { Geom "" } }
#   FieldBox::Exist         { No }
#   FieldBox::FileClose     { No }
#   FieldBox::FileOpen      { No File }
#   FieldBox::FileReOpen    { No }
#   FieldBox::FileSelect    { No File FID }
#   FieldBox::Descriptors   { }
#   FieldBox::Get           { }
#   FieldBox::GetContent    { { Id "" } }
#   FieldBox::GetFID        { args }
#   FieldBox::GetFile       { args }
#   FieldBox::GetSelected   { args }
#   FieldBox::InfoCommand   { No Index }
#   FieldBox::Init          { No }
#   FieldBox::Insert        { No }
#   FieldBox::PopUp         { X Y  } 
#   FieldBox::Raise         { }
#   FieldBox::Restrict      { No }
#   FieldBox::RestrictClear { No }
#   FieldBox::RestrictSet   { No Var Value }
#   FieldBox::Select        { }
#   FieldBox::Show          { Field }
#
#   FieldParams::Window     { { Field "" } }
#   FieldParams::GetInfo    { { Field "" } }
#   FieldParams::GetMatrix  { { Field "" } }
#   FieldParams::SetInfo    { { Field "" } { Write False } }
#
# Remarques :
#
#===============================================================================

package provide FieldBox 2.2

catch { SPI::Splash "Loading Widget Package FieldBox 2.2" }

package require Bubble
package require ListboxBubble
package require SelectBox
package require DateStuff
package require FileBox
package require Info

namespace eval FieldBox {
   variable Lbl
   variable Bubble
   variable Error
   variable Data
   variable Param

   set Param(Geom)     { 415x200+[winfo rootx .]+[winfo rooty .] }
   set Param(Title)    { "FieldBox" "FieldBox" }

   #----- Variables de suivit du contenu

   set Data(Current)  ""               ;#Numero de boite ayant le focus
   set Data(BoxList)  ""               ;#Liste des numeros de boite FieldBox
   set Data(FIDList)  ""               ;#Liste des numeros de fichiers
   
   set Param(ShowDesc)  False          ;#Afficher les descripteurs
   set Param(Desc)      { ^* >> ## !! HY }

   fstdfield hide $Param(Desc)

   #----- Textes et labels

   set Lbl(AllFiles)       { "Tous les fichiers" "Every file" }
   set Lbl(Bubble)         { "Bulles d'information" "Info bubbles" }
   set Lbl(SelectAll)      { "Selectionner tout" "Select all" }
   set Lbl(SelectClear)    { "Annuler la selection" "Clear selection" }
   set Lbl(SelectClearAll) { "Annuler la selection (Toute les boites)" "Clear selection (All boxes)" }
   set Lbl(Close)          { "Fermer fichier(s)" "Closefile(s)" }
   set Lbl(Copy)           { "Copier dans" "Copy to" }
   set Lbl(NewBox)         { "Nouvelle boite" "New box" }
   set Lbl(CloseBox)       { "Fermer cette boite" "Close box" }
   set Lbl(NoFiles)        { "Aucun fichier" "No file" }
   set Lbl(Open)           { "Ouvrir fichier" "Open file" }
   set Lbl(ReOpen)         { "Re-ouvrir fichier" "Re-open file" }
   set Lbl(Level)          { "NIVEAU" "LEVEL" }
   set Lbl(Params)         { "Détails du champ" "Field details" }
   set Lbl(Desc)           { "Afficher les descripteurs" "Show descriptors" }

   #----- Erreurs

   set Error(Empty)       { "Ce fichier n'est pas un fichier standards ou alors il est vide."
                            "This file is not a standard file or it is empty." }
   set Error(Exist)       { "Ce fichier n'existe pas."
                            "This file does not exist." }
   #----Bulles d'aides

   set Bubble(Files)      { "Selection des fichiers standards" "Standard file selection" }
   set Bubble(File)       { "Fichier(s) standard courant" "Current standard file(s)" }
   set Bubble(Nb)         { "Nombre de champs affichees" "Number od displayed field" }
   set Bubble(Refresh)    { "Mettre a jour l'affichage" "Update display" }
   set Bubble(Restrict)   { "Annuler les criteres de restrictions" "Clear the restriction criteria" }
   set Bubble(Select)     { "Criteres de restriction\nde selection des champs" "Field restriction criteria" }
}

#----------------------------------------------------------------------------
# Nom      : <FieldBox::Clear>
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

proc FieldBox::Clear { { Var True } } {
   variable Data

   foreach box $Data(BoxList) {
      .fieldbox$box.data.list selection clear 0 end
   }

   if { $Var } {
      foreach fld $FSTD::Data(List) {
         fstdfield free $fld
      }

      set FSTD::Data(List) ""
   } else {
      FieldBox::Select
   }
}

#----------------------------------------------------------------------------
# Nom      : <FieldBox::Create>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une boite permettant de selectionner des champs.
#
# Parametres   :
#   <Parent>   : Nom du widget
#   <Title>    : Titre des donnees
#   <Geom>     : Geometrie de la fenetre
#
# Retour:
#   <No>       : Numero de la boite
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldBox::Create { Parent Title { Geom "" } } {
   global GDefs
   variable Lbl
   variable Bubble
   variable Data
   variable Param

   for { set no 0 } { $no < 20 } { incr no } {
      if { $no ni $Data(BoxList) } {
         lappend Data(BoxList) $no
         break
      }
   }

   set id  .fieldbox$no
   set spc Data$no

   FieldBox::Init $no

   toplevel     $id
   wm title     $id "[lindex $Param(Title) $GDefs(Lang)] ($Title)"
   wm transient $id .
   wm resizable $id False True
   wm maxsize   $id 515 10000
   wm minsize   $id 515 300
   wm protocol  $id WM_DELETE_WINDOW "FieldBox::Close $no"

   #----- Afficher la fenetre par dessus la derniere

   if { $Geom!="" } {
      wm geom   $id $Geom
   } else {
      if { [llength $Data(BoxList)] == 1 } {
         eval wm geom $id $Param(Geom)
      } else {
         set previd  .fieldbox[lindex $Data(BoxList) [expr [llength $Data(BoxList)]-2]]
         wm geom   $id +[winfo rootx $previd]+[winfo rooty $previd]
      }
   }

   #----- Initialiser le resultat

   frame $id.header
      SelectBox::Create $id.header.var "VAR"  \
         FieldBox::${spc}::Data(Var) "" 4 10 FieldBox::Restrict $no
      SelectBox::Create $id.header.type "TYPE"  \
         FieldBox::${spc}::Data(Type) "" 4 10 FieldBox::Restrict $no
      SelectBox::Create $id.header.level [lindex $Lbl(Level) $GDefs(Lang)]  \
         FieldBox::${spc}::Data(Level) "" 10 10 FieldBox::Restrict $no
      SelectBox::Create $id.header.ip2 "IP2"  \
         FieldBox::${spc}::Data(IP2) "" 10 10 FieldBox::Restrict $no
      SelectBox::Create $id.header.ip3 "IP3"  \
         FieldBox::${spc}::Data(IP3) "" 10 10 FieldBox::Restrict $no
      SelectBox::Create $id.header.eticket "ETICKET"  \
         FieldBox::${spc}::Data(Eticket) "" 12 10 FieldBox::Restrict $no
      SelectBox::Create $id.header.date "DATEV"  \
         FieldBox::${spc}::Data(Date) "" 10 10 FieldBox::Restrict $no
      button $id.header.info -bitmap "@$GDefs(Dir)/share/bitmap/CLEAR.xbm" -relief raised \
         -bd 1 -command "FieldBox::RestrictClear $no"
      pack $id.header.var $id.header.type $id.header.level $id.header.ip2 \
           $id.header.ip3 $id.header.eticket $id.header.date -side left -ipadx 3 -fill y
      pack $id.header.info -side left -fill both -expand true
   pack $id.header -side top -fill x

   #----- Creer la listbox concernant les champs.

   frame $id.data -relief raised -bd 1
      listbox $id.data.list -yscrollcommand [list $id.data.scrolly set] -xscrollcommand "$id.data.list xview 0;catch " \
         -height 5  -bd 1 -relief sunken -bg $GDefs(ColorLight) -exportselection 0 -selectmode extended -width 52
      scrollbar $id.data.scrolly -orient vertical -command [list $id.data.list yview] -bd 1 -width 10
      pack $id.data.list -side left -fill both -expand true
      pack $id.data.scrolly -side right -fill y
   pack $id.data -side top -fill both -expand true

#   tkdnd::drag_source register $id.data.list DND_Text 1
#   bind $id.data.list <B1-Motion> "puts dtdtd; break"
#   bind $id.data.list <<DragInitCmd>> {list copy DND_Text [FieldBox::PasteClick %W %Y]}
   
   frame $id.info
      button $id.info.refresh -image DOCSEL -bd 1 -command "set FieldBox::Data(Current) $no ;FieldBox::Select"
      menubutton $id.info.file -menu $id.info.file.menu -bd 1 -relief raised -image OPEN
      menubutton $id.info.name -textvariable FieldBox::${spc}::Data(FileInfo) -anchor w -bg $GDefs(ColorLight) \
          -relief raised -bd 1 -width 20 -menu $id.info.name.list
      label $id.info.lbl -textvariable FieldBox::${spc}::Data(ShowRead) -bg $GDefs(ColorLight) \
          -relief raised -bd 1
      pack $id.info.refresh $id.info.file -side left -fill both
      pack $id.info.name -side left -fill both -expand true
      pack $id.info.lbl -side left -fill both -ipadx 2
   pack $id.info -side top -fill x

   menu $id.info.file.menu
      $id.info.file.menu add command -label "[lindex $Lbl(Open) $GDefs(Lang)] ..." -image OPEN -compound left \
         -command "FieldBox::FileOpen $no \[FileBox::Create $id \"\" Load \[list \$FileBox::Type(FSTD)\]\]"
      $id.info.file.menu add command -label [lindex $Lbl(Close) $GDefs(Lang)] -image FOLD -compound left \
         -command "FieldBox::FileClose $no"
      $id.info.file.menu add separator
      $id.info.file.menu add command -label [lindex $Lbl(ReOpen) $GDefs(Lang)] -image OPEN -compound left \
         -command "FieldBox::FileReOpen $no"
      $id.info.file.menu add separator
      $id.info.file.menu add command -label [lindex $Lbl(NewBox) $GDefs(Lang)] -image FRAME0 -compound left \
         -command "FieldBox::Create $Parent Clone"
      $id.info.file.menu add command -label [lindex $Lbl(CloseBox) $GDefs(Lang)] -image FRAMEDEL -compound left \
         -command "FieldBox::Close $no"

   menu $id.info.name.list -bg $GDefs(ColorLight)
      $id.info.name.list add command -label [lindex $Lbl(AllFiles) $GDefs(Lang)] \
         -command "set FieldBox::Data(FileCurrent) -1 ; set FieldBox::${spc}::Data(FileInfo) \"[lindex $Lbl(AllFiles) $GDefs(Lang)]\" ; set FieldBox::${spc}::Data(FID) \"\" ; FieldBox::Restrict $no"

   set FieldBox::${spc}::Data(FileInfo) [lindex $Lbl(NoFiles) $GDefs(Lang)]

   #----- Creer les bulles d'info

   ListboxBubble::Create $id.data.list 0 FieldBox::InfoCommand $no

   #----- Permettre la selection d'un champs dans la liste.

   bind $id.data.list <Double-ButtonRelease-1> "set FieldBox::Data(Current) $no ; FieldBox::Select"
   bind $id.data.list <ButtonRelease-1>        "set FieldBox::Data(Current) $no"
   bind $id.data.list <ButtonPress-3>          "set FieldBox::Data(Current) $no ; FieldBox::PopUp %X %Y"

   bind $id <Key-Up>                           "set FieldBox::Data(Current) $no ; FieldBox::Select"
   bind $id <Key-Down>                         "set FieldBox::Data(Current) $no ; FieldBox::Select"
   bind $id <Control-Alt-backslash>            "FieldBox::Clear False"

   Bubble::Create $id.info         $Bubble(Files)
   Bubble::Create $id.info.name    $Bubble(File)
   Bubble::Create $id.info.lbl     $Bubble(Nb)
   Bubble::Create $id.header       $Bubble(Select)
   Bubble::Create $id.header.info  $Bubble(Restrict)
   Bubble::Create $id.info.refresh $Bubble(Refresh)

   #----- Creer le menu contextuel

   if { ![winfo exist .fieldmenu] } {

      menu .fieldmenu
         .fieldmenu add command -label [lindex $Lbl(SelectClear) $GDefs(Lang)] -accelerator "Ctrl-\\" \
            -command ".fieldbox\$FieldBox::Data(Current).data.list selection clear 0 end; FieldBox::Select" 
         .fieldmenu add command -label [lindex $Lbl(SelectClearAll) $GDefs(Lang)] -accelerator "Ctrl-Alt-\\" \
            -command "FieldBox::Clear False" 
         .fieldmenu add command -label [lindex $Lbl(SelectAll) $GDefs(Lang)] -accelerator "Ctrl-/" \
            -command ".fieldbox\$FieldBox::Data(Current).data.list selection set 0 end"
         .fieldmenu add separator
         .fieldmenu add command -label "[lindex $Lbl(Params) $GDefs(Lang)] ..." \
            -command { FieldParams::Window; FieldBox::Select  }
         .fieldmenu add command -label "[lindex $Lbl(Copy) $GDefs(Lang)] ..."\
            -command { FieldBox::FieldCopy [FileBox::Create .fieldbox$FieldBox::Data(Current) "" Save ""] }
         .fieldmenu add separator
         .fieldmenu add checkbutton -label [lindex $Lbl(Desc) $GDefs(Lang)] -onvalue True -offvalue False \
            -variable FieldBox::Param(ShowDesc) -command FieldBox::Descriptors
         .fieldmenu add checkbutton -label [lindex $Lbl(Bubble) $GDefs(Lang)] \
            -variable ListboxBubble::Data(State) -command "ListboxBubble::Activate"
   }
   set Data(Current) $no
   return $no
}

#----------------------------------------------------------------------------
# Nom      : <FieldBox::Descriptors>
# Creation : Aout 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher / Cacher les descripteurs
#
# Parametres   :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldBox::Descriptors { } {
   variable Param
   variable Data
   
   if { $Param(ShowDesc) } {
      fstdfield hide {}
   } else {
      fstdfield hide $Param(Desc)
   }
   
   set FieldBox::Data(FileCurrent) -1  
   FieldBox::FileReOpen $Data(Current)
}

#----------------------------------------------------------------------------
# Nom      : <FieldBox::FieldCopy>
# Creation : Juin 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Transferer un champs dans un autre fichier.
#
# Parametres   :
#   <File>     : Path complet du cichier vers ou transferer
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldBox::FieldCopy { File } {
   variable Data

   if { $File=="" } {
      return
   }

   fstdfile open FIELDBOXFILE write $File

   foreach idx [.fieldbox$Data(Current).data.list curselection] {

      set info [.fieldbox$Data(Current).data.list get $idx]
      set fid  [lindex $info end-5]
      set fidx [lindex $info end-4]

      fstdfield read FIELDBOXTMP $fid $fidx
      fstdfield write FIELDBOXTMP FIELDBOXFILE 0 True
   }
   fstdfile close FIELDBOXFILE
}

#----------------------------------------------------------------------------
# Nom      : <FieldBox::Exist>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Verifie qu la boite existe.
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

proc FieldBox::Exist { No } {

   return [winfo exist .fieldbox$No]
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::FileClose>
# Creation : Juillet 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Fermeture d'un fichier standard.
#
# Parametres :
#  <No>      : Numero de FieldBox
#
# Retour    :
#
# Remarque :
#    -On verifie la variable Data(FileCurrent) :
#       -1 : On ferme tout les fichiers
#        n : On ferme le fichier correspondant a cet index
#
#-------------------------------------------------------------------------------

proc FieldBox::FileClose { No } {
   variable Data
   variable Lbl
   global GDefs

   #----- On recupere les variable du module courant

   upvar #0 FieldBox::Data${No}::Data data

   #----- Determiner les fichiers a fermer

   if { $data(FileCurrent) == "-1" } {

      #----- Fermer les fichiers

      foreach id $data(FIDList) type $data(TypeList) {
         if { $type=="fstdfield" } {
            fstdfile close $id
         } else {
            gribfile close $id
         }
         set idx [lsearch -exact $Data(FIDList) $id]
         set Data(FIDList) [lreplace $Data(FIDList) $idx $idx]
      }

      set data(FileList)  ""
      set data(TypeList)  ""
      set data(FIDList)   ""
      set data(FieldList) ""
      .fieldbox${No}.info.name.list delete 1 end
    } else {

      set idx       [lsearch -exact $data(FileList) $data(FileCurrent)]
      set fid       [lindex $data(FIDList) $idx]
      set type      [lindex $data(TypeList) $idx]
      set data(FileList)  [lreplace $data(FileList)  $idx $idx]
      set data(TypeList)  [lreplace $data(TypeList)  $idx $idx]
      set data(FIDList)   [lreplace $data(FIDList)   $idx $idx]
      set data(FieldList) [lreplace $data(FieldList) $idx $idx]

      .fieldbox${No}.info.name.list delete [expr 1 + $idx]

      #----- Fermer le fichier

      if { $type=="fstdfield" } {
         fstdfile close $fid
      } else {
         gribfile close $fid
      }

      set idx [lsearch -exact $Data(FIDList) $fid]
      set Data(FIDList) [lreplace $Data(FIDList) $idx $idx]
   }

   if { [llength $data(FileList)] == 0 } {
      set data(FileInfo)    [lindex $Lbl(NoFiles) $GDefs(Lang)]
      set data(FileCurrent) -1
   } elseif { [llength $data(FileList)] == 1 } {
      set data(FileInfo)    "[format %02i $data(FIDList)] ...[string range $data(FileList) end-35 end]"
      set data(FileCurrent) 0
   } else {
      set data(FileInfo) [lindex $Lbl(AllFiles) $GDefs(Lang)]
      set data(FileCurrent) -1
   }
   FieldBox::Insert $No
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::FileOpen>
# Creation : Juillet 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Ouverture d'un fichier standard.
#
# Parametres :
#  <No>      : Numero de FieldBox
#  <File>    : Path du fichier
#
# Retour    :
#
# Remarque :
#     - Si No < 0 l'ouverture se fera dans la derniere boite
#
#-------------------------------------------------------------------------------

proc FieldBox::FileOpen { No File } {
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

   upvar #0 FieldBox::Data${No}::Data data

   foreach file $File {

      #----- Le fichier existe il ???
      set rem 0
      if { ![file exists $file] && [set rem [string first : $file]]==-1 } {
         Dialog::Error .fieldbox$No $Error(Exist) "\n\n$file"
         continue
      }

      #----- Si le fichier est deja ouvert
      if { $file in $data(FileList) } {
         continue
      }

      #----- Determiner l'index du fichier a ouvrir
      for { set fid 1 } { $fid < 10000 } { incr fid } {
         if { $fid ni $Data(FIDList) } {
            break
         }
      }

      #----- Ouvrir le fichier
      set index ""
      if { [fstdfile is $file] || $rem } {
         set bad [catch { set index [fstdfile open $fid read $file SPI] } msg]
         if { $bad || ![llength $index] } {
            fstdfile close $fid
         } else {
            lappend data(TypeList) fstdfield
         }
      } elseif { [gribfile is $file] } {
         set bad [catch { set index [gribfile open $fid read $file SPI] }]
         if { $bad || ![llength $index] } {
            gribfile close $fid
         } else {
            lappend data(TypeList) gribfield
         }
      }

      if { $index=="" } {
         Dialog::Error .fieldbox$No $Error(Empty) "\n\n$file"
      } else {
         lappend data(FileList)  $file
         lappend data(FIDList)   $fid
         lappend data(FieldList) $index
         lappend Data(FIDList)   $fid

         .fieldbox$No.info.name.list add command -label "[format %02i $fid] $file" \
            -command "FieldBox::FileSelect $No $file $fid"

         if { [llength $data(FileList)]==1 } {
            set data(FileInfo) "[format %02i $fid] ...[string range $file end-35 end]"
            set data(FileCurrent) $file
         } else {
           set data(FileInfo) [lindex $Lbl(AllFiles) $GDefs(Lang)]
           set data(FileCurrent) -1
         }
      }
   }
   FieldBox::Insert $No
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::FileReOpen>
# Creation : Juillet 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Re Ouverture de fichier standard.
#
# Parametres :
#  <No>      : Numero de FieldBox
#
# Retour    :
#
# Remarque :
#     - Si No < 0 l'ouverture se fera dans la derniere boite
#
#-------------------------------------------------------------------------------

proc FieldBox::FileReOpen { No } {
   variable Data
   variable Lbl
   variable Error
   global GDefs

   #----- Selectionner la derniere boite

   if { $No < 0 } {
      set No [lindex Data(BoxList) end]
   }

   #----- On recupere les variable du module courant

   upvar #0 FieldBox::Data${No}::Data data

   if { $data(FileCurrent)!=-1 } {
      set idx   [lsearch -exact $data(FileList) $data(FileCurrent)]
      set fids  [lindex $data(FIDList) $idx]
      set types [lindex $data(TypeList) $idx]
      set files $data(FileCurrent)
   } else {
      set idx 0
      set fids  $data(FIDList)
      set types $data(TypeList)
      set files $data(FileList)
   }

   foreach file $files fid $fids type $types {

      #----- Le fichier existe il ???

      if { ![file exists $file] } {
         Dialog::Error .fieldbox$No "[lindex $Error(Exist) $GDefs(Lang)]\n\n$file" $GDefs(Lang)
         lset data(FieldList) $idx ""
      } else {
         if { $type=="fstdfield" } {
            fstdfile close $fid
            catch { set index [fstdfile open $fid read $file SPI] }
            if { $index=="" } {
               fstdfile close $fid
            }
        } else {
            gribfile close $fid
            catch { set index [gribfile open $fid read $file SPI] }
            if { $index=="" } {
               gribfile close $fid
            }
         }

         if { $index == "" } {
            Dialog::Error .fieldbox$No "[lindex $Error(Empty) $GDefs(Lang)]\n\n$file" $GDefs(Lang)
            lset data(FieldList) $idx ""
            lreplace data(FileList) $idx $idx
         } else {
            lset data(FieldList) $idx $index

            if { $idx == 0 } {
               set data(FileInfo) "[format %02i $fid] ...[string range $file end-35 end]"
               set data(FileCurrent) $file
            } else {
               set data(FileInfo) [lindex $Lbl(AllFiles) $GDefs(Lang)]
               set data(FileCurrent) -1
            }
         }
      }
      incr idx
   }
   FieldBox::Insert $No
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::Get>
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

proc FieldBox::Get { } {
   variable Data

   return $Data(BoxList)
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::GetContent>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la liste de tout les champs de toutes les boites.
#
# Parametres :
#   <Id>     : No de la boite (""=courante, -1=toute, 0...n=numero)
#
# Retour     :
#   <Files>  : Liste des fichiers.
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc FieldBox::GetContent { { Id "" } } {
   variable Data

   #----- Boite courante

   if { $Id=="" } {
      if { $Data(Current)!="" } {
         upvar #0 FieldBox::Data${FieldBox::Data(Current)}::Data data
         return [join $data(FieldList)]
      } else {
         return ""
      }
   }

   #----- Numero de boite

   if { $Id!=-1 } {
      upvar #0 FieldBox::Data${Id}::Data data
      return [join $data(FieldList)]
   }

   #----- Toute les boites

   set fields {}

   foreach box $Data(BoxList) {

      upvar #0 FieldBox::Data${box}::Data data

      if { $fields!="" } {
         set fields [list $fields]
      }
      eval set fields \[concat $fields $data(FieldList)\]
   }
   return $fields
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::GetFID>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la liste de tout les id de fichiers de toutes les boites.
#
# Parametres :
#   <args>   : No de la boite (""=courante, -1=toute, 0...n=numero)
#
# Retour     :
#   <FID>    : Liste des id de fichiers.
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc FieldBox::GetFID { { Id "" } } {
   variable Data

   #----- Boite courante

   if { $Id=="" } {
      upvar #0 FieldBox::Data${FieldBox::Data(Current)}::Data data
      set Id $data(FIDList)
   }

   #----- Numero de boite

   if { $Id!=-1 } {
      upvar #0 FieldBox::Data${Id}::Data data
      return [join $data(FIDList)]
   }

   #----- Toute les boites

   return $Data(FIDList)
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::GetFile>
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

proc FieldBox::GetFile { args } {
   variable Data

   #----- Boite courante

   if { $args=="" } {
      set args $FieldBox::Data(Current)
   }

   #----- Numero de boite

   if { $args!=-1 } {
      upvar #0 FieldBox::Data${args}::Data data
      return $data(FileList)
   }

   #----- Toute les boites

   set files {}

   foreach box $Data(BoxList) {

      upvar #0 FieldBox::Data${box}::Data data

      if { $files!="" } {
         set files [list $fields]
      }
      eval set files \[concat $fields $data(FileList)\]
   }
   return $files
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::GetSelected>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la liste de tout les champs selectionnes parmi
#            toutes les boites.
#
# Parametres :
#   <args>   : No de la boite (""=courante, -1=toute, 0...n=numero)
#
# Retour     :
#   <Fields> : Liste des parametres de tous les champs selectionnes
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc FieldBox::GetSelected { args } {
   variable Data

   set fields {}

   #----- Boite courante

   if { $args=="" } {
      foreach idx [.fieldbox$Data(Current).data.list curselection] {
         lappend fields [.fieldbox$Data(Current).data.list get $idx]
      }
      return $fields
   }

   #----- Numero de boite

   if { $args!=-1 } {
      foreach idx [.fieldbox${args}.data.list curselection] {
         lappend fields [.fieldbox${args}.data.list get $idx]
      }
      return $fields
   }

   #----- Toute les boites

   foreach box $Data(BoxList) {
      foreach idx [.fieldbox$box.data.list curselection] {
         lappend fields [.fieldbox$box.data.list get $idx]
      }
   }
   return $fields
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::FileSelect>
# Creation : Juillet 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Fermeture d'un fichier standard.
#
# Parametres :
#  <No>      : Numero de FieldBox
#  <File>    : Path complet du fichier
#  <FID>     : Numero du fichier
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc FieldBox::FileSelect { No File FID } {

   #----- On recupere les variable du module courant

   upvar #0 FieldBox::Data${No}::Data data

   set data(FileCurrent) $File
   set data(FileInfo) "[format %02i $FID] ...[string range $File end-35 end]"
   set data(FID) $FID
   FieldBox::Restrict $No
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::InfoCommand>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Execute la commande de recherche de l'information sur le champs.
#
# Parametres :
#  <No>      : Numero de FieldBox
#  <Index>   : Index de l'item selectionne dans la liste.
#
# Remarques :
#    -Cette commande est appelee par le package ListboxBubble.
#
#-------------------------------------------------------------------------------

proc FieldBox::InfoCommand { No Index } {
   global GDefs
   variable Data

   set line  [.fieldbox$No.data.list get $Index]
   set date  ""
   set nv    [lindex $line 0]
   set tv    [lindex $line 1]
   set level "[lindex $line 2] [lindex $line 3]"
   set id    [lindex $line end-5]_[lindex $line end-4]
   set date  [lindex $line end-6]
   set ip1   [lindex $line end-3]

   #----- Date de validite
   #----- Pour contourner l'erreur des date de descripteur de grille (date: 19010101..)
   catch { set date [DateStuff::StringDateFromSeconds [clock scan "[string range $date 0 7] [string range $date 8 end]" -timezone :UTC] $GDefs(Lang)] }

   if { [fstddict isvar $nv] } {
      eval set info \[format \"%s  (%s)\" [fstddict varinfo $nv -lang $GDefs(Lang) -short -units]\]
   } else {
      set info  "$nv ??? (???)"
   }   
   
   if { [fstddict istype $tv] } {
      append info  "\n[fstddict typeinfo $tv -lang $GDefs(Lang) -short]"
   } else {
      append info  "\n???"
   }

   append info " $level\n$date\n($id)"

   return $info
}

#----------------------------------------------------------------------------
# Nom      : <FieldBox::Init>
# Creation : Decembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise les variable d'une nouvelle boite FieldBox.
#
# Parametres   :
#   <No>       : Numero de la boite
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldBox::Init { No } {

   namespace eval Data$No {
      variable Data
      variable Sel

      #----- Variables relatives champs

      set Data(NbRead)   0                ;#Nombre de champs lu
      set Data(NbShow)   0                ;#Nombre de champs affichee
      set Data(ShowRead) 0                ;#Nombre de champs affichee sur le nombre de champs lu

      set Data(FieldList)   ""            ;#Liste des champs complete
      set Data(FileCurrent) ""            ;#Fichier actif
      set Data(FileInfo)    ""            ;#Nom du fichier affiche
      set Data(FileList)    ""            ;#Liste des fichiers ouverts
      set Data(FIDList)     ""            ;#Liste des identificateurs de fichiers
      set Data(TypeList)    {}            ;#Liste des types de fichiers

      set Data(FID)     ""
      set Data(Var)     ""
      set Data(Type)    ""
      set Data(Level)   ""
      set Data(IP2)     ""
      set Data(IP3)     ""
      set Data(Eticket) ""
      set Data(Date)    ""
   }
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::Insert>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Insere les champs dans la liste.
#
# Parametres :
#  <No>      : Numero de FieldBox
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc FieldBox::Insert { No } {
   global   env
   variable Data

   #----- On recupere les variable du module courant

   upvar #0 FieldBox::Data${No}::Data data

   set id .fieldbox$No

   $id.data.list delete 0 end
   $id config -cursor watch
   update idletasks

   set data(NbRead)  0

   foreach fields $data(FieldList) {
      foreach field $fields {

         #----- Extraire les informations

         set var     [lindex $field 0]
         set tvar    [lindex $field 1]
         set level   [lrange $field 2 3]
         set ip2     [lrange $field 4 5]
         set ip3     [lrange $field 6 7]
         set eticket [lindex $field 8]
         set date    [lindex $field 9]

         #----- Determiner la liste de selection

         set AVAR($var)     1
         set ATYP($tvar)    1
         set ALEV($level)   1
         set AIP2($ip2)     1
         set AIP3($ip3)     1
         set AETI($eticket) 1
         set ADAT($date)    1
      }
      #----- Inserer les donnees dans la liste
      eval .fieldbox$No.data.list insert end $fields
   }

   set data(NbRead) [.fieldbox$No.data.list index end]
   set data(NbShow) $data(NbRead)

   set data(FID)     ""
   set data(Var)     ""
   set data(Type)    ""
   set data(Level)   ""
   set data(IP2)     ""
   set data(IP3)     ""
   set data(Eticket) ""
   set data(Date)    ""

   set data(ShowRead) "$data(NbShow)/$data(NbRead)"

   #----- Update des listes de selections

   SelectBox::Insert $id.header.var     [array names AVAR *]
   SelectBox::Insert $id.header.type    [array names ATYP *]
   SelectBox::Insert $id.header.level   [array names ALEV *]
   SelectBox::Insert $id.header.ip2     [array names AIP2 *]
   SelectBox::Insert $id.header.ip3     [array names AIP3 *]
   SelectBox::Insert $id.header.eticket [array names AETI *]
   SelectBox::Insert $id.header.date    [array names ADAT *]

   $id config -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::Close>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Ferme tout les fichiers ouvert et detruit la fenetre.
#
# Parametres :
#  <No>      : Numero de FieldBox
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc FieldBox::Close { No } {
   variable Data

   if { ![winfo exists .fieldbox${No}] } {
      return
   }

   #----- Fermer tout les fichiers

   set FieldBox::Data${No}::Data(FileCurrent) -1
   FieldBox::FileClose $No

   #----- Annuler toute selection

   .fieldbox${No}.data.list selection clear 0 end
   FieldBox::Select

   #----- Detruire l'interface

   SelectBox::Destroy .fieldbox${No}.header.var
   SelectBox::Destroy .fieldbox${No}.header.type
   SelectBox::Destroy .fieldbox${No}.header.level
   SelectBox::Destroy .fieldbox${No}.header.ip2
   SelectBox::Destroy .fieldbox${No}.header.ip3
   SelectBox::Destroy .fieldbox${No}.header.eticket
   SelectBox::Destroy .fieldbox${No}.header.date

   ListboxBubble::Destroy .fieldbox${No}.data.list
   destroy .fieldbox${No}

   #----- Cleanup des variables

   set idx [lsearch -exact $Data(BoxList) $No]
   set Data(BoxList) [lreplace $Data(BoxList) $idx $idx]
   namespace delete Data${No}
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::CloseAll>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Ferme toute les boites de champs.
#
# Parametres :
#  <No>      : Numero de FieldBox
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc FieldBox::CloseAll { } {
   variable Data

   foreach no $Data(BoxList) {
      FieldBox::Close $no
   }
}

#----------------------------------------------------------------------------
# Nom      : <FieldBox::Raise>
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

proc FieldBox::Raise { } {
   variable Data

   foreach box $Data(BoxList) {
      wm deiconify .fieldbox$box
   }
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::Restrict>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue une restriction dans les champs affichees.
#
# Parametres :
#  <No>      : Numero de FieldBox
#  <args>    : Valeur des parametres du SelectBox
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc FieldBox::Restrict { No args } {
   variable Data

   .fieldbox${No} configure -cursor watch
   update idletasks

   #----- On recupere les variable du module courant

   upvar #0 FieldBox::Data${No}::Data data

   #----- Creer la chaine de selection

   if { $data(Var)=="" } {
      set svar "\\S+"
   } else {
      set svar ([join [string map { + \\\\+ ^ \\\\^ } $data(Var)] |])
   }
   if { $data(Type)=="" } {
      set styp "\\S+"
   } else {
      set styp ([join $data(Type) |])
   }
   if { $data(Level)=="" } {
      set sip1 "\\S+ \\S+"
   } else {
      set sip1 ([join $data(Level) |])
   }
   if { $data(IP2)=="" } {
      set sip2 "\\S+ \\S+"
   } else {
      set sip2 ([join $data(IP2) |])
   }
   if { $data(IP3)=="" } {
      set sip3 "\\S+ \\S+"
   } else {
      set sip3 ([join $data(IP3) |])
   }
   if { $data(Eticket)=="" } {
      set seti "\\S+"
   } else {
      set seti ([join [string map { + \\\\+ } $data(Eticket)] |])
   }
   if { $data(Date)=="" } {
      set sdat "\\d+"
   } else {
      set sdat ([join $data(Date) |])
   }
   if { $data(FID)=="" } {
      set sfid "\\d+"
   } else {
      set sfid ($data(FID))
   }

   set str  "^$svar\\s+$styp\\s+$sip1\\s+$sip2\\s+$sip3\\s+$seti\\s+$sdat $sfid \\d+ \\d+ \\d+ \\d+ .+field$"
   
   .fieldbox$No.data.list delete 0 end
   set data(NbShow) 0

   foreach fields $data(FieldList) {
      eval .fieldbox$No.data.list insert end [lsearch -all -inline -regexp $fields $str]
   }
   set data(NbShow) [.fieldbox$No.data.list index end]
   set data(ShowRead) "$data(NbShow)/$data(NbRead)"
   .fieldbox${No} configure -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::RestrictClear>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Reinitialise les listes pour effacer les restrictions.
#
# Parametres :
#  <No>      : Numero de FieldBox
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc FieldBox::RestrictClear { No } {
   variable Sel

   SelectBox::Clear .fieldbox$No.header.var     0
   SelectBox::Clear .fieldbox$No.header.type    0
   SelectBox::Clear .fieldbox$No.header.level   0
   SelectBox::Clear .fieldbox$No.header.ip2     0
   SelectBox::Clear .fieldbox$No.header.ip3     0
   SelectBox::Clear .fieldbox$No.header.eticket 0
   SelectBox::Clear .fieldbox$No.header.date    1
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::RestrictSet>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue une restriction dans les champs affichees.
#
# Parametres :
#  <No>      : Numero de FieldBox
#  <Var>     : Variable a restreindre (Var Type Level IP2 IP3 Eticket Date
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc FieldBox::RestrictSet { No Var Value } {

   upvar #0 FieldBox::Data${No}::Data data

   set data($Var) $Value
   FieldBox::Restrict $No
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::PasteClick>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Selection du champs a etre utilise.
#
# Parametres :
#  <No>      : Numero de FieldBox
#  <Y>       : Coordonnee y du curseur
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc FieldBox::PasteClick { Widget Y } {

   if { ![llength [set idxs [$Widget curselection]]] } {
      set idxs  [$Widget nearest $Y]
      $Widget   selection set $idxs
   }

   set idx [lindex $idxs 0]
   
   set data [$Widget get $idx]
   return field([lindex $data end-5],[lindex $data end-4])  
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::Select>
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

proc FieldBox::Select { } {
   variable Data

   if { ![llength $Data(BoxList)] } {
      return
   }

   if { [winfo exists $Page::Data(Canvas)] } {
      $Page::Data(Canvas) config -cursor watch
   }
   . config -cursor watch
   update idletasks

   #----- Recuperer les champs selectionne

   set flds ""

   foreach box $Data(BoxList) {

      .fieldbox$box config -cursor watch

      #----- Recuperer les champs selectionner

      set idxs [.fieldbox$box.data.list curselection]

      foreach idx $idxs {

         set info [.fieldbox$box.data.list get $idx]
         set fid  [lindex $info end-5]
         set fidx [lindex $info end-4]
         set type [lindex $info end]

         set fld  FLD${fid}_${fidx}
         set lidx [lsearch $FSTD::Data(List) $fld]

         if { $lidx==-1 } {
            eval $type read $fld $fid $fidx
            
            #----- If this is a text field
            set nv [fstdfield define $fld -NOMVAR]
            if { $nv=="INFO" || $nv=="META" || $nv=="TEXT" || $nv=="PROJ" || ($nv=="OL" && [fstdfield define $fld -TYPVAR]=="X") } {
               if { [winfo exists .fieldboxparams] } {
                  FieldParams::GetInfo   $fld
                  FieldParams::GetMatrix $fld
               }
               FieldBox::Show $fld
               continue
            }
            
            #---- If this is a vertical grid (profile/xsection)
            if { [fstdfield define $fld -GRTYP]=="V" } {
               if { [fstdfield define $fld -NI]>1 } {
                  #----- If more than 1 in I, this is an xsection
                  if { $Graph::Data(Graph)=="" || $Graph::Data(Type$Graph::Data(Graph))!="Section" } {
                     Graph::Params
                     if { ![SPI::ObjectAdd Graph ::Section] } {
                        continue
                     } 
                     Graph::PosDel $Graph::Data(Graph) $Graph::Data(Type$Graph::Data(Graph))
                  }
               
               } else {
                  #----- Otherwise, this is a profile
                  if { $Graph::Data(Graph)=="" || ($Graph::Data(Type$Graph::Data(Graph))!="Profile" && $Graph::Data(Type$Graph::Data(Graph))!="TimeSection") } {
                     Graph::Params
                     if { ![SPI::ObjectAdd Graph ::Profile] } {
                        continue
                     }                 
                     Graph::PosDel $Graph::Data(Graph) $Graph::Data(Type$Graph::Data(Graph))
                  }
               }
            }
            
            #----- Si le champs n'etait pas deja selectionnee

            eval $type stats $fld -tag \"$Page::Data(Frame) $Page::Data(VP) $box\"
            lappend Viewport::Data(Data$Page::Data(VP)) $fld
            lappend flds $fld
         } else {
            set vp [lindex [fstdfield stats $fld -tag] 1]
            if { [Page::Registered All Viewport $vp]==-1 } {
               .fieldbox$box.data.list selection clear $idx
            } else {
               lappend flds $fld
            }
         }
      }
      .fieldbox$box config -cursor left_ptr
   }

   #----- Eliminer les champs qui ne sont plus selectionne

   set tofree {}
   foreach fld $FSTD::Data(List) {
      if { [lsearch -exact $flds $fld]==-1 && [fstdfield is $fld True] } {
         Viewport::AssignedTo $fld fr vp
         if { [Page::Registered All Viewport $vp]!=-1 } {
            Viewport::UnAssign $fr $vp $fld -1
         }
         lappend tofree $fld
      }
   }

   set FSTD::Data(List) $flds
   FSTD::ParamUpdate

   if { [winfo exists .fieldboxparams] } {
      FieldParams::GetInfo   [lindex $flds 0]
      FieldParams::GetMatrix [lindex $flds 0]
   }

   #----- Vider la liste de frame des animations

   Animator::EmptyPlayList

   foreach frame $Page::Data(Frames) {
      Viewport::UpdateData    $frame
      Page::Update            $frame
      Page::UpdateCommand     $frame
   }

   foreach fld $tofree {
      fstdfield free $fld
   }

   if { [winfo exists $Page::Data(Canvas)] } {
      $Page::Data(Canvas) config -cursor left_ptr
   }
   . config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <FieldBox::PopUp>
# Creation : Juin 2013 - J.P. Gauthier - CMC/CMOE
#
# But      : Configurer le popup selon le type de donnees.
#
# Parametres :
#   <X>      : Position X du popup
#   <Y>      : Position Y du popup
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldBox::PopUp { X Y  } {
   variable Data

   if { ![llength [set idxs [.fieldbox$Data(Current).data.list curselection]]] } {
       .fieldmenu entryconfigure 4 -state disabled
       .fieldmenu entryconfigure 5 -state disabled
   } else {
      set info [.fieldbox$Data(Current).data.list get [lindex $idxs 0]]
   
      switch [lindex $info end] {
         "fstdfield" { .fieldmenu entryconfigure 4 -state normal
                       .fieldmenu entryconfigure 5 -state normal
                     }
         "gribfield" { .fieldmenu entryconfigure 4 -state disabled
                       .fieldmenu entryconfigure 5 -state disabled
                     }
      }
   }
   tk_popup .fieldmenu $X $Y
}

#-------------------------------------------------------------------------------
# Nom      : <FieldBox::Show>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher le contenue d'un champs
#
# Parametres :
#  <Field>   : Identificateur du champs
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc FieldBox::Show { Field } {
   global GDefs

   set var [fstdfield define $Field -NOMVAR]
   set title "$var [fstdfield define $Field -TYPVAR] [fstdfield define $Field -ETIKET]"

   if { $var=="TEXT" || $var=="META" || $var=="PROJ" } {
      Dialog::Text .infocode $title [MetData::TextDecode $Field] 100 50
   } else {
      Dialog::Text .infocode $title [Info::Format [FSTD::Data $Field]] 100 50
   }
}

#----------------------------------------------------------------------------
# Nom      : <FieldParams::Window>
# Creation : Juin 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une fenetre d'affichage / edition des parametres FSTD.
#
# Parametres   :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

namespace eval FieldParams {
   variable Lbl
   variable Param
   variable Data

   set Param(Geom)     { 250x570+[winfo rootx .]+[winfo rooty .] }
   set Param(Title)    { "Détails du champs" "Field details" }

   set Lbl(Close)      { "Fermer" "Close" }
   set Lbl(Apply)      { "Appliquer" "Apply" }
   set Lbl(Save)       { "Enregistrer" "Save" }
   set Lbl(Values)     { "Valeurs" "Values" }
   set Lbl(Info)       { "Informations" "Informations" }

   set Data(Field)     ""
   set Data(ParamsOut) { NOMVAR TYPVAR IP1 IP2 IP3 ETIKET DATEO DEET NPAS NI NJ NK NBITS DATYP GRTYP IG1 IG2 IG3 IG4 SWA LNG DLTF UBC EX1 EX2 EX3 }
   set Data(ParamsIn)  { NOMVAR TYPVAR IP1 IP2 IP3 ETIKET DATEO DEET NPAS NBITS IG1 IG2 IG3 IG4 GRTYP }
   set Data(Lock)      False
}

proc FieldParams::Window { { Field "" } } {
   global GDefs
   variable Lbl
   variable Param
   variable Data

   set Data(Lock) False
   set Data(Done) False

   if { ![winfo exists .fieldboxparams] } {
      toplevel .fieldboxparams

      wm title         .fieldboxparams [lindex $Param(Title) $GDefs(Lang)]
      wm resizable     .fieldboxparams 1 1
      eval wm geometry .fieldboxparams $Param(Geom)
      wm transient     .fieldboxparams .
      wm protocol      .fieldboxparams  WM_DELETE_WINDOW { Viewport::FollowerRemove FieldParams; FieldParams::GetInfo ""; destroy .fieldboxparams }

      TabFrame::Create .fieldboxparams.tab 1 "FieldParams::GetMatrix \$FieldParams::Data(Field); catch"
      set Data(Tab1) [TabFrame::Add .fieldboxparams.tab 1 [lindex $Lbl(Info) $GDefs(Lang)] True]

      frame $Data(Tab1).lbl
         label $Data(Tab1).lbl.nomvar -width 7 -anchor w -text NOMVAR
         label $Data(Tab1).lbl.typvar -width 7 -anchor w -text TYPVAR
         label $Data(Tab1).lbl.ip1 -width 7 -anchor w -text IP1
         label $Data(Tab1).lbl.ip2 -width 7 -anchor w -text IP2
         label $Data(Tab1).lbl.ip3 -width 7 -anchor w -text IP3
         label $Data(Tab1).lbl.etiket -width 7 -anchor w -text ETIKET
         label $Data(Tab1).lbl.dateo -width 7 -anchor w -text DATEO
         label $Data(Tab1).lbl.deet -width 7 -anchor w -text DEET
         label $Data(Tab1).lbl.npas -width 7 -anchor w -text NPAS
         label $Data(Tab1).lbl.ni -width 7 -anchor w -text NI
         label $Data(Tab1).lbl.nj -width 7 -anchor w -text NJ
         label $Data(Tab1).lbl.nk -width 7 -anchor w -text NK
         label $Data(Tab1).lbl.nbits -width 7 -anchor w -text NBITS
         label $Data(Tab1).lbl.datyp -width 7 -anchor w -text DATYP
         label $Data(Tab1).lbl.grtyp -width 7 -anchor w -text GRTYP
         label $Data(Tab1).lbl.ig1 -width 7 -anchor w -text IG1
         label $Data(Tab1).lbl.ig2 -width 7 -anchor w -text IG2
         label $Data(Tab1).lbl.ig3 -width 7 -anchor w -text IG3
         label $Data(Tab1).lbl.ig4 -width 7 -anchor w -text IG4
         label $Data(Tab1).lbl.swa -width 7 -anchor w -text SWA
         label $Data(Tab1).lbl.lng -width 7 -anchor w -text LNG
         label $Data(Tab1).lbl.dltf -width 7 -anchor w -text DLTF
         label $Data(Tab1).lbl.ubc -width 7 -anchor w -text UBC
         label $Data(Tab1).lbl.ex1 -width 7 -anchor w -text EX1
         label $Data(Tab1).lbl.ex2 -width 7 -anchor w -text EX2
         label $Data(Tab1).lbl.ex3 -width 7 -anchor w -text EX3

      frame $Data(Tab1).var
         entry $Data(Tab1).var.nomvar -bg $GDefs(ColorLight) -bd 1 -width 4 -textvariable FieldParams::Data(NOMVAR)
         entry $Data(Tab1).var.typvar -bg $GDefs(ColorLight) -bd 1 -width 2 -textvariable FieldParams::Data(TYPVAR)
         entry $Data(Tab1).var.ip1 -bg $GDefs(ColorLight) -bd 1 -width 8 -textvariable FieldParams::Data(IP1)
         entry $Data(Tab1).var.ip2 -bg $GDefs(ColorLight) -bd 1 -width 8 -textvariable FieldParams::Data(IP2)
         entry $Data(Tab1).var.ip3 -bg $GDefs(ColorLight) -bd 1 -width 8 -textvariable FieldParams::Data(IP3)
         entry $Data(Tab1).var.etiket -bg $GDefs(ColorLight) -bd 1 -width 12 -textvariable FieldParams::Data(ETIKET)
         entry $Data(Tab1).var.dateo -bg $GDefs(ColorLight) -bd 1 -width 12 -textvariable FieldParams::Data(DATEO)
         entry $Data(Tab1).var.deet -bg $GDefs(ColorLight) -bd 1 -width 12 -textvariable FieldParams::Data(DEET)
         entry $Data(Tab1).var.npas -bg $GDefs(ColorLight) -bd 1 -width 12 -textvariable FieldParams::Data(NPAS)
         entry $Data(Tab1).var.ni -bg $GDefs(ColorLight) -bd 1 -width 8 -textvariable FieldParams::Data(NI) -state disabled
         entry $Data(Tab1).var.nj -bg $GDefs(ColorLight) -bd 1 -width 8 -textvariable FieldParams::Data(NJ) -state disabled
         entry $Data(Tab1).var.nk -bg $GDefs(ColorLight) -bd 1 -width 8 -textvariable FieldParams::Data(NK) -state disabled
         entry $Data(Tab1).var.nbits -bg $GDefs(ColorLight) -bd 1 -width 3 -textvariable FieldParams::Data(NBITS)
         entry $Data(Tab1).var.datyp -bg $GDefs(ColorLight) -bd 1 -width 3 -textvariable FieldParams::Data(DATYP)
         entry $Data(Tab1).var.grtyp -bg $GDefs(ColorLight) -bd 1 -width 1 -textvariable FieldParams::Data(GRTYP)
         entry $Data(Tab1).var.ig1 -bg $GDefs(ColorLight) -bd 1 -width 6 -textvariable FieldParams::Data(IG1)
         entry $Data(Tab1).var.ig2 -bg $GDefs(ColorLight) -bd 1 -width 6 -textvariable FieldParams::Data(IG2)
         entry $Data(Tab1).var.ig3 -bg $GDefs(ColorLight) -bd 1 -width 6 -textvariable FieldParams::Data(IG3)
         entry $Data(Tab1).var.ig4 -bg $GDefs(ColorLight) -bd 1 -width 6 -textvariable FieldParams::Data(IG4)
         entry $Data(Tab1).var.swa -bg $GDefs(ColorLight) -bd 1 -width 12 -textvariable FieldParams::Data(SWA) -state disabled
         entry $Data(Tab1).var.lng -bg $GDefs(ColorLight) -bd 1 -width 12 -textvariable FieldParams::Data(LNG) -state disabled
         entry $Data(Tab1).var.dltf -bg $GDefs(ColorLight) -bd 1 -width 12 -textvariable FieldParams::Data(DLTF) -state disabled
         entry $Data(Tab1).var.ubc -bg $GDefs(ColorLight) -bd 1 -width 12 -textvariable FieldParams::Data(UBC) -state disabled
         entry $Data(Tab1).var.ex1 -bg $GDefs(ColorLight) -bd 1 -width 12 -textvariable FieldParams::Data(EX1) -state disabled
         entry $Data(Tab1).var.ex2 -bg $GDefs(ColorLight) -bd 1 -width 12 -textvariable FieldParams::Data(EX2) -state disabled
         entry $Data(Tab1).var.ex3 -bg $GDefs(ColorLight) -bd 1 -width 12 -textvariable FieldParams::Data(EX3) -state disabled
      pack $Data(Tab1).lbl $Data(Tab1).var -side left -padx 5 -pady 5 -anchor nw

      foreach param $Data(ParamsOut) {
         pack $Data(Tab1).lbl.[string tolower $param] -side top -anchor nw -fill y
         pack $Data(Tab1).var.[string tolower $param] -side top -anchor nw
      }

      set Data(Tab2) [TabFrame::Add .fieldboxparams.tab 1 [lindex $Lbl(Values) $GDefs(Lang)] True]

      frame $Data(Tab2).data -relief sunken -bd 1
         scrollbar $Data(Tab2).data.scrolly -relief sunken -bd 1 -width 10 -command "$Data(Tab2).data.table yview"
         scrollbar $Data(Tab2).data.scrollx -relief sunken -bd 1 -width 10 -orient horizontal -command "$Data(Tab2).data.table xview"
         table $Data(Tab2).data.table -relief sunken -bd 1 -bg $GDefs(ColorLight) -variable FieldParams::Table \
         -resizeborders col -anchor w -titlecols 1 -titlerows 1 -rows 1 -cols 1 -multiline False -drawmode fast \
         -yscrollcommand "$Data(Tab2).data.scrolly set" -xscrollcommand "$Data(Tab2).data.scrollx set" -width 1 -height 1 -selectmode extended \
         -validatecommand "FieldParams::SetMatrix %c %r %S" -validate True -highlightbackground $GDefs(ColorHighLight) -invertselected True
         pack $Data(Tab2).data.scrollx -side bottom -fill x
         pack $Data(Tab2).data.scrolly -side left -fill y
         pack $Data(Tab2).data.table -side left -fill both -expand true -before $Data(Tab2).data.scrolly
      pack $Data(Tab2).data -side top -fill both -expand true -padx 5 -pady 5

      checkbutton $Data(Tab2).data.table.lock -bd 1 -variable FieldParams::Data(Lock) -relief raised -indicatoron False -image TABLETO \
         -command { if $FieldParams::Data(Lock) { Viewport::FollowerAdd FieldParams } else { Viewport::FollowerRemove FieldParams } }
      $Data(Tab2).data.table window configure 0,0 -window $Data(Tab2).data.table.lock -sticky nsew

      frame .fieldboxparams.cmd
         button .fieldboxparams.cmd.close  -text [lindex $Lbl(Close) $GDefs(Lang)] -bd 1 -command { Viewport::FollowerRemove FieldParams; destroy .fieldboxparams; set FieldParams::Data(Done) True }
         button .fieldboxparams.cmd.apply  -text [lindex $Lbl(Apply) $GDefs(Lang)] -bd 1 -command { FieldParams::SetInfo $FieldParams::Data(Field); Viewport::FollowerRemove FieldParams; destroy .fieldboxparams; set FieldParams::Data(Done) True }
         button .fieldboxparams.cmd.save   -text [lindex $Lbl(Save) $GDefs(Lang)]  -bd 1 -command { FieldParams::SetInfo $FieldParams::Data(Field) True; Viewport::FollowerRemove FieldParams; destroy .fieldboxparams; set FieldParams::Data(Done) True }
         pack .fieldboxparams.cmd.close .fieldboxparams.cmd.apply .fieldboxparams.cmd.save -side left
         pack .fieldboxparams.cmd.save -side left
      pack .fieldboxparams.tab -side top -fill both -expand True -padx 5 -pady 5 -ipadx 5
      pack .fieldboxparams.cmd -side top -anchor e -padx 5 -pady 5

   } else {
      wm deiconify .fieldboxparams
      raise        .fieldboxparams
   }

   TabFrame::Select .fieldboxparams.tab 0

   FieldParams::GetInfo $Field
   FieldParams::GetMatrix $Field

   if { $Field!="" } {
      tkwait variable FieldParams::Data(Done)
   }
}

proc FieldParams::Follower { Page Canvas VP Lat Lon X Y } {
   variable Data

   if { [fstdfield is $Data(Field)] } {
      set ij [fstdfield stats $Data(Field) -coordpoint $Lat $Lon]
      set index [expr int(round([lindex $ij 1])+1)],[expr int(round([lindex $ij 0])+1)]
      $Data(Tab2).data.table see $index
      $Data(Tab2).data.table selection clear all
      $Data(Tab2).data.table selection set $index
   }
}

#----------------------------------------------------------------------------
# Nom      : <FieldParams::GetInfo>
# Creation : Juin 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Instaurer les parametres d'un champs dans les variables de l'interface
#            des parametres.
#
# Parametres  :
#   <Field>   : Identificateur du champs
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldParams::GetInfo { { Field "" } } {
   global GDefs
   variable Param
   variable Data

   set Data(Field) $Field

   if { [fstdfield is $Field] } {
      foreach param $Data(ParamsOut) {
         set Data($param) [fstdfield define $Field -$param]
      }
      wm title .fieldboxparams "[lindex $Param(Title) $GDefs(Lang)]: [fstdfield define $Field -NOMVAR] ([fstdfield stats $Field -level] [fstdfield stats $Field -leveltype]) [clock format [fstdstamp toseconds [fstdfield define $Field -DATEV]] -format "%H:%M %Y%m%d" -timezone :UTC]"
   } else {
      foreach param $Data(ParamsOut) {
         set Data($param) ""
      }
      wm title .fieldboxparams "[lindex $Param(Title) $GDefs(Lang)]"
   }
}

#----------------------------------------------------------------------------
# Nom      : <FieldParams::SetInfo>
# Creation : Juin 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Instaurer les parametres d'un champs dans les variables de l'interface
#            des parametres.
#
# Parametres  :
#   <Field>   : Identificateur du champs
#   <Write>   : Write the modified field to it's file
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldParams::SetInfo { { Field "" } { Write False } } {
   variable Data

   if { [fstdfield is $Field] } {
      foreach param $Data(ParamsIn) {
         fstdfield define $Field -$param $Data($param)
      }
      if { $Write } {
         if { [set file [FileBox::Create .fieldboxparams "" Save [list $FileBox::Type(FSTD)]]]!="" } {
            fstdfile open FIELDPARAMS write $file
            fstdfield write $Field FIELDPARAMS 0 True
            fstdfile close FIELDPARAMS
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <FieldParams::GetMatrix>
# Creation : Fevrier 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Instaurer la matrice des valeurs d'un champs dans les variables de l'interface
#            des parametres.
#
# Parametres  :
#   <Field>   : Identificateur du champs
#
# Retour:
#
# Remarques :
#     - Si il n'y as pas de champs specifie, on utilise la selection de la boite de champs
#
#----------------------------------------------------------------------------

proc FieldParams::GetMatrix { { Field "" } } {
   global GDefs
   variable Lbl
   variable Param
   variable Data

   set Data(Field) $Field

   if { [TabFrame::Current .fieldboxparams.tab]==1 && [fstdfield is $Field] } {

      .fieldboxparams configure -cursor watch
      update idletasks

      set rows [fstdfield define $Field -NJ]
      set cols [fstdfield define $Field -NI]
      set rs [lindex [$Data(Tab2).data.table configure -rows] 4]
      set cs [lindex [$Data(Tab2).data.table configure -cols] 4]

      $Data(Tab2).data.table configure -rows [expr $rows+1] -cols [expr $cols+1]
      fstdfield stats $Field -matrix FieldParams::Table

      for { set c 0 } { $c<$cols } { incr c } {
         if {![winfo exists $Data(Tab2).data.table.c$c] } {
            label $Data(Tab2).data.table.c$c -bd 1 -relief raised
         }
         $Data(Tab2).data.table.c$c configure -text $c
         $Data(Tab2).data.table window configure 0,[expr $c+1] -window $Data(Tab2).data.table.c$c -sticky nsew
      }
      for { set c $c } { $c<$cs } { incr c } {
         destroy radiobutton $Data(Tab2).data.table.c$c
      }
      for { set r 0 } { $r<$rows } { incr r } {
         if {![winfo exists $Data(Tab2).data.table.r$r] } {
            label $Data(Tab2).data.table.r$r -bd 1 -relief raised
         }
         $Data(Tab2).data.table.r$r configure -text $r
         $Data(Tab2).data.table window configure [expr $r+1],0 -window $Data(Tab2).data.table.r$r -sticky nsew
      }
      for { set r $r } { $r<$rs } { incr r } {
         destroy radiobutton $Data(Tab2).data.table.r$r
      }
      .fieldboxparams configure -cursor left_ptr
   }
}

#----------------------------------------------------------------------------
# Nom      : <FieldParams::SetMatrix>
# Creation : Fevrier 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Chander la valeur du champs selon les valeurs de la table.
#
# Parametres  :
#   <I>       : Colone
#   <J>       : Ligne
#   <Value>   : Valeur
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc FieldParams::SetMatrix { I J Value } {
   variable Data

   catch { fstdfield stats $Data(Field) -gridvalue [expr $I-1] [expr $J-1] $Value }

   foreach frame $Page::Data(Frames) {
      Page::Update            $frame
      Page::UpdateCommand     $frame
   }
   return True
}
