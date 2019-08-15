#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Interface d'affichage de base de données (Iso, Oil, etc.)
# Fichier   : DBBox.tcl
# Creation  : Avril 2016 - E. Legault-Ouellet - CMC/CMOE
#
# Description:
#
#     Cette interface permet l'affichage et la sélection d'un item d'une base
#     de données textuelle
#
# Fonctions:
#     DBBox::Create      { Parent DB {Command ""} }
#     DBBox::CheckDB     { DB }
#     DBBox::ReadDB      { DB }
#     DBBox::Search      { DB Column Pattern }
#     DBBox::SearchClear { DB {Columns {}} }
#     DBBox::Sort        { DB W }
#     DBBox::GetFmt      { DB W }
#     DBBox::Update      { DB W }
#
#
#===============================================================================

package provide DBBox 1.0

catch { SPI::Splash "Loading Widget Package DBBox 1.0" }

namespace eval DBBox {
   variable Lbl
   variable Param

   set Lbl(Close)    {"Fermer" "Close"}
}

#-------------------------------------------------------------------------------
# Nom      : <DBBox::Create>
# Creation : Mai 1997 - J.P. Gauthier - CMC/CMOE
#
# But      : Gere la fenetre de selection des isotopes
#
# Parametres  :
#   <Parent>   : Fenetre Parent
#   <Cmd>      : Commande à exécuter pour chaque sélection
#
# Remarques :
#   - Si une commande est passe en parametre, elle est execute pour chaque
#     selection avec comme seul argument la ligne selectionnee.
#
# Retour:
#   <sel>   : La fonction retourne la derniere ligne selectionnee
#
#-------------------------------------------------------------------------------
proc DBBox::Create { Parent DB {Cmd ""} } {
   global   GDefs
   variable Lbl
   variable Data
   variable Search
   variable Param

   CheckDB $DB
   
   $Parent config -cursor X_cursor
   update idletasks

   toplevel .dbbox -bg $GDefs(ColorLight)
   wm transient .dbbox $Parent
   wm title .dbbox [lindex $Lbl(${DB}Title) $GDefs(Lang)]
   wm resizable .dbbox 0 1
   wm protocol .dbbox WM_DELETE_WINDOW { }

   #----- Create the header (title + searchbox)
   set hlst {}
   set h .dbbox.head
   frame $h
      foreach key $Param(${DB}Lst) w $Param(${DB}Width) lbl [lindex $Lbl(${DB}Lst) $GDefs(Lang)] {
         lappend hlst [frame $h.$key]
         #pack [label $h.$key.title -bd 1 -text $lbl -height 5 -relief raised -width $w] -side top -fill y
         pack [radiobutton $h.$key.title -bd 1 -text $lbl -height 5 -relief raised -width $w -indicatoron 0 -variable DBBox::Param(Sort) -value $key -command [list DBBox::Sort $DB .dbbox.body.box]] -side top -fill y
         pack [entry $h.$key.search -relief sunken -bd 1 -width $w -bg $GDefs(ColorLight) -textvariable DBBox::Search($DB$key)] -side top -fill x
         bind $h.$key.search <KeyRelease> [list DBBox::Update $DB .dbbox.body.box]
      }
      pack {*}$hlst -side left -fill both -expand true
   pack $h -side top -anchor w -fill x

   #----- Body (actual content)
   set b .dbbox.body
   frame $b
      listbox $b.box -relief sunken -bd 1 -exportselection false  -highlightthickness 0 \
         -yscrollcommand "$b.scroll set" -height 25 -width 170 -background $GDefs(ColorLight)
      pack $b.box -side left -expand true -fill both

      scrollbar $b.scroll -command "$b.box yview" -bd 1 -width 10  -highlightthickness 0
      pack $b.scroll  -side left -fill y
   pack $b -side top -anchor w -expand true -fill both

   #----- Afficher le bouton pour fermer la fenetre de selection
   set t .dbbox.tail
   frame $t
      button $t.close -bd 1 -text [lindex $Lbl(Close) $GDefs(Lang)] -highlightthickness 0 -command {
         if { [llength [.dbbox.body.box curselection]] } {
            set DBBox::Data(Result) [lindex $DBBox::Data(Shown) [.dbbox.body.box curselection]]
         } else {
            set DBBox::Data(Result) ""
         }
      }
      pack $t.close -side top -fill both -expand true
   pack $t -side top -fill x

   if { $Cmd != "" } {
      bind $b.box <B1-ButtonRelease> "$Cmd \[lindex \$DBBox::Data(Shown) \[%W nearest %y\]\]"
   }
   bind $b.box <Double-1> {if { [llength [.dbbox.body.box curselection]] } { set DBBox::Data(Result) [lindex $DBBox::Data(Shown) [.dbbox.body.box curselection]] }}

   update

   #----- Inserer les données dans la ScrollBox

   set Param(Sort) ""
   set Param(SortOrder) "increasing"
   SearchClear $DB
   Update $DB $b.box

   #----- Attente de la selection du fichier

   set prevgrab [grab current]
   grab .dbbox
   focus .dbbox.head.[lindex $Param(${DB}Lst) 0].search
   set Data(Result) ""
   tkwait variable DBBox::Data(Result)

   catch { destroy .dbbox }
   $Parent config -cursor left_ptr

   if { $prevgrab!="" } {
      grab $prevgrab
   }
   return $Data(Result)
}

#-------------------------------------------------------------------------------
# Nom      : <DBBox::CheckDB>
# Creation : Avril 2016 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Vérifie si la base de donnée est disponible et la rend disponible
#            si tel n'est pas le cas (lazy loading)
#
# Parametres :
#     <DB>  : La base de données à vérifier
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc DBBox::CheckDB { DB } {
   variable Data

   if { ![info exists Data(DB$DB)] } {
      DBBox::ReadDB $DB
   }
}

#-------------------------------------------------------------------------------
# Nom      : <DBBox::ReadDB>
# Creation : Avril 2016 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Lit la base de données spécifiée
#
# Parametres :
#     <DB>  : La base de données à lire
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc DBBox::ReadDB { DB } {
   variable Param
   variable Data
   
   #----- Make sure the file exists
   if { ![info exists Param(${DB}File)] } {
      set err "Invalid DB : '$DB'. No definition loaded for that DB. Available DB are : \[[lmap db [array names Param *File] {string range 0 end-4}]\]"
      Log::Print ERROR "DBBox : $err"
      return -code error $err
   } elseif { $Param(${DB}File)=="" || ![file exists $Param(${DB}File)] } {
      set err "Invalid DB : '$DB'. The DB file ($Param(${DB}File)) doesn't exist"
      Log::Print ERROR "DBBox : $err"
      return -code error $err
   }

   #----- Read the DB

   set idxs $Param(${DB}Idx)
   set nb   [llength $idxs]
   set data {}
   set view {}

   set fd   [open $Param(${DB}File) r]
   while { [gets $fd line]>=0 } {
      if { [string index $line 0]!="c" && [string index $line 0]!="#" && [llength $line]>=$nb } {
         set lst {}
         foreach idx $idxs {
            if { [string first @ $idx]==-1 } {
               lappend lst [lindex $line $idx]
            } else {
               lappend lst [join [lrange $line {*}[split $idx @]]]
            }
         }

         lappend view $lst
         lappend data $line
      }
   }
   close $fd

   set Data(DB$DB) $data
   set Data(View$DB) $view

   if { [llength [info procs Post$DB]] } {
       Post$DB
   }
}

#-------------------------------------------------------------------------------
# Nom      : <DBBox::Search>
# Creation : Avril 2016 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Parmet à un script la recherche de données dans une base de données
#
# Parametres :
#     <DB>     : La base de données à parcourir, préfixée de 'DB' pour la BD
#                complète ou 'View' pour la BD affichée. (Ex: ViewOil)
#     <Column> : La colonne (nom ou index) où appliquer le pattern
#     <Pattern>: Le pattern de recherche
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc DBBox::Search { DB Column Pattern } {
   variable Param
   variable Data

   set db [regsub {^(?:DB|View)} $DB ""]

   CheckDB $db

   #----- Find the column we are searching
   if { [string is integer $Column] } {
      set idx $Column
   } else {
      if { [set idx [lsearch -exact $Param(${db}Lst) $Column]] == -1 } {
         return {}
      } elseif { [string match DB* $DB] } {
          set idx [lindex $Param(${db}Idx) $idx]
      }
   }

   return [lsearch -glob -nocase -index $idx -inline -all $Data($DB) $Pattern]
}

#-------------------------------------------------------------------------------
# Nom      : <DBBox::SearchClear>
# Creation : Avril 2016 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Réinitialise les critères de recherche
#
# Parametres :
#     <DB>     : La base de données
#     <Columns>: Les colonnes (nom ou index) à réinitialisé.
#                Aucune colonnes = toutes les colonnes
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc DBBox::SearchClear { DB {Columns {}} } {
   variable Search
   variable Param
   variable Data

   if { [llength $Columns] } {
      foreach col $Columns {
         if { [string is integer $col] } {
            set Search($DB[lindex $Param(${DB}Lst) $col]) ""
         } else {
            set Search($DB$col) ""
         }
      }
   } else {
      foreach key $Param(${DB}Lst) {
         set Search($DB$key) ""
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <DBBox::Sort>
# Creation : Avril 2016 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Sort sur une colone
#
# Parametres :
#     <DB>     : La base de données
#     <W>   : Le widget (listbox) à updater
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc DBBox::Sort { DB W } {
   variable Param

   #----- Set the sorting order
   if { ![info exists Param(SortPrev)] || $Param(Sort)!=$Param(SortPrev) || $Param(SortOrder)=="decreasing" } {
      set Param(SortOrder) "increasing"
   } else {
      set Param(SortOrder) "decreasing"
   }

   set Param(SortPrev)  $Param(Sort)

   Update $DB $W
}

#-------------------------------------------------------------------------------
# Nom      : <DBBox::GetFmt>
# Creation : Avril 2016 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Retourne (en le construisant si nécessaire) le format (de style
#            printf) à utiliser pour formater les données.
#
# Parametres :
#     <DB>  : La base de données
#     <W>   : Le widget (listbox) où seront affichées les données
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc DBBox::GetFmt { DB W } {
   variable Param

   if { [info exists Param(${DB}Fmt)] } {
      return $Param(${DB}Fmt)
   }

   update

   set fmt  ""
   set nb   0
   set font [$W cget -font]
   set cw   [font measure $font -displayof $W " "] ;# The next lines assume that this is a fixed-width font

   foreach key $Param(${DB}Lst) just $Param(${DB}Just) {
      set ent  .dbbox.head.$key
      set gap  [lindex [$ent.search bbox 0] 0]
      set x    [winfo x $ent]
      set w    [winfo width $ent]
      set j    [string index $just 0]

      #----- Left pad with spaces if needed
      set pad [expr {double($x+$gap-$nb*$cw)/double($cw)}]
      set pad [expr {int($nb>0 ? ceil($pad) : $pad)}]

      #----- Calculate the number of characters we can put in this field
      set adj  [expr {($nb+$pad)*$cw-$x-$gap}]
      set nc   [expr {int(double($w-2*$gap-($adj>0?$adj:0))/double($cw))}]

      #----- Adjust the gap and the number of chars if we use the centered justification
      if { $j == "c" } {
         set j [string index $just 1]
         set c [string range $just 2 end]

         set adj [expr {double($nc-$c)*0.5}]
         if { $j == "r" } {
            set nc [expr {$nc-int($adj)}]
         } else {
            set nc   [expr {$nc-int(ceil($adj))}]
            set pad  [expr {$pad+int($adj)}]
         }
      }

      #----- Append to the format string
      append fmt [string repeat " " $pad] % [expr {$j=="l"?"-":""}] $nc.$nc s

      #----- Update our new position
      set nb [expr {$nb+$pad+$nc}]
   }

   #----- Save the format
   set Param(${DB}Fmt) $fmt

   return $fmt
}

#-------------------------------------------------------------------------------
# Nom      : <DBBox::Update>
# Creation : Avril 2016 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Update la liste des items affichés en tenant compte des critères de
#            rechercheé.
#
# Parametres :
#     <DB>  : La base de données à lire
#     <W>   : Le widget (listbox) à updater
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc DBBox::Update { DB W } {
   variable Search
   variable Param
   variable Data

   #----- Clear the listbox
   $W delete 0 end

   #----- Only keep items that match the criterias
   set lst $Data(View$DB)
   set idx 0
   foreach key $Param(${DB}Lst) {
      if { [info exists Search($DB$key)] && $Search($DB$key)!="" } {
         set lst [lsearch -glob -nocase -index $idx -inline -all $lst "*[join $Search($DB$key) *]*[set lst ""]"]
      }
      incr idx
   }

   #----- Sort based on the selected column (if necessary)
   if { [set idx [lsearch -exact $Param(${DB}Lst) $Param(Sort)]]!=-1 } {
      set lst [lsort -index $idx -nocase -$Param(SortOrder) $lst[set lst ""]]
   }

   #----- Add the items that match the search criterias
   set fmt [GetFmt $DB $W]
   foreach row $lst {
      $W insert end [format $fmt {*}$row]
   }

   #----- Keep a copy of what we've added (some DB have spaces in some of their fields which mean that we need to keep the list formatting)
   set Data(Shown) $lst

   update
}

