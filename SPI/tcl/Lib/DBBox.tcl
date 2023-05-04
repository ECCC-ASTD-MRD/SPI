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
#     DBBox::Sort        { DB W }
#     DBBox::Update      { DB W }
#
#
#===============================================================================

package provide DBBox 1.0

catch { SPI::Splash "Loading Widget Package DBBox 1.0" }

namespace eval DBBox {
   variable Lbl
   variable Param

   # To avoid tables larger than the screen, use the following parameter to allocate width to table columns
   # In other words, no table will be larger than the following parameter's value (chracters wide).
   set Param(MaxWidthInCharacters) 250
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
   variable Param

   CheckDB $DB

   $Parent config -cursor X_cursor
   update idletasks

   toplevel .dbbox -bg $GDefs(ColorLight)
   wm transient .dbbox $Parent
   wm title .dbbox [lindex $Lbl(${DB}Title) $GDefs(Lang)]
   wm resizable .dbbox 1 1
   wm protocol .dbbox WM_DELETE_WINDOW {set DBBox::Data(Result) ""}

   #----- Main frame
   frame [set f .dbbox.main]

   #----- Table
   scrollbar $f.sy -relief sunken -bd 1 -width 10 -command "$f.tbl yview"
   table [set tbl $f.tbl] -relief flat -bd 1 -bg $GDefs(ColorLight) -anchor w -yscrollcommand "$f.sy set" \
      -selectmode browse -selecttype row -sparsearray 1 -drawmode fast -state normal -cursor left_ptr \
      -bd 0 -bordercursor sb_h_double_arrow -resizeborders col -colstretchmode unset -colwidth 0 \
      -width [llength $Param(${DB}Lst)] -height 32 -maxwidth 1800 -maxheight 1000 \
      -multiline 0 -rows 2 -cols [llength $Param(${DB}Lst)] -titlecols 0 -titlerows 2 \
      -highlightbackground $GDefs(ColorHighLight) -invertselected 0 \
      -insertbackground $GDefs(ColorLight) -variable ::DBBox::Tbl -autoclear 1 -padx 2 -ellipsis "…"
   pack $f.sy -side right -fill y
   pack $f.tbl -side left -fill both -expand 1

   pack $f -side top -expand 1 -fill both

   #----- Table header
   $tbl set row 0,0 [lindex $Lbl(${DB}Lst) $GDefs(Lang)]
   $tbl height 0 [::tcl::mathfunc::max {*}[lmap l [lindex $Lbl(${DB}Lst) $GDefs(Lang)] {llength [split $l \n]}]]

   #----- Table column widths adapted to their content
   #  The minimal column width is controlled by tktable's -colwidth above
   #  There is no maximum column width, rather we allocate width from Param(MaxWidthInCharacters)'s value
   #  To avoid users having to resize an excessive amount of columns, columns needing less width are prioritized
   #
   #  Ex:
   #  We have 200 characters of width to allocate to the table and
   #  the following 8 columns which need these respective widths :
   #   75  50  50  40  25  15  10  10  =  275 characters large
   #  We have to cut down 75 characters of width from our columns
   #
   #  Since we prioritize the smaller columns, we will cut down width from the larger ones. Result :
   #   35  35  35  35  25  15  10  10  =  200 characters large
   #  -40 -15 -15  -5                  =   75 characters large removed
   #
   #  If we instead had 230 characters of width to allocate to the table.
   #  We have to cut down 45 characters of width from our columns. Result :
   #   44  44  44  40  25  15  10  10  =  232 characters large
   #  -31  -6  -6                      =   43 characters large removed
   #   -1  -1                          =    2 characters large removed (remaining)
   #  As you can see from the line above, we still had 2 characters of width to remove,
   #  but we couldn't divide it evenly amoungst the 3 largest columns.
   #  We instead removed 1 character width per column, starting from the largest initial one until we were done
   set tableContentByColumn [lmap l [lindex $Lbl(${DB}Lst) $GDefs(Lang)] {split $l \n}]
   foreach row $Data(View$DB) {
      for {set i 0} {$i < [llength $row]} {incr i} {
         lset tableContentByColumn $i [linsert [lindex $tableContentByColumn $i] end [lindex $row $i]]
      }
   }

   set widthInCharacters 0
   set columnWidths {}
   for {set i 0} {$i < [llength $tableContentByColumn]} {incr i} {
      set minColWidth [expr [::tcl::mathfunc::max {*}[lmap x [lindex $tableContentByColumn $i] {string length $x}]]+1]
      if { $minColWidth < [$tbl width $i]} {
         set minColWidth [$tbl width $i]
      }
      lappend columnWidths [list $i $minColWidth]
      incr widthInCharacters $minColWidth
   }

   #-----   Adjust column widths if too wide
   set columnWidths [lsort -index 1 -decreasing -integer $columnWidths]
   while { $widthInCharacters > $Param(MaxWidthInCharacters) } {
      set maxWidth [lindex [lindex $columnWidths 0] 1]
      set diffMaxToRunnerUpWidth 0
      set maxWidthCols {}
      foreach columnWidth $columnWidths {
         set currentWidth [lindex $columnWidth 1]
         if { $currentWidth != $maxWidth } {
            set diffMaxToRunnerUpWidth [expr $maxWidth-$currentWidth]
            break
         }
         lappend maxWidthCols $columnWidth
      }

      set nbMaxWidthCols [llength $maxWidthCols]
      set removableWidth [expr $nbMaxWidthCols*$diffMaxToRunnerUpWidth]
      set widthToRemove [expr $widthInCharacters-$Param(MaxWidthInCharacters)]
      if { $removableWidth >= $widthToRemove } {
         set widthToRemovePerCol [expr $widthToRemove/$nbMaxWidthCols]
         set remainingWidthToRemove [expr $widthToRemove%$nbMaxWidthCols]
         set newColWidth [expr $maxWidth-$widthToRemovePerCol]
         for {set i 0} {$i < $nbMaxWidthCols} {incr i} {
            set maxWidthCol [lindex $maxWidthCols $i]
            lset maxWidthCol 1 [expr $i < $remainingWidthToRemove ? [expr $newColWidth-1] : $newColWidth]
            lset columnWidths $i $maxWidthCol
         }

         #----- Update widthInCharacters to exit while loop
         set widthInCharacters [expr $widthInCharacters-$widthToRemove]
      } else {
         set newColWidth [expr $maxWidth-$diffMaxToRunnerUpWidth]
         for {set i 0} {$i < $nbMaxWidthCols} {incr i} {
            set maxWidthCol [lindex $maxWidthCols $i]
            lset maxWidthCol 1 $newColWidth
            lset columnWidths $i $maxWidthCol
         }

         #----- Update widthInCharacters to reenter while loop
         set widthInCharacters [expr $widthInCharacters-$removableWidth]
      }
   }

   #-----   Apply column widths to table
   foreach colWidth $columnWidths {
      $tbl width [lindex $colWidth 0] [lindex $colWidth 1]
   }

   #----- Tagging command
   $tbl configure -rowtagcommand [list apply [list {Row} {
      if { $Row >= 2 } {
         return data
      } elseif { $Row == 1 } {
         return search
      }
   }]]

   #----- Table formatting
   $tbl tag configure title -anchor center -relief raised -multiline 1 -justify center -bg $GDefs(ColorFrame) -fg black -bd 1
   $tbl tag configure sel -relief flat -bg $GDefs(ColorFrame)
   $tbl tag configure search -state normal -relief sunken -multiline 0
   $tbl tag configure data -state disabled -relief flat
   $tbl tag configure sort -fg blue
   $tbl tag configure click -relief sunken
   $tbl tag configure int -anchor e
   $tbl tag configure fp -anchor e
   $tbl tag configure exp -anchor e
   $tbl tag configure str -anchor w

   $tbl tag raise search
   $tbl tag raise click
   $tbl tag raise sort

   #----- Bindings for scrolling
   bind $tbl <ButtonPress-4> [list $tbl yview scroll -1 units]
   bind $tbl <ButtonPress-5> [list $tbl yview scroll 1 units]

   #----- Bindings for sorting
   bind $tbl <ButtonPress-1> [list apply [list {W X Y} {
      lassign [split [set idx [$W index @$X,$Y]] ,] row col
      #----- If on the first row and not on a column border
      if { $row==0
           && ($col==0 || [$W index @[expr $X-2],$Y col]==$col)
           && ($col-1==[$W cget -cols] || [$W index @[expr $X+2],$Y col]==$col) } {
         $W tag cell click $idx
      }
   } [namespace current]] %W %x %y]
   bind $tbl <ButtonRelease-1> [list apply [list {DB W X Y} {
      variable Param

      lassign [split [set idx [$W index @$X,$Y]] ,] row col
      #----- Make sure we were clicking on this button
      if { [$W tag includes click $idx] } {
         Sort $DB $W $col
      }

      $W clear tags 0,0 0,[$W cget -cols]
      if { $Param(Sort) >= 0 } {
         $W tag cell sort 0,$Param(Sort)
      }
   } [namespace current]] $DB %W %x %y]

   #----- Bindings for the search boxes
   bind $tbl <KeyRelease>     [list after idle [list DBBox::Update $DB $tbl]]
   bind $tbl <Key-KP_Enter>   {%W selection clear all; %W activate ""}
   bind $tbl <Key-Return>     {%W selection clear all; %W activate ""}
   bind $tbl <Motion>         [list apply [list {W X Y} {
      if { [$W tag includes search [$W index @$X,$Y]] } {
         $W configure -cursor "xterm"
         update idletasks
      } elseif { [$W cget -cursor] == "xterm" } {
         $W configure -cursor "left_ptr"
         update idletasks
      }
   } [namespace current]] %W %x %y]

   #----- Binding for the selection
   bind $tbl <Double-1> [list apply [list {W X Y DB} {
      lassign [split [set idx [$W index @$X,$Y]] ,] row col
      if { $row >= 2 } {
         #set DBBox::Data(Result) [$W get $row,0 $row,[$W cget -cols]]
         set DBBox::Data(Result) [lindex $DBBox::Data(View$DB) $row-2]
      }
   }] %W %x %y $DB]
   if { $Cmd != "" } {
      bind $tbl <ButtonRelease-1> +[list apply [list {W X Y Cmd} {
         lassign [split [set idx [$W index @$X,$Y]] ,] row col
         if { $row >= 2 } {
            {*}$Cmd [$W get $row,0 $row,[$W cget -cols]]
         }
      }] %W %x %y $Cmd]
   }

   update

   #----- Default vars + fill the table

   set Param(Sort) -1
   set Param(SortOrder) "increasing"
   Update $DB $tbl

   #----- Wait for selection

   set prevgrab [grab current]
   grab .dbbox
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
      set err "Invalid DB : '$DB'. No definition loaded for that DB. Available DB are : \[[lmap db [array names Param *File] {string range $db 0 end-4}]\]"
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
# Nom      : <DBBox::Sort>
# Creation : Avril 2016 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Sort sur une colone
#
# Parametres :
#  <DB>     : La base de données
#  <W>      : Le widget (listbox) à updater
#  <Col>    : La colone sur laquelle faire le trie
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc DBBox::Sort { DB W Col } {
   variable Param

   if { ![string is integer -strict $Col] } {
      if { [set idx [lsearch -exact $Param(${DB}Lst) $Col]]!=-1 } {
         set Col $idx
      } else {
         return -code error "Invalid column: $Col"
      }
   }

   if { $Param(Sort)!=$Col || $Param(Sort)==$Col && $Param(SortOrder)=="decreasing" } {
      set Param(SortOrder) "increasing"
   } else {
      set Param(SortOrder) "decreasing"
   }

   set Param(Sort) $Col

   Update $DB $W
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
   variable Tbl
   variable Param
   variable Data

   set ntr [$W cget -titlerows]
   set ntc [$W cget -titlecols]

   #----- Clear the listbox
   $W delete rows $ntr

   #----- Only keep items that match the criterias
   set lst $Data(View$DB)
   set a ""
   catch {
      lassign [split [set a [$W index active]] ,] r c
      if { $r == 1 } {
         set lst [lsearch -glob -nocase -index $c -inline -all $lst "*[join $Tbl(active) *]*[set lst ""]"]
      }
   }
   foreach {idx val} [array get Tbl 1,*] {
      if { $idx != $a } {
         set lst [lsearch -glob -nocase -index [lindex [split $idx ,] 1] -inline -all $lst "*[join $val *]*[set lst ""]"]
      }
   }

   #----- Sort based on the selected column (if necessary)
   if { $Param(Sort) >= 0 } {
      #----- Seperate the list in two, depending on the sorted column's type (string or number)
      set elementsWhereColumnValueIsString {}
      set elementsWhereColumnValueIsNumber {}
      foreach ele $lst {
         set columnValue [lindex $ele $Param(Sort)]
         if { [string is double -strict $columnValue] } {
            lappend elementsWhereColumnValueIsNumber $ele
         } else {
            lappend elementsWhereColumnValueIsString $ele
         }
      }

      #----- Sort each list according to the sorted column's type
      set elementsWhereColumnValueIsNumber [lsort -index $Param(Sort) -real -$Param(SortOrder) $elementsWhereColumnValueIsNumber[set elementsWhereColumnValueIsNumber ""]]
      set elementsWhereColumnValueIsString [lsort -index $Param(Sort) -nocase -$Param(SortOrder) $elementsWhereColumnValueIsString[set elementsWhereColumnValueIsString ""]]

      #----- Resulting list
      #      Append elements where column value is number first, then those where column value is string.
      set lst [list {*}$elementsWhereColumnValueIsNumber {*}$elementsWhereColumnValueIsString]
   }

   #----- Add the items that match the search criterias
   $W configure -rows [expr $ntr+[llength $lst]]
   set r $ntr
   foreach row $lst {
      $W set row $r,$ntc $row
      incr r
   }
}

