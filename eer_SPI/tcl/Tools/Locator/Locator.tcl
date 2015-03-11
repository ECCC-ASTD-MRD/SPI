#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Interface de selection de source.
# Fichier   : Locator.tcl
# Creation  : Juin 1997 - J.P. Gauthier - CMC/CMOE
#
# Description:
#
#       Cette interface permet de rechercher et de selectionne une source de type
#       Volcan, Nucleaire, CTBT, Station, ou villes grace a une interface.
#
# Remarques :
#      format des fichiers : [format "%-8s:%-29s:%-20s:%6.3f:%1s:%7.3f:%1s:%8s" ...]
#
#===============================================================================

#----- Lire les sources d'execution
source $GDefs(Dir)/tcl/Tools/Locator/Locator.ctes
source $GDefs(Dir)/tcl/Tools/Locator/Locator.txt
source $GDefs(Dir)/tcl/Tools/Locator/Locator.int

#-------------------------------------------------------------------------------
# Nom      : <Locator::RangeCheck>
# Creation : Avril 2002 - J.P. Gauthier - CMC/CMOE
#
# But     : Verifier l'inclusion dans un range.
#
# Parametres  :
#   <Info>    : Information sur la localisation
#
# Retour    :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Locator::RangeCheck { Info } {
   variable Data

   set lat [lindex $Info 3]
   if { [lindex $Info 4]=="S" } {
      set lat [expr -$lat]
   }

   set lon  [lindex $Info 5]
   if { [lindex $Info 6]=="W" } {
      set lon [expr -$lon]
   }

   if { [expr $Data(Lon0)*$Data(Lon1)]<0 } {
      set delta [expr $Data(Lon1)-$Data(Lon0)]
   } else {
      set delta 0
   }

   if { $lat>=$Data(Lat0) && $lat<=$Data(Lat1) } {
      if { $delta<=180 } {
         if { $lon>=$Data(Lon0) && $lon<=$Data(Lon1) } {
            .locator.list.box insert end [join $Info " "]
         }
      } else {
         if { ($lon<=$Data(Lon0) && $lon>-180) || ($lon>=$Data(Lon1) && $lon<180) } {
            .locator.list.box insert end [join $Info " "]
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Locator::Close>
# Creation : Fevrier 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Termine l'application et nettoie.
#
# Parametres :
#   Aucune.
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Locator::Close { } {
   variable Data

   if { $Page::Data(ToolMode)=="Locator" } {
      SPI::ToolMode SPI Zoom
   }
   set Data(Active) 0
   set Data(Inst)  1e32

   Locator::Clear

   . config -cursor left_ptr
   destroy .locator

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#----------------------------------------------------------------------------
# Nom      : <Locator::Clear>
# Creation : Juin 2013 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer toutes les primitives.
#
# Parametres :
#  <Frames>  : Identificateurs de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Locator::Clear { { Frames {} } } {
   variable Data

   set Data(Coo) ""

   if { [winfo exists $Data(Canvas)] } {
      $Data(Canvas) delete RANGELOCATOR
      SPI::IcoDel LOCATION
   }
}

#----------------------------------------------------------------------------
# Nom      : <Locator::Export>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Exporter la selection dans un fichier de description d'icones.
#
# Parametres :
#  <File>    : Nom du device pour imprimer .
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Locator::Export { File } {
   global GDefs
   variable Lbl
   variable Data

   #----- Verifier la validite du path

   if { [string length $File] == 0 } {
      return
   }

   set f [open $File w]

   #----- Enregistrer l'entete

   puts $f "IconList 3.0"
   puts $f "[lindex [lindex $Lbl(PrintType) $GDefs(Lang)] $Data(Current)]"
   puts $f ""

   set file [lindex [[lindex $Data(Icons) $Data(Current)] configure -file] end]
   puts $f "$file"

   #----- Imprime la liste de sources

   foreach line [.locator.list.box get 0 end] {
      set name [string trim [string range $line 9 37]]
      set lat  [string range $line 61 69]
      set latd [string range $line 71 71]
      set lon  [string range $line 73 82]
      set lond [string range $line 84 84]
      set elev [string range $line 86 end]

      if { $latd == "S" } {
         set lat [expr -$lat]
      }
      if { $lond == "W" } {
         set lon [expr -$lon]
      }
      puts $f "\"$name\" [format "%.6f" $lat] [format "%.6f" $lon] $elev"
   }
   close $f
}

#-------------------------------------------------------------------------------
# Nom      : <Locator::Format>
# Creation : Fevrier 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Formatage de sortie pour les donnees d'une source.
#
# Parametres :
#  <Line>    : Ligne d'information
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Locator::Format { Line } {
   variable Data

   set no     [string trim [string range $Line 0 7]]
   set name   [string trim [string range $Line 9 37]]
   set area   [string trim [string range $Line 39 59]]
   set lat    [string trim [string range $Line 61 69]]
   set latdir [string range $Line 71 71]
   set lon    [string trim [string range $Line 73 82]]
   set londir [string range $Line 84 84]
   set elev   [string trim [string range $Line 86 end]]

   # ----- Convertir les valeurs de la coordonnee en +/-

   if { $latdir == "S" } {
      set lat [expr -1 * $lat]
   }
   if { $londir == "W" } {
      set lon [expr -1 * $lon]
   }

   #----- Initialiser les variables de recuperation

   set SPI::Src(Type) $Data(Current)
   set SPI::Src(No)   $no
   set SPI::Src(Name) $name
   set SPI::Src(Area) $area
   set SPI::Src(Lat)  $lat
   set SPI::Src(Lon)  $lon
   set SPI::Src(Elev) $elev

   return [list $Data(Current) $no $name $area $lat $lon $elev]
}

#-------------------------------------------------------------------------------
# Nom      : <Locator::Get>
# Creation : Fevrier 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Selectionner le type de source affiche.
#
# Parametres :
#  <Type>    : Type de source
#  <Mode>    : Mode d'insertion
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Locator::Get { Type { Mode 0 } } {
   variable Data

   #----- Recuperer les donnees si necessaire

   if { [llength $Data($Type)] == 0 } {
      Locator::Load $Type
   }

   #----- Nettoyer le canvas

   set Data(Coo)    ""

   if { [winfo exists $Data(Canvas)] } {
      $Data(Canvas) delete RANGELOCATOR
      SPI::IcoDel LOCATION
   }

   Locator::Insert $Type $Mode 1
}

#-------------------------------------------------------------------------------
# Nom      : <Locator::Insert>
# Creation : Fevrier 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Insere les sources dans le listbox selon les criteres.
#
# Parametres :
#   <Type>   : Type de source.
#   <Mode>   : Mode d'insertion
#   <Update> : Update de l'usager
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Locator::Insert { Type Mode Update } {
   global   GDefs
   variable Lbl
   variable Msg
   variable Data

   #----- Garder l'instance courante

   incr Data(Inst)
   set inst $Data(Inst)

   . config -cursor watch
   .locator.list.box config -cursor watch
   .locator.list.box delete 0 end
   update idletasks

   #----- Effectuer les selections

   if { $Mode } {
      Locator::Sort $Type
   } else {
      set Data(No)    ""
      set Data(Name)  ""
      set Data(Area)  ""
      set Data(Sort) ""
      set Data(Lat0)  ""
      set Data(Lat1)  ""
      set Data(Lon0)  ""
      set Data(Lon1)  ""
   }

   set Data(Job) [lindex $Msg(Insert) $GDefs(Lang)]

   if { $Data(No)=="" && $Data(Name)=="" && $Data(Area)=="" } {
      set find 0
   } else {
      set find 1
      set Data(Job) [lindex $Msg(Search) $GDefs(Lang)]
   }

   if { $Data(Lat0)=="" || $Data(Lat1)=="" || $Data(Lon0)=="" || $Data(Lon1)=="" } {
      set rng 0
   } else {
      set rng 1
      set Data(Job) [lindex $Msg(Search) $GDefs(Lang)]
   }
   update idletasks

   foreach info $Data($Type) {

      if { $Update } {
         update
         if { $inst < $Data(Inst) } {
            return
         }
      }

      if { $find } {
         if { [string match -nocase *$Data(No)*   [lindex $info 0]] &&
              [string match -nocase *$Data(Name)* [lindex $info 1]] &&
              [string match -nocase *$Data(Area)* [lindex $info 2]] } {
            if { $rng } {
               Locator::RangeCheck $info
            } else {
              .locator.list.box insert end [join $info " "]
            }
         }
      } else {
         if { $rng } {
            Locator::RangeCheck $info
         } else {
            .locator.list.box insert end [join $info " "]
         }
      }
   }

   #----- Afficher les sources du range

   if { $rng } {
      set ico ""
      set lst ""
      set dat  [.locator.list.box get 0 end]
      set icon [lindex $Data(Icons) $Data(Current)]

      foreach d $dat {
         set sel [Locator::Format $d]
         lappend lst $sel
         lappend ico [list [lindex $sel 2] [lindex $sel 4] [lindex $sel 5] [lindex $sel 6] $icon]
      }
      set SPI::Src(Info) $lst
      SPI::IcoAdd $Data(Frame) LOCATION "" $ico
   }

   set Data(Nb)   [.locator.list.box index end]
   set Data(Job)  ""
   . config -cursor left_ptr
   .locator.list.box config -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <Locator::Load>
# Creation : Fevrier 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire un fichier de description de source.
#
# Parametres :
#   <Type>   : Type de source.
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Locator::Load { Type } {
   global   GDefs
   variable Data
   variable Msg

   set Data(Job) [lindex $Msg(Read) $GDefs(Lang)]
   update idletasks

   set f [open [lindex $Data(Files) $Type] r]

   while { [gets $f ligne] >= 0 } {
      lappend Data($Type) [split $ligne :]
   }
   close $f

   set Data(Job) ""
}

#----------------------------------------------------------------------------
# Nom      : <Locator::PrintCommand
# Creation : Decembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Impression de la liste de sources courante.
#
# Parametres :
#  <Widget>  : Nom du widget a imprimer .
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Locator::PrintCommand { Widget } {
   global   GDefs
   variable Lbl
   variable Data

   set f [open /tmp/[pid].txt w]

   #----- Imprime l'entete

   puts $f "[lindex $Lbl(PrintList) $GDefs(Lang)] ([lindex [lindex $Lbl(PrintType) $GDefs(Lang)] $Data(Current)])\n"
   puts $f "[lindex $Lbl(PrintCrit) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]=$Data(No)\t\t\
            [lindex $Lbl(Name) $GDefs(Lang)]=$Data(Name)\t\t [lindex $Lbl(Area) $GDefs(Lang)]=$Data(Area)"
   puts $f "[lindex $Lbl(PrintNb)   $GDefs(Lang)] $Data(Nb)\n"
   puts $f "[lindex $Lbl(PrintHead) $GDefs(Lang)]"
   puts $f "--------------------------------------------------------------------------------------------\n"

   #----- Imprime la liste de sources

   foreach ligne [$Widget get 0 end] {
      puts $f "$ligne"
   }
   close $f

   PrintBox::PrintTXT /tmp/[pid].txt
   PrintBox::Destroy
}

#-------------------------------------------------------------------------------
# Nom      : <Locator::Select>
# Creation : Aout 2002 J.P. Gauthier - CMC/CMOE
#
# But     : Activer la selection de l'usager.
#
# Parametres :
#   <List>   : Liste
#   <Y>      : Position dans la liste
#   <Locate> : Ratation sur la localisation
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Locator::Select { List Y { Locate False } } {
   variable Data

   if { [winfo exists $Data(Canvas)] } {
      $Data(Canvas) delete RANGELOCATOR
   }

   #----- Nettoyer le canvas

   set Data(Coo)    ""

   #----- Recuperer l'information

   set info [Locator::Format [$List get [$List nearest $Y]]]
   SPI::IcoAdd $Data(Frame) LOCATION "" [list [list [lindex $info 2] [lindex $info 4] [lindex $info 5] [lindex $info 6] [lindex $Data(Icons) $Data(Current)]]]

   set SPI::Src(Info) $info
   if { $Locate } {
      SPI::Locate $SPI::Src(Lat) $SPI::Src(Lon)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Locator::SelectSource>
# Creation : Aout 2014 J.P. Gauthier - CMC/CMOE
#
# But     : Selectionner une source directement par son nom, sans interface.
#
# Parametres :
#   <Type>   : Type de source
#   <Name>   : Nom de la source
#
# Retour:
#   <Info>  :  { No Name Area Lat Lon Elev }
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Locator::SelectSource { Type Name } {
   variable Data
   
   set Data(Current) $Type
   
   set name [string map { _ " " } $Name]
   if { [catch { set line [split [exec egrep -i ".+: ${name}\\s*:.+:.+:.+:.+:.+:.+" [lindex $Data(Files) $Type]] :] }] } {
      return ""
   } else {
      return [set SPI::Src(Info) [Locator::Format [join $line " "]]]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Locator::Sort>
# Creation : Fevrier 2000 - J.P. Gauthier - CMC/CMOE
#
# But     : Tri la liste de source.
#
# Parametres :
#   <Type>   : Type de source
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Locator::Sort { Type } {
   global   GDefs
   variable Data
   variable Msg

   set Data(Job) [lindex $Msg(Sort) $GDefs(Lang)]
   update idletasks

   switch $Data(Sort) {
      "No"   { set Data($Type) [lsort -dictionary -index 0 $Data($Type)] }
      "Name" { set Data($Type) [lsort -dictionary -index 1 $Data($Type)] }
      "Area" { set Data($Type) [lsort -dictionary -index 2 $Data($Type)] }
      "Elev" { set Data($Type) [lsort -integer    -index 7 $Data($Type)] }
      "Lat"  { set Data($Type) [lsort -real       -index 3 $Data($Type)]
               set Data($Type) [lsort -dictionary -index 4 $Data($Type)] }
      "Lon"  { set Data($Type) [lsort -real       -index 5 $Data($Type)]
               set Data($Type) [lsort -dictionary -index 6 $Data($Type)] }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Locator::Draw...>
# Creation : Juin 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de manipulations de la selection sur la projection.
#
# Parametres :
#   <Frame   : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Ces fonctions sont appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc Locator::DrawInit  { Frame VP } {
   variable Data

   set Data(Lat0)   $Viewport::Map(LatCursor)
   set Data(Lon0)   $Viewport::Map(LonCursor)
}

proc Locator::Draw      { Frame VP } {
   variable Data

   if { [winfo exists $Data(Canvas)] } {
      $Data(Canvas) delete RANGELOCATOR
   }

   set Data(Lat1)   $Viewport::Map(LatCursor)
   set Data(Lon1)   $Viewport::Map(LonCursor)
   set Data(Canvas) $Page::Data(Canvas)
   set Data(Frame)  $Frame
   set Data(VP)     $VP

   Viewport::DrawRange $Frame $VP $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) RANGELOCATOR red
}

proc Locator::DrawDone { Frame VP } {
   variable Data

   if { $Data(Lat0)>$Data(Lat1) } {
      set tmp $Data(Lat1)
      set Data(Lat1) $Data(Lat0)
      set Data(Lat0) $tmp
   }

   if { $Data(Lon0)>$Data(Lon1) } {
      set tmp $Data(Lon1)
      set Data(Lon1) $Data(Lon0)
      set Data(Lon0) $tmp
   }

   if { $Data(Lat0)==$Data(Lat1) || $Data(Lon0)==$Data(Lon1) } {
      set Data(Coo) ""
   } else {
      set Data(Coo) "$Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1)"
   }

   Locator::Insert $Data(Current) 1 0
}

proc Locator::MoveInit { Frame VP } {
   variable Data

   set Data(LonD) $Viewport::Map(LonCursor)
   set Data(LatD) $Viewport::Map(LatCursor)
}

proc Locator::Move { Frame VP } {
   variable Data

   #----- Effectuer la translation

   set lat0 [expr $Data(Lat0) + $Viewport::Map(LatCursor) - $Data(LatD)]
   set lat1 [expr $Data(Lat1) + $Viewport::Map(LatCursor) - $Data(LatD)]

   if { $lat0 > -90.0 && $lat0 < 90.0 && $lat1 > -90.0 && $lat1 < 90.0 } {

      set Data(Lat0) $lat0
      set Data(Lat1) $lat1
      eval set Data(Lon0) [Viewport::CheckCoord [expr $Data(Lon0) + $Viewport::Map(LonCursor) - $Data(LonD)]]
      eval set Data(Lon1) [Viewport::CheckCoord [expr $Data(Lon1) + $Viewport::Map(LonCursor) - $Data(LonD)]]
   }

   #----- Reaffecter le point de reference de translation

   if { [winfo exists $Data(Canvas)] } {
      $Data(Canvas) delete RANGELOCATOR
   }

   set Data(LonD)   $Viewport::Map(LonCursor)
   set Data(LatD)   $Viewport::Map(LatCursor)
   set Data(Canvas) $Page::Data(Canvas)
   set Data(Frame)  $Frame
   set Data(VP)     $VP

   Viewport::DrawRange $Frame $VP $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) RANGELOCATOR red
}

proc Locator::MoveDone { Frame VP } {
   variable Data

   Locator::DrawDone $Frame $VP
}

#----------------------------------------------------------------------------
# Nom      : <Locator::UpdateItems>
# Creation : Juillet 2002 - J.P. Gauthier - CMC/CMOE
#
# But      :
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Locator::UpdateItems { Frame } {
   variable Data

   if { ![llength [info commands $Data(VP)]] } {
      set Data(VP) ""
   }

   if { $Data(Coo)!="" && $Frame==$Data(Frame) && $Data(VP)!="" } {
      $Data(Canvas) delete RANGELOCATOR
      Viewport::DrawRange $Data(Frame) $Data(VP) $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) RANGELOCATOR red
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Locator::AsProject>
# Creation : Aout 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder l'etat de l'outils dans un projet SPI.
#
# Parametres :
#   <File>   : Descripteur de fichier ou ecrire les commandes
#
# Remarques :
#    - Le fichier est deja ouvert, il suffit d'y ecrire les commandes a executer
#      afin de re-instaurer l'outils dans son etat actuel.
#
#-------------------------------------------------------------------------------

proc Locator::AsProject { File } {
   variable Data
   variable Param

   if { [winfo exists .locator] } {
      puts $File "#----- Tool: Locator\n"
      puts $File "set Locator::Param(Dock)   $Param(Dock)"
      puts $File "set Locator::Param(Geom)   [winfo geometry .locator]"
      puts $File "Locator::Window"
      puts $File "set Locator::Data(Lat0)    \"$Data(Lat0)\""
      puts $File "set Locator::Data(Lat1)    \"$Data(Lat1)\""
      puts $File "set Locator::Data(Lon0)    \"$Data(Lon0)\""
      puts $File "set Locator::Data(Lon1)    \"$Data(Lon1)\""
      puts $File "set Locator::Data(No)      \"$Data(No)\""
      puts $File "set Locator::Data(Name)    \"$Data(Name)\""
      puts $File "set Locator::Data(Area)    \"$Data(Area)\""
      puts $File "set Locator::Data(Sort)    \"$Data(Sort)\""
      puts $File "set Locator::Data(Current) $Data(Current)"
      puts $File "Locator::Get $Data(Current) 1"
      puts $File "\n"
   }
}
