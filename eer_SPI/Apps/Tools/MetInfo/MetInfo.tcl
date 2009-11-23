#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Package d'interface pour SPI
# Fichier  : MetInfo.tcl
# Creation : Octobre 2000
#
# Description:
#    Permet l'obtention de messages meteorologiques
#
#===============================================================================

#----- Lire les sources d'execution

source $GDefs(Dir)/Apps/Tools/MetInfo/MetInfo.ctes
source $GDefs(Dir)/Apps/Tools/MetInfo/MetInfo.txt
source $GDefs(Dir)/Apps/Tools/MetInfo/MetInfo.int

#-------------------------------------------------------------------------------
# Nom      : <MetInfo::Close>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ferme l'interface.
#
# Parametres :
#
# Remarques :
#    -On doit redefinir les fonctions de range des projections
#
#-------------------------------------------------------------------------------

proc MetInfo::Close { } {
   global Range
   variable Data

   Locator::Close

   set Data(Active)  0
   trace vdelete SPI::Src(Info) w { MetInfo::Source  }

   destroy .metinfo

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#-------------------------------------------------------------------------------
# Nom      : <MetInfo::FileGet>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recuperer les dates des fichiers disponibles.
#
# Parametres :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc MetInfo::FileGet { } {
   variable Data

   set prev $Data(File)

   switch $Data(Type) {
      metar
         -
      sa {
         set Data(Path) [lindex $Data(RawPath) [lsearch -exact $Data(LType) $Data(Type)]]
      }
      radiosonde
         -
      synop {
         set Data(Path) [lindex $Data(BurpPath) [lsearch -exact $Data(LType) $Data(Type)]]
      }
   }

   set files [lsort -dictionary [glob $Data(Path)/*_]]
   set list ""

   #----- Recuperer seulement les noms

   foreach file $files {
      lappend list [file tail $file]
   }

   ComboBox::DelAll .metinfo.sel.file
   ComboBox::AddList .metinfo.sel.file $list

   #----- Selectionner une date
   if { [lsearch -exact $list $prev] != -1 } {
      set Data(File) $prev
   } else {
      set Data(File) [lindex $list end]
   }
}

#----------------------------------------------------------------------------
# Nom      : <MetInfo::ExportFileGet>
# Creation : Mars 2003 - S. Gaudreault - CMC/CMOE
#
# But      : Recuperer les dates des fichiers disponibles dans la fenetre
#            d'exportation en scipuff
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetInfo::ExportFileGet { } {
   variable Data

   set Data(Path) [lindex $Data(BurpPath) [lsearch -exact $Data(LType) $Data(Type)]]
   set files [lsort -dictionary -decreasing [glob $Data(Path)/*_]]

   .export.mid.filesfrm.files delete 0 end

   #----- Recuperer seulement les noms

   foreach file $files {
      .export.mid.filesfrm.files insert end [file tail $file]
   }
}

#----------------------------------------------------------------------------
# Nom      : <MetInfo::ExportFile>
# Creation : Mars 2003 - S. Gaudreault - CMC/CMOE
#
# But      : Convertion des fichier burb sélectionné en format scipuff.
#
#
# Parametres :
#    <outFile> : prefixe pour le nom du fichier de sauvegarde.
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc MetInfo::ExportFile { outFile } {
   variable Data
   variable Param
   variable Msg

   set typeList    ""
   set stationList ""
   set fileList    ""
   set generatedFiles ""
   set errFiles       ""

   set result ""

   foreach index [.export.mid.typefrm.type curselection] {
      lappend typeList [.export.mid.typefrm.type get $index]
   }

   foreach index [.export.mid.stationfrm.station curselection] {
      lappend stationList [.export.mid.stationfrm.station get $index]
   }

   foreach index [.export.mid.filesfrm.files  curselection] {
      lappend fileList [.export.mid.filesfrm.files get $index]
   }

   foreach type $typeList {

      Log::Print INFO "Extraction of $type observations"

      foreach station $stationList {
         foreach file $fileList {
            set path [lindex $Data(BurpPath) [lsearch -exact $Data(LType) $type]]

            set stidNum    [lindex $station end]
            set stidLet    [lindex $station 0]

            switch $type {
               radiosonde  {
                  Log::Print INFO "Extract radiosonde info from $path/$file"
                  set output [split [exec $Param(Bin)/scipuff -stn $stidNum -prf $path/$file] "\n"]

                  if { [llength $output] == 6 } {
                     set output [split [exec $Param(Bin)/scipuff -stn $stidLet -prf $path/$file] "\n"]
                  }
               }
               metar {
                  set stid [lindex $station 0]
                  Log::Print INFO "Extract metar info from $path/$file"
                  set output [split [exec $Param(Bin)/scipuff -stn $stidLet -sfc $path/$file] "\n"]

                  if { [llength $output] == 6 } {
                     set output [split [exec $Param(Bin)/scipuff -stn $stidNum -sfc $path/$file] "\n"]
                  }
               }
               synop {
                  Log::Print INFO "Extract synop info from $path/$file"
                  set output [split [exec $Param(Bin)/scipuff -stn $stidNum -sfc $path/$file] "\n"]

                  if { [llength $output] == 6 } {
                     set output [split [exec $Param(Bin)/scipuff -stn $stidLet -sfc $path/$file] "\n"]
                  }
               }
            }

            update

            set j 0
            foreach line $output {
               if { $j < 7 } {
                  incr j
                  continue
               }
               lappend result $line
               incr j
            }
         }
      }

      if { [llength $result] == 0 } {
         lappend errFiles $type
      }

      Log::Print INFO "Sorting data ..."
      set tmpFile /tmp/$type[pid]
      set sortedTmpFile /tmp/sorted$type[pid]

      foreach line $result {
         exec echo $line >> $tmpFile
      }

      if { [file exists $tmpFile]  == 1 } {

         if { $type == "radiosonde" } {
            exec echo "PROFILE" >> $sortedTmpFile
            exec echo "13"  >> $sortedTmpFile
            exec echo "ID      LAT     LON     YEAR    MONTH   DAY     HOUR    Z       DIR     WSPD" >> $sortedTmpFile
            exec echo "T       Q       P" >> $sortedTmpFile
            exec echo "NONE    N       W                               HRS     M               M/S" >> $sortedTmpFile
            exec echo "C       %       MBAR" >> $sortedTmpFile
            exec echo "-999.00"  >> $sortedTmpFile
         } else {
            exec echo "SURFACE" >> $sortedTmpFile
            exec echo "12" >> $sortedTmpFile
            exec echo "ID      LAT     LON     YEAR    MONTH   DAY     HOUR    DIR     WSPD    T" >> $sortedTmpFile
            exec echo "Q       P" >> $sortedTmpFile
            exec echo "NONE    N       W                               HRS             M/S     C" >> $sortedTmpFile
            exec echo "%       MBAR" >> $sortedTmpFile
            exec echo "-99.00" >> $sortedTmpFile
         }

         Log::Print INFO "generate file(s) ..."

         if { $type == "radiosonde" } {
            exec sort $tmpFile | uniq | sort -k8,8 -g -s | sort -k1,1 -s -g | sort -k7,7 -s -g | sort -k6,6 -s -g | sort -k5,5 -s -g | sort -k4,4 -s -g >> $sortedTmpFile
         } else {
            exec sort $tmpFile | uniq |  sort -k7,7 -s -g | sort -k6,6 -s -g | sort -k5,5 -s -g | sort -k4,4 -s -g >> $sortedTmpFile
         }

         set extension [lindex $Data(Extension) [lsearch -exact $Data(LType) $type]]

         file rename -force $sortedTmpFile  $outFile-$type.$extension
         file delete $tmpFile
         file delete $sortedTmpFile ;#ne devrait plus exister, mais mieux vaut ne pas prendre de chance ...

         lappend generatedFiles $outFile-$type.$extension
         Log::Print INFO "done."
      }
      set result ""
   }

   if { [llength $errFiles] != 0 } {
      Dialog::Error .export $Msg(Empty) "\n\n$errFiles"
   }

   if { [llength $generatedFiles] != 0 } {
      Dialog::Info .export $Msg(Complete) "\n\n$generatedFiles"
   }

   Log::Print INFO "Extraction of observations completed"
}

#-------------------------------------------------------------------------------
# Nom      : <MetInfo::MsgGet>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recuperer les messages disponibles.
#
# Parametres :
#
# Remarques :
#    -METAR: Station avec id de quatre lettre seulement
#    -SA   : Statio navec id de trois lettre ou trois derniere des quatres lettres
#
#-------------------------------------------------------------------------------

proc MetInfo::MsgGet { } {
   global GDefs
   variable Data
   variable Param
   variable Msg

   .metinfo.info.list delete 0 end

   if { $Data(Path) != "" && $Data(Station) != "" && $Data(File) != "" } {

      .metinfo.info.list config -cursor watch
      set Data(Msg) [lindex $Msg(Msg) $GDefs(Lang)]
      update idletasks

      set lines ""

      set stidNum [lindex $Data(Station) end]
      set stidLet [lindex $Data(Station) 0]

      switch $Data(Type) {
         radiosonde  {
            set lines [split [exec $Param(Bin)/scipuff -stn $stidNum -prf $Data(Path)/$Data(File) ] "\n"]
            if { [llength $lines] == 6 } {
               set lines [split [exec $Param(Bin)/scipuff -stn $stidLet -prf $Data(Path)/$Data(File) ] "\n"]
            }
         }

        metar {
            set lines [split [exec $Param(Bin)/search -me $Data(Path)/$Data(File) $stidLet] "\n"]

            foreach line $lines {
               .metinfo.info.list insert end "$line"
            }

            .metinfo.info.list config -cursor left_ptr
            set Data(Msg) "[llength $lines] observations"
            update idletasks

            return;
         }
         sa {
            if { [string length $stidLet] > 3 } {
               set stidLet [string range $stidLet 1 4]
            }
            set lines [split [exec $Param(Bin)/search -sa $Data(Path)/$Data(File) $stidLet] "\n"]

            foreach line $lines {
               .metinfo.info.list insert end "$line"
            }

            .metinfo.info.list config -cursor left_ptr
            set Data(Msg) "[llength $lines] observations"
            update idletasks

            return;
         }

         synop {
            set lines [split [exec $Param(Bin)/scipuff -stn $stidNum -sfc $Data(Path)/$Data(File) ] "\n"]
            if { [llength $lines] == 6 } {
               set lines [split [exec $Param(Bin)/scipuff -stn $stidLet -sfc $Data(Path)/$Data(File) ] "\n"]
            }
         }
      }

      # on saute l'entete
      set lines [lrange $lines 7 end]

      #On affiche l'entete approprié
      if { [llength $lines] } {
         .metinfo.info.list insert end "YEAR MONTH DAY HOUR Z DIR WSPD T Q P"
         foreach line $lines {
            .metinfo.info.list insert end "[lrange $line 3 end]"
         }
      }


      .metinfo.info.list config -cursor left_ptr
      set Data(Msg) "[llength $lines] observations"
      update idletasks
   }
}

#-------------------------------------------------------------------------------
# Nom      : <MetInfo::Source>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recupere la sortie du dictionnaire des stations.
#
# Parametres :
#  <Array>   : Variable array
#  <Index>   : Index dans la variable Array
#  <Op>      : Operation effectuer sur la variable
#
# Remarques :
#   -Cette procedure repond a un "trace" sur une variable a l'interieur de SPI
#    afin de recuperer l'information necessaire automatiquement (SPI::Src(Info))
#
#-------------------------------------------------------------------------------

proc MetInfo::Source { Array Index Op } {
   variable Data

   .metinfo.info.list config -cursor watch
   $Page::Data(Canvas) config -cursor watch
   update idletasks

   if { $SPI::Src(Info) != "" && $SPI::Src(Type)==3 } {
      ComboBox::DelAll .metinfo.sel.stat
      foreach station $SPI::Src(Info) {
         ComboBox::Add .metinfo.sel.stat "[lindex $station 1] [lindex $station 2] [lindex $station 3]"
      }
      .metinfo.info.list delete 0 end
      .metinfo.info.list config -cursor left_ptr
      set Data(Stat) "[llength $SPI::Src(Info)] stations"
   }
   $Page::Data(Canvas) config -cursor left_ptr
   .metinfo.info.list config -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <MetInfo::UpdateItems>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items relatifs station sur
#            la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Remarques :
#    - Cette fonctions est appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc MetInfo::UpdateItems { Frame } {

   if { $SPI::Src(Type)==3 } {

      set i 0

      foreach station $SPI::Src(Info) {
         $Locator::Data(Canvas) bind LOCATION$i <ButtonPress-1> "set MetInfo::Data(Station) \"[lindex $station 1] [lindex $station 2] [lindex $station 3]\" ; MetInfo::MsgGet"
         incr i
      }
  }
}

#-------------------------------------------------------------------------------
# Nom      : <MetInfo::AsProject>
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

proc MetInfo::AsProject { File } {
   variable Data
   variable Param

   if { [winfo exists .metinfo] } {
      puts $File "#----- Tool: MetInfo\n"
      puts $File "set MetInfo::Param(Dock)   $Param(Dock)"
      puts $File "set MetInfo::Param(Geom)   [winfo geometry .metinfo]"
      puts $File "MetInfo::Window"
      puts $File "\n"
   }
}
