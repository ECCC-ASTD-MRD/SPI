#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Boite a Outils.
# Fichier  : HFManager.tcl
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
#
# Description: Permettre de gerer des fichiers sur diverses machines.
#
#===============================================================================

#----- Lire les sources d'execution

source $GDefs(Dir)/tcl/Tools/HFManager/HFManager.ctes
source $GDefs(Dir)/tcl/Tools/HFManager/HFManager.int
source $GDefs(Dir)/tcl/Tools/HFManager/HFManager.txt

#-------------------------------------------------------------------------------
# Nom      : <HFManager::Close>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Quitter l'interface.
#
# Parametres:
#
# Retour    :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc HFManager::Close { } {
   variable Data

   set Data(Active) 0
   destroy .hfman

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#-------------------------------------------------------------------------------
# Nom      : <HFManager::FileGet>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la selection de fichier de l'usager.
#
# Parametres:
#   <Id>    : Identificateur de la boite
#
# Retour    :
#   <Files> : Liste des fichiers avec leur path complet
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc HFManager::FileGet { Id } {
   variable Host
   variable Save

   set idxs [.hfman.host.$Id.file.sel.list curselection]
   set files ""

   eval set params \$HFManager::Host(\$HFManager::Host(Name$Id))

   foreach idx $idxs {
      set file [file tail [lindex [.hfman.host.$Id.file.sel.list get $idx] end]]
      if { [string length $file]>0 } {
         lappend files $HFManager::Host(Path$Id)/$file
      }
   }

   return $files
}

#-------------------------------------------------------------------------------
# Nom      : <HFManager::FileDo>
# Creation : Decembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Executer la commande sur les fichiers un a la fois.
#
# Parametres:
#  <Prefix> : Prefixe de la commande
#  <Post>   : Postfixe de la commande
#  <Files>  : Liste des fichiers
#
# Retour    :
#
# Remarques :
#   -Du a la limitation du nombre de caractere de rsh, on doit effectuer les commandes
#    sur un fichier a la fois afin d'etre sur de ne pas depasser la limite
#
#-------------------------------------------------------------------------------

proc HFManager::FileDo { Prefix Post Files } {

   foreach file $Files {
      eval exec $Prefix \"$Post $file\"
   }
}

#-------------------------------------------------------------------------------
# Nom      : <HFManager::FileCommand>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Executer les commandes possibles sur les fichiers.
#
# Parametres:
#  <Id>     : Identificateur de la boite
#  <Command>: Commande a effectuer
#
# Retour    :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc HFManager::FileCommand { Id Command } {
   global GDefs env
   variable Host
   variable Lbl
   variable Txt
   variable Save

   .hfman config -cursor watch
   .hfman.host.$Id.file.sel.list config -cursor watch

   eval set params \$HFManager::Host(\$HFManager::Host(Name$Id))
   eval set prefix \$HFManager::Host(Prefix\$HFManager::Host(Name$Id))

   set file [HFManager::FileGet $Id]

   if { [llength $file] > 0 } {

      switch $Command {

        "FSTD" {
            set paths { }
            foreach path $file {
               lappend paths $HFManager::Host(Name$Id):$path
            }
            SPI::FileOpen NEW FieldBox "$HFManager::Host(Name$Id):$file" $FileBox::Type(FSTD) $paths
            return
            }

        "CHMOD" {
            set info [HFManager::GetInfo $Lbl(ModeT) $Lbl(Mode)]

            if { $info != "" } {
               HFManager::FileDo "$prefix [lindex $params $Save(ConnexionTypeIdx)] $HFManager::Host(Name$Id)"  "chmod $info" $file
            } else {
               .hfman config -cursor left_ptr
               .hfman.host.$Id.file.sel.list config -cursor left_ptr
               return
            }
         }
         "CHOWN" {
            set info [HFManager::GetInfo $Lbl(OwnerT) $Lbl(Owner)]

            if { $info != "" } {
               HFManager::FileDo "$prefix [lindex $params $Save(ConnexionTypeIdx)] $HFManager::Host(Name$Id)"  "chown $info" $file
            } else {
               .hfman config -cursor left_ptr
               .hfman.host.$Id.file.sel.list config -cursor left_ptr
               return
            }
         }
         "CHGRP" {
            set info [HFManager::GetInfo $Lbl(GroupT) $Lbl(Group)]

            if { $info != "" } {
               FileDo "$prefix [lindex $params $Save(ConnexionTypeIdx)] $HFManager::Host(Name$Id)"  "chgrp $info" $file
            } else {
               .hfman config -cursor left_ptr
               .hfman.host.$Id.file.sel.list config -cursor left_ptr
               return
            }
        }
        "RM" {
            if { [Dialog::Default .hfman 350 WARNING $Txt(Delete) "" 0 $Lbl(No) $Lbl(Yes)] } {
               if { [lindex $params $Save(ConnexionTypeIdx)]=="weather" } {
                  HFManager::WeatherFileRM $Id $file
               } else {
                  HFManager::FileDo "$prefix [lindex $params $Save(ConnexionTypeIdx)] $HFManager::Host(Name$Id)"  "rm -f" $file
               }
            } else {
               .hfman config -cursor left_ptr
               .hfman.host.$Id.file.sel.list config -cursor left_ptr
               return
            }
         }
         "RMDIR" {
            if { [Dialog::Default .hfman 350 WARNING $Txt(DeleteD) "" 0 $Lbl(No) $Lbl(Yes)] } {
                HFManager::FileDo "$prefix [lindex $params $Save(ConnexionTypeIdx)] $HFManager::Host(Name$Id)"  "rm -f -r" $file
            } else {
               .hfman config -cursor left_ptr
               .hfman.host.$Id.file.sel.list config -cursor left_ptr
               return
            }
         }
         "RENAME" {
            set info [HFManager::GetInfo $Lbl(NameT) $Lbl(Name)]

            if { $info != "" } {
               eval exec $prefix [lindex $params $Save(ConnexionTypeIdx)] -n $HFManager::Host(Name$Id) \"mv [lindex $file 0] $HFManager::Host(Path$Id)/$info\"
            } else {
               .hfman config -cursor left_ptr
               .hfman.host.$Id.file.sel.list config -cursor left_ptr
               return
            }
         }
         "STAT" {
            eval Dialog::Text .stat Stat \[exec $prefix [lindex $params $Save(ConnexionTypeIdx)] -n $HFManager::Host(Name$Id) \"stat $file\"\] 75 10
            return
         }
         "COPY" {
            set Host(From)      $Host(Name$Id)
            set Host(FromFiles) $file
            set Host(FromDel)   0
            set Host(FromTGZ)   0
            .hfman config -cursor left_ptr
            .hfman.host.$Id.file.sel.list config -cursor left_ptr
            return
         }
         "PASTE" {
            set Host(Format)   NONE
         }
         "PASTETGZ" {
            set Command PASTE
            set Host(Format)   TGZ
         }
         "PASTECMC" {
            set Command PASTE
            set Host(Format)   CMC
         }
      }
      HFManager::HostFiles $Id
   }

   if { $Command == "MKDIR" } {
      set info [HFManager::GetInfo $Lbl(MkDirT) $Lbl(MkDir)]

      if { $info != "" } {
         eval exec $prefix [lindex $params $Save(ConnexionTypeIdx)] -n $HFManager::Host(Name$Id) \"mkdir $HFManager::Host(Path$Id)/$info\"
         HFManager::HostFiles $Id
      }
   }

   if { $Command == "PASTE" } {
      set dir [pwd]
      foreach file $Host(FromFiles) {
         set filename [file tail $file]
         switch $Host(Format) {
            "TGZ" {
               exec tar -C [file dirname $file] -cf - $filename | gzip > /tmp/$filename[pid].tgz
               eval exec $prefix [lindex $params $Save(ConnexionTypeIdx)] $Host(From):/tmp/$filename[pid].tgz $Host(Name$Id):$Host(Path$Id)/$filename.tgz
               file delete -force /tmp/$filename[pid].tgz
            }

            "CMC" {
               cd [file dirname $file]
               exec cmcarc -a $file -f /tmp/$filename[pid].cmc
               eval exec $prefix [lindex $params $Save(ConnexionTypeIdx)] $Host(From):/tmp/$filename[pid].cmc $Host(Name$Id):$Host(Path$Id)/$filename.cmc
               file delete -force /tmp/$filename[pid].cmc

            }
            default { eval exec $prefix [lindex $params $Save(CopyCmdIdx)] $Host(From):$file $Host(Name$Id):$Host(Path$Id)/$filename }
         }
      }
      cd $dir
      HFManager::HostFiles $Id
      set Host(Format) NONE
   }

   .hfman.host.$Id.file.sel.list config -cursor left_ptr
   .hfman config -cursor left_ptr
}

proc HFManager::WeatherFileRM { Id files } {
   global env
   global GDefs
   variable Host
   variable Weather

   if { ![file exists $Weather(MustExist)] } {
      Dialog::Error .hfman [format [lindex $Error(MustExist) $GDefs(Lang)] $Weather(MustExist)]
      return
   }

   #----- Folder strucutre
   set date [clock format [clock seconds] -format "%Y%m%dT%H%M%SZ"]
   set rmFolder $Weather(TraceDir)/RM
   set datedRmFolder $rmFolder/$date
   file mkdir $datedRmFolder
   set cleanupFileLink $rmFolder/realtime_eer_cleanup.txt
   set cleanupFile $datedRmFolder/realtime_eer_cleanup.txt
   set folderSnapshotFile $datedRmFolder/folder_snapshot.txt
   set transferLog $datedRmFolder/transfer.log

   set filepaths [lmap x $files {string range $x [expr [string first $Weather(BasePath) $Host(Path$Id)]+[string length $Weather(BasePath)]] end}]

   set f [open $cleanupFile w]
   foreach filepath $filepaths {
      puts $f $filepath
   }
   close $f

   exec chmod 644 $cleanupFile
   file delete -force $cleanupFileLink
   file link $cleanupFileLink $cleanupFile

   set f [open $folderSnapshotFile w]
   puts $f "$Host(Path$Id):"
   foreach fileinfo $Host(File$Id) {
      puts $f $fileinfo
   }
   close $f

   set err [catch { exec $env(EER_DIRSCRIPT)/CMOI_webprods.ksh $cleanupFileLink eer/data/vaac/realtime_eer_cleanup.txt $GDefs(TransmitHost) 2>$transferLog } msg]

   if { $err } {
      Log::Print ERROR "Problemes avec la copie vers le PDS !!!"
   } else {
      Log::Print INFO "Les fichiers ( $filepaths ) seront effaces sous $Host(Name$Id) !"
   }
}

#-------------------------------------------------------------------------------
# Nom      : <HFManager::FilePopup>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Activer et desactiver les options selon le cas.
#
# Parametres:
#
# Retour    :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc HFManager::FilePopup { Id X Y LY } {
   variable Host
   variable Save

   eval set params \$Host(\$Host(Name$Id))

   #----- If no selection is made, select the one under the mouse
   if { ![llength [.hfman.host.$Id.file.sel.list curselection]] } {
      .hfman.host.$Id.file.sel.list selection set [.hfman.host.$Id.file.sel.list nearest $LY] [.hfman.host.$Id.file.sel.list nearest $LY]
   }

   #----- Reset feature states
   #-----   Enable Update Host
   .hfmanpopup$Id entryconfigure 0 -state normal
   #-----   Enable Action
   .hfmanpopup$Id entryconfigure 1 -state normal
   #-----   Enable Copy
   .hfmanpopup$Id entryconfigure 3 -state normal
   #-----   Enable Paste
   .hfmanpopup$Id entryconfigure 4 -state normal
   .hfmanpopup$Id entryconfigure 5 -state normal
   .hfmanpopup$Id entryconfigure 6 -state normal
   #-----   Enable Close Host
   .hfmanpopup$Id entryconfigure 9 -state normal

   #-----   Enable actions but RM
   .hfmanpopup$Id.action entryconfigure 0 -state normal
   .hfmanpopup$Id.action entryconfigure 1 -state normal
   .hfmanpopup$Id.action entryconfigure 2 -state normal
   .hfmanpopup$Id.action entryconfigure 5 -state normal
   .hfmanpopup$Id.action entryconfigure 6 -state normal
   .hfmanpopup$Id.action entryconfigure 7 -state normal
   .hfmanpopup$Id.action entryconfigure 9 -state normal
   .hfmanpopup$Id.action entryconfigure 11 -state normal

   #----- Toggle off host related features
   if { "$Host(Name$Id)"=="" || "$Host(Path$Id)"=="" } {
      #----- Disable Update Host
      .hfmanpopup$Id entryconfigure 0 -state disabled
      #----- Disable Action
      .hfmanpopup$Id entryconfigure 1 -state disabled
      #----- Disable Copy
      .hfmanpopup$Id entryconfigure 3 -state disabled
      #----- Disable Paste
      .hfmanpopup$Id entryconfigure 4 -state disabled
      .hfmanpopup$Id entryconfigure 5 -state disabled
      .hfmanpopup$Id entryconfigure 6 -state disabled
   } else {
      #----- Copy Paste Feature
      if { [lindex $params $Save(CopyCmdIdx)] == "-" } {
         #----- Disable Copy
         .hfmanpopup$Id entryconfigure 3 -state disabled
         #----- Disable Paste
         .hfmanpopup$Id entryconfigure 4 -state disabled
         .hfmanpopup$Id entryconfigure 5 -state disabled
         .hfmanpopup$Id entryconfigure 6 -state disabled
      } elseif { "$Host(From)"=="" || "$Host(FromFiles)"=="" } {
         #----- Disable Paste
         .hfmanpopup$Id entryconfigure 4 -state disabled
         .hfmanpopup$Id entryconfigure 5 -state disabled
         .hfmanpopup$Id entryconfigure 6 -state disabled
      }

      #----- Actions
      if { [lindex $params $Save(ConnexionTypeIdx)] == "weather" } {
         #----- Disable actions but RM
         .hfmanpopup$Id.action entryconfigure 0 -state disabled
         .hfmanpopup$Id.action entryconfigure 1 -state disabled
         .hfmanpopup$Id.action entryconfigure 2 -state disabled
         .hfmanpopup$Id.action entryconfigure 5 -state disabled
         .hfmanpopup$Id.action entryconfigure 6 -state disabled
         .hfmanpopup$Id.action entryconfigure 7 -state disabled
         .hfmanpopup$Id.action entryconfigure 9 -state disabled
         .hfmanpopup$Id.action entryconfigure 11 -state disabled
      }
   }

   #----- Toggle off window related features
   if { [llength $Host(Ids)]==1 } {
      #----- Disable Close Host
      .hfmanpopup$Id entryconfigure 9 -state disabled
   }

   tk_popup .hfmanpopup$Id $X $Y 0
}

#-------------------------------------------------------------------------------
# Nom      : <HFManager::HostClose>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Ferme une fenetre d'hote.
#
# Parametres:
#
# Retour    :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc HFManager::HostClose { Id } {
   variable Host

   #----- Rechercher un index valide

   set idx [lsearch -exact $Host(Ids) $Id]
   set Host(Ids) [lreplace $Host(Ids) $idx $idx]

   #----- Detruire la fenetre

   .hfman.host forget .hfman.host.$Id
   ComboBox::Destroy .hfman.host.$Id.path.sel
   ComboBox::Destroy .hfman.host.$Id.host.sel
   destroy .hfman.host.$Id
}

#-------------------------------------------------------------------------------
# Nom      : <HFManager::HostFiles>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer le contenu du repertoire de l'hote.
#
# Parametres:
#  <Id>     : Identificateur de la boite
#
# Retour    :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc HFManager::HostFiles { Id } {
   global GDefs env
   variable Host
   variable Save

   if { $Host(Name$Id)=="" } {
      return
   }

   .hfman config -cursor watch
   .hfman.host.$Id.file.sel.list config -cursor watch

   .hfman.host.$Id.file.sel.list selection clear 0 end
   set Host(File$Id) ""

   update idletasks

   eval set params \$Host(\$Host(Name$Id))
   eval set prefix \$Host(Prefix\$Host(Name$Id))

   set X11Forwarding ""

   if { $Host(Path$Id) == "" } {

      #----- Dans le cas d'un ssh on limite l'acces aux path sauvergardees

      if { [lindex $params $Save(ConnexionTypeIdx)] == "ssh" } {
         set Host(Path$Id) [lindex [lindex $params $Save(PathsIdx)] 0]

         #----- disables X11 forwarding ( bug pour l'usager ops@polaris, ... )

         set X11Forwarding "-x"

      } elseif { [lindex $params $Save(ConnexionTypeIdx)]=="weather" } {
         Log::Print DEBUG "on regarde sur site web meteo ... "
         set Host(Path$Id) [lindex [lindex $params $Save(PathsIdx)] 0]
      } else {
         eval set Host(Path$Id) \[exec $prefix [lindex $params $Save(ConnexionTypeIdx)] -n $Host(Name$Id) pwd\]
      }
   }

   if { [lindex $params $Save(ConnexionTypeIdx)]=="weather" } {
      HFManager::WeatherFiles $Id
   } else {
      if { $Host(Wild$Id) != "" } {
         set path  $Host(Path$Id)/$Host(Wild$Id)
      } else {
         set path  $Host(Path$Id)
      }
      catch { eval set Host(File$Id) \[split \[exec $prefix [lindex $params $Save(ConnexionTypeIdx)] -n $X11Forwarding $Host(Name$Id) ls -lap $path\] \\n\] }
   }
   .hfman.host.$Id.file.sel.list config -cursor left_ptr
   .hfman config -cursor left_ptr
}

proc HFManager::WeatherFiles { Id } {
   variable Host
   variable Weather

   set Host(File$Id) {}

   foreach filepath [glob $Weather(HostFilesGlobPattern)] {
      set f [open $filepath r]
      set folders [split [string map "{\n\n} {\u0080}" [read $f]] "\u0080"]
      close $f

      for {set i 0} {$i < [llength $folders]} {incr i} {
         set folder [split [string trim [lindex $folders $i]] "\n"]
         set folderPath [string range [lindex $folder 0] 0 end-1]
         set files [lrange $folder 2 end]

         set currentRelativePath [string range $folderPath [expr [string first $Weather(BasePath) $folderPath]+[string length $Weather(BasePath)]] end]
         set relativePath [string range $Host(Path$Id) [expr [string first $Weather(BasePath) $Host(Path$Id)]+[string length $Weather(BasePath)]] end]
         if { $currentRelativePath == $relativePath } {
            lappend Host(File$Id) {*}$files
            break
         }
      }
   }

   #----- Lsort -unique as second operation to avoid combining items by -index only
   set Host(File$Id) [lsort -index end [lsort -unique $Host(File$Id)]]
}

#-------------------------------------------------------------------------------
# Nom      : <HFManager::HostOpen>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer une nouvelle fenetre d'hote.
#
# Parametres:
#
# Retour    :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc HFManager::HostOpen { } {
   variable Host

   #----- Rechercher un index valide

   set id 0
   set no 0

   while { $id!=-1 } {
      incr no
      set id [lsearch -exact $Host(Ids) host$no]
   }
   lappend Host(Ids) host$no

   #----- Creer la fenetre

   HFManager::CreateHost host$no
   .hfman.host add .hfman.host.host$no

   return host$no
}

#-------------------------------------------------------------------------------
# Nom      : <HFManager::HostPath>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le changement de repertoire.
#
# Parametres:
#  <Id>     : Identificateur de la boite
#  <Y>      : Coornonne du curseur dans la liste  de fichiers
#
# Retour    :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc HFManager::HostPath { Id Y } {
   global GDefs
   variable Host
   variable Save
   variable Error

   if { $Host(Name$Id)!="" } {
      .hfman config -cursor watch
      .hfman.host.$Id.file.sel.list config -cursor watch
      update idletasks

      eval set params \$Host(\$Host(Name$Id))
      eval set prefix \$Host(Prefix\$Host(Name$Id))

      set sel [.hfman.host.$Id.file.sel.list get [.hfman.host.$Id.file.sel.list nearest $Y]]

      if { [llength $sel] >= 8 } {
         set file [lindex $sel 8]
         set link [lindex $sel end]

         if { "$file" != "$link" } {
            set ck $link
         } else {
            set ck $file
         }

         if { [string range $ck end end] == "/" } {
            if { [string range $ck 0 0] == "/" || $Host(Path$Id)=="" } {
               set path $ck
            } else {
               set path $Host(Path$Id)/$ck
            }

            #----- Restriction des repertoires pour un acces ssh

            if { [lindex $params $Save(ConnexionTypeIdx)] == "ssh" } {
               eval set Host(Path$Id) \[exec $prefix [lindex $params $Save(ConnexionTypeIdx)] -n -x $Host(Name$Id) \"cd $path\; pwd\" \]
            } else {
               eval set Host(Path$Id) \[exec $prefix [lindex $params $Save(ConnexionTypeIdx)] -n $Host(Name$Id) \"cd $path \; pwd\" \]
            }
            HFManager::HostFiles $Id
         }
      }
      .hfman.host.$Id.file.sel.list config -cursor left_ptr
      .hfman config -cursor left_ptr
   }
}

#-------------------------------------------------------------------------------
# Nom      : <HFManager::HostPathDel>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer le repertoire courant.
#
# Parametres:
#  <Id>     : Identificateur de la boite
#
# Retour    :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc HFManager::HostPathDel { Id } {
   variable Host
   variable Save

   if { $Host(Name$Id)!="" } {

      eval upvar #0 HFManager::Host(\$HFManager::Host(Name$Id)) params

      set paths [lindex $params $Save(PathsIdx)]
      set idx [lsearch -exact $paths $Host(Path$Id)]

      if { $idx != -1 } {
         lset params $Save(PathsIdx) [lreplace $paths $idx $idx]
      }

      HFManager::HostWrite $Id
   }
}

#-------------------------------------------------------------------------------
# Nom      : <HFManager::HostPathSave>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder le repertoire courant.
#
# Parametres:
#  <Id>     : Identificateur de la boite
#
# Retour    :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc HFManager::HostPathSave { Id } {
   variable Host
   variable Save

   if { $Host(Name$Id)!="" } {

      eval upvar #0 HFManager::Host(\$HFManager::Host(Name$Id)) params

      set paths [lindex $params $Save(PathsIdx)]

      if { $Host(Path$Id)!="" && [lsearch -exact $paths $Host(Path$Id)] == -1 } {
         lappend paths $Host(Path$Id)
         lset params $Save(PathsIdx) $paths
      }

      HFManager::HostWrite $Id
   }
}

#-------------------------------------------------------------------------------
# Nom      : <HFManager::HostSelect>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer l'hote
#
# Parametres:
#  <Id>        : Identificateur de la boite
#  <Hostnamne> : Nom de l'hote
#  <Hostpath>  : Path sur l'hote
#
# Retour    :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc HFManager::HostSelect { Id { Hostname "" } { Hostpath "" } } {
   global GDefs
   variable Host
   variable Save
   variable Error

   if { $Hostname!="" } {
      set Host(Name$Id) $Hostname
   }

   #----- On verifie l'existance de l'hote

   set idx [lsearch -exact $Host(List) $Host(Name$Id)]

   if { $idx == -1 } {
      set home [split [exec ssh -n -x $Host(Name$Id) pwd] " "]
      if { [llength $home] == 1 } {
         lappend Host(List) $Host(Name$Id)
         eval set Host(\$Host(Name$Id)) \"- ssh scp \{\}\"
         eval set Host(Prefix\$Host(Name$Id)) \"\"
         ComboBox::AddList .hfman.host.$Id.host.sel $Host(Name$Id)
         HFManager::HostWrite $Id
      } else {
         Dialog::Error .hfman $Error(Host)
         set $Host(Name$Id) ""
         return
      }
   }

   eval set params \$Host(\$Host(Name$Id))

   #----- On limites les fonctions pour les acces ssh

   if { [lindex $params $Save(ConnexionTypeIdx)] == "ssh" } {
      .hfman.host.$Id.path.save configure -state disabled
      .hfman.host.$Id.path.del  configure -state disabled
   } else {
      .hfman.host.$Id.path.save configure -state normal
      .hfman.host.$Id.path.del  configure -state normal
   }

   ComboBox::DelAll  .hfman.host.$Id.path.sel
   ComboBox::AddList .hfman.host.$Id.path.sel [lindex $params $Save(PathsIdx)]

   set Host(Path$Id) $Hostpath

   HFManager::HostFiles $Id
}

#-------------------------------------------------------------------------------
# Nom      : <HFManager::AsProject>
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

proc HFManager::AsProject { File } {
   variable Host
   variable Param

   if { [winfo exists .hfman] } {
      puts $File "#----- Tool: HFManager\n"
      puts $File "set HFManager::Param(Dock)   $Param(Dock)"
      puts $File "set HFManager::Param(Geom)   [winfo geometry .hfman]"
      puts $File "HFManager::Window"

      foreach id $Host(Ids) {
         puts $File "HFManager::CreateHost $id"
         puts $File "HFManager::HostSelect $id $Host(Name$id) $Host(Path$Id)"
      }
      puts $File "\n"
   }
}

#-------------------------------------------------------------------------------
# Nom      : <HFManager::HostRead>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire les definitions des hotes sauvegarder par l'usager.
#
# Parametres:
#
# Retour    :
#
# Remarques :
#  - The read data would benefit being saved inside a dictionary
#-------------------------------------------------------------------------------
proc HFManager::HostRead { } {
   global env
   global GDefs
   variable Host
   variable Save

   set Host(List) {}

   if { [file exists $Save(Path)] } {

      set f [open $Save(Path) r]

      while { ![eof $f] } {

         gets $f line

         if { $line != "" && [string range $line 0 0] != "#" } {
            set host [lindex $line $Save(DomainNameIdx)]
            lappend Host(List) $host

            set Host($host) $line

            set udoCmdOptions [lindex $line $Save(UdoCmdOptionsIdx)]
            if { $udoCmdOptions != "-" } {
               set Host(Prefix$host) "udo $udoCmdOptions"
            } else {
               set Host(Prefix$host) ""
            }
         }
      }
      close $f
   }

   if { [lsearch -exact $Host(List) $GDefs(Host)]==-1 } {
      set Host($GDefs(Host)) $Save(DefaultValues)
      lset Host($GDefs(Host)) 0 $GDefs(Host)
      set Host(Prefix$GDefs(Host)) ""
      lappend Host(List) $GDefs(Host)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <HFManager::HostWrite>
# Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Ecrire les definitions des hotes sauvegarder par l'usager.
#
# Parametres:
#  <Id>     : Identificateur de la boite
#
# Retour    :
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc HFManager::HostWrite { Id } {
   global env
   global GDefs
   variable Host
   variable Save
   variable Error

   if { ![file exists $Save(MustExist)] } {
      Dialog::Error .hfman [format [lindex $Error(MustExist) $GDefs(Lang)] $Save(MustExist)]
      return
   }

   if { [file exists $Save(Path)] } {
      file rename -force $Save(Path) $Save(Path)$Save(BackupExtension)
   }

   #----- Create formatString for data
   set columnHeaders $Save(ColumnHeaders)
   lset columnHeaders 0 "#[lindex $columnHeaders 0]"
   set maxLengths [lmap x $columnHeaders {string length $x}]
   foreach host $Host(List) {
      eval set params \$Host($host)
      set paramLengths [lmap x $params {string length $x}]
      set maxLengths [lmap max $maxLengths current $paramLengths {expr $current > $max ? $current : $max}]
   }
   lset maxLengths end 1
   set formatString [lmap max $maxLengths {set tmp "%-${max}s"}]
   set formatString [string map "{ } {  }" $formatString]

   set f [open $Save(Path) w]
   puts $f $Save(Header)
   puts $f [format $formatString {*}$columnHeaders]

   foreach host $Host(List) {
      eval set params \$Host($host)
      set params [string map [list "\{" "\{\{" "\}" "\}\}"] $params]
      puts $f "[format $formatString {*}$params]"
   }

   close $f

   eval set params \$Host(\$Host(Name$Id))

   ComboBox::DelAll  .hfman.host.$Id.path.sel
   ComboBox::AddList .hfman.host.$Id.path.sel [lindex $params $Save(PathsIdx)]
}

