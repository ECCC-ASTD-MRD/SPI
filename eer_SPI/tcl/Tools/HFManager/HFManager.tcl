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

   set idxs [.hfman.host.$Id.file.sel.list curselection]
   set files ""

   eval set params \$HFManager::Host(\$HFManager::Host(Name$Id))

   if { [lindex $params 1]=="wget" } {
      set fileindex 3
   } else {
      set fileindex end
   }
puts stderr "$idxs $fileindex"

   foreach idx $idxs {
      set file [file tail [lindex [.hfman.host.$Id.file.sel.list get $idx] $fileindex]]
      if { [string length $file]>0 } {
         lappend files $HFManager::Host(Path$Id)/$file
      }
   }
puts stderr "$idxs $files"
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
   global GDefs
   variable Host
   variable Lbl
   variable Txt

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
               HFManager::FileDo "$prefix [lindex $params 1] $HFManager::Host(Name$Id)"  "chmod $info" $file
            } else {
               .hfman config -cursor left_ptr
               .hfman.host.$Id.file.sel.list config -cursor left_ptr
               return
            }
         }
         "CHOWN" {
            set info [HFManager::GetInfo $Lbl(OwnerT) $Lbl(Owner)]

            if { $info != "" } {
               HFManager::FileDo "$prefix [lindex $params 1] $HFManager::Host(Name$Id)"  "chown $info" $file
            } else {
               .hfman config -cursor left_ptr
               .hfman.host.$Id.file.sel.list config -cursor left_ptr
               return
            }
         }
         "CHGRP" {
            set info [HFManager::GetInfo $Lbl(GroupT) $Lbl(Group)]

            if { $info != "" } {
               FileDo "$prefix [lindex $params 1] $HFManager::Host(Name$Id)"  "chgrp $info" $file
            } else {
               .hfman config -cursor left_ptr
               .hfman.host.$Id.file.sel.list config -cursor left_ptr
               return
            }
        }
        "RM" {
            if { [Dialog::Default .hfman 350 WARNING $Txt(Delete) "" 0 $Lbl(No) $Lbl(Yes)] } {
               if { [lindex $params 1]=="wget" } {
                  #----- send a file to pds
                  set err [catch { exec $GDefs(Dir)/Script/HFdel.ksh $Host(Name$Id) $file $GDefs(FrontEnd) $GDefs(TransmitUser) } msg]
                  if { $err } {
                     Log::Print ERROR "Problems while calling HFdel.ksh :\n\n\t$msg"
                  }
               } else {
                  HFManager::FileDo "$prefix [lindex $params 1] $HFManager::Host(Name$Id)"  "rm -f" $file
               }
            } else {
               .hfman config -cursor left_ptr
               .hfman.host.$Id.file.sel.list config -cursor left_ptr
               return
            }
         }
         "RMDIR" {
            if { [Dialog::Default .hfman 350 WARNING $Txt(DeleteD) "" 0 $Lbl(No) $Lbl(Yes)] } {
                HFManager::FileDo "$prefix [lindex $params 1] $HFManager::Host(Name$Id)"  "rm -f -r" $file
            } else {
               .hfman config -cursor left_ptr
               .hfman.host.$Id.file.sel.list config -cursor left_ptr
               return
            }
         }
         "RENAME" {
            set info [HFManager::GetInfo $Lbl(NameT) $Lbl(Name)]

            if { $info != "" } {
               eval exec $prefix [lindex $params 1] -n $HFManager::Host(Name$Id) \"mv [lindex $file 0] $HFManager::Host(Path$Id)/$info\"
            } else {
               .hfman config -cursor left_ptr
               .hfman.host.$Id.file.sel.list config -cursor left_ptr
               return
            }
         }
         "STAT" {
            eval Dialog::Text .stat Stat \[exec $prefix [lindex $params 1] -n $HFManager::Host(Name$Id) \"stat $file\"\] 75 10
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
         eval exec $prefix [lindex $params 1] -n $HFManager::Host(Name$Id) \"mkdir $HFManager::Host(Path$Id)/$info\"
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
               eval exec $prefix [lindex $params 2] $Host(From):/tmp/$filename[pid].tgz $Host(Name$Id):$Host(Path$Id)/$filename.tgz
               file delete -force /tmp/$filename[pid].tgz
            }

            "CMC" {
               cd [file dirname $file]
               exec cmcarc -a $file -f /tmp/$filename[pid].cmc
               eval exec $prefix [lindex $params 2] $Host(From):/tmp/$filename[pid].cmc $Host(Name$Id):$Host(Path$Id)/$filename.cmc
               file delete -force /tmp/$filename[pid].cmc

            }
            default { eval exec $prefix [lindex $params 2] $Host(From):$file $Host(Name$Id):$Host(Path$Id)/$filename }
         }
      }
      cd $dir
      HFManager::HostFiles $Id
      set Host(Format) NONE
   }

   .hfman.host.$Id.file.sel.list config -cursor left_ptr
   .hfman config -cursor left_ptr
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

   #----- If no selection is made, select the one under the mouse
   if { ![llength [.hfman.host.$Id.file.sel.list curselection]] } {
      .hfman.host.$Id.file.sel.list selection set [.hfman.host.$Id.file.sel.list nearest $LY] [.hfman.host.$Id.file.sel.list nearest $LY]
   }

   if { "$Host(Name$Id)"=="" || "$Host(Path$Id)"=="" } {
      .hfmanpopup$Id entryconfigure 0 -state disabled
      .hfmanpopup$Id entryconfigure 1 -state disabled
      .hfmanpopup$Id entryconfigure 3 -state disabled
      .hfmanpopup$Id entryconfigure 4 -state disabled
      .hfmanpopup$Id entryconfigure 5 -state disabled
      .hfmanpopup$Id entryconfigure 6 -state disabled
   } else {
      if { "$Host(From)"=="" || "$Host(FromFiles)"=="" } {
         .hfmanpopup$Id entryconfigure 4 -state disabled
         .hfmanpopup$Id entryconfigure 5 -state disabled
         .hfmanpopup$Id entryconfigure 6 -state disabled
      } else {
         .hfmanpopup$Id entryconfigure 4 -state normal
         .hfmanpopup$Id entryconfigure 5 -state normal
         .hfmanpopup$Id entryconfigure 6 -state normal
      }

      .hfmanpopup$Id entryconfigure 0 -state normal
      .hfmanpopup$Id entryconfigure 1 -state normal
      .hfmanpopup$Id entryconfigure 3 -state normal
   }

   if { [llength $Host(Ids)]==1 } {
      .hfmanpopup$Id entryconfigure 9 -state disabled
   } else {
      .hfmanpopup$Id entryconfigure 9 -state normal
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
   global GDefs
   variable Host

   if { $Host(Name$Id)!="" } {
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

         if { [lindex $params 1] == "ssh" } {
            set Host(Path$Id) [lindex [lindex $params 3] 0]

            #----- disables X11 forwarding ( bug pour l'usager ops@polaris, ... )

            set X11Forwarding "-x"

         } elseif { [lindex $params 1]!="wget" } {
            eval set Host(Path$Id) \[exec $prefix [lindex $params 1] -n $Host(Name$Id) pwd\]
         } else {
            Log::Print DEBUG "on regarde sur site web meteo ... "
            set Host(Path$Id) [lindex [lindex $params 3] 0]
         }
      }

      if { [lindex $params 1]=="wget" } {
         set err [catch { exec $GDefs(Dir)/Script/HFwget.ksh $Host(Name$Id) $Host(Path$Id) /tmp/index[pid].res } msg]
         if { $err } {
            Log::Print ERROR "Problems while calling HFwget.ksh :\n\n\t$msg"
         }

         if { $Host(Wild$Id) != "" } {
            Log::Print DEBUG "egrep $Host(Wild$Id) /tmp/index[pid].res"
            catch { eval set Host(File$Id) \[split \[exec egrep $Host(Wild$Id) /tmp/index[pid].res ] \\n\] }
         } else {
            catch { eval set Host(File$Id) \[split \[exec cat /tmp/index[pid].res ] \\n\] }
         }
         file delete -force /tmp/index[pid].res
      } else {
         if { $Host(Wild$Id) != "" } {
            set path  $Host(Path$Id)/$Host(Wild$Id)
         } else {
            set path  $Host(Path$Id)
         }
         catch { eval set Host(File$Id) \[split \[exec $prefix [lindex $params 1] -n $X11Forwarding $Host(Name$Id) ls -lap $path\] \\n\] }
      }
      .hfman.host.$Id.file.sel.list config -cursor left_ptr
      .hfman config -cursor left_ptr
   }
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

            if { [lindex $params 1] == "ssh" } {
               eval set Host(Path$Id) \[exec $prefix [lindex $params 1] -n -x $Host(Name$Id) \"cd $path\; pwd\" \]
            } else {
               eval set Host(Path$Id) \[exec $prefix [lindex $params 1] -n $Host(Name$Id) \"cd $path \; pwd\" \]
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

   if { $Host(Name$Id)!="" } {

      eval upvar #0 HFManager::Host(\$HFManager::Host(Name$Id)) params

      set paths [lindex $params 3]
      set idx [lsearch -exact $paths $Host(Path$Id)]

      if { $idx != -1 } {
         set params "[lindex $params 0] [lindex $params 1] [lindex $params 2] {[lreplace $paths $idx $idx]}"
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

   if { $Host(Name$Id)!="" } {

      eval upvar #0 HFManager::Host(\$HFManager::Host(Name$Id)) params

      set paths [lindex $params 3]

      if { $Host(Path$Id)!="" && [lsearch -exact $paths $Host(Path$Id)] == -1 } {
         lappend paths $Host(Path$Id)
         set params "[lindex $params 0] [lindex $params 1] [lindex $params 2] {$paths}"
      }

      HFManager::HostWrite $Id
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
#
#-------------------------------------------------------------------------------

proc HFManager::HostRead { } {
   global env
   global GDefs
   variable Host

   set Host(List) {}

   if { [file exists $env(HOME)/.spi/HFManager] } {

      set f [open $env(HOME)/.spi/HFManager r]

      while { ![eof $f] } {

         gets $f line

         if { $line != "" && [string range $line 0 0] != "#" } {
            set host [lindex $line 0]
            lappend Host(List) $host

            set Host($host) [lrange $line 1 end]

            if { [lindex $line 1] != "-" } {
               set Host(Prefix$host) "udo [lindex $line 1]"
            } else {
               set Host(Prefix$host) ""
            }
         }
      }
      close $f
   }

   if { [lsearch -exact $Host(List) $GDefs(Host)]==-1 } {
      set Host($GDefs(Host)) [list - ssh scp {}]
      set Host(Prefix$GDefs(Host)) ""
      lappend Host(List) $GDefs(Host)
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

   if { [lindex $params 1] == "ssh" } {
      .hfman.host.$Id.path.save configure -state disabled
      .hfman.host.$Id.path.del  configure -state disabled
   } else {
      .hfman.host.$Id.path.save configure -state normal
      .hfman.host.$Id.path.del  configure -state normal
   }

   ComboBox::DelAll  .hfman.host.$Id.path.sel
   ComboBox::AddList .hfman.host.$Id.path.sel [lindex $params 3]

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
   variable Host

   if { ![file exists $env(HOME)/.spi] } {
      file mkdir $env(HOME)/.spi
   }

   if { [file exists $env(HOME)/.spi/HFManager] } {
      file rename -force $env(HOME)/.spi/HFManager $env(HOME)/.spi/HFManager.old
   }

   set f [open $env(HOME)/.spi/HFManager w]

   puts $f "# Liste des hotes pour HFManager"
   puts $f "#"
   puts $f "#Domain complet           udo      Connnexion Copy  Paths"
   puts $f "#"

   foreach host $Host(List) {

      eval set params \$Host($host)
      puts $f "[format "%-25s %-7s  %-10s %-5s" $host [lindex $params 0] [lindex $params 1] [lindex $params 2]] {[lindex $params 3]}"
   }
   close $f

   eval set params \$Host(\$Host(Name$Id))

   ComboBox::DelAll  .hfman.host.$Id.path.sel
   ComboBox::AddList .hfman.host.$Id.path.sel [lindex $params 3]
}

