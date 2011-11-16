#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : NowCaster.tcl
# Creation : Avril 2006 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Outils de now-casting.
#
# Remarques :
#
#===============================================================================

#----- Lire les sources d'execution

source $GDefs(Dir)/Apps/Tools/NowCaster/NowCaster.ctes
source $GDefs(Dir)/Apps/Tools/NowCaster/NowCaster.txt
source $GDefs(Dir)/Apps/Tools/NowCaster/NowCaster.int

#----- Lire les types de donnees

foreach type [lsort [glob $GDefs(Dir)/Apps/Tools/NowCaster/Types/*.tcl]] {
   lappend NowCaster::Data(Types) [file rootname [file tail $type]]
   source $type
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Close>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Ferme tout les fichiers ouvert et detruit la fenetre.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Close { } {
   variable Data

   if { [lindex [split $Page::Data(ToolMode) ":"] 0]=="NowCaster" } {
      SPI::ToolMode SPI Zoom
   }

   set Data(Active) 0
   set Data(Coo)    ""

   after cancel NowCaster::Now
   destroy .nowcaster

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Cast>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Mise a jour des donnees pour la date specifiee.
#
# Parametres :
#   <Sec>    : Date en secondes (0 -> utilise la date globale)
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Cast { { Sec 0 } } {
   global GDefs
   variable Msg
   variable Data

   if { $Sec } {
      set Data(Sec) $Sec
   }
   if { [string is integer $Data(Sec)] } {
      set Data(Date) [clock format $Data(Sec) -format "%Y%m%d %H:%M" -gmt true]

      foreach type $Data(Types) {
         NowCaster::${type}::Cast $Data(Sec)
      }

#      Viewport::LinkDo $Page::Data(VP)

      foreach page $Page::Data(Frames) {
#         projection configure $page -date $Data(Sec)
         Page::Update         $page
         Page::UpdateCommand  $page
         if { [set dt [expr $Data(Sec1)-$Data(Sec0)]]!=0 } {
            CVClock::Time $page $Data(Sec) [expr ($Data(Sec)-$Data(Sec0))*100.0/($Data(Sec1)-$Data(Sec0))]
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Now>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialisation de l'evenement de rafraichissement des donnees.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Now { } {
   global GDefs
   variable Msg
   variable Data

   set sec [clock seconds]
   .nowcaster.time.set configure -to $sec

   set Data(Sec)  $sec    ;#Date active
   set Data(Sec1) $sec    ;#Date limite
   set Data(Date) [clock format $Data(Sec) -format "%Y%m%d %H:%M" -gmt true]

   foreach type $Data(Types) {
      NowCaster::${type}::Now $Data(Sec) True
   }

   if { $Data(Now) } {
      after $Data(Delay) NowCaster::Now
   } else {
      after cancel NowCaster::Now
   }
   set Data(Job)  ""

   foreach page $Page::Data(Frames) {
      Page::Update         $page
      Page::UpdateCommand  $page
      CVClock::Time        $page $Data(Sec) -1
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::SetTimeScale>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Instaurer l'echelle temporelle.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::SetTimeScale { Sec0 { Sec1 0 } } {
   variable Data

   if { $Sec1 } {
      if { $Data(Sec0)==1e32 } {
         set Data(Sec1) $Sec1
      } else {
         set Data(Sec1) [expr $Data(Sec1)>$Sec1?$Data(Sec1):$Sec1]
      }
      set Data(Sec0) [expr $Data(Sec0)<$Sec0?$Data(Sec0):$Sec0]
   } else {
      set Data(Sec1) [expr $Data(Sec1)>$Sec0?$Data(Sec1):$Sec0]
   }

   set sec0 [clock scan "-1 $Data(PlayRange)" -base $Data(Sec1) -gmt True]
#   set Data(Sec0) [expr $sec0>$Data(Sec0)?$sec0:$Data(Sec0)]

   if { [winfo exists .nowcaster.time.set] } {
      .nowcaster.time.set configure -from $Data(Sec0) -to $Data(Sec1) -resolution $Data(PlayDelay)
   }
   NowCaster::Cast $Data(Sec1)
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Play>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Animer temporellement les donnees.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::Play { } {
   variable Data

   while { $Data(Play) } {
      update

      if { $Data(Sec)==$Data(Sec1) } {
         set Data(Sec) $Data(Sec0)
      } else {
         set dt [expr $Data(Sec1)-$Data(Sec)]
         if { $dt<0 || $dt<$Data(PlayDelay) } {
            set Data(Sec) $Data(Sec1)
         } else {
            incr Data(Sec) $Data(PlayDelay)
         }
      }
      NowCaster::Cast $Data(Sec)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::UpdateTypes>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour les parametres et l'affichage.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc NowCaster::UpdateTypes { } {
   variable Data

   foreach type $Data(Types) {
      NowCaster::${type}::Update
   }

   Page::Update $Page::Data(Frame)
   Page::UpdateCommand $Page::Data(Frame)
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::AsProject>
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

proc NowCaster::AsProject { File } {
   variable Data
   variable Param

   if { [winfo exists .nowcaster] } {
      puts $File "#----- Tool: NowCaster\n"
      puts $File "set NowCaster::Param(Dock)   $Param(Dock)"
      puts $File "set NowCaster::Param(Geom)   [winfo geometry .nowcaster]"
      puts $File "NowCaster::Window"
      puts $File "\n"
   }
}
