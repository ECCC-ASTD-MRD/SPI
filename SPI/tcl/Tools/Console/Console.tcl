#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie d'outils SPI.
# Fichier  : Console.tcl
# Creation : Janvier 2006 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Permet d'afficher une console de commande.
#
# Fonctions:
#
#
# Remarques :
#
#===============================================================================

#----- Lire les sources d'execution

source $GDefs(Dir)/tcl/Tools/Console/Console.ctes
source $GDefs(Dir)/tcl/Tools/Console/Console.txt
source $GDefs(Dir)/tcl/Tools/Console/Console.int

#----------------------------------------------------------------------------
# Nom      : <Console::Close>
# Creation : Janvier 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Fermer l'outils Console.
#
# Parametres   :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Console::Close { } {

   destroy .console

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#----------------------------------------------------------------------------
# Nom      : <Console::History>
# Creation : Janvier 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Parcourir l'historique des commandes.
#
# Parametres   :
#   <Text>     : Widget Text
#   <Index>    : Index dans l'historique
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Console::History { Text Index } {
   variable Data

   $Text delete 1.0 end

   if { $Data(HistoryIndex)<0 } { set Data(HistoryIndex) 0 }
   if { $Data(HistoryIndex)>=[llength $Data(History)] } {
     set Data(HistoryIndex) [llength $Data(History)]
   } else {
      $Text insert end [lindex $Data(History) $Data(HistoryIndex)]
   }
   $Text mark set insert end
   update idletasks
   after 100
}

#----------------------------------------------------------------------------
# Nom      : <Console::Eval>
# Creation : Janvier 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Evalue la commande tappe
#
# Parametres   :
#   <Text>     : Widget Text
#   <Keep>     : Garder dans l'historique
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Console::Eval { Text Keep } {
   global errorInfo
   variable Data

   set Data(HistoryIndex) 0

   set cmd [string trim [$Text get 1.0 end] '\n']

   if { [string length [string trim $cmd]]!=0 } {

      #----- Catch any error so as to switch to next line anyway

      if { [catch { uplevel #0 $cmd } errno] } {
         error $errno $errorInfo
      } else {
         if { $Keep } {
            lappend Data(History) $cmd
         }
      }
   }
   $Text delete 1.0 end
   $Text mark set insert end
}

#----------------------------------------------------------------------------
# Nom      : <Console::Write>
# Creation : Janvier 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Ecrire les parametres de configuration
#
# Parametres   :
#   <File>     : Identificateur du fichier
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Console::Write { File } {

   puts $File "set Console::Data(History) \{ $Console::Data(History) \}"
}

#-------------------------------------------------------------------------------
# Nom      : <Console::AsProject>
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

proc Console::AsProject { File } {
   variable Data
   variable Param

   if { [winfo exists .console] } {
      puts $File "#----- Tool: Console\n"
      puts $File "set Console::Param(Dock)   $Param(Dock)"
      puts $File "set Console::Param(Geom)   [winfo geometry .console]"
      puts $File "set Console::Data(History) { $Data(History) }"
      puts $File "Console::Window"
      puts $File "\n"
   }
}
