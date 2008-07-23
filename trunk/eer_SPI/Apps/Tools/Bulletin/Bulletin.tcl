#=============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# Dorval, Quebec
#
# Projet     : Boite a outils
# Nom        : <Bull_Volc.tcl>
# Creation   : Janvier 1997 - S. Trudel - CMC/CMOE
#
# Description:
#
#        Permettre d'affiche un bulletin relatif a une
#        presence de cendres volcaniques.
#
# Remarques  :
#
#===========================================================================

#----- Lire les sources d'execution

source $GDefs(Dir)/Apps/Tools/Bulletin/Bulletin.ctes
source $GDefs(Dir)/Apps/Tools/Bulletin/Bulletin.txt
source $GDefs(Dir)/Apps/Tools/Bulletin/Bulletin.int

#----------------------------------------------------------------------------
# Nom      : <Bulletin::CommandToggle>
# Creation : Septembre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue les evenements de changement entre deux etats.
#
# Parametres :
#  <Cmd1>    : Premiere commande a effectuer
#  <Sleep1>  : Delai avant d'effectuer la premiere commande (ms)
#  <Cmd2>    : Deuxieme commande a effctuer
#  <Sleep2>  : Delai avant d'effectuer la deuxieme commande (ms)
#
# Remarques :
#
#--------------------------------------------------------------------------------

proc Bulletin::CommandToggle { Cmd1 Sleep1 Cmd2 Sleep2 } {

   catch {
      eval $Cmd1
      after $Sleep1 [list Bulletin::CommandToggle $Cmd2 $Sleep2 $Cmd1 $Sleep1]
   }
}

#----------------------------------------------------------------------------
# Nom      : <Bulletin::DatesMenu>
# Creation : Juin 1997 - J.P. Gauthier - CMC/CMOE
#
# But      : Insere les dates disponibles dans les menus de dates
#
# Parametres :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Bulletin::DatesMenu { } {
   variable Bul

   #----- Extraire le repertoire

   set dirlist [exec ls $Bul(Path)]
   set Bul(Archives) {}
   set mois_lst { xxx Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec }

   #----- Garder seulement les fichier archives

   foreach item $dirlist {
      if { [string compare [string range $item 11 14] "tar"] == 0 } {
         lappend Bul(Archives) $item
      }
   }

   #----- Inserer les commandes dans les menus

   destroy .bulletin.bas.for.menu
   menu .bulletin.bas.for.menu -tearoff 0 -bd 1

   set annee_presente [exec date -u +%Y]
   set old_annee $annee_presente

   foreach item $Bul(Archives) {

      #----- Extraire la date du nom des fichiers archives

      set annee [string range $item 4 7]

      #-----Verifier le premier chiffre des mois pour le 0

      set dix [string range $item 8 8]

      if { $dix == 0 } {
         set mois [ string range $item 9 9 ]
      } else {
         set mois [string range $item 8 9 ]
      }

      set date [lindex $mois_lst $mois]\/$annee

      #----- Faire des sous-menus pour les annees passe

      if { $annee != $annee_presente && $annee != $old_annee } {

         set menufor .bulletin.bas.for.menu.$annee

         .bulletin.bas.for.menu add cascade -label ___/$annee -menu $menufor
         menu $menufor -tearoff 0 -bd 1

      } elseif { $annee == $annee_presente } {

         set menufor .bulletin.bas.for.menu
      }

      $menufor add command -label $date -command "set Bulletin::Bul(Date) $date; Bulletin::InsertArchives $item"
      set old_annee $annee
   }

   #----- Inserer les fichiers non archives

   set date [exec date -u +%b/%Y]
   .bulletin.bas.for.menu add command -label $date -command "set Bulletin::Bul(Date) $date; Bulletin::InsertBull"

   set Bul(Date) $date
}

#----------------------------------------------------------------------------
# Nom      : <Bulletin::Draw>
# Creation : Juillet 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche la region selectionne dans le viewport actif en utilisant
#            l'outils de dessin
#
# Parametres :
#   <Draw>   : Type de dessin (line,poly)
#   <Zoom>   : Zoom sur la region
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Bulletin::Draw { Draw Zoom } {
   global GDefs
   variable Msg

   set text [string map { "\n" "" " " "" "." "" "-" " " } [.bulletin.mid.t get sel.first sel.last]]
   set Data(Lat0) 999
   set Data(Lon0) 999
   set Data(Lat1) -999
   set Data(Lon1) -999

   set ok [catch {
      foreach coord $text {
          set da [string index $coord 0]
          set la [Convert::Minute2Decimal "[string range $coord 1 2] [string range $coord 3 4]"]

         if { $da=="S" } {
            set la [expr -$la]
         }

         set do [string index $coord 5]
         set lo [Convert::Minute2Decimal "[string range $coord 6 8] [string range $coord 9 10]"]
         if { $do=="W" } {
            set lo [expr -$lo]
         }
         lappend coords $la $lo 0.0
         set Data(Lat0) [expr $la<$Data(Lat0)?$la:$Data(Lat0)]
         set Data(Lon0) [expr $lo<$Data(Lon0)?$lo:$Data(Lon0)]
         set Data(Lat1) [expr $la>$Data(Lat1)?$la:$Data(Lat1)]
         set Data(Lon1) [expr $lo>$Data(Lon1)?$lo:$Data(Lon1)]
      }
   } error]

   if { $ok } {
      Dialog::CreateErrorListing .bulletin [lindex $Msg(Coords) $GDefs(Lang)]\n\n$error [.bulletin.mid.t get sel.first sel.last] $GDefs(Lang)
   } else {
      Drawing::Window
      set Drawing::Data(GeoRef) 1
      set Drawing::Current(Color) #000000
      set Drawing::Current(Width) 2
      set Drawing::Current(Line) 0
      set Drawing::Current(Pattern) @$GDefs(Dir)/Resources/Bitmap/stipple6-32.xbm
      set Drawing::Current(Outline) #000000
      Drawing::ItemAdd $Page::Data(Frame) $Draw $coords
      SPI::ToolMode SPI Zoom

      if { $Zoom } {
         ProjCam::CloseUp $Page::Data(Frame) $Page::Data(Frame) $Viewport::Data(VP) $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) 0.10
      } else {
         Viewport::GoTo $Page::Data(Frame) $Data(Lat0) $Data(Lon0)
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Bulletin::InsertArchives>
# Creation : Juillet 1997 - J.P. Gauthier - CMC/CMOE -
#
# But      : Insere les bulletins des mois archives.
#
# Parametres :
#    <Nom>   : Nom du fichier de bulletin.
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Bulletin::InsertArchives { Nom } {
   variable Bul

   .bulletin config -cursor watch
   update idletasks

   #----- Efface les anciens fichiers d'archives

   file delete -force $Bul(PathTmp)
   file mkdir $Bul(PathTmp)

   set path [pwd]
   cd $Bul(PathTmp)

   #----- Decompresse les archives selectionnees

   Debug::TraceProc "Getting archives $Bul(Path)/$Nom"
   catch { exec tar xf $Bul(Path)/$Nom }

   #----- Insere les archives dans la liste de bulletins

   set file_lst [exec ls -1r]
   set Bul(Lst) {}

   catch { .bulletin.mid.frm.scrllist delete 0 end }
   foreach bulle $file_lst {

      lappend Bul(Lst) $Bul(PathTmp)/$bulle
      .bulletin.mid.frm.scrllist insert end $bulle
   }

   Bulletin::SelectBull .bulletin.mid.frm.scrllist 0 0
   cd $path

   .bulletin config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Bulletin::InsertBull>
# Creation : Juillet 1997 - J.P. Gauthier - CMC/CMOE -
#
# But      : Insere les bulletins relatifs au mois actuel.
#
# Parametres :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Bulletin::InsertBull { } {
   variable Bul

   set file_lst [exec ls -1r $Bul(Path)]

   catch { .bulletin.mid.frm.scrllist delete 0 end }
   set Bul(Lst) {}

   foreach bulle $file_lst {

      if { [string compare [string range $bulle 11 14] "tar"] != 0 \
           && [string compare [string range $bulle 0 2] "ash"] == 0 } {

         lappend Bul(Lst) $Bul(Path)/$bulle
         .bulletin.mid.frm.scrllist insert end $bulle
      }
   }
   Bulletin::SelectBull .bulletin.mid.frm.scrllist 0 0
}

#----------------------------------------------------------------------------
# Nom      : <Bulletin::PrintCommand>
# Creation : Octobre 2005 - J.P. Gauthier - CMC/CMOE
#
# But      : Impression du bulletin courant.
#
# Parametres :
#  <Widget>  : Nom du widget a imprimer .
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Bulletin::PrintCommand { Widget } {
   global   GDefs
   variable Lbl
   variable Msg
   variable Bul

   #----- Recuperer la selection

   set what     1
   set selected [.bulletin.mid.t tag ranges sel]

   #----- Si il y a selection, verifier ce qu'il faut imprimer

   if { $selected != "" } {
      set what [Dialog::CreateDefault .bulletin 300 [lindex $Lbl(Print) $GDefs(Lang)] \
               [lindex $Msg(Selection) $GDefs(Lang)] warning 0 \
               [lindex $Lbl(Selection) $GDefs(Lang)] [lindex $Lbl(Tout) $GDefs(Lang)]]
   }

   set f [open /tmp/Bulletin[pid].txt w]

   #----- Imprime l'entete

   set bulle [lindex $Bul(Lst) [lindex [.bulletin.mid.frm.scrllist curselection] 0]]

   puts $f "Bulletin volcanique $bulle."
   puts $f " "
   close $f

   #----- Imprime le tout(1) ou la selection(0)
   if { $what==1 } {
      exec cat $bulle | tr '\[a-z\]' '\[A-Z\]' >> /tmp/[pid].txt
   } else {
      exec echo "[.bulletin.mid.t get [lindex $selected 0] [lindex $selected 1]]" >> /tmp/Bulletin[pid].txt
   }

   PrintBox::PrintTXT /tmp/Bulletin[pid].txt
   PrintBox::Destroy
}

#----------------------------------------------------------------------------
# Nom      : <Bulletin::ReadCurrent>
# Creation : Juin 1997 - J.P. Gauthier - CMC/CMOE
#
# But      : afficher les bulletins courant.
#
# Parametres :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Bulletin::ReadCurrent { } {
   global GDefs
   variable Bul
   variable Msg

   .bulletin config -cursor watch
   set time [clock format [clock seconds] -format "%H:%M" -gmt true]
   set Bul(Doing) "[lindex $Msg(Courant) $GDefs(Lang)] ($time)"
   update idletasks

   if { ![file exists $Bul(PathTmp)] } {
      file mkdir $Bul(PathTmp)
   }

   #----- relire la liste des bulletins au cas ou il y en aurait des nouveau

   .bulletin.bas.for.menu invoke end

   #----- Si par hasard on vient de changer de mois, updater le menu des archives
   #      Nombre de fichier < 5 dans la liste pour couvrir la premiere journee d'un
   #      nouveau mois

   if { [.bulletin.mid.frm.scrllist index end ] < 4 } {
      DatesMenu
   }

   #----- si on a deja un courant dans la liste, le supprimer

   if { $Bul(Courant_Name) != "" } {
      file delete $Bul(PathTmp)/$Bul(Courant_Name)
   }

   #----- Recherher les nouveau courants pour l'heure actuelle

   set Bul(Courant_Name) "ash_courant_$time"

   catch { exec $Bul(PathScript)/volcanic_script $Bul(PathTmp)/tmp/$Bul(Courant_Name) }
   file rename $Bul(PathTmp)/tmp/$Bul(Courant_Name) $Bul(PathTmp)/$Bul(Courant_Name)

   #----- Inserer un item courant dans la liste

   .bulletin.mid.frm.scrllist insert 0 $Bul(Courant_Name)
   set Bul(Lst) [linsert $Bul(Lst) 0 $Bul(PathTmp)/$Bul(Courant_Name)]

   #----- Affiche le contenu du fichier courant.

   Bulletin::SelectBull .bulletin.mid.frm.scrllist 0 0
   set Bul(Doing) ""
   .bulletin config -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Bulletin::SelectBull>
# Creation : Fevrier 1997 - S. Trudel - CMC/CMOE
#
# But      : afficher un nouveau bulletin volcanique.
#
# Parametres    :
#    <Parent>   : path du parent.
#    <Position> : indice dans la liste des bulletins.
#    <Bind>     : indique que l'appel se fait a partir du <bind>.
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Bulletin::SelectBull { Parent Position Bind } {
   variable Bul
   variable Search

   $Parent selection clear 0 end

   #----- si l'appel se fait a partir du bind, on recupere la position de
   #      la souris sinon on recupere l'index de la liste.

   if { $Bind } {
      set Bul(Choix) [ $Parent nearest $Position]
   } else {
      $Parent selection set $Position
      set Bul(Choix) $Position
   }

   #----- Afficher le nouveau contenu du bulletin.

   .bulletin.mid.t delete 1.0 end
   .bulletin.mid.t insert 0.0 [exec cat [lindex $Bul(Lst) $Bul(Choix)] | tr '\[a-z\]' '\[A-Z\]' ]

   #----- Preparer les index de recherche

   set Search(LocB) 1.0
   set Search(LocE) [.bulletin.mid.t index end]

   #----- Rehausse les mots cles.

   Bulletin::TextSearch .bulletin.mid.t "ASH" search
   .bulletin.mid.t tag configure search -background red -foreground white

   Bulletin::TextSearch .bulletin.mid.t "ASH UUA " search1
   .bulletin.mid.t tag configure search1 -background red -foreground white

   Bulletin::TextSearch .bulletin.mid.t "VOLCANO" search2
   .bulletin.mid.t tag configure search2 -background yellow -foreground black

   Bulletin::TextSearch .bulletin.mid.t "VOLCANIC" search3
   .bulletin.mid.t tag configure search3 -background yellow -foreground black

   Bulletin::TextSearch .bulletin.mid.t "WV" search4
   .bulletin.mid.t tag configure search4 -background orange -foreground black

   Bulletin::TextSearch .bulletin.mid.t "FV" search5
   .bulletin.mid.t tag configure search5 -background blue -foreground white

   Bulletin::TextSearch .bulletin.mid.t "VAAC" search6
   .bulletin.mid.t tag configure search6 -background darkgreen -foreground white
}

#----------------------------------------------------------------------------
# Nom      : <Bulletin::SearchNext>
# Creation : Septembre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue la recherche d'un token dans la liste des bulletins
#            presentement affichee.
#
# Parametres    :
#  <TextWidget> : Path du "widget de texte"
#  <Direct>     : Direction de recherche
#
# Remarques :
#    - L'algorithme est recursif, il s'appelle lui meme lorsqu'il change de
#      bulletin.
#    - En recherche avant (forwards), on incremente l'index de debut vers l'index
#      de fin.
#    - En recherche arriere (backwards), on decrement l'index de fin vers l'index
#      de debut. (Ou l'index de debut est l'index de fin et vice-versa)
#
#----------------------------------------------------------------------------

proc Bulletin::SearchNext { TextWidget Direct } {
   global GDefs
   variable Search
   variable Bul
   variable Lbl
   variable Msg

   set Bul(Doing) "[lindex $Msg(Recherche) $GDefs(Lang)] $Search(Token)"
   .bulletin configure -cursor watch
   update

   if { ! $Search(Handle) } {
      set Bul(Doing) "[lindex $Msg(Cancel) $GDefs(Lang)]"
      return
   }
   $TextWidget tag delete found

   #----- Effectuer la recherche

   if { $Direct == "forwards" } {
      set Search(LocE) [$TextWidget index end]

      set Search(LocB) [$TextWidget search -forwards -nocase -regexp -count Search(Count) \
         $Search(Token) $Search(LocB) $Search(LocE)]

      #----- Si la recherche est concluante, centrer la vue sur le token

      if { $Search(LocB) != "" } {
         $TextWidget tag add found $Search(LocB) "$Search(LocB) + $Search(Count) char"
         $TextWidget see $Search(LocB)
         set Search(LocB) [expr $Search(LocB) + 1.0]

      #----- Sinon parcourir les autres bulletins recursivement si en mode global

      } else {
         if { $Bul(Choix) < [expr [.bulletin.mid.frm.scrllist index end] -1] && $Search(Mode) } {
            incr Bul(Choix)
            Bulletin::SelectBull .bulletin.mid.frm.scrllist $Bul(Choix) false
            .bulletin.mid.frm.scrllist see $Bul(Choix)
            Bulletin::SearchNext $TextWidget $Direct
         } else {
            Dialog::CreateDefault .bulletin 300 [lindex $Lbl(SearchEnd) $GDefs(Lang)] \
            [lindex $Msg(Fin) $GDefs(Lang)] warning 0 Ok
            set Search(LocB) [expr $Search(LocE)-1.0]
         }
      }
   } else {
      set Search(LocE) 0.0

      set Search(LocB) [$TextWidget search -backwards -nocase -regexp -count Search(Count) \
         $Search(Token) $Search(LocB) $Search(LocE)]

      #----- Si la recherche est concluante, centrer la vue sur le token

      if { $Search(LocB) != "" } {
         $TextWidget tag add found $Search(LocB) "$Search(LocB) + $Search(Count) char"
         $TextWidget see $Search(LocB)
         set Search(LocB) [expr $Search(LocB) - 1.0]

      #----- Sinon parcourir les autres bulletins recursivement si en mode global

      } else {
         if { $Bul(Choix) > 0  && $Search(Mode) } {
            incr Bul(Choix) -1
            Bulletin::SelectBull .bulletin.mid.frm.scrllist $Bul(Choix) false
            .bulletin.mid.frm.scrllist see $Bul(Choix)
            set Search(LocB) [$TextWidget index end]
            Bulletin::SearchNext $TextWidget $Direct
         } else {
            Dialog::CreateDefault .bulletin 300 [lindex $Lbl(SearchEnd) $GDefs(Lang)] \
            [lindex $Msg(Fin) $GDefs(Lang)] warning 0 Ok
            set Search(LocB) $Search(LocE)
         }
      }
   }
   .bulletin configure -cursor left_ptr
}

#----------------------------------------------------------------------------
# Nom      : <Bulletin::Close>
# Creation : Septembre 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Termine l'application et supprime le repertoire temporaire.
#
# Parametres :

# Remarques :
#
#----------------------------------------------------------------------------

proc Bulletin::Close { } {
   variable Data
   variable Bul

   file delete -force $Bul(PathTmp)
   set Data(Active) 0
   destroy .bulletin

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#----------------------------------------------------------------------------
# Nom      : <Bulletin::TextSearch>
# Creation : ???
#
# But      : Search for all instances of a given string in a text widget and
#            apply a given tag to each instance found.
#
# Parametres :
#  <W>       : The window in which to search.  Must be a text widget.
#  <String>  : The string to search for.  The search is done using
#              exact matching only;  no special characters.
#  <Tag>     : Tag to apply to each instance of a matching string.
#
# Remarques :
#
#--------------------------------------------------------------------------------

proc Bulletin::TextSearch { W String Tag } {

   if {$String == ""} {
      return
   }
   set cur 1.0
   while 1 {
   set cur [$W search -count length $String $cur end]
      if {$cur == ""} {
         break
      }
      $W tag add $Tag $cur "$cur + $length char"
      set cur [$W index "$cur + $length char"]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Bulletin::AsProject>
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

proc Bulletin::AsProject { File } {
   variable Data
   variable Param

   if { [winfo exists .bulletin] } {
      puts $File "#----- Tool: Bulletin\n"
      puts $File "set Bulletin::Param(Dock)   $Param(Dock)"
      puts $File "set Bulletin::Param(Geom)   [winfo geometry .bulletin]"
      puts $File "Bulletin::Window"
   }
}
