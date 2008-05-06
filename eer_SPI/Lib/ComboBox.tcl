#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : ComboBox.tk
# Creation : Mai 1998 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Ce package permet de creer et de manipuler une "ComboBox"
#
# Fonctions:
#
#    ComboBox::Add        { W Item }
#    ComboBox::AddList    { W List }
#    ComboBox::Close      { W }
#    ComboBox::Count      { W }
#    ComboBox::Create     { W IVar Edit Type Mode Max List Width Height args }
#    ComboBox::Del        { W Item }
#    ComboBox::DelAll     { W }
#    ComboBox::Destroy    { W }
#    ComboBox::Disable    { W }
#    ComboBox::Enable     { W }
#    ComboBox::Exec       { W Pos }
#    ComboBox::Index      { W Search Item }
#    ComboBox::List       { W }
#    ComboBox::Open       { W }
#    ComboBox::Place      { W { Set True } }
#    ComboBox::Select     { W }
#    ComboBox::SelectNext { W Incr }
#    ComboBox::Set        { W Pos }
#    ComboBox::SetExec    { W }
#
# Remarques :
#    -Concu a partir de namespace donc utilisable seulement en TCL 8.0 et +
#
#===============================================================================

package provide ComboBox 3.1

proc IdComboBox { Show } {

   if { $Show } {
      puts "(INFO) Loading Standard CMC/CMOE Widget Package ComboBox Version 3.1"
   }
}

namespace eval ComboBox {
   global  GDefs
   variable Resources
   variable Data

   set Data(Grab) {}

   #----- Definitions des parametres du ComboBox

   catch {

      set Resources(Up)            "@$GDefs(Dir)/Resources/Bitmap/combobox_up.ico"
      set Resources(Down)          "@$GDefs(Dir)/Resources/Bitmap/combobox_down.ico"
      set Resources(Scale)         "@$GDefs(Dir)/Resources/Bitmap/combobox_scale.ico"
      set Resources(Border)        1
      set Resources(Background)    $GDefs(ColorLight)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Add>
# Creation : Fevrier 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ajoute un item dans la liste du combobox.
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou inserer
#   <Item>   : Item a inserer dans la liste du ComboBox
#
# Retour   :
#  <index> : Index ou l'item a ete inserer dans la liste (-1 si pas inserer)
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Add { W Item } {

   #----- Extraction des variables d'etats du Combobox

   upvar #0 ComboBox::${W}typ Typ
   upvar #0 ComboBox::${W}mod Mod
   upvar #0 ComboBox::${W}max Max
   upvar #0 ComboBox::${W}lst Lst
   upvar #0 ComboBox::${W}top Top

   set nb [ComboBox::Count $W]

   #----- Verification du nombre maximum d'item

   if { $Max > 0 && $nb >= $Max } {

      return -1

   } else {
      if { (($Mod == "nodouble") && ([lsearch -exact $Lst $Item] == -1)) || ($Mod == "double") } {

         $Top.content delete 0 end
         lappend Lst $Item

         #----- Tri des valeurs, si specifie dans l'etat

         if { $Typ == "sorted" } {
            set Lst [lsort $Lst]
         }

         #----- Reinsertion des items de la liste de depart en mode "nodouble"

         set size 0
         foreach item $Lst {
            set w [string length $item]
            set size [expr $w>$size?$w:$size]
            $Top.content insert end $item
         }
         $Top.content configure -width [expr $size+2]

      } else {
         return -1
      }
      return [lsearch -exact $Lst $Item]
  }
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::AddList>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ajoute une liste d'item dans la liste du combobox.
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou inserer
#   <List>   : Liste d'item a inserer dans la liste du ComboBox
#
# Retour   :
#  <index> : Index ou l'item a ete inserer dans la liste (-1 si pas inserer)
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::AddList { W List } {

   foreach item "$List" {
      ComboBox::Add $W $item
   }
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Close>
# Creation : Mars 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Fermer la liste du ComboBox.
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou supprimer
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Close { W } {
   variable Resources
   variable Data

   upvar #0 ComboBox::${W}top Top

   if { [winfo ismapped $Top] } {
      $W.box invoke
      grab release $Top
      foreach grab $Data(Grab) {
         grab $grab
      }
   }
}

#------------------------------------------------------------------------------
# Nom      : <ComboBox::Count>
# Creation : Fevrier 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Retourne le compte du nombre d'item dans la liste.
#
# Parametres :
#   <W>      : Nom du widget ComboBox.
#
# Retour   :
#  <count> : Nombre d'elements.
#
# Remarques : C'est presque comme la commande "... index end" sauf que dans le cas
#             de 0 elle retourne "none"
#
#-------------------------------------------------------------------------------

proc ComboBox::Count { W } {

   upvar #0 ComboBox::${W}lst Lst

   return [llength $Lst]
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Disable>
# Creation : Fevrier 2007 - J.P. Gauthier - CMC/CMOE -
#
# But      : Desactiver un  combobox.
#
# Parametres :
#   <W>      : Nom du widget ComboBox
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Disable { W } {

   $W.select configure -state disabled
   $W.box configure -state disabled
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Enable>
# Creation : Fevrier 2007 - J.P. Gauthier - CMC/CMOE -
#
# But      : Activer un  combobox.
#
# Parametres :
#   <W>      : Nom du widget ComboBox
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Enable { W } {

   upvar #0 ComboBox::${W}edit Edit

   if { $Edit!="noedit" } {
      $W.select configure -state normal
   }
   $W.box configure -state normal
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Create>
# Creation : Fevrier 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Creer une combobox.
#
# Parametres :
#   <W>      : Nom du widget ComboBox
#   <Var>    : Variable a assigner quand une selection dans la liste est faite
#   <Edit>   : Entry editable ou non (edit, editclose ou noedit)
#   <Type>   : Type de liste (sorted ou unsorted)
#   <Mode>   : Doublon accepte ou non (double ou nodouble)
#   <Max>    : Nombre maximum d'item dans la liste (-1, pas de maximum)
#   <List>   : Liste initiale de valeur dans la liste
#   <Width>  : Largeur en nombre de caractere
#   <Height> : Hauteur de la liste
#   <args>   : Postcommande a effectue apres selection dans la liste
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Create { W IVar Edit Type Mode Max List Width Height args } {
   variable ${W}var
   variable ${W}edit
   variable ${W}cmd
   variable ${W}typ
   variable ${W}mod
   variable ${W}max
   variable ${W}lst
   variable ${W}top
   variable Resources

   upvar #0 $IVar Var

   #----- Etablissement des variables d'etats du ComboBox

   set ${W}var  $IVar         ; #Variable assignee
   set ${W}typ  $Type         ; #Type de liste
   set ${W}mod  $Mode         ; #Mode de la liste
   set ${W}max  $Max          ; #Maximum d'item dans la liste
   set ${W}edit $Edit         ; #Mode editable
   set ${W}cmd "$args"        ; #Postcommande affectee a chaque item
   set ${W}lst  {}            ; #Liste des valeur du listbox

   #----- Creation du ComboBox

   frame $W -relief sunken -bd 1
      entry $W.select -textvariable $IVar -relief flat -bd $Resources(Border) -width $Width \
         -bg $Resources(Background) -disabledbackground $Resources(Background) -disabledforeground black
      checkbutton $W.box -bitmap $Resources(Down) -relief raised -indicatoron false -bd $Resources(Border) \
         -selectcolor "" -variable ${W}on -command "ComboBox::Invoke $W"
      pack $W.select -side left -fill both -expand true
      pack $W.box -side left -fill y

   #----- Creation de la boite

   regsub -all "\\." $W "" top
   set ${W}top .$top

   if { ![winfo exists .$top] } {
      toplevel .$top
      wm withdraw .$top
      wm overrideredirect .$top 1

      listbox .$top.content -height $Height -width 0 -bg $Resources(Background) -selectmode single \
         -bd $Resources(Border) -relief sunken -yscrollcommand ".$top.scroll.bar set" \
         -exportselection false -cursor hand1
      pack .$top.content -side left -fill both -expand true

      frame .$top.scroll
         scrollbar .$top.scroll.bar -command ".$top.content yview" -cursor left_ptr -width 12 \
            -bd $Resources(Border)
         label .$top.scroll.box -bitmap $Resources(Scale) -highlightthickness 1 \
            -cursor double_arrow -bd $Resources(Border) -relief raised -bd 1
         pack .$top.scroll.bar -side top -fill y -expand true
         pack .$top.scroll.box -side top -fill x

      place .$top.scroll -rely 0.0 -relx 1.0 -relheight 1.0 -anchor ne
   }

   bind .$top.scroll.box <B1-Motion>       "ComboBox::Resize $W %Y"
   bind .$top.scroll.box <ButtonPress-1>   ".$top.scroll.box configure -relief sunken"
   bind .$top.scroll.box <ButtonRelease-1> ".$top.scroll.box configure -relief raised"

   #----- Evenements de controle des actions

   #----- Activation du Combo avec un click de souris
   # bind  $W.select  <B1-ButtonRelease> "if { !\[winfo ismapped .$top\] } { $W.box invoke }"

   #----- Si on tappe dans l'entree, rechercher la chaine dans la liste
   bind $W.select <Any-KeyRelease> "if { \"%K\"!=\"Return\" } { if { \"$Edit\"==\"edit\" } { ComboBox::Open $W; ComboBox::Select $W False } }"

   #----- Apres Enter dans l'entree, prendre ce qui est selectionne dans la liste
   bind $W.select <Key-Up>   "ComboBox::Open $W; ComboBox::SelectNext $W -1"
   bind $W.select <Key-Down> "ComboBox::Open $W; ComboBox::SelectNext $W  1"
   bind $W.select <Return>   "ComboBox::SetExec $W"

   #----- Si la fenetre est deplacee, deplacer aussi le menu
   #bind [winfo toplevel $W.box] <Configure> "+if { \[winfo ismapped .$top\] } { ComboBox::Place $W }"

   bind $W <Destroy> "ComboBox::Destroy $W"

   #----- Selection dans la liste
   bind .$top.content <B1-ButtonRelease> "ComboBox::Exec $W %y"
   bind .$top.content <B1-Motion> "ComboBox::Set $W %y"

   #----- Si en mode non editable

   if { $Edit == "noedit" } {
      $W.select config -state disabled -cursor left_ptr
   }

   #----- Insertion des items de la liste de depart

   ComboBox::AddList $W $List
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Del>
# Creation : Fevrier 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Supprime un item de la liste du combobox.
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou supprimer
#   <Item>   : Item a supprimer dans la liste du ComboBox
#
# Retour   :
#  <index> : Index de l'item supprime (-1 si pas trouve)
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Del { W Item } {

   upvar #0 ComboBox::${W}lst Lst
   upvar #0 ComboBox::${W}top Top

   #----- Recherche et suppression de l'item selectionne

   set no_item [lsearch -exact $Lst $Item]

   if { $no_item != -1} {
      set Lst [lreplace $Lst $no_item $no_item]
      $Top.content delete $no_item
   }

   #----- Si non trouve on retourne -1

   return $no_item
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::DelAll>
# Creation : Fevrier 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Supprime tout les items de la liste du combobox.
#
# Parametres  :
#   <W>       : Nom du widget ComboBox ou supprimer
#   <Current> : Effacer la vaeur courante de la variable
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::DelAll { W { Current True } } {

   upvar #0 ComboBox::${W}lst Lst
   upvar #0 ComboBox::${W}var Var
   upvar #0 ComboBox::${W}top Top

   #----- Supprime la liste complete

   set Lst ""

   if { $Current } {
      set $Var ""
   }
   $Top.content delete 0 end
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Destroy>
# Creation : Mai 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Detruit le combobox et desassigne toute les variables.
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou inserer
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Destroy { W } {

   upvar #0 ComboBox::${W}top Top

   catch { bind [winfo toplevel $W.box] <Configure> "" }

   destroy $W
   destroy $Top
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Exec>
# Creation : Fevrier 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Execute l'action du ComboBox apres la selection (Postcommande).
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou inserer
#   <Pos>    : Position de l'item selectionne
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Exec { W Pos } {

   upvar #0 ComboBox::${W}var Var
   upvar #0 ComboBox::${W}cmd Cmd
   upvar #0 ComboBox::${W}top Top

   uplevel #0 set $Var \{[$Top.content get [$Top.content nearest $Pos]]\}

   ComboBox::Close $W

   if { $Cmd != "" } {
      uplevel #0 eval $Cmd
   }
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Index>
# Creation : Fevrier 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Retourne l'index d'un item dans la liste du combobox.
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou supprimer
#   <Search> : Mode de recherche (Tel que dans lsearch)
#   <Item>   : Item a rechercher dans la liste du ComboBox
#
# Retour   :
#  <index> : Index de l'item demande (-1 si pas trouve)
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Index { W Search Item } {

   upvar #0 ComboBox::${W}lst Lst

   #----- Recherche de l'item selectionne, Si non trouve on retourne -1

   return [lsearch -$Search $Lst $Item]
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Invoke>
# Creation : Decembre 1999 - J.P. Gauthier - CMC/CMOE -
#
# But      : Effectue l'invokation du ComboBox.
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou supprimer
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Invoke { W } {
   variable Resources

   upvar #0 ComboBox::${W}top Top

   if { [winfo ismapped $Top] } {
      wm withdraw $Top
      $W.box config -bitmap $Resources(Down)
   } else {
      ComboBox::Place $W
      $W.box config -bitmap $Resources(Up)
   }

   ComboBox::Select $W
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::List>
# Creation : Mars 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Retourne la liste de tout ses elements.
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou supprimer
#
# Retour   :
#  <Lst>   : Liste de tous les elements
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::List { W } {

   upvar #0 ComboBox::${W}lst Lst

   return $Lst
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Open>
# Creation : Mars 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ouvrir la liste du ComboBox.
#
# Parametres :
#   <W>      : Nom du widget ComboBox
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Open { W } {
   variable Resources

   upvar #0 ComboBox::${W}top Top

   if { ![winfo ismapped $Top] } {
      $W.box invoke
   }
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Place>
# Creation : Mars 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Effectue le positionnement de la liste deroulante en verifiant
#            les limites de l'ecran pour garder la liste completement visible.
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou supprimer
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Place { W } {
   variable Data

   upvar #0 ComboBox::${W}top Top

   set yloc [expr [winfo rooty $W] + [winfo height $W]]
   set xloc [winfo rootx $W]

   set screeny [winfo screenheight $W]
   set screenx [winfo screenwidth  $W]

   #----- Verification de la hauteur

   set lh [winfo height $Top]
   set lh [expr $lh<10?[winfo reqheight $Top.content]:$lh]
   set lh [expr $lh>$screeny?$screeny:$lh]

   #----- Verification de la largeur

   set bw [winfo reqwidth $Top.content]
   set ew [winfo width $W]
   set lw [expr $bw>$ew?$bw:$ew]
   set lw [expr $lw>$screenx?$screenx:$lw]

   #----- Recuperer les limites

   set right  [expr $xloc+$lw]
   set bottom [expr $yloc+$lh]

   #----- Verification du bas de l'ecran

   if { $bottom > $screeny } {
      set yloc [expr $yloc-($bottom-$screeny)]
   }

   #----- Verification du cote droit de l'ecran

   if { $right > $screenx } {
      set xloc [expr $xloc-($right-$screenx)]
   }

   #----- Verification du cote gauche de l'ecran

   if { $xloc < 0 } {
      set xloc 0
   }

   #----- Positionnement de la liste deroulante

   $Top configure -width $lw -height $lh
   wm geometry $Top ${lw}x${lh}+$xloc+$yloc
   wm deiconify $Top
   raise $Top

   bind $Top <ButtonPress-1> "if { (%x<0 || %y<0 || %x>$lw || %y>$lh) } { ComboBox::Close $W }"

   set Data(Grab) [grab current]
   grab -global $Top
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Resize>
# Creation : Decembre 1999 - J.P. Gauthier - CMC/CMOE -
#
# But      : Effectue le redimensionnement du ComboBox.
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou supprimer
#   <Y>      : Coordonnee en y de la souris
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Resize { W Y } {

   upvar #0 ComboBox::${W}top Top

   #----- Calculer la dimension de la police

   set divisor [font metrics [lindex [$Top.content configure -font] 4] -linespace]
   set h       [expr $Y-[winfo rooty $Top]]
   set w       [winfo width $W]

   if { [expr $h/($divisor+3)] > 2 } {

      $Top configure -height $h
      wm geometry $Top [winfo width $Top]x$h

      bind $Top <ButtonPress-1> "if { (%x<0 || %y<0 || %x>$w || %y>$h) } { ComboBox::Close $W }"
   }
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Select>
# Creation : Mars 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Effectue la selection.
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou supprimer
#   <Set>    : Selectionner l'item, ou seulement se repositionner dessus
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Select { W { Set True } } {

   upvar #0 ComboBox::${W}var Var
   upvar #0 ComboBox::${W}top Top

   #----- Extraire la valeur affichee

   set index [ComboBox::Index $W glob *[$W.select get]*]

   #----- Selectionner la valeur

   $Top.content selection clear 0 end
   if { $Set } {
      $Top.content selection set $index
   }
   $Top.content yview $index
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::SelectNext>
# Creation : Mars 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Effectue la selection de l'item suivant.
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou supprimer
#   <Incr>   : Direction de l'increment
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::SelectNext { W Incr } {

   upvar #0 ComboBox::${W}var Var
   upvar #0 ComboBox::${W}top Top

   #----- Extraire la valeur affichee

   set index [$Top.content curselection]
   if { $index=="" } {
      set index 0
   }

   incr index $Incr

   set index [expr $index<0?0:($index>[$Top.content index end]-1?[$Top.content index end]-1:$index)]

   #----- Selectionner la valeur

   $Top.content selection clear 0 end
   $Top.content selection set $index
   $Top.content yview $index

   uplevel #0 set $Var \{[$Top.content get $index]\}
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::Set>
# Creation : Mai 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Effectue la selection en continue (Scrolling).
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou supprimer
#   <Pos>    : Coordonnee y du curseur
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::Set { W Pos } {

   upvar #0 ComboBox::${W}var Var
   upvar #0 ComboBox::${W}top Top

   set index [$Top.content nearest $Pos]

   $Top.content selection clear 0 end
   $Top.content selection set $index

   uplevel #0 set $Var \{[$Top.content get $index]\}
}

#-------------------------------------------------------------------------------
# Nom      : <ComboBox::SetExec>
# Creation : Mai 1998 - J.P. Gauthier - CMC/CMOE -
#
# But      : Effectue la selection et l'execution de la commande lors de
#            la selection a partir d'une chaine partielle.
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou supprimer
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc ComboBox::SetExec { W } {

   upvar #0 ComboBox::${W}var Var
   upvar #0 ComboBox::${W}cmd Cmd
   upvar #0 ComboBox::${W}top Top

   set index [$Top.content curselection]

   if { $index != "" } {
      uplevel #0 set $Var \{[$Top.content get $index]\}
   }

   ComboBox::Close $W

   if { $Cmd != "" } {
      uplevel #0 eval $Cmd
   }
}
