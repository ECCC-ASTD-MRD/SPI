#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : SelectBox.tcl
# Creation : Juin 1999 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Ce package permet de creer et de manipuler une "SelectBox"
#    (une liste de selection multiple afficher en superposition)
#
# Fonctions:
#
#    SelectBox::Create  { W Img IVar List Width Height args }
#    SelectBox::Clear   { W Update }
#    SelectBox::Destroy { W }
#    SelectBox::Exec    { W }
#    SelectBox::Get     { W }
#    SelectBox::Insert  { W List }
#
# Remarques :
#    -Concu a partir de namespace donc utilisable seulement en TCL 8.0 et +
#
#===============================================================================

package provide SelectBox 1.0

proc IdSelectBox { Show } {

   if { $Show } {
      puts "(INFO) Loading Standard CMC/CMOE Widget Package SelectBox Version 1.0"
   }
}

namespace eval SelectBox {
   global GDefs
   variable Resources
   variable Data

   #----- Definitions des differentes resources du widget

   catch { set Resources(BgCol)  $GDefs(ColorFrame) }
   catch { set Resources(SlCol)  $GDefs(ColorLight) }
   set Resources(Relief) sunken
   set Resources(Border) 1

   #----- Definitions des variables de donnees du widget

   set Data(Widgets)     ""
}

#-------------------------------------------------------------------------------
# Nom      : <SelectBox::Create>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE -
#
# But      : Creer une combobox.
#
# Parametres :
#   <W>      : Nom du widget SelectBoxx
#   <Label>  : Label
#   <IVar>   : Variable a assigner quand une selection dans la liste est faite
#   <List>   : Liste initiale de valeur dans la liste
#   <Width>  : Largeur de la liste
#   <Height> : Hauteur de la liste
#   <args>   : Postcommande a effectue apres selection dans la liste
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SelectBox::Create { W Label IVar List Width Height args } {
   global GDefs
   variable ${W}var
   variable ${W}cmd
   variable ${W}sel
   variable ${W}on
   variable Resources
   variable Data

   upvar #0 $IVar Var

   #----- Etablissement des variables d'etats du ComboBox

   set ${W}var $IVar         ; #Variable assignee
   set ${W}sel ""            ; #Selection dans la liste
   set ${W}cmd "$args"       ; #Postcommande affectee a chaque item
   set ${W}on  0             ; #Variable de selection

   lappend Data(Widgets) $W

   #----- Creation du boutton d'activation

   checkbutton $W -text $Label -relief raised -indicatoron false -bd $Resources(Border) -width $Width\
      -selectcolor $Resources(SlCol) -state disabled -anchor w\
      -variable SelectBox::${W}on -command "SelectBox::Exec $W"

   #----- Creation de la boite

   regsub -all "\\." $W "" top

   toplevel .$top
   wm withdraw .$top
   wm overrideredirect .$top 1

   listbox .$top.content -height $Height -width 0 -bg $Resources(BgCol) -yscrollcommand ".$top.scroll set"\
      -selectmode multiple -bd $Resources(Border) -relief $Resources(Relief) -setgrid 1 -exportselection false \
      -selectbackground $Resources(SlCol) -cursor left_ptr
   scrollbar .$top.scroll -bd $Resources(Border) -command ".$top.content yview" -width 10 -cursor left_ptr
   pack .$top.content .$top.scroll -side left -fill both

   #----- Si la fenetre est deplacee, deplacer aussi le menu

   bind [winfo toplevel $W] <Configure> "+if { \[winfo ismapped .$top\] }\
     { wm geometry .$top +\[winfo rootx $W\]+\[expr \[winfo rooty $W\] + \[winfo height $W\]\] }"

   bind .$top.content <ButtonRelease-3> "SelectBox::Exec $W"

   #----- Insertion des items de la liste de depart

   SelectBox::Insert $W $List
}

#-------------------------------------------------------------------------------
# Nom      : <SelectBox::Clear>
# Creation : Juillet 1999 - J.P. Gauthier - CMC/CMOE -
#
# But      : Supprime la selection du SelectBox.
#
# Parametres :
#   <W>      : Nom du widget SelectBox
#   <Update> : Effectuer la commande associee
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SelectBox::Clear { W Update } {

   upvar #0 SelectBox::${W}sel Sel
   upvar #0 SelectBox::${W}cmd Cmd
   upvar #0 SelectBox::${W}var Var

   regsub -all "\\." $W "" top

   .$top.content selection clear 0 end
   set Sel ""
   set $Var ""

   #----- Executer la commande de l'usager pour la selection

   if { $Update } {
      eval $Cmd [list $Var]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SelectBox::Destroy>
# Creation : Juillet 1999 - J.P. Gauthier - CMC/CMOE -
#
# But      : Detruit le selectbox et desassigne toute les variables.
#
# Parametres :
#   <W>      : Nom du widget SelectBox ou inserer
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SelectBox::Destroy { W } {
   variable ${W}var
   variable ${W}cmd
   variable ${W}sel
   variable ${W}on
   variable Data

   #----- Supprimer les variables relatives a cette boite

   unset ${W}var
   unset ${W}cmd
   unset ${W}sel
   unset ${W}on

   #----- Supprimer de la liste des boites existantes

   set idx [lsearch -exact $Data(Widgets) $W]
   set Data(Widgets) [lreplace $Data(Widgets) $idx $idx]

   bind [winfo toplevel $W] <Configure> ""

   #----- Supprimer les widgets

   regsub -all "\\." $W "" top

   destroy $W
   destroy .$top
}

#-------------------------------------------------------------------------------
# Nom      : <SelectBox::Exec>
# Creation : Juillet 1999 - J.P. Gauthier - CMC/CMOE -
#
# But      : Gere le comportement du SelectBox.
#
# Parametres :
#   <W>      : Nom du widget SelectBox
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SelectBox::Exec { W } {
   variable Data

   upvar #0 SelectBox::${W}sel Sel
   upvar #0 SelectBox::${W}cmd Cmd
   upvar #0 SelectBox::${W}var Var
   upvar #0 SelectBox::${W}on  On

   regsub -all "\\." $W "" top

   if { [winfo ismapped .$top] } {

      #----- Recuperer les informations selectionnees

      set Sel [.$top.content curselection]
      set $Var ""
      foreach sel $Sel {
         lappend $Var [.$top.content get $sel]
      }

      wm withdraw .$top
      set On 0

      #----- Executer la commande de l'usager pour la selection

      eval $Cmd [list $Var]
   } else {

      #----- Fermeture de tout les autres SelectBox

      foreach widget $Data(Widgets) {
         regsub -all "\\." $widget "" toptemp
         if { [winfo ismapped .$toptemp] } {
            SelectBox::Exec $widget
         }
      }

      #----- Ouverture du SelectBox actif

      wm geometry .$top +[winfo rootx $W]+[expr [winfo rooty $W] + [winfo height $W]]
      wm deiconify .$top
      raise .$top
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SelectBox::Get>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Retourner le contenu de la liste
#
# Parametres :
#   <W>      : Nom du widget SelectBox
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SelectBox::Get { W } {

   regsub -all "\\." $W "" top

   return [.$top.content get 0 end]
}

#-------------------------------------------------------------------------------
# Nom      : <SelectBox::Insert>
# Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE -
#
# But      : Insere les donees dans le SelectBox.
#
# Parametres :
#   <W>      : Nom du widget ComboBox ou inserer
#   <List>   : Liste d'item a inserer dans la liste du ComboBox
#
# Retour   :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SelectBox::Insert { W List } {

   regsub -all "\\." $W "" top

   .$top.content delete 0 end

   if { [llength $List] <= 1 } {
     $W configure -state disabled
   } else {
     eval .$top.content insert end [lsort -dictionary $List]
     $W configure -state normal
   }
}
