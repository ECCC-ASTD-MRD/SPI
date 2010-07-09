#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : FrameDefs.tk
# Creation : Fevrier 2000 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Permet de creer divers type de frames.
#
#       TabFrame  :  Frame de type tabulateur superposes
#
# Fonctions:
#
#    TabFrame::Add            { Tab Level Title Color }
#    TabFrame::Create         { Tab Level Command { Width -1 } { Height -1 } }
#    TabFrame::Clear          { Tab }
#    TabFrame::Current        { Tab }
#    TabFrame::Previous       { Tab }
#    TabFrame::Delete         { Tab Level No }
#    TabFrame::Destroy        { Tab }
#    TabFrame::Disable        { Tab No }
#    TabFrame::Edit           { Tab Level No }
#    TabFrame::Enable         { Tab No }
#    TabFrame::GetLabel       { Tab No }
#    TabFrame::GetLevel       { Tab No }
#    TabFrame::GetTabs        { Tab }
#    TabFrame::Is             { Tab }
#    TabFrame::NbFrame        { Tab }
#    TabFrame::Place          { Tab Level No Nb X Top }
#    TabFrame::PlaceHidder    { Tab No Top }
#    TabFrame::Select         { Tab No { Command True } }
#    TabFrame::SelectPrevious { Tab }
#
# Remarques :
#    -Concu a partir de namespace donc utilisable seulement en TCL 8.0 et +
#
#===============================================================================

package provide TabFrame 2.0

catch { SPI::Splash "Loading Widget Package TabFrame 2.0" }

namespace eval TabFrame {
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::Add>
# Creation : Fevrier 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ajout d'un onglet.
#
# Parametres     :
#   <Tab>        : Frame Parent
#   <Level>      : Niveau sur lequel positionner le tab
#   <Title>      : Titre du tabulateur
#   <Edit>       : Titre du tabulateur editable
#   <ColorFrame> : Couleur du tabulateur et de son frame
#   <ColorTab>   : Couleur du tabulateur
#
# Retour     :
#   <Path>   : Path complet du frame du tab
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::Add { Tab Level Title Edit { ColorFrame "" } { ColorTab "" } } {
   global GDefs
   variable Data

   #----- Verifier si une couleur a ete specifie

   eval set no \$Data(Nb$Tab)

   #----- Creer les diverses parties du widget

   entry $Tab.tab$no  -width [expr [string length $Title]+2] -relief raised -bd 1 -fg black \
      -disabledforeground black -disabledbackground $GDefs(ColorFrame)  -cursor left_ptr -justify center
   $Tab.tab$no insert end "$Title"
   $Tab.tab$no configure -state disabled

   frame $Tab.frame$no -relief raised -bd 1

   if { $ColorFrame!="" } {
      $Tab.tab$no configure    -bg $ColorFrame -disabledbackground $ColorFrame
      $Tab.frame$no configure  -bg $ColorFrame
   }

   if { $ColorTab!="" } {
      $Tab.tab$no configure    -bg $ColorTab -disabledbackground $ColorTab
   }

   set xloc 0
   foreach i $Data(W$Level$Tab) {
     incr xloc [winfo reqwidth $Tab.tab$i]
   }

   TabFrame::Place $Tab $Level $no $Data(Level$Tab) $xloc $Data(Top$Tab)

   bind $Tab.tab$no <ButtonPress-1>        "TabFrame::Select $Tab $no"
   bind $Tab.tab$no <Enter>                "$Tab.tab$no configure -fg $GDefs(ColorHighLight) "
   bind $Tab.tab$no <Leave>                "$Tab.tab$no configure -fg black"

   if { $Edit } {
      bind $Tab.tab$no <Double-ButtonPress-1> "$Tab.tab$no configure -state normal -relief sunken"
      bind $Tab.tab$no <Any-KeyRelease>       "TabFrame::Edit $Tab $Level $no"
      bind $Tab.tab$no <Return>               "$Tab.tab$no configure -state disabled -relief raised"
   }

   #----- Memoriser les informations de positionnement

   incr Data(Nb$Tab)
   lappend Data(W$Level$Tab) "$no"
   set Data(Current$Tab) $no

   return "$Tab.frame$no"
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::Edit>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Edition du libeller d'un onglet.
#
# Parametres :
#   <Tab>    : Frame Parent
#   <Level>  : Niveau sur lequel positionner le tab
#   <No>     : Titre du tabulateur
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::Edit { Tab Level No } {
   variable Data

   $Tab.tab$No  configure -width [expr [string length [$Tab.tab$No get]]+2]

   set xloc 0

   foreach no $Data(W$Level$Tab) {
      TabFrame::Place $Tab $Level $no $Data(Level$Tab) $xloc $Data(Top$Tab)
      incr xloc [winfo reqwidth $Tab.tab$no]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::Clear>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE -
#
# But      : Reinitialiser les onglets.
#
# Parametres :
#   <Tab>    : Frame Parent
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::Clear { Tab } {
   variable Data

   set Data(Nb$Tab) 0
   set Data(Current$Tab) -1
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::Create>
# Creation : Fevrier 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Creer un frame conteneur d'onglets.
#
# Parametres    :
#   <Tab>       : Identificateur du Tab maitre
#   <Level>     : Nombre de niveaux
#   <Command>   : "Callback" a appeler lors de la selection d'un onglet
#   <Width>     : Largeur (default -1)
#   <Height>    : Hauteur (default -1)
#
# Remarques :
#   La commande "callback" doit prendre en arguments le path et le numero
#   de l'onglet:
#
#        callback { Path No }
#
#-------------------------------------------------------------------------------

proc TabFrame::Create { Tab Level Command { Top 1 } { Width -1 } { Height -1 } } {
   variable Data

   set Data(Nb$Tab)       0        ;# Nombre de tab
   set Data(Level$Tab)    $Level   ;# Nombre de niveau du tab
   set Data(Command$Tab)  $Command ;# Callback de selection
   set Data(Current$Tab)  -1       ;# Onglet courant
   set Data(Previous$Tab) -1       ;#Onglet Precedent
   set Data(Top$Tab)      $Top     ;# Position des onglets

   for { set i 1 } { $i <= $Level } { incr i } {
      set Data(W$i$Tab)     "" ;# Liste des tab pour un niveau
   }

   frame $Tab -width $Width -height $Height
   frame $Tab.hidder

   bind $Tab <Configure> "update idletasks; TabFrame::PlaceHidder $Tab \$TabFrame::Data(Current$Tab) \$TabFrame::Data(Top$Tab)"
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::Is>
# Creation : Octobre 2007 - J.P. Gauthier - CMC/CMOE -
#
# But      : Verifier si un fenetre est un onglet.
#
# Parametres    :
#   <Tab>       : Identificateur du Tab
#
# Retour        :
#   <Is>        : True ou False
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::Is { Tab } {
   return [info exists ::TabFrame::Data(Nb$Tab)]
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::Current>
# Creation : Decembre 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Retourner le numero d 'onglet ayant le focus.
#
# Parametres    :
#   <Tab>       : Identificateur du Tab maitre
#
# Retour        :
#   <No>        : Numero de l'onglet
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::Current { Tab } {
   variable Data

   return $Data(Current$Tab)
}

proc TabFrame::Previous { Tab } {
   variable Data

   return $Data(Previous$Tab)
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::NbFrame>
# Creation : Decembre 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Retourner le nombre d 'onglet.
#
# Parametres    :
#   <Tab>       : Identificateur du Tab maitre
#
# Retour        :
#   <No>        : Nombre d'onglet
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::NbFrame { Tab } {
   variable Data

   return $Data(Nb$Tab)
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::Delete>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Suppression d'un onglet.
#
# Parametres :
#   <Tab>    : Frame Parent
#   <Level>  : Niveau sur lequel positionner le tab
#   <No>     : Titre du tabulateur
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::Delete { Tab Level { No -1 } } {
   variable Data

   #----- Onglet courant ???

   if { $No==-1 } {
      set No $Data(Current$Tab)
   }

   #----- Parcourir les onglets

   set xloc 0
   set Data(Current$Tab) -1

   foreach no $Data(W$Level$Tab) {

      if { $no!=$No } {
         TabFrame::Place $Tab $Level $no $Data(Level$Tab) $xloc $Data(Top$Tab)
         incr xloc [winfo reqwidth $Tab.tab$no]
         set Data(Current$Tab) $no
      } else {
         destroy $Tab.tab$no $Tab.frame$no
      }
   }

   #----- Supprimer de la liste des onglets

   set idx [lsearch -exact $Data(W$Level$Tab) $No]
   set Data(W$Level$Tab) [lreplace $Data(W$Level$Tab) $idx $idx]

   return "$Tab.frame$No"
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::Destroy>
# Creation : Fevrier 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Destruction du widget des onglets.
#
# Parametres     :
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::Destroy { Tab } {
   variable Data

   for { set i 1 } { $i <= $Data(Level$Tab) } { incr i } {
      unset Data(W$i$Tab)
   }
   unset Data(Nb$Tab)
   unset Data(Level$Tab)
   unset Data(Command$Tab)
   unset Data(Top$Tab)

   destroy $Tab
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::Disable>
# Creation : Fevrier 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Desactivation d'un onglets.
#
# Parametres   :
#   <TabNo>    : Frame du tab
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::Disable { Tab No } {
   global GDefs

   bind $Tab.tab$No <ButtonPress-1> ""
   bind $Tab.tab$No <Enter>         ""
   bind $Tab.tab$No <Leave>         ""

   $Tab.tab$No configure -fg $GDefs(ColorOff)  -disabledforeground $GDefs(ColorOff)
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::Enable>
# Creation : Fevrier 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Reactivation d'un onglets.
#
# Parametres     :
#   <Tab>    : Frame Parent
#   <No>     : Numero du tabulateur
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::Enable { Tab No } {
   global GDefs

   bind $Tab.tab$No <ButtonPress-1> "TabFrame::Select $Tab $No"
   bind $Tab.tab$No <Enter>         "$Tab.tab$No configure -fg $GDefs(ColorHighLight) "
   bind $Tab.tab$No <Leave>         "$Tab.tab$No configure -fg black"

   $Tab.tab$No configure -fg black -disabledforeground black
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::GetTabs>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recupere la liste des onglets
#
# Parametres :
#   <Tab>    : Frame Parent
#
# Retour     :
#   <List>   : Liste de onglets
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::GetTabs { Tab { Index -1 } } {
   variable Data

   set list {}
   for { set i 1 } { $i<=$Data(Level$Tab) } { incr i } {
      set list [concat $list $Data(W$i$Tab)]
   }

   if { $Index!=-1 } {
      return [lindex $list $Index]
   } else {
      return $list
   }
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::GetLabel>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recupere le titre d'un onglets
#
# Parametres :
#   <Tab>    : Frame Parent
#   <No>     : Numero de l'onglet
#
# Retour     :
#   <Label>  : Texte de l'onglet
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::GetLabel { Tab No } {

   return [$Tab.tab$No get]
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::GetLevel>
# Creation : Fevrier 2004 - J.P. Gauthier - CMC/CMOE -
#
# But      : Recupere le niveau d'un onglet
#
# Parametres :
#   <Tab>    : Frame Parent
#   <No>     : Numero de l'onglet
#
# Retour     :
#   <Level>  : Niveau de l'onglet
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::GetLevel { Tab No } {
   variable Data

   set i 0
   for { set i 1 } { $i<=$Data(Level$Tab) } { incr i } {
      if { [lsearch -integer $Data(W$i$Tab) $No]!=-1 } {
         break
      }
   }
   return $i
}
#-------------------------------------------------------------------------------
# Nom      : <TabFrame::Label>
# Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE -
#
# But      : Renomer un onglet
#
# Parametres :
#   <Tab>    : Frame Parent
#   <Level>  : Niveau sur lequel positionner le tab
#   <No>     : Titre du tabulateur
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::Label { Tab Level No Label } {
   variable Data

   $Tab.tab$No configure -text " $Label "

   #----- Parcourir les onglets

   set xloc 0

   foreach no $Data(W$Level$Tab) {

      TabFrame::Place $Tab $Level $no $Data(Level$Tab) $xloc $Data(Top$Tab)
      incr xloc [winfo reqwidth $Tab.tab$no]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::Place>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Positionner un onglet
#
# Parametres :
#   <Tab>    : Frame Parent
#   <Level>  : Niveau sur lequel positionner le tab
#   <No>     : Numero du tabulateur
#   <X>      : Position en X
#   <Top     : Position de l'onglet
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::Place { Tab Level No Nb X Top { Hid False } } {

   set y [expr [winfo reqheight $Tab.tab$No] -1]

   if { $X==-1 } {
      set X [expr [winfo x $Tab.tab$No]-[lindex [$Tab configure -bd] end]]
   }

   if { $Top } {
      place $Tab.tab$No -x $X -y [expr ($Level-1)* $y] -anchor nw
      place $Tab.frame$No -x 0 -y [expr $Level*$y] -relwidth 1.0 -relheight 1.0 -height -[expr $Level*$y]
   } else {
      place $Tab.tab$No -x $X -rely 1.0 -y -[expr ($Level-1)*$y] -anchor sw
      place $Tab.frame$No -x 0 -y 0 -relwidth 1.0 -relheight 1.0 -height -[expr $Level*$y]
   }

   update idletasks

   raise $Tab.tab$No
   raise $Tab.frame$No

   if { $Hid } {
      TabFrame::PlaceHidder $Tab $No $Top
   }
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::PlaceHidder>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE -
#
# But      : Positionner le "hidder" d'un onglet
#
# Parametres :
#   <Tab>    : Frame Parent
#   <No>     : Numero du tabulateur
#   <Top     : Position de l'onglet
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::PlaceHidder { Tab No Top } {

   if { $No==-1 } {
      return
   }

   if { $Top } {
      place $Tab.hidder \
         -x [expr [winfo x $Tab.tab$No] + 1] \
         -y [expr [winfo y $Tab.frame$No] -1] \
         -width [expr [winfo reqwidth $Tab.tab$No] -2] \
         -height [expr 1+1]
   } else {
     place $Tab.hidder \
         -x [expr [winfo x $Tab.tab$No] + 1] \
         -y [expr [winfo height $Tab.frame$No] - 1] \
         -width [expr [winfo reqwidth $Tab.tab$No] -2] \
         -height [expr 2*1-1]
   }

   $Tab.hidder configure -bg [lindex [$Tab.frame$No configure -bg] 4]
   raise $Tab.hidder
}

#-------------------------------------------------------------------------------
# Nom      : <TabFrame::Select>
# Creation : Fevrier 2000 - J.P. Gauthier - CMC/CMOE -
#
# But      : Selection d'un onglets.
#
# Parametres :
#   <Tab>    : Frame Parent
#   <No>     : Numero du tabulateur
#   <Command>: Evaluer la commande associee
#
# Retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc TabFrame::SelectPrevious { Tab } {
   variable Data

   TabFrame::Select $Tab $Data(Previous$Tab) False
}

proc TabFrame::Select { Tab No { Command True } } {
   variable Data

   if { ![winfo exists $Tab.frame$No] } {
      return
   }
   set Data(Previous$Tab) $Data(Current$Tab)

   #----- Deplacer les rangees d'onglets tabs selon l'ordre

   set y  [expr [winfo reqheight $Tab.tab[lindex $Data(W1$Tab) 0]]-1]
   set l 1
   set level [TabFrame::GetLevel $Tab $No]

   if { $Data(Level$Tab)>1 } {

      #----- Pour tout les niveaux non vide, deplacer les onglets

      for { set lvl 1 } { $lvl<=$Data(Level$Tab) } { incr lvl } {

        if { $lvl!=$level && [llength $Data(W$lvl$Tab)] } {
            foreach tab $Data(W$lvl$Tab) {
               TabFrame::Place $Tab $l $tab $Data(Level$Tab) -1 $Data(Top$Tab)
            }
            incr l
         }
      }

      #----- Abbaiser la rangees d'onglets selectionnee

      foreach tab $Data(W$level$Tab) {
         if { $tab!=$Tab } {
           TabFrame::Place $Tab $l $tab $Data(Level$Tab) -1 $Data(Top$Tab)
         }
      }
   }

   #----- Placer le tab selectionne

   TabFrame::Place $Tab $l $No $Data(Level$Tab) -1 $Data(Top$Tab) True

   #----- Effectuer le Callback de selection

   set Data(Current$Tab) $No

   if { $Command && $Data(Command$Tab)!="" } {
      eval eval \$Data(Command$Tab) $Tab $No
   }
}
