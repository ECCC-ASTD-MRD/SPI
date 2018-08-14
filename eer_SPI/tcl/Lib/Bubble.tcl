#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : Bubble.tk
# Creation : Mai 1998 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Permet d'afficher des bulles d'aides relatives a des widget du
#    style windows.
#
# Fonctions:
#
#    Bubble::Activate { { State -1 } }
#    Bubble::Create   { Widget Hlp }
#    Bubble::Enter    { Widget X Y }
#    Bubble::Follow   { X Y }
#    Bubble::Leave    { Widget }
#    Bubble::Show     { Widget X Y }
#
# Remarques :
#    -Concu a partir de namespace donc utilisable seulement en TCL 8.0 et +
#
#===============================================================================

namespace eval Grab {
   variable Data

   set Data(Active) 0
   set Data(Dest)   {}

   if { [winfo exists .grab] != 1 } {
      toplevel .grab -background ""

      wm overrideredirect .grab true
      wm state            .grab withdrawn

      label .grab.it -anchor w -justify left
      pack .grab.it
   }
   event add <<Grab>> <B1-Motion>
}

proc Grab::BindOut { Widget Type } {
   bind $Widget <<Grab>> "Grab::Move %W $Type %X %Y"
}

proc Grab::BindIn { Widget } {

   bind $Widget <Enter> { set Grab::Data(Dest) %W }
   bind $Widget <Leave> { set Grab::Data(Dest) {} }
}

proc Grab::Move { Widget Type X Y } {
   variable Data

   if { !$Data(Active) } {

      switch $Type {

         listbox { #----- Listbox case

            set idxs [$Widget curselection]
            foreach idx $idxs {
               lappend txt [$Widget get $idx]
            }
            .grab.it configure -text [join $txt \n]
         }
      }
      set Data(Active) True
      set Data(Dest)   {}

      wm state .grab normal
      event add <<UnGrab>> <ButtonRelease-1>
      bind . <<UnGrab>> "Grab::Put $Widget $Type %X %Y"
   }

   wm geometry .grab +[expr $X+5]+[expr $Y+5]
}

proc Grab::Put { Widget Type X Y } {
   variable Data

   set Data(Active) False
   wm state .grab withdrawn
   event delete <<UnGrab>>
}


package provide Bubble 2.2

catch { SPI::Splash "Loading Widget Package Bubble 2.2" }

namespace eval Bubble {
   variable Resources
   variable Param
   variable Data

   #----- Definitions des differentes resources du widget

   set Resources(Background) white       ;#Couleur de fond de la bulle
   set Resources(Foreground) black       ;#Couleur du texte
   set Resources(Relief)     raised      ;#Relief de la bulle
   set Resources(Border)     1           ;#Bordure de la bulle
   set Resources(Font)       "-*-*-*-r-*-*-12-*-*-*-*-*-iso8859-1" ;#Font a utiliser dans les bulles

   #----- Definitions des variables de traitements du widget

   set Param(State)      False       ;#Etat des bulles (Activation ou Desactivation)
   set Param(Delay)      500         ;#Delai d'affichage de la bulle (en Millisecondes)
   set Param(StateList)  ""          ;#Liste des etats des bulles de chaque widget
   set Param(WidgetList) ""          ;#Liste des widgets ayant une bulle

   #----- Definitions des variables de donnees du widget

   set Data(HlpList)    ""          ;#Liste des textes d'aides
}

#-------------------------------------------------------------------------------
# Nom      : <Bubble::Activate>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE -
#
# But      : Activer et desactiver les bulles d'aides.
#
# Parametres :
#   <State>  : Etat d'activation des bulles
#
# Remarques :
#    -La desactivation s'effectue simplement en detruisant les "bindings" associes
#     aux widgets. Les structures et variables restent en memoire.
#    -L'inverse pour l'activation
#
#-------------------------------------------------------------------------------

proc Bubble::Activate { { State -1 } } {
   variable Param

   if { $State!=-1 } {
      set Param(State) $State
   }

   if { $Param(State) } {

      #----- Mode Actif

      foreach widget $Param(WidgetList) {

         set w [lindex $widget 0]

         if { [winfo exists $w] } {

            bind $w <Enter>   "Bubble::Enter  \"$widget\" %X %Y"
            bind $w <Motion>  "Bubble::Follow \"$widget\" %X %Y"
            bind $w <Leave>   "Bubble::Leave  \"$widget\""
            bind $w <Destroy> "Bubble::Leave  \"$widget\""

            $w config -cursor question_arrow
         }
      }
   } else {

      #----- Mode Desactiver

      foreach widget $Param(WidgetList) {

         set w [lindex $widget 0]

         if { [winfo exists $w] } {

            bind $w <Enter>  ""
            bind $w <Motion> ""
            bind $w <Leave>  ""

            $w config -cursor left_ptr
         }
      }
      wm state .bubble withdrawn
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Bubble::Create>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE -
#
# But     : Creer la bulle d'aide.
#
# Parametres  :
#    <Widget> : Path du widget concerne
#    <Hlp>    : Text de la bulle
#   <WrapLen>: Longeure de la bulle
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Bubble::Create { Widget Hlp { WrapLen 0 } } {
   variable Param
   variable Data
   variable Resources

   #----- Creer la bulle d'aide
 
   if { [winfo exists .bubble] != 1 } {
      toplevel .bubble -background ""

      wm overrideredirect .bubble true
      wm state            .bubble withdrawn
      wm attributes       .bubble -type tooltip -alpha 0.85

      label .bubble.hlp -anchor w -justify left -bg $Resources(Background) -fg $Resources(Foreground)\
         -bd $Resources(Border) -relief $Resources(Relief) -font $Resources(Font) -wraplength $WrapLen
      pack .bubble.hlp -fill both -expand true
   }
   
   if {($WrapLen > 0) && [winfo exists .bubble.hlp]} { 
      .bubble.hlp configure -wraplength $WrapLen
   }

   #----- Verifier si le widget en question est deja dans la liste
   #      Si non, on insere ses informations dans les listes
   #      Si oui, on update simplement la valeur du texte d'aide


   set idx [lsearch -exact $Param(WidgetList) $Widget]

   if { $idx == -1 } {
      lappend Param(WidgetList) $Widget
      lappend Data(HlpList)    $Hlp
      lappend Param(StateList)  0
   } else {
      set Data(HlpList) [lreplace $Data(HlpList) $idx $idx $Hlp]
   }

   #----- Activer les evenements d'appel de la bulle
   Bubble::Activate
}

#-------------------------------------------------------------------------------
# Nom      : <Bubble::Enter>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE -
#
# But      : Lance l'evenement d'affichage de la bulle si le curseur est sur
#            le widget.
#
# Parametres  :
#    <Widget> : Path du widget concerne
#    <X>      : Coordonnee X du curseur
#    <Y>      : Coordonnee Y du curseur
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Bubble::Enter { Widget X Y { Delay True } } {
   variable Param

   if { [llength $Widget]>1 } {
      set Widget " [lindex $Widget 0] [[lindex $Widget 0] index active] "
   }
   set idx [lsearch -exact $Param(WidgetList) $Widget]
   set Param(StateList) [lreplace $Param(StateList) $idx $idx 0]

   if { $Delay } {
      after $Param(Delay) [list Bubble::Show $Widget $X $Y]
   } else {
      Bubble::Show $Widget $X $Y
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Bubble::Leave>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE -
#
# But      : Effectue l'action de supprimer la bulle lorsque le
#            curseur quitte le widget
#
# Parametres  :
#    <Widget> : Path du widget concerne
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Bubble::Leave { Widget } {
   variable Param

   if { [llength $Widget]>1 } {
      set Widget " [lindex $Widget 0] [[lindex $Widget 0] index active] "
   }
   set idx [lsearch -exact $Param(WidgetList) $Widget]
   set Param(StateList) [lreplace $Param(StateList) $idx $idx 1]

   wm state .bubble withdrawn
}

#-------------------------------------------------------------------------------
# Nom      : <Bubble::Follow>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE -
#
# But     : Fait suivre la bulle au curseur.
#
# Parametres :
#    <X>     : Coordonnee x ou afficher la bulle
#    <Y>     : Coordonnee y ou afficher la bulle
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Bubble::Follow { Widget X Y } {

   if { [winfo viewable .bubble] } {
      set X [expr ($X+10+[winfo width .bubble])>[winfo screenwidth .bubble]?$X-[winfo width .bubble]-20:$X]
      set Y [expr ($Y+20+[winfo height .bubble])>[winfo screenheight .bubble]?$Y-[winfo height .bubble]-20:$Y]
      wm geometry .bubble +[expr $X + 10]+[expr $Y + 20]
   }

   if { [llength $Widget]>1 } {
      Bubble::Enter $Widget $X $Y False
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Bubble::Show>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE -
#
# But     : Afficher la bulle d'aide.
#
# Parametres  :
#    <Widget> : Path du widget concerne
#    <X>      : Coordonnee x ou afficher la bulle
#    <Y>      : Coordonnee y ou afficher la bulle
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Bubble::Show { Widget X Y } {
   global GDefs
   variable Param
   variable Data
   variable Resources

   set idx   [lsearch -exact $Param(WidgetList) $Widget]
   set hlp   [lindex [lindex $Data(HlpList) $idx] $GDefs(Lang)]
   set state [lindex $Param(StateList) $idx]

   #----- Si le curseur est toujours sur le widget concerne, afficher

   if { $state == 0 } {
      .bubble.hlp configure -text $hlp
      update idletasks
      set X [expr ($X+10+[winfo width .bubble])>[winfo screenwidth .bubble]?$X-[winfo width .bubble]-20:$X]
      set Y [expr ($Y+20+[winfo height .bubble])>[winfo screenheight .bubble]?$Y-[winfo height .bubble]-20:$Y]
      wm geometry .bubble +[expr $X+10]+[expr $Y+20]
      wm state .bubble normal
      raise .bubble
   }

   if { $idx==-1 } {
      wm state .bubble withdrawn
   }
}
