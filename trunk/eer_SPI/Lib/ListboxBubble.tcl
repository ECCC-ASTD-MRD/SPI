#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : ListboxBubble.tk
# Creation : Mai 1999 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Permet d'afficher des bulles d'aides relatives a des elements d'un listbox.
#
# Fonctions:
#
#   ListboxBubble::Activate   { }
#   ListboxBubble::Create     { Widget Mode args }
#   ListboxBubble::Destroy    { Widget } {
#   ListboxBubble::GetInfo    { Widget WidgetY }
#   ListboxBubble::SetCommand { Widget Command }
#   ListboxBubble::SetInfo    { Widget List }
#   ListboxBubble::SetMode    { Widget Mode }
#   ListboxBubble::Show       { Widget WidgetY X Y }
#
# Remarques :
#    -Concu a partir de namespace donc utilisable seulement en TCL 8.0 et +
#    -La methode de recuperation de l'information sur un item de listbox peut
#     se faire de deux maniere, par une commande prenant en argument l'index et
#     retournant l'information ou par une liste d'information specifique.
#
#===============================================================================

package provide ListboxBubble 1.2

catch { SPI::Splash "Loading Widget Package ListboxBubble 1.2" }

namespace eval ListboxBubble {
   variable Data
   variable Resources

   #----- Definitions des differentes resources du widget

   set Resources(Background)   white       ;#Couleur de fond de la bulle
   set Resources(Foreground)   black       ;#Couleur du texte
   set Resources(Relief)       raised      ;#Relief de la bulle
   set Resources(Font)         "-*-*-*-r-*-*-12-*-*-*-*-*-iso8859-1" ;#Font a utiliser dans les bulles
   set Resources(Border)       1           ;#Bordure de la bulle

   #----- Definitions des variables de donnees du widget

   set Data(InfoList)     ""          ;#Liste des liste de textes d'information
   set Data(CommandList)  ""          ;#Liste des commande pour obtenir le texte d'information
   set Data(ModeList)     ""          ;#Liste des modes de fonctionnement (0 commande, 1 list d'info)
   set Data(WidgetList)   ""          ;#Liste des widgets ayant une bulle
   set Data(State)        1           ;#Etat d'activation des bulles
}

#----------------------------------------------------------------------------
# Nom      : <ListboxBubble::Activate>
# Creation : Septembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Active les fonctions des bulles.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ListboxBubble::Activate { } {
   variable Data

   foreach widget $Data(WidgetList) {
      if { [winfo exists $widget] } {
         if { $Data(State) } {
            bind $widget <Motion> "ListboxBubble::Show $widget %y %X %Y"
            bind $widget <Leave>  "wm state .listbubble withdrawn"
         } else {
            bind $widget <Motion> ""
            bind $widget <Leave>  ""
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <ListboxBubble::Create>
# Creation : Mai 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer des bulles d'information pour les items d'un listbox.
#
# Parametres :
#   <Widget> : Nom du widget listbox
#   <Mode>   : Mode d'utilisation (0 commande, 1 liste d'information)
#
# Retour:
#
# Remarques :
#    -Si le mode est 0 (Commande) alors le package utilisera une commande passe dans
#     "args" pour obtenir l'information sur l'item, sinon il utilisera une liste
#     passe dans "args".
#    -Si le mode est 0 (Commande) alors la commande utilise doit prendre en argument
#     un entier qui est l'index de l'item du listbox sous le curseur de la souris.
#
#----------------------------------------------------------------------------

proc ListboxBubble::Create { Widget Mode args } {
   variable Data
   variable Resources

   set Data(Index) -1

   lappend Data(ModeList)   $Mode
   lappend Data(WidgetList) $Widget

   if { $Mode == 0 } {
      lappend Data(CommandList) "$args"
      lappend Data(InfoList)    ""
   } else {
      lappend Data(CommandList) ""
      lappend Data(InfoList)    "$args"
   }

   #----- Creer la bulle d'aide

   if { [winfo exists .listbubble] != 1 } {
      toplevel .listbubble -background ""

      wm overrideredirect .listbubble true
      wm state            .listbubble withdrawn

      label .listbubble.hlp -anchor w -justify left -bg $Resources(Background) -fg $Resources(Foreground)\
         -bd $Resources(Border) -relief $Resources(Relief) -font $Resources(Font)
      pack .listbubble.hlp -fill both -expand true
   }

   ListboxBubble::Activate
}

#----------------------------------------------------------------------------
# Nom      : <ListboxBubble::Destroy>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Detruit tout ce qui est relatif aux pacakge.
#
# Parametres :
#   <Widget> : Identificateur du widget
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ListboxBubble::Destroy { Widget } {
   variable Data

   set widx [lsearch -exact $Data(WidgetList) $Widget]

   set Data(WidgetList)  [lreplace $Data(WidgetList) $widx $widx]
   set Data(ModeList)    [lreplace $Data(ModeList) $widx $widx]
   set Data(CommandList) [lreplace $Data(CommandList) $widx $widx]
   set Data(InfoList)    [lreplace $Data(InfoList) $widx $widx]

   bind $Widget <Motion> ""
   bind $Widget <Leave>  ""

   if { [winfo exists .listbubble] } {
      wm state .listbubble withdrawn
   }
}

#----------------------------------------------------------------------------
# Nom      : <ListboxBubble::Show>
# Creation : Mai 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Permet a la bulle d'information de suivre le curseur.
#
# Parametres :
#   <Widget> : Nom du widget
#   <WidgetY>: Coordonnee y dans le widget
#   <X>      : Coordonnee x sur l'ecran
#   <Y>      : Coordonnee y sur l'ecran
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ListboxBubble::Show { Widget WidgetY X Y } {

   if { [ListboxBubble::GetInfo $Widget $WidgetY] } {
      wm geometry .listbubble +[expr $X+10]+[expr $Y+10]
      wm state .listbubble normal
      raise .listbubble
   } else {
      wm state .listbubble withdrawn
   }
}

#----------------------------------------------------------------------------
# Nom      : <ListboxBubble::GetInfo>
# Creation : Mai 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Recupere si necessaire l'information sur les experiences.
#
# Parametres :
#   <Widget> : Nom du widget
#   <WidgetY>: Coordonnee y dans le widget
#
# Retour:
#   <Bool>   : L'information a change ou non
#
# Remarques :
#
#----------------------------------------------------------------------------

proc ListboxBubble::GetInfo { Widget WidgetY } {
   variable Data
   variable Resources

   #----- Determiner les parametres des bulles pour le widget actif

   set widx [lsearch -exact $Data(WidgetList) $Widget]
   set mode [lindex $Data(ModeList) $widx]

   #----- Determiner si le curseur est bien positionne

   set index [$Widget nearest $WidgetY]
   set bbox  [$Widget bbox $index]

   #----- Y a-t-il des elements

   if { [llength $bbox] == 0 } {
      return false
   }

   if { $WidgetY > [lindex $bbox 1] && $WidgetY < [expr [lindex $bbox 1] + [lindex $bbox 3]] } {

      #----- Si le curseur a change d'item

      if { $Data(Index) != $index } {
         set Data(Index) $index

         #----- Obtenir l'information de l'item du listbox

         if { $mode } {
            set infoliste [lindex $Data(InfoList) $widx]
            set hlp [lindex $infoliste $Data(Index)]
         } else {
            set command [lindex $Data(CommandList) $widx]
            eval set hlp \[$command $Data(Index)\]
         }

         #----- Separer les diverses lignes de la bulle pour l'insertion

         .listbubble.hlp configure -text $hlp
      }
      return true
   } else {
      return false
   }
}

#----------------------------------------------------------------------------
# Nom      : <ListboxBubble::SetCommand>
# Creation : Mai 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise la commande des informations pour un widget particulier.
#
# Parametres :
#   <Widget> : Nom du widget listbox
#   <List>   : Liste des informations
#
# Retour:
#
# Remarques :
#   -Il est de la responsabilite de l'utilisateur de s'assurer que le mode
#    de fonctionnement correspond avec la methode de recuperation de l'information
#
#----------------------------------------------------------------------------

proc ListboxBubble::SetCommand { Widget Command } {
   variable Data

   set widx [lsearch -exact $Data(WidgetList) $Widget]

   set Data(CommandList) [lreplace $Data(CommandList) $widx $widx "$Command"]
   set Data(InfoList)    [lreplace $Data(InfoList) $widx $widx ""]
}

#----------------------------------------------------------------------------
# Nom      : <ListboxBubble::SetMode>
# Creation : Mai 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise le mode d'utilisation pour un widget particulier.
#
# Parametres :
#   <Widget> : Nom du widget listbox
#   <List>   : Liste des informations
#
# Retour:
#
# Remarques :
#   -Il est de la responsabilite de l'utilisateur de s'assurer que le mode
#    de fonctionnement correspond avec le commande ou la liste d'info
#
#----------------------------------------------------------------------------

proc ListboxBubble::SetMode { Widget Mode } {
   variable Data

   set widx [lsearch -exact $Data(WidgetList) $Widget]

   set Data(ModeList)  [lreplace $Data(ModeList) $widx $widx "$List"]
}

#----------------------------------------------------------------------------
# Nom      : <ListboxBubble::SetInfo>
# Creation : Mai 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise les informations pour un widget particulier.
#
# Parametres :
#   <Widget> : Nom du widget listbox
#   <List>   : Liste des informations
#
# Retour:
#
# Remarques :
#   -Il est de la responsabilite de l'utilisateur de s'assurer que le mode
#    de fonctionnement correspond avec la methode de recuperation de l'information
#
#----------------------------------------------------------------------------

proc ListboxBubble::SetInfo { Widget List } {
   variable Data

   set widx [lsearch -exact $Data(WidgetList) $Widget]

   set Data(InfoList)    [lreplace $Data(InfoList) $widx $widx "$List"]
   set Data(CommandList) [lreplace $Data(CommandList) $widx $widx ""]
}
