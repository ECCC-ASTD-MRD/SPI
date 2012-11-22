#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : OneTimeBubble.tk
# Creation : Juillet 2010 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Permet d'afficher une bulle de description.
#
# Fonctions:
#
#   CanvasBubble::Create     { Canvas Tag Text }
#   CanvasBubble::Destroy    { Canvas args }
#   CanvasBubble::Show       { Canvas Tag X Y }
#   CanvasBubble::Hide       { }
#
# Remarques :
#    -Concu a partir de namespace donc utilisable seulement en TCL 8.0 et +
#    -La methode de recuperation de l'information sur un item de Canvas peut
#     se faire de deux maniere, par une commande prenant en argument l'index et
#     retournant l'information ou par une liste d'information specifique.
#
#===============================================================================

package provide OneTimeBubble 1.0

catch { SPI::Splash "Loading Widget Package OneTimeBubble 1.0" }

namespace eval OneTimeBubble {
   variable Data
   variable Resources

   #----- Definitions des differentes resources du widget

   set Resources(Background)   white       ;#Couleur de fond de la bulle
   set Resources(Foreground)   black       ;#Couleur du texte
   set Resources(Relief)       raised      ;#Relief de la bulle
   set Resources(Font)         "-*-*-*-r-*-*-12-*-*-*-*-*-iso8859-1" ;#Font a utiliser dans les bulles
   set Resources(Border)       1           ;#Bordure de la bulle

   #----- Definitions des variables de donnees du widget

   set Data(Ids)          {}          ;#Liste des Ids valides.
   set Data(Showing)      -1          ;#Id of the bubble being shown.
}


#----------------------------------------------------------------------------
# Nom      : <CanvasBubble::Create>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer des bulles d'information pour les items d'un Canvas.
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#   <Tag>    : Nom du Tag
#   <Text>   : Texte d'information
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc OneTimeBubble::Create { } {
   variable Data
   variable Resources

   #----- Trouve un id valide

   set id 0
   foreach id $Data(Ids) {
      if { $id != $Data(Ids) } {
         break
      }
      incr id
   }

   #----- Ajoute l'id a la liste des ids valides tout en conservant le sort.

   set Data(Ids) [linsert $Data(Ids) $id $id]
   set Data(Text$id) ""

   #----- Creer la bulle d'aide

   if { [winfo exists .onetimebubble] != 1 } {
      toplevel .onetimebubble -background ""

      wm overrideredirect .onetimebubble true
      wm state            .onetimebubble withdrawn

      label .onetimebubble.hlp -anchor w -justify left -bg $Resources(Background) -fg $Resources(Foreground)\
         -bd $Resources(Border) -relief $Resources(Relief) -font $Resources(Font)
      pack .onetimebubble.hlp -fill both -expand true
   }

   return $id
}

#----------------------------------------------------------------------------
# Nom      : <CanvasBubble::Show>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Permet a la bulle d'information de suivre le curseur.
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#   <Tag>    : Nom du tag
#   <X>      : Coordonnee x sur l'ecran
#   <Y>      : Coordonnee y sur l'ecran
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc OneTimeBubble::Show { Id X Y {Text ""} } {
   variable Data
   variable Resources

   #----- S'assure que le Id est valide
   if { $Id=="" || [lsearch -sorted -integer $Data(Ids) $Id]==-1 } {
      return
   }

   #----- Regarde si on doit updater le texte ou si on utilise celui qui est deja en memoire.
   if { $Text != "" } {
      set Data(Text$Id) $Text
   }

   set Data(Showing) $Id
   .onetimebubble.hlp configure -text $Data(Text$Id)

   wm geometry .onetimebubble +[expr $X+10]+[expr $Y+10]
   wm state .onetimebubble normal
   raise .onetimebubble
}

proc OneTimeBubble::Hide { Id } {
   variable Data

   #----- S'assure que le Id est valide
   if { $Id=="" || [lsearch -sorted -integer $Data(Ids) $Id]==-1 } {
      return
   }

   #----- Si c'est le Id qui est affiche en ce moment dans la bulle
   if { $Data(Showing) == $Id } {
      set Data(Showing) -1
      wm state .onetimebubble withdrawn
   }
}

proc OneTimeBubble::Destroy { Id } {
   variable Data

   #----- S'assure que le Id est valide
   if { $Id=="" || [set idx [lsearch -sorted -integer $Data(Ids) $Id]]==-1 } {
      return
   }

   #----- Si la bulle affiche les infos de ce id, cache la bulle.
   if { $Data(Showing) == $Id } {
      set Data(Showing) -1
      wm state .onetimebubble withdrawn
   }

   #----- Supprime le id de la liste des ids valides
   set Data(Ids) [lreplace $Data(Ids) $idx $idx]
   set Data(Text$Id) ""
}
