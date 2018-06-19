#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : CanvasBubble.tk
# Creation : Mai 1999 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Permet d'afficher des bulles d'aides relatives a des elements d'un Canvas.
#
# Fonctions:
#
#   CanvasBubble::Activate   { Canvas}
#   CanvasBubble::Create     { Canvas Tag Text }
#   CanvasBubble::Destroy    { Canvas args }
#   CanvasBubble::Show       { Canvas Tag X Y }
#
# Remarques :
#    -Concu a partir de namespace donc utilisable seulement en TCL 8.0 et +
#    -La methode de recuperation de l'information sur un item de Canvas peut
#     se faire de deux maniere, par une commande prenant en argument l'index et
#     retournant l'information ou par une liste d'information specifique.
#
#===============================================================================

package provide CanvasBubble 1.2

catch { SPI::Splash "Loading Widget PackageCanvasBubble 1.2" }

namespace eval CanvasBubble {
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
   set Data(TagList)      ""          ;#Liste des tags ayant une bulle
   set Data(TagCurrent)   ""          ;#Tag courant
   set Data(State)        1           ;#Etat d'activation des bulles

   #----- Definition des écrans physiques
   set Data(ScreenRes)  ""
   catch {set Data(ScreenRes) [regexp -inline -all {(\d+)x(\d+)\+(\d+)\+(\d+)} [exec xrandr --nograb]]}
}

#----------------------------------------------------------------------------
# Nom      : <CanvasBubble::Activate>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Active les fonctions des bulles.
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CanvasBubble::Activate { Canvas } {
   variable Data

   foreach tag $Data(TagList$Canvas) {
      if { $Data(State$Canvas) } {
         $Canvas bind $tag <Enter> "CanvasBubble::Show $Canvas $tag %X %Y"
         $Canvas bind $tag <Leave> "wm state .canvasbubble withdrawn"
      } else {
         $Canvas bind $tag <Enter> ""
         $Canvas bind $tag <Leave> ""
      }
   }
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
#   <WrapLen>: Longeure de la bulle
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc CanvasBubble::Create { Canvas Tag Text { WrapLen 0 } } {
   variable Data
   variable Resources

   set idx -1

   if { [info exists Data(TagList$Canvas)] } {
      set idx [lsearch -exact $Data(TagList$Canvas) $Tag]
   } else {
      set Data(State$Canvas) 1
   }

   if { $idx == -1 } {
      lappend Data(TagList$Canvas)   $Tag
      lappend Data(InfoList$Canvas)  $Text
   } else {
      set Data(InfoList$Canvas) [lreplace $Data(InfoList$Canvas) $idx $idx $Text]
   }

   #----- Creer la bulle d'aide

   if { [winfo exists .canvasbubble] != 1 } {
      toplevel .canvasbubble -background ""

      wm overrideredirect .canvasbubble true
      wm state            .canvasbubble withdrawn
      wm attributes       .canvasbubble -type tooltip -alpha 0.85

      label .canvasbubble.hlp -anchor w -justify left -bg $Resources(Background) -fg $Resources(Foreground)\
         -bd $Resources(Border) -relief $Resources(Relief) -font $Resources(Font) -wraplength $WrapLen
      pack .canvasbubble.hlp -fill both -expand true
   }

   CanvasBubble::Activate $Canvas
}

#----------------------------------------------------------------------------
# Nom      : <CanvasBubble::Destroy>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Detruit tout ce qui est relatif aux package.
#
# Parametres :
#   <Canvas> : Identificateur du Canvas
#   <args>   : Liste de tags
#
# Retour:
#
# Remarques :
#   -si args est vide , toutes les bulles du canvas specifie seront detruites
#    sinon sulement celle des tags specifie
#
#----------------------------------------------------------------------------

proc CanvasBubble::Destroy { Canvas args } {
   variable Data

   if { $args=="" } {
      set Data(TagList$Canvas)     ""
      set Data(InfoList$Canvas)    ""
   } else {
      foreach tag $args {
         set tidx [lsearch -exact $Data(TagList$Canvas) $tag]
         set Data(TagList$Canvas)  [lreplace $Data(TagList$Canvas) $tidx $tidx]
         set Data(InfoList$Canvas) [lreplace $Data(InfoList$Canvas) $tidx $tidx]

         $Canvas bind $tag <Motion> ""
         $Canvas bind $tag <Leave>  ""
      }
   }

   wm state .canvasbubble withdrawn
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

proc CanvasBubble::Show { Canvas Tag X Y } {
   variable Data
   variable Resources

   set idx [lsearch -exact $Data(TagList$Canvas) $Tag]

   set Data(TagCurrent) $idx

   if { $idx > -1 } {

      .canvasbubble.hlp configure -text [lindex $Data(InfoList$Canvas) $idx]

      #----- Get window and screen dimensions
      update
      set w    [winfo width .canvasbubble.hlp]
      set h    [winfo height .canvasbubble.hlp]
      set sw   [winfo screenwidth .canvasbubble.hlp]
      set sh   [winfo screenheight .canvasbubble.hlp]
      set sdx  0
      set sdy  0
      set adj  10

      #----- Fix for multiple screen with different resolutions
      foreach {res resx resy dx dy} $Data(ScreenRes) {
         #----- Check if we are in that current physical screen
         if { $dx<=$X && $X<=$dx+$resx && $dy<=$Y && $Y<=$dy+$resy } {
            set sw   $resx
            set sh   $resy
            set sdx  $dx
            set sdy  $dy

            #----- Substract the screen delta
            incr X -$dx
            incr Y -$dy

            break
         }
      }

      #----- Adjust origin in X depending on whether there is enough space on the right or on which side there is more space
      if { $X+$adj+$w>$sw && $sw-$X-$adj-$w<$X-$adj-$w } {
         set X [expr $X-$adj-$w]
      } else {
         set X [expr $X+$adj]
      }

      #----- Adjust origin in Y, trying to fit everything
      if { $Y+$h > $sh } {
         set Y [expr {max($sh-$h-10,0)}]
      } else {
         set Y [expr $Y+$adj]
      }

      #----- Add the screen delta
      incr X $sdx
      incr Y $sdy

      wm geometry .canvasbubble +$X+$Y
      wm state .canvasbubble normal
      raise .canvasbubble
   }
}
