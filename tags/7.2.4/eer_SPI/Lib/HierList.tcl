#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : HierList.tk
# Version  : 1.0 ($Revision: 0.1 $)
# Creation : Février 2003 - Stéphane Gaudreault - CMC/CMOE
#
# Description:
#    Affiche une liste hierarchique
#
# Fonctions:
#
# Remarques :
#    -Concu a partir de namespace donc utilisable seulement en TCL 8.0 et +
#    -Supporte les callback lors de la selection d'un élément
#
# Modification:
#
#   Nom         :
#   Date        :
#   Description :
#
#===============================================================================

package provide HierList 1.0

proc IdHierList { Show } {
   if { $Show } {
      puts "(INFO) Loading Standard CMC/CMOE Widget Package HierList Version 1.0"
   }
}

namespace eval HierList {
   # this is the public interface
   namespace export HierList

   variable Data
   variable Ressources

   set Data(Frame)         ""
   set Data(CallBack1)      ""
   set Data(IsLeaf)         ""
   set Data(ItemDebut)      ""
   set Data(ItemFin)          ""
   set Data(HighLightList)   ""
   set Data(MaxY)             0

   set Rect(x1) 0
   set Rect(y1) 0
   set Rect(x2) 0
   set Rect(y2) 0

   set Ressources(Plus)  "@$GDefs(Dir)/Resources/Bitmap/plus.ico"
   set Ressources(Minus) "@$GDefs(Dir)/Resources/Bitmap/minus.ico"
   set Ressources(Font)  -*-courier-bold-r-*-*-12-*-*-*-*-*-iso8859-1
}

#----------------------------------------------------------------------------
# Nom   : <HierList::Collapse>
# Creation : Frévrier 2003 - S. Gaudreault - CMC/CMOE
#
# But   : Fermeture d'une sous-liste
#
# Parametres :
#  <tag>  : tag de l'item dans le canvas
#  <nextY>    : coordonnée en y de l'item
#
# Remarques :
#
# Modifications  :
#
#   Nom : - S. Gaudreault
#   Date: - avril 2003
#   Description : - correction d'un bug de la bar de défillement
#--------------------------------------------------------------------------------

proc HierList::Collapse { tag nextY } {
   variable Data
   variable Ressources

   set canvas $Data(Frame).canvas
   set level [lindex [split $tag _] 1]

   set maxYRemonte 0
   set minYDelete [expr [winfo height $canvas] + 5000]

   # Changer le moins pour un plus
   $canvas itemconfigure SIGN_$level -bitmap $Ressources(Plus)

   # construction du tag de meme niveau suivant.
   # oui oui, je sais que c'est vraiment laid ...
   set courant [lindex [split $level -] end]
   set levelPrefix [string range $tag 0 [expr [string last $courant $tag] -1]]
   set nextSameLevel $levelPrefix[expr $courant+1]
   set nextLevelY [lindex [$canvas coords $nextSameLevel] 1]

   #si il n'existe pas d'item suivant sur le meme niveau,
   if { "$nextLevelY" == "" } {
      set sousListe [$canvas find enclosed 0 $nextY  10000 [expr $Data(MaxY) + 10000]]
      foreach item $sousListe {
         set coordY [lindex [$canvas coord $item] end]

         if { $coordY < $minYDelete } {
            set minYDelete $coordY
         }

         $canvas delete $item
      }
   } else {
      #Sinon, on efface les element de la sous-liste ET
      #on remonte les autres éléments en dessous.
      set sousListe [$canvas find enclosed 0 $nextY 100000 $nextLevelY]
      foreach item $sousListe {
         $canvas delete $item
      }

      set deplacement [expr -1 * ([lindex [$canvas coords $nextSameLevel] 1] - \
            [lindex [$canvas coords $tag] 1] - 15) ]

      set itemBellow [lsort -decreasing [$canvas find enclosed 0 $nextY 10000 \
            [expr  $Data(MaxY) + 10000]]]
      foreach item $itemBellow {
         $canvas move $item 0 $deplacement

         set tagItemMoved [lindex [$canvas gettags $item] 0]
         set element [lindex [$canvas gettags $item] 1]
         set ycoord [lindex [$canvas coords $item] 1]
         set coord [$canvas coords $tagItemMoved]
         set path [lindex [$canvas gettags $item] 2]

         if { [lindex $coord end] > $maxYRemonte } {
            set maxYRemonte [lindex $coord end]
         }

         $canvas bind $tagItemMoved <ButtonPress-1> \
               "HierList::HighLight1 [lindex $coord 0] [lindex $coord 1] $tagItemMoved; HierList::Select $path $element $tagItemMoved $ycoord"
         $canvas bind $tagItemMoved <Shift-Button-1> \
               "HierList::HighLightRect [lindex $coord 0] [lindex $coord 1] $tagItemMoved"
      }
   }

   if { $minYDelete > $maxYRemonte } {
      set maxY $minYDelete
   } else {
      set maxY $maxYRemonte
   }

   #redimensionner le canvas
   $canvas configure -height [expr 15 + $maxY]
   $canvas configure -scrollregion "1 1 280 [expr 15 +  $maxY]"

   set Data(MaxY) [expr 15 + $maxY]

   HierList::Highlight $Data(HighLightList)
}

#----------------------------------------------------------------------------
# Nom   : <HierList::Create>
# Creation : Frévrier 2003 - S. Gaudreault - CMC/CMOE
#
# But   : Création d'une liste hierachique
#
# Parametres :
#  <Frame> : Le nom du widget de base
#
# Remarques :
#
# Modifications  :
#
#   Nom : -
#   Date: -
#   Description : -
#--------------------------------------------------------------------------------

proc HierList::Create { Frame } {
   variable Data
   variable Ressources

   set Data(Frame) $Frame

   frame $Frame -bg white

   canvas $Frame.canvas -bg white -relief sunken -bd 1 \
         -yscrollcommand "$Frame.scroll set" -scrollregion "1 1 280 1" \
         -width 150 -height 5000
   scrollbar $Frame.scroll -orient vertical -bd 1 -width 10 \
         -command "$Frame.canvas yview"
   pack $Frame.canvas -side left -fill both -expand true -anchor w
   pack $Frame.scroll -side left -fill y -anchor e
#         -yscrollcommand "$Frame.scroll set" -scrollregion "1 1 280 5000" \

}

#----------------------------------------------------------------------------
# Nom   : <HierList::Destroy>
# Creation : Frévrier 2003 - S. Gaudreault - CMC/CMOE
#
# But   : Tout est dans le nom de la fobnction !!!
#
# Parametres :
#  <window>  : Le widget principal
#
# Remarques :
#
# Modifications  :
#
#   Nom : -
#   Date: -
#   Description : -
#--------------------------------------------------------------------------------

proc HierList::Destroy { window } {
   destroy $window
}

#----------------------------------------------------------------------------
# Nom   : <HierList::Display>
# Creation : Frévrier 2003 - S. Gaudreault - CMC/CMOE
#
# But   : Affichage d'une liste hierachique
#
# Parametres :
#  <List>       : Liste d'items a afficher (premier niveau)
#  <CallBack1>   : Fonction qui retourne une liste de sous-items
#  <isLeaf>      : Fonction qui détermie su un élément peut etre explosé.
#
# Remarques :
#
# Modifications  :
#
#   Nom : -
#   Date: -
#   Description : -
#--------------------------------------------------------------------------------

proc HierList::Display { List CallBack1 isLeaf } {
   global GDefs
   variable Data
   variable Ressources

   set Data(CallBack1)   $CallBack1
   set Data(IsLeaf)      $isLeaf

   set canvas $Data(Frame).canvas
   set nextY 15

   #----- Cleanup du canvas
   $canvas delete SIGN TEXT SELECTRECTANGLE

   set maxY 0
   set i 0
   foreach elem $List {
      eval set isleaf \[$Data(IsLeaf) $elem\]

      if { [string first "/" $elem] != -1 } {
         set splitlist [split $elem /]
         set elemtext [lindex $splitlist end]
      } else {
         set elemtext $elem
      }

      if { "$isleaf" == "0" } {
         $canvas create bitmap 10 $nextY \
               -tags "SIGN_$i $elem $elemtext" -bitmap $Ressources(Plus)
      }

      if { $nextY > $maxY } {
         set maxY $nextY
      }

      $canvas create text 30 $nextY -text "$elemtext" \
            -anchor w -tags "TEXT_$i $elem $elemtext" -font $Ressources(Font) -fill black

      if { $i == 0 } {
         $canvas create rectangle 0 [expr $nextY - 7] \
               200 [expr $nextY + 7] \
               -fill yellow -outline orange -tag "SELECTRECTANGLE $elemtext"
         $canvas lower SELECTRECTANGLE
      }

      set coord [$canvas coords TEXT_$i]

      $canvas bind TEXT_$i <ButtonPress-1> "HierList::HighLight1 [lindex $coord 0] [lindex $coord 1] TEXT_$i; HierList::Select $elemtext $elem TEXT_$i $nextY"
      $canvas bind SIGN_$i <ButtonPress-1> "HierList::Select $elemtext $elem TEXT_$i $nextY"
      $canvas bind TEXT_$i <Enter> "$canvas config -cursor hand2"
      $canvas bind SIGN_$i <Enter> "$canvas config -cursor hand2"
      $canvas bind TEXT_$i <Leave> "$canvas config -cursor left_ptr"
      $canvas bind SIGN_$i <Leave> "$canvas config -cursor left_ptr"
      $canvas bind TEXT_$i <Shift-Button-1> "HierList::HighLightRect [lindex $coord 0] [lindex $coord 1] TEXT_$i"

      incr nextY 15
      incr i
   }

   #agrandir le canvas
   $canvas configure -height [expr 15 + $maxY]
   $canvas configure -scrollregion "1 1 280 [expr 15 +  $maxY]"

   set Data(MaxY) [expr 15 + $maxY]
}

#----------------------------------------------------------------------------
# Nom   : <HierList::Expand>
# Creation : Frévrier 2003 - S. Gaudreault - CMC/CMOE
#
# But   : Fonction appelée lorsqu'un item est sélectionné
#
# Parametres :
#  <path> : chemin du parent de l'item dans la hierachie
#  <sousListe> : Liste d'element a afficher
#  <tag>  : tag de l'item dans le canvas
#  <nextY>    : coordonnée en y de l'item
#
# Remarques :
#
# Modifications  :
#
#   Nom : - S. Gaudreault
#   Date: - avril 2003
#   Description : - correction d'un bug de la bar de défillement
#--------------------------------------------------------------------------------

proc HierList::Expand { parentpath sousListe tag nextY } {
   variable Data
   variable Ressources

   set canvas $Data(Frame).canvas
   set level [lindex [split $tag _] 1]
   set indentation [expr [llength [split $level -]] * 15 ]
   set itemBellow [lsort -decreasing [$canvas find enclosed 0 $nextY 10000 [expr  $Data(MaxY) + 10000]]]

   set deplacement [expr 15 * [llength $sousListe]]

   # Changer le plus pour un moins
   $canvas itemconfigure SIGN_$level -bitmap $Ressources(Minus)

   set maxY 0

   # on déplace les items
   foreach item $itemBellow {
      $canvas move $item 0 $deplacement

      set tagItemMoved [lindex [$canvas gettags $item] 0]
      set coord [$canvas coords $tagItemMoved]

      if { [lindex $coord end] > $maxY } {
         set maxY [lindex $coord end]
      }

      set element [lindex [$canvas gettags $item] 1]
      set path [lindex [$canvas gettags $item] 2]

      $canvas bind $tagItemMoved <ButtonPress-1> \
            "HierList::HighLight1 [lindex $coord 0] [lindex $coord 1] $tagItemMoved; HierList::Select $path $element $tagItemMoved [lindex [$canvas coords $item] 1]"
      $canvas bind $tagItemMoved <Shift-Button-1> \
            "HierList::HighLightRect [lindex $coord 0] [lindex $coord 1] $tagItemMoved"
   }

   set i 0
   foreach elem $sousListe {
      set nextY [expr $nextY + 15]
      set newTextTag "TEXT_$level-$i"
      set newSingTag "SIGN_$level-$i"

      if { $nextY > $maxY } {
         set maxY $nextY
      }

      eval set isleaf \[$Data(IsLeaf) $elem\]

      if { [string first "/" $elem] != -1 } {
         set splitlist [split $elem /]
         set elemtext [lindex $splitlist end]
      } else {
         set elemtext $elem
      }

      set path "$parentpath/$elemtext"

      if { "$isleaf" == "0" } {
         set deplacement [expr $indentation + 15]
         $canvas create bitmap $indentation $nextY \
               -bitmap $Ressources(Plus) -tags "$newSingTag $elem $path"
      } else {
         set deplacement $indentation
      }

      $canvas create text $deplacement $nextY -text "$elemtext" \
            -anchor w -tags "$newTextTag $elem $path" -font $Ressources(Font) -fill black

      set coord [$canvas coords $newTextTag]
      $canvas bind $newTextTag <ButtonPress-1> "HierList::HighLight1 [lindex $coord 0] [lindex $coord 1] $newTextTag; HierList::Select $path $elem $newTextTag $nextY"
      $canvas bind $newSingTag <ButtonPress-1> "HierList::Select $path $elem $newTextTag $nextY"
      $canvas bind $newTextTag <Enter> "$canvas config -cursor hand2"
      $canvas bind $newSingTag <Enter> "$canvas config -cursor hand2"
      $canvas bind $newTextTag <Leave> "$canvas config -cursor left_ptr"
      $canvas bind $newSingTag <Leave> "$canvas config -cursor left_ptr"
      $canvas bind $newTextTag <Shift-Button-1> "HierList::HighLightRect [lindex $coord 0] [lindex $coord 1] $newTextTag"

      incr i
   }

   #agrandir le canvas
   $canvas configure -height [expr 15 + $maxY]
   $canvas configure -scrollregion "1 1 280 [expr 15 +  $maxY]"

   set Data(MaxY) [expr 15 + $maxY]

   HierList::Highlight $Data(HighLightList)
}

#----------------------------------------------------------------------------
# Nom   : <HierList::Highlight>
# Creation : Frévrier 2003 - S. Gaudreault - CMC/CMOE
#
# But   : Met en évidence une liste d'item
#
# Parametres :
#
# Remarques :
#
# Modifications  :
#
#   Nom : -
#   Date: -
#   Description : -
#--------------------------------------------------------------------------------

proc HierList::Highlight { pathlist { color "red" } } {
   variable Data

   set canvas $Data(Frame).canvas
   set Data(HighLightList) $pathlist

   $canvas delete HIGHLIGHTRECTANGLE

   if { [llength $pathlist] == 0 } {
      return
   }

   foreach tag $pathlist {
      set itemList   [$canvas find withtag $tag]

      foreach item $itemList {
         set coordY [lindex [$canvas coords $tag] 1]

         if { [$canvas type $tag] == "rectangle" } {
            set coordY [expr $coordY + 7]
         }

         if { $coordY == "" } {
            continue
         }

         $canvas create rectangle 0 [expr $coordY - 7] \
               200 [expr $coordY + 7] \
               -fill $color  -tag "HIGHLIGHTRECTANGLE"
         $canvas lower HIGHLIGHTRECTANGLE
      }
   }
}

#----------------------------------------------------------------------------
# Nom   : <HierList::HighLight1>
# Creation : avril 2003 - S. Gaudreault - CMC/CMOE
#
# But   : Selection d'un interval avec la sourie
#
# Parametres :
#  <x> : coord en x de l'item
#  <y> : "    "    y   "
#  <selectedItemTag>  : tag de l'item dans le canvas
#
# Remarques :
#
# Modifications  :
#
#   Nom : -
#   Date: -
#   Description : -
#--------------------------------------------------------------------------------

proc HierList::HighLight1 {  x y selectedItemTag  } {
   variable Data
   variable Ressources
   variable Rect

   set canvas $Data(Frame).canvas
   $canvas delete SELECTRECTANGLE

   set Rect(x1) $x
   set Rect(y1) $y
   $canvas create rectangle 0 [expr $Rect(y1) - 7] \
         [expr [winfo width $canvas] + 10] [expr $Rect(y1) + 7 ] \
         -fill yellow -outline orange -tag SELECTRECTANGLE
   $canvas lower SELECTRECTANGLE

   set Data(ItemDebut)  [lindex [$canvas gettags $selectedItemTag] 2]
   set Data(ItemFin)    ""
}

#----------------------------------------------------------------------------
# Nom   : <HierList::HighLightRect>
# Creation : avril 2003 - S. Gaudreault - CMC/CMOE
#
# But   : Afficher un rectangle de couleur en dessous de la selection
#
# Parametres :
#  <x> : coord en x de l'item
#  <y> : "    "    y   "
#  <selectedItemTag>  : tag de l'item dans le canvas
#
# Remarques :
#
# Modifications  :
#
#   Nom : -
#   Date: -
#   Description : -
#--------------------------------------------------------------------------------

proc HierList::HighLightRect {  x y selectedItemTag  } {
   variable Data
   variable Ressources
   variable Rect

   set canvas $Data(Frame).canvas
   $canvas delete SELECTRECTANGLE

   if { ("$Data(ItemDebut)" != "") && ("$Data(ItemFin)" == "") } {
      if { $Rect(y1) > $y } {
         set tmp $y
         set y $Rect(y1)
         set Rect(y1) $tmp
      }

      $canvas create rectangle 0 [expr $Rect(y1) - 7] \
            [expr [winfo width $canvas] + 10] [expr $y + 7] \
            -fill yellow -outline orange -tag SELECTRECTANGLE
      $canvas lower SELECTRECTANGLE

      set Data(ItemFin)      [lindex [$canvas gettags $selectedItemTag] 2]

   } else {
      HierList::HighLight1 $x $y $selectedItemTag
   }
}

#----------------------------------------------------------------------------
# Nom   : <HierList::GetSelection>
# Creation : Frévrier 2003 - S. Gaudreault - CMC/CMOE
#
# But   : Retourne l'item selectionné ou l'interval de selection
#
# Parametres :
#
# Remarques :
#
# Modifications  :
#
#   Nom : -
#   Date: -
#   Description : -
#--------------------------------------------------------------------------------

proc HierList::GetSelection { } {
   variable Data

   if { ($Data(ItemDebut) != "") && ($Data(ItemFin) != "") } {
      return "$Data(ItemDebut) $Data(ItemFin)"
   } else {
      set canvas $Data(Frame).canvas
      return [lindex [$canvas gettags SELECTRECTANGLE] 1]
   }
}

#----------------------------------------------------------------------------
# Nom   : <HierList::Select>
# Creation : Frévrier 2003 - S. Gaudreault - CMC/CMOE
#
# But   : Fonction appelée lorsqu'un item est sélectionné
#
# Parametres :
#  <path> : chemin de l'item dans la hierachie
#   <elem> : texte de l'item
#   <tag>  : tag de l'item dans le canvas
#  <y>    : coordonnée en y de l'item
#
# Remarques :
#
# Modifications  :
#
#   Nom : -
#   Date: -
#   Description : -
#--------------------------------------------------------------------------------

proc HierList::Select { path elem tag y } {
   variable Data
   variable Ressources

   set itemlevel [llength [split [lindex [split $tag _] 1] -]]

   set canvas $Data(Frame).canvas
   $canvas delete SELECTRECTANGLE HIGHLIGHTRECTANGLE

   $canvas create rectangle 0 [expr $y - 7] \
         [expr [winfo width $canvas] + 10] [expr $y + 7] \
         -fill yellow -outline orange -tag "SELECTRECTANGLE $path"
   $canvas lower SELECTRECTANGLE

   $canvas configure -cursor watch
   eval set sousListe \[$Data(CallBack1) $elem\]
   eval set isLeaf    \[$Data(IsLeaf) $elem\]

   if { ($sousListe == "") || ("isLeaf" == "1") } {
   } else {
      set level [lindex [split $tag _] 1]
      if {[$canvas itemcget SIGN_$level -bitmap] == $Ressources(Plus) } {
         HierList::Expand $path $sousListe $tag $y
      } else {
         HierList::Collapse $tag $y
      }
   }
}

#----------------------------------------------------------------------------
# Nom   : <HierList::SelectionType>
# Creation : Frévrier 2003 - S. Gaudreault - CMC/CMOE
#
# But   : Retourne RANGE si on a selectionné plusieurs items
#         avec le 3e bouton de la sourie, UNIQ sinon
#
# Parametres :
#
# Remarques :
#
# Modifications  :
#
#   Nom : -
#   Date: -
#   Description : -
#--------------------------------------------------------------------------------

proc HierList::SelectionType { } {
   variable Data

   if { ($Data(ItemDebut) != "") && ($Data(ItemFin) != "") } {
      return "RANGE"
   } else {
      return "UNIQ"
   }
}
