#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de definitions pour les les projections
# Fichier   : Data_Areas.tcl
# Creation  : Decembre 1999 - J.P. Gauthier - CMC/CMOE
#
# Description: Definitions d'une structure de donnees et de procedures
#              relatives aux regions affichables sur les projections
#
# Fonctions:
#    Areas::CreateWidget  { Parent }
#    Areas::Init          { }
#    Areas::Read          { File Fill Line { Field "" } }
#    Areas::Display       { Type }
#    Areas::DisplayToggle { Type No }
#    Areas::DisplayId     { Type { Display "" } }
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide Areas 4.0

catch { SPI::Splash "Loading Data Package Areas 4.0" }

namespace eval Areas {
   variable Lbl
   variable Data

   set Lbl(Id)   { "Identification" "Identification" }
   set Lbl(All)  { "Toutes" "All" }

   set Data(Layers) {}
   set Data(Languages) { FRANCAIS ENGLISH }
}

#----------------------------------------------------------------------------
# Nom      : <Areas::CreateWidget>
# Creation : Aout 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer un menu d'options d'affichage des regions.
#
# Parametres :
#  <Parent>  : Identificateur du widget parent
#  <Canvas>  : Identificateur du canvas ou seront affichee les regions
#  <VP>      : Identificateur du viewport
#  <Ident>   : Identificateur de la liste de regions
#
# Retour:
#  <widget>  : Identificateur du widget (Menu)
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Areas::CreateWidget { Parent } {
   global GDefs
   variable Data
   variable Lbl

   menu $Parent.areas -tearoff 1 -bd 1

   set no 0
   foreach layer $Data(Layers) {
      $Parent.areas add cascade -label $layer -menu $Parent.areas.t$no
      menu $Parent.areas.t$no -tearoff 1

      set Data(Id$layer) False
      $Parent.areas.t$no add checkbutton -label [lindex $Lbl(All) $GDefs(Lang)] -onvalue True -offvalue False -variable Areas::Data(All$layer) -command "Areas::Display $layer"
      $Parent.areas.t$no add checkbutton -label [lindex $Lbl(Id) $GDefs(Lang)] -onvalue True -offvalue False -variable Areas::Data(Id$layer) -command "Areas::DisplayId $layer"
      $Parent.areas.t$no add separator

      for { set i 0 } { $i<[ogrlayer define $layer -nb] } { incr i } {
        if { $i && ![expr $i%20] } {
           $Parent.areas.t$no add checkbutton -variable Areas::Data(Toggle$i) -label [ogrlayer define $layer -feature $i $Data(Field$layer)] \
              -command "Areas::DisplayToggle $layer $i" -columnbreak 1
        } else {
           $Parent.areas.t$no add checkbutton -variable Areas::Data(Toggle$i) -label [ogrlayer define $layer -feature $i $Data(Field$layer)] \
              -command "Areas::DisplayToggle $layer $i"
        }
      }

      incr no
   }

   return $Parent.areas
}

#----------------------------------------------------------------------------
# Nom      : <Areas::Init>
# Creation : Aout 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les liste de regions
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Areas::Init { } {
   global GDefs

   Areas::Read $GDefs(Dir)/Data/RSMC.shp     #AAAA00 #FFFF00
   Areas::Read $GDefs(Dir)/Data/VAAC.shp     #AA0000 #FF0000
   Areas::Read $GDefs(Dir)/Data/FIR.shp      #00AA00 #00FF00
   Areas::Read $GDefs(Dir)/Data/MWO.shp      #0000AA #0000FF
   Areas::Read $GDefs(Dir)/Data/Volcano.shp  #AA0000 #FF0000
   Areas::Read $GDefs(Dir)/Data/TimeZone.shp #AAAA00 #FFFF00 TZ
   Areas::Read $GDefs(Dir)/Data/RADAR.shp    #AAAAAA #FFFFFF ID
}

#----------------------------------------------------------------------------
# Nom      : <Areas::Read>
# Creation : Aout 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire les donnees des regions
#
# Parametres :
#   <File>   : Nom de fichier
#   <Fill>   : Couleur de remplissage
#   <Line>   : Couleur du contour
#   <Field>  : Champs a afficher
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Areas::Read { File Fill Line { Field "" } } {
   global GDefs
   variable Data

   foreach layer [ogrfile open $File read $File] {
      set l [lindex $layer 2]
      eval ogrlayer read $l $layer
      ogrlayer configure $l -width 1 -font XFont14 -activeoutline $Line -activefill $Fill -transparency 50

      set Data(Type$l) {}

      if { $Field!="" } {
         set Data(Field$l) $Field
      } else {
         set Data(Field$l) [lindex $Data(Languages) $GDefs(Lang)]
      }
      lappend Data(Layers) $l
   }
}

#----------------------------------------------------------------------------
# Nom      : <Areas::Display>
# Creation : Mai 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche ou supprime toutes les regions de la projection.
#
# Parametres :
#  <Type>    : Identificateur de la liste de regions
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Areas::Display { Type } {
   global GDefs
   variable Data

   set f {}

   for { set n 0 } { $n<[ogrlayer define $Type -nb] } { incr n } {

      if { $Data(All$Type) } {
         set Data(Toggle$n) 1
         lappend f $n
      } else {
         set Data(Toggle$n) 0
      }
   }
   ogrlayer define $Type -featurehighlight $f

   set idx [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $Type]
   if { ![llength $f] } {
      set Viewport::Data(Data$Page::Data(Frame)) [lreplace $Viewport::Data(Data$Page::Data(Frame)) $idx $idx]
   } elseif { $idx==-1 } {
      lappend Viewport::Data(Data$Page::Data(Frame)) $Type
   }
   set Viewport::Data(Data) $Viewport::Data(Data$Page::Data(Frame))

   projection configure $Page::Data(Frame) -data $Viewport::Data(Data$Page::Data(Frame))
   Page::Update $Page::Data(Frame)
}

#----------------------------------------------------------------------------
# Nom      : <Areas::DisplayToggle>
# Creation : Aout 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche ou supprime la region de la projection.
#
# Parametres :
#  <Type>    : Identificateur de la liste de regions
#  <No>      : Numero de la region
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Areas::DisplayToggle { Type No } {
   global GDefs
   variable Data

   set Data(All$Type) 0

   set f [ogrlayer define $Type -featurehighlight]
   set idx [lsearch -exact $f $No]

   if { $idx==-1 } {
      lappend f $No
   } elseif { [llength $f] } {
      set f [lreplace $f $idx $idx]
   }
   ogrlayer define $Type -featurehighlight $f

   set idx [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $Type]
   if { ![llength $f] } {
      set Viewport::Data(Data$Page::Data(Frame)) [lreplace $Viewport::Data(Data$Page::Data(Frame)) $idx $idx]
   } elseif { $idx==-1 } {
      lappend Viewport::Data(Data$Page::Data(Frame)) $Type
   }
   set Viewport::Data(Data) $Viewport::Data(Data$Page::Data(Frame))

   projection configure $Page::Data(Frame) -data $Viewport::Data(Data$Page::Data(Frame))
   Page::Update $Page::Data(Frame)
}

#----------------------------------------------------------------------------
# Nom      : <Areas::DisplayId>
# Creation : Aout 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche ou supprime l'identificateur de la region.
#
# Parametres :
#  <Type>    : Identificateur de la liste de regions
#  <Display> : Affichage ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Areas::DisplayId { Type { Display "" } } {
   global GDefs
   variable Data

   if { $Display!="" } {
      set Data(Id$Type) $Display
   }

   if { $Data(Id$Type) } {
      ogrlayer define $Type -label $Data(Field$Type)
   } else {
      ogrlayer define $Type -label {}
   }
   Page::Update $Page::Data(Frame)
}
