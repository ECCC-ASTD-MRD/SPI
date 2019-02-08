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
   set Lbl(Fill) { "Remplissage" "Fill" }

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
      set l [lindex $layer 0]
      $Parent.areas add cascade -label $l -menu $Parent.areas.t$no
      menu $Parent.areas.t$no -tearoff 1

      $Parent.areas.t$no add checkbutton -label [lindex $Lbl(All) $GDefs(Lang)] -onvalue True -offvalue False -variable Areas::Data(All$layer) -command "Areas::Display $layer"
      $Parent.areas.t$no add checkbutton -label [lindex $Lbl(Fill) $GDefs(Lang)] -onvalue True -offvalue False -variable Areas::Data(Fill$layer) -command "Areas::DisplayFill \"$layer\""
      $Parent.areas.t$no add checkbutton -label [lindex $Lbl(Id) $GDefs(Lang)] -onvalue True -offvalue False -variable Areas::Data(Id$layer) -command "Areas::DisplayId \"$layer\""
      foreach l $layer {
         $Parent.areas.t$no add separator

         if { [set nb [ogrlayer define $l -nb]]<50 } {
            for { set i 0 } { $i<[ogrlayer define $l -nb] } { incr i } {
               if { $i && ![expr $i%20] } {
                  $Parent.areas.t$no add checkbutton -variable Areas::Data(Toggle$l$i) -label [ogrlayer define $l -feature $i $Data(Field$l)] \
                     -command "Areas::DisplayToggle $l $i" -columnbreak 1
               } else {
                  $Parent.areas.t$no add checkbutton -variable Areas::Data(Toggle$l$i) -label [ogrlayer define $l -feature $i $Data(Field$l)] \
                     -command "Areas::DisplayToggle $l $i"
               }
            }
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
   variable Data

   Areas::Read $GDefs(Dir)/data/RSMC.shp                                                                                     #AAAA00 #FFFF00 False True
   Areas::Read [list $GDefs(Dir)/data/VAAC.shp $GDefs(Dir)/data/VAAC_555km.shp $GDefs(Dir)/data/VAAC_Washington_backup.shp ] #AA0000 #FF0000 False True
   Areas::Read $GDefs(Dir)/data/FIR.shp                                                                                      #00AA00 #00FF00 False True
   Areas::Read $GDefs(Dir)/data/MWO.shp                                                                                      #0000AA #0000FF False True
   Areas::Read $GDefs(Dir)/data/Volcano.shp                                                                                  #AA0000 #FF0000 False True
   Areas::Read $GDefs(Dir)/data/ISTOP_Regions.shp                                                                            #AA0000 #FF0000 False True RegionDesc
   Areas::Read $GDefs(Dir)/data/TimeZone.shp                                                                                 #AAAA00 #FFFF00 False True TZ
   Areas::Read $GDefs(Dir)/data/RADAR.shp                                                                                    #AAAAAA #FFFFFF False True ID
   Areas::Read $GDefs(Dir)/data/ModelDomain.shp                                                                              #00C0CA #000000 False False NAME
   Areas::Read $GDefs(Dir)/data/land_PubMesoZone.shp                                                                         #CCCCCC #BDBDBD False False NAME
   Areas::Read $GDefs(Dir)/data/land_PubStdZone.shp                                                                          #CCCCCC #BDBDBD False False NAME
   Areas::Read $GDefs(Dir)/data/water_MarStdZone.shp                                                                         #C5DAFF #B3CFCF False False NAME
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
#   <Name>   : Nom a afficherChamps a afficher
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Areas::Read { File FillColor LineColor Id Fill { Field "" } } {
   global GDefs
   variable Data

   foreach file $File {
      foreach layer [ogrfile open $file read $file] {
         set l [lindex $layer 2]
         eval ogrlayer read $l $layer

         set Data(FillColor$l) $FillColor

         if { $Field!="" } {
            set Data(Field$l) $Field
         } else {
            set Data(Field$l) [lindex $Data(Languages) $GDefs(Lang)]
         }
      }
      lappend layerid $l

      ogrlayer configure $l -width 2 -font XFont16 -activeoutline $LineColor -activefill [expr {"$Fill"?"$FillColor":""}] -labelvar [expr {"$Id"?"$Data(Field$l)":""}] -transparency 50
   }
   set Data(Id$layerid) $Id
   set Data(Fill$layerid) $Fill

   lappend Data(Layers) $layerid
}

#----------------------------------------------------------------------------
# Nom      : <Areas::Display>
# Creation : Mai 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche ou supprime toutes les regions de la projection.
#
# Parametres :
#  <args>    : Liste des identificateurs de la liste de regions.
#  <All>     : Force display of areas.
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Areas::Display { args { All "" } } {
   global GDefs
   variable Data

   foreach type $args {
      set f {}

      if { $All!="" } {
         set Data(All$type) $All
      }
      
      for { set n 0 } { $n<[ogrlayer define $type -nb] } { incr n } {

         if { $Data(All$type) } {
            set Data(Toggle$type$n) 1
            lappend f $n
         } else {
            set Data(Toggle$type$n) 0
         }
      }
      ogrlayer define $type -featurehighlight $f

      set idx [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $type]
      if { ![llength $f] } {
         set Viewport::Data(Data$Page::Data(Frame)) [lreplace $Viewport::Data(Data$Page::Data(Frame)) $idx $idx]
      } elseif { $idx==-1 } {
         lappend Viewport::Data(Data$Page::Data(Frame)) $type
      }
   }
   set Viewport::Data(Data) $Viewport::Data(Data$Page::Data(Frame))

   projection configure $Page::Data(Frame) -data $Viewport::Data(Data$Page::Data(Frame))
   Page::Update $Page::Data(Frame)
}

#----------------------------------------------------------------------------
# Nom      : <Areas::Clear>
# Creation : Novembre 2018 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprime toutes les regions de la projection.
#
# Parametres :
#  <args>    : Liste des identificateurs de la liste de regions.
#  <All>     : Force display of areas.
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Areas::Clear { } {
   global GDefs
   variable Data

   foreach type $Data(Layers) {
      set type [lindex $type 0]
      set Data(All$type) False
      
      for { set n 0 } { $n<[ogrlayer define $type -nb] } { incr n } {
         set Data(Toggle$type$n) 0
      }
      ogrlayer define $type -featurehighlight {}

      set idx [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $type]
      if { [set idx [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $type]]!=-1 } {
         set Viewport::Data(Data$Page::Data(Frame)) [lreplace $Viewport::Data(Data$Page::Data(Frame)) $idx $idx]
      }
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

proc Areas::DisplayList { Type List } {
   global GDefs
   variable Data
   
   ogrlayer define $Type -featurehighlight $List

   set idx [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $Type]
   if { ![llength $List] } {
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
#  <Types>   : Liste des identificateurs de la liste de regions.
#  <Display> : Affichage ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Areas::DisplayId { Types { Display "" } } {
   global GDefs
   variable Data

   foreach type $Types {
      if { $Display!="" } {
         set Data(Id$Types) $Display
      }

      if { $Data(Id$Types) } {
         ogrlayer configure $type -labelvar $Data(Field$type)
      } else {
         ogrlayer configure $type -labelvar {}
      }
   }
   Page::Update $Page::Data(Frame)
}

#----------------------------------------------------------------------------
# Nom      : <Areas::DisplayFill>
# Creation : Aout 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche ou supprime le remplissage des regions.
#
# Parametres :
#  <Types>   : Liste des identificateurs de la liste de regions.
#  <Display> : Affichage ou non
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Areas::DisplayFill { Types { Display "" } } {
   global GDefs
   variable Data

   foreach type $Types {
      if { $Display!="" } {
         set Data(Fill$Types) $Display
      }

      if { $Data(Fill$Types) } {
         ogrlayer configure $type -activefill $Data(FillColor$type)
      } else {
         ogrlayer configure $type -activefill {}
      }
   }
   Page::Update $Page::Data(Frame)
}
