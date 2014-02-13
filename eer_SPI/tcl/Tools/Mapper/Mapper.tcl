#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : Mapper.tcl
# Creation : Juin 2002 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Permet d'afficher un selecteur de champs de fichier geotiff.
#
# Remarques :
# #
#===============================================================================

#----- Lire les sources d'execution

source $GDefs(Dir)/tcl/Tools/Mapper/Mapper.ctes
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper.txt
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper.int
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper_Geo.tcl
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper_GeoLocator.tcl
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper_Cutter.tcl
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper_OGR.tcl
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper_GDAL.tcl
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper_MDL.tcl
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper_WKT.tcl
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper_DepotWare.tcl

#-------------------------------------------------------------------------------
# Nom      : <Mapper::Close>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Ferme tout les fichiers ouvert et detruit la fenetre.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::Close { } {
   variable Data

   if { $Page::Data(ToolMode)=="Mapper" || $Page::Data(ToolMode)=="Mapper::Cutter" } {
      SPI::ToolMode SPI Zoom
   }

   trace remove variable Viewport::Data(Data) write Mapper::Toggler

   set Data(Active) 0
   set Mapper::DepotWare::Data(Coo)    ""

   #----- Cleanup des donnees actives
   Mapper::Clear

   if { [winfo exists .mapperparams] } {
        Mapper::ParamsClose
   }

   Viewport::FollowerRemove Mapper
   Mapper::DepotWare::CacheClean

   destroy .mapper

   if { !$SPI::Param(Window) } { SPI::Quit }
}

#-------------------------------------------------------------------------------
# Nom      : <NowCaster::Clear>
# Creation : Juin 2013 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer toutes les données.
#
# Parametres :
#  <Frames>  : Identificateurs de Page
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::Clear { { Frames {} } } {
   variable Data
   
   if { ![llength $Frames] } {
      set Frames $Page::Data(Frames)
   }
   
   foreach frame $Frames {
      if { [info exist Viewport::Data(Data$frame)] && [llength $Viewport::Data(Data$frame)] } {

         #----- Unassign from projection
         set objects $Viewport::Data(Data$frame)

         #----- Free data objects
         foreach object $objects {
            Mapper::Del $object $frame
         }
      }

      if { [winfo exists $frame.page.canvas]} {
         $frame.page.canvas delete MAPPERSEARCH MAPPERCUTTER MAPPERGEOLOCATOR MAPPERVERTEX
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::Follower>
# Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Plugin au bindings de suivit de coordonnees du Viewport
#
# Parametres :
#   <Frame>  : Page courante
#   <Canvas> : Canvas courant
#   <VP>     : Viewport courant
#   <Lat>    : Lattitude
#   <Lon>    : Longitude
#   <X>      : Pixel en X
#   <Y>      : Pixel en Y
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::Follower  { Frame Canvas VP Lat Lon X Y } {
   variable Data

   set Data(Job) ""
   set list {}

   foreach data [projection configure $Frame -data] {
      if { [gdalband is $data] } {
         lappend list [list $data [gdalband stats $data -coordpoint $Lat $Lon] [gdalband stats $data -unproject $Lat $Lon] [gdalband stats $data -coordvalue $Lat $Lon]]
         append Data(Job) "([gdalband stats $data -coordvalue $Lat $Lon]) "
      }
      if { [ogrlayer is $data] } {
         lappend list [list $data { -1 -1 } [ogrlayer stats $data -unproject $Lat $Lon] -]
      }
   }
   return $list
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::Scroll>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Changer l'ordre d'affichage des objets geographiques.
#
# Parametres :
#   <Side>   : Direction du deplacement dans la liste
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::Scroll { Side } {
   variable Data

   if { [set idx [$Data(Tab1).select.list curselection]]!="" } {
      set data [lindex $Viewport::Data(Data) $idx]
      set new $idx

      switch -- $Side {
         "F"      { set new [expr [llength $Viewport::Data(Data)] -1] }
         "B"      { set new 0 }
         default  { incr new $Side }
      }

      if { $new>=0 && $new<[llength $Viewport::Data(Data)] } {
         if { $new < $idx } {
            set Viewport::Data(Data) [lreplace $Viewport::Data(Data) $idx $idx]
            set Viewport::Data(Data) [linsert $Viewport::Data(Data) $new $data]
         } else {
            set Viewport::Data(Data) [lreplace $Viewport::Data(Data) $idx $idx]
            set Viewport::Data(Data) [linsert $Viewport::Data(Data) $new $data]
         }
         set Viewport::Data(Data$Page::Data(Frame)) $Viewport::Data(Data)

         $Data(Tab1).select.list selection clear 0 end
         $Data(Tab1).select.list selection set $new

         Mapper::UpdateData $Page::Data(Frame)
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::Del>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer un objet geographique de la liste d'affichage.
#
# Parametres :
#   <Object> : Object to delete
#   <Frame>  : Page contenant l'objet
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::Del { { Object "" } { Frame "" } } {
   variable Msg
   variable Lbl
   variable Data

   set idx 0
   if { $Object=="" } {
      if { [set idx [$Data(Tab1).select.list curselection]]!="" } {
         set Object [$Data(Tab1).select.list get $idx]
      }
   }

   if { $Frame=="" } {
      set Frame $Page::Data(Frame)
   }

   if { $Object!="" } {

      #----- If any data has been changed
      if { [ogrlayer is $Object] && [ogrlayer ischanged $Object] } {
         if { [Dialog::Default .mapper 300 WARNING $Msg(Save) "\n\n\t$Object" 0 $Lbl(No) $Lbl(Yes)] } {
            Mapper::Write $Object
         }      
      }
   
      set Viewport::Data(Data$Frame) [lreplace $Viewport::Data(Data$Frame) $idx $idx]
      Mapper::UpdateData $Frame

      if { [info exists Mapper::Data(Id$Object)] } {
         if { [gdalband is $Object] } {
            gdalband free $Object BandX$Object BandY$Object
            gdalfile close $Data(Id$Object)
            ogrgeometry free MASK$Object  MASKRING$Object

            set tag WMSLEGEND[string map { % "" . "" " " "" \' " " \" " " } $Object]
            Shape::UnBind $Frame.page.canvas $tag
            $Frame.page.canvas delete $tag
            catch { image delete $tag }

         } elseif { [ogrlayer is $Object] } {
            ogrlayer free $Object
            ogrfile close $Data(Id$Object)
         } elseif { [model is $Object] } {
            model free $Object
         }
         unset Data(Id$Object)
      }
      
      if { [colormap is $Object] } {
         colormap free $Object
      }

      if { $Object==$Data(Object) } {
         destroy .mapperparams
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::SetGeoRef>
# Creation : Fevrier 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Selectionner la georeference de l'item courant comme projection par defaut
#            selections.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::SetGeoRef { } {
   global GDefs
   variable Data
   variable Msg

   if { [set idx [$Data(Tab1).select.list curselection]]!="" } {
      set object [$Data(Tab1).select.list get $idx]

      if { [gdalband is $object] } {
         set Data(GeoRef) [gdalband define $object -georef]
      } elseif { [ogrlayer is $object] } {
         set Data(GeoRef) [ogrlayer define $object -georef]
      } elseif { [model is $object] } {
         set Data(GeoRef) [model define $object -georef]
      }

      if { [georef is $Data(GeoRef)] } {
         set Viewport::Map(GeoRef) $Data(GeoRef)
         projection configure $Page::Data(Frame) -type grid -georef $Data(GeoRef)
         Viewport::ConfigGet $Page::Data(Frame) $Viewport::Data(VP)
         Viewport::Reset $Page::Data(Frame)
      } else {
         Dialog::Error . $Msg(GeoRef)
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::Locate>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE -
#
# But      : Centrer la projection sur l'objet selectionne
#
# Parametres :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::Locate { } {
   variable Data

   if { [set idx [$Data(Tab1).select.list curselection]]!="" } {
      set object [$Data(Tab1).select.list get $idx]

      if { [gdalband is $object] } {
         set coords [gdalband project $object [expr [gdalband define $object -width]/2.0] [expr [gdalband define $object -height]/2.0]]
      } elseif { [ogrlayer is $object] } {
         set coords [ogrlayer stats $object -centroid 0]
         if { [llength $coords] } {
            set coords [ogrlayer project $object [lindex $coords 0] [lindex $coords 1]]
         }
      } elseif { [model is $object] } {
         if { [model define $object -projection]=="" } {
            set coords [model matrix $object -locate]
         } else {
            set coords [model define $object -coordinate]
         }
      }
      if { [llength $coords] } {
         SPI::Locate [lindex $coords 0] [lindex $coords 1]
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::Zoom>
# Creation : Fevrier 2006 - J.P. Gauthier - CMC/CMOE -
#
# But      : Zoomer la camera sur les limites de la donnee
#
# Parametres :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::Zoom { { All False } } {
   variable Data

   set objects {}
   set y0 1e32
   set y1 -1e32
   set x0 1e32
   set x1 -1e32
   set zoom False

   if { $All } {
      set objects [$Data(Tab1).select.list get 0 end]
   } else {
      if { [set idx [$Data(Tab1).select.list curselection]]!="" } {
         set objects [$Data(Tab1).select.list get $idx]
      }
   }

   foreach object $objects {
      set coords0 [set coords1 {}]

      if { [gdalband is $object] } {
         set coords [gdalband stats $object -llextent]
      } elseif { [ogrlayer is $object] } {
         set coords [ogrlayer stats $object -llextent]
      } elseif { [model is $object] } {
         set coords [model stats $object -llextent]
      }
      if { [llength $coords] } {
         set zoom True
         set x0 [lindex $coords 1]
         set y0 [lindex $coords 0]
         set x1 [lindex $coords 3]
         set y1 [lindex $coords 2]
      }
   }

   if { $zoom } {
      ProjCam::CloseUp $Page::Data(Frame) $Page::Data(Frame) $Viewport::Data(VP) $y0 $x0 $y1 $x1 0.0
   }
}

proc Mapper::ZoomFull { } {
   variable Data

   if { [set idx [$Data(Tab1).select.list curselection]]!="" } {
      set object [$Data(Tab1).select.list get $idx]

      if { [gdalband is $object] } {
         set x [expr [gdalband define $object -width]/2]
         set y [expr [gdalband define $object -height]/2]
         set c0 [gdalband stats $object -project [expr $x-0.5] $y]
         set c1 [gdalband stats $object -project [expr $x+0.5] $y]

         set c   [concat $c0 $c1]
         set dxy [projection function $Page::Data(Frame) -dist $c 0.0]

         ProjCam::ZoomIn $Page::Data(Frame) $Page::Data(Frame) $Viewport::Data(VP) [$Viewport::Data(VP) -distpix $dxy]
      }
   }
}

proc Mapper::GetColor { } {
   return [format "#%02X%02X%02X" [expr int(rand()*255)] [expr int(rand()*255)] [expr int(rand()*255)]]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::Read>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Lecture d'une donnee geographique.
#
# Parametres :
#   <Files>  : Fichiers a lire
#   <Full>   : Lectur complete ou partielle
#   <Mode>   : Mode de donnees (ANY,GDAL ou OGR)
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::Write { Object } {

   if { [ogrlayer is $Object] } {
      Mapper::OGR::Write [FileBox::Create .mapper - Save $Mapper::OGR::Data(WriteFormats)] $FileBox::Param(Type) $Object
   } else {
   }
}

proc Mapper::Read { Files { Full False } { Mode ANY } } {
   global   GDefs
   variable Data
   variable Msg

   if { $Files=="" } {
      return
   }

   Mapper::Cursor watch

   foreach file $Files {
       switch $Mode {
          ANY  { if { [Mapper::GDAL::Read $file "" 3 $Full]=="" && [Mapper::OGR::Read $file]=="" } {
                    Dialog::Error . $Msg(BadFile)
                 }
               }
          GDAL { if { [Mapper::GDAL::Read $file "" 3 $Full]=="" } {
                    Dialog::Error . $Msg(BadFile)
                 }
               }
          OGR  { if { [Mapper::OGR::Read $file]=="" } {
                    Dialog::Error . $Msg(BadFile)
                 }
               }
      }
      Mapper::UpdateData $Page::Data(Frame)
   }
   Mapper::Cursor left_ptr
}

proc Mapper::Progress { Object } {
   global GDefs
   variable Lbl

   if { [ogrlayer is $Object] } {
      set ready [ogrlayer define $Object -nbready]
      set total [ogrlayer define $Object -nb]

      if { $ready!=$total } {
         set pc [expr double($ready)/$total*100.0]
         SPI::Progress $pc "[lindex $Lbl(Project) $GDefs(Lang)] $Object [format "%.1f" ${pc}]%"
         after 100 Mapper::Progress \"$Object\"
         return
      }
   }
   SPI::Progress 0 ""
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::ProjFile>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer la projection Wkt a partir d'un fichier
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#   <Index>  : Index de l'item
#   <Locate> : Centre sur l'item
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::ProjFile { Widget File } {

   if { $File!="" } {
      set proj ""

      set f [open $File]
      while { ![eof $f] } {
         append proj [gets $f]
      }
      close $f

      $Widget delete 0.0 end
      $Widget insert 0.0 $proj
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::Draw...>
# Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de manipulations de la selection sur la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Ces fonctions sont appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc Mapper::Pick { VP X Y } {
   global GDefs
   variable Data
   variable Msg
   variable Lbl

   set mid [$Viewport::Data(VP) -unproject $X $Y]

   if { !$Data(PickSize) } {
      #----- Pixel pick
      set co0 $mid
      set co1 $co0
      set coords [list [lindex $co0 0] [lindex $co0 1]]
   } else {
      #----- Rectangle pick
      set co0 [$VP -unproject [expr $X-$Data(PickSize)] [expr $Y-$Data(PickSize)]]
      set co1 [$VP -unproject [expr $X+$Data(PickSize)] [expr $Y-$Data(PickSize)]]
      set co2 [$VP -unproject [expr $X+$Data(PickSize)] [expr $Y+$Data(PickSize)]]
      set co3 [$VP -unproject [expr $X-$Data(PickSize)] [expr $Y+$Data(PickSize)]]
      set coords [list [lindex $co0 0] [lindex $co0 1] [lindex $co1 0] [lindex $co1 1] [lindex $co2 0] [lindex $co2 1] [lindex $co3 0] [lindex $co3 1] [lindex $co0 0] [lindex $co0 1]]
   }

   #----- Est-ce que les coordonnees sont valides
   if { [lindex $co0 0]==-999.0 || [lindex $co1 0]==-999.0 } {
      return
   }
   
   for { set i [expr [llength $Viewport::Data(Data)]-1] } { $i>=0 } { incr i -1 } {
      set object [lindex $Viewport::Data(Data) $i]
      if { [ogrlayer is $object] && [ogrlayer configure $object -active] } {
         if { [set idx [ogrlayer pick $object $coords False]]!="" } {

            $Data(Tab1).select.list selection clear 0 end
            $Data(Tab1).select.list selection set [lsearch -exact $Viewport::Data(Data) $object]
            set Data(Object) $object
            set Mapper::OGR::Data(Index) $idx
            
            Mapper::OGR::ParamsGet $Data(Object)
            Mapper::OGR::Params $Data(Object) 2
            Mapper::OGR::Table  $Data(Object) $idx

            #----- Check if this is an index to some other data
            if { ![catch { set files [ogrlayer define $object -feature $idx IDX_PATH]}] } {
               if { $files!="" } {
                  if { ![Dialog::Default . 400 INFO $Msg(Index) "\n\n$files\n" 0 $Lbl(Yes) $Lbl(No)] } {
                     foreach file $files {
                        set path [file dirname [ogrfile filename $Data(Id$object)]]
                        set file $path/../$file
                        set band [Mapper::GDAL::Read $file]
                        ogrgeometry copy MASK$band [ogrlayer define $object -geometry $idx]
                        ogrgeometry stats MASK$band -transform [gdalband define $band -georef]
                        gdalband configure $band -mask MASK$band
                        set Mapper::Data(Cut) True

                        set Viewport::Data(Data) $Viewport::Data(Data$Page::Data(Frame))
                        set Data(Job) [lindex $Msg(Render) $GDefs(Lang)]
                        update idletasks
                        projection configure $Page::Data(Frame) -data $Viewport::Data(Data$Data(Frame))
                        Page::Update $Page::Data(Frame)
                        set Data(Job) ""
                     }
                  }
               }
            }
            break
         }
      } elseif { [gdalband is $object] && [info exists ::Mapper::DepotWare::WMS::Data($object)]} {
         Dialog::Text .mapperpick [lindex $Lbl(Pick) $GDefs(Lang)] [gdalband pick $object [lrange $mid 0 1]]
      }               
   }
}

proc Mapper::DrawClear  { Frame VP } {
   variable Data

   set Data(Lat0) $Data(Lat1)
   set Data(Lon0) $Data(Lon1)
   set Data(Coo) ""
}

proc Mapper::DrawInit  { Frame VP } {
   global GDefs
   variable Data
   variable Lbl

   set Data(InfoId)  ""
   set Data(InfoObs) ""
   set Data(Lat0) $Viewport::Map(LatCursor)
   set Data(Lon0) $Viewport::Map(LonCursor)

   if { $Data(PickSize)>=0 } {
      Mapper::Pick $VP $Viewport::Map(X) $Viewport::Map(Y)
   }
}

proc Mapper::Draw     { Frame VP } {
   variable Data
   
   if { $Data(PickSize)>=0 } {
      return
   }
   
   if { [winfo exists $Data(Canvas)] } {
      $Data(Canvas) delete MAPPERRANGE
   }
   set Data(Lat1)   $Viewport::Map(LatCursor)
   set Data(Lon1)   $Viewport::Map(LonCursor)
   set Data(Canvas) $Page::Data(Canvas)
   set Data(Frame)  $Frame
   set Data(VP)     $VP

   Viewport::DrawRange $Frame $VP $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) MAPPERRANGE red
}

proc Mapper::DrawDone { Frame VP } {
   variable Data
   
   if { $Data(PickSize)>=0 } {
      return
   }

   if { $Data(Lat0)>$Data(Lat1) } {
      set tmp $Data(Lat1)
      set Data(Lat1) $Data(Lat0)
      set Data(Lat0) $tmp
   }

   if { $Data(Lon0)>$Data(Lon1) } {
      set tmp $Data(Lon1)
      set Data(Lon1) $Data(Lon0)
      set Data(Lon0) $tmp
   }

   if { $Data(Lat0)==$Data(Lat1) || $Data(Lon0)==$Data(Lon1) } {
      set Data(Coo) ""
   } else {
      set Data(Coo) "$Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1)"
   }

   set coords [list $Data(Lat0) $Data(Lon0) $Data(Lat0) $Data(Lon1) $Data(Lat1) $Data(Lon1) $Data(Lat1) $Data(Lon0) $Data(Lat0) $Data(Lon0)]
  
   for { set i [expr [llength $Viewport::Data(Data)]-1] } { $i>=0 } { incr i -1 } {
      set object [lindex $Viewport::Data(Data) $i]
      if { [ogrlayer is $object] && [ogrlayer configure $object -active] } {
         ogrlayer define $object -featureselect {}
         if { [set idx [ogrlayer pick $object $coords True]]!="" } {
            set Data(Object) $object

            ogrlayer define $object -featureselect [list [list - # $idx]]
            ogrlayer define $object -featurehighlight {}

            Mapper::OGR::ParamsGet $object
            Mapper::OGR::Params $object 4
            Mapper::OGR::Table  $object
            break
         }
      }
   }
}

proc Mapper::MoveInit { Frame VP } {
   variable Data

   set Data(LonD) $Viewport::Map(LonCursor)
   set Data(LatD) $Viewport::Map(LatCursor)
}

proc Mapper::Move { Frame VP } {
   variable Data

   if { $Data(PickSize)>=0 } {
     return
   }
   
   #----- Effectuer la translation

   set lat0 [expr $Data(Lat0) + $Viewport::Map(LatCursor) - $Data(LatD)]
   set lat1 [expr $Data(Lat1) + $Viewport::Map(LatCursor) - $Data(LatD)]

   if { $lat0 > -90.0 && $lat0 < 90.0 && $lat1 > -90.0 && $lat1 < 90.0 } {

      set Data(Lat0) $lat0
      set Data(Lat1) $lat1
      eval set Data(Lon0) [Viewport::CheckCoord [expr $Data(Lon0) + $Viewport::Map(LonCursor) - $Data(LonD)]]
      eval set Data(Lon1) [Viewport::CheckCoord [expr $Data(Lon1) + $Viewport::Map(LonCursor) - $Data(LonD)]]
   }

   #----- Reaffecter le point de reference de translation

   if { [winfo exists $Data(Canvas)] } {
      $Data(Canvas) delete MAPPERRANGE
   }

   set Data(LonD)   $Viewport::Map(LonCursor)
   set Data(LatD)   $Viewport::Map(LatCursor)
   set Data(Canvas) $Page::Data(Canvas)
   set Data(Frame)  $Frame
   set Data(VP)     $VP

   Viewport::DrawRange $Frame $VP $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) MAPPERRANGE red
}

proc Mapper::MoveDone { Frame VP } {
   variable Data

   Mapper::DrawDone $Frame $VP
}

#----------------------------------------------------------------------------
# Nom      : <Mapper::UpdateData>
# Creation : Juin 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour l'affichage des donnees pour une page specifique
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Data>    : Donnees
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Mapper::UpdateData { Frame args } {
   global GDefs
   variable Data
   variable License
   variable Msg

   if { [llength $args] } {
      set Viewport::Data(Data$Frame) [concat $Viewport::Data(Data$Frame) $args]
   }
   set Viewport::Data(Data) $Viewport::Data(Data$Frame)

   set Data(Canvas) $Page::Data(Canvas)
   set Data(Frame)  $Page::Data(Frame)
   set Data(VP)     $Viewport::Data(VP)

   set Data(Job) [lindex $Msg(Render) $GDefs(Lang)]
   update idletasks

   set license ""
   foreach data $Viewport::Data(Data$Frame) {
      if { [info exists Mapper::License($data)] } {
         lappend license "$License($data)"
      }
   }

   if { [projection is $Frame ] } {
      projection configure $Frame -data $Viewport::Data(Data) -license [join $license \n]
      Page::Update $Frame
   }

   set Data(Job) ""
}

#----------------------------------------------------------------------------
# Nom      : <Mapper::UpdateItems>
# Creation : Juin 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour la selectiond des donnees
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Mapper::UpdateItems { { Frame "" } } {
   global GDefs
   variable Data

   if { ![llength [info commands $Data(VP)]] } {
      set Data(VP) ""
   }
   
   if { $Frame=="" } {
      set Frame $Data(Frame)
   }
   
   if { $Frame==$Data(Frame) && $Data(VP)!="" } {

      #----- Canvas might not exist anymore so catch this call
      catch { $Data(Canvas) delete MAPPERSEARCH MAPPERCUTTER MAPPERGEOLOCATOR MAPPERRANGE }
      
      if { $Data(Coo)!="" && $Frame==$Data(Frame) && $Data(VP)!="" } {
         Viewport::DrawRange $Data(Frame) $Data(VP) $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) MAPPERRANGE red
      }
      
      
      if { $Mapper::DepotWare::Data(Coo)!="" } {
         Viewport::DrawRange $Frame $Data(VP) $Mapper::DepotWare::Data(Lat0) $Mapper::DepotWare::Data(Lon0) $Mapper::DepotWare::Data(Lat1) $Mapper::DepotWare::Data(Lon1) MAPPERSEARCH red
      }

      if { $Data(CutShow) && [info exists Data(Mask$Data(Object))] && [llength $Data(Mask$Data(Object))] } {
         set tmp $Data(Mask$Data(Object))
         lappend tmp [lindex $tmp 0] [lindex $tmp 1] 0

         Viewport::DrawLine $Frame $Data(VP) $tmp "PAGE$Data(VP) MAPPERCUTTER" red 2
      }
   }

   if { $Data(RefVP)!="" && [Page::Registered $Data(Frame4).georef.hfrm.map Viewport $Data(RefVP)]!=-1 } {
      $Frame.page.canvas delete MAPPERGEOLOCATOR
      set Viewport::Data(VP) [Page::Registered $Frame Viewport]

      foreach curindex $Mapper::GeoLocator::Data(GCPS) {

         #----- Assigner les bonnes valeurs pour redessiner le canvas (la liste xy)
         if { $Frame == "$Data(Frame4).georef.hfrm.map" } {
            set xy [$Mapper::Data(RefVP) -grid [lindex $Mapper::GeoLocator::Data(GCP$curindex) 0] [lindex  $Mapper::GeoLocator::Data(GCP$curindex) 1]]
         } else {

            #----- Assigner le LatLon et redessiner les points dans le frame courant

            if { [lindex $Mapper::GeoLocator::Data(GCP$curindex) 2] != -999.0 } {
               set xy [$Viewport::Data(VP) -project [lindex $Mapper::GeoLocator::Data(GCP$curindex) 4] [lindex $Mapper::GeoLocator::Data(GCP$curindex) 5] 0.0]
            } else {
               continue
            }
         }
         if { [lindex $xy 2]>0 } {
            $Frame.page.canvas create oval [expr [lindex $xy 0] - 3] [expr [lindex $xy 1] - 3] [expr [lindex $xy 0] + 3] [expr [lindex $xy 1] + 3] -fill red -outline red -width 1 -tag MAPPERGEOLOCATOR
            $Frame.page.canvas create text [expr [lindex $xy 0] + 8] [expr [lindex $xy 1] + 8] -text $curindex -fill red -tag MAPPERGEOLOCATOR -anchor nw
      }
      }
   }
   
   if { [ogrgeometry is $Mapper::OGR::Data(Geom)] } {
      $Frame.page.canvas delete MAPPERVERTEX MAPPERLINE
      Mapper::OGR::VertexShow $Page::Data(Frame) $Viewport::Data(VP) $Mapper::OGR::Data(Geom)
      Mapper::OGR::VertexLine $Page::Data(Frame) $Viewport::Data(VP)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::AsProject>
# Creation : Aout 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder l'etat de l'outils dans un projet SPI.
#
# Parametres :
#   <File>   : Descripteur de fichier ou ecrire les commandes
#
# Remarques :
#    - Le fichier est deja ouvert, il suffit d'y ecrire les commandes a executer
#      afin de re-instaurer l'outils dans son etat actuel.
#
#-------------------------------------------------------------------------------

proc Mapper::AsProject { File } {
   variable Data
   variable Param

   if { [winfo exists .mapper] } {
      puts $File "#----- Tool: Mapper\n"
      puts $File "set Mapper::Param(Dock)   $Param(Dock)"
      puts $File "set Mapper::Param(Geom)   [winfo geometry .mapper]"
      puts $File "Mapper::Window\n"

      if { [winfo exists .mapperparams] } {
         puts $File "Mapper::Params True $Mapper::Data(Object)"
         puts $File "wm geometry .mapperparams =[wm geometry .mapperparams]"
         puts $File "TabFrame::Select .mapperparams.tab [TabFrame::Current .mapperparams.tab]\n"
      }
   }
}

proc Mapper::AsProjectPerPage { File Page { Params True } } {
   variable Data

   foreach id $Viewport::Data(Data$Page) {
      if { [gdalband is $id] } {
         set bands {}
         foreach band { 0 1 2 3 } {
            if  { $Data(Band$band$id)!="" } {
               lappend bands [list "" [lindex $Data(Band$band$id) 1] [lindex $Data(Band$band$id) 2] [lindex $Data(Band$band$id) 3] [lindex $Data(Band$band$id) 4]]
            } else {
               break
            }
         }
         puts $File "   set band \[Mapper::GDAL::Read [gdalfile filename [gdalband define $id -fid]] \{ $bands \}\]"

         if { $Params } {
            puts $File "   gdalband configure \$band -dataspec [gdalband configure $id -dataspec]"
         }
      } elseif { [ogrlayer is $id] } {
         puts $File "   set layer \[Mapper::OGR::Read [ogrfile filename [ogrlayer define $id -fid]]\]"

         if { $Params } {
            puts $File "   ogrlayer configure \$layer -dataspec [ogrlayer configure $id -dataspec]"
         }
      }
   }

   if { [llength $Viewport::Data(Data$Page)] } {
      puts $File "   Mapper::UpdateData $Page"
   }
}
