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

source $GDefs(Dir)/Apps/Tools/Mapper/Mapper.ctes
source $GDefs(Dir)/Apps/Tools/Mapper/Mapper.txt
source $GDefs(Dir)/Apps/Tools/Mapper/Mapper.int
source $GDefs(Dir)/Apps/Tools/Mapper/Mapper_Geo.tcl
source $GDefs(Dir)/Apps/Tools/Mapper/Mapper_GeoLocator.tcl
source $GDefs(Dir)/Apps/Tools/Mapper/Mapper_Cutter.tcl
source $GDefs(Dir)/Apps/Tools/Mapper/Mapper_WKT.tcl
source $GDefs(Dir)/Apps/Tools/Mapper/Mapper_DepotWare.tcl

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

   foreach frame $Page::Data(Frames) {
      set lst {}
      if { [info exist Viewport::Data(Data$frame)] && [llength $Viewport::Data(Data$frame)] } {
         foreach object $Viewport::Data(Data$frame) {
            if { [gdalband is $object] && [info exists Mapper::Data(Id$object)] } {
               gdalband free $object
               gdalfile close $Data(Id$object)
               ogrgeometry free MASK$object  MASKRING$object
            } elseif { [ogrlayer is $object] && [info exists Mapper::Data(Id$object)] } {
               ogrlayer free $object
               ogrfile close $Data(Id$object)
            } elseif { [model is $object] } {
               model free $object
            } else {
               lappend lst $object
            }
            if { [colormap is $object] } {
               colormap free $object
            }
         }

         #----- Update de la projection

         set Viewport::Data(Data$frame) {}
         Mapper::UpdateData $frame
      }
   }

   if { [winfo exists $Data(Canvas)]} {
      $Data(Canvas) delete MAPPERSEARCH MAPPERCUTTER MAPPERGEOLOCATOR
   }

   if { [winfo exists .mapperparams] } {
        Mapper::ParamsClose
   }

   Viewport::FollowerRemove Mapper
   Mapper::DepotWare::CacheClean

   destroy .mapper

   if { !$SPI::Param(Window) } { SPI::Quit }
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
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::Del { } {
   variable Data

   if { [set idx [$Data(Tab1).select.list curselection]]!="" } {
      set object [$Data(Tab1).select.list get $idx]

      set Viewport::Data(Data$Page::Data(Frame)) [lreplace $Viewport::Data(Data$Page::Data(Frame)) $idx $idx]
      Mapper::UpdateData $Page::Data(Frame)

      if { [gdalband is $object] } {
         gdalband free $object
         gdalfile close $Data(Id$object)
         ogrgeometry free MASK$object  MASKRING$object 
      } elseif { [ogrlayer is $object] } {
         ogrlayer free $object
         ogrfile close $Data(Id$object)
      } elseif { [model is $object] } {
         model free $object
      }
      if { [colormap is $object] } {
         colormap free $object
      }

      if { $object==$Data(Object) } {
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
          ANY  { if { [Mapper::ReadBand $file "" 3 $Full]=="" && [Mapper::ReadLayer $file]=="" } {
                    Dialog::Error . $Msg(BadFile)
                 }
               }
          GDAL { if { [Mapper::ReadBand $file "" 3 $Full]=="" } {
                    Dialog::Error . $Msg(BadFile)
                 }
               }
          OGR  { if { [Mapper::ReadLayer $file]=="" } {
                    Dialog::Error . $Msg(BadFile)
                 }
               }
      }
      Mapper::UpdateData $Page::Data(Frame)
   }
   Mapper::Cursor left_ptr
}

proc Mapper::ReadBand { File { Bands "" } { Nb 3 } { Full False } } {
   global GDefs errorInfo
   variable Data
   variable Msg

   #---- If an id is passed, use it
   if  { [info exists Data(Id$File)] } {
      set id $File
   } else {
      set id [file tail [file rootname $File]]
      set no 1
      while { [gdalband is $id] } {
         set id $id$no
         incr no
      }

      if  { ![info exists Data(Id$id)] } {
         set Data(Id$id) GDAL[incr Data(IdNo)]
      }

      gdalfile close $Data(Id$id)
      set bands {}
      eval set bad [catch { set bands [gdalfile open $Data(Id$id) read $File] }]

      if { $bad || ![llength $bands] } {
         return ""
      }
      set Data(Band$id) $bands
   }

   set Data(Job)   [lindex $Msg(Read) $GDefs(Lang)]
   update idletasks;

   if { ![llength $Bands] } {
      set interp [gdalfile colorinterp $Data(Id$id)]
      if { $interp=="Red"  } {
         set Bands [lrange $bands 0 $Nb]
         set Data(Interp) LINEAR
      } else {
         set Data(Interp) NEAREST
         set Bands [list [lindex $bands 0]]
      }
   }

   #----- Check for generic naming
   for { set i 0 } { $i<[llength $Bands] } { incr i } {
      if { [lindex [lindex $Bands $i] 0]=="" } {
         lset Bands $i 0 $Data(Id$id)
      }
   }

   set Data(Band0$id) ""
   set Data(Band1$id) ""
   set Data(Band2$id) ""
   set Data(Band3$id) ""
   set Data(Bands$id) {}
   set Data(BandX$id) ""
   set Data(BandY$id) ""

   if  { [llength $Bands]>=1 } {
      set Data(Band0$id) [lindex $Bands 0]
      lappend Data(Bands$id) red
   }
   if  { [llength $Bands]>=2 } {
      set Data(Band1$id) [lindex $Bands 1]
      lappend Data(Bands$id) green
   }
   if  { [llength $Bands]>=3 } {
      set Data(Band2$id) [lindex $Bands 2]
      lappend Data(Bands$id) blue
   }
   if  { [llength $Bands]>=4 } {
      set Data(Band3$id) [lindex $Bands 3]
      lappend Data(Bands$id) alpha
   }

   set er [catch { gdalband read $id $Bands $Full } errmsg]

   if { $er } {
      error $errmsg $errorInfo
   }

   set map [gdalband configure $id -colormap]
   gdalband configure $id -interpolation $Data(Interp)

   foreach min [gdalband stats $id -min] band $Data(Bands$id) {
      colormap configure $map -min $band [lindex $min 0]
   }
   foreach max [gdalband stats $id -max] band $Data(Bands$id) {
      colormap configure $map -max $band [lindex $max 0]
  }

   set Data(Job) ""

   if { [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $id]==-1 } {
      lappend Viewport::Data(Data$Page::Data(Frame)) $id
   }

   return $id
}

proc Mapper::ReadPos { Id BandX BandY } {
   global GDefs errorInfo
   variable Data
   variable Msg

   if  { ![info exists Data(Id$Id)] } {
      return
   }

   set Data(Job)   [lindex $Msg(Read) $GDefs(Lang)]
   update idletasks;

   set err [catch { gdalband read BandX$Id [list $BandX] } errmsg ]
   if { $err } {
      error $errmsg $errorInfo
   }

   set err [catch { gdalband read BandY$Id [list $BandY] } errmsg ]
   if { $err } {
      error $errmsg $errorInfo
   }

   gdalband define $Id -positional BandX$Id BandY$Id

   set Data(Proj)       [gdalband define $Id -projection]
   set Data(Trans)      [gdalband define $Id -transform]
   set Data(InvTrans)   [gdalband define $Id -invtransform]

   set Data(BandX$Id) $BandX
   set Data(BandY$Id) $BandY
   set Data(Job) ""
}

proc Mapper::ReadLayer { File { Index {} } { SQL "" } } {
   global GDefs
   variable Msg
   variable Data

   if  { ![info exists Data(Id$File)] } {
      set Data(Id$File) OGR[incr Data(IdNo)]
   }

   eval set bad [catch { set idxs [ogrfile open $Data(Id$File) read $File] }]

   if { $bad } {
      return ""
   }

   set Data(Job)   [lindex $Msg(Read) $GDefs(Lang)]
   update idletasks;

   #----- If a layer index has been specified, use it
   if { [llength $Index] } {
      set idxs {}
      foreach idx $Index {
         lappend idxs [list $Data(Id$File) [lindex $idx 0] [lindex $idx 1]]
      }
   }

   foreach idx $idxs {
      set layer [lindex $idx 2]
      if { ![ogrlayer is $layer] } {
         if { $SQL!="" } {
            ogrlayer sqlselect $layer $File $SQL
         } else {
            eval ogrlayer read \$layer $idx
         }

         if { [ogrlayer define $layer -nb]==0 } {
            Dialog::Error . $Msg(NoFeature) $layer
            ogrlayer free $layer
            ogrfile close $File
            continue
         }
         ogrlayer configure $layer -font OGRFONT -activeoutline yellow -width 1

         if { [ogrlayer define $layer -space]==2 } {
            ogrlayer configure $layer -outline black -fill [Mapper::GetColor]
         } else {
            ogrlayer configure $layer -outline [Mapper::GetColor]
         }

         if { [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $layer]==-1 } {
            lappend Viewport::Data(Data$Page::Data(Frame)) $layer
         }
         set Data(Id$layer) $File
      }
      if { ![colormap is $layer] } {
         colormap create $layer
         colormap copy $layer OGRMAPDEFAULT
         ogrlayer configure $layer -colormap $layer
      }
   }
   set Data(Job) ""
   Mapper::Progress $layer

   return $layer
}

proc Mapper::Progress { Object } {
   global GDefs
   variable Lbl

   if { [ogrlayer is $Object] } {
      set ready [ogrlayer define $Object -nbready]
      set total [ogrlayer define $Object -nb]

      if { $ready!=$total } {
         set pc [expr double($ready)/$total*100.0]
         SPI::Progress $pc "[lindex $Lbl(Process) $GDefs(Lang)] $Object [format "%.1f" ${pc}]%"
         after 100 Mapper::Progress \"$Object\"
         return
      }
   }
   SPI::Progress 0 ""
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::ParamsGDALGet>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les parametres de donnees raster GDAL
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::ParamsGDALGet { Object } {
   global   GDefs
   variable Data

   set Data(Band)       red
   set Data(Sample)     [gdalband configure $Object -texsample]
   set Data(Texture)    [gdalband configure $Object -texsize]
   set Data(Resolution) [gdalband configure $Object -texres]
   set Data(Tran)       [gdalband configure $Object -transparency]
   set Data(Interp)     [gdalband configure $Object -interpolation]
   set Data(Topo)       [gdalband configure $Object -topography]
   set Data(TopoFactor) [gdalband configure $Object -topographyfactor]
   set Data(Mask)       [gdalband configure $Object -mask]
   set Data(ColorMap)   [gdalband configure $Object -colormap]
   set Data(Proj)       [gdalband define $Object -projection]
   set Data(Trans)      [gdalband define $Object -transform]
   set Data(InvTrans)   [gdalband define $Object -invtransform]
   set Data(NoData)     [gdalband stats $Object -nodata]

   if { $Data(Mask)!="" } {
      set Data(Cut) True
   }

   set Data(Meta)     [join [gdalfile metadata $Data(Id$Object)] \n]

   set Data(Red)   $Data(Band0$Object)
   set Data(Green) $Data(Band1$Object)
   set Data(Blue)  $Data(Band2$Object)
   set Data(Alpha) $Data(Band3$Object)
   set Data(BandX) $Data(BandX$Object)
   set Data(BandY) $Data(BandY$Object)

   Mapper::UpdateItems $Data(Frame)
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::ParamsGDALSet>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Parametrer un objet de type Model
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::ParamsGDALSet { Object { CheckData True } } {
   variable Data

   if { $Data(Init) } {
      return
   }

   Mapper::Cursor watch

   if { $CheckData } {
      if { $Data(Band0$Object)!=$Data(Red) || $Data(Band1$Object)!=$Data(Green) || $Data(Band2$Object)!=$Data(Blue) || $Data(Band3$Object)!=$Data(Alpha) } {
         Mapper::ReadBand $Object [list $Data(Red) $Data(Green) $Data(Blue) $Data(Alpha)]
      }

      if { $Data(BandX$Object)!=$Data(BandX) || $Data(BandY$Object)!=$Data(BandY) } {
         Mapper::ReadPos $Object $Data(BandX) $Data(BandY)
      }
   }

   gdalband configure $Object -texsample $Data(Sample) -texres $Data(Resolution) -texsize $Data(Texture) -transparency $Data(Tran) \
      -interpolation $Data(Interp) -topography $Data(Topo) -topographyfactor $Data(TopoFactor) -font XFont12
   gdalband stats $Object -nodata $Data(NoData)

   if { [ogrgeometry is MASK$Object] } {
      gdalband configure $Object -mask MASK$Object
   } else {
      gdalband configure $Object -mask ""
   }

   if { [winfo exists .mapper] } {
      set Data(Proj) [string trim [$Data(Frame1).proj.val get 0.0 end] "\n"]
      gdalband define $Object -projection $Data(Proj) -transform $Data(Trans)
   #   gdalband define $Object -invtransform $Data(InvTrans)

      Page::Update $Page::Data(Frame)
      Mapper::CurveDefine $Object $Data(Bands$Object)
   }

   Mapper::Cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::ParamsModelGet>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les parametres du modele
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::ParamsModelGet { Object } {
   variable Data

   set Data(Proj)    [model define $Object -projection]
   set Data(Width)   [model configure $Object -width]
   set Data(Color)   [model configure $Object -outline]
   set Data(Dash)    [model configure $Object -dash]
   set Data(Texture) [model configure $Object -rendertexture]
   set Data(Light)   [model configure $Object -light]

   set tmp [model matrix $Object -locate]
   set Data(Lat) [lindex $tmp 0]
   set Data(Lon) [lindex $tmp 1]
   set Data(Ele) [lindex $tmp 2]

   set tmp [model matrix $Object -rotate]
   set Data(RX) [lindex $tmp 0]
   set Data(RY) [lindex $tmp 1]
   set Data(RZ) [lindex $tmp 2]

   set tmp [model matrix $Object -scale]
   set Data(SX) [lindex $tmp 0]
   set Data(SY) [lindex $tmp 1]
   set Data(SZ) [lindex $tmp 2]

   set Data(Ambi) [model material $Object -ambient]
   set Data(Emis) [model material $Object -emissive]
   set Data(Diff) [model material $Object -diffuse]
   set Data(Spec) [model material $Object -specular]

   #----- There might be no material and if so, those scale bound variables won't be happy
   catch { set Data(Shin) [model material $Object -shininess] }
   catch { set Data(Tram) [model material $Object -transparency] }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::ParamsModelSet>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Parametrer un objet de type Model
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::ParamsModelSet { Object } {
   variable Data

   if { $Data(Init) } {
      return
   }
   Mapper::Cursor watch

   set Data(Proj) [string trim [$Data(Frame1).proj.val get 0.0 end] "\n"]
   model define $Object -projection $Data(Proj)
   model configure $Object -width $Data(Width)
   model configure $Object -outline $Data(Color)
   model configure $Object -dash $Data(Dash)
   model configure $Object -rendertexture $Data(Texture)
   model configure $Object -light $Data(Light)

   model matrix $Object -locate $Data(Lat) $Data(Lon) $Data(Ele)
   model matrix $Object -rotate $Data(RX) $Data(RY) $Data(RZ)
   model matrix $Object -scale $Data(SX) $Data(SY) $Data(SZ)

   model material  $Object -ambient $Data(Ambi)
   model material  $Object -emissive $Data(Emis)
   model material  $Object -diffuse $Data(Diff)
   model material  $Object -specular $Data(Spec)
   model material  $Object -shininess $Data(Shin)
   model material  $Object -transparency $Data(Tram)

   Page::Update $Page::Data(Frame)

   Mapper::Cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::ParamsOGRGet>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les parametres de donnees vectorielle OGR
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::ParamsOGRGet { Object } {
   global   GDefs
   variable Data

   set Data(Fill) black

#   set Data(Font)        [ogrlayer configure $Object -font]
   set Data(Dash)        [ogrlayer configure $Object -dash]
   set Data(Icon)        [ogrlayer configure $Object -icon]
   set Data(Size)        [ogrlayer configure $Object -size]
   set Data(LabelVar)    [ogrlayer configure $Object -labelvar]
   set Data(SizeVar)     [ogrlayer configure $Object -sizevar]
   set Data(MapVar)      [ogrlayer configure $Object -mapvar]
   set Data(Topo)        [ogrlayer configure $Object -topography]
   set Data(TopoFactor)  [ogrlayer configure $Object -topographyfactor]
   set Data(Extr)        [ogrlayer configure $Object -extrude]
   set Data(ExtrFactor)  [ogrlayer configure $Object -extrudefactor]
   set Data(ColorMap)    [ogrlayer configure $Object -colormap]
   set Data(Color)       [ogrlayer configure $Object -outline]
   set Data(Highlight)   [ogrlayer configure $Object -activeoutline]
   set Data(Width)       [ogrlayer configure $Object -width]
   if { $Data(Width)<0 } {
      set Data(Burn) -1
      set Data(Width) [expr -$Data(Width)]
   } else {
      set Data(Burn) 1
   }
   set Data(Tran)        [ogrlayer configure $Object -transparency]
   set Data(Proj)        [ogrlayer define $Object -projection]
   set Data(Mask)        [ogrlayer define $Object -mask]
   set Data(Fields)      [ogrlayer define $Object -field]

   set value             [ogrlayer configure $Object -value]
   set Data(Order)       [lindex $value 0]
   set Data(Mantisse)    [lindex $value 1]

    set fill              [ogrlayer configure $Object -fill]
#   set Data(Stipple)     [ogrlayer configure $Object -stipple]

   if { $fill!="" } {
      set Data(FillSel) 1
      set Data(Fill)    $fill
   } else {
      set Data(FillSel) 0
   }

   set Data(Intervals)   [ogrlayer configure $Object -intervals]
   set Data(Min)         [ogrlayer configure $Object -min]
   set Data(Max)         [ogrlayer configure $Object -max]

   if { $Data(Min)!=$Data(Max) } {
      set Data(Intervals) ""
      if { $Data(Min)!="" } {
         append Data(Intervals) "\[$Data(Min)"
      }
      if { $Data(Max)!="" } {
         append Data(Intervals) " $Data(Max)\]"
      }
   }

   if { [llength [set interlabels [ogrlayer configure $Object -interlabels]]] } {
      set inters $Data(Intervals)
      set Data(Intervals) ""
      foreach label $interlabels inter $inters {
         append Data(Intervals) "$inter ($label) "
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::ParamsOGRSet>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Parametrer un objet de donnees vectorielle OGR
#
# Parametres :
#   <Object> : Donnee geographique a parametrer
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::ParamsOGRSet { Object } {
   variable Data

   if { $Data(Init) } {
      return
   }

   Mapper::Cursor watch

   #----- Verifier pour un range plutot que des niveaux

   set inter $Data(Intervals)
   set label {}
   set min   ""
   set max   ""

   if { [set from [string first "\[" $Data(Intervals)]]!=-1 } {
      set min [lindex [string range $Data(Intervals) [incr from] end] 0]
      set inter {}
   }

   if { [set to [string first "\]" $Data(Intervals)]]!=-1 } {
      set max [lindex [string range $Data(Intervals) 0 [incr to -1]] end]
      set inter {}
   }

   if { [string first "(" $Data(Intervals)]!=-1 } {
      set inter {}
      foreach { val } [split $Data(Intervals) )] {
         if { [llength [set val [split $val (]]]>1 } {
            lappend inter [lindex $val 0]
            lappend label [lindex $val 1]
         }
      }
   }

#   ogrlayer configure $Object -stipple $Data(Stipple)
#   ogrlayer configure $Object -font OGRFONT

   ogrlayer configure $Object -dash $Data(Dash) -colormap $Data(ColorMap) -outline $Data(Color) -activeoutline $Data(Highlight) \
      -icon $Data(Icon) -size $Data(Size) -sizevar $Data(SizeVar) -mapvar $Data(MapVar) -width [expr $Data(Width)*$Data(Burn)] -transparency $Data(Tran) \
      -min $min -max $max -intervals $inter -interlabels $label -value $Data(Order) $Data(Mantisse) -topography $Data(Topo) -topographyfactor $Data(TopoFactor) \
      -extrude $Data(Extr) -extrudefactor $Data(ExtrFactor) -labelvar $Data(LabelVar)

   if { $Data(FillSel) } {
      ogrlayer configure $Object -fill $Data(Fill)
   } else {
      ogrlayer configure $Object -fill ""
   }

   ogrlayer define $Object -mask $Data(Mask)

   if { [winfo exists .mapper] } {
      set Data(Proj) [string trim [$Data(Frame1).proj.val get 0.0 end] "\n"]
      ogrlayer define $Object -projection $Data(Proj)

      Mapper::SelectOGRApply $Object
   }

   Page::Update     $Page::Data(Frame)
   ColorBar::Update $Page::Data(Frame)
   Mapper::Progress $Object
   Mapper::Cursor left_ptr
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

proc Mapper::PickOGR { VP X Y } {
   global GDefs
   variable Data
   variable Msg
   variable Lbl

   if { !$Data(PickSize) } {
      #----- Pixel pick
      set co0 [$Viewport::Data(VP) -unproject $X $Y]
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
      if { [ogrlayer is $object] } {
         if { [set idx [lindex [ogrlayer pick $object $coords $Data(PickAll)] 0]]!="" } {

            set Data(Index) $idx
            set Data(Object) $object
            Mapper::ParamsOGRGet $Data(Object)
            Mapper::ParamsOGR $Data(Object) 2
            Mapper::TableOGR  $Data(Object) $idx

            #----- Check if this is an index to some other data
            if { ![catch { set files [ogrlayer define $object -feature $Data(Index) IDX_PATH]}] } {
               if { $files!="" } {
                  if { ![Dialog::Default . 400 INFO $Msg(Index) "\n\n$files\n" 0 $Lbl(Yes) $Lbl(No)] } {
                     foreach file $files {
                        set path [file dirname $Data(Id$object)]
                        set file $path/../$file
                        set band [Mapper::ReadBand $file]
                        ogrgeometry copy MASK$band [ogrlayer define $object -geometry $Data(Index)]
                        ogrgeometry stats MASK$band -transform [gdalband define $band -georef]
                        gdalband configure $band -mask MASK$band

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
      }
   }
}

proc Mapper::DrawInit  { Frame VP } {
   global GDefs
   variable Data
   variable Lbl

   set Data(InfoId)  ""
   set Data(InfoObs) ""

   Mapper::PickOGR $VP $Viewport::Map(X) $Viewport::Map(Y)
}

proc Mapper::Draw     { Frame VP } {
}

proc Mapper::DrawDone { Frame VP } {
}

proc Mapper::MoveInit { Frame VP } {
   variable Data

   set Data(LonD) $Viewport::Map(LonCursor)
   set Data(LatD) $Viewport::Map(LatCursor)
}

proc Mapper::Move { Frame VP } {
}

proc Mapper::MoveDone { Frame VP } {
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
         append license "$License($data)\n"
      }
   }

   if { [projection is $Frame ] } {
      projection configure $Frame -data $Viewport::Data(Data) -license $license
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

proc Mapper::UpdateItems { Frame } {
   global GDefs
   variable Data

   if { ![llength [info commands $Data(VP)]] } {
      set Data(VP) ""
   }

   if { $Frame==$Data(Frame) && $Data(VP)!="" } {

      #----- Canvas might not exist anymore so catch this call
      catch { $Data(Canvas) delete MAPPERSEARCH MAPPERCUTTER MAPPERGEOLOCATOR }
      if { $Mapper::DepotWare::Data(Coo)!="" } {
         Viewport::DrawRange $Frame $Data(VP) $Mapper::DepotWare::Data(Lat0) $Mapper::DepotWare::Data(Lon0) $Mapper::DepotWare::Data(Lat1) $Mapper::DepotWare::Data(Lon1) MAPPERSEARCH red
      }

      if { $Data(CutShow) && [info exists Data(Mask$Data(Object))] && [llength $Data(Mask$Data(Object))] } {
         set tmp $Data(Mask$Data(Object))
         lappend tmp [lindex $tmp 0] [lindex $tmp 1] 0

         Viewport::DrawLine $Frame $Data(VP) $tmp "$Page::Data(Tag)$Data(VP) MAPPERCUTTER" red 2
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
         puts $File "Mapper::Params True [$Data(Tab1).select.list curselection]"
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
         puts $File "   set band \[Mapper::ReadBand [gdalfile filename [gdalband define $id -fid]] \{ $bands \}\]"

         if { $Params } {
            puts $File "   gdalband configure \$band -dataspec [gdalband configure $id -dataspec]"
         }
      } elseif { [ogrlayer is $id] } {
         puts $File "   set layer \[Mapper::ReadLayer [ogrfile filename [ogrlayer define $id -fid]]\]"

         if { $Params } {
            puts $File "   ogrlayer configure \$layer -dataspec [ogrlayer configure $id -dataspec]"
         }
      }
   }

   if { [llength $Viewport::Data(Data$Page)] } {
      puts $File "   Mapper::UpdateData $Page"
   }
}
