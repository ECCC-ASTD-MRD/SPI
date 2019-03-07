#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Package d'interface pour SPI
# Fichier  : Grid.tcl
# Creation : Juin 2003
#
# Description:
#    Coquille vide demontrant les points d'entree pour la creation de nouveaux outils
#    a l'interface SPI. Ces fichiers representent la structure standard de SPI. Une
#    fonctionnalite de base de selection est implantee pour fin de demonstration.
#
#    Toutes les fonctions decrites sont le minimum necessaire au fonctionnement d'un
#    outils a travers l'interface SPI
#
#    Pour creer un nouvel outil, il suffit de renommer ces fichiers (tcl,int,txt,ctes) au nom
#    de l'outils que vous desirez et de remplacer "Grid" et "gridmaker" par le
#    meme nom.
#
#    Par la suite il suffit d'inserer la ligne suivante dans le fichier $HOME/.spi/SPI
#
#       SPI::ToolDef <path>/Grid.tcl
#
#    et de modifier les references au <path> dans les 3 lignes ci-bas pour sourcer le tout.
#
#===============================================================================

package require Grid 2.0

#----- Lire les sources d'execution

source $GDefs(Dir)/tcl/Tools/Grid/Grid.ctes
source $GDefs(Dir)/tcl/Tools/Grid/Grid.txt
source $GDefs(Dir)/tcl/Tools/Grid/Grid.int

#-------------------------------------------------------------------------------
# Nom      : <Grid::Close>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ferme l'interface de l'outil.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Grid::Close { } {
   variable Data

   #----- Si le mode etait celui de l'outils, revert to SPI
   if { $Page::Data(ToolMode)=="Grid" } {
      SPI::ToolMode SPI Zoom
   }

   #----- Cleanup de l'outils
   set Data(Active) 0

   $Data(Canvas) delete GRIDMAKER

   Viewport::UnAssign $Data(Frame) $Data(VP)

   destroy .gridmaker

   if { !$SPI::Param(Window) } { SPI::Quit }
}

proc Grid::Switch { } {
   variable Param
   variable Data
  
   array set Grid::Param [lindex $Data(GridParams) $Data(GridNo)]
   set Data(GridId) MODELGRID$Data(GridNo)
   set Data(GridDepend) $Data(GridNo)
   set Param(LockCenter) [expr $Data(GridNo)>0?True:False]
   
   Grid::WindowSet $Data(Tab).grid
}

proc Grid::Add { } {
   variable Param
   variable Data
   variable Msg
   
   if  { $Data(GridNo)==0 && !$Param(NI) } {
      Dialog::Error . $Grid::Msg(GridAdd)
      return
   }
   
   set Data(GridNo) [llength $Data(GridParams)]
   set Data(GridId) MODELGRID$Data(GridNo)
   lappend Data(GridParams) [array get Grid::Param]

   set Param(LockCenter) [expr $Data(GridNo)>0?True:False]
   
   for { set i 0 } { $i<[llength $Data(GridParams)] } { incr i } { lappend grids $i }
   Option::Set $Data(Tab).grid.sel.no $grids
   $Data(Tab).grid.sel.no.b.m invoke end
   
   Grid::Create $Data(GridId)
}

proc Grid::Del { { All False } } {
   variable Param
   variable Data
   
   if { $All } {
      set Data(GridParams) {}
      set Data(GridNo) 0
      Viewport::UnAssign $Data(Frame) $Data(VP)
      Option::Set $Data(Tab).grid.sel.no {}
   } elseif { $Data(GridNo) && [llength $Data(GridParams)]>1 } {
      Viewport::UnAssign $Data(Frame) $Data(VP) $Data(GridId)
      set Data(GridParams) [lreplace $Data(GridParams) $Data(GridNo) $Data(GridNo)]

      for { set i 0 } { $i<[llength $Data(GridParams)] } { incr i } { lappend grids $i }
      Option::Set $Data(Tab).grid.sel.no $grids
      $Data(Tab).grid.sel.no.b.m invoke end     
   }
}

proc Grid::ConfigSet { } {
   variable Param
   variable Data

   set texture 0
   set grid    0
   
   switch $Data(GridSize) {
      7 { set Data(GridBoundary) 0; set texture 1 }
      6 { set Data(GridBoundary) 1 }
      default { set Data(GridBoundary) 0; set grid $Data(GridSize) }
   }

   lset Data(GridParams) $Data(GridNo) [array get Grid::Param]
   
   for { set no 0 } { $no<[llength $Data(GridParams)] } { incr no } {
      if { [fstdfield is MODELGRID$no] } {
#         fstdfield configure MODELGRID$no -rendergrid $grid -renderboundary $Data(GridBoundary) \
#            -width 2 -color $Data(Color$no) -colormap GRID$no -mapall True -rendertexture $texture -interpdegree NEAREST
         fstdfield configure MODELGRID$no -renderparticle $grid -renderboundary $Data(GridBoundary) \
            -width 2 -color $Data(Color$no) -colormap GRID$no -rendertexture $texture -interpdegree NEAREST
      }
   }
   Viewport::UpdateData $Data(Frame)        
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Apply>
# Creation : November 2018 - J.P. Gauthier - CMC/CMOE
#
# But      : Apply parameters to the grid and it's dependencies
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Grid::Apply { } {
   variable Data

   Grid::Create $Grid::Data(GridId)
   Grid::Cascade
   Grid::SettingsShow
}

proc Grid::Cascade { } {
   variable Param
   variable Data
  
   if { [llength $Data(GridParams)]==0 || $Param(Type)!="ZE" } {
      return
   }
   
   #----- Loop on cascaded grids
   set pno $Data(GridNo)
   set latm $Data(LatM)
   set lonm $Data(LonM)
   set Data(LatM) 0
   set Data(LonM) 0
   for { set no [expr $pno+1] } { $no<[llength $Data(GridParams)] } { incr no } {
      array set gridp [lindex $Data(GridParams) [expr $no-1]]
      array set Grid::Param [lindex $Data(GridParams) $no]
      set Param(XLat1) $gridp(XLat1)
      set Param(XLon1) $gridp(XLon1)
      set Param(Angle) $gridp(Angle)
      
      set Data(GridNo) $no
      Grid::Create MODELGRID$no
   }
   set Data(LatM) $latm
   set Data(LonM) $lonm
   set Data(GridNo) $pno
   array set Grid::Param [lindex $Data(GridParams) $Data(GridNo)]
}

#----------------------------------------------------------------------------
# Nom      : <Grid::SettingsRead>
# Creation : Novembre 2018 - J.P. Gauthier - CMC/CMOE
#
# But      : Read a gem settings file 
#
# Parametres :
#    <File>  : Path to gem settings file
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::SettingsRead { File } {
   variable Settings

   if { $File=="" } {
      return
   }

   if { ![file exists $File] } {
      Log::Print WARNING "Could not find the namelist $File"
      return
   }
   
   array unset Settings

   set f [open $File r]
   while { ![eof $f] } {
      gets $f line

      #----- Check for block beginning
      set tok [string index [string trimleft $line] 0]
      if { $tok=="&" || $tok=="$" } {
         gets $f line

         #----- While not at a block end
         set char [string index [string trimleft $line] 0]
         while { $char!="/" && $char!="$" && $char!="&" && ![eof $f] } {

            #----- Insert all settings in Settings array
            foreach item [split $line ,] {
               if { [string trim $item]!="" } {

                  #----- Get the token name if not an array of values
                  if { [llength [set item [split $item =]]]==2 } {
                     set token [string toupper [string trim [lindex $item 0]]]
                  }
                  lappend Settings($token) [string map -nocase { "'" "" ".true." "True" .false. "False" } [string trim [lindex $item end]]]
               }
            }
            gets $f line
            set char [string index [string trimleft $line] 0]
         }
      }
   }
   close $f
}

#----------------------------------------------------------------------------
# Nom      : <Grid::SettingsBuild>
# Creation : Novembre 2018 - J.P. Gauthier - CMC/CMOE
#
# But      : Build the gem settings strings for the Grd and optional Grdc 
#
# Parametres :
#    <Params>: Grid parameters
#    <C>     : Generate Grdc string
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::SettingsBuild { Params { C False } } {
   
   array set param $Params
   switch $param(Type) {
         "ZE"    { if { $C } {
                      return [format "/
  &grdc
  Grdc_ni     = %i, Grdc_nj     = %i,
  Grdc_dx     = %.6f, Grdc_dy     = %.6f,
  Grdc_lonr   = %9.4f, Grdc_latr  = %8.4f,
  Grdc_maxcfl = %i, Grdc_nbits  = 32, Grdc_nfe    = CASC_NESDT\n" \
         $param(RNI) $param(RNJ) $param(ResLLX) $param(ResLLY) $param(LonR) $param(LatR) $param(MaxCFL)]
                   } else {      
                      return [format "&grid
  Grd_typ_S  = 'LU',
  Grd_ni     = %i, Grd_nj     = %i,
  Grd_dx     = %.6f, Grd_dy     = %.6f,
  Grd_lonr   = %9.4f, Grd_latr  = %8.4f,
  Grd_xlon1  = %9.4f, Grd_xlat1 = %8.4f,
  Grd_xlon2  = %9.4f, Grd_xlat2 = %8.4f,
  Grd_maxcfl = %i\n" \
         $param(RNI) $param(RNJ) $param(ResLLX) $param(ResLLY) $param(LonR) $param(LatR) $param(XLon1) $param(XLat1) $param(XLon2) $param(XLat2) $param(MaxCFL)]
                   }
                 }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Grid::SettingsShow>
# Creation : Novembre 2018 - J.P. Gauthier - CMC/CMOE
#
# But      : Generate the gem settings and show them in the interface 
#
# Parametres :
#    <Path>  : Tree path
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::SettingsShow { } {
   variable Param
   variable Data
   
   $Data(Tab).settings.text delete 0.0 end

   foreach grid $Data(GridParams) {
      catch { $Data(Tab).settings.text insert end [Grid::SettingsBuild $grid]\n }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Grid::ProjectSave>
# Creation : Novembre 2018 - J.P. Gauthier - CMC/CMOE
#
# But      : Save grid information and GenPhysX job to a modeling tree 
#
# Parametres :
#    <Path>  : Tree path
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::ProjectSave { Path } {
   variable Data
   variable GenPhysX
   variable Msg

   if { $Path=="" } {
      Dialog::Error .gridmaker $Msg(SavePath)
      return False
   }
   
   set no    0
   set gridc {}
   
   foreach grid $Data(GridParams) {
      array set param $grid
   
      #----- Figure out the extension in km (ex: 1p0 2p5 0p25)
      set resk [format "%.2f" [expr $param(ResMX)/1000.0]]
      if { [expr $resk-floor($resk)]==0 } {
         set res [expr int(floor($resk))]p0
      } else {
         set res [string trimright [string map { . p } $resk] 0]
      }
      file mkdir $Path/$res/
      
      #----- Write namelist grid and gridc
      exec echo [Grid::SettingsBuild $grid] > $Path/$res/gem_settings.nml
      if { [llength $gridc] } {
         exec echo [Grid::SettingsBuild $gridc True] >> $Path/$res/gem_settings.nml     
      }
      set gridc $grid
      
      #----- Write RPN grid file
      file delete -force $Path/$res/grid.fstd 
      fstdfile open FILE write $Path/$res/grid.fstd 
      Grid::Write FILE MODELGRID$no
      fstdfile close FILE

      #----- Create GenphysX job file
      set f [open $Path/$res/Gem_geophy.sh w 0755]
      puts $f "#!/bin/bash"
#TODO      puts $f ". ssmuse-sh -x eccc/cmd/cmds/apps/SPI/beta"
      puts $f "GenPhysX -gridfile $Path/$res/grid.fstd -target RELWS-1.0 -result $Path/$res/Gem_geophy -batch -mach $GenPhysX(Host) -t $GenPhysX(Time) -cm $GenPhysX(Memory) -cpus $GenPhysX(CPU)"
      close $f
      
      exec $Path/$res/Gem_geophy.sh &
      
      incr no
   } 
   return True
}

#----------------------------------------------------------------------------
# Nom      : <Grid::ProjectLoad>
# Creation : Novembre 2018 - J.P. Gauthier - CMC/CMOE
#
# But      : Load grid information from a modeling tree
#
# Parametres :
#    <Path>  : Tree path
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::ProjectLoad { Path } {
   variable Data
   variable Param
   variable Settings

   if { $Path=="" } {
      return
   }
   
   Grid::Del True
  
   foreach path [lsort -dictionary -increasing [glob -nocomplain $Path/*p*]] {
      Log::Print INFO "Loading $path"
      if { [file exists $path/gem_settings.nml] } {
         Grid::SettingsRead $path/gem_settings.nml
#???         Grd_iref   = 675      ,     Grd_jref  = 600

         set Param(Type)   ZE
         set Param(NI)     $Settings(GRD_NI)
         set Param(NJ)     $Settings(GRD_NJ)
         set Param(ResLLX) $Settings(GRD_DX)
         set Param(ResLLY) $Settings(GRD_DY)
         set Param(LonR)   $Settings(GRD_LONR)
         set Param(LatR)   $Settings(GRD_LATR)
         set Param(XLon1)  $Settings(GRD_XLON1)
         set Param(XLat1)  $Settings(GRD_XLAT1)
         set Param(XLon2)  $Settings(GRD_XLON2)
         set Param(XLat2)  $Settings(GRD_XLAT2)
         set Param(MaxCFL) $Settings(GRD_MAXCFL)
         
         set Param(ResMX) [expr int(round($Param(ResLLX)*$Param(LL2M)))]
         set Param(ResMY) [expr int(round($Param(ResLLY)*$Param(LL2M)))]
         set Param(Lat0)  [expr $Param(XLat1)-($Param(NJ)*$Param(ResLLY)*0.5)]
         set Param(Lon0)  [expr $Param(XLon1)-($Param(NI)*$Param(ResLLX)*0.5)]
         set Param(Lat1)  [expr $Param(XLat1)+($Param(NJ)*$Param(ResLLY)*0.5)]
         set Param(Lon1)  [expr $Param(XLon1)+($Param(NI)*$Param(ResLLX)*0.5)]

         Grid::Add
      }
   }
   
   #----- Select 1st grid to start with
   set Data(GridNo) 0
   Grid::Switch
   Grid::SettingsShow 
   
   Viewport::GoTo $Data(Frame) $Param(XLat1) $Param(XLon1)
}

#----------------------------------------------------------------------------
# Nom      : <Grid::...>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de creation et deplacement interactifs
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#   <VP>     : Identificateur de la vue
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::MoveInit { Canvas VP { Modifier "" } } {
   variable Data
   
   set Data(VP)     $VP
}

proc Grid::Move { Frame VP { Modifier "" } } {
   variable Sim
   variable Param
   variable Data

   set Data(VP)    $VP

   set Data(LatM) $Viewport::Map(LatD)
   set Data(LonM) $Viewport::Map(LonD)

   Grid::Apply 
}

proc Grid::MoveDone { Canvas VP { Modifier "" } } {
   variable Param
   variable Data
   
   set Data(VP)    $VP

   if { $Data(GridNo)>0 } {
      set Param(LatR) [expr $Param(LatR)+$Data(LatM)]
      set Param(LonR) [expr $Param(LonR)+$Data(LonM)]
   } else {
      set Param(Lat0) [expr $Param(Lat0)+$Data(LatM)]
      set Param(Lon0) [expr $Param(Lon0)+$Data(LonM)]
      set Param(Lat1) [expr $Param(Lat1)+$Data(LatM)]
      set Param(Lon1) [expr $Param(Lon1)+$Data(LonM)]
   }
  
   set Data(LatM) 0.0
   set Data(LonM) 0.0

   Grid::Create $Data(GridId) 
}

proc Grid::DrawInit { Canvas VP { Modifier "" } } {
   variable Param
   variable Data
   
   set Data(VP)     $VP

   set Param(Lat0) $Viewport::Map(LatCursor)
   set Param(Lon0) $Viewport::Map(LonCursor)
}

proc Grid::Draw     { Canvas VP { Modifier "" } } {
   variable Param
   variable Data
   
   set Data(VP)    $VP

   set Param(Lat1) $Viewport::Map(LatCursor)
   set Param(Lon1) $Viewport::Map(LonCursor)

   if { [lindex $Param(Type) 0]=="PS" || [lindex $Param(Type) 0]=="PSZ"} {
      set Param(Lat0) $Viewport::Map(LatCursor)
      set Param(Lon0) $Viewport::Map(LonCursor)
   }

   Grid::Apply 
}

proc Grid::DrawDone { Canvas VP { Modifier "" } } {
   variable Param
   variable Data
   
   set Data(VP)    $VP

   #----- Reorder corner LowerLeft and UpperRight
   if { $Param(Lat1)<$Param(Lat0) } {
      set tmp $Param(Lat0)
      set Param(Lat0) $Param(Lat1)
      set Param(Lat1) $tmp
   }

   if { $Param(Lon1)<$Param(Lon0) } {
      set tmp $Param(Lon0)
      set Param(Lon0) $Param(Lon1)
      set Param(Lon1) $tmp
   }
   
   Grid::Create $Data(GridId)
}

#-------------------------------------------------------------------------------
# Nom      : <Grid::Update>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" de l'outils apres une mise a jour dans SPI
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc Grid::Update { Frame } {
   variable Data
}

#-------------------------------------------------------------------------------
# Nom      : <Grid::UpdateItems>
# Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items relatifs a cet outils sur
#            la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc Grid::UpdateItems { Frame } {
   global   GDefs
   variable Data
   variable Param

   if { $Data(VP)!="" } {
      $Data(Canvas) delete GRIDMAKER

      if { [winfo exists $Data(Tab).grid] } {
         #----- Adjust increment resolution relative to zoom
         set d [expr [$Data(VP) -distpix]/(1852.0*60)*2]
         $Data(Tab).grid.ll0.lat configure -increment $d
         $Data(Tab).grid.ll0.lon configure -increment $d
         $Data(Tab).grid.ll1.lat configure -increment $d
         $Data(Tab).grid.ll1.lon configure -increment $d
         $Data(Tab).grid.mid.xlat1 configure -increment $d
         $Data(Tab).grid.mid.xlon1 configure -increment $d
         $Data(Tab).grid.ref.latr configure -increment $d
         $Data(Tab).grid.ref.lonr configure -increment $d
      }
      
#      Viewport::DrawRange $Data(Frame) $Data(VP) $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) GRIDMAKER red
      if { $Grid::Param(Type)=="ZE" } {
         Viewport::DrawLine $Data(Frame) $Data(VP) [list $Grid::Param(XLat1) $Grid::Param(XLon1) 0.0 $Grid::Param(XLat2) $Grid::Param(XLon2) 0.0] [list GRIDMAKER GRIDMAKERROT PAGE$Data(VP)] $Data(Color0) 2 TRUE
#         $Data(Frame).page.canvas bind GRIDMAKERROT <Enter>     "$Data(Frame).page.canvas config -cursor exchange"
#         $Data(Frame).page.canvas bind GRIDMAKERROT <Leave>     "$Data(Frame).page.canvas config -cursor hand1"
#         $Data(Frame).page.canvas bind GRIDMAKERROT <B1-Motion> "set Grid::Param(XLat1) $Viewport::Map(LonCursor);set Grid::Param(XLat2) $Viewport::Map(LatCursor); puts stderr [expr [projection function $Page::Data(Frame) -bearing $Param(XLat1) $Param(XLon1) $Param(XLat2) $Param(XLon2)]+90.0]"
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Grid::PageActivate>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items relatifs a cet outils
#            lors d'un changement de page par l'usager.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------

proc Grid::PageActivate { Frame } {
}

#-------------------------------------------------------------------------------
# Nom      : <Grid::AsProject>
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

proc Grid::AsProject { File } {
   variable Data
   variable Param

   if { [winfo exists .gridmaker] } {
      puts $File "#----- Tool: Grid\n"
      puts $File "set Grid::Param(Dock)   $Param(Dock)"
      puts $File "set Grid::Param(Geom)   [winfo geometry .gridmaker]"
      puts $File "Grid::Window"
      puts $File "\n"
   }
}
