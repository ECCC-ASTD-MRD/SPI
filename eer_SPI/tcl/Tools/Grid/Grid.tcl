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

package require Dialog

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
   } elseif { [llength $Data(GridParams)]>1 } {
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
         fstdfield configure MODELGRID$no -rendergrid $grid -renderboundary $Data(GridBoundary) \
            -width 2 -color $Data(Color$no) -colormap GRID$no -rendertexture $texture -interpdegree NEAREST
      }
   }
   Viewport::UpdateData $Data(Frame)        
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Reset>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Reset internal parameters.
#
# Parametres :
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Reset { } {
   variable Param
   variable Data

   set Param(NI)         0                                                         ;# Number of gridpoint in I
   set Param(NJ)         0                                                         ;# Number of gridpoint in J
   set Param(Lat0)       0                                                         ;# Latitude of first bbox corner
   set Param(Lat1)       0                                                         ;# Latitude of second bbox corner
   set Param(Lon0)       0                                                         ;# Longitude of first bbox corner
   set Param(Lon1)       0                                                         ;# Longitude of second bbox corner
   set Param(LatD0)      0                                                         ;# Latitude delta relative to inside grid
   set Param(LatD1)      0                                                         ;# Latitude delta relative to inside grid
   set Param(LonD0)      0                                                         ;# Longitude delta relative to inside grid
   set Param(LonD1)      0                                                         ;# Longitude delta relative to inside grid
   set Param(LatM)       0                                                         ;# Delta on latitute translating grid
   set Param(LonM)       0                                                         ;# Delta on longitude whehn translating grid
   set Param(PGSM)       ""                                                        ;# Grid description for PGSM
   set Param(GridInfo)   ""                                                        ;# General grid description
   set Param(LatR)       0.0                                                       ;
   set Param(LonR)       180.0                                                     ;
   set Param(MaxCFL)     10                                                        ;
   set Param(XLat1)      0                                                         ;# Center latitude
   set Param(XLon1)      0                                                         ;# Center longitude
   set Param(XLat2)      0                                                         ;# Rotation axis latitude
   set Param(XLon2)      90                                                        ;# Rotation axis longitude
   set Param(Angle)      0                                                         ;# Rotation angle
   set Param(Extend)     0                                                         ;# Internal extension
   set Param(LockCenter) False                                                     ;# Fixe the grid center
   
   lset Data(GridParams) $Data(GridNo) [array get Grid::Param]

   fstdfield free $Data(GridId) ${Data(GridId)}TIC ${Data(GridId)}TAC ${Data(GridId)}PROJ ${Data(GridId)}MTRX
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Init>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser des parametres selon le type de grille
#
# Parametres :
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Init { } {
   variable Param
   variable Data

   switch $Param(Type) {
      "PS"   { set Param(NI) 229; set Param(NJ) 229; }
      "PS_N" { set Param(NI) 229; set Param(NJ) 229; set Param(ResM)  150000; set Param(ResLL) [expr $Param(ResM)/$Param(LL2M)]; set Param(Lon0) 0.0; set Param(Lat0)  90.0 }
      "PS_S" { set Param(NI) 229; set Param(NJ) 229; set Param(ResM)  150000; set Param(ResLL) [expr $Param(ResM)/$Param(LL2M)]; set Param(Lon0) 0.0; set Param(Lat0) -90.0 }
      "LL"   { }
      "ZL"   { }
      "ZE"   { }
      "UTM"  { }
   }

   Grid::Create $Data(GridId)
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Center>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Centrer la grille sur une localisation
#
# Parametres :
#    <Lat>   : Latitude centrale
#    <Lon>   : Longitude centrale
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Center { Lat Lon { Update True } } {
   variable Param
   variable Data

   set Param(XLat1) $Lat
   set Param(XLon1) $Lon
   
   switch $Param(Type) {
      "PS"   { set Param(Lon0) $Lon; set Param(Lat0) $Lat ; set Param(Lon1) 0.0; set Param(Lat1) 0.0 }
      "PS_N" { set Param(Lon0) 0.0;  set Param(Lat0)  90.0; set Param(Lon1) 0.0; set Param(Lat1) 0.0 }
      "PS_S" { set Param(Lon0) 0.0;  set Param(Lat0) -90.0; set Param(Lon1) 0.0; set Param(Lat1) 0.0 }
      "LL"   -
      "ZL"   -
      "ZE"   -
      "UTM"  { set dlat [expr ($Param(Lat1)-$Param(Lat0))*0.5]
               set dlon [expr ($Param(Lon1)-$Param(Lon0))*0.5]
               set Param(Lat0) [expr $Lat-$dlat]
               set Param(Lat1) [expr $Lat+$dlat]
               set Param(Lon0) [expr $Lon-$dlon]
               set Param(Lon1) [expr $Lon+$dlon]
             }
   }

   if { $Update } {
      Grid::Create $Data(GridId) 
   }
}

#----------------------------------------------------------------------------
# Nom      : <Grid::BBoxOrder>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Ordonne les coordonnees de la bounding box
#
# Parametres :
#
# Retour:
#
# Remarques :
#    Cette fonction est utilise avec "uplevel" afin de simuler une macro
#
#----------------------------------------------------------------------------

proc Grid::BBoxOrder { } {

   uplevel {
      if { $Data(GridNo)>0 } {
         set LatR [expr $LatR+$Param(LatM)]
         set LonR [expr $LonR+$Param(LonM)]
      } else {
         set Lat0 [expr $Lat0+$Param(LatM)]
         set Lon0 [expr $Lon0+$Param(LonM)]
         set Lat1 [expr $Lat1+$Param(LatM)]
         set Lon1 [expr $Lon1+$Param(LonM)]
      }

      if { $Lat1<$Lat0 } {
         set tmp $Lat0
         set Lat0 $Lat1
         set Lat1 $tmp
      }

      if { $Lon1<$Lon0 } {
         set tmp $Lon0
         set Lon0 $Lon1
         set Lon1 $tmp
      }
            
      #----- If this is a cascaded grid
      if { [set idx [lsearch $Grid::Data(GridParams) $Grid::Data(GridNo)]]>0 } {
         #----- Store the size differences
         array set grid [lindex $Grid::Data(GridParams) [expr $idx-1]]
         set Param(LatD0) [expr $Lat0-$grid(Lat0)]
         set Param(LatD1) [expr $Lat1-$grid(Lat1)]
         set Param(LonD0) [expr $Lon0-$grid(Lon0)]
         set Param(LonD1) [expr $Lon1-$grid(Lon1)]
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Grid::NIJ>
# Creation : Juin 2018 - J.P. Gauthier - CMC/CMOE
#
# But      : Calcule les NIJ ou les Lat1 Lon1 selon le cas
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Grid::NIJ { } {
   variable Msg
   variable Lbl

   uplevel {
      if { $Param(GetNIJ) } {
         set ni [expr int(ceil(($Lon1-$Lon0)/$Res))+1]
         set nj [expr int(ceil(($Lat1-$Lat0)/$Res))+1]

         if { $Param(SizeWarn) && [expr $ni*$nj]>$Param(NIJWarn) } {
            if { ![Dialog::Default . 400 WARNING $Grid::Msg(Size) "\n\n\t$ni x $nj" 0 $Grid::Lbl(No) $Grid::Lbl(Yes)] } {
               return ""
            }
            set Param(SizeWarn) False
         }

         set Param(NI) $ni
         set Param(NJ) $nj

         #----- Check inclusiveness
         if { $Data(GridNo)>0 } {
            array set gridp [lindex $Data(GridParams) [expr $Data(GridNo)-1]]
            
            set si  [expr $Param(NI)*$Param(ResM)]
            set sj  [expr $Param(NJ)*$Param(ResM)]
            set spi [expr $gridp(NI)*$gridp(ResM)]
            set spj [expr $gridp(NJ)*$gridp(ResM)]
            
            #----- Make sure the grid encloses completely the previous grid
            if { $si<$spi } { set Param(NI) [expr $spi/$Param(ResM)+1] }
            if { $sj<$spj } { set Param(NJ) [expr $spj/$Param(ResM)+1] }

            if { $Data(GridNo)>$Data(GridDepend) } {
               set Param(NI) [expr int(($spi+$Param(DNI))/$Param(ResM))]
               set Param(NJ) [expr int(($spj+$Param(DNJ))/$Param(ResM))]
            } else {
               set Param(DNI) [expr $si - $spi]
               set Param(DNJ) [expr $sj - $spj]
            }
            
            #----- Check LatR,LonR translations
            if { $LatR!=0.0 || $LonR!=180.0 } {
               set plat0 [expr $gridp(Lat0)+$gridp(LatR)]
               set plon0 [expr $gridp(Lon0)+($gridp(LonR)-180)]
               set plat1 [expr $gridp(Lat1)+$gridp(LatR)]
               set plon1 [expr $gridp(Lon1)+($gridp(LonR)-180)]
               
               if { [expr $Lat0+$LatR]>$plat0 }       { set LatR [expr $plat0-$Lat0] }
               if { [expr $Lon0+($LonR-180)]>$plon0 } { set LonR [expr $plon0-$Lon0+180] }
               if { [expr $Lat1+$LatR]<$plat1 }       { set LatR [expr $plat1-$Lat1] }
               if { [expr $Lon1+($LonR-180)]<$plon1 } { set LonR [expr $plon1-$Lon1+180] }
            }
         }

      } else {
         set Param(Lat1) [set Lat1 [expr $Lat0+$Param(NJ)*$Res]]
         set Param(Lon1) [set Lon1 [expr $Lon0+$Param(NI)*$Res]]
      }
      
      set Param(XLat1) [expr ($Lat0+$Lat1)*0.5]
      set Param(XLon1) [expr ($Lon0+$Lon1)*0.5]
   }
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
   variable Param

   Grid::Create $Grid::Data(GridId)
   set Param(LatM) 0
   set Param(LonM) 0
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
   for { set no [expr $pno+1] } { $no<[llength $Data(GridParams)] } { incr no } {
      array set gridp [lindex $Data(GridParams) [expr $no-1]]
      array set Grid::Param [lindex $Data(GridParams) $no]
      set Param(XLat1) $gridp(XLat1)
      set Param(XLon1) $gridp(XLon1)
      set Param(Angle) $gridp(Angle)
      
      set Param(NI) [expr $Param(DNI)+$gridp(NI)]
      set Param(NJ) [expr $Param(DNJ)+$gridp(NJ)]
      set Data(GridNo) $no
      Grid::Create MODELGRID$no
   }
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
                      return [format "&grdc
  Grdc_ni     = %i, Grdc_nj     = %i,
  Grdc_dx     = %.4f, Grdc_dy     = %.4f,
  Grdc_lonr   = %9.4f, Grdc_latr  = %8.4f,
  Grdc_maxcfl = %i \
  Grdc_nbits  = \
  Grdc_nfe    = \n" \
         $param(NI) $param(NJ) $param(ResLL) $param(ResLL) $param(LonR) $param(LatR) $param(MaxCFL)]
                   } else {      
                      return [format "&grid
  Grd_typ_S  = 'LU',
  Grd_ni     = %i, Grd_nj     = %i,
  Grd_dx     = %.4f, Grd_dy     = %.4f,
  Grd_lonr   = %9.4f, Grd_latr  = %8.4f,
  Grd_xlon1  = %9.4f, Grd_xlat1 = %8.4f,
  Grd_xlon2  = %9.4f, Grd_xlat2 = %8.4f,
  Grd_maxcfl = %i\n" \
         $param(NI) $param(NJ) $param(ResLL) $param(ResLL) $param(LonR) $param(LatR) $param(XLon1) $param(XLat1) $param(XLon2) $param(XLat2) $param(MaxCFL)]
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

   if { $Path=="" } {
      return
   }
   
   set no    [llength $Data(GridParams)]
   set gridc {}
   foreach grid $Data(GridParams) {
      array set param $grid
      
      #----- Figure out the extension in km (ex: 1p0 2p5 0p25)
      set resk [format "%.2f" [expr $param(ResM)/1000.0]]
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
      Grid::Write FILE MODELGRID[expr $no-1]
      fstdfile close FILE

      #----- Create GenphysX job file
      set f [open $Path/$res/geophy.sh w 0755]
      puts $f "#!/bin/bash"
      puts $f "GenPhysX -gridfile $Path/$res/grid.fstd -target $GenPhysX(Target) -result $Path/$res/geophy -batch -mach $GenPhysX(Host) -t $GenPhysX(Time) -cm $GenPhysX(Memory) -cpus $GenPhysX(CPU) &"
      close $f
      
      incr no -1
   } 
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
   
   foreach path [lsort -increasing [glob -nocomplain $Path/*p*]] {
      Log::Print INFO "Loading $path"
      if { [file exists $path/gem_settings.nml] } {
         Grid::SettingsRead $path/gem_settings.nml
#???         Grd_iref   = 675      ,     Grd_jref  = 600

         set Param(Type)   ZE
         set Param(NI)     $Settings(GRD_NI)
         set Param(NJ)     $Settings(GRD_NJ)
         set Param(ResLL)  $Settings(GRD_DX)
         set Param(ResLL)  $Settings(GRD_DY)
         set Param(LonR)   $Settings(GRD_LONR)
         set Param(LatR)   $Settings(GRD_LATR)
         set Param(XLon1)  $Settings(GRD_XLON1)
         set Param(XLat1)  $Settings(GRD_XLAT1)
         set Param(XLon2)  $Settings(GRD_XLON2)
         set Param(XLat2)  $Settings(GRD_XLAT2)
         set Param(MaxCFL) $Settings(GRD_MAXCFL)
         
         set Param(ResM)  [expr $Param(ResLL)*$Param(LL2M)]
         set Param(Lat0)  [expr $Param(XLat1)-($Param(NJ)*$Param(ResLL)*0.5)]
         set Param(Lon0)  [expr $Param(XLon1)-($Param(NI)*$Param(ResLL)*0.5)]
         set Param(Lat1)  [expr $Param(XLat1)+($Param(NJ)*$Param(ResLL)*0.5)]
         set Param(Lon1)  [expr $Param(XLon1)+($Param(NI)*$Param(ResLL)*0.5)]

         Grid::Add
      }
   }
   
   #----- Select 1st grid to start with
   set Data(GridNo) 0
   Grid::Switch
   
   Viewport::GoTo $Data(Frame) $Param(XLat1) $Param(XLon1)
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Create>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonction de creation de grille appelee par l'interface
#
# Parametres :
#    <GridInfo> : Grid description string
#    <ID>       : Identificateur du champs qui sera cree
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Create { { ID MODELGRID } { GridInfo {} } } {
   variable Param
   variable Data

   if { [llength $GridInfo] } {
      scan $GridInfo "%s %i %i %f %f %f %f %f %f" Param(Type) Param(NI) Param(NJ) Param(Lat0) Param(Lon0) Param(Lat1) Param(Lon1) Param(ResM) Param(ResLL)
   }

   if { $Param(LockCenter) } {
      Grid::Center $Param(XLat1) $Param(XLon1) False
   }
   if { [string match "PS*" [lindex $Param(Type) 0]] || ($Param(Lat0)!=$Param(Lat1) && $Param(Lon0)!=$Param(Lon1)) } {

      switch $Param(Type) {
         "PS"    { Grid::CreatePS  $Param(Lat0) $Param(Lon0) $Param(ResM) $Param(NI) $Param(NJ) $ID }
         "PS_S"  -
         "PS_N"  { Grid::CreatePS  $Param(Lat0) $Param(Lon0) $Param(ResM) $Param(NI) $Param(NJ) $ID }
         "LL"    { Grid::CreateL   $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResLL) $ID }
         "ZL"    { Grid::CreateZL  $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResLL) $ID }
         "ZE"    { Grid::CreateZE  $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(LatR) $Param(LonR) $Param(ResLL) $Param(Angle) $ID }
         "UTM"   { Grid::CreateUTM $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResLL) $ID }
      }
      set Param(GridInfo) [format "$Param(Type) $Param(NI) $Param(NJ) %.7f %.7f %.7f %.7f %.2f %.7f" $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResM) $Param(ResLL)]

      if { [info exists ::tk_version] } {         
         set Data(Frame)  $Page::Data(Frame)
         set Data(Canvas) $Page::Data(Canvas)
         set Data(VP)     $Viewport::Data(VP)
         
         Grid::ConfigSet
         Viewport::Assign $Data(Frame) $Data(VP) $ID False 0
         Grid::UpdateItems $Page::Data(Frame)       
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Decode>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonction de creation de grille a partir d'une définition d'expérience
#
# Parametres :
#    <Scale> : Grid description string
#    <Lat>   : Latitude centrale
#    <Lon>   : Longitude centrale
#
# Retour:
#    <grid>  : Grid parameters
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Decode { Scale { Lat 0.0 } { Lon 0.0 } } {
   variable Param
   variable Data

   set center      False

   set Param(Id)   [lindex $Scale 0]                          ;#----- Grid scale name
   set Param(Type) [string trimleft  [lindex $Scale 1] "("]   ;#----- Grid type
   set res         [lindex $Scale 2]                          ;#----- Grid scale resolution
   set dim         [string trimright [lindex $Scale 4] ")"]   ;#----- Grid size NIxNJ

   #----- Get resolutions in meters and degrees
   switch [lindex $Scale 3] {
     "deg," { set Param(ResLL) $res;             set Param(ResM)  [expr $Param(ResLL)*$Param(LL2M)] }
     "m,"   { set Param(ResM)  $res;             set Param(ResLL) [expr $Param(ResM)/$Param(LL2M)] }
     "km,"  { set Param(ResM)  [expr $res*1000]; set Param(ResLL) [expr $Param(ResM)/$Param(LL2M)] }
   }

   #----- Get grid dimensions
   if { [set idx [string first "x" $dim]] != -1 } {
      set Param(NI) [string range $dim 0 [expr $idx - 1]]
      set Param(NJ) [string range $dim [expr $idx + 1] end]
   }

   #----- If hemispheric, check for north or south
   if { [string match "*HEMI*" $Param(Id)] } {
      if { $Lat>0 } {
         set Param(Type) "PS_N"
         set Param(Lat0) 90.0
      } else {
         set Param(Type) "PS_S"
         set Param(Lat0) -90.0
      }
      set Param(Lon0) 0.0
   }

   #----- For latlon grid, check for global case
   if { $Param(Type)=="ZL" || $Param(Type)=="LL" || $Param(Type)=="ZE" } {
      if { [expr ($Param(NI)*$Param(ResLL))>=(360-$Param(ResLL))] } {
          set Param(Lon0) -180
          set Param(Lon1) 180
          set Param(Lat0) -90
          set Param(Lat1) 90
      } else {
          set center True
          set Param(Lon0) 0
          set Param(Lon1) [expr $Param(ResLL)*$Param(NI)]
          set Param(Lat0) 0
          set Param(Lat1) [expr $Param(ResLL)*$Param(NJ)]
      }
   }

   if { $center || $Lon!=0.0 } {
      Grid::Center $Lat $Lon
   } else {
      Grid::Create $Data(GridId)
   }
   return $Param(GridInfo)
}

#----------------------------------------------------------------------------
# Nom      : <Grid::CreatePS>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Creation d'une grille Polaire Stereographique (PS)
#
# Parametres :
#   <Lat>    : Latitude centrale
#   <Lon>    : Longitude centrale
#   <Res>    : Resolution en metres
#   <NI>     : Nombre de point de grille en I
#   <NJ>     : Nombre de point de grille en J
#   <ID>     : Identificateur du champs qui sera cree
#
# Retour:
#   <ID>     : Identificateur du champs cree
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::CreatePS { Lat Lon Res NI NJ { ID MODELGRID } } {
   variable Param

   set xg3 $Res
   set lat [expr $Lat+$Param(LatM)]
   set lon [expr $Lon+$Param(LonM)]

   #----- choix de l'hemisphere SUD on NORD
   if { $lat<=0 } {
      set grtyp SUD
      set nhem  2
      set xg4  [expr 90.0+$Lon]
      set xg4  [expr floor(fmod($xg4+360.0,360.0))]
   } else {
      set grtyp NORD
      set nhem  1
      set xg4   [expr (270.0-$Lon+360.0)/360.0]
      set xg4   [expr ($xg4-floor($xg4))*360.0]
   }

   set dd60 1.0
   set xy [fstdgrid xyfll $lat $lon $dd60 $xg4 $nhem]

   set xg1 [expr ((($NI-1.0)/2.0) * $xg3 - [lindex $xy 0]) / $xg3 + 1.0]
   set xg2 [expr ((($NJ-1.0)/2.0) * $xg3 - [lindex $xy 1]) / $xg3 + 1.0]

   fstdfield free $ID
   fstdfield create $ID $NI $NJ 1
   fstdfield define $ID -NOMVAR "GRID" -ETIKET "GRID" -TYPVAR X -GRTYP [string index $grtyp 0] $xg1 $xg2 $xg3 $xg4

   set Param(PGSM) [format "PS $NI $NJ %.7f %.7f %.7f %.7f $grtyp" $xg1 $xg2 $xg3 $xg4]

   return ${ID}
}

#----------------------------------------------------------------------------
# Nom      : <Grid::CreateL>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Creation d'une grille LatLon (L)
#
# Parametres :
#   <Lat0>   : Latitude du premier coin
#   <Lon0>   : Longitude du premier coin
#   <Lat1>   : Latitude du deuxieme coin
#   <Lon1>   : Longitude du deuxieme coin
#   <Res>    : Resolution en degree
#   <ID>     : Identificateur du champs qui sera cree
#
# Retour:
#   <ID>     : Identificateur du champs cree
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::CreateL { Lat0 Lon0 Lat1 Lon1 Res { ID MODELGRID } } {
   variable Param
   variable Data

   Grid::BBoxOrder
   Grid::NIJ

   fstdfield create ${ID} $Param(NI) $Param(NJ) 1 $Param(Data)
   fstdfield define ${ID} -NOMVAR "GRID" -ETIKET "GRID" -TYPVAR X -GRTYP L $Lat0 $Lon0 $Res $Res

   set Param(PGSM) [format "LATLON $Param(NI) $Param(NJ) %.7f %.7f %.7f %.7f" $Lat0 $Lon0 $Res $Res]

   return ${ID}
}

#----------------------------------------------------------------------------
# Nom      : <Grid::CreateZL>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Creation d'une grille Z sur reference de grille latlon (L)
#
# Parametres :
#   <Lat0>   : Latitude du premier coin
#   <Lon0>   : Longitude du premier coin
#   <Lat1>   : Latitude du deuxieme coin
#   <Lon1>   : Longitude du deuxieme coin
#   <Res>    : Resolution en degree
#   <ID>     : Identificateur du champs qui sera cree
#
# Retour:
#   <ID>     : Identificateur du champs cree
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------
proc Grid::CreateLZ { Lat0 Lon0 Lat1 Lon1 Res { ID MODELGRID } } {
   Grid::CreateZL $Lat0 $Lon0 $Lat1 $Lon1 $Res $ID
}
proc Grid::CreateZL { Lat0 Lon0 Lat1 Lon1 Res { ID MODELGRID } } {
   variable Param
   variable Data

   Grid::BBoxOrder
   Grid::NIJ

   fstdfield free ${ID} ${ID}TIC ${ID}TAC
   fstdfield create ${ID}TIC $Param(NI) 1 1
   fstdfield create ${ID}TAC 1 $Param(NJ) 1

   fstdfield define ${ID}TIC -NOMVAR ">>" -ETIKET "GRID" -TYPVAR X -GRTYP L 0 0 1.0 1.0
   fstdfield define ${ID}TAC -NOMVAR "^^" -ETIKET "GRID" -TYPVAR X -GRTYP L 0 0 1.0 1.0

   #----- Compute tic grid coordinates.
   set lon $Lon0
   for { set i 0 } { $i < $Param(NI) } { incr i } {
      fstdfield stats ${ID}TIC -gridvalue $i 0 $lon
      set lon [expr $lon+$Res]
   }

   #----- Compute tac grid coordinates.
   set lat $Lat0
   for { set j 0 } { $j < $Param(NJ) } { incr j } {
      fstdfield stats ${ID}TAC -gridvalue 0 $j $lat
      set lat [expr $lat+$Res]
   }
   
   #----- Create the grid ans assign the tic/tac
   fstdfield create ${ID} $Param(NI) $Param(NJ) 1 $Param(Data)
   fstdfield define ${ID} -NOMVAR "GRID" -ETIKET "GRID" -TYPVAR X -GRTYP ZL
   fstdfield define ${ID} -positional ${ID}TIC ${ID}TAC

   set Param(PGSM) ""

   return ${ID}
}

#----------------------------------------------------------------------------
# Nom      : <Grid::CreateZE>
# Creation : Juin 2018 - Michel Van Eeckhout - CMC/CMDS
#
# But      : Creation d'une grille Z sur reference de grille E
#
# Parametres :
#   <Lat0>   : Latitude du premier coin
#   <Lon0>   : Longitude du premier coin
#   <Lat1>   : Latitude du deuxieme coin
#   <Lon1>   : Longitude du deuxieme coin
#   <Res>    : Resolution en degres
#   <Angle>  : Angle en degres
#   <ID>     : Identificateur du champs qui sera cree
#
# Retour:
#   <ID>     : Identificateur du champs cree
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::CreateZE { Lat0 Lon0 Lat1 Lon1 LatR LonR Res Angle { ID MODELGRID } { Check True } } {
   variable Param
   variable Data

   if { $Check } {
      Grid::BBoxOrder
      Grid::NIJ
   }

   set ll [projection function $Page::Data(Frame) -circle $Param(XLat1) $Param(XLon1) [expr $Res*1852.0*60*0.75*$Param(NI)] [expr $Angle-90.0]]
   set Param(XLat2) [lindex $ll 0]
   set Param(XLon2) [lindex $ll 1]
      
   fstdfield free ${ID} 
   georef free ${ID}
   
   #----- Create the grid 
   catch { georef create ${ID} }
   georef define ${ID} -rpn $Param(NI) $Param(NJ) $Res $Res $Param(XLat1) $Param(XLon1) $Param(XLat2) $Param(XLon2) $LatR $LonR $Param(MaxCFL)

   #----- Get size, it is possible that the grid build algorithm adjusts the nixnj
   set sz [georef define ${ID} -size]
   set ni [lindex $sz 0]
   set nj [lindex $sz 1]
   set Param(Extend) [expr $ni-$Param(NI)]
  
   fstdfield create ${ID} $ni $nj 1 $Param(Data)
   fstdfield define ${ID} -georef ${ID} -NOMVAR "GRID" -ETIKET "GRID" -TYPVAR X -GRTYP ZE

   set d [expr $Param(Extend)]
   set dni [expr $ni-$d]
   set dnj [expr $nj-$d]
   
   #----- Mark the inside grid
   vexpr - ${ID}(($d,$dni),($d,$dnj))=1
   
   set Param(PGSM) ""

   return ${ID}
}

#----------------------------------------------------------------------------
# Nom      : <Grid::CreateUTM>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Creation d'une grille UTM avec la methode WKT
#
# Parametres :
#   <Lat0>   : Latitude du premier coin
#   <Lon0>   : Longitude du premier coin
#   <Lat1>   : Latitude du deuxieme coin
#   <Lon1>   : Longitude du deuxieme coin
#   <Res>    : Resolution en degree
#   <ID>     : Identificateur du champs qui sera cree
#
# Retour:
#   <ID>     : Identificateur du champs cree
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::CreateUTM { Lat0 Lon0 Lat1 Lon1 Res { ID MODELGRID } } {
   variable Param
   variable Data
   variable Msg
   variable Lbl

   Grid::BBoxOrder

   set zone     [expr int(ceil((180+(($Lon1+$Lon0)/2))/6))]
   set meridian [expr -((180-($zone*6))+3)]
   set wkt      "\{PROJCS\[\"WGS_1984_UTM_Zone_${zone}N\",\
         GEOGCS\[\"GCS_WGS_1984\",\
            DATUM\[\"D_WGS_1984\",\
               SPHEROID\[\"WGS_1984\",6378137.0,298.257223563\]\],\
            PRIMEM\[\"Greenwich\",0.0\],\
            UNIT\[\"Degree\",0.0174532925199433\]\],\
         PROJECTION\[\"Transverse_Mercator\"\],\
         PARAMETER\[\"False_Easting\",500000.0\],\
         PARAMETER\[\"False_Northing\",0.0\],\
         PARAMETER\[\"Central_Meridian\",$meridian\],\
         PARAMETER\[\"Scale_Factor\",0.9996\],\
         PARAMETER\[\"Latitude_Of_Origin\",0.0\],\
         UNIT\[\"Meter\",1.0\]\]\}"

   fstdfield free ${ID} ${ID}PROJ ${ID}MTRX
   georef free $ID

   georef create $ID $wkt

   set xy0 [georef unproject $ID $Lat0 $Lon0]
   set xy1 [georef unproject $ID $Lat1 $Lon1]

   set ni [expr int(ceil(([lindex $xy1 0] - [lindex $xy0 0])/$Res))+1]
   set nj [expr int(ceil(([lindex $xy1 1] - [lindex $xy0 1])/$Res))+1]

   if { $Param(SizeWarn) && [expr $ni*$nj]>$Param(NIJWarn) } {
      if { ![Dialog::Default . 400 WARNING $Msg(Size) "\n\n\t$ni x $nj" 0 $Lbl(No) $Lbl(Yes)] } {
         return ""
      }
      set Param(SizeWarn) False
   }

   set Param(NI) $ni
   set Param(NJ) $nj

   set scalex    [expr abs($Res)]
   set scaley    [expr -1.0 * abs($Res)]
   set uly       [lindex $xy1 1]
   set ulx       [lindex $xy0 0]

   set  transform [list $ulx $scalex 0.000000000000000 $uly 0.000000000000000 $scaley]

   georef define $ID -transform $transform

#    #----- Create projection and transform field
#    fstdfield create ${ID}PROJ [string length $wkt] 1 1 UByte
#    fstdfield define ${ID}PROJ -NOMVAR "PROJ" -ETIKET "GRID" -TYPVAR X -GRTYP X
#    fstdfield define ${ID}PROJ -DATA [binary format A* $wkt]
# 
#    fstdfield create ${ID}MTRX 6 1 1 Float32
#    fstdfield define ${ID}MTRX -NOMVAR "MTRX" -ETIKET "GRID" -TYPVAR X -GRTYP X
#    fstdfield define ${ID}MTRX -DATA [binary format f* $transform]

   #----- Create the grid ans assign the tic/tac
   fstdfield create ${ID} $Param(NI) $Param(NJ) 1 $Param(Data)
   fstdfield define ${ID} -georef $ID -NOMVAR "GRID" -ETIKET "GRID" -TYPVAR X -GRTYP W

   set Param(PGSM) ""

   return ${ID}
}

#----------------------------------------------------------------------------
# Nom      : <Grid::Write>
# Creation : Avril 2015 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarde d'une grille dans un fichier
#
# Parametres :
#   <FILE>   : Identificateur de fichier RPN
#   <ID>     : Identificateur du champs grille
#   <IP1>    : IP1 des descripteurs
#   <IP2>    : IP2 des descripteurs
#   <IP3>    : IP3 des descripteurs
#   <ETIKET> : ETIKET des descripteurs
#   <Grid>   : Write field on grid
#
# Retour:
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::Write { FILE ID { IP1 0 } { IP2 0 } { IP3 0 } { ETIKET GRID } { Grid True }} {

   set dateo [fstdstamp fromseconds [clock seconds]]

   #----- If no IP specified, use "unique" values take from current datestamp
   if { !$IP1 && !$IP2 && !$IP3 } {
      set IP1 [string range $dateo 0 2]
      set IP2 [string range $dateo 3 5]
      set IP3 [string range $dateo 6 9]
   }
    
   switch [fstdfield define $ID -GRTYP] {
      "Z"     { fstdfield define ${ID} -DATEO $dateo -TYPVAR X -IG1 $IP1 -IG2 $IP2 -IG3 $IP3 -IG4 0 }
      "W"     { fstdfield define ${ID} -DATEO $dateo -TYPVAR X -IG1 $IP1 -IG2 $IP2 -IG3 $IP3 -IG4 0 }
      default { fstdfield define ${ID} -DATEO $dateo }
   }

   fstdfield writegeo $ID $FILE $ETIKET
   
   if { $Grid } {
      fstdfield write ${ID} $FILE -8 True
   }
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

   set Param(LatM) $Viewport::Map(LatD)
   set Param(LonM) $Viewport::Map(LonD)

   Grid::Apply 
}

proc Grid::MoveDone { Canvas VP { Modifier "" } } {
   variable Param
   variable Data
   
   set Data(VP)    $VP

   if { $Data(GridNo)>0 } {
      set Param(LatR) [expr $Param(LatR)+$Param(LatM)]
      set Param(LonR) [expr $Param(LonR)+$Param(LonM)]
   } else {
      set Param(Lat0) [expr $Param(Lat0)+$Param(LatM)]
      set Param(Lon0) [expr $Param(Lon0)+$Param(LonM)]
      set Param(Lat1) [expr $Param(Lat1)+$Param(LatM)]
      set Param(Lon1) [expr $Param(Lon1)+$Param(LonM)]
   }
   
   set Param(LatM) 0
   set Param(LonM) 0
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

   if { [lindex $Param(Type) 0]=="PS" } {
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
