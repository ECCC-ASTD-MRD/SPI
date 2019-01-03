#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Package d'interface pour SPI
# Fichier  : Grid.ctes
# Creation : Juin 2003
#
# Description:
#    Coquille vide demontrant les points d'entree pour la creation de nouveaux outils
#    a l'interface SPI
#
#    Descriptions des variables internes du module
#
#===============================================================================

package provide Grid 2.0
package require Dialog

catch { SPI::Splash "Loading Package Grid 2.0" }

namespace eval Grid {
   variable Data
   variable Param
   
   set Data(GridNb)       0
   set Data(GridNo)       0
   set Data(GridDepend)   0
   set Data(GridId)       MODELGRID                                                ;# Grid identificator 
   set Data(GridParams)   { "" }
   set Data(GridSize)     2
   set Data(GridColor)    black
   set Data(GridBoundary) 0
   set Data(Color0)       black                          
   set Data(Color1)       blue                        
   set Data(Color2)       red                        
   set Data(Color3)       yellow                        
   set Data(Color4)       green                         
   set Data(SavePath)     ""
   set Data(LatM)         0                                                        ;# Delta on latitute translating grid
   set Data(LonM)         0                                                        ;# Delta on longitude when translating grid
      
   set Param(Id)         "User"                                                    ;# Grid identification (name)
   set Param(Data)       Float32                                                   ;# Data format of GRID field
   set Param(Type)       "ZE"                                                      ;# Current grid type
   set Param(Types)      { "PS" "PS_N" "PS_S" "LL" "ZL" "ZE" "UTM" }               ;# List of grid types
   set Param(ResMX)      10000                                                     ;# Grid resolution in meters
   set Param(ResMY)      10000                                                     ;# Grid resolution in meters
   set Param(ResMs)      { 1 5 10 100 250 500 1000 2000 2500 5000 10000 25000 50000 150000 }    ;# List of predefined resolution in meters
   set Param(ResLLX)     0.1                                                       ;# Grid resolution in degree
   set Param(ResLLY)     0.1                                                       ;# Grid resolution in degree
   set Param(ResLLs)     { 0.01 0.1 0.25 0.5 1 2 }                                 ;# List of predefined resolution in degrees
   set Param(GetNIJ)     True                                                      ;# Calculate NI NJ from corner coordinates
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
   set Param(PGSM)       ""                                                        ;# Grid description for PGSM
   set Param(GridInfo)   ""                                                        ;# General grid description
   set Param(ND)         40                                                        ;# Number of gridpoint for buffer between resolutions
   set Param(NIJWarn)    4000000                                                   ;# Warning grid size 2000x2000
   set Param(SizeWarn)   [expr [info exists ::tk_version]?True:False]              ;# Warn for large grid (Only in interactive mode)
   set Param(LL2M)       [expr 1852.0*60]                                          ;# Conversion factor from degrees to meters
   set Param(LockCenter) False                                                     ;# Fixe the grid center
   set Param(FFT)        1                                                         ;# Direction of FFT check

   set Param(LatR)     0.0                                                         ;
   set Param(LonR)     180.0                                                       ;
   set Param(MaxCFL)   4                                                           ;
   set Param(XLat1)    0                                                           ;# Center latitude
   set Param(XLon1)    0                                                           ;# Center longitude
   set Param(XLat2)    0                                                           ;# Rotation axis latitude
   set Param(XLon2)    90                                                          ;# Rotation axis longitude
   set Param(Angle)    0                                                           ;# Rotation angle
   set Param(PI)       0                                                           ;# Internal pilot zone in I
   set Param(PJ)       0                                                           ;# Internal pilot zone in J
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
   set Param(RNI)        0                                                         ;# Real number of gridpoint in I
   set Param(RNJ)        0                                                         ;# Real number of gridpoint in J
   set Param(Lat0)       0                                                         ;# Latitude of first bbox corner
   set Param(Lat1)       0                                                         ;# Latitude of second bbox corner
   set Param(Lon0)       0                                                         ;# Longitude of first bbox corner
   set Param(Lon1)       0                                                         ;# Longitude of second bbox corner
   set Param(LatD0)      0                                                         ;# Latitude delta relative to inside grid
   set Param(LatD1)      0                                                         ;# Latitude delta relative to inside grid
   set Param(LonD0)      0                                                         ;# Longitude delta relative to inside grid
   set Param(LonD1)      0                                                         ;# Longitude delta relative to inside grid
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
   set Param(PI)         0                                                         ;# Internal extension in I
   set Param(PJ)         0                                                         ;# Internal extension in J
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
      "PS_N" { set Param(NI) 229; set Param(NJ) 229; 
               set Param(Lon0) 0.0; set Param(Lat0)  90.0
               set Param(ResMX)  150000; set Param(ResLLX) [expr $Param(ResMX)/$Param(LL2M)]; 
               set Param(ResMY)  $Param(ResMX); set Param(ResLLY) $Param(ResLLX) }
      "PS_S" { set Param(NI) 229; set Param(NJ) 229; 
               set Param(Lon0) 0.0; set Param(Lat0)  -90.0
               set Param(ResMX)  150000; set Param(ResLLX) [expr $Param(ResMX)/$Param(LL2M)]; 
               set Param(ResMY)  $Param(ResMX); set Param(ResLLY) $Param(ResLLX) }
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
# Nom      : <Grid::CheckBBoxOrder>
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

proc Grid::CheckBBoxOrder { } {

   uplevel {
      if { $Data(GridNo)>0 } {
         if { $Param(Type)=="ZE"  } {
            set LatR [expr $LatR+$Data(LatM)]
            set LonR [expr $LonR+$Data(LonM)]
         }
      } else {
         if { $Param(Type)=="ZE"  } {
            set LatR 0.0
            set LonR 180.0
         }
         set Lat0 [expr $Lat0+$Data(LatM)]
         set Lon0 [expr $Lon0+$Data(LonM)]
         set Lat1 [expr $Lat1+$Data(LatM)]
         set Lon1 [expr $Lon1+$Data(LonM)]
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
# Nom      : <Grid::CheckFFT>
# Creation : Decembre 2018 - J.P. Gauthier - CMC/CMOE
#
# But      : Calcul du prochain entier > M qui se factorise en K
#
# Parametres :
#     <N>       : Dimension a vérifier
#     <Incr>    : Direction de l'incrément (défaut: 1)
#     <Min>     : Valeur minimale (défaut: 8)
#     <Factors> : Liste des facteurs  (défaut: {2 3 5})
#
# Retour:
#     <N>    : Valeur factorisable selon la liste K
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Grid::CheckFFT { N { Incr 1 } { Min 8 } { Factors { 2 3 5 } } } {

   if { $N<$Min } { set N [expr $Min+1] }

   set v $N
   while { $v!=1 } {
      set nk 0
      foreach k $Factors {
         if { [expr $v%$k]==0 } { 
            set v [expr $v/$k]
            break
         }
         incr nk
      }
      if { $nk==[llength $Factors] } {
         set v [incr N $Incr]
      }
   }
 
   if { $N<$Min } { set N [expr $Min+1] }
   return $N
}

#----------------------------------------------------------------------------
# Nom      : <Grid::CheckNIJ>
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

proc Grid::CheckNIJ { } {
   variable Msg
   variable Lbl

   uplevel {
      if { $Param(GetNIJ) } {
         set ni [expr int(ceil(($Lon1-$Lon0)/$ResX))+1]
         set nj [expr int(ceil(($Lat1-$Lat0)/$ResY))+1]

         if { $Param(SizeWarn) && [expr $ni*$nj]>$Param(NIJWarn) } {
            if { [info exists ::tk_version] && ![Dialog::Default . 400 WARNING $Grid::Msg(Size) "\n\n\t$ni x $nj" 0 $Grid::Lbl(No) $Grid::Lbl(Yes)] } {
               return ""
            }
            set Param(SizeWarn) False
         }

         set Param(NI) $ni
         set Param(NJ) $nj
      }
     
      #----- Check inclusiveness
      if { $Data(GridNo)>0 } {
         array set gridp [lindex $Data(GridParams) [expr $Data(GridNo)-1]]
         
         #----- Minimal distance between grids
         set dx [expr $Param(ND)*$Param(ResMX)*2]
         set dy [expr $Param(ND)*$Param(ResMY)*2]

         set si  [expr ($Param(NI)+$Param(PI))*$Param(ResMX)]
         set sj  [expr ($Param(NJ)+$Param(PJ))*$Param(ResMY)]
         set spi [expr $gridp(RNI)*$gridp(ResMX)+$dx]
         set spj [expr $gridp(RNJ)*$gridp(ResMY)+$dy]
         
         
         #----- Make sure the grid encloses completely the previous grid
         if { $si<$spi } { set Param(NI) [expr $spi/$Param(ResMX)-$Param(PI)+1] }
         if { $sj<$spj } { set Param(NJ) [expr $spj/$Param(ResMY)-$Param(PJ)+1] }
#             if { $Data(GridNo)>$Data(GridDepend) } {
#                set Param(NI) [expr int(($spi+$Param(DNI))/$Param(ResMX)-$Param(PI))]
#                set Param(NJ) [expr int(($spj+$Param(DNJ))/$Param(ResMY)-$Param(PJ))]
#             } else {
#                set Param(DNI) [expr $si - $spi]
#                set Param(DNJ) [expr $sj - $spj]
#             }
         #----- Check LatR,LonR translations
         if { [info exists LatR] && $Data(GridNo)<2 && ($LatR!=0.0 || $LonR!=180.0) } {
            set plat0 [expr $gridp(XLat1)+$gridp(LatR)-($spj/(1852*60)*0.5)]
            set plon0 [expr $gridp(XLon1)+($gridp(LonR)-180)-($spi/(1852*60)*0.5)]
            set plat1 [expr $gridp(XLat1)+$gridp(LatR)+($spj/(1852*60)*0.5)]
            set plon1 [expr $gridp(XLon1)+($gridp(LonR)-180)+($spi/(1852*60)*0.5)]
            
            if { [expr $Lat0+$LatR]>$plat0 }       { set LatR [expr $plat0-$Lat0] }
            if { [expr $Lon0+($LonR-180)]>$plon0 } { set LonR [expr $plon0-$Lon0+180] }
            if { [expr $Lat1+$LatR]<$plat1 }       { set LatR [expr $plat1-$Lat1] }
            if { [expr $Lon1+($LonR-180)]<$plon1 } { set LonR [expr $plon1-$Lon1+180] }
         }
      }

      #----- Check FFT constraint
      if { $Param(Type)=="ZE"  } {    
          set Param(NI) [Grid::CheckFFT $Param(NI) $Param(FFT)]
      }
      
      if { $Param(GetNIJ) } {
      } else {
         #----- If not first grid, we have to keep the same center
         if { $Data(GridNo)>0 } {
            set Lat0 [expr $Param(XLat1)-($Param(NJ)*$Param(ResLLY))*0.5]
            set Lon0 [expr $Param(XLon1)-($Param(NI)*$Param(ResLLX))*0.5]
         } 
         set Lat1 [expr $Lat0+$Param(NJ)*$Param(ResLLY)]
         set Lon1 [expr $Lon0+$Param(NI)*$Param(ResLLX)]
      }
      
      set Param(XLat1) [expr ($Lat0+$Lat1)*0.5]
      set Param(XLon1) [expr ($Lon0+$Lon1)*0.5]
   }
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
      scan $GridInfo "%s %i %i %f %f %f %f %f %f" Param(Type) Param(NI) Param(NJ) Param(Lat0) Param(Lon0) Param(Lat1) Param(Lon1) Param(ResMX) Param(ResLLX)
   }

   if { $Param(LockCenter) } {
      Grid::Center $Param(XLat1) $Param(XLon1) False
   }
   if { [string match "PS*" [lindex $Param(Type) 0]] || ($Param(Lat0)!=$Param(Lat1) && $Param(Lon0)!=$Param(Lon1)) } {

      switch $Param(Type) {
         "PS"    { Grid::CreatePS  $Param(Lat0) $Param(Lon0) $Param(ResMX) $Param(ResMY) $Param(NI) $Param(NJ) $ID }
         "PS_S"  -
         "PS_N"  { Grid::CreatePS  $Param(Lat0) $Param(Lon0) $Param(ResMX) $Param(ResMY) $Param(NI) $Param(NJ) $ID }
         "LL"    { Grid::CreateL   $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResLLX) $Param(ResLLY) $ID }
         "ZL"    { Grid::CreateZL  $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResLLX) $Param(ResLLY) $ID }
         "ZE"    { Grid::CreateZE  $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(LatR) $Param(LonR) $Param(ResLLX) $Param(ResLLY) $Param(Angle) $ID }
         "UTM"   { Grid::CreateUTM $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResLLX) $Param(ResLLY) $ID }
      }
      set Param(GridInfo) [format "$Param(Type) $Param(NI) $Param(NJ) %.7f %.7f %.7f %.7f %.2f %.7f" $Param(Lat0) $Param(Lon0) $Param(Lat1) $Param(Lon1) $Param(ResMX) $Param(ResLLX)]

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
     "deg," { set Param(ResLLX) $res;             set Param(ResMX)  [expr $Param(ResLLX)*$Param(LL2M)] }
     "m,"   { set Param(ResMX)  $res;             set Param(ResLLX) [expr $Param(ResMX)/$Param(LL2M)] }
     "km,"  { set Param(ResMX)  [expr $res*1000]; set Param(ResLLX) [expr $Param(ResMX)/$Param(LL2M)] }
   }
   set Param(ResLLY) $Param(ResLLX)
   set Param(ResMY)  $Param(ResMX)
   
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
      if { [expr ($Param(NI)*$Param(ResLLX))>=(360-$Param(ResLLX))] } {
          set Param(Lon0) -180
          set Param(Lon1) 180
          set Param(Lat0) -90
          set Param(Lat1) 90
      } else {
          set center True
          set Param(Lon0) 0
          set Param(Lon1) [expr $Param(ResLLX)*$Param(NI)]
          set Param(Lat0) 0
          set Param(Lat1) [expr $Param(ResLLY)*$Param(NJ)]
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
   variable Data

   set xg3 $Res
   set lat [expr $Lat+$Data(LatM)]
   set lon [expr $Lon+$Data(LonM)]

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
#   <ResX>   : Resolution en degree
#   <ResY>   : Resolution en degree
#   <ID>     : Identificateur du champs qui sera cree
#
# Retour:
#   <ID>     : Identificateur du champs cree
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::CreateL { Lat0 Lon0 Lat1 Lon1 ResX ResY  { ID MODELGRID } } {
   variable Param
   variable Data

   Grid::CheckBBoxOrder
   Grid::CheckNIJ

   fstdfield create ${ID} $Param(NI) $Param(NJ) 1 $Param(Data)
   fstdfield define ${ID} -NOMVAR "GRID" -ETIKET "GRID" -TYPVAR X -GRTYP L $Lat0 $Lon0 $ResY $ResX

   set Param(PGSM) [format "LATLON $Param(NI) $Param(NJ) %.7f %.7f %.7f %.7f" $Lat0 $Lon0 $ResY $ResX]

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
#   <ResX>   : Resolution en degree
#   <ResY>   : Resolution en degree
#   <ID>     : Identificateur du champs qui sera cree
#
# Retour:
#   <ID>     : Identificateur du champs cree
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------
proc Grid::CreateLZ { Lat0 Lon0 Lat1 Lon1 ResX ResY { ID MODELGRID } } {
   Grid::CreateZL $Lat0 $Lon0 $Lat1 $Lon1 $ResX $ResY $ID
}
proc Grid::CreateZL { Lat0 Lon0 Lat1 Lon1 ResX ResY { ID MODELGRID } } {
   variable Param
   variable Data

   Grid::CheckBBoxOrder
   Grid::CheckNIJ

   fstdfield free ${ID} ${ID}TIC ${ID}TAC
   fstdfield create ${ID}TIC $Param(NI) 1 1
   fstdfield create ${ID}TAC 1 $Param(NJ) 1

   fstdfield define ${ID}TIC -NOMVAR ">>" -ETIKET "GRID" -TYPVAR X -GRTYP L 0 0 1.0 1.0
   fstdfield define ${ID}TAC -NOMVAR "^^" -ETIKET "GRID" -TYPVAR X -GRTYP L 0 0 1.0 1.0

   #----- Compute tic grid coordinates.
   set lon $Lon0
   for { set i 0 } { $i < $Param(NI) } { incr i } {
      fstdfield stats ${ID}TIC -gridvalue $i 0 $lon
      set lon [expr $lon+$ResX]
   }

   #----- Compute tac grid coordinates.
   set lat $Lat0
   for { set j 0 } { $j < $Param(NJ) } { incr j } {
      fstdfield stats ${ID}TAC -gridvalue 0 $j $lat
      set lat [expr $lat+$ResY]
   }
   
   #----- Create the grid ans assign the tic/tac
   fstdfield create ${ID} $Param(NI) $Param(NJ) 1 $Param(Data)
   fstdfield define ${ID} -NOMVAR "GRID" -ETIKET "GRID" -TYPVAR X -GRTYP ZL
   fstdfield define ${ID} -positional ${ID}TIC ${ID}TAC

   set Param(PGSM) ""

   return ${ID}
}

proc Grid::CreateZLFromCenter { LatC LonC NI NJ ResX ResY { ID MODELGRID } } {
   variable Param
   variable Data

#   Grid::CheckBBoxOrder
#   Grid::CheckNIJ

   set Param(NI) $NI
   set Param(NJ) $NJ
   
   fstdfield free ${ID} ${ID}TIC ${ID}TAC
   fstdfield create ${ID}TIC $Param(NI) 1 1
   fstdfield create ${ID}TAC 1 $Param(NJ) 1

   fstdfield define ${ID}TIC -NOMVAR ">>" -ETIKET "GRID" -TYPVAR X -GRTYP L 0 0 1.0 1.0
   fstdfield define ${ID}TAC -NOMVAR "^^" -ETIKET "GRID" -TYPVAR X -GRTYP L 0 0 1.0 1.0

   #----- Compute tic grid coordinates.
   set lon [expr $LonC-$ResX*$Param(NI)/2]
   for { set i 0 } { $i < $Param(NI) } { incr i } {
      fstdfield stats ${ID}TIC -gridvalue $i 0 $lon
      set lon [expr $lon+$ResX]
   }

   #----- Compute tac grid coordinates.
   set lat [expr $LatC-$ResY*$Param(NJ)/2]
   for { set j 0 } { $j < $Param(NJ) } { incr j } {
      fstdfield stats ${ID}TAC -gridvalue 0 $j $lat
      set lat [expr $lat+$ResY]
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
#   <ResX>   : Resolution en degres
#   <ResY>   : Resolution en degres
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

proc Grid::CreateZE { Lat0 Lon0 Lat1 Lon1 LatR LonR ResX ResY Angle { ID MODELGRID } { Check True } } {
   variable Param
   variable Data

   if { $Check } {
      Grid::CheckBBoxOrder
      Grid::CheckNIJ
   }

   set ll [projection function $Page::Data(Frame) -circle $Param(XLat1) $Param(XLon1) [expr $ResX*1852.0*60*0.75*$Param(NI)] [expr $Angle-90.0]]
   set Param(XLat2) [lindex $ll 0]
   set Param(XLon2) [lindex $ll 1]
      
   fstdfield free ${ID} 
   georef free ${ID}
   
   #----- Create the grid 
   catch { georef create ${ID} }
   georef define ${ID} -rpn $Param(NI) $Param(NJ) $ResX $ResY $Param(XLat1) $Param(XLon1) $Param(XLat2) $Param(XLon2) $LatR $LonR $Param(MaxCFL)

   #----- Get size, it is possible that the grid build algorithm adjusts the nixnj
   set sz [georef define ${ID} -size]
   set Param(RNI) [lindex $sz 0]
   set Param(RNJ) [lindex $sz 1]
   set Param(PI)  [expr $Param(RNI)-$Param(NI)]
   set Param(PJ)  [expr $Param(RNJ)-$Param(NJ)]
  
   fstdfield create ${ID} $Param(RNI) $Param(RNJ) 1 $Param(Data)
   fstdfield define ${ID} -georef ${ID} -NOMVAR "GRID" -ETIKET "GRID" -TYPVAR X -GRTYP ZE

   set di  [expr $Param(PI)]
   set dj  [expr $Param(PJ)]
   set dni [expr $Param(RNI)-$Param(PI)]
   set dnj [expr $Param(RNJ)-$Param(PJ)]
   
   #----- Mark the inside grid
   catch { vexpr - ${ID}(($di,$dni),($dj,$dnj))=1 }
   
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
#   <ResX>   : Resolution en degree
#   <ResY>   : Resolution en degree
#   <ID>     : Identificateur du champs qui sera cree
#
# Retour:
#   <ID>     : Identificateur du champs cree
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Grid::CreateUTM { Lat0 Lon0 Lat1 Lon1 ResX ResY { ID MODELGRID } } {
   variable Param
   variable Data
   variable Msg
   variable Lbl

   Grid::CheckBBoxOrder

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

   set ni [expr int(ceil(([lindex $xy1 0] - [lindex $xy0 0])/$ResX))+1]
   set nj [expr int(ceil(([lindex $xy1 1] - [lindex $xy0 1])/$ResY))+1]

   if { $Param(SizeWarn) && [expr $ni*$nj]>$Param(NIJWarn) } {
      if { [info exists ::tk_version] && ![Dialog::Default . 400 WARNING $Msg(Size) "\n\n\t$ni x $nj" 0 $Lbl(No) $Lbl(Yes)] } {
         return ""
      }
      set Param(SizeWarn) False
   }

   set Param(NI) $ni
   set Param(NJ) $nj

   set scalex    [expr abs($ResX)]
   set scaley    [expr -1.0 * abs($ResY)]
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
 
   switch -glob [fstdfield define $ID -GRTYP] {
      "Z*"    { fstdfield define ${ID} -DATEO $dateo -TYPVAR X -IG1 $IP1 -IG2 $IP2 -IG3 $IP3 -IG4 0 }
      "W"     { fstdfield define ${ID} -DATEO $dateo -TYPVAR X -IG1 $IP1 -IG2 $IP2 -IG3 $IP3 -IG4 0 }
      default { fstdfield define ${ID} -DATEO $dateo }
   }

   fstdfield writegeo $ID $FILE $ETIKET
   
   if { $Grid } {
      fstdfield write ${ID} $FILE -8 True
   }
}

