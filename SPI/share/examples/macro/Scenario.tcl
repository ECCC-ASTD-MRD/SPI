#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de script.
# Fichier  : Scenario.tcl
# Creation : Janvier 2018 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Scenario pour produire une animation
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::Scenario {} {
   global env
   variable Param
   variable Data

   set Param(In)   ""
   set Param(Info) { "Scenario" "Scenario" }
   
   set Param(Counter)    5177  ;# Frame counter
   set Param(Transition) 20    ;# Number of transition frame
   set Param(Save)       False ;# Save frames
   set Param(PLabel)     ""
   set Param(PParam)     ""
   set Param(PVar)       ""
   set Param(Topo)       True
   set Param(Loop)       True
   set Param(Width)      1920
   set Param(Height)     1080
   set Param(Scenarios)  { ScenarioAll.tcl }

#   set Param(Models)    List of models to include, then for each model
#      set Param(File...)   RPN file for the model
#      set Param(Label...)  Label for the model
#      set Param(To...)     Camera name to use to zoom to model
#      set Param(Fly...)    Flight path around the model                                           [Optional]
#      set Param(Rotate...) Number of degrees by which to rotate the globe 360                     [Optional]
#      set Param(Time...)   Time range to animate { "YYYY/MM/DD HH:MM" "YYYY/MM/DD HH:MM" }        [Optional]
#      set Param(Var...)    FSTD variable list
#      set Param(OGR...)    OGR data to display                                                    [Optional]
#      set Param(Loop...)   Number of loop over time
#      set Param(Desc...)   Long description of the model                  
#      set Param(Param...)  FSTD configuration parameter for each var 

   font create LARGE  -family arial -size -20 -weight bold
   font create MEDIUM -family arial -size -20 -weight bold
   font create LABEL  -family helvetica -size -12 -weight bold
}

proc Macro::Scenario::Execute { } {
   variable Param

   #----- Setup product
   if { $Param(Save) } { glrender -wait True }

   Page::Size $Page::Data(Frame)  $Param(Width) $Param(Height)

   set ProjCam::Param(Function) QUADRATIC
   set Viewport::Map(Delay) 1000.0
   set Viewport::Resources(FillCoast) ""
   set Viewport::Resources(FillLake)  ""
#   projection configure $Page::Data(Frame)  -draw 1 -mapcoord 0 -mapriver 0 -maplake 0 -sun True -date [clock scan "21/06/2018 18:00" -timezone :UTC]
   projection configure $Page::Data(Frame) -draw 1 -mapcoord 0 -mapriver 0 -maplake 0
   
   $Page::Data(Canvas) create colorbar -x [expr [Page::CanvasWidth $Page::Data(Frame)]-100-5] -y [expr ([Page::CanvasHeight $Page::Data(Frame)]-500)/2.0] -width 100 -height 500 -tags "COLORBAR" -barsplit 0 -barside right \
      -barborder 1 -barwidth 30 -bg white -transparency 75 -showfactor False
#   $Page::Data(Canvas) create colorbar -x [expr [Page::CanvasWidth $Page::Data(Frame)]-500-5] -y 5 -width 500 -height 60 -tags "COLORBAR" -barsplit 0 -barside right \
#      -barborder 1 -barwidth 20 -bg white -transparency 75 -showfactor False
   set SPI::Data(ShowLOGO) True
   SPI::DrawImage $Page::Data(Frame) LOGO 10 10 nw 1 0 0

   CVProgressBar::Create $Page::Data(Frame) 5 38 20 330

   $Page::Data(Canvas) itemconfigure $Page::Data(VP) -bg #000000
   $Page::Data(Canvas) create rectangle -10 -10 -10 -10 -fill white -transparency 7 -width 1 -outline black -tag BDESC
   $Page::Data(Canvas) create rectangle 5 5 335 33 -fill white -transparency 75 -width 1 -outline black
   $Page::Data(Canvas) create text 8 39 -font MEDIUM -fill #ff0000 -anchor nw -tag LABEL
   $Page::Data(Canvas) create text 8 39 -font MEDIUM -fill #ff0000 -anchor nw -tag PLABEL
   $Page::Data(Canvas) create text [expr [Page::CanvasWidth $Page::Data(Frame)]/2] [expr [Page::CanvasHeight $Page::Data(Frame)]-8] -font MEDIUM -fill #ff0000 -anchor s -justify center -tag DESC
   
   set Param(OSM) [Mapper::DepotWare::TMS::Load OpenStreetMap 2]
   if { $Param(Topo) } {
      gdalband configure $Param(OSM) -topography INTERNAL -texsample 32
   }
   
   #----- Let's start the show
   foreach scenario $Param(Scenarios) {
      Macro::Scenario::Shoot $scenario
   }
   
   #----- Produce mp4
#   file delete Scenario.mp4
#   exec cat *.jpg | avconv -f image2pipe -r 10 -c:v mjpeg -i - -map 0 -c:v mpeg4 -r 20 -pix_fmt yuv420p -qscale 1 Scenario.mp4
}
   
proc Macro::Scenario::Shoot { Scenario } {
   variable Param
   
   uplevel #0 source $Scenario

   set lstv    {}
   set dtran   100
   set nbmodel [llength $Param(Models)]
   
   #----- Back to globe for looping animation   
   if { $Param(Loop) } {
      lappend Param(Models) [lindex $Param(Models) 0]
   }
   
   foreach model $Param(Models) {
   
      $Page::Data(Canvas) itemconfigure DESC   -text $Param(Desc$model)
      $Page::Data(Canvas) itemconfigure LABEL  -text $Param(Label$model)
      $Page::Data(Canvas) itemconfigure PLABEL -text $Param(PLabel)
      set bbox [$Page::Data(Canvas) bbox DESC]
      set bbox [list [expr [lindex $bbox 0]-3] [expr [lindex $bbox 1]-3] [expr [lindex $bbox 2]+3] [expr [lindex $bbox 3]+1]]
      $Page::Data(Canvas) coords BDESC $bbox

      CVProgressBar::Set $Page::Data(Frame) 0

      fstdfile open MODEL read $Param(File$model)
      
      #----- Extract available dates
      set dates { }
      foreach v $Param(Var$model) {
         eval lappend dates [fstdfile info MODEL DATEV $v]
      }
      set dates [lsort -increasing -unique $dates]
      
      #----- If a time range is specified
      if { [info exists Param(Time$model)] } {
         set t0 [clock scan [lindex $Param(Time$model) 0] -format "%d/%m/%Y %H:%M" -timezone :UTC]
         set t1 [clock scan [lindex $Param(Time$model) 1] -format "%d/%m/%Y %H:%M" -timezone :UTC]
         set new {}
         foreach date $dates { 
            if { $date>=$t0 && $date<=$t1 } {
               lappend new $date
            }
         }
         set dates $new
      }
      
      set f 0
      for { set l 0 } { $l<$Param(Loop$model) } { incr l } {
         foreach date $dates {
            if { [llength $dates]==1 } {
               Macro::Scenario::Disappears $lstv
               set f 20
            }
            
            Viewport::UnAssign $Page::Data(Frame) $Page::Data(VP) "" -1
            if { [info exists Param(OGR$model)] } {
               Viewport::Assign $Page::Data(Frame) $Page::Data(VP) $Param(OGR$model) -1 
            }
            CVProgressBar::Incr $Page::Data(Frame) [expr 1.0/([llength $dates]*$Param(Loop$model))]
      
            foreach v $Param(Var$model) {
               #----- if the field does not exists, keep previous
               catch { fstdfield read FLD$v MODEL [fstdstamp fromseconds $date] "" -1 -1 -1 "" "$v" }
            }
            
            if { !$f } {
               foreach v $Param(Var$model) p $Param(Param$model) {
                  eval fstdfield configure FLD$v $p 
                  set dtran [fstdfield configure FLD$v -transparency]
                  eval fstdfield configure FLD$v -transparency 0
               }
               set a 100
          
               set ProjCam::Param(Proc) {
                  if { [expr [$Page::Data(VP) -distpix]<128] } {
                     gdalband configure $Param(OSM) -topography NONE
                     projection configure $Page::Data(Frame) -draw False
                  } else {
                     gdalband configure $Param(OSM) -topography INTERNAL                 
                     projection configure $Page::Data(Frame) -draw True
                  }
                  if { [expr [$Page::Data(VP) -distpix]<8] } {
                     glrender -zbuffer True
                  } else {
                     glrender -zbuffer False
                  }
                  #----- New data
                  $Page::Data(Canvas) itemconfigure LABEL -transparency  [expr 100-$a]
                  $Page::Data(Canvas) itemconfigure DESC  -transparency  [expr 100-$a]
                  $Page::Data(Canvas) itemconfigure BDESC -transparency  [expr min(100-$a,75)]
                  set lstv {}
                  foreach v $Param(Var$model) {
                     fstdfield configure FLD$v -transparency [expr min(100-$a,$dtran)]
                     lappend lstv FLD$v
                  }
                  $Page::Data(Canvas) itemconfigure COLORBAR -data $lstv -transparency [expr min(100-$a,75)]
                  
                  #----- Old data
                  foreach v $Param(PVar) p $Param(PParam) {
                     if { [fstdfield is PFLD$v] } {
                        eval fstdfield configure PFLD$v $p -transparency $a
                        lappend lstv PFLD$v
                     }
                  }
                  $Page::Data(Canvas) itemconfigure PLABEL -transparency $a
                  set a [expr $a<=0?0:($a-2)]
                  Viewport::Assign $Page::Data(Frame) $Page::Data(VP) $lstv -1
                  Macro::Scenario::Print
               }
               ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) $Param(To$model) [expr $Param(Counter)==0]
               if { !$nbmodel } { break }
            } else {
               set lstv {}
               foreach v $Param(Var$model) p $Param(Param$model) {
                  eval fstdfield configure FLD$v $p -transparency $dtran
                  lappend lstv FLD$v
               }
               $Page::Data(Canvas) itemconfigure COLORBAR -data $lstv 
               if { [llength $dates]==1 } {
                  Macro::Scenario::Appears $lstv fstdfield $Param(Label$model) $Param(Desc$model)
               }
               $Page::Data(Canvas) itemconfigure PLABEL -transparency 0
               $Page::Data(Canvas) itemconfigure LABEL  -transparency 100
               $Page::Data(Canvas) itemconfigure DESC   -transparency [expr 100-($f-20)*5]
               $Page::Data(Canvas) itemconfigure BDESC  -transparency [expr min(75,100-($f-20)*5)]
            }
           
            #----- Rotates the globe if need be        
            if { [info exists Param(Rotate$model)] } {
               set Viewport::Map(Lon) [expr fmod($Viewport::Map(Lon)+$Param(Rotate$model),360)]
               Viewport::Rotate $Page::Data(Frame) $Viewport::Map(Lat) $Viewport::Map(Lon)
            }
            Viewport::Assign $Page::Data(Frame) $Page::Data(VP) $lstv
            Macro::Scenario::Print
            incr f
            if { !$nbmodel } { break }
         }
         if { !$nbmodel } { break }
         
         #----- Process flight plan is any
         if { [info exists Param(Fly$model)] } {
            projcam define $Page::Data(Frame) -path $Param(Fly$model)
            for { set pos 0.0 } { $pos<[expr [llength $Param(Fly$model)]-1] } { set pos [expr $pos+0.025] } {
               set ll [projcam define $Page::Data(Frame) -fly $pos]
               projection configure $Page::Data(Frame) -location [lindex $ll 0] [lindex $ll 1]

               Macro::Scenario::Print
            }       
         }
      }
      foreach v $Param(Var$model) {
         catch { fstdfield copy PFLD$v FLD$v }
      }
      set Param(PLabel) $Param(Label$model)
      set Param(PParam) $Param(Param$model)
      set Param(PVar)   $Param(Var$model)
      fstdfile close MODEL

      Macro::Scenario::Pause
      incr nbmodel -1
   } 

   #----- Non standard animation
   set ProjCam::Param(Proc) {
      Macro::Scenario::Print
   }
   
#    $Page::Data(Canvas) itemconfigure DESC -text ""
#    $Page::Data(Canvas) itemconfigure LABEL -text ""
#    CVProgressBar::Set $Page::Data(Frame) 0
#       
#       ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) $Param(To[lindex $Param(Models) 0])
#       Viewport::UnAssign $Page::Data(Frame) $Page::Data(VP)
#       Macro::Scenario::Print
#    
   eval fstdfield free [fstdfield all]
   
   Log::Print INFO "Number total of frames: $Param(Counter)"
}

proc Macro::Scenario::Print { } {
   variable Param

   $Page::Data(Canvas) itemconf $Page::Data(VP) -update True
   update idletasks
   incr Param(Counter)
   
   puts "Frame #$Param(Counter)"
   if { $Param(Save) } { 
      PrintBox::Save $Page::Data(Frame) 0 0  [Page::CanvasWidth $Page::Data(Frame)] [Page::CanvasHeight $Page::Data(Frame)] /fs/cetus/fs2/ops/cmoe/afsr005/Scenario/img[format %05i $Param(Counter)] jpg
   }
}

proc Macro::Scenario::Pause { { Delay 0 } } {
   variable Param
   
   set Delay [expr $Delay?$Delay:$Param(Transition)]
   
   for { set f 0 } { $f <= $Delay } { incr f } { 
      Macro::Scenario::Print
   }
}

proc Macro::Scenario::Appears { Ids { Type fstdfield } { Label "" } { Desc "" } } {

   foreach id $Ids {
      eval $Type configure $id -transparency 0
   }
   Viewport::Assign $Page::Data(Frame) $Page::Data(VP) $Ids
   set bbox [$Page::Data(Canvas) bbox DESC]
   set bbox [list [expr [lindex $bbox 0]-3] [expr [lindex $bbox 1]-3] [expr [lindex $bbox 2]+3] [expr [lindex $bbox 3]+1]]
   $Page::Data(Canvas) coords BDESC $bbox
   
   for { set a 0 } { $a<90 } { incr a 5 } {
      foreach id $Ids {
         eval $Type configure $id -transparency $a
      }
      if { $Label!="" } {
         $Page::Data(Canvas) itemconfigure LABEL -text $Label -transparency $a
      }
      if { $Desc!="" } {
         $Page::Data(Canvas) itemconfigure DESC -text $Desc -transparency $a
         $Page::Data(Canvas) itemconfigure BDESC -transparency [expr min($a,75)]
      }
#      $Page::Data(Canvas) itemconfigure COLORBAR -transparency $a
      $Page::Data(Canvas) itemconfigure $Page::Data(VP) -update True
      update idletasks
      Macro::Scenario::Print
   }
}

proc Macro::Scenario::Disappears { Ids { Type fstdfield } { Label "" } { Desc "" } } {

   for { set a 95 } { $a >0 } { incr a -5 } {
      foreach id $Ids {
         eval $Type configure $id -transparency $a
      }
      if { $Label!="" } {
         $Page::Data(Canvas) itemconfigure LABEL -text $Label -transparency $a
      }
      if { $Desc!="" } {
         $Page::Data(Canvas) itemconfigure DESC  -text $Desc -transparency $a
         $Page::Data(Canvas) itemconfigure BDESC -transparency [expr min($a,75)]
      }
#      $Page::Data(Canvas) itemconfigure COLORBAR -transparency $a
      $Page::Data(Canvas) itemconfigure $Page::Data(VP) -update True
      update idletasks
      Macro::Scenario::Print
   }
   Viewport::UnAssign  $Page::Data(Frame) $Page::Data(VP) $Ids
}

proc Macro::Scenario::Clean { } {

}

proc Macro::Scenario::Args { } {
   global argv argc
   variable Param

   if { $argc>0 } { set Param(Save)       [lindex $argv 0] }
   if { $argc>1 } { set Param(Scenarios)  [lindex $argv 1] }
}
