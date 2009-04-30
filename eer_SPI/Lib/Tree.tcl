#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de definitions pour les domaines des projections
# Fichier   : Tree.tcl
# Creation  : Janvier 2004 - J.P. Gauthier - CMC/CMOE
#
# Description: Structure de donnees Arbre
#
# Fonctions:
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide Tree 1.0

catch { SPI::Splash "Loading Widget Package Tree 1.0" }

namespace eval Tree { } {
   variable Data

   set Data(Id) 0
}

proc Tree::Find { Tree Node { Path { } } } {

   if { [set idx [lsearch -glob $Tree "{*} $Node"]]!=-1 } {
      lappend Path $idx
      return $Path
   } else {
      set idx -1
      foreach branch $Tree {
         set path $Path
         lappend path [incr idx] 0
         set p [Tree::Find [lindex $branch 0] $Node $path]
         if { [llength $p]>[llength $path] } {
            return $p
         }
      }
   }
}

proc Tree::Add { Tree Node { Parent "" } } {

   upvar #0 $Tree tree

   set path {}

   if { $Parent!="" } {
      set path [Tree::Find $tree $Parent]

      if { ![llength $path] } {
         puts stderr "Tree::Add Could not find parent node: $Parent"
         return 0
      }
   }
   return "$path [Tree::Put $Tree $path $Node]"
}

proc Tree::Del { Tree Node } {

   upvar #0 $Tree tree

   set path [Tree::Find $tree $Node]

   if { ![llength $path] } {
      puts stderr "Tree::Del Could not find node: $Node"
      return 0
   }

   set branch [lindex $tree [lrange $path 0 end-1]]
   set branch [lreplace $branch [lindex $path end] [lindex $path end]]
   lset tree [lrange $path 0 end-1] $branch

   return 1
}

proc Tree::Merge { Tree Branch { Parent "" } } {

   upvar #0 $Tree tree
   upvar #0 $Branch branch

   set path {}

   if { $Parent!="" } {
      set path [Tree::Find $tree $Parent]

      if { ![llength $path] } {
         puts stderr "Tree::Add Could not find parent node: $Parent"
         return 0
      }
   }
   return "$path [Tree::Put $Tree $path $branch True]"
}

proc Tree::Put { Tree Path Node { Merge False } } {

   upvar #0 $Tree tree

   if { [llength $Path] } {
      eval set leaf \[lindex \$tree $Path 0\]
      set idx [llength $leaf]

      if { $Merge } {
         set leaf [concat $leaf $Node]
      } else {
         lappend leaf [list {} $Node]
      }
      eval lset tree $Path 0 \$leaf
   } else {
      set idx [llength $tree]
      if { $Merge } {
         set tree [concat $tree $Node]
      } else {
         lappend tree [list {} $Node]
      }
   }

   return $idx
}

proc Tree::Set { Tree Path Node } {

   upvar #0 $Tree tree

   eval lset tree $Path \$Node
}

proc Tree::FromDirectory { Tree Dir { Pattern * } { Path { } } } {

   upvar #0 $Tree tree

   set files [lsort -dictionary [glob -nocomplain -tails -directory $Dir $Pattern]]

   #----- Process directories

   set idx 0
   foreach file $files {
      if { [file isdirectory $Dir/$file] } {
         Tree::Set $Tree $Path $file
         if { [llength $Path] } {
            Tree::FromDirectory $Tree $Dir/$file $Pattern [concat $Path 0 $idx]
         } else {
            Tree::FromDirectory $Tree $Dir/$file $Pattern [concat $Path $idx]
         }
         incr idx
      }
   }

   #----- Process files

   foreach file $files {
      if { ![file isdirectory $Dir/$file] } {
         Tree::Set $Tree $Path $file
      }
   }
}

proc Tree::Parse { Tree { Proc "" } { Path { } } } {

   set i  0

   foreach branch $Tree {

      eval $Proc \[lindex \$branch end\] \[concat $Path $i 1\]

      if { [llength [lindex $branch 0]] } {
         Tree::Parse [lindex $branch 0] $Proc [concat $Path $i 0]
      }
      incr i
   }
}

proc Tree::ParseBranch { Tree { Proc "" } { Level 0 } } {

   set level [expr $Level+1]

   foreach branch $Tree {

      if { $Proc!="" } {
         eval $Proc $branch
      } else {
         eval puts stderr \[format \"%[expr $Level*3]s%s\" \"\" \[lindex \$branch end\]\]
      }

      if { [llength  [lindex $branch 0]] } {
         Tree::ParseBranch [lindex $branch 0] $Proc $level
      }
   }
}

proc Tree::ParseLeaf { Tree { Proc "" } { Level 0 } } {

   set level [expr $Level+1]

   foreach branch $Tree {

      if { [llength [lindex $branch 0]] } {
         Tree::ParseLeaf [lindex $branch 0] $Proc $level
      }
      if { $Proc!="" } {
         eval $Proc [lindex $branch end]
      } else {
         eval puts stderr \[format \"%[expr $Level*3]s%s\" \"\" \[lindex \$branch end\]\]
      }
   }
}

proc Tree::Path { Tree Node { Separator " " } } {

   upvar #0 $Tree tree

   set path [Tree::Find $tree $Node]
   set str  ""

   puts stderr $path

   for { set i 0 } { $i<[llength $path] } { incr i 2 } {
      eval set str \$Separator\[lindex \$tree [lrange $path 0 end-$i] 1\]\$str
   }

   return $str
}

proc Tree::Render { Canvas Tag X Y Tree { Proc "" } { Path { } } } {
   variable Data

   set d  20
   set d2 5
   set i  0

   set x  $X
   set y  $Y

   foreach branch $Tree {

      set y0 $y

      if { $Proc!="" } {
         eval set follow \[$Proc $Canvas $x $y $Tag[incr Data(Id)] \$branch \[concat $Path $i 1\]\]
      } else {
         $Canvas create text $x $y -text [lindex $branch end] -anchor w -tag "$Tag"
         set follow 1
      }

      if { $follow>=0 } {
         if { [llength $Path] } {
            $Canvas create line [expr $x-$d] $y [expr $x-$d2] $y -fill black -width 1 -tag "$Tag ${Tag}FRAME"
         }

         set ny [incr y $d]

         if { $follow==1 && [llength [lindex $branch 0]] } {
            set y [Tree::Render $Canvas $Tag [expr $x+$d] $y [lindex $branch 0] $Proc [concat $Path $i 0]]
            $Canvas create line $x [expr $y0+$d2] $x [expr $Data(NY)-$d] -fill black -width 1 -tag "$Tag ${Tag}FRAME"
         }
      }
      incr i
   }

   #----- Height of the branch

   set Data(NY) $y

   $Canvas lower ${Tag}FRAME

   return $y
}

#----- Test

#set ShapeTree {}

#Tree::Add ShapeTree Carre1
#Tree::Add ShapeTree Carre2
#Tree::Add ShapeTree Rond1 Carre1
#Tree::Add ShapeTree Rond2 Carre1
#Tree::Add ShapeTree Text1 Rond1
#Tree::Add ShapeTree Text2 Rond2

#Tree::ParseBranch $ShapeTree

#set DirTree {}

#Tree::FromDirectory DirTree ../../
#Tree::ParseBranch $DirTree

#set MetTree {}
#Tree::Add MetTree diag
#Tree::Add MetTree prog

#Tree::Add MetTree regeta prog
#Tree::Add MetTree regetae prog
#Tree::Add MetTree regetaw prog

#Tree::Add MetTree glbeta prog
#Tree::Add MetTree 00Z glbeta
#Tree::Add MetTree 12Z glbeta

#set ChronosTree {}
#Tree::Add ChronosTree a
#Tree::Add ChronosTree b
#Tree::Add ChronosTree c a
#puts $ChronosTree
#
#Tree::Merge MetTree ChronosTree diag
#puts $MetTree
#Tree::ParseBranch $MetTree
#puts stderr [Tree::Path MetTree 12Z /]
