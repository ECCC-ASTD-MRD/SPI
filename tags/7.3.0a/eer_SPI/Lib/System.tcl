#!/bin/sh
# Let's start up with the proper wish\
DIR=$0
# Let's start up with the proper wish\
exec ${DIR%System.tcl}/../wish "$0" "$@"

#----- Check for the nvidia file access problem

if { [file exists /dev/nvidiactl] && ![file readable /dev/nvidiactl] } {
   puts 0
   exit 0
}

#----- Try to load the extension

set GDefs(Dir)  [file dirname [info script]]/..
set GDefs(Arch) $tcl_platform(os)
set GDefs(Ext)  [info sharedlibextension]

load $GDefs(Dir)/Shared/$GDefs(Arch)/libTkglCanvas$GDefs(Ext) glCanvas

namespace eval System {}

#----- Test for hardware implementation

proc System::OpenGL { } {
   if { [catch { glcanvas .testcanvas } ] } {
      puts 0
   } else {
      puts 1
   }
   exit 0
}

System::OpenGL
