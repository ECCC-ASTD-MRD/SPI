#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Exemple de scripts.
# Fichier    : TCL_Vector.tcl
# Creation   : Mars 2005 - J.P. Gauthier - CMC/CMOE
# Description: Demonstration de l'usage des appels systeme
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclSystem
package require Logger

Log::Start [info script] 0.1

set fs [lindex $argv 0]

puts "System info"
puts "  name     : [system info -name]"
puts "  arch     : [system info -arch]"
puts "  os       : [system info -os]"
puts "  release  : [system info -osrelease]"
puts "  version  : [system info -osversion]"

puts "  nbcpu    : [system info -nbcpu]"
puts "  uptime   : [system info -uptime]"
puts "  loads    : [system info -loads]"
puts "  totalmem : [system info -totalmem]"
puts "  freemem  : [system info -freemem]"
puts "  sharedmem: [system info -sharedmem]"
puts "  buffermem: [system info -buffermem]"
puts "  totalswap: [system info -totalswap]"
puts "  freeswap : [system info -freeswap]"
puts "  process  : [system info -process]"
puts "  totalhigh: [system info -totalhigh]"
puts "  freehigh : [system info -freehigh]"
puts "  memunit  : [system info -memunit]"

puts "\nFile system info for $fs"
puts "   Type      : [system filesystem $fs -type]"
puts "   Blocks    : [system filesystem $fs -blocksize -blocks -blockfree -blockused]"
puts "   Size      : [system filesystem $fs -size]"
puts "   Free      : [system filesystem $fs -free]"
puts "   Used      : [system filesystem $fs -used]"

puts "\nSystem limit for process"
puts "   CPU Time   : [system limit -cpu]"
puts "   Data size  : [system limit -data]"
puts "   Stack size : [system limit -stack]"

system limit -stack 3600
puts "   Stack new size :[system limit -stack]"

proc ManageSignal { Signal } {
   puts "Received signal: $Signal"
}

puts "System signal (pid:[pid])"
puts "  Current SIGUSR1 value: [system signal SIGUSR1 -trap ManageSignal]"
puts "  Current SIGUSR1 value: [system signal SIGUSR1]"

#vwait forever

puts "\nSystem usage for process"
after 5000
set calls [list -utime -stime -cutime -cstime -rss -shared -data -stack -minpagefault -majpagefault -swap -inblock -outblock -signal -vcswitch -ivcswitch]
eval set vals \[system usage $calls\]
foreach call $calls val $vals {
   puts "   $call : $val"
}

#system fork
#system daemonize -lock /tmp/test.pid

Log::End
