#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"

package require TclData
package require Logger

# note : by default, R calculates the sample variance and we want the population variance (we want to divide by N instead of R's N-1).
set R_CMDS {
   nb    "nb    <- length(x)"
   medx  "medx  <- median(x)"
   medy  "medy  <- median(y)"
   sumx  "sumx  <- sum(x)"
   sumy  "sumy  <- sum(y)"
   minx  "minx  <- min(x)"
   miny  "miny  <- min(y)"
   maxx  "maxx  <- max(x)"
   maxy  "maxy  <- max(y)"
   avgx  "avgx  <- mean(x)"
   avgy  "avgy  <- mean(y)"
   varx  "varx  <- var(x) * (length(x)-1)/length(x)"
   vary  "vary  <- var(y) * (length(y)-1)/length(y)"
   rmse  "rmse  <- sqrt(mean((y-x)^2))"
   nrmse "nrmse <- sqrt(mean((y-x)^2)) / mean(x)"
   sdevx "sdevx <- sd(x) * sqrt((length(x)-1)/length(x))"
   sdevy "sdevy <- sd(y) * sqrt((length(y)-1)/length(y))"
   cor   "cor   <- cor(x,y)"
   cov   "cov   <- cov(x,y) * (length(x)-1)/length(x)"
   regb  "regb  <- summary(lm(y~x))$coefficients[2]"
   rega  "rega  <- summary(lm(y~x))$coefficients[1]"
   errb  "errb  <- summary(lm(y~x))$coefficients[4]"
   erra  "erra  <- summary(lm(y~x))$coefficients[3]"
   ssx   "ssx   <- sum(x^2)"
   ssy   "ssy   <- sum(y^2)"
   ssxy  "ssxy  <- sum(x*y)"
   mb    "mb    <- sum(y-x)/length(x)"
   nmb   "nmb   <- sum(y-x)/sum(x) * 100"
   me    "me    <- sum(abs(y-x))/length(x)"
   nme   "nme   <- sum(abs(y-x))/sum(x) * 100"
   mnb   "mnb   <- sum((y-x)/x, na.rm=TRUE)/length(x)"
   mne   "mne   <- sum(abs((y-x)/x), na.rm=TRUE)/length(x)"
   mfb   "mfb   <- sum((y-x)/(y+x))*200.0/length(x)"
   mfe   "mfe   <- sum(abs(y-x)/(y+x), na.rm=TRUE)*200/length(x)"
   lmnb  "lmnb  <- sum(log(y/x), na.rm=TRUE)/length(x)-1"
   lmne  "lmne  <- sum(abs(log(y/x)), na.rm=TRUE)/length(x)-1"
   mre   "mre   <- sum(abs(1-y/x))/length(x)"
   maxb  "maxb  <- max(y-x)"
   maxe  "maxe  <- max(abs(y-x))"
   maxre "maxre <- max(abs(1-y/x))"
   na    "na    <- length(xi)-length(x)"
   rna   "rna   <- length(x)/length(xi)"
   nmse  "nmse  <- sum((y-x)^2) / (mean(x)*mean(y)*length(x))"
   gmb   "t<-y/x>0; gmb <- exp( sum(log(y[t]/x[t])) / (length(which(t))+length(which( x==0 & y==0 ))) )"
   gmv   "t<-y/x>0; gmv <- exp( sum(log(y[t]/x[t])^2) / (length(which(t))+length(which( x==0 & y==0 ))) )"
   foex  "foex  <- ((length(which(x<y))+length(which(x==y))/2.0)/length(x) - 0.5) * 100"
   fa2   "t<-y/x; fa2 <- length(which( 0.5<=t & t<=2.0 ))/length(x) * 100"
   fa5   "t<-y/x; fa5 <- length(which( 0.2<=t & t<=5.0 ))/length(x) * 100"
   fa10  "t<-y/x; fa10 <- length(which( 0.1<=t & t<=10.0 ))/length(x) * 100"
   fb    "fb    <- 2.0*(mean(y)-mean(x))/(mean(y)+mean(x))"
   nad   "nad   <- sum(abs(x-y))/sum(x+y)"
   fms   "minxy<-pmin(x,y); fms <- sum(minxy) / sum(pmax(x-minxy,0) + pmax(y-minxy,0) + minxy) * 100"
   osf   "t<-sum(pmin(x,y)); osf <- sqrt( (1-t/sum(x))^2 + (1-t/sum(y))^2 )"
   pcc   "pcc   <- cor(x, y, method='pearson')"
   ksp   "ksp   <- ks.test(x,y)$statistic * 100"
   rank  "rpcc<-cor(x, y, method='pearson');rfb<-(mean(y)-mean(x))/(mean(y)+mean(x));rminxy<-pmin(x,y);rfms<-sum(minxy)/sum(pmax(x-minxy,0)+pmax(y-minxy,0)+minxy);rfa2<-length(which(0.5<=y/x&y/x<=2))/length(x);rksp<-ks.test(x,y)$statistic;rnad<-sum(abs(x-y))/sum(x+y); rank <- rpcc^2 + (1-abs(rfb)) + rfms + rfa2 + (1-rksp) + (1-rnad)"
   nbeq  "nbeq  <- length(which( x == y ))"
   nbgt  "nbgt  <- length(which( x < y ))"
   nblt  "nblt  <- length(which( x > y ))"
   nbfa  "nbfa  <- length(which( x==0 & y!=0 ))"
   nbmi  "nbmi  <- length(which( x!=0 & y==0 ))"
   nbnp  "nbnp  <- length(which( x==0 & y==0 ))"

   sdev  "sdev  <- sd(x) * sqrt((length(x)-1)/length(x))"
   var   "var   <- var(x) * (length(x)-1)/length(x)"
   med   "med   <- median(x)"
   uniq  "uniq  <- length(unique(x))"
   sum   "sum   <- sum(x)"
   min   "min   <- min(x)"
   max   "max   <- max(x)"
   avg   "avg   <- mean(x)"
}

set VARS_COMBO { nb medx medy sumx sumy minx miny maxx maxy avgx avgy
                 varx vary ssx ssy ssxy rmse sdev sdevx sdevy cor cov
                 regb rega errb erra mb nmb nme me mnb mne mfb mfe lmnb
                 lmne mre maxb maxe maxre nrmse na rna nmse gmb gmv foex
                 fa2 fa5 fa10 fb nad fms osf pcc ksp rank nbeq nbgt nblt
                 nbfa nbmi nbnp }
set VARS_ALONE { med uniq sum min max avg var }

proc ReadFromChan { Chan Var {Flag ""} } {
   append $Var [read $Chan]
   if { [eof $Chan] } {
      fileevent $Chan readable ""
      if { $Flag != "" } {
         set $Flag done
      }
   }
}

proc GetStatsR { Svars X Y } {
   global R_CMDS

   #----- Setup the R connection

   lassign [chan pipe] errfd errps
   set rpd [open "|R --slave --no-save 2>@$errps" r+]
   fconfigure $rpd -buffering line -blocking 0
   chan configure $errfd -buffering line -blocking 0

   unset -nocomplain ::R_DONE
   set ::R_OUT ""
   set ::R_ERR ""

   fileevent $rpd readable [list ReadFromChan $rpd ::R_OUT ::R_DONE]
   fileevent $errfd readable [list ReadFromChan $errfd ::R_ERR]

   #----- Setup the data

   if { [llength $Y] } {
      puts $rpd "xi <- c([join $X ","])"
      puts $rpd "yi <- c([join $Y ","])"

      puts $rpd "valid <- !is.na(xi) & !is.na(yi)"
      puts $rpd "x <- xi\[valid\]"
      puts $rpd "y <- yi\[valid\]"
   } else {
      puts $rpd "xi <- c([join $X ","])"
      puts $rpd "x <- xi\[!is.na(xi)\]"
   }

   #----- Calculate the stats

   foreach s $Svars {
      puts $rpd [dict get $R_CMDS $s]
   }
   puts $rpd "cat(sprintf('%.17G', c([join $Svars ,])), '\\n', sep=',', file='')"
   close $rpd w

   #----- Drain out/err streams

   vwait ::R_DONE

   if { $::R_ERR != "" } {
      Log::Print WARNING "Received the following from R :\n\n$::R_ERR"
   }
   Log::Print DEBUG "R results :\n\n($::R_OUT)"

   #----- Close the R connection

   catch {close $rpd}
   catch {chan close $errfd}

   return [split [string trim $::R_OUT \n,] ,]
}

proc GetStatsVexpr { Svars FldX FldY } {
   set res {}

   if { $FldY != "" } {
      vexpr NIL sall($FldX, $FldY)

      foreach s $Svars {
         lappend res [vexpr NIL s${s}()]
      }
   } else {
      foreach s $Svars {
         lappend res [vexpr NIL s${s}($FldX)]
      }
   }

   return $res
}

proc RunTest { Svars FldX FldY } {
   #----- Read the matrix

   fstdfield stats $FldX -matrix arr1
   if { $FldY != "" } {
      fstdfield stats $FldY -matrix arr2
   } else {
      set arr2 {}
   }

   #----- Transform the arrays into lists

   foreach i {1 2} {
      set dat$i {}
      foreach key [lsort -dictionary [array names arr$i]] {
         lappend dat$i [subst \$arr${i}($key)]
      }
      array unset arr$i
      #set fd [open "dat$i.txt" "w"]; puts $fd [join [subst \$dat$i] \n]; close $fd
   }

   #----- Calculate the values

   set res_r [GetStatsR $Svars $dat1 $dat2]
   set res_v [GetStatsVexpr $Svars $FldX $FldY]

   unset dat1
   unset dat2

   #----- Compare the values

   foreach s $Svars sr $res_r sv $res_v {
      set sr [format "%.8e" $sr]
      set sv [format "%.8e" $sv]

      #puts "$s : Comparing ($sv) with ($sr)"
      if { $sr == $sv } {
         Log::Print INFO "==> TEST PASSED : $s"
      } else {
         Log::Print ERROR "==> TEST FAILED : $s (vexpr=$sv != $sr=R)"
      }
   }
}

#set Log::Param(Level) DEBUG
Log::Start [info script] 0.1

set fn  DataIn/2005102612_012
set fldX 131073 ;# TT at 1.000 sg (12000) for T+18
set fldY 130049 ;# TT at 0.9930 sg (11930) for T+18

fstdfile open FLE read $fn
fstdfield read X FLE $fldX
fstdfield read Y FLE $fldY

#----- Run the test with normal fields

RunTest $VARS_COMBO X Y
RunTest $VARS_ALONE X ""

#----- Add some NoData values

foreach i {X Y} {
   vexpr MSK$i ifelse($i>20.0, 0, 1)
   vexpr $i ifelse(MSK$i,$i,NAN)
}

#----- Rerun the test

RunTest $VARS_COMBO X Y
RunTest $VARS_ALONE X ""

#----- Free resources

foreach i {X Y} {
   fstdfield free $i
   fstdfield free MSK$i
}
fstdfile close FLE

Log::End -1

#puts [GetStatsR {nb medx medy avgx avgy} {1.1 1.5 1.75 4.00 6.00 6.98 9.00} {1.4 1.64 1.71 3.8 6.32 8.7 9.2}]
