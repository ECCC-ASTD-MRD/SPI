#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : SPI (Tools)
# Fichier  : FVCN.tcl
# Creation : Septembre 2001
#
# Description:
#    Redaction de message FVCN
#
#===============================================================================

namespace eval Writer::FVCN {
   global GDefs
   variable Msg
   variable Data

   #----- Definitions des messages

   set Msg(NoHeader)   { "Aucune Entete FVCN n'est disponible !" "No FVCN header available" }

   #----- Chaines standard

   set Data(Info1)  "AVO, "
   set Data(Info2)  "CVO, "
   set Data(Info3)  "GOES, "
   set Data(Info4)  "POES, "
   set Data(Info5)  "AIREP, "
   set Data(Info6)  "PIREP, "
   set Data(Info7)  "MET ANAL, "
   set Data(Info8)  "MET PROG, "
   set Data(Info9)  "SOUNDING, "
   set Data(Info10) "MWO REYKJAVIK, "
   set Data(Info11) "VA TEST, "
   set Data(Info12) "VA EXERCISE, "

   set Data(Prev2)  "THIS STATEMENT UPDATES MESSAGE"

   set Data(Details1)  "UNKNOWN"
   set Data(Details2)  "VA TEST"
   set Data(Details3)  "VA EXERCISE"

   set Data(Next1)  "WILL BE ISSUED BY"
   set Data(Next2)  "NO LATHER THAN"
   set Data(Next3)  "NO FURTHER ADVISORIES"

   set Data(NoVA00)  "VA NOT IDENTIFIABLE FROM SATELLITE DATA"
   set Data(NoVA06)  "NO VA EXPECTED"
   set Data(NoVA12)  "NO VA EXPECTED"
   set Data(NoVA18)  "NO VA EXPECTED"

   set Data(OBS)    "OBS VA CLD:"
   set Data(EST)    "EST VA CLD:"
   set Data(FCST06) "FCST VA CLD  +6 HR:"
   set Data(FCST12) "FCST VA CLD +12 HR:"
   set Data(FCST18) "FCST VA CLD +18 HR:"
   set Data(EST1)   "FL___/___ (COORDONNEES LAT/LONG)"

   set Data(Rem1)  "RESPONSIBILITY FOR THIS EVENT HAS BEEN TRANSFERRED BY THE"
   set Data(Rem2)  "RESPONSIBILITY FOR THIS EVENT IS BEING TRANSFERRED TO THE"
   set Data(Rem3)  "WE ARE INVESTIGATING THIS REPORT. AN UPDATED ADVISORY WILL BE ISSUED AS SOON AS POSSIBLE"
   set Data(Rem4)  "VA TEST VA TEST VA TEST VA TEST VA TEST"
   set Data(Rem5)  "VA EXERCISE VA EXERCISE VA EXERCISE"
   set Data(Rem12) "THIS STATEMENT UPDATES MESSAGE"
   set Data(Rem22) "THE NEXT STATEMENT WILL BE ISSUED BY THE"
   set Data(Rem23) "BY __/__ UTC UNDER THE HEADER"

   set Data(Ids)    "PANC EGRR KNES"
   set Data(IdPANC) "ANCHORAGE VAAC"
   set Data(IdEGRR) "LONDON VAAC"
   set Data(IdKNES) "WASHINGTON VAAC"
   set Data(NoPANC) "FVAK"
   set Data(NoEGRR) "FVXX"
   set Data(NoKNES) "FVXX"
   set Data(NoNONE) ""
   set Data(VAAC)   "MONTREAL"
   set Data(NA)     "NOT AVAILABLE"

   set Data(WWW)    "VA GRAPHICAL PRODUCT AVAILABLE AT\nHTTP://METEO.EC.GC.CA/EER (ALL LOWER CASE)"
   set Data(Ret)    "PLEASE SEE FV____ DDHHMM ISSUED BY ______ VAAC WHICH DESCRIBES CONDITIONS OVER OR NEAR THE MONTREAL VAAC AREA OF RESPONSIBILITY"

   set Data(Colors)   { blue green red black }
   set Data(Stipples) "@$GDefs(Dir)/Resources/Bitmap/raydiagleft04.xbm @$GDefs(Dir)/Resources/Bitmap/raydiagright04.xbm @$GDefs(Dir)/Resources/Bitmap/rayver04.xbm @$GDefs(Dir)/Resources/Bitmap/rayhor04.xbm"
   set Data(Delay)    60000
   set Data(Seconds)  [clock seconds]
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::New>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Vider toute l'information de l'interface
#
# Parametres :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::New { Pad Mode } {
   variable Data

   set Data(Mode$Pad) $Mode

   Writer::FVCN::Init $Pad

   switch $Mode {
      "RET" {
         $Pad.remarks configure -height 3
         $Pad.remarks insert 0.0 $Data(Ret)
         set Data(HRemarks$Pad) 3
         pack forget $Pad.head.test
         Writer::FVCN::UpdateTime $Pad $Data(Delay)
      }
      "EXP" {
         set Data(Area$Pad)  ""
         set Data(InfoSel5$Pad) 1
         set Data(Obs$Pad) $Data(EST)
         Writer::FVCN::UpdateTime $Pad $Data(Delay) 1
         Writer::FVCN::SetRem  $Pad $Pad.remarks 3 NONE
         Writer::FVCN::SetNext $Pad $Pad.next 1 FVCN
         Writer::FVCN::SetInfo $Pad $Pad.info
         pack forget $Pad.head.test
     }
      default {
         $Pad.remarks insert 0.0 NIL
         $Pad.details insert 0.0 UNKNOWN
         Writer::FVCN::UpdateTime $Pad $Data(Delay)
         Writer::FVCN::GraphInit $Pad
      }
   }

   Writer::FVCN::AshUpdate $Pad 00
   Writer::FVCN::AshUpdate $Pad 06
   Writer::FVCN::AshUpdate $Pad 12
   Writer::FVCN::AshUpdate $Pad 18

   Writer::FVCN::Site "" UNKNOWN "" "" "" ""

   Writer::FVCN::PageInit $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::Init>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser une nouvelle structure
#
# Parametres :
#    <Pad>   : Identificateur du pad
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::Init { Pad } {
   variable Data

   #----- Definitions des variables internes

   set Data(Test$Pad)     False
   set Data(Sent$Pad)     0
   set Data(No$Pad)       FVCN00
   set Data(Id$Pad)       CWAO
   set Data(Date$Pad)     DDMMHH
   set Data(Cor$Pad)      ""
   set Data(Issued$Pad)   ""
   set Data(Site$Pad)     UNKNOWN
   set Data(Location$Pad) N0000E00000
   set Data(Area$Pad)     $Data(NA)
   set Data(Elev$Pad)     0M
   set Data(Advisory$Pad) ""
   set Data(Info$Pad)     ""
   set Data(Code$Pad)     UNKNOWN
   set Data(Next$Pad)     ""
   set Data(Year$Pad)    0
   set Data(Obs$Pad)     $Data(OBS)
   set Data(Lat$Pad)     0.0
   set Data(Lon$Pad)     0.0
   set Data(Page$Pad)    ""

   #----- Regions de cendres par niveaux et heures

   set Data(Level$Pad)   L0
   set Data(Hour$Pad)    00

   foreach h { 00 06 12 18 } {
      set Data(FCST$h$Pad)  ""
      set Data(L0$h$Pad)    ""
      set Data(L1$h$Pad)    ""
      set Data(L2$h$Pad)    ""
      set Data(LVL$h)       {}
      set Data(HAsh$h$Pad)  1
   }

   set Data(HVAAC$Pad)    1
   set Data(HInfo$Pad)    1
   set Data(HDetails$Pad) 1
   set Data(HNext$Pad)    1
   set Data(HRemarks$Pad) 1

   set Data(InfoSel1$Pad)  0
   set Data(InfoSel2$Pad)  0
   set Data(InfoSel3$Pad)  0
   set Data(InfoSel4$Pad)  0
   set Data(InfoSel5$Pad)  0
   set Data(InfoSel6$Pad)  0
   set Data(InfoSel7$Pad)  0
   set Data(InfoSel8$Pad)  0
   set Data(InfoSel9$Pad)  0
   set Data(InfoSel10$Pad) 0

   set Data(Handle$Pad) "YES"
   set Data(File$Pad)   "FVCN"

   Writer::FVCN::SetDate $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::SetDate>
# Creation : Juillet 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser les date d'observation et de previsions
#
# Parametres :
#    <Pad>   : Identificateur du pad
#    <Secs>  : Secondes
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::SetDate { Pad { Secs 0 } } {
   variable Data

   if { !$Secs } {
      set Secs [clock seconds]
   }
   set Data(Date00$Pad)  [clock format $Secs -format "%d/%H%MZ" -gmt True]
   set Data(Date06$Pad)  [clock format [expr $Secs+3600*6] -format "%d/%H%MZ" -gmt True]
   set Data(Date12$Pad)  [clock format [expr $Secs+3600*12] -format "%d/%H%MZ" -gmt True]
   set Data(Date18$Pad)  [clock format [expr $Secs+3600*18] -format "%d/%H%MZ" -gmt True]

   return 1
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::Layout>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser le layout du produit graphique FVCN#
#
# Parametres :
#    <Frame> : Page ou creer le layout
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::Layout { Frame } {
   variable Data

   Page::Size $Frame 1015 675

   set Viewport::Resources(FillCoast) #DCDCDC
   set Viewport::Resources(FillLake)  ""
   set Viewport::Resources(Coast)     #000000      ;#Cotes
   set Viewport::Resources(Lake)      #0000ff      ;#Lacs
   set Viewport::Resources(Polit)     #8C8C8C      ;#Bordures politiques
   set Viewport::Resources(Coord)     #000000      ;#Latlon

   set Viewport::Map(Coast)       1           ;#Cotes
   set Viewport::Map(Lake)        1           ;#Lacs
   set Viewport::Map(River)       0           ;#Rivieres
   set Viewport::Map(Polit)       1           ;#Bordures politiques
   set Viewport::Map(Admin)       1           ;#Bordures politiques internes
   set Viewport::Map(City)        0           ;#Villes
   set Viewport::Map(Road)        0           ;#Routes
   set Viewport::Map(Rail)        0           ;#Chemin de fer
   set Viewport::Map(Util)        0           ;#Utilitaires
   set Viewport::Map(Canal)       0           ;#Canal/Aqueduc
   set Viewport::Map(Topo)        0           ;#Topographie
   set Viewport::Map(Bath)        0           ;#Bathymetrie
   set Viewport::Map(Text)        0           ;#Texture
   set Viewport::Map(Coord)       1           ;#Positionnement des latlon (<0=Ocean,>0=Partout)
   set Viewport::Map(CoordDef)    10.0        ;#Intervale entre les latlon en degres
   set Viewport::Map(CoordNum)    2           ;#Numerotation des latlon

   set Data(VP00$Frame) [Viewport::Create $Frame 5     5 500 270 False False]
   set Data(VP06$Frame) [Viewport::Create $Frame 510   5 500 270 False False]
   set Data(VP12$Frame) [Viewport::Create $Frame 5   280 500 270 False False]
   set Data(VP18$Frame) [Viewport::Create $Frame 510 280 500 270 False False]

   $Frame.page.canvas create text    255 140 -text $Data(NoVA00) -tag NOVA00  -font XFont20
   $Frame.page.canvas create text    760 140 -text $Data(NoVA06) -tag NOVA06  -font XFont20
   $Frame.page.canvas create text    230 415 -text $Data(NoVA12) -tag NOVA12 -font XFont20
   $Frame.page.canvas create text    760 415 -text $Data(NoVA18) -tag NOVA18 -font XFont20

   $Frame.page.canvas create rectangle 5     5 210  21 -fill white -outline black -width 0.5
   $Frame.page.canvas create rectangle 510   5 715  21 -fill white -outline black -width 0.5
   $Frame.page.canvas create rectangle 5   280 210 296 -fill white -outline black -width 0.5
   $Frame.page.canvas create rectangle 510 280 715 296 -fill white -outline black -width 0.5

   $Frame.page.canvas create text    9     7 -text "Date 0" -anchor nw -tag DATE00 -font XFont12
   $Frame.page.canvas create text    514   7 -text "Date 1" -anchor nw -tag DATE06 -font XFont12
   $Frame.page.canvas create text    9   282 -text "Date 2" -anchor nw -tag DATE12 -font XFont12
   $Frame.page.canvas create text    514 282 -text "Date 3" -anchor nw -tag DATE18 -font XFont12

   $Frame.page.canvas create rectangle 5 555 1010 670 -fill white -outline black -width 0.5
   $Frame.page.canvas create text   10 560 -text "Col 1" -anchor nw -tag COL0 -font XFont12
   $Frame.page.canvas create text  455 560 -text "Col 2" -anchor n  -tag COL1 -font XFont12
   $Frame.page.canvas create text 1005 560 -text "Col 3" -anchor ne -tag COL2 -font XFont12

   set dx 320
   set dy 665
   foreach no { 0 1 2 }  {
      $Frame.page.canvas create text      $dx $dy -text "" -anchor se -tag LVL$no -font XFont12
      $Frame.page.canvas create rectangle [expr $dx+5] [expr $dy-20] [expr $dx+45] $dy -fill "" -outline "" -stipple "" -width 2 -tag CVL$no
      incr dx 140
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::GraphInit>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialiser le produit graphique
#
# Parametres :
#    <Pad>   : Identificateur du pad
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::GraphInit { Pad } {
   variable Data

   if { [winfo exist $Data(Page$Pad)] } {
      raise $Data(Page$Pad)
   } else {
      wm geom .writer 1522x757
      set Data(Page$Pad) $Pad.pg
      Page::Create $Pad.pg 1020 715 False
      pack $Data(Page$Pad) -side right -anchor nw

      SPI::LayoutLoad $Data(Page$Pad) Writer::FVCN
      SPI::LayoutLock $Data(Page$Pad)
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::GraphUpdate>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour les items du produit graphique
#
# Parametres :
#    <Pad>   : Identificateur du pad
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::GraphUpdate { Pad { Location False } } {
   variable Data

   if { [winfo exist $Data(Page$Pad)] } {

      if { $Location } {
         Viewport::Rotate $Data(Page$Pad) $Data(Lat$Pad) $Data(Lon$Pad)
      }

      #----- Update text

      set text  "VA ADVISORY\n[format "%-12s" DTG:] $Data(Issued$Pad)\n[format "%-12s" VAAC:] $Data(VAAC)\n[format "%-12s" "VOLCANO:"] $Data(Site$Pad)\n[format "%-12s" "PSN:"] $Data(Location$Pad)\n[format "%-12s" "AREA:"] $Data(Area$Pad)\n[format "%-12s" "SUMMIT ELEV:"] $Data(Elev$Pad)\n[format "%-12s" "ADVISORY NR:"] $Data(Advisory$Pad)"
      $Data(Page$Pad).page.canvas itemconfigure COL0 -text $text

      set text1 [Writer::BlocFormat "INFO SOURCE:" [Writer::TextExtract word 30 "" $Pad.info] 21]
      set text2 "[format "%-21s" "AVIATION COLOUR CODE:"] $Data(Code$Pad)"
      set text3 [Writer::BlocFormat "ERUPTION DETAILS:" [Writer::TextExtract word 30 "" $Pad.details] 21]

      $Data(Page$Pad).page.canvas itemconfigure COL1 -text $text1\n$text2\n$text3

      set text1 [Writer::BlocFormat "RMK:" [Writer::TextExtract char 35 "" $Pad.remarks] 13]
      set text2 [Writer::BlocFormat "NXT ADVISORY:" [Writer::TextExtract char 35 "" $Pad.next] 13]

      $Data(Page$Pad).page.canvas itemconfigure COL2 -text  $text1\n$text2
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::GraphAreaColor>
# Creation : Mai 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour les couleurs pour chaque niveau
#
# Parametres :
#    <Pad>   : Identificateur du pad
#
# Remarques :
#           : Pas de niveaux= pas de couleur donc pas de region affichee
#----------------------------------------------------------------------------

proc Writer::FVCN::GraphAreaColor { Pad } {
   variable Data

   if { [winfo exist $Data(Page$Pad)] } {
      #----- Update Legend, We have to extract the level heights from the main FVCN
      #----- to dispacth the colors ans stipplings
      set lvls {}

      foreach h { 00 06 12 18 } {
         set text [$Pad.ash$h get 0.0 end]
         set Data(LVL$h) {}
         set l 0

         #----- find all level string occurence (FLnnn...)
         foreach { lvl i } [regexp -all -nocase -inline {([A-Z]|[0-9]){3,5}/FL+[0-9]{2,3} [NS][0-9]{4}} $text] {
            if { [lsearch -exact $lvls [set lvl [lindex $lvl 0]]]==-1 } {
               lappend lvls $lvl
            }
            lappend Data(LVL$h) $lvl
            incr l
         }
      }

      #----- Assign color and filling to each level
      set i 0
      foreach lvl $lvls {
         set Data(Color$lvl) [lindex $Data(Colors) $i]
         set Data(Stipple$lvl) [lindex $Data(Stipples) $i]
         $Data(Page$Pad).page.canvas itemconfigure LVL$i -text $lvl
         $Data(Page$Pad).page.canvas itemconfigure CVL$i -fill $Data(Color$lvl) -outline $Data(Color$lvl) -stipple $Data(Stipple$lvl)
         incr i
         puts stderr "$i $lvl $Data(Color$lvl)"
      }
      for { } { $i<5 } { incr i } {
         $Data(Page$Pad).page.canvas itemconfigure LVL$i -text ""
         $Data(Page$Pad).page.canvas itemconfigure CVL$i -fill "" -outline ""
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::Open>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Consulter un message
#
# Parametres :
#  <Pad>     : Identificateur du Pad
#  <File>    : Path complet du fichier
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Writer::FVCN::Open { Pad File } {
   global   GDefs
   variable Data

   if { [string length $File] == 0 } {
      return 0
   }

   Writer::FVCN::Init $Pad

   if { [string first ".ret" $File] != -1 } {
      set Data(Mode$Pad) RET
   } else {
      set Data(Mode$Pad) NEW
      Writer::FVCN::GraphInit $Pad
   }

   if { [string first ".sent" $File] != -1 } {
      set Data(Sent$Pad) 1
   } else {
      set Data(Sent$Pad) 0
   }

   if { $Data(Mode$Pad)=="NEW" && !$Data(Sent$Pad) } {
      set Data(Handle$Pad) "YES"
   } else {
      set Data(Handle$Pad) ""
   }
   set Data(File$Pad) [file tail $File]

   Writer::PadName $Pad "REA [string range $Data(File$Pad) 0 11]..."
   Writer::FVCN::Read $Pad $File $Data(Mode$Pad)
   Writer::FVCN::PageInit $Pad
   Writer::FVCN::UpdateTime $Pad $Data(Delay)

   set coo [Writer::FVCN::UnFormatCoord $Data(Location$Pad)]
   set Data(Lat$Pad) [lindex $coo 0]
   set Data(Lon$Pad) [lindex $coo 1]

   Writer::FVCN::GraphUpdate $Pad False
   Writer::FVCN::UpdateItems $Writer::Data(Frame)
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::Correct>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Correction d'un message.
#
# Parametres :
#  <Pad>     : Identificateur du Pad
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Writer::FVCN::Correct { Pad } {
   global   GDefs
   variable Data
   variable Msg
   variable Lbl

   if { $Pad=="" } {
      return
   }

   #----- On ne permet pas la correction d'un nouveau message

   if { $Data(File$Pad)=="FVCN" } {
      Dialog::CreateError . [lindex $Writer::Msg(NEW) $GDefs(Lang)] $GDefs(Lang)
      return
   }

   set no [string index $Data(Cor$Pad) 2]

   if { $no!="" } {
      scan $no "%c" no
      set no [format "%c" [incr no]]
      set Data(Cor$Pad) "CC$no"
   } else {
      set Data(Cor$Pad) "CCA"
   }

   set Data(Handle$Pad) "YES"
   Writer::FVCN::UpdateTime $Pad $Data(Delay)

   Writer::PadName $Pad "COR [string range $Data(File$Pad) 0 11]..."
   Writer::FVCN::PageInit $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::Update>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Mise-a-jour d'un message
#
# Parametres :
#  <Pad>     : Identificateur du Pad
#
# Remarques :
#    Aucune.
#
#----------------------------------------------------------------------------

proc Writer::FVCN::Update { Pad } {
   global   GDefs
   variable Data
   variable Msg

   if { $Pad=="" } {
      return
   }

   #----- On ne permet pas la mise-a-jour d'un nouveau message

   if { $Data(File$Pad)=="FVCN" } {
      Dialog::CreateError . [lindex $Writer::Msg(NEW) $GDefs(Lang)] $GDefs(Lang)
      return
   }

   #----- On ne permet pas la mise-a-jour d'une retransmission 'RET'

   if { $Data(Mode$Pad)=="RET" } {
      Dialog::CreateError . [lindex $Writer::Msg(RET) $GDefs(Lang)] $GDefs(Lang)
      return
   }

   Debug::TraceProc "Writer::FVCN::Update : Checking for Site $Data(Site$Pad)"

   #----- Ajustement du numero du bulletin
   #         - incremente si il y a eu transmission.
   #         - non incremente si il y a eu sauvegarde.

   if { $Data(Sent$Pad) } {
      set lst [split $Data(Advisory$Pad) /]
      set no [string trimleft [lindex $lst 1] 0]
      set Data(Advisory$Pad) "[lindex $lst 0]/[format "%03d" [incr no 1]]"
   }

   set Data(Handle$Pad) "YES"
   Writer::FVCN::UpdateTime $Pad $Data(Delay)

   Writer::PadName $Pad "UPD [string range $Data(File$Pad) 0 11]..."
   Writer::FVCN::PageInit $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::Format>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Formatte le message dans un fichier texte
#
# Parametres :
#   <Pad>    : Identificateur du Pad
#   <Mode>   : Mode du message (NEW ou RETRANSMIT).
#
# Retour     :
#   <File>   : Fichier temporaire
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::Format { Pad Mode } {
   global   GDefs
   variable Data

   set file $GDefs(DirEER)/eer_Tmp/FVCN[pid]_[clock seconds].txt

   set f [open $file w]

   #----- Header

   puts $f "$Data(No$Pad) $Data(Id$Pad) $Data(Date$Pad) $Data(Cor$Pad)"
   puts $f "VA ADVISORY"
   puts $f "[format "%-21s" DTG:] $Data(Issued$Pad)"
   puts $f "[format "%-21s" VAAC:] $Data(VAAC)"

   #----- Info

   puts $f "[format "%-21s" "VOLCANO:"] $Data(Site$Pad)"
   puts $f "[format "%-21s" "PSN:"] $Data(Location$Pad)"
   puts $f "[format "%-21s" "AREA:"] $Data(Area$Pad)"
   puts $f "[format "%-21s" "SUMMIT ELEV:"] $Data(Elev$Pad)"
   puts $f "[format "%-21s" "ADVISORY NR:"] $Data(Advisory$Pad)"

   puts $f [Writer::BlocFormat "INFO SOURCE:" [Writer::TextExtract word 47 "" $Pad.info]]

   if { $Mode=="RET" } {
      puts $f [Writer::BlocFormat "RMK:" [Writer::TextExtract word 47 "" $Pad.remarks]]
   } else {

      puts $f "[format "%-21s" "AVIATION COLOUR CODE:"] $Data(Code$Pad)"

      puts $f [Writer::BlocFormat "ERUPTION DETAILS:" [Writer::TextExtract word 47 "" $Pad.details]]

      #----- Ash cloud

      puts $f "[format "%-21s" "OBS VA DTG:"] $Data(Date00$Pad)"
      puts $f [Writer::BlocFormat $Data(Obs$Pad) [Writer::TextExtract char 47 "" $Pad.ash00]]
      puts $f [Writer::BlocFormat $Data(FCST06)  [Writer::TextExtract char 47 "" $Pad.ash06]]
      puts $f [Writer::BlocFormat $Data(FCST12)  [Writer::TextExtract char 47 ""  $Pad.ash12]]
      puts $f [Writer::BlocFormat $Data(FCST18)  [Writer::TextExtract char 47 ""  $Pad.ash18]]

      #----- Next

      puts $f [Writer::BlocFormat "RMK:" [Writer::TextExtract word 47 "" $Pad.remarks]]
      if { $Mode!="EXP" } {
         puts $f [Writer::BlocFormat "" [split $Data(WWW) \n]]
      }
      puts $f [Writer::BlocFormat "NXT ADVISORY:" [Writer::TextExtract word 47 ""  $Pad.next]]
   }

   close $f

   return $file
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::FormatCoord>
# Creation : Sepetmbre 2001 - J.P.Gauthier - CMC/CMOE
#
# But      : Formater les coordonnes
#
# Parametres :
#    <Lat>   : Latitude
#    <Lon>   : Longitude
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::FormatCoord { Lat Lon } {

   if { $Lat < 0 } {
      set Lat [expr -$Lat]
      set dir "S"
   } else {
      set dir "N"
   }
   set Lat "[Convert::Decimal2Minute $Lat 0 True] $dir"
   set Lat "$dir[format "%02i" [lindex $Lat 0]][lindex $Lat 1]"

   if { $Lon < 0 } {
      set Lon [expr -$Lon]
      set dir "W"
   } else {
      set dir "E"
   }
   set Lon "[Convert::Decimal2Minute $Lon 0 True] $dir"
   set Lon "$dir[format "%03i" [lindex $Lon 0]][lindex $Lon 1]"

   return "$Lat $Lon"
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::UnFormatCoord>
# Creation : Sepetmbre 2001 - J.P.Gauthier - CMC/CMOE
#
# But      : Formater les coordonnes
#
# Parametres :
#    <Lat>   : Latitude
#    <Lon>   : Longitude
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::UnFormatCoord { Coord } {

   if { [llength $Coord]!=2 } {
      return "0.0 0.0"
   } else {
      set la [lindex $Coord 0]
      set lo [lindex $Coord 1]

      set lat [Convert::Minute2Decimal "[string range $la 1 2] [string range $la 3 4]"]
      set lon [Convert::Minute2Decimal "[string range $lo 1 3] [string range $lo 4 5]"]

      if { [string index $la 0]=="S" } {
         set lat [expr -$lat]
      }

      if { [string index $lo 0]=="W" } {
         set lon [expr -$lon]
      }

      return "$lat $lon"
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::GetAdvisory>
# Creation : Sepetmbre 2001 - S. Trudel - CMC/CMOE
#
# But      : Ajuster le numero du message.
#
# Parametres :
#    <Pad>   : Identificateur du Pad
#    <Name>  : Nom de la source.
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::GetAdvisory { Pad Name } {
   global   GDefs
   variable Data

   regsub -all "\[^a-zA-Z0-9-\]" $Name - name

   #----- Recupere tout les fichiers transmis.

   set filelist [glob -nocomplain $GDefs(DirMsg)/FVCN/*.sent*]
   set no 1

   #----- On increment si toute les conditions suivantes sont respectees :
   #         - il existe deja un message emis pour la meme annee.
   #         - le message n'est pas une correction.

   foreach file $filelist {

      set file [file tail $file]

      if { [string first $name $file]!=-1 && [string first $Data(Year$Pad) $file]!=-1 } {

         #----- si le fichier est une correction, on ne le compter pas.

         if { [string last .cor.sent ${file}]==-1 } {
            incr no 1
         }
      }
   }

   return "$Data(Year$Pad)/[format "%03d" $no]"
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::GetNo>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer un numero valide d'entete FVCN pour une source specifie.
#
# Parametres :
#    <Name>  : Nom de la source.
#
# Retour  :
#    <no> : Si no != "" il y a au moins une entete FVCN.. de disponible.
#
# Remarques :
#    - On conserve la meme entete FVCNxx si :
#         - il existe un message deja emis a l'interieure de 48 hres.
#      sinon
#         - on en trouve une qui est disponible.
#         sinon
#            - on ne peut traiter cette nouvelle source.
#
#----------------------------------------------------------------------------

proc Writer::FVCN::GetNo { Name } {
   global   GDefs
   variable Data
   variable Msg

   regsub -all "\[^a-zA-Z0-9-\]" $Name - name

   #----- Recupere tout les fichiers transmis.

   set filelist [glob -nocomplain $GDefs(DirMsg)/FVCN/*.sent]

   Debug::TraceProc "FVCN::GetNo: Msg list: $filelist"

   set entetes_exclues {}

   foreach file $filelist {

      set file [file tail $file]

      Debug::TraceProc "FVCN::GetNo: Processing... $file"

      #----- on determine le nombre d'heures qui s'est ecoule
      #      depuis la transmission.

      set sec [clock scan "[string range $file 4 5]/[string range $file 6 7]/[string range $file 0 3]\
                  [string range $file 8 9]:[string range $file 10 11]:00" -gmt True]
      set delta [expr ($Data(Seconds) - $sec)/3600]

      #----- On conserve la meme entete FVCNxx si :
      #         - il existe un message deja emis en 48 hres.

      if { $delta<48.0 && [string first $name $file]!=-1 } {

         Debug::TraceProc "FVCN::GetNo: There is already a message emitted within 48 hours ($name)"

         #----- on conserve la meme entete FVCNxx.

         set no [string range $file 14 19]
         Debug::TraceProc "FVCN::GetNo: Selected $no"
         return $no

      } else {

         if { $delta<48.0 } {

            #----- on doit exclure cette entete FVCNxx.

            lappend entetes_exclues [string range ${file} 14 19]
            Debug::TraceProc "FVCN::GetNo: There is another message emitted within 48 hours ([string range ${file} 13 18])"

         } else {

            #----- on doit choisir une entete encore disponible.

            foreach entete { FVCN01 FVCN02 FVCN03 FVCN04 } {
               if { [lsearch -exact ${entetes_exclues} ${entete}]==-1 } {
                  set no [string trim ${entete}]
                  Debug::TraceProc "FVCN::GetNo: Found header $no"
                  return $no
               }
            }

            Debug::TraceProc "FVCN::GetNo: No header available !!!"
            Dialog::CreateError . [lindex $Msg(NoHeader) $GDefs(Lang)] $GDefs(Lang)
            return ""
         }
      }
   }

   #----- on doit choisir une entete encore disponible.

   foreach entete { FVCN01 FVCN02 FVCN03 FVCN04 } {
      if { [lsearch -exact ${entetes_exclues} ${entete}]==-1 } {
         set no [string trim ${entete}]
         return $no
      }
   }

   Debug::TraceProc "FVCN::GetNo: No header available !!!"
   Dialog::CreateError . [lindex $Msg(NoHeader) $GDefs(Lang)] $GDefs(Lang)
   return ""
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::SetInfo>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer les sources d'informations.
#
# Parametres :
#    <Pad>   : Identiicateur du Pad
#    <Text>  : Identificateur du widget.
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::SetInfo { Pad Text } {
   variable Data

   #----- Creer la ligne de texte avec les options selectionee

   foreach src { 1 2 3 4 5 6 7 8 9 10 11 12 } {

      if { $Data(InfoSel${src}$Pad) } {

         if { [$Text search -exact $Data(Info${src}) 0.0] == "" } {
            $Text insert insert $Data(Info${src}) SOURCES${src}
         }
      } else {
          set idx [$Text tag ranges SOURCES${src}]
          if { $idx != "" } {
             $Text delete [lindex $idx 0] [lindex $idx 1]
          }
      }
   }
   set Data(HInfo$Pad) [Writer::TextExpand $Text 47 32]
   Writer::FVCN::PageInit $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::Clear>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer tout ce qui a trait au "layout" precedent
#
# Parametres :
#  <Pad>    : Identificateur du Pad
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::Clear { Pad } {
   variable Data

   after cancel $Data(Handle$Pad)
   trace vdelete SPI::Src(Info) w { Writer::FVCN::Source }

   destroy $Pad.fvcn $Pad.code $Pad.volcano $Pad.advisory $Pad.issued $Pad.no $Pad.date $Pad.corid
   destroy $Pad.location $Pad.area $Pad.elev $Pad.ash $Pad.vaac $Pad.details
   destroy $Pad.info $Pad.ash00 $Pad.ash06 $Pad.ash12 $Pad.ash18 $Pad.remarks $Pad.next
   destroy $Pad.optvolcano $Pad.optcode $Pad.optdetails $Pad.optinfo $Pad.optvaac $Pad.optnext $Pad.optrem $Pad.optobs $Pad.optash00 $Pad.optash06 $Pad.optash12 $Pad.optash18

   if { $Writer::Data(Canvas)!="" } {
      $Writer::Data(Canvas) delete FVCN
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::LayoutInit>
# Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue l'affichage des parties fixe de la carte.
#
# Parametres :
#  <Pad>     : Identificateur du pad
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::LayoutInit { Pad } {
   global   GDefs
   variable Data
   variable Bubble

   label $Pad.fvcn

   label $Pad.code     -bg gray75 -font XFont12 -bd 0 -anchor w -textvariable Writer::FVCN::Data(Code$Pad)
   label $Pad.volcano  -bg gray75 -font XFont12 -bd 0 -anchor w -textvariable Writer::FVCN::Data(Site$Pad)
   label $Pad.advisory -bg white  -font XFont12 -bd 0 -anchor w -textvariable Writer::FVCN::Data(Advisory$Pad)
   label $Pad.issued   -bg white  -font XFont12 -bd 0 -anchor w -textvariable Writer::FVCN::Data(Issued$Pad)
   label $Pad.no       -bg white  -font XFont12 -bd 0 -anchor w -textvariable Writer::FVCN::Data(No$Pad)
   label $Pad.date     -bg white  -font XFont12 -bd 0 -anchor w -textvariable Writer::FVCN::Data(Date$Pad)
   label $Pad.corid    -bg white  -font XFont12 -bd 0 -anchor w -textvariable Writer::FVCN::Data(Cor$Pad)
   label $Pad.vaac     -bg white  -font XFont12 -bd 0 -anchor w -textvariable Writer::FVCN::Data(VAAC)
   label $Pad.obs      -bg white  -font XFont12 -bd 0 -anchor w -textvariable Writer::FVCN::Data(Obs$Pad)

   entry $Pad.ash      -bg gray75 -width 12 -font XFont12 -bd 0 -textvariable Writer::FVCN::Data(Date00$Pad)

   entry $Pad.location -bg gray75 -width 12 -font XFont12 -bd 0 -textvariable Writer::FVCN::Data(Location$Pad)
   entry $Pad.area     -bg gray75 -width 47 -font XFont12 -bd 0 -textvariable Writer::FVCN::Data(Area$Pad)
   entry $Pad.elev     -bg gray75 -width 8  -font XFont12 -bd 0 -textvariable Writer::FVCN::Data(Elev$Pad)

   text  $Pad.details   -bg gray75 -height 1 -width 47 -font XFont12 -bd 0 -wrap word
   text  $Pad.info      -bg gray75 -height 1 -width 47 -font XFont12 -bd 0 -wrap word
   text  $Pad.ash00     -bg gray75 -height 1 -width 47 -font XFont12 -bd 0 -wrap char
   text  $Pad.ash06     -bg gray75 -height 1 -width 47 -font XFont12 -bd 0 -wrap char
   text  $Pad.ash12     -bg gray75 -height 1 -width 47 -font XFont12 -bd 0 -wrap char
   text  $Pad.ash18     -bg gray75 -height 1 -width 47 -font XFont12 -bd 0 -wrap char
   text  $Pad.remarks   -bg gray75 -height 1 -width 47 -font XFont12 -bd 0 -wrap word
   label $Pad.www       -bg white  -font XFont12 -bd 0 -anchor w -textvariable Writer::FVCN::Data(WWW) -justify left
   text  $Pad.next      -bg gray75 -height 1 -width 47 -font XFont12 -bd 0 -wrap word

   #----- Auto resizing des text widgets

   bind $Pad.details <Any-KeyRelease> "set Writer::FVCN::Data(HDetails$Pad) \[Writer::TextExpand %W 47 64\] ; Writer::FVCN::PageInit $Pad"
   bind $Pad.info    <Any-KeyRelease> "set Writer::FVCN::Data(HInfo$Pad)    \[Writer::TextExpand %W 47 32\] ; Writer::FVCN::PageInit $Pad"
   bind $Pad.ash00   <Any-KeyRelease> "set Writer::FVCN::Data(HAsh00$Pad)   \[Writer::TextExpand %W 47\] ; Writer::FVCN::PageInit $Pad; set Writer::FVCN::Data(Date00$Pad) \[$Pad.ash00  get 1.0 1.8\]; Writer::FVCN::UpdateGraphItems $Pad"
   bind $Pad.ash06   <Any-KeyRelease> "set Writer::FVCN::Data(HAsh06$Pad)   \[Writer::TextExpand %W 47\] ; Writer::FVCN::PageInit $Pad; set Writer::FVCN::Data(Date06$Pad) \[$Pad.ash06  get 1.0 1.8\]; Writer::FVCN::UpdateGraphItems $Pad"
   bind $Pad.ash12   <Any-KeyRelease> "set Writer::FVCN::Data(HAsh12$Pad)   \[Writer::TextExpand %W 47\] ; Writer::FVCN::PageInit $Pad; set Writer::FVCN::Data(Date12$Pad) \[$Pad.ash12  get 1.0 1.8\]; Writer::FVCN::UpdateGraphItems $Pad"
   bind $Pad.ash18   <Any-KeyRelease> "set Writer::FVCN::Data(HAsh18$Pad)   \[Writer::TextExpand %W 47\] ; Writer::FVCN::PageInit $Pad; set Writer::FVCN::Data(Date18$Pad) \[$Pad.ash18  get 1.0 1.8\]; Writer::FVCN::UpdateGraphItems $Pad"
   bind $Pad.remarks <Any-KeyRelease> "set Writer::FVCN::Data(HRemarks$Pad) \[Writer::TextExpand %W 47 [expr 256-[string length $Data(WWW)]]\] ; Writer::FVCN::PageInit $Pad"
   bind $Pad.next    <Any-KeyRelease> "set Writer::FVCN::Data(HNext$Pad)    \[Writer::TextExpand %W 47\] ; Writer::FVCN::PageInit $Pad"

   #----- Menu d'options

   menubutton $Pad.optvolcano -bg white -bd 0 -menu $Pad.optvolcano.menu -image opt -width $Writer::Data(Height) -height $Writer::Data(Height)
   menu $Pad.optvolcano.menu -bd 1 -tearoff 0 -activeborderwidth 0 -bg white
      $Pad.optvolcano.menu add command -label Select -command "Locator::Window 0"
      $Pad.optvolcano.menu add separator
      $Pad.optvolcano.menu add command -label UNKNOWN -command { Writer::FVCN::Site "" UNKNOWN "" "" "" "" }
      $Pad.optvolcano.menu add command -label UNNAMED -command { Writer::FVCN::Site "" UNNAMED "" "" "" "" }

   menubutton $Pad.optcode -bg white -bd 0 -menu $Pad.optcode.menu -image opt -width $Writer::Data(Height) -height $Writer::Data(Height)
   menu $Pad.optcode.menu -bd 1 -tearoff 0 -activeborderwidth 0 -bg white
      foreach code { UNKNOWN GREEN YELLOW ORANGE RED "NOT GIVEN" NIL } {
         $Pad.optcode.menu add radiobutton -indicatoron false -label $code -variable Writer::FVCN::Data(Code$Pad) -value $code
      }

   menubutton $Pad.optinfo -bg white -bd 0 -menu $Pad.optinfo.menu -image opt -width $Writer::Data(Height) -height $Writer::Data(Height)
   menu $Pad.optinfo.menu -bd 1 -tearoff 0 -activeborderwidth 0 -bg white
      foreach idx { 1 2 3 4 5 6 7 8 9 10 11 12} {
         set Data(InfoSel$idx$Pad) 0
         if { $idx==11 } {
             $Pad.optinfo.menu add separator
         }
         $Pad.optinfo.menu add checkbutton -label $Data(Info$idx) -variable Writer::FVCN::Data(InfoSel$idx$Pad) \
            -command "Writer::FVCN::SetInfo $Pad $Pad.info"
      }

   menubutton $Pad.optdetails -bg white -bd 0 -menu $Pad.optdetails.menu -image opt -width $Writer::Data(Height) -height $Writer::Data(Height)
   menu $Pad.optdetails.menu -bd 1 -tearoff 0 -activeborderwidth 0 -bg white
      $Pad.optdetails.menu add command -label $Data(Details1) -command "Writer::FVCN::SetDetails $Pad $Pad.details 1 FVCN"
      $Pad.optdetails.menu add command -label $Data(Details2) -command "Writer::FVCN::SetDetails $Pad $Pad.details 2 FVCN"
      $Pad.optdetails.menu add command -label $Data(Details3) -command "Writer::FVCN::SetDetails $Pad $Pad.details 3 FVCN"

   menubutton $Pad.optnext -bg white -bd 0 -menu $Pad.optnext.menu -image opt -width $Writer::Data(Height) -height $Writer::Data(Height)
   menu $Pad.optnext.menu -bd 1 -tearoff 0 -activeborderwidth 0 -bg white
      $Pad.optnext.menu add command -label $Data(Next1) -command "Writer::FVCN::SetNext $Pad $Pad.next 1 FVCN"
      $Pad.optnext.menu add command -label $Data(Next2) -command "Writer::FVCN::SetNext $Pad $Pad.next 2 FVCN"
      $Pad.optnext.menu add command -label $Data(Next3) -command "Writer::FVCN::SetNext $Pad $Pad.next 3 FVCN"

   menubutton $Pad.optrem -bg white -bd 0 -menu $Pad.optrem.menu -image opt -width $Writer::Data(Height) -height $Writer::Data(Height)
   menu $Pad.optrem.menu -bd 1 -tearoff 0 -activeborderwidth 0 -bg white
      $Pad.optrem.menu add cascade -label $Data(Rem1) -menu $Pad.optrem.menu.from
      $Pad.optrem.menu add cascade -label $Data(Rem2) -menu $Pad.optrem.menu.to
      $Pad.optrem.menu add command -label $Data(Rem3) -command "Writer::FVCN::SetRem $Pad $Pad.remarks 3 NONE"
      $Pad.optrem.menu add separator
      $Pad.optrem.menu add command -label $Data(Rem4) -command "Writer::FVCN::SetRem $Pad $Pad.remarks 4 NONE"
      $Pad.optrem.menu add command -label $Data(Rem5) -command "Writer::FVCN::SetRem $Pad $Pad.remarks 5 NONE"
      $Pad.optrem.menu add separator
      $Pad.optrem.menu add command -label NIL -command "Writer::FVCN::SetRem $Pad $Pad.remarks 6 NONE"

   menu $Pad.optrem.menu.from -bd 1 -tearoff 0 -activeborderwidth 0 -bg white
   menu $Pad.optrem.menu.to -bd 1 -tearoff 0 -activeborderwidth 0 -bg white

   foreach id $Data(Ids) {
      $Pad.optrem.menu.from add command -label $Data(Id$id) -command "Writer::FVCN::SetRem $Pad $Pad.remarks 1 $id"
      $Pad.optrem.menu.to add command -label $Data(Id$id) -command "Writer::FVCN::SetRem $Pad $Pad.remarks 2 $id"
   }

   menubutton $Pad.optobs -bg white -bd 0 -menu $Pad.optobs.menu -image opt -width $Writer::Data(Height) -height $Writer::Data(Height)
   menu $Pad.optobs.menu -bd 1 -tearoff 0 -activeborderwidth 0 -bg white
      foreach lbl [list $Data(OBS) $Data(EST)] {
         $Pad.optobs.menu add radiobutton -indicatoron false -label $lbl -variable Writer::FVCN::Data(Obs$Pad) -value $lbl \
            -command "Writer::FVCN::UpdateGraphItems $Pad"
      }

   radiobutton $Pad.optash00 -variable Writer::FVCN::Data(Hour$Pad) -value 00 -indicatoron false -image box -selectimage check \
      -bg white -bd 0 -selectcolor white -width $Writer::Data(Height) -height $Writer::Data(Height) \
      -command { SPI::ToolMode Writer::FVCN Draw True; if { $Writer::Data(Canvas)!="" } { $Writer::Data(Canvas) delete FVCN } }

   foreach h { 06 12 18 } {
      radiobutton $Pad.optash$h -variable Writer::FVCN::Data(Hour$Pad) -value $h -indicatoron false -image box -selectimage check \
         -bg white -bd 0 -selectcolor white -width $Writer::Data(Height) -height $Writer::Data(Height) \
         -command { SPI::ToolMode Writer::FVCN Draw True }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::AshUpdate>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Mise a jour des regions de cendres pour une heure specifie
#            dans la description textuelle.
#
# Parametres :
#  <Pad>     : Identificateur du Pad
#  <Hour>    : Heure
#  <Text>    : Texte de la prevision des regions
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::AshUpdate { Pad Hour { Text "" } } {
   variable Data

   if { $Text=="" } {

      #----- Convertir en format text
      foreach l { L0 L1 L2 } lvl { "SFC/FL200" "FL200/FL350" "FL350/FL600" } {

         set coords ""

         if { [llength $Data($l$Hour$Pad)] } {
            foreach { lat lon elev } $Data($l$Hour$Pad) {
               lappend coords [Writer::FVCN::FormatCoord $lat $lon]
            }
            lappend coords [Writer::FVCN::FormatCoord [lindex $Data($l$Hour$Pad) 0] [lindex $Data($l$Hour$Pad) 1]]
         }

         if { [llength $coords] } {
            append Text "$lvl [join $coords " - "] "
         }
      }

      if { $Text=="" } {
         if { $Hour=="00" } {
            if { $Data(Mode$Pad)=="EXP" } {
               set Text $Data(EST1)
            } else {
               set Text $Data(NoVA00)
            }
         } else {
            set Text "NO VA EXP"
         }
      }
   }

   set Data(FCSTh$Hour$Pad) $Text
   $Pad.ash$Hour delete 0.0 end
   $Pad.ash$Hour insert 0.0 $Text

   set Data(HAsh$Hour$Pad) [Writer::TextExpand $Pad.ash$Hour 47]
   Writer::FVCN::PageInit $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::PrintCommand>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer la commande d'impression "plugin" pour les cas RSMC a l'interieur
#            de la PrintBox.
#
# Parametres :
#   <Canvas> : Identificateur du canvas
#
# Retour:
#
# Remarques :
#   Cette fonction est appelee par PrintBox pour effectuer l'impression selon les
#   parametres du PrintBox lui-meme et du "Widget PlugIn" PrintBox::PlugInCommand
#   definit ci-haut.
#
#----------------------------------------------------------------------------

proc Writer::FVCN::PrintCommand { Canvas } {
   variable Data

   set file [Writer::FVCN::Format $Writer::Data(Pad) $Data(Mode$Writer::Data(Pad))]
   set PrintBox::Print(FullName) [string trimright $PrintBox::Print(FullName) ".$PrintBox::Print(Device)"]

   PrintBox::PrintTXT $file

   #----- Graphical product
   if { [winfo exists $Data(Page$Writer::Data(Pad))] } {
      set PrintBox::Print(FullName) $PrintBox::Print(FullName)
      set PrintBox::Print(Angle) landscape
      PrintBox::PrintCommand $Data(Page$Writer::Data(Pad))
   }

   PrintBox::Destroy
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::SetDetails>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer les details pre-defini.
#
# Parametres :
#    <Pad>   : Identificateur du Pad
#    <Text>  : Identificateur du widget.
#    <No>    : Nuomero du type d'advisory.
#    <FV>    : Id du FV.
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::SetDetails { Pad Text No FV } {
   variable Data

   $Text delete 0.0 end

   switch $No {
      1 { $Text insert 0.0 "$Data(Details1)" }
      2 { $Text insert 0.0 "$Data(Details2)" }
      3 { $Text insert 0.0 "$Data(Details3)" }
   }

   set Data(HDetails$Pad) [Writer::TextExpand $Text 47 64]
   Writer::FVCN::PageInit $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::SetNext>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer le prochaine adisory.
#
# Parametres :
#    <Pad>   : Identificateur du Pad
#    <Text>  : Identificateur du widget.
#    <No>    : Nuomero du type d'advisory.
#    <FV>    : Id du FV.
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::SetNext { Pad Text No FV } {
   variable Data

   $Text delete 0.0 end

   switch $No {
      1 { $Text insert 0.0 "$Data(Next1) $Data(Next$Pad)" }
      2 { $Text insert 0.0 "$Data(Next2) $Data(Next$Pad)" }
      3 { $Text insert 0.0 "$Data(Next3)" }
   }

   set Data(HNext$Pad) [Writer::TextExpand $Text 47]
   Writer::FVCN::PageInit $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::SetRem>
# Creation : Juin 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Inserer la remarque.
#
# Parametres :
#    <Pad>   : Identificateur du Pad
#    <Text>  : Identificateur du widget.
#    <No>    : Nuomero du type de remarque.
#    <FV>    : Id du FV.
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::SetRem { Pad Text No FV } {
   variable Data

   $Text delete 0.0 end

   set no $Data(No$FV)
   switch $No {
      1 { $Text insert 0.0 "$Data(Rem1) $Data(Id$FV). $Data(Rem12) ${no}___" }
      2 { $Text insert 0.0 "$Data(Rem2) $Data(Id$FV). $Data(Rem22) $Data(Id$FV) $Data(Rem23) ${no}___" }
      3 { $Text insert 0.0 "$Data(Rem3)" }
      4 { $Text insert 0.0 "$Data(Rem4)" }
      5 { $Text insert 0.0 "$Data(Rem5)" }
      default { $Text insert 0.0 "NIL" }
   }

   set Data(HRemarks$Pad) [Writer::TextExpand $Text 47 256]
   Writer::FVCN::PageInit $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::PageInit>
# Creation : Septembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue l'affichage des parties fixe du resume.
#
# Parametres :
#  <Canvas>  : Identificateur du canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::PageInit { Pad } {
   global   GDefs
   variable Data

   $Pad.canvas delete HEADER DESC INFO ASH NEXT FOOTER WIN

   #----- Numero de message

   set y 4
   set x [expr 10+$Writer::Data(Width)*22 ]

   $Pad.canvas create window 2 $y -anchor nw -tags WIN -window $Pad.no
   $Pad.canvas create text [expr 2+$Writer::Data(Width)*7] $y -anchor nw -font XFont12 -tags HEADER -text "$Data(Id$Pad)"
   $Pad.canvas create window [expr 2+$Writer::Data(Width)*12] $y -anchor nw -tags WIN -window $Pad.date
   $Pad.canvas create window [expr 2+$Writer::Data(Width)*19] $y -anchor nw -tags WIN -window $Pad.corid
   incr y $Writer::Data(Height)

   #----- Identification du message

   $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags HEADER -text "VA ADVISORY"
   incr y $Writer::Data(Height)

   $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags HEADER -text "DTG:"
   $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.issued
   incr y $Writer::Data(Height)

   $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags HEADER -text "VAAC:"
   $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.vaac
   set y [expr $y+$Writer::Data(Height)*$Data(HVAAC$Pad)]

   #----- Description

   $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags DESC -text "VOLCANO:"
   $Pad.canvas create window $x $y -anchor ne -tags WIN -window $Pad.optvolcano
   $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.volcano
   incr y $Writer::Data(Height)

   $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags DESC -text "PSN:"
   $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.location
   incr y $Writer::Data(Height)

   $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags DESC -text "AREA:"
   $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.area
   incr y $Writer::Data(Height)

   $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags DESC -text "SUMMIT ELEV:"
   $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.elev
   incr y $Writer::Data(Height)

   $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags DESC -text "ADVISORY NR:"
   $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.advisory
   incr y $Writer::Data(Height)

   #----- Retransimssion

   if { $Data(Mode$Pad)=="RET" } {

      #----- Bas de page

      $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags NEXT -text "RMK:"
      $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.remarks

   } else {

      #----- Information

      $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags INFO -text "INFO SOURCE:"
      $Pad.canvas create window $x $y -anchor ne -tags WIN -window $Pad.optinfo
      $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.info
      set y [expr $y+$Writer::Data(Height)*$Data(HInfo$Pad)]

      $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags INFO -text "AVIATION COLOUR CODE:"
      $Pad.canvas create window $x $y -anchor ne -tags WIN -window $Pad.optcode
      $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.code
      incr y $Writer::Data(Height)

      $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags INFO -text "ERUPTION DETAILS:"
      $Pad.canvas create window $x $y -anchor ne -tags WIN -window $Pad.optdetails
      $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.details
      set y [expr $y+$Writer::Data(Height)*$Data(HDetails$Pad)]

      #----- Ash data

      $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags ASH -text "OBS VA DTG:"
      $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.ash
      incr y $Writer::Data(Height)

      $Pad.canvas create window  1 $y -anchor nw -tags ASH -window $Pad.obs
      $Pad.canvas create window [expr $x-$Writer::Data(Height)] $y -anchor ne -tags WIN -window $Pad.optobs
      $Pad.canvas create window $x $y -anchor ne -tags WIN -window $Pad.optash00
      $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.ash00
      set y [expr $y+$Writer::Data(Height)*$Data(HAsh00$Pad)]

      $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags ASH -text $Data(FCST06)
      $Pad.canvas create window $x $y -anchor ne -tags WIN -window $Pad.optash06
      $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.ash06
      set y [expr $y+$Writer::Data(Height)*$Data(HAsh06$Pad)]

      $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags ASH -text $Data(FCST12)
      $Pad.canvas create window $x $y -anchor ne -tags WIN -window $Pad.optash12
      $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.ash12
      set y [expr $y+$Writer::Data(Height)*$Data(HAsh12$Pad)]

      $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags ASH -text $Data(FCST18)
      $Pad.canvas create window $x $y -anchor ne -tags WIN -window $Pad.optash18
      $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.ash18
      set y [expr $y+$Writer::Data(Height)*$Data(HAsh18$Pad)]

      #----- Whats next

      $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags NEXT -text "RMK:"
      $Pad.canvas create window $x $y -anchor ne -tags WIN -window $Pad.optrem
      $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.remarks
      set y [expr $y+$Writer::Data(Height)*$Data(HRemarks$Pad)]

      if { $Data(Mode$Pad)=="NEW" } {
         $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.www
         set y [expr $y+$Writer::Data(Height)*2]
      }

      $Pad.canvas create text 2 $y -anchor nw -font XFont12 -tags NEXT -text "NXT ADVISORY:"
      $Pad.canvas create window $x $y -anchor ne -tags WIN -window $Pad.optnext
      $Pad.canvas create window $x $y -anchor nw -tags WIN -window $Pad.next
      set y [expr $y+$Writer::Data(Height)*$Data(HNext$Pad)]

   }

   #----- Graphical product

   Writer::FVCN::GraphUpdate $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::Read>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire le contenu d'un message
#
# Parametres :
#  <File>    : Path complet du fichier
#
# Retour    :
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::Read { Pad File Mode } {
   global GDefs
   variable Data

   set f [open $File r]

   gets $f Data(No$Pad)
   gets $f Data(Id$Pad)
   gets $f Data(Date$Pad)
   gets $f Data(Cor$Pad)
   gets $f Data(Issued$Pad)
   gets $f line
   gets $f Data(Site$Pad)
   gets $f Data(Location$Pad)
   gets $f Data(Area$Pad)
   gets $f Data(Elev$Pad)
   gets $f Data(Advisory$Pad)
   gets $f line ; $Pad.info insert 0.0 $line

   if { $Mode=="NEW" } {
      gets $f Data(Code$Pad)
      gets $f line ; $Pad.details insert 0.0 $line
      gets $f Data(Obs$Pad)

      #----- Have to check for older file format
      if { [string index $Data(Obs$Pad) end]!=":" } {
         set Data(Date00$Pad) $Data(Obs$Pad)
         gets $f line
         $Pad.ash00 insert 0.0 $line
         set hours { 06 12 18 }
      } else {
         set hours { 00 06 12 18 }
      }

      foreach h $hours {
         gets $f line
         set Data(Date$h$Pad) [lindex $line 0]
         set Data(L0$h$Pad)   [lindex $line 1]
         set Data(L1$h$Pad)   [lindex $line 2]
         set Data(L2$h$Pad)   [lindex $line 3]
         if { [llength $line]==5 } {
            Writer::FVCN::AshUpdate $Pad $h [lindex $line 4]
         } else {
            Writer::FVCN::AshUpdate $Pad $h
         }
      }
      gets $f line ; $Pad.next insert 0.0 $line
   }
   gets $f line  ; $Pad.remarks insert 0.0 $line

   #----- Get graphical FVCN view if it exists
   catch {
      gets $f line
      Viewport::GoTo $Data(Page$Pad) [lindex $line 10] [lindex $line 11] [lindex $line 3] [lindex $line 1] [lindex $line 0] [lindex $line 2]
   }

   close $f

   #----- Update des dimension des textes

   set Data(HDetails$Pad) [Writer::TextExpand $Pad.details 47]
   set Data(HInfo$Pad)    [Writer::TextExpand $Pad.info 47]
   set Data(HRemarks$Pad) [Writer::TextExpand $Pad.remarks 47]
   set Data(HNext$Pad)    [Writer::TextExpand $Pad.next 47]
}

#-------------------------------------------------------------------------------
# Nom      : <Writer::FVCN::Send>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Transmettre le message.
#
# Parametres :
#   <Pad>    : Identificateur du Pad
#   <Backup> : Methode de backup
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Writer::FVCN::Send { Pad { Backup 0 } } {
   global   GDefs
   global   env
   variable Data

   #----- Sauvegarder le message

   set name [Writer::FVCN::Write $Pad 1]
   set file [Writer::FVCN::Format $Pad $Data(Mode$Pad)]

   #----- Transmettre le message avec le script operationnel.

   exec chmod 644 $file

   if { $Backup } {
      Debug::TraceProc "Writer::FVCN::Send: Sending via metmanager $name"
      set ErrCatch [catch  { exec ssh metmgr1 -l $GDefs(TransmitUser) -n -x ". ~/.profile; export DISPLAY=$env(DISPLAY); export TERM=$env(TERM); /opt/mm/bin/amxmit -s ncp1lx $file " } MsgCatch]

      if { $ErrCatch != 0 } {
         Debug::TraceProc "Error : Unable to sent the $file via metmanager.\n\n$MsgCatch"
      }

   } else {
      Debug::TraceProc "Writer::FVCN::Send: Sending via nanproc $name"
      if { $GDefs(FrontEnd)!=$GDefs(Host) } {
         set ErrCatch [catch { exec ssh $GDefs(FrontEnd) -l $GDefs(TransmitUser) -n -x ". ~/.profile; nanproc -bs -p b -f $file " } MsgCatch]

         if { $ErrCatch != 0 } {
            Debug::TraceProc "Error : Unable to sent the $file via nanproc.\n\n$MsgCatch"
         }

      } else {
         set ErrCatch [catch { exec nanproc -bs -p b -f $file } MsgCatch]

         if { $ErrCatch != 0 } {
            Debug::TraceProc "Error : Unable to sent the $file via nanproc.\n\n$MsgCatch"
         }
      }
   }

   catch  { exec ssh $GDefs(FrontEnd) -l $GDefs(TransmitUser) -n -x ". ~/.profile; webprods -f $file -s weather -D 0 -p eer/data/vaac/FVCN_messages/$name.txt"  }

   #----- Graphical product
   if { [winfo exists $Data(Page$Pad)] } {
      PrintBox::Image $Data(Page$Pad) png $file landscape
      exec chmod 644 $file.png
      catch  { exec ssh $GDefs(FrontEnd) -l $GDefs(TransmitUser) -n -x ". ~/.profile; webprods -f $file.png -s weather -D 0 -p eer/data/vaac/FVCN_messages/$name.png"  }
   }

   if { !$Backup } {
      file delete -force $file
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::Source>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Recuperer les informations d'une source et initialiser les
#            parametres qui en depande.
#
# Parametres :
#  <Array>   : Variable array
#  <Index>   : Index dans la variable Array
#  <Op>      : Operation effectuer sur la variable
#
# Retour:
#
# Remarques :
#   -Cette procedure repond a un "trace" sur une variable a l'interieur de SPI
#    afin de recuperer l'information necessaire automatiquement (SPI::Src(Info))
#
#----------------------------------------------------------------------------

proc Writer::FVCN::Source { Array Index Op } {

   Writer::FVCN::Site $SPI::Src(No) "[string toupper $SPI::Src(Name)]" $SPI::Src(Lat) $SPI::Src(Lon) $SPI::Src(Elev) "[string toupper $SPI::Src(Area)]"
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::ToolBar>
# Creation : Fevrier 2006 - J.P. Gauthier - CMC/CMOE
#
# But      : Installer les outils selon le type de message
#
# Parametres :
#  <Pad>     : Frame ou integrer les fonctions
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::ToolBar { Pad } {
   global GDefs

   checkbutton $Pad.head.mode -variable Page::Data(ToolMode) -onvalue Writer::FVCN -offvalue SPI \
      -image ARROW -indicatoron 0 -relief sunken -bd 1 -overrelief raised -offrelief flat -selectcolor $GDefs(ColorFrame) \
      -command { SPI::ToolMode $Page::Data(ToolMode) Draw True }
   button $Pad.head.save -image OPEN -bd 0 -relief flat -overrelief raised \
      -command { Writer::${Writer::Data(Type)}::Write $Writer::Data(Pad) 0 }
   button $Pad.head.print -image PRINT -bd 0 -relief flat -overrelief raised \
      -command { PrintBox::Create $Writer::Data(Pad).canvas PRINT Writer::$Writer::Data(Type) }
   button $Pad.head.send -image ENVELOPE -bd 0 -relief flat -overrelief raised \
      -command { Writer::Send }
   button $Pad.head.send2 -image ENVELOPE2 -bd 0 -relief flat -overrelief raised \
      -command { Writer::Send 1 }
   checkbutton $Pad.head.test -variable Writer::${Writer::Data(Type)}::Data(Test$Pad) -onvalue True -offvalue False \
      -text [lindex $Writer::Lbl(Test) $GDefs(Lang)] -indicatoron 0 -relief groove -bd 2 -overrelief flat -offrelief groove  \
      -command { Writer::${Writer::Data(Type)}::Test $Writer::Data(Pad) }
   button $Pad.head.close -image DELETE -bd 0 -relief flat -overrelief raised \
      -command { Writer::PadClose 1 }
   pack $Pad.head.mode $Pad.head.save $Pad.head.print $Pad.head.send $Pad.head.send2 -side left -padx 2
   pack $Pad.head.test -side left -ipadx 2 -padx 2 -fill y
   pack $Pad.head.close -side right -padx 2

   Bubble::Create $Pad.head.mode  [lindex $Writer::Bubble(Select) $GDefs(Lang)]
   Bubble::Create $Pad.head.save  [lindex $Writer::Bubble(Save) $GDefs(Lang)]
   Bubble::Create $Pad.head.print [lindex $Writer::Bubble(Print) $GDefs(Lang)]
   Bubble::Create $Pad.head.send  [lindex $Writer::Bubble(Send) $GDefs(Lang)]
   Bubble::Create $Pad.head.send2 [lindex $Writer::Bubble(SendBackup) $GDefs(Lang)]
   Bubble::Create $Pad.head.close [lindex $Writer::Bubble(Close) $GDefs(Lang)]
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::Test>
# Creation : Novembre 20008 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher les la chaines TEST dans le message
#
# Parametres :
#  <Pad>     : Frame du message
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------
proc Writer::FVCN::Test { Pad } {
   variable Data

   if { [winfo exist $Data(Page$Pad)] } {
      if { $Data(Test$Pad) } {
         set x 800
         set y 20
         for { set i 0 } { $i < 5 } { incr i } {
            $Data(Page$Pad).page.canvas create image $x $y -image TEST -tags "TEST" -anchor ne
            incr x -128
            incr y 128
         }
         Shape::BindMove $Data(Page$Pad).page.canvas TEST
      } else {
         $Data(Page$Pad).page.canvas delete TEST
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::UpdateTime>
# Creation : Mai 1999 - J. P. Gauthier - CMC/CMOE
#
# But      : Ajuster les dates et heures de toutes les informations en comportant.
#
# Parametres :
#   <Pad>    : Identificateur du Pad
#   <Delay>  : Refresh time in milliseconds
#   <Hours>  : Hours to next advisory
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::UpdateTime { Pad Delay { Hours 6 } } {
   variable Data

   if { $Data(Handle$Pad) != "" } {

      set Data(Seconds)     [clock seconds]
      set Data(Date$Pad)    [clock format $Data(Seconds) -format "%d%H%M" -gmt True]
      set Data(Issued$Pad)  [clock format $Data(Seconds) -format "%Y%m%d/%H%MZ" -gmt True]
      set Data(Year$Pad)    [clock format $Data(Seconds) -format "%Y" -gmt True]

      # ----- Determiner le moment du prochain message, si il
      #       y a lieu (6 heures plus tard).
      #       Notez bien qu'on determine toujours cette date,
      #       puisque l'usager pourra changer la valeur du
      #       statement actuel ( "NO FURTHER" ou "NEXT ADVISORY" ).

      set next "[expr $Data(Seconds) + $Hours * 3600]"
      set Data(Next$Pad)  "[string toupper [clock format $next -format "%Y%m%d/%H%MZ" -gmt True]]"

      if { $Delay } {
         set Data(Handle$Pad) [after $Delay "Writer::FVCN::UpdateTime $Pad $Delay $Hours"]
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::VertexAdd>
# Creation : Septembvre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajout d'un point au polygone.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X de la souris
#  <Y>       : Coordonnee Y de la souris
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::VertexAdd { Frame VP X Y } {
   variable Data

   if { $VP==-1 } {
      return
   }

   if { [winfo exists $Writer::Data(Canvas)] } {
      $Writer::Data(Canvas) delete FVCN
   }

   set field [lindex [Viewport::Assigned $Frame $VP fstdfield] 0]

   if { [fstdfield is $field] } {

      set loc [$VP -unproject $X $Y]
      set X   [lindex $loc 0]
      set Y   [lindex $loc 1]

      #----- si le vertex est valide on l'ajoute a la liste

      if { $X!=-999 && $Y!=-999 } {

         set p $Writer::Data(Pad)
         set h $Data(Hour$p)

         set date [fstdstamp todate [fstdfield define $field -DATEV]]
         set Data(Date$h$p) "[lindex $date 2]/[lindex $date 3][lindex $date 4]Z"
         set Data(Level$p) [lindex { L0 L0 L0 L1 L2 } [expr [fstdfield define $field -IP3]>4?0:[fstdfield define $field -IP3]]]

         lappend Data($Data(Level$p)$h$p) $X $Y 0

         set Writer::Data(Canvas) $Page::Data(Canvas)
         set Writer::Data(Frame)  $Frame
         set Writer::Data(VP)     $VP

         Writer::FVCN::AshUpdate   $p $h
         Writer::FVCN::UpdateItems $Frame $VP $p
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::VertexDelete>
# Creation : Septembvre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Suppression d'un point au polygone.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X de la souris
#  <Y>       : Coordonnee Y de la souris
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::VertexDelete { Frame VP } {
   variable Data

   set field [lindex [Viewport::Assigned $Frame $VP fstdfield] 0]

   if { [winfo exists $Writer::Data(Canvas)] } {
      $Writer::Data(Canvas) delete FVCN
   }

   if { [fstdfield is $field] } {

      set p $Writer::Data(Pad)
      set h $Data(Hour$p)

      set Data(Level$p) [lindex { L0 L0 L0 L1 L2 } [expr [fstdfield define $field -IP3]>4?0:[fstdfield define $field -IP3]]]
      set Data($Data(Level$p)$h$p) [lreplace $Data($Data(Level$p)$h$p) end-2 end]

      set Writer::Data(Canvas) $Page::Data(Canvas)
      set Writer::Data(Frame)  $Frame
      set Writer::Data(VP)     $VP

      Writer::FVCN::AshUpdate   $p $h
      Writer::FVCN::UpdateItems $Frame $VP $p
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::VertexFollow>
# Creation : Septembvre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une ligne entre le dernier vertex creer et la position du
#            curseur de la souris.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#  <X>       : Coordonnee X de la souris
#  <Y>       : Coordonnee Y de la souris
#  <Scan>    : Mode Scan
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::VertexFollow { Frame VP X Y Scan } {
   global   GDefs
   variable Data

   if { $VP==-1 } {
      return
   }

   if { [winfo exists $Writer::Data(Canvas)] } {
      $Writer::Data(Canvas) delete VERTEXFOLLOW
   }

   set field [lindex [Viewport::Assigned $Frame $VP fstdfield] 0]

   if { [fstdfield is $field] } {

      set loc [$VP -unproject $X $Y]
      set X [lindex $loc 0]
      set Y [lindex $loc 1]

      if { $X!=-999 && $Y!=-999 } {

         set p $Writer::Data(Pad)
         set h $Data(Hour$p)

         set Data(Level$p) [lindex { L0 L0 L0 L1 L2 } [expr [fstdfield define $field -IP3]>4?0:[fstdfield define $field -IP3]]]

         set tmp $Data($Data(Level$p)$h$p)
         lappend tmp $X $Y 0

         set Writer::Data(Canvas) $Page::Data(Canvas)
         set Writer::Data(Frame)  $Frame
         set Writer::Data(VP)     $VP

         Viewport::DrawArea $Frame $VP $tmp "$Page::Data(Tag)$VP FVCN VERTEXFOLLOW" VERTEXFOLLOW red red\
            @$GDefs(Dir)/Resources/Bitmap/raydiagleft08.xbm 0 2
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::Site>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : format le retour de la sous-routine de selection de source.
#
# Parametres :
#    Aucun.
#
# Remarques :
#    - le nom et la sous-region du volcan contenu dans la ligne
#      qui est retournee par le selecteur est encoder comme suit :
#      tout les blancs ont ete substituer par des underscores (_).
#
#----------------------------------------------------------------------------

proc Writer::FVCN::Site { No Name Lat Lon Elev Area } {
   variable Data

   if { $Writer::Data(Type)!="FVCN" } {
      return
   }

   # ----- Ajustement le numero associe a l'entete FVCN du message.

   set Data(Lat$Writer::Data(Pad)) $Lat
   set Data(Lon$Writer::Data(Pad)) $Lon

   set Data(No$Writer::Data(Pad)) [GetNo $Name]
   if { $Data(No$Writer::Data(Pad))!="" } {

      Debug::TraceProc "Writer::FVCN::Site: Valid source selected ($Name)"

      set Data(Advisory$Writer::Data(Pad)) [GetAdvisory $Writer::Data(Pad) $Name]

      if  { $Name!="UNKNOWN" && $Name!="UNNAMED"} {
         set Data(Site$Writer::Data(Pad))     "$Name $No"
         set Data(Location$Writer::Data(Pad)) "[Writer::FVCN::FormatCoord $Lat $Lon]"
         set Data(Elev$Writer::Data(Pad))     "${Elev}M"
         set Data(Area$Writer::Data(Pad))     "$Area"
      } else {
         set Data(Site$Writer::Data(Pad))     "$Name"
         set Data(Location$Writer::Data(Pad)) "UNKNOWN"
         set Data(Elev$Writer::Data(Pad))     ""
         if { $Data(Mode$Writer::Data(Pad))=="EXP" } {
            set Data(Area$Writer::Data(Pad))  ""
         } else {
            set Data(Area$Writer::Data(Pad))  $Data(NA)
         }
      }

      #----- Update graphical product

      Writer::FVCN::GraphUpdate $Writer::Data(Pad) True
   } else {
      Debug::TraceProc "Writer::FVCN::Site: Invalid source ($Name)"
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::UpdateItems>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      :
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du viewport
#  <Pad>     : Identificateur du pad
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::UpdateItems { Frame { VP "" } { Pad "" } } {
   global GDefs
   variable Data

   if { $Pad=="" } {
      set Pad $Writer::Data(Pad)
   }

   if { $Frame==$Writer::Data(Frame) } {
      if { $VP=="" } {
         set VP $Writer::Data(VP)
      }

      if { [winfo exists $Writer::Data(Canvas)] } {
         $Writer::Data(Canvas) delete FVCN
      }

      if { ![llength [info commands $VP]] } {
         set Writer::Data(VP) ""
      }

      if { $Pad!="" && $Writer::Data(VP)!="" } {
         Viewport::DrawArea $Writer::Data(Frame) $Writer::Data(VP) $Data($Data(Level$Pad)$Data(Hour$Pad)$Pad) "$Page::Data(Tag)$VP FVCN" FVCN red red\
            @$GDefs(Dir)/Resources/Bitmap/raydiagleft08.xbm 0 2
      }
   }

   Writer::FVCN::UpdateGraphItems $Pad
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::UpdateGraphItems>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Mettre a jour les items de region
#
# Parametres :
#  <Pad>     : Identificateur du pad
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Writer::FVCN::UpdateGraphItems { Pad } {
   global GDefs
   variable Data

   if { [winfo exists $Data(Page$Pad)] } {

      Writer::FVCN::GraphAreaColor $Pad

      set f $Data(Page$Pad)
      $Data(Page$Pad).page.canvas delete ICOVAAC

      #----- Loop on each hours with the associated label
      foreach h { 00 06 12 18 } l [list "$Data(Obs$Pad)" $Data(FCST06) $Data(FCST12) $Data(FCST18)] {

         #----- configure the label
         $Data(Page$Pad).page.canvas itemconfigure DATE$h -text "$l $Data(Date$h$Pad)"
         $Data(Page$Pad).page.canvas delete FVCN$h

         #----- Loop on all known flight level layer for this hour
         set no 0
         set va 0
         foreach lvl $Data(LVL$h)  {
            set i -1
            set coords {}

            #----- Figure out index of this area within the known layer for color and filling
            foreach l { L0 L1 L2 } {
               if { [llength [set coords $Data($l$h$Pad)]] } {
                  incr i
               }
               if { $i==$no } {
                  break
               }
            }

            #----- If the area is valid, draw it
            if  { [llength $coords)]>=4 } {
               Viewport::DrawArea $Data(Page$Pad) $Data(VP$h$f) $coords "$Page::Data(Tag)$Data(VP$h$f) FVCN$no$h FVCN$h FVCN" FVCN$no$h \
                  $Data(Color$lvl) $Data(Color$lvl) $Data(Stipple$lvl) False 2
               incr va
            }
            incr no
         }

         #----- Set no ash label if area is not defined
         if { $va } {
            $Data(Page$Pad).page.canvas itemconfigure NOVA$h -text ""
         } else {
            $Data(Page$Pad).page.canvas itemconfigure NOVA$h -text $Data(NoVA$h)
         }
         if { [set xy [ $Data(VP$h$f) -project $Data(Lat$Writer::Data(Pad)) $Data(Lon$Writer::Data(Pad)) 0]]!="" && [lindex $xy 2]>0 } {
            Shape::DrawIcoVAAC $Data(Page$Pad).page.canvas $xy "ICOVAAC" black 5 False
         }
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Writer::FVCN::Write>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Ecrire le messgae dans un fichier selon un format specifique.
#
# Parametres :
#  <Pad>     : Identificateur du PAd
#  <Sent>    : Message envoye (-1 = autosave mode).
#
# Retour    :
#   <File>  : Nom du fichier
#
# Remarques :
#    - noter que le format est strict, ceci de facon a relire
#      le message correctement.
#
#    - traiter le cas ou l'usager entre un delimitateur... ou
#      simplement changer de delimitateur.
#      On peut simplement illiminer les lignes des textes qui
#      contiennent des points (.).
#
#----------------------------------------------------------------------------

proc Writer::FVCN::Write { Pad Sent } {
   global GDefs
   variable Data

   if { $Sent==-1 } {
      set file $Writer::Data(AutoSaveFile)
   } else {

      #----- On s'assure que le nom du fichier soit qu'une seul chaine.

      regsub -all "\[^a-zA-Z0-9-\]" $Data(Site$Pad) - file

      #----- Determine le nom du fichier.

      set date [clock format $Data(Seconds) -format "%Y%m%d-%H%MZ" -gmt True]

      if { !$Sent } {
         set file $Data(No$Pad)-[lindex [split $Data(Advisory$Pad) /] end]_${file}.msg
      } else {
         set file ${date}_$Data(No$Pad)-[lindex [split $Data(Advisory$Pad) /] end]_${file}.msg
      }

      if { $Data(Mode$Pad)=="RET" } {
         set file $file.ret
      }

      if { $Data(Cor$Pad)!="" } {
         set file $file.cor
      }

      if { $Sent } {
         set file $file.sent
         set Data(Sent$Pad) 1
      }

      if { [file exists $GDefs(DirMsg)/FVCN/$file] } {
         set ok [Dialog::CreateDefault .writer 300 [lindex $Writer::Lbl(Warning) $GDefs(Lang)] \
            "[lindex $Writer::Msg(Exist) $GDefs(Lang)]\n\t$file\n" \
            info 0 [lindex $Writer::Lbl(No) $GDefs(Lang)] [lindex $Writer::Lbl(Yes) $GDefs(Lang)]]

         if { !$ok } {
            return ""
         }
      }
   }

   set f [open $GDefs(DirMsg)/FVCN/$file w]

   puts $f "$Data(No$Pad)"
   puts $f "$Data(Id$Pad)"
   puts $f "$Data(Date$Pad)"
   puts $f "$Data(Cor$Pad)"
   puts $f "$Data(Issued$Pad)"
   puts $f "$Data(VAAC)"
   puts $f "$Data(Site$Pad)"
   puts $f "$Data(Location$Pad)"
   puts $f "$Data(Area$Pad)"
   puts $f "$Data(Elev$Pad)"
   puts $f "$Data(Advisory$Pad)"
   puts $f "[Writer::TextExtract none 47 "" $Pad.info]"

   if { $Data(Mode$Pad)=="RET" } {
      puts $f "[Writer::TextExtract none 47 "" $Pad.remarks]"
   } else {
      puts $f "$Data(Code$Pad)"
      puts $f "[Writer::TextExtract none 47 "" $Pad.details]"
      puts $f "$Data(Obs$Pad)"
      foreach h { 00 06 12 18 } {
         set Data(FSCT$h$Pad) [Writer::TextExtract none 47 "" $Pad.ash$h]
         puts $f "{$Data(Date$h$Pad)} {$Data(L0$h$Pad)} {$Data(L1$h$Pad)} {$Data(L2$h$Pad)} {$Data(FSCT$h$Pad)}"
      }
      puts $f "[Writer::TextExtract none 47 "" $Pad.remarks]"
      puts $f "[Writer::TextExtract none 47 "" $Pad.next]"
   }

   #----- Save graphical FVCN view
   if { [winfo exists $Data(Page$Pad)] } {
      puts $f [ProjCam::Mem $Data(Page$Pad) _____]
   }

   close $f

   exec chgrp cmcfe $GDefs(DirMsg)/FVCN/$file
   exec chmod 660 $GDefs(DirMsg)/FVCN/$file

   return $file
}
