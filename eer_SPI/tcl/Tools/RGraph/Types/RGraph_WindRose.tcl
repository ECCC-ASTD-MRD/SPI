
namespace eval RGraph::WindRose {
    variable Data
    variable Param

    set Param(Title)        {"Rose des vents" "Wind Rose"}
    set Param(RPackages)    {openair}

    variable Lbl

    set Lbl(WindSpeedInt)   {"Intervalle vitesse " "Wind speed interval"}
    set Lbl(Angle)          {"Angle              " "Angle              "}
    set Lbl(Type)           {"Type               " "Type               "}
    set Lbl(Color)          {"Palette de couleurs" "Color set          "}
    set Lbl(GridLine)       {"Interval grille    " "Grid line interval "}
    set Lbl(Breaks)         {"Nombre d'étages    " "Number of breaks   "}
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::WindRose::New>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors de la création d'un graph
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <Var>    : Variable contenant les données du graph
#
# Retour:
#
# Remarques : Optionnel
#
#----------------------------------------------------------------------------
proc RGraph::WindRose::New { Id Var } {
    upvar #0 $Var graph

    set graph(WindSpeedInt) 2
    set graph(Angle)        30
    set graph(Type)         default
    set graph(Color)        default
    set graph(GridLine)     ""
    set graph(Breaks)       4
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::WindRose::ParamsWindow>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback pour la création de l'interface spécifique à ce type de graph
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <W>      : Frame où ajouter les widgets
#  <Var>    : Variable contenant les données du graph
#
# Retour:
#
# Remarques : Optionnel
#
#----------------------------------------------------------------------------
proc RGraph::WindRose::ParamsWindow { Id W Var } {
    global GDefs
    variable Lbl

    #----- Wind speed interval
    frame [set f $W.wsi]
        label $f.lbl -text [lindex $Lbl(WindSpeedInt) $GDefs(Lang)]
        entry $f.ent -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable ${Var}(WindSpeedInt) -width 1 -validate key -validatecommand "string is double %P"
        pack $f.lbl -side left
        pack $f.ent -side left -fill x -expand 1
    pack $f -side top -fill x
    bind $f.ent <Key-Return> [list RGraph::UpdateGraph $Id 1]

    #----- Angle of "spokes"
    frame [set f $W.angle]
        label $f.lbl -text [lindex $Lbl(Angle) $GDefs(Lang)]
        entry $f.ent -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable ${Var}(Angle) -width 1 -validate key -validatecommand "string is double %P"
        pack $f.lbl -side left
        pack $f.ent -side left -fill x -expand 1
    pack $f -side top -fill x
    bind $f.ent <Key-Return> [list RGraph::UpdateGraph $Id 1]

    #----- Type (default season year weekday)
    Option::Create $W.type [lindex $Lbl(Type) $GDefs(Lang)] ${Var}(Type) 0 -1 {default year hour month season weekday weekend monthyear daylight dst} [list RGraph::UpdateGraph $Id 1]
    pack $W.type -side top -ipady 1 -fill x

    #----- Color palette (default increment heat jet hue)
    Option::Create $W.color [lindex $Lbl(Color) $GDefs(Lang)] ${Var}(Color) 0 -1 {default increment heat jet hue} [list RGraph::UpdateGraph $Id 1]
    pack $W.color -side top -ipady 1 -fill x

    #----- Grid line
    frame [set f $W.gridline]
        label $f.lbl -text [lindex $Lbl(GridLine) $GDefs(Lang)]
        entry $f.ent -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable ${Var}(GridLine) -width 1 -validate key -validatecommand "string is double %P"
        pack $f.lbl -side left
        pack $f.ent -side left -fill x -expand 1
    pack $f -side top -fill x
    bind $f.ent <Key-Return> [list RGraph::UpdateGraph $Id 1]

    #----- Breaks (number of intervals)
    frame [set f $W.breaks]
        label $f.lbl -text [lindex $Lbl(Breaks) $GDefs(Lang)]
        entry $f.ent -relief sunken -bd 1 -bg $GDefs(ColorLight) -textvariable ${Var}(Breaks) -width 1 -validate key -validatecommand "string is integer %P"
        pack $f.lbl -side left
        pack $f.ent -side left -fill x -expand 1
    pack $f -side top -fill x
    bind $f.ent <Key-Return> [list RGraph::UpdateGraph $Id 1]
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::WindRose::GetData>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Collecte les données nécessaires au graph et les transfère à R
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <Var>    : Variable contenant les données du graph
#  <DataSrc>: Source pour les données
#
# Retour:
#
# Remarques : Nécessaire
#
#----------------------------------------------------------------------------
proc RGraph::WindRose::GetData { Id Var DataSrc } {
    global GDefs
    variable Param

    set msg "([lindex $Param(Title) $GDefs(Lang)]) [lindex $RGraph::Msg(ReadingFld) $GDefs(Lang)]"
    SPI::Progress 0 $msg

    try {
        if { [llength [set fld [lindex [RGraph::GetSelectedFields -vect 1] 0]]] } {
            #----- Get the content of the fieldbox and only keep the fields we want
            set flds {}
            foreach m [FieldBox::GetContent [lindex $fld end]] {
                set ok 1
                foreach idx {0 1 8 12 14} {
                    if { [lindex $m $idx] != [lindex $fld $idx] } {
                        set ok 0
                        break
                    }
                }

                if { $ok } {
                    lappend flds $m
                }
            }
            SPI::Progress 5 $msg

            set lat $RGraph::Data(Lat$Id)
            set lon $RGraph::Data(Lon$Id)

            #----- Process the fields
            set data {}
            set perc [expr {95.0/[llength $flds]}]
            foreach fld $flds {
                if { [catch {
                    fstdfield read RGRAPHFLD$Id [lindex $fld 10] [lindex $fld 11]
                    lassign [fstdfield stats RGRAPHFLD$Id -coordvalue $lat $lon] spd dir
                }] } {
                    continue
                }
                if { $spd != "-" } {
                    lappend data [list [lindex $fld 9] $spd $dir]
                }
                SPI::Progress +$perc $msg
            }

            #----- Transfer the data into the R world
            if { [llength $data] } {
                R tcllst2rdf [list {date ws wd} $data] $Id {STRING 2 DOUBLE}
                R exec "$Id\$date <- as.POSIXct($Id\$date,tz=\"GMT\",format=\"%Y%m%d%H%M\")"
            } else {
                return -code error "No data available for the selected field at that coordinate"
            }
        } else {
            return -code error "No valid field selected, no data will be shown"
        }
    } finally {
        SPI::Progress 0
    }
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::WindRose::RCode>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Retourne le code R pour l'affichage du graph
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <Var>    : Variable contenant les données du graph
#
# Retour:
#
# Remarques : Nécessaire
#
#----------------------------------------------------------------------------
proc RGraph::WindRose::RCode { Id Var } {
    upvar #0 $Var graph

    # TclRSyntax
    return "
windRose($Id,paddle=FALSE,ws.int=$graph(WindSpeedInt),angle=$graph(Angle),type='$graph(Type)',cols='$graph(Color)',grid.line=[expr {$graph(GridLine)==""?"NULL":$graph(GridLine)}],breaks=$graph(Breaks),bias.corr=F,auto.text=F)
"
}
