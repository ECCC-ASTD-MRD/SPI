
namespace eval RGraph::SkewTLogP {
    variable Data
    variable Param

    set Param(Title)        {"Skew-T log-P" "Skew-T log-P"}
    set Param(RPackages)    {RadioSonde}

    variable Lbl

    set Lbl(Winds)          {"Barbules de vent" "Wind barbs "}
    set Lbl(ColTemp)        {"Température     " "Temperature"}
    set Lbl(ColDewpt)       {"Point de rosé   " "Dew point  "}
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::SkewTLogP::New>
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
proc RGraph::SkewTLogP::New { Id Var } {
    global GDefs
    variable Param
    upvar #0 $Var graph

    set graph(Winds)        1
    set graph(ColTemp)      "#000000"
    set graph(ColDewpt)     "#FF0000"
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::SkewTLogP::ParamsWindow>
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
proc RGraph::SkewTLogP::ParamsWindow { Id W Var } {
    global GDefs
    variable Lbl

    #----- Winds toggle
    frame [set f $W.winds]
        label $f.l -text [lindex $Lbl(Winds) $GDefs(Lang)] -anchor w
        checkbutton $f.b -indicatoron 1 -variable ${Var}(Winds) -command [list RGraph::UpdateGraph $Id 1]
        pack $f.l $f.b -side left
    pack $W.winds -side top -anchor w
    
    #----- Color for temperature line
    frame [set f $W.temp]
        label $f.l -text [lindex $Lbl(ColTemp) $GDefs(Lang)] -anchor w
        ColorBox::CreateSel $f.col ${Var}(ColTemp) [list RGraph::UpdateGraph $Id 1]
        pack $f.l $f.col -side left
    pack $f -side top -anchor w
     
    #----- Color for dew point line
    frame [set f $W.dewpt]
        label $f.l -text [lindex $Lbl(ColDewpt) $GDefs(Lang)] -anchor w
        ColorBox::CreateSel $f.col ${Var}(ColDewpt) [list RGraph::UpdateGraph $Id 1]
        pack $f.l $f.col -side left
    pack $f -side top -anchor w
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::SkewTLogP::GetData>
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
proc RGraph::SkewTLogP::GetData { Id Var DataSrc } {
    global GDefs
    variable Param

    set msg "([lindex $Param(Title) $GDefs(Lang)]) [lindex $RGraph::Msg(ReadingFld) $GDefs(Lang)]"
    SPI::Progress 0 $msg

    try {
        #----- Loop on all open file boxes
        set ok 0
        foreach box [FieldBox::Get] {
            set flds {}
            unset -nocomplain ip1s

            #----- If something is selected in that box
            if { [llength [set fld [lindex [FieldBox::GetSelected $box] 0]]] } {
                set datev [lindex $fld 9]

                #----- Loop on all the fields and only keep the ones that might be of interest
                foreach fld [FieldBox::GetContent $box] {
                    if { [lindex $fld 9]==$datev && [lindex $fld 0] in {TT TD ES HR UU VV P0} } {
                        lappend flds $fld
                        if { [lindex $fld 0] != "P0" } {
                            lappend ip1s([lindex $fld 12]) $fld
                        }
                    }
                }

                #----- Break if we have all the fields we need
                set vars [lsort -unique [lmap fld $flds {lindex $fld 0}]]
                if { "UU" in $vars && "VV" in $vars
                    && ( "TD" in $vars || "ES" in $vars || "HR" in $vars )
                    && "TT" in $vars } {
                    set ok 1
                    break
                }
            }
        }
        if { !$ok } {
            return -code error "No valid selection was found in fieldboxes, no data will be shown"
        }
        SPI::Progress 5 $msg

        set lat $RGraph::Data(Lat$Id)
        set lon $RGraph::Data(Lon$Id)

        #----- Read P0 if we have it
        if { "P0" in $vars } {
            set fld [lsearch -exact -index 0 -inline $flds P0]
            fstdfield read RGRAPHP0$Id [lindex $fld 10] [lindex $fld 11]
        }

        #----- Check which var we take for the dew point
        set nb [llength [lsearch -all -exact -index 0 $flds TT]]
        set dpt [expr {
            "TD" in $vars && [llength [lsearch -all -exact -index 0 $flds TD]]==$nb ? "TD" :
            "ES" in $vars && [llength [lsearch -all -exact -index 0 $flds ES]]==$nb ? "ES" :
            "HR"}]
        
        #----- Get the percentage each row processed brings
        set perc [expr {95.0/[array size ip1s]}]

        #----- Loop on the levels
        set data {}
        foreach ip1 [array names ip1s] {
            try {
                set row {}

                #----- Get the temperature
                if { ![llength [set fld [lsearch -inline -exact -index 0 $ip1s($ip1) TT]]] } {
                    continue
                }
                fstdfield read RGRAPHFLD$Id [lindex $fld 10] [lindex $fld 11]
                lappend row [fstdfield stats RGRAPHFLD$Id -coordvalue $lat $lon]

                #----- Calculate the pressure field
                fstdgrid pressure RGRAPHFLD$Id RGRAPHP0$Id
                lappend row [fstdfield stats RGRAPHFLD$Id -coordvalue $lat $lon]

                #----- Read what we'll use for the dew point
                if { ![llength [set fld [lsearch -inline -exact -index 0 $ip1s($ip1) $dpt]]] } {
                    continue
                }
                fstdfield read RGRAPHFLD$Id [lindex $fld 10] [lindex $fld 11]
                lappend row [fstdfield stats RGRAPHFLD$Id -coordvalue $lat $lon]

                #----- Read the winds
                if { ![llength [set fld [lsearch -inline -exact -index 0 $ip1s($ip1) UU]]] } {
                    continue
                }
                fstdfield read RGRAPHFLD$Id [lindex $fld 10] [lindex $fld 11]
                lappend row {*}[fstdfield stats RGRAPHFLD$Id -coordvalue $lat $lon]

                #----- Add to the data pool
                if { "-" ni $row } {
                    lappend data $row
                }
            } on error {} {
                continue
            } finally {
                SPI::Progress +$perc $msg
            }
        }

        #----- Transfer the data into the R world
        if { [llength $data] } {
            R tcllst2rdf [list [list temp press [expr {$dpt=="TD"?"dewpt":$dpt}] wspd dir] $data] $Id {5 DOUBLE}
            R exec "attributes($Id)\$date <- as.POSIXct('$datev',tz=\"GMT\",format=\"%Y%m%d%H%M\")"
            R exec "$Id <- $Id\[order($Id\$press,decreasing=TRUE),\]"
            if { $dpt == "ES" } {
                R exec "$Id\$dewpt <- $Id\$temp - $Id\$ES"
            } elseif { $dpt == "HR" } {
                #----- Found here : https://iridl.ldeo.columbia.edu/dochelp/QA/Basic/dewpoint.html
                #----- RH = 100% * (E/Es)
                #----- E = E0 * exp[(L/Rv)*{(1/T0)-(1/Td)}]
                #----- Es = E0 * exp[(L/Rv)*{(1/T0)-(1/T)}]
                #----- Where E0=0.611 kPa, (L/RV)=5423K, T0=273K
                #----- Ergo: Td = 1/( (1/T) - ln(RH/100)/(L/Rv) )
                R exec "$Id\$dewpt <- 1./( (1./($Id\$temp+273.15)) - log($Id\$RH)/5423 ) - 273.15"
            }
        } else {
            return -code error "No data available for the selected field at that coordinate"
        }
    } finally {
        SPI::Progress 0
        fstdfield free RGRAPHFLD$Id RGRAPHP0$Id
    }
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::SkewTLogP::RCode>
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
proc RGraph::SkewTLogP::RCode { Id Var } {
    upvar #0 $Var graph

    # TclRSyntax
    return "
plotsonde($Id,winds=$graph(Winds),col=c('$graph(ColTemp)','$graph(ColDewpt)'))
"
}
