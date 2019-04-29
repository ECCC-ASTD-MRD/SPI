#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2121 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Package d'interface pour SPI
# Fichier  : RGraph.tcl
# Creation : Décembre 2018
#
# Description:
#    Outil pour faire des graphiques en utilisant le langage R
#===============================================================================

#----- Lire les sources d'execution

set base [file rootname [info script]]
source $base.ctes
source $base.txt
source $base.int

#-------------------------------------------------------------------------------
# Nom      : <RGraph::Call>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Wrapper autour d'une procédure pour n'appeler cette dernière que si
#            elle existe
#
# Parametres :
#  <Proc>   : La procédure à appeler si elle existe
#  <args>   : Arguments de la procédure
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::ProcExists { Proc } {
    return [llength [info procs $Proc]]
}
proc RGraph::Call { Proc args } {
    if { [ProcExists $Proc] } {
        return [$Proc {*}$args]
    }
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::Init>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Procédure d'initialisation du module
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::Init { } {
    variable Data

    if { !$Data(Init) } {
        #----- Put here to speedup SPI startup
        uplevel #0 {
            package require TclR
            package require TclRDevice
        }

        set Data(RPackages)     [R exec -get {library()$results[,'Package']}]
        set Data(GraphTypes)    {}

        #----- Load all the different graphs
        foreach f [glob -nocomplain $::env(HOME)/.spi/RGraph/*.tcl $Data(Home)/types/*.tcl] {
            uplevel #0 source $f
        }

        #----- Compile the list of graphs that we have the packages for
        foreach ns [namespace children] {
            set nsn [namespace tail $ns]
            set ok  1

            #----- Make sure the required R packages are available
            foreach p [set ${ns}::Param(RPackages)] {
                if { $p ni $Data(RPackages) } {
                    Log::Print WARNING "RGraph $nsn removed from available graphs because required package $p is missing"
                    set ok 0
                }
            }

            if { $ok } {
                set ${ns}::Param(Init) 0
                lappend Data(GraphTypes) $nsn
            }
        }

        set Data(Init) 1
    }
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::Destroy>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Assigne le type de graph
#
# Parametres :
#  <Id>     : Identificateur du graph
#  <Type>   : Nouveau type du graph
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::SetType { Id Type } {
    variable Data

    CursorWait $Id {
        set ns ::RGraph::$Type

        if { ![info exists Data(Type$Id)] || $Data(Type$Id)!=$Type } {
            if { [info exists Data(Type$Id)] } {
                Call ::RGraph::$Data(Type$Id)::CleanUp $Id ::RGraph::Data$Id
            }
            unset -nocomplain ::RGraph::Data$Id ::RGraph::RData$Id
            set Data(DataValid$Id) 0
            Call ${ns}::New $Id ::RGraph::Data$Id
            if { [info exists Data(Tab$Id)] } {
                ParamsWindow $Id $Type $Data(Tab$Id)
            }
        }

        set Data(Type$Id) $Type

        #----- Setup the graph type if not already done
        if { ![set ${ns}::Param(Init)] } {
            #----- Load the libraries
            foreach lib [set ${ns}::Param(RPackages)] {
                R exec "library($lib)"
            }
            #----- Run the type-specific setup if defined
            Call ${ns}::RSetup
            #----- Mark setup done
            set ${ns}::Param(Init) 1
        }
    }
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::NewId>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Retourne un nouvel identifiant unique d'objet.
#
# Parametres :
#  <Frame>  : Identificateur de Page
#  <Id>     : Identificateur du graph
#
# Retour: Un nouvel identifiant unique d'objet
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::NewId { {Register 1} } {
    variable Data

    set Data(Id) rdev[incr Data(Nb)]
    if { $Register } {
        lappend Data(Ids) $Data(Id)
    }
    return $Data(Id)
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::Exists>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Test si l'id est valide et est associé à un RGraph ou non
#
# Parametres :
#  <Id>     : Identificateur du graph
#
# Retour: True si valide, False sinon
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::Exists { Id } {
    return [info exists ::RGraph::Data(Graph$Id)]
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::GetFrame>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Retourne le frame d'un RGraph
#
# Parametres :
#  <Id>     : Identificateur du graph
#
# Retour: Le frame contenant le RGraph ou "" si id invalide.
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::GetFrame { Id } {
    variable Data

    if { [info exists Data(Graph$Id)] } {
        return $Data(Graph$Id)
    }
    return ""
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::RCode>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Retourne le code R à exécuter pour un graph donné
#
# Parametres :
#  <Id>     : Identificateur du graph
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::RCode { Id } {
    variable Data

    if { [info exists Data(RCode$Id)] } {
        return $Data(RCode$Id)
    } elseif { [info exists Data(Type$Id)] } {
        return [::RGraph::$Data(Type$Id)::RCode $Id ::RGraph::Data$Id]
    }
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::CursorWait>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Wrapper pour mettre le curseur "watch" sur les trois frame/canvas
#            connectés à un RGraph
#
# Parametres :
#  <Ids>    : Identificateurs des objets à mettre à jour
#  <ROnly>  : 1 pour seulement mettre à jour le graph (code R), 0 pour mettre
#             à jour les données aussi.
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::CursorSet { W C } {
    set c [$W cget -cursor]
    $W configure -cursor $C
    return $c
}
proc RGraph::CursorWait { Id Code } {
    variable Data

    catch {set cg [CursorSet $Data(Canvas$Id) watch]}
    catch {set cp [CursorSet $Data(Param$Id) watch]}
    catch {set cf [CursorSet $Data(Frame$Id).page.canvas watch]}
    update idletasks

    try {
        uplevel 1 $Code
    } finally {
        catch {$Data(Canvas$Id) configure -cursor [expr {$cg=="watch"?"left_ptr":$cg}]}
        catch {$Data(Param$Id) configure -cursor [expr {$cp=="watch"?"left_ptr":$cp}]}
        catch {$Data(Frame$Id).page.canvas configure -cursor [expr {$cf=="watch"?"left_ptr":$cf}]}
        update idletasks
    }
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::UpdateGraph>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Met à jour les graphiques
#
# Parametres :
#  <Ids>    : Identificateurs des objets à mettre à jour
#  <ROnly>  : 1 pour seulement mettre à jour le graph (code R), 0 pour mettre
#             à jour les données aussi.
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::UpdateGraph { Ids {ROnly 0} } {
    variable Data

    foreach id $Ids {
        if { [info exists Data(Type$id)] && ($Data(Lat$id)!=""&&$Data(Lon$id)!="" || [lindex $Data(DataSrc$id) 0]=="RData") } {
            #----- Get the namespace
            set ns ::RGraph::$Data(Type$id)

            #----- Select the device in R and make sure we have the right one
            if { [R exec -get "if($Data(Dev$id) %in% dev.list()) names(dev.set($Data(Dev$id)))"] == "TclRDevice" } {
                CursorWait $id {
                    #----- Update the data
                    if { !$ROnly } {
                        if { [set err [catch {${ns}::GetData $id ::RGraph::Data$id $Data(DataSrc$id)} msg]] } {
                            Log::Print WARNING "Could not update graph data : $msg"
                        }
                        $Data(Canvas$id) itemconfigure $id -blank $err
                        set Data(DataValid$id) [expr !$err]
                        DataTableUpdate $id
                    }

                    #----- Run the plotting commands
                    if { $Data(DataValid$id) } {
                        R exec [RCode $id]
                    }
                }
            }
        }
    }
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::GetSelectedFields>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Retourne les champs sélectionnés correspondants aux critères donnés.
#
# Parametres :
#  <args>   : Les critères de sélection de la forme :
#               -champ {liste des valeurs voulues} -autrechamp {autre liste} ...
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::GetSelectedFields { args } {
    #----- Compile the search criterias
    set filter {}
    set vect {}
    foreach {key val} $args {
        switch -exact -- $key {
            -var    {lappend filter [list 0 $val]}
            -typvar {lappend filter [list 1 $val]}
            -lvl1   {lappend filter [list 2 $val]}
            -unit1  {lappend filter [list 3 $val]}
            -lvl2   {lappend filter [list 4 $val]}
            -unit2  {lappend filter [list 5 $val]}
            -lvl3   {lappend filter [list 6 $val]}
            -unit3  {lappend filter [list 7 $val]}
            -etiket {lappend filter [list 8 $val]}
            -datev  {lappend filter [list 9 $val]}
            -ip1    {lappend filter [list 12 $val]}
            -ip2    {lappend filter [list 13 $val]}
            -ip3    {lappend filter [list 14 $val]}
            -type   {lappend filter [list 15 $val]}
            -vect   {set vect [expr {$val ? {UU VV UUW VVW} : {}}]} ;#TODO : use the list returned by [join [fstdfield vector]] for vect
        }
    }

    #----- Loop on all open file boxes
    set res {}
    foreach box [FieldBox::Get] {
        #----- Get the selection for that box
        foreach fld [FieldBox::GetSelected $box] {
            #----- Apply the filter
            if { [llength $filter] } {
                set ok 1
                foreach {idx lst} {
                    if { [lindex $fld $idx] ni $lst } {
                        set ok 0
                        break
                    }
                }
                if { !$ok } {
                    continue
                }
            }
            if { [llength $vect] && [lindex $fld 0] ni $vect } {
                continue
            }
            lappend fld $box
            lappend res $fld
        }
    }

    return $res
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::DataExport>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Exporte les données d'un dataset R en un fichier R.
#
# Parametres :
#  <Id>     : Identificateur du graph
#  <File>   : Fichier dans lequel exporter les données (vide=demander à l'usager)
#  <W>      : Widget ayant commandé l'export
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::DataExport { Id {File ""} {W .} } {
    #----- Ask for the path and filename
    if { $File == "" } {
        set File [FileBox::Create $W "" Save [list {R Data File {*.RData *}} {Comma Separated Values {*.csv}}]]
    }
    if { $File != "" } {
        switch -exact [file extension $File] {
            ".csv" {
                set fd [open $File w]
                lassign [R rdf2tcllst $Id] title rows
                puts $fd [join $title ,]
                foreach row $rows {
                    puts $fd [join $row ,]
                }
                close $fd
            }
            default {
                set File [string map {' \\'} $File]
                set fn [file rootname [file tail $File]]

                #----- Save the data into R format
                R exec "
                    rgraphenv <- new.env()
                    rgraphenv\$'$fn' <- $Id
                    save(list='$fn',file='$File',compress=TRUE,envir=rgraphenv)
                    rm(rgraphenv)
                "
            }
        }
    }
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::DataLoad>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Importe les données d'un fichier R en dataset R
#
# Parametres :
#  <Id>     : Identificateur du graph et nom du dataset résultant dans R
#  <File>   : Fichier contenant les données
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::DataLoad { Id File } {
    if { $File!="" && [file exists $File] } {
        R exec "
            rgraphenv <- new.env()
            load(file='$File',envir=rgraphenv)
            $Id <- rgraphenv\[\[ls(envir=rgraphenv)\[1\]\]\]
            rm(rgraphenv)
        "
    } else {
        return -code error "Invalid file to load data from : \"$File\""
    }
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::DataTableUpdate>
# Creation : Février 2019 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Met à jour les données du tableau RData à partir du dataframe R
#
# Parametres :
#  <Id>     : Identificateur du graph
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::DataTableUpdate { Id } {
    variable Data

    #----- Erase the old data
    array unset ::RGraph::RData$Id

    if { $Data(DataValid$Id) && [info exists Data(RData$Id)] && [winfo exists $Data(RData$Id)] } {
        set tbl $Data(RData$Id)

        set dat [R r2tcl $Id]
        set names [lsearch -regexp -all -not -inline [dict keys $dat] {^attr\.}]

        #----- Needs to be in normal state for the data update
        $tbl configure -state normal

        #----- Adjust the number of rows and columns
        set nrow [llength [dict get $dat [lindex $names 0]]]
        set ncol [llength $names]
        $tbl configure -rows [expr $nrow+1] -cols [expr $ncol+1]

        #----- Set the row and column titles
        $tbl set row 0,1 $names
        $tbl set col 1,0 [R exec -get "1:$nrow"]

        #----- Set the data
        set col 0
        foreach name $names {
            $tbl set col 1,[incr col] [dict get $dat $name]
        }

        #----- Set back to disable to avoid editing
        $tbl configure -state disable
    }
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Create>
# Creation : Janvier 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback pour la création d'un RDevice (canvas item) dans un canvas
#
# Parametres :
#  <Frame>  : Identificateur de Page
#  <X0>     : Position en X du coin supérieur gauche
#  <Y0>     : Position en Y du coin supérieur gauche
#  <Width>  : Largeur de l'item à créer
#  <Height> : Hauteur de l'item à créer
#  <Active> : 1 si active, 0 sinon
#  <Full>   : 1 si prend l'ensemble du canvas (full screen), 0 sinon
#  <Id>     : Identificateur du graph (""=objet courant, NEW=nouvel Id)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------
proc RGraph::Create { Frame X0 Y0 Width Height Active Full {Id ""} } {
    variable Data

    if { $Id == "" } {
        set Id $Data(Id)
    } elseif { $Id == "NEW" } {
        set Id [NewId]
    }

    #----- Create the RDevice
    set c $Frame.page.canvas
    $c create rdevice $X0 $Y0 [expr $X0+$Width] [expr $Y0+$Height] -tags "PAGE$Id RGraph$Id $Id" -alias 1

    #----- Save the link between the frame and the id and the device
    set Data(Graph$Id)      $Frame
    set Data(Canvas$Id)     $c
    set Data(Full$Id)       $Full
    set Data(Active$Id)     $Active
    set Data(X$Id)          $X0
    set Data(Y$Id)          $Y0
    set Data(Width$Id)      $Width
    set Data(Height$Id)     $Height
    set Data(Dev$Id)        [R exec -get {dev.cur()}]
    set Data(DataValid$Id)  0
    set Data(DataSrc$Id)    ""
    set Data(Lat$Id)        ""
    set Data(Lon$Id)        ""
    set Data(SrcInfo$Id)    ""

    lappend Data(Graph$Frame) $Id

    #----- Add the interactive controls
    if { $Active } {
        Page::ActiveWrapper RGraph $Frame $Id $X0 $Y0 [expr $X0+$Width] [expr $Y0+$Height]
    } elseif { $Full } {
        Page::ActiveFull RGraph $Frame $Id $Full
    }
    Page::Register $Frame RGraph $Id

    #----- Make sure the graph's destroy proc is called when the user destroys the frame (when we are in a new page/window)
    bind $Frame <Destroy> [list ::RGraph::Destroy $Frame $Id]

    #----- Make the bindings for the graph
    $c bind $Id <Leave>             {set Page::Data(Coord) ""; set Page::Data(Value) ""}
    $c bind $Id <ButtonPress-1>     [list RGraph::OnDragInit $Id %W %x %y]
    $c bind $Id <ButtonRelease-1>   [list RGraph::OnDragEnd $Id %W %x %y]
    $c bind $Id <Motion>            [list RGraph::OnMotion $Id %W %x %y]
    $c bind $Id <ButtonPress-2>     [list RGraph::OnBoxZoomInit $Id %W %x %y]
    $c bind $Id <B2-Motion>         [list RGraph::OnBoxZoomDrag $Id %W %x %y]
    $c bind $Id <ButtonRelease-2>   [list RGraph::OnBoxZoom $Id %W %x %y]
    $c bind $Id <ButtonRelease-3>   [list RGraph::OnZoomReset $Id %W]
    $c bind $Id <Button-4>          [list RGraph::OnMouseWheel $Id %W %x %y 1]
    $c bind $Id <Button-5>          [list RGraph::OnMouseWheel $Id %W %x %y -1]

    #----- Reset the source info value that comes from the locator tool if we change the values manually
    trace add variable RGraph::Data(Lat$Id) write "set RGraph::Data(SrcInfo$Id) {} ;#ignore args#"
    trace add variable RGraph::Data(Lon$Id) write "set RGraph::Data(SrcInfo$Id) {} ;#ignore args#"

    #----- Update items when we change the SrcInfo
    trace add variable RGraph::Data(SrcInfo$Id) write "RGraph::UpdateItems {} $Id ;#ignore args#"

    return $Id
}

proc RGraph::CreateBatch { Lat Lon Type DataSrc args } {
    variable Data

    Init

    #----- Create a new RGraph
    set id [NewId 0]
    if { ![SPI::ObjectAdd RGraph "" 2] } {
        Close $id
        return ""
    }
    SetType $id $Type

    #----- Set the params
    set Data(Lat$id)        $Lat
    set Data(Lon$id)        $Lon
    set Data(DataSrc$id)    $DataSrc

    #----- Apply the graph's specific attributes (if any)
    upvar ::RGraph::Data$id attr
    foreach {key val} $args {
        set attr($key) $val
    }

    #----- Update the graph
    if { [catch {UpdateGraph $id}] || !$Data(DataValid$id) } {
        return ""
    }

    return $id
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Resize>
# Creation : Janvier 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback de resize lors d'un déplacement/scale
#
# Parametres :
#  <Frame>  : Identificateur de Page
#  <Id>     : Identificateur du graph
#  <X0>     : Position en X du coin supérieur gauche (-999 si inconnue)
#  <Y0>     : Position en Y du coin supérieur gauche (-999 si inconnue)
#  <X1>     : Position en X du coin inférieur droit
#  <Y1>     : Position en Y du coin inférieur droit
#  <Limit>  : 0 si resize final, 1 si en cours de resize (en cours de dragging)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------
proc RGraph::Resize { Frame Id X0 Y0 X1 Y1 Limit } {
    variable Data

    set c $Data(Canvas$Id)

    if { $X0 == -999 } {
        set X0 $Data(X$Id)
        set Y0 $Data(Y$Id)
    }

    #----- Resize the RDevice
    $c itemconfigure $Id -blank $Limit
    $c coords $Id $X0 $Y0 $X1 $Y1

    #----- Update the values
    set Data(X$Id)          $X0
    set Data(Y$Id)          $Y0
    set Data(Width$Id)      [expr $X1-$X0]
    set Data(Height$Id)     [expr $Y1-$Y0]
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::XYCanvas2RDev>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Converti une coordonnée relative au canvas à une coordonnée rdevice
#
# Parametres :
#  <Id>     : Identificateur du rdevice
#  <Canvas> : Canvas widget
#  <X>      : Position en X par rapport au canvas
#  <Y>      : Position en Y par rapport au canvas
#
# Retour: Position en X et Y dans le système du rdevice
#
# Remarques :
#
#----------------------------------------------------------------------------
proc RGraph::XYCanvas2RDev { Id Canvas X Y {Raw 1} } {
    lassign [$Canvas coords $Id] x0 y0 x1 y1

    #----- In case we scrolled
    if { $Raw } {
        set X [$Canvas canvasx $X]
        set Y [$Canvas canvasy $Y]
    }

    #----- Get the position relative to the item instead of the canvas
    #----- Note that rdevices need the position relative to the bottom left corner
    set X [expr {int(max(min($X,$x1),$x0)-$x0)}]
    set Y [expr {int($y1-max(min($Y,$y1),$y0))}]

    return [list $X $Y]
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::OnMotion>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors d'un déplacement à l'intérieur du canvas
#
# Parametres :
#  <Id>     : Identificateur du graph
#  <Canvas> : Canvas widget
#  <X>      : Position en X par rapport au canvas
#  <Y>      : Position en Y par rapport au canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------
proc RGraph::OnMotion { Id Canvas X Y } {
    variable Data

    set Page::Data(Coord) ""
    set Page::Data(Value) ""

    if { $Data(DataValid$Id) } {
        #----- If we are in drag mode (note: this is different from box mode)
        if { [info exists Data(Drag0)] } {
            OnDrag $Id $Canvas $X $Y
        } elseif { [ProcExists ::RGraph::$Data(Type$Id)::OnMotion] } {
            ::RGraph::$Data(Type$Id)::OnMotion $Id ::RGraph::Data$Id {*}[XYCanvas2RDev $Id $Canvas $X $Y]
        }
    }
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::OnMouseWheel>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors de l'activation de la molette de la souris
#
# Parametres :
#  <Id>     : Identificateur du graph
#  <Canvas> : Canvas widget
#  <X>      : Position en X par rapport au canvas
#  <Y>      : Position en Y par rapport au canvas
#  <D>      : Incrément de la molette (>0 vers le haut, <0 vers le bas)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------
proc RGraph::OnMouseWheel { Id Canvas X Y D } {
    variable Data

    if { $Data(DataValid$Id) && [ProcExists ::RGraph::$Data(Type$Id)::OnMouseWheel] } {
        #----- Make the callback
        CursorWait $Id {
            ::RGraph::$Data(Type$Id)::OnMouseWheel $Id ::RGraph::Data$Id {*}[XYCanvas2RDev $Id $Canvas $X $Y] $D
        }
    }
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::OnBoxZoom>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors d'un zoom par région
#
# Parametres :
#  <Id>     : Identificateur du graph
#  <Canvas> : Canvas widget
#  <X>      : Position en X par rapport au canvas
#  <Y>      : Position en Y par rapport au canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------
proc RGraph::OnBoxZoomInit { Id Canvas X Y } {
    variable Data

    $Canvas delete ZOOMBOX

    if { $Data(DataValid$Id) && [ProcExists ::RGraph::$Data(Type$Id)::OnBoxZoom] } {
        set Data(ZoomX0) [$Canvas canvasx $X]
        set Data(ZoomY0) [$Canvas canvasy $Y]

        $Canvas create rectangle $Data(ZoomX0) $Data(ZoomY0) $Data(ZoomX0) $Data(ZoomY0) -fill {} -outline red -width 2 -tags ZOOMBOX
    }
}
proc RGraph::OnBoxZoomDrag { Id Canvas X Y } {
    variable Data

    if { $Data(DataValid$Id) && [ProcExists ::RGraph::$Data(Type$Id)::OnBoxZoom] } {
        $Canvas coords ZOOMBOX $Data(ZoomX0) $Data(ZoomY0) [$Canvas canvasx $X] [$Canvas canvasy $Y]
    }
}
proc RGraph::OnBoxZoom { Id Canvas X Y } {
    variable Data

    $Canvas delete ZOOMBOX

    if { $Data(DataValid$Id) && [ProcExists ::RGraph::$Data(Type$Id)::OnBoxZoom] } {
        #----- Make the callback
        CursorWait $Id {
            ::RGraph::$Data(Type$Id)::OnBoxZoom $Id ::RGraph::Data$Id {*}[XYCanvas2RDev $Id $Canvas $Data(ZoomX0) $Data(ZoomY0) 0] {*}[XYCanvas2RDev $Id $Canvas $X $Y]
        }
    }
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::OnZoomReset>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors d'un zoom reset
#
# Parametres :
#  <Id>     : Identificateur du graph
#  <Canvas> : Canvas widget
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------
proc RGraph::OnZoomReset { Id Canvas } {
    variable Data

    $Canvas delete ZOOMBOX
    OnDragEnd $Id $Canvas -1 -1

    if { $Data(DataValid$Id) && [ProcExists ::RGraph::$Data(Type$Id)::OnZoomReset] } {
        #----- Make the callback
        CursorWait $Id {
            ::RGraph::$Data(Type$Id)::OnZoomReset $Id ::RGraph::Data$Id
        }
    }
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::OnDrag*>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors d'un drag
#
# Parametres :
#  <Id>     : Identificateur du graph
#  <Canvas> : Canvas widget
#  <X>      : Position en X par rapport au canvas
#  <Y>      : Position en Y par rapport au canvas
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------
proc RGraph::OnDragInit { Id Canvas X Y } {
    variable Data

    if { $Data(DataValid$Id) && [ProcExists ::RGraph::$Data(Type$Id)::OnDrag] } {
        catch {$Data(Canvas$Id) configure -cursor fleur}
        update idletasks
        set Data(Drag0) [XYCanvas2RDev $Id $Canvas $X $Y]
        set Data(DragL) $Data(Drag0)
    } else {
        unset -nocomplain Data(Drag0) Data(DragL)
    }
}
proc RGraph::OnDrag { Id Canvas X Y } {
    variable Data

    #----- No need to check if the function exists, it has to if this is called
    CursorWait $Id {
        set pos [XYCanvas2RDev $Id $Canvas $X $Y]
        ::RGraph::$Data(Type$Id)::OnDrag $Id ::RGraph::Data$Id {*}$Data(Drag0) {*}$pos {*}$Data(DragL)
        set Data(DragL) $pos
    }
}
proc RGraph::OnDragEnd { Id Canvas X Y } {
    variable Data

    catch {$Data(Canvas$Id) configure -cursor left_ptr}
    update idletasks
    unset -nocomplain Data(Drag0) Data(DragL)
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::Destroy>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Callback lorsque le rdevice (canvas item) est activé
#
# Parametres :
#  <Frame>  : Identificateur de Page
#  <Id>     : Identificateur du graph
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::Activate { Frame Id } {
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::Destroy>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Libère les ressources associées à l'id donnée.
#            Appelé lorsque le rdevice (canvas item) est détruit.
#
# Parametres :
#  <Frame>  : Identificateur de Page
#  <Id>     : Identificateur du graph
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::Destroy { Frame Id } {
    #----- Destroy the associated param window
    Close $Id
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::Close>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE -
#
# But      : Ferme l'interface de l'outil et libère le rdevice associé.
#
# Parametres :
#  <Id>     : Identificateur du graph
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------
proc RGraph::Close { {Id ""} } {
    variable Data

    #----- Wipe everything
    if { $Id == "" } {
        foreach id $Data(Ids) {
            Close $id
        }

        if { !$SPI::Param(Window) } { SPI::Quit }
    } else {
        #----- Si le mode etait celui de l'outils, revert to SPI
        if { $Page::Data(ToolMode)=="RGraph" && $Data(Id)==$Id } {
            SPI::ToolMode SPI Zoom
        }

        #----- Call type's cleanup proc
        if { [info exists Data(Type$Id)] } {
            Call ::RGraph::$Data(Type$Id)::CleanUp $Id ::RGraph::Data$Id
        }

        #----- Remove the Locator traces
        Locator $Id 0

        #----- Destroy the param window
        if { [info exists Data(Param$Id)] } {
            destroy $Data(Param$Id)
        }

        #----- Destroy the associated graph
        if { [info exists Data(Graph$Id)] } {
            set frm $Data(Graph$Id)

            if { [info exists Data(Canvas$Id)] && [winfo exists [set c $Data(Canvas$Id)]] } {
                #----- Delete the canvas item and the active buttons
                Page::ActiveUnWrapper RGraph $frm $Id
                $c delete $Id ZOOMBOX

                #----- Reset the binding
                bind $frm <Destroy> ""
            }

            #----- Unregister from the frame
            Page::UnRegister $frm RGraph $Id

            #----- Cleanup graph-specific data
            if { [llength $Data(Graph$frm)] > 1 } {
                set Data(Graph$frm) [lsearch -inline -all -not -exact $Data(Graph$frm) $Id]
            } else {
                array unset Data Graph$frm
            }
        }

        #----- Free the fonts
        catch {font delete $Data(MarkerFont$Id)}

        #----- Remove values for that ID
        array unset Data *$Id

        set Data(Ids) [lsearch -all -inline -not -exact $Data(Ids) $Id]

        #----- Mark tool as inactive if this is the last Id
        if { ![llength $Data(Ids)] } {
            set Data(Active)    0
            set Data(Id)        ""
        } elseif { $Data(Id) == $Id } {
            set Data(Id) ""
        }

        #----- Free specific type's params
        unset -nocomplain ::RGraph::Data$Id ::RGraph::RData$Id
    }
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Draw...>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Fonctions de manipulation sur la projection
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <VP>      : Identificateur du Viewport
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------
proc RGraph::DrawInit { Frame VP } {
    variable Data

    set id $Data(Id)

    set Data(Frame$id)  $Frame
    set Data(VP$id)     $VP
    set Data(Lat$id)    $Viewport::Map(LatCursor)
    set Data(Lon$id)    $Viewport::Map(LonCursor)

    UpdateItems $Frame $id
    UpdateGraph $id
}

proc RGraph::Draw { Frame VP } {
    variable Data

    set id $Data(Id)

    set Data(Lat$id) $Viewport::Map(LatCursor)
    set Data(Lon$id) $Viewport::Map(LonCursor)

    UpdateItems $Frame $id
    UpdateGraph $id
}

proc RGraph::DrawDone { Frame VP } {
    variable Data

    set id  $Data(Id)
    set lat $Viewport::Map(LatCursor)
    set lon $Viewport::Map(LonCursor)
    
    if { $Data(Lat$id)!=$lat || $Data(Lon$id)!=$lon } {
        set Data(Lat$id) $lat
        set Data(Lon$id) $lon

        UpdateItems $Frame $id
        UpdateGraph $id
    }
}

proc RGraph::MoveInit { Frame VP } {
}

proc RGraph::Move { Frame VP } {
}

proc RGraph::MoveDone { Frame VP } {
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::Locator>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Active ou désactive l'outil de localisation et update la localisation
#            de la source en fonction de la localisation choisie.
#
# Parametres :
#  <Id>     : Identificateur du graph
#  <Active> : Si on active la capture de la localisation ou non.
#
# Remarques :
#    - Cette fonctions est appele par SPI au besoin.
#
#-------------------------------------------------------------------------------
proc RGraph::Locator { Id {Active -1} } {
    variable Data

    set cmdloc "
        if { \[llength \[lindex \$SPI::Src(Info) 0\]\] == 1 } {
            set RGraph::Data(Lat$Id)        \$SPI::Src(Lat)
            set RGraph::Data(Lon$Id)        \$SPI::Src(Lon)
            set RGraph::Data(SrcInfo$Id)    \$SPI::Src(Info)
            RGraph::UpdateGraph $Id
        }
        #rest of args ignored#"

    set cmdact "
        if { !\$Locator::Data(Active) } {
            set RGraph::Data(Locator$Id) 0
            RGraph::Locator $Id
        }
        #rest of args ignored#"

    if { $Active != -1 } {
        set Data(Locator$Id) $Active
    }

    if { $Data(Locator$Id) } {
        if { ![info exists Data(Frame$Id)] } {
            set Data(VP$Id)     $Page::Data(VP) 
            set Data(Frame$Id)  $Viewport::Data(Frame$Data(VP$Id))
        }
        Locator::Window
        trace add variable Locator::Data(Active) write $cmdact
        trace add variable SPI::Src(Info) write $cmdloc
    } else {
        trace remove variable Locator::Data(Active) write $cmdact
        trace remove variable SPI::Src(Info) write $cmdloc
    }
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::Update>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE
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
proc RGraph::Update { Frame } {
    variable Data

    #----- Check if we have an RGraph in this frame
    if { [info exists Data(Graph$Frame)] } {
        #----- Update the graphs
        RGraph::UpdateGraph $Data(Graph$Frame)
    }
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::UpdateItems>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE
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
proc RGraph::UpdateItems { Frame {Id ""} } {
    global   GDefs
    variable Data

    if { $Id == "" } {
        foreach id $Data(Ids) {
            UpdateItems $Frame $id
        }
    } else {
        #----- Make sure we update the items based on the right frame (not the graph frame, for instance)
        if { [info exists Data(Frame$Id)] } {
            if { $Frame == "" } {
                set Frame $Data(Frame$Id)
            } elseif { $Frame != $Data(Frame$Id) } {
                return
            }
        } else {
            return
        }
        set c $Frame.page.canvas

        #----- Update the marker
        catch {
            $c delete RGraphMarker$Id

            set lat $Data(Lat$Id)
            set lon $Data(Lon$Id)

            if { [string is double -strict $lat] && [string is double -strict $lon] } {
                lassign [$Data(VP$Id) -project $lat $lon 0] x y z

                if { $x!="" && $z>0.0 } {
                    if { [llength $Data(SrcInfo$Id)] } {
                        #----- Location from Locator tool
                        lassign $Data(SrcInfo$Id) stype sid sname sarea
                        set text $sname
                        if { $sid!="" && $sid!="-" } {
                            append text " ($sid)"
                        }
                        if { $sarea!="" && $sarea!="-" && ![string is digit $sarea] } {
                            append text "\n$sarea"
                        }
                        append text [format "\n%.4f %.4f\n%s" $lat $lon $Id]
                    } elseif { [info exists Data(Type$Id)] } {
                        set text [format "%s\n%.4f %.4f\n%s" [lindex [set ::RGraph::$Data(Type$Id)::Param(Title)] $GDefs(Lang)] $lat $lon $Id]
                    } else {
                        set text [format "%.4f %.4f\n%s" $lat $lon $Id]
                    }
                    $c create text [expr $x+6] $y -text $text -fill $Data(MarkerCol$Id) -font $Data(MarkerFont$Id) \
                        -tags "PAGE$Data(VP$Id) PAGE$Id RGraphMarker$Id RGraph$Id" -anchor w
                    $c create oval [expr $x-2] [expr $y-2] [expr $x+2] [expr $y+2] -fill $Data(MarkerCol$Id) \
                        -tags "PAGE$Data(VP$Id) PAGE$Id RGraphMarker$Id RGraph$Id" -outline $Data(MarkerCol$Id)
                }
            }
        }
    }
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::PageActivate>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE
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
proc RGraph::PageActivate { Frame } {
}

#-------------------------------------------------------------------------------
# Nom      : <RGraph::AsProject>
# Creation : Décembre 2018 - E. Legault-Ouellet - CMC/CMOE
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
proc RGraph::AsProject { File } {
    variable Data
    variable Param

    #----- Not supported for the moment
    return

    if { [winfo exists .rgraph] } {
        puts $File "#----- Tool: RGraph\n"
        puts $File "set RGraph::Param(Dock)   $Param(Dock)"
        puts $File "set RGraph::Param(Geom)   [winfo geometry .rgraph]"
        puts $File "RGraph::Window"
        puts $File "\n"
    }
}
