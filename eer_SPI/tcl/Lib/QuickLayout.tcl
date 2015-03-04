#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2121 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : SPI
# Fichier  : QuickLayout.tcl
# Creation : Mars 2015 - E. Legault-Ouellet - CMC/CMOE
#
# Description:
#    Outil pour créer un layout rapidement
#
#===============================================================================

package provide QuickLayout 1.0

catch { SPI::Splash "Loading QuickLayout tool" }

namespace eval QuickLayout {
    #----- Parameters

    variable Param

    set Param(NbVP)         1
    set Param(Height)       340
    set Param(Width)        550
    set Param(Dimension)    Page

    set Param(BdX)          5
    set Param(BdY)          5
    set Param(Gap)          5

    set Param(Expand)       0
    set Param(Method)       OptimizePerimeter

    #----- Labels

    variable Lbl

    set Lbl(Title)          {"Outil de mise en page rapide" "Quick Layout Tool"}
    set Lbl(Dims)           {"Dimensions" "Dimensions"}
    set Lbl(Options)        {"Options" "Options"}

    set Lbl(NbVP)           {"Nombre de vues" "Number of viewports"}
    set Lbl(Width)          {"Largeur       " "Width              "}
    set Lbl(Height)         {"Hauteur       " "Height             "}
    set Lbl(Dimension)      {"Dimensions de " "Dimensions of      "}
    set Lbl(Dimensions)     { {"Page" "Vue"} {"Page" "Viewport"} }

    set Lbl(Expand)         {"Élargir si possible" "Expand if possible"}
    set Lbl(Gap)            {"Espace entre les vues  " "Space between viewports"}
    set Lbl(BdX)            {"Bordure en X           " "Border X               "}
    set Lbl(BdY)            {"Bordure en Y           " "Border Y               "}
    set Lbl(Method)         {"Méthode de mise en page" "Layout method          "}
    set Lbl(Methods)        { {"Prioriser les colonnes" "Prioriser les rangées" "Plus petit périmètre" "Manuel"}
                              {"Prefer columns" "Prefer rows" "Smallest perimeter" "Manual"} }
    set Lbl(NbRows)         {"Nb rangées " "Nb Rows   "}
    set Lbl(NbCols)         {"Nb colonnes" "Nb Columns"}

    set Lbl(Close)          {"Fermer" "Close"}
    set Lbl(Apply)          {"Appliquer" "Apply"}

    #----- Internal data

    variable Data

    set Data(NbRows)        1
    set Data(NbCols)        1

    set Data(Methods)       {PreferColumns PreferRows OptimizePerimeter Manual}
    set Data(Dimensions)    {Page VP}
}

#-------------------------------------------------------------------------------
# Nom      : <QuickLayout::IntInput>
# Creation : Mars 2015 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Crée un label et une entrée pour un int
#
# Parametres :
#  <Widget>  : Nom du frame qui sera créé
#  <Text>    : Texte bilingue du label
#  <VarName> : Nom de la variable associé au entry
#  <Inc>     : Si on ajoute des boutons d'incréments ou non (Default : Non)
#
# Retour    :
#   Le path du widget (paramètre Widget)
#
# Remarque :
#
#-------------------------------------------------------------------------------
proc QuickLayout::IntInput { Widget Text VarName {Inc False}} {
    global GDefs
    variable Param

    frame $Widget
        label $Widget.l -text [lindex $Text $GDefs(Lang)] -anchor w
        entry $Widget.e -textvariable $VarName -validate key -vcmd "string is integer %P" -bg $GDefs(ColorLight)
        pack $Widget.l -side left -padx {0 2}
        pack $Widget.e -side left -fill x -expand True -padx {2 0}

        #----- Inc up and down buttons
        if { $Inc } {
            frame $Widget.inc -relief sunken -bd 1
                button $Widget.inc.up -bitmap @$GDefs(Dir)/share/bitmap/up.xbm -bd 1 -command "incr $VarName"
                button $Widget.inc.dwn -bitmap @$GDefs(Dir)/share/bitmap/down.xbm -bd 1 -command "if { \[set $VarName\]>0 } { incr $VarName -1 }"
                pack $Widget.inc.up $Widget.inc.dwn -side top -fill y -expand True
            pack $Widget.inc -side left
        }

    bind $Widget.e <Return> ::QuickLayout::LayoutGrid
    bind $Widget.e <KP_Enter> ::QuickLayout::LayoutGrid

    return $Widget
}

#-------------------------------------------------------------------------------
# Nom      : <QuickLayout::ComboInput>
# Creation : Mars 2015 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Crée un label et un combo box
#
# Parametres :
#  <Widget>  : Nom du frame qui sera créé
#  <Key>     : Clé dans les variable Param et Data associée au combobox
#
# Retour    :
#   Le path du widget (paramètre Widget)
#
# Remarque :
#
#-------------------------------------------------------------------------------
proc QuickLayout::ComboInput { Widget Key } {
    global GDefs
    variable Data
    variable Lbl

    frame $Widget
        label $Widget.l -text [lindex $Lbl($Key) $GDefs(Lang)] -anchor w
        ComboBox::Create $Widget.e ::QuickLayout::Data(${Key}Txt) noedit unsorted nodouble [llength $Data(${Key}s)] [lindex $Lbl(${Key}s) $GDefs(Lang)] \
            25 [llength $Data(${Key}s)] [list ::QuickLayout::ComboSelect $Key]
        pack $Widget.l -side left -padx {0 2}
        pack $Widget.e -side left -padx {2 0} -fill x -expand True

    return $Widget
}

#-------------------------------------------------------------------------------
# Nom      : <QuickLayout::ComboSelect>
# Creation : Mars 2015 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Gère le changement des combobox
#
# Parametres :
#  <Key>     : Clé dans les variable Param et Data associée au combobox
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------
proc QuickLayout::ComboSelect { Key } {
    global GDefs
    variable Param
    variable Data
    variable Lbl

    #----- Get the selected method from the corresponding label

    if { [set idx [lsearch -exact [lindex $Lbl(${Key}s) $GDefs(Lang)] $Data(${Key}Txt)]] == -1 } {
        Log::Print ERROR "The selected $Key is not in the list : \"$Data(${Key}Txt)\""
        return
    }
    set Param($Key) [lindex $Data(${Key}s) $idx]

    #----- Method special case

    if { $Key=="Method" } {
        #----- If manual, pack the rest of the interface

        if { $Param($Key)=="Manual" } {
            eval $Data(ManualPack)
        } else {
            eval $Data(ManualForget)
        }
    }

    #----- Update the layout

    ::QuickLayout::LayoutGrid
}

#-------------------------------------------------------------------------------
# Nom      : <QuickLayout::Window>
# Creation : Mars 2015 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Crée la fenêtre pour appliquer un QuickLayout
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------
proc QuickLayout::Window { Frame } {
    global GDefs
    variable Param
    variable Data
    variable Lbl

    #----- Update some values

    if { $Param(Dimension) == "Page" } {
        set Param(Width)  [Page::CanvasWidth $Frame]
        set Param(Height) [Page::CanvasHeight $Frame]
    }

    set Data(MethodTxt)     [lindex [lindex $Lbl(Methods) $GDefs(Lang)] [lsearch -exact $Data(Methods) $Param(Method)]]
    set Data(DimensionTxt)  [lindex [lindex $Lbl(Dimensions) $GDefs(Lang)] [lsearch -exact $Data(Dimensions) $Param(Dimension)]]
    set Data(Frame)         $Frame

    #----- Create the window

    set w .qcklyt

    if { ![winfo exists $w] } {
        toplevel        $w -class Dialog
        #wm withdraw     $w
        wm title        $w [lindex $Lbl(Title) $GDefs(Lang)]
        #wm geometry     $w 375x400+[winfo rootx .]+[winfo rooty .]
        #wm protocol     $w WM_DELETE_WINDOW "wm withdraw $w"
        wm protocol     $w WM_DELETE_WINDOW "destroy $w"
        wm transient    $w .

        frame $w.frm
            #----- Dimensions

            labelframe [set lf $w.frm.dims] -text [lindex $Lbl(Dims) $GDefs(Lang)]
                pack [IntInput $lf.nb $Lbl(NbVP) ::QuickLayout::Param(NbVP) True] -side top -fill x -expand True -padx 2
                pack [IntInput $lf.width $Lbl(Width) ::QuickLayout::Param(Width)] -side top -fill x -expand True -padx 2
                pack [IntInput $lf.height $Lbl(Height) ::QuickLayout::Param(Height)] -side top -fill x -expand True -padx 2
                pack [ComboInput $lf.dim Dimension] -side top -fill x -expand True -padx 2
            pack $lf -side top -fill x -expand True -padx 5 -pady 5 -anchor n

            #----- Options

            labelframe [set lf $w.frm.opt] -text [lindex $Lbl(Options) $GDefs(Lang)]
                #----- Expand viewport
                checkbutton $lf.expand -text [lindex $Lbl(Expand) $GDefs(Lang)] -variable ::QuickLayout::Param(Expand) -command ::QuickLayout::LayoutGrid
                pack $lf.expand -side top -padx 2 -anchor w

                #----- Gap + Borders
                pack [IntInput $lf.gap $Lbl(Gap) ::QuickLayout::Param(Gap) True] -side top -fill x -expand True -padx 2
                pack [IntInput $lf.bdx $Lbl(BdX) ::QuickLayout::Param(BdX) True] -side top -fill x -expand True -padx 2
                pack [IntInput $lf.bdy $Lbl(BdY) ::QuickLayout::Param(BdY) True] -side top -fill x -expand True -padx 2

                #----- Method selection
                pack [ComboInput $lf.method Method] -side top -fill x -expand True -padx 2

                #----- Manual method entry widgets
                frame [set f $lf.manual]
                    pack [IntInput $f.cols $Lbl(NbCols) ::QuickLayout::Data(NbCols) True] -side top -fill x -expand True -padx 2
                    pack [IntInput $f.rows $Lbl(NbRows) ::QuickLayout::Data(NbRows) True] -side top -fill x -expand True -padx 2
                set Data(ManualPack) "pack $f -side top -fill x -expand True -after $lf.method"
                set Data(ManualForget) "pack forget $f"
            pack $lf -side top -fill x -expand True -padx 5 -pady 5 -anchor n
        pack $w.frm -side top -fill x -expand True -anchor n

        frame [set f $w.btns]
            button $f.cancel -text [lindex $Lbl(Close) $GDefs(Lang)] -command [list destroy $w]
            button $f.apply -text [lindex $Lbl(Apply) $GDefs(Lang)] -command ::QuickLayout::LayoutGrid
            pack $f.cancel $f.apply -side right -anchor e
        pack $w.btns -side top -fill x -expand True -padx 5 -pady 5 -anchor s
    }

    LayoutGrid
}

#-------------------------------------------------------------------------------
# Nom      : <QuickLayout::LayoutGrid>
# Creation : Mars 2015 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Crée la grille de viewports et positionne ces derniers sur la grille
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------
proc QuickLayout::LayoutGrid { } {
    variable Param
    variable Data

    #----- Clear the current layout

    Viewport::Destroy $Data(Frame)

    if { ![string is integer $Param(NbVP)] || $Param(NbVP)<1 } {
        Log::Print ERROR "Number of viewport is not valid : \"$Param(NbVP)\""
        return
    }

    #----- Calculate the new grid

    set N       $Param(NbVP)
    set ratio   [expr {double($Param(Width))/double($Param(Height))}]

    switch $Param(Method) {
        PreferColumns {
            set nbc [expr {int(ceil(sqrt($N)))}]
            set nbr [expr {int(ceil(double($N)/double($nbc)))}]
        }
        PreferRows {
            set nbr [expr {int(ceil(sqrt($N)))}]
            set nbc [expr {int(ceil(double($N)/double($nbr)))}]
        }
        OptimizePerimeter {
            set nr [expr {sqrt($N*$ratio)}]

            if { (ceil($N/floor($nr))*$ratio+floor($nr)) > (ceil($N/ceil($nr))*$ratio+ceil($nr)) } {
                set nbr [expr {int(ceil($nr))}]
                set nbc [expr {int(ceil($N/ceil($nr)))}]
            } else {
                set nbr [expr {int(floor($nr))}]
                set nbc [expr {int(ceil($N/floor($nr)))}]
            }
        }
        Manual {
            set nbr $Data(NbRows)
            set nbc $Data(NbCols)

            if { $nbr*$nbc < $N } {
                Log::Print ERROR "Not enough rows or columns for the number of viewport (${nbr}x$nbc=[expr {$nbr*$nbc}] < $N)"
                return
            }
        }
        default {
            Log::Print ERROR "Invalid method : \"$Param(Method)\""
            return
        }
    }

    set Data(NbRows) $nbr
    set Data(NbCols) $nbc

    #----- Update the dimensions

    switch $Param(Dimension) {
        Page {
            set w [expr {($Param(Width)-2*$Param(BdX)-($nbc-1)*$Param(Gap))/$nbc}]
            set h [expr {($Param(Height)-2*$Param(BdY)-($nbr-1)*$Param(Gap))/$nbr}]

            Page::Size $Data(Frame) $Param(Width) $Param(Height)
        }
        VP {
            set w $Param(Width)
            set h $Param(Height)

            Page::Size $Data(Frame) \
                [expr {$w*$nbc+2*$Param(BdX)+($nbc-1)*$Param(Gap)}] \
                [expr {$h*$nbr+2*$Param(BdY)+($nbr-1)*$Param(Gap)}]
        }
        default {
            Log::Print ERROR "Invalid dimension : \"$Param(Dimension)\""
            return
        }
    }

    set wt  [expr {$nbc*$w + ($nbc-1)*$Param(Gap)}]
    set ht  [expr {$nbr*$h + ($nbr-1)*$Param(Gap)}]

    #----- Create the viewports according to the grid definition

    #upvar ColorBar::Param cb
    #set cbparams [list $cb(Full) $cb(BG) $cb(Alpha) $cb(Split) $cb(Factor) $cb(Border) $cb(Width) $cb(Side)]

    set y $Param(BdY)
    set n 0

    for {set j 0} {$j<$nbr} {incr j} {
        set x $Param(BdX)

        for {set i 0} {$i<$nbc && $n<$Param(NbVP)} {incr i; incr n} {
            #----- Treat the special case where the grid will be incomplete
            if { $i==0 && $Param(NbVP)-$n<$nbc } {
                set nbr [expr {$Param(NbVP)-$j*$nbc}]

                if { $Param(Expand) } {
                    #----- Modify the width of the viewport to fill the row
                    set w [expr {$wt/$nbr}]
                } else {
                    #----- Center the viewports
                    incr x [expr {($wt-$nbr*$w-($nbr-1)*$Param(Gap))/2}]
                }
            }

            #----- Create the viewport and position the hypothetical colormap

            set Data(VP$n) [Viewport::Create $Data(Frame) $x $y $w $h 1 0]
            #set ColorBar::Data($Data(VP$n)0) [list [expr {$x+$w-$Param(CB_Width)}] $y $Param(CB_Width) $Param(VP_Height) [set Data(CB$n) [ColorBar::Tag $Data(VP$n) CB]:$Data(VP$n)0] {*}$cbparams]

            incr x [expr {$w+$Param(Gap)}]
        }

        incr y [expr {$h+$Param(Gap)}]
    }

    ::Page::Activate $Data(Frame) True
    ::Page::ModeSelect $::Page::Data(Mode) $Data(Frame)
}

#-------------------------------------------------------------------------------
# Nom      : <QuickLayout::LayoutClear>
# Creation : Janvier 2015 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Détruit les composantes reliées au layout
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------
proc QuickLayout::LayoutClear { Frame } {
    Viewport::Destroy $Frame
}

