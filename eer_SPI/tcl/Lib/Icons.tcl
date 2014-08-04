#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Librairie de definitions pour les domaines des projections
# Fichier   : Icons.tcl
# Creation  : Novembre 2003 - J.P. Gauthier - CMC/CMOE
#
# Description: Icon loader pour tout les autres packages et aplications
#
# Fonctions:
#
# Remarques :
#   Aucune
#
#===============================================================================

package provide Icons 1.0

catch { SPI::Splash "Loading Widget Package Icons 1.0" }

image create photo DOCK        -file $GDefs(Dir)/share/image/Icon/Dock.gif
image create photo DOCKDELETE  -file $GDefs(Dir)/share/image/Icon/DockDelete.gif
image create photo MODEL       -file $GDefs(Dir)/share/image/Icon/Thermometer.gif
image create photo RESET       -file $GDefs(Dir)/share/image/Icon/BlueCircle.gif
image create photo INTEROGATE  -file $GDefs(Dir)/share/image/Icon/Help3.gif
image create photo INFOLOG     -file $GDefs(Dir)/share/image/Icon/Inform.gif

image create photo FRAMESAVE   -file $GDefs(Dir)/share/image/Icon/FrameSave.gif
image create photo FRAMEPOS    -file $GDefs(Dir)/share/image/Icon/FramePos.gif
image create photo FRAMEADD    -file $GDefs(Dir)/share/image/Icon/FrameAdd.gif
image create photo FRAMEDEL    -file $GDefs(Dir)/share/image/Icon/FrameDelete.gif
image create photo FRAMEEXT    -file $GDefs(Dir)/share/image/Icon/FrameExt.gif
image create photo FRAME0      -file $GDefs(Dir)/share/image/Icon/Frame.gif
image create photo FRAME1      -file $GDefs(Dir)/share/image/Icon/Frame1.gif
image create photo FRAME2      -file $GDefs(Dir)/share/image/Icon/Frame2.gif
image create photo FRAME3      -file $GDefs(Dir)/share/image/Icon/Frame3.gif
image create photo FRAMER      -file $GDefs(Dir)/share/image/Icon/FrameR.gif
image create photo FRAMEL      -file $GDefs(Dir)/share/image/Icon/FrameL.gif
image create photo FRAMET      -file $GDefs(Dir)/share/image/Icon/FrameT.gif
image create photo FRAMEB      -file $GDefs(Dir)/share/image/Icon/FrameB.gif

image create photo CALC        -file $GDefs(Dir)/share/image/Icon/Calculator.gif
image create photo CALCSAVE    -file $GDefs(Dir)/share/image/Icon/CalculatorSave.gif
image create photo CALCDEL     -file $GDefs(Dir)/share/image/Icon/CalculatorDelete.gif
image create photo PLUS        -file $GDefs(Dir)/share/image/Icon/Plus.gif
image create photo MINUS       -file $GDefs(Dir)/share/image/Icon/Minus.gif
image create photo UP          -file $GDefs(Dir)/share/image/Icon/TrUp.gif
image create photo DOWN        -file $GDefs(Dir)/share/image/Icon/TrDown.gif
image create photo BOMB        -file $GDefs(Dir)/share/image/Icon/Bomb.gif
image create photo CAUTION     -file $GDefs(Dir)/share/image/Icon/Caution.gif
image create photo SUN         -file $GDefs(Dir)/share/image/Icon/Sun.gif
image create photo ROAD        -file $GDefs(Dir)/share/image/Icon/Road.gif
image create photo CLOCK       -file $GDefs(Dir)/share/image/Icon/Clock.gif
image create photo PIN         -file $GDefs(Dir)/share/image/Icon/Pin.gif
image create photo PINSAVE     -file $GDefs(Dir)/share/image/Icon/PinSave.gif
image create photo PINDEL      -file $GDefs(Dir)/share/image/Icon/PinDelete.gif
image create photo PINADD      -file $GDefs(Dir)/share/image/Icon/PinAdd.gif
image create photo PINNEW      -file $GDefs(Dir)/share/image/Icon/PinNew.gif
image create photo FLATTEN     -file $GDefs(Dir)/share/image/Icon/Flatten.gif

image create photo FLYAROUND   -file $GDefs(Dir)/share/image/Icon/FlyAround.gif
image create photo FLYCIRCLE   -file $GDefs(Dir)/share/image/Icon/FlyCircle.gif
image create photo FLYTO       -file $GDefs(Dir)/share/image/Icon/FlyTo.gif
image create photo FLYTHROUGH  -file $GDefs(Dir)/share/image/Icon/FlyThrough.gif

image create photo BLANK       -file $GDefs(Dir)/share/image/Icon/Blank.gif
image create photo LINK        -file $GDefs(Dir)/share/image/Icon/Chain.gif
image create photo LINKSET     -file $GDefs(Dir)/share/image/Icon/ChainSet.gif
image create photo UNLINK      -file $GDefs(Dir)/share/image/Icon/UnChain.gif
image create photo EYE         -file $GDefs(Dir)/share/image/Icon/Eyeball.gif
image create photo WORLD       -file $GDefs(Dir)/share/image/Icon/World.gif
image create photo WORLDRESET  -file $GDefs(Dir)/share/image/Icon/WorldReset.gif
image create photo NOWORLD     -file $GDefs(Dir)/share/image/Icon/NoWorld.gif
image create photo ARROW       -file $GDefs(Dir)/share/image/Icon/Select.gif
image create photo ARROWLINE   -file $GDefs(Dir)/share/image/Icon/SelectLine.gif
image create photo ARROWSQUARE -file $GDefs(Dir)/share/image/Icon/SelectSquare.gif
image create photo ARROWGRID   -file $GDefs(Dir)/share/image/Icon/SelectGrid.gif
image create photo ARROWPOLY   -file $GDefs(Dir)/share/image/Icon/SelectPolygon.gif
image create photo ARROWDEL    -file $GDefs(Dir)/share/image/Icon/SelectDel.gif
image create photo ARROWADD    -file $GDefs(Dir)/share/image/Icon/SelectAdd.gif
image create photo DELETE      -file $GDefs(Dir)/share/image/Icon/Delete.gif
image create photo CHECK       -file $GDefs(Dir)/share/image/Icon/Check.gif
image create photo SCHECK      -file $GDefs(Dir)/share/image/Icon/SmallCheck.gif
image create photo OK          -file $GDefs(Dir)/share/image/Icon/Ok.gif
image create photo PENCIL      -file $GDefs(Dir)/share/image/Icon/Pencil.gif
image create photo CIRCLE      -file $GDefs(Dir)/share/image/Icon/Circle.gif
image create photo OVAL        -file $GDefs(Dir)/share/image/Icon/Oval.gif
image create photo SQUARE      -file $GDefs(Dir)/share/image/Icon/Square.gif
image create photo LINE        -file $GDefs(Dir)/share/image/Icon/Line.gif
image create photo POLY        -file $GDefs(Dir)/share/image/Icon/Polygon.gif
image create photo BARB        -file $GDefs(Dir)/share/image/Icon/Barb.gif
image create photo DASH1       -file $GDefs(Dir)/share/image/Icon/Dash1.gif
image create photo DASH2       -file $GDefs(Dir)/share/image/Icon/Dash2.gif
image create photo VAL         -file $GDefs(Dir)/share/image/Icon/Gauge.gif
image create photo STREAM      -file $GDefs(Dir)/share/image/Icon/Stream.gif
image create photo RULER       -file $GDefs(Dir)/share/image/Icon/Ruler.gif
image create photo COMPASS     -file $GDefs(Dir)/share/image/Icon/Compass.gif
image create photo SCALE       -file $GDefs(Dir)/share/image/Icon/Scale.gif
image create photo BITMAP      -file $GDefs(Dir)/share/image/Icon/ImageBitmap.gif
image create photo IMAGE       -file $GDefs(Dir)/share/image/Icon/ImageColor.gif
image create photo TEXT        -file $GDefs(Dir)/share/image/Icon/Text.gif
image create photo ERROR       -file $GDefs(Dir)/share/image/Icon/Error.gif
image create photo TOOL        -file $GDefs(Dir)/share/image/Icon/Hammer.gif
image create photo COLORMAP    -file $GDefs(Dir)/share/image/Icon/Colormap.gif
image create photo BACK        -file $GDefs(Dir)/share/image/Icon/ToBack.gif
image create photo FRONT       -file $GDefs(Dir)/share/image/Icon/ToFront.gif
image create photo RANGE       -file $GDefs(Dir)/share/image/Icon/Ranges.gif
image create photo MAGLOCK     -file $GDefs(Dir)/share/image/Icon/MagLock.gif
image create photo TABLETO     -file $GDefs(Dir)/share/image/Icon/TableTo.gif
image create photo TARGET      -file $GDefs(Dir)/share/image/Icon/Target.gif

image create photo OPEN        -file $GDefs(Dir)/share/image/Icon/Open.gif
image create photo OPENDIR     -file $GDefs(Dir)/share/image/Icon/OpenDir.gif
image create photo OPENDOC     -file $GDefs(Dir)/share/image/Icon/OpenDoc.gif
image create photo PAGE        -file $GDefs(Dir)/share/image/Icon/DocumentSetup.gif
image create photo PAGESAVE    -file $GDefs(Dir)/share/image/Icon/PageSave.gif
image create photo PAGEDEL     -file $GDefs(Dir)/share/image/Icon/PageDelete.gif
image create photo PAGERUN     -file $GDefs(Dir)/share/image/Icon/PageRun.gif
image create photo DOCRUN      -file $GDefs(Dir)/share/image/Icon/DocumentRun.gif
image create photo DOCDEL      -file $GDefs(Dir)/share/image/Icon/DocumentDelete.gif
image create photo DOCSEL      -file $GDefs(Dir)/share/image/Icon/DocumentTo.gif
image create photo DOCSAVE     -file $GDefs(Dir)/share/image/Icon/DocumentSave.gif
image create photo DOCLOAD     -file $GDefs(Dir)/share/image/Icon/DocumentLoad.gif
image create photo DOCIN       -file $GDefs(Dir)/share/image/Icon/DocumentIn.gif
image create photo DOCOUT      -file $GDefs(Dir)/share/image/Icon/DocumentOut.gif
image create photo DOCNEW      -file $GDefs(Dir)/share/image/Icon/DocumentNew.gif
image create photo DOC         -file $GDefs(Dir)/share/image/Icon/Document.gif
image create photo DOCWRITE    -file $GDefs(Dir)/share/image/Icon/DocumentDraw.gif
image create photo FOLD        -file $GDefs(Dir)/share/image/Icon/Folder.gif
image create photo FOLDIN      -file $GDefs(Dir)/share/image/Icon/FolderIn.gif
image create photo FOLDOUT     -file $GDefs(Dir)/share/image/Icon/FolderOut.gif
image create photo FOLDUP      -file $GDefs(Dir)/share/image/Icon/FolderUp.gif
image create photo FOLDHID     -file $GDefs(Dir)/share/image/Icon/Magnify.gif
image create photo SHEET       -file $GDefs(Dir)/share/image/Icon/Sheet.gif
image create photo BOOK        -file $GDefs(Dir)/share/image/Icon/Book.gif
image create photo BOOKSAVE    -file $GDefs(Dir)/share/image/Icon/BookSave.gif
image create photo GRID        -file $GDefs(Dir)/share/image/Icon/Grid.gif
image create photo GRIDADD     -file $GDefs(Dir)/share/image/Icon/GridAdd.gif
image create photo GRIDREM     -file $GDefs(Dir)/share/image/Icon/GridRemove.gif
image create photo GRIDTO      -file $GDefs(Dir)/share/image/Icon/GridTo.gif
image create photo GRIDSAVE    -file $GDefs(Dir)/share/image/Icon/GridSave.gif
image create photo GRIDDEL     -file $GDefs(Dir)/share/image/Icon/GridDelete.gif

image create photo MOUSE       -file $GDefs(Dir)/share/image/Icon/MouseWheel.gif
image create photo MODEZOOM    -file $GDefs(Dir)/share/image/Icon/Magnify.gif
image create photo MODECAM     -file $GDefs(Dir)/share/image/Icon/Camera.gif
image create photo MODEFLY     -file $GDefs(Dir)/share/image/Icon/Airplane.gif
image create photo MODEMAG     -file $GDefs(Dir)/share/image/Icon/DocumentMag.gif
image create photo ROTATE      -file $GDefs(Dir)/share/image/Icon/RotCCDown.gif
image create photo BINOCULAR   -file $GDefs(Dir)/share/image/Icon/Binocular.gif
image create photo PARAMS      -file $GDefs(Dir)/share/image/Icon/Parameters.gif
image create photo GRAPH       -file $GDefs(Dir)/share/image/Icon/Graph.gif
image create photo GRAPHDATA   -file $GDefs(Dir)/share/image/Icon/SearchRow.gif
image create photo GRAPHZOOM   -file $GDefs(Dir)/share/image/Icon/GraphZoom.gif
image create photo GRAPHRESET  -file $GDefs(Dir)/share/image/Icon/GraphReset.gif
image create photo OPTIONS     -file $GDefs(Dir)/share/image/Icon/Options.gif
image create photo CAMDEL      -file $GDefs(Dir)/share/image/Icon/CameraDelete.gif
image create photo CAMSAVE     -file $GDefs(Dir)/share/image/Icon/CameraSave.gif
image create photo CAMPATH     -file $GDefs(Dir)/share/image/Icon/CameraPath.gif
image create photo ZOOMBACK    -file $GDefs(Dir)/share/image/Icon/Begin.gif
image create photo ZOOMCLR     -file $GDefs(Dir)/share/image/Icon/Left.gif
image create photo CAMDOWN     -file $GDefs(Dir)/share/image/Icon/Down.gif
image create photo PRINT       -file $GDefs(Dir)/share/image/Icon/Print.gif
image create photo SAVE        -file $GDefs(Dir)/share/image/Icon/Save.gif
image create photo BUBBLE      -file $GDefs(Dir)/share/image/Icon/HelpBubble.gif
image create photo BUBBLEGRAPH -file $GDefs(Dir)/share/image/Icon/GraphBubble.gif
image create photo INFO        -file $GDefs(Dir)/share/image/Icon/InfoBubble.gif
image create photo PALETTE     -file $GDefs(Dir)/share/image/Icon/Palette.gif
image create photo PALETTEDEL  -file $GDefs(Dir)/share/image/Icon/PaletteDel.gif
image create photo PALETTESAVE -file $GDefs(Dir)/share/image/Icon/PaletteSave.gif
image create photo COMPUTER    -file $GDefs(Dir)/share/image/Icon/Computer.gif
image create photo LOCATION    -file $GDefs(Dir)/share/image/Icon/Location.gif
image create photo FINGER      -file $GDefs(Dir)/share/image/Icon/FingerDown.gif
image create photo ENVELOPE    -file $GDefs(Dir)/share/image/Icon/Envelope.gif
image create photo ENVELOPE2   -file $GDefs(Dir)/share/image/Icon/NewEnvelope.gif
image create photo ENVELOPECA  -file $GDefs(Dir)/share/image/Icon/EnvelopeCA.gif
image create photo ADDRESS     -file $GDefs(Dir)/share/image/Icon/BCard.gif

image create photo VCRPLAYB    -file $GDefs(Dir)/share/image/Icon/VCRPlayBackward.gif
image create photo VCRPLAYF    -file $GDefs(Dir)/share/image/Icon/VCRPlayForward.gif
image create photo VCRFRAMEB   -file $GDefs(Dir)/share/image/Icon/VCRFrameBackward.gif
image create photo VCRFRAMEF   -file $GDefs(Dir)/share/image/Icon/VCRFrameForward.gif
image create photo VCRSTOP     -file $GDefs(Dir)/share/image/Icon/VCRStop.gif
image create photo VCRREWIND   -file $GDefs(Dir)/share/image/Icon/VCRRewind.gif
image create photo VCRFORWIND  -file $GDefs(Dir)/share/image/Icon/VCRForwind.gif
image create photo VCRCYCLE    -file $GDefs(Dir)/share/image/Icon/VCRCycle.gif
image create photo VCRSAVE     -file $GDefs(Dir)/share/image/Icon/VCRSave.gif
image create photo VCRWEB      -file $GDefs(Dir)/share/image/Icon/VCRWeb.gif
image create photo VCRLOCK     -file $GDefs(Dir)/share/image/Icon/Lock.gif
image create photo VCRUNLOCK   -file $GDefs(Dir)/share/image/Icon/UnLock.gif

image create photo ICO_VOLC    -file $GDefs(Dir)/share/image/Symbol/Icon/Type_VOLCANO.gif
image create photo ICO_NUCL    -file $GDefs(Dir)/share/image/Symbol/Icon/Type_NUCLEAR.gif
image create photo ICO_CTBT    -file $GDefs(Dir)/share/image/Symbol/Icon/Type_CTBT.gif
image create photo ICO_FIRE    -file $GDefs(Dir)/share/image/Symbol/Icon/Type_FIRE.gif
image create photo ICO_OTHE    -file $GDefs(Dir)/share/image/Symbol/Icon/Type_OTHER.gif
image create photo ICO_BIO     -file $GDefs(Dir)/share/image/Symbol/Icon/Type_BIO.gif
image create photo ICO_SPILL   -file $GDefs(Dir)/share/image/Symbol/Icon/Type_SPILL.gif
image create photo ICO_THERE   -file $GDefs(Dir)/share/image/Symbol/Icon/Type_THERE.gif
image create photo ICO_PLANE   -file $GDefs(Dir)/share/image/Symbol/Icon/Type_PLANE.gif

image create photo BAD_VOLC    -file $GDefs(Dir)/share/image/Symbol/Icon/Bad_VOLCANO.gif
image create photo BAD_NUCL    -file $GDefs(Dir)/share/image/Symbol/Icon/Bad_NUCLEAR.gif
image create photo BAD_CTBT    -file $GDefs(Dir)/share/image/Symbol/Icon/Bad_CTBT.gif
image create photo BAD_FIRE    -file $GDefs(Dir)/share/image/Symbol/Icon/Bad_FIRE.gif
image create photo BAD_OTHE    -file $GDefs(Dir)/share/image/Symbol/Icon/Bad_OTHER.gif
image create photo BAD_BIO     -file $GDefs(Dir)/share/image/Symbol/Icon/Bad_BIO.gif
image create photo BAD_SPILL   -file $GDefs(Dir)/share/image/Symbol/Icon/Bad_SPILL.gif
image create photo BAD_THERE   -file $GDefs(Dir)/share/image/Symbol/Icon/Bad_THERE.gif
image create photo BAD_PLANE   -file $GDefs(Dir)/share/image/Symbol/Icon/Bad_PLANE.gif

image create photo ACT_VOLC    -file $GDefs(Dir)/share/image/Symbol/Icon/Active_VOLCANO.gif
image create photo ACT_NUCL    -file $GDefs(Dir)/share/image/Symbol/Icon/Active_NUCLEAR.gif
image create photo ACT_CTBT    -file $GDefs(Dir)/share/image/Symbol/Icon/Active_CTBT.gif
image create photo ACT_FIRE    -file $GDefs(Dir)/share/image/Symbol/Icon/Active_FIRE.gif
image create photo ACT_OTHE    -file $GDefs(Dir)/share/image/Symbol/Icon/Active_OTHER.gif
image create photo ACT_BIO     -file $GDefs(Dir)/share/image/Symbol/Icon/Active_BIO.gif
image create photo ACT_SPILL   -file $GDefs(Dir)/share/image/Symbol/Icon/Active_SPILL.gif
image create photo ACT_THERE   -file $GDefs(Dir)/share/image/Symbol/Icon/Active_THERE.gif
image create photo ACT_PLANE   -file $GDefs(Dir)/share/image/Symbol/Icon/Active_PLANE.gif

image create photo SEL_VOLC  -file $GDefs(Dir)/share/image/Symbol/Icon/Sel_VOLCANO.gif
image create photo SEL_NUCL  -file $GDefs(Dir)/share/image/Symbol/Icon/Sel_NUCLEAR.gif
image create photo SEL_CTBT  -file $GDefs(Dir)/share/image/Symbol/Icon/Sel_CTBT.gif
image create photo SEL_FIRE  -file $GDefs(Dir)/share/image/Symbol/Icon/Sel_FIRE.gif
image create photo SEL_OTHE  -file $GDefs(Dir)/share/image/Symbol/Icon/Sel_OTHER.gif
image create photo SEL_STAT  -file $GDefs(Dir)/share/image/Symbol/Icon/Sel_STATION.gif
image create photo SEL_CITY  -file $GDefs(Dir)/share/image/Symbol/Icon/Sel_CITY.gif
image create photo SEL_THERE -file $GDefs(Dir)/share/image/Symbol/Icon/Sel_THERE.gif
image create photo SEL_PLANE -file $GDefs(Dir)/share/image/Symbol/Icon/Sel_PLANE.gif
