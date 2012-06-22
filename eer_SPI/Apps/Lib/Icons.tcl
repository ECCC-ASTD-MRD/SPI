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

image create photo DOCK        -file $GDefs(Dir)/Resources/Image/Icon/Dock.gif
image create photo DOCKDELETE  -file $GDefs(Dir)/Resources/Image/Icon/DockDelete.gif
image create photo MODEL       -file $GDefs(Dir)/Resources/Image/Icon/Thermometer.gif
image create photo RESET       -file $GDefs(Dir)/Resources/Image/Icon/BlueCircle.gif
image create photo INTEROGATE  -file $GDefs(Dir)/Resources/Image/Icon/Help3.gif
image create photo INFOLOG     -file $GDefs(Dir)/Resources/Image/Icon/Inform.gif

image create photo FRAMESAVE   -file $GDefs(Dir)/Resources/Image/Icon/FrameSave.gif
image create photo FRAMEPOS    -file $GDefs(Dir)/Resources/Image/Icon/FramePos.gif
image create photo FRAMEADD    -file $GDefs(Dir)/Resources/Image/Icon/FrameAdd.gif
image create photo FRAMEDEL    -file $GDefs(Dir)/Resources/Image/Icon/FrameDelete.gif
image create photo FRAMEEXT    -file $GDefs(Dir)/Resources/Image/Icon/FrameExt.gif
image create photo FRAME0      -file $GDefs(Dir)/Resources/Image/Icon/Frame.gif
image create photo FRAME1      -file $GDefs(Dir)/Resources/Image/Icon/Frame1.gif
image create photo FRAME2      -file $GDefs(Dir)/Resources/Image/Icon/Frame2.gif
image create photo FRAME3      -file $GDefs(Dir)/Resources/Image/Icon/Frame3.gif
image create photo FRAMER      -file $GDefs(Dir)/Resources/Image/Icon/FrameR.gif
image create photo FRAMEL      -file $GDefs(Dir)/Resources/Image/Icon/FrameL.gif
image create photo FRAMET      -file $GDefs(Dir)/Resources/Image/Icon/FrameT.gif
image create photo FRAMEB      -file $GDefs(Dir)/Resources/Image/Icon/FrameB.gif

image create photo CALC        -file $GDefs(Dir)/Resources/Image/Icon/Calculator.gif
image create photo CALCSAVE    -file $GDefs(Dir)/Resources/Image/Icon/CalculatorSave.gif
image create photo CALCDEL     -file $GDefs(Dir)/Resources/Image/Icon/CalculatorDelete.gif
image create photo PLUS        -file $GDefs(Dir)/Resources/Image/Icon/Plus.gif
image create photo MINUS       -file $GDefs(Dir)/Resources/Image/Icon/Minus.gif
image create photo UP          -file $GDefs(Dir)/Resources/Image/Icon/TrUp.gif
image create photo DOWN        -file $GDefs(Dir)/Resources/Image/Icon/TrDown.gif
image create photo BOMB        -file $GDefs(Dir)/Resources/Image/Icon/Bomb.gif
image create photo CAUTION     -file $GDefs(Dir)/Resources/Image/Icon/Caution.gif
image create photo SUN         -file $GDefs(Dir)/Resources/Image/Icon/Sun.gif
image create photo ROAD        -file $GDefs(Dir)/Resources/Image/Icon/Road.gif
image create photo CLOCK       -file $GDefs(Dir)/Resources/Image/Icon/Clock.gif
image create photo PIN         -file $GDefs(Dir)/Resources/Image/Icon/Pin.gif
image create photo PINSAVE     -file $GDefs(Dir)/Resources/Image/Icon/PinSave.gif
image create photo PINDEL      -file $GDefs(Dir)/Resources/Image/Icon/PinDelete.gif
image create photo PINADD      -file $GDefs(Dir)/Resources/Image/Icon/PinAdd.gif
image create photo PINNEW      -file $GDefs(Dir)/Resources/Image/Icon/PinNew.gif
image create photo FLATTEN     -file $GDefs(Dir)/Resources/Image/Icon/Flatten.gif

image create photo FLYAROUND   -file $GDefs(Dir)/Resources/Image/Icon/FlyAround.gif
image create photo FLYCIRCLE   -file $GDefs(Dir)/Resources/Image/Icon/FlyCircle.gif
image create photo FLYTO       -file $GDefs(Dir)/Resources/Image/Icon/FlyTo.gif
image create photo FLYTHROUGH  -file $GDefs(Dir)/Resources/Image/Icon/FlyThrough.gif

image create photo BLANK       -file $GDefs(Dir)/Resources/Image/Icon/Blank.gif
image create photo LINK        -file $GDefs(Dir)/Resources/Image/Icon/Chain.gif
image create photo LINKSET     -file $GDefs(Dir)/Resources/Image/Icon/ChainSet.gif
image create photo UNLINK      -file $GDefs(Dir)/Resources/Image/Icon/UnChain.gif
image create photo EYE         -file $GDefs(Dir)/Resources/Image/Icon/Eyeball.gif
image create photo WORLD       -file $GDefs(Dir)/Resources/Image/Icon/World.gif
image create photo WORLDRESET  -file $GDefs(Dir)/Resources/Image/Icon/WorldReset.gif
image create photo NOWORLD     -file $GDefs(Dir)/Resources/Image/Icon/NoWorld.gif
image create photo ARROW       -file $GDefs(Dir)/Resources/Image/Icon/Select.gif
image create photo ARROWLINE   -file $GDefs(Dir)/Resources/Image/Icon/SelectLine.gif
image create photo ARROWSQUARE -file $GDefs(Dir)/Resources/Image/Icon/SelectSquare.gif
image create photo ARROWDEL    -file $GDefs(Dir)/Resources/Image/Icon/SelectDel.gif
image create photo ARROWADD    -file $GDefs(Dir)/Resources/Image/Icon/SelectAdd.gif
image create photo DELETE      -file $GDefs(Dir)/Resources/Image/Icon/Delete.gif
image create photo CHECK       -file $GDefs(Dir)/Resources/Image/Icon/Check.gif
image create photo SCHECK      -file $GDefs(Dir)/Resources/Image/Icon/SmallCheck.gif
image create photo OK          -file $GDefs(Dir)/Resources/Image/Icon/Ok.gif
image create photo PENCIL      -file $GDefs(Dir)/Resources/Image/Icon/Pencil.gif
image create photo CIRCLE      -file $GDefs(Dir)/Resources/Image/Icon/Circle.gif
image create photo OVAL        -file $GDefs(Dir)/Resources/Image/Icon/Oval.gif
image create photo SQUARE      -file $GDefs(Dir)/Resources/Image/Icon/Square.gif
image create photo LINE        -file $GDefs(Dir)/Resources/Image/Icon/Line.gif
image create photo POLY        -file $GDefs(Dir)/Resources/Image/Icon/Polygon.gif
image create photo BARB        -file $GDefs(Dir)/Resources/Image/Icon/Barb.gif
image create photo DASH1       -file $GDefs(Dir)/Resources/Image/Icon/Dash1.gif
image create photo DASH2       -file $GDefs(Dir)/Resources/Image/Icon/Dash2.gif
image create photo VAL         -file $GDefs(Dir)/Resources/Image/Icon/Gauge.gif
image create photo STREAM      -file $GDefs(Dir)/Resources/Image/Icon/Stream.gif
image create photo RULER       -file $GDefs(Dir)/Resources/Image/Icon/Ruler.gif
image create photo COMPASS     -file $GDefs(Dir)/Resources/Image/Icon/Compass.gif
image create photo SCALE       -file $GDefs(Dir)/Resources/Image/Icon/Scale.gif
image create photo BITMAP      -file $GDefs(Dir)/Resources/Image/Icon/ImageBitmap.gif
image create photo IMAGE       -file $GDefs(Dir)/Resources/Image/Icon/ImageColor.gif
image create photo TEXT        -file $GDefs(Dir)/Resources/Image/Icon/Text.gif
image create photo ERROR       -file $GDefs(Dir)/Resources/Image/Icon/Error.gif
image create photo TOOL        -file $GDefs(Dir)/Resources/Image/Icon/Hammer.gif
image create photo COLORMAP    -file $GDefs(Dir)/Resources/Image/Icon/Colormap.gif
image create photo BACK        -file $GDefs(Dir)/Resources/Image/Icon/ToBack.gif
image create photo FRONT       -file $GDefs(Dir)/Resources/Image/Icon/ToFront.gif
image create photo RANGE       -file $GDefs(Dir)/Resources/Image/Icon/Ranges.gif
image create photo MAGLOCK     -file $GDefs(Dir)/Resources/Image/Icon/MagLock.gif
image create photo TABLETO     -file $GDefs(Dir)/Resources/Image/Icon/TableTo.gif
image create photo TARGET      -file $GDefs(Dir)/Resources/Image/Icon/Target.gif

image create photo OPEN        -file $GDefs(Dir)/Resources/Image/Icon/Open.gif
image create photo OPENDIR     -file $GDefs(Dir)/Resources/Image/Icon/OpenDir.gif
image create photo OPENDOC     -file $GDefs(Dir)/Resources/Image/Icon/OpenDoc.gif
image create photo PAGE        -file $GDefs(Dir)/Resources/Image/Icon/DocumentSetup.gif
image create photo PAGESAVE    -file $GDefs(Dir)/Resources/Image/Icon/PageSave.gif
image create photo PAGEDEL     -file $GDefs(Dir)/Resources/Image/Icon/PageDelete.gif
image create photo PAGERUN     -file $GDefs(Dir)/Resources/Image/Icon/PageRun.gif
image create photo DOCRUN      -file $GDefs(Dir)/Resources/Image/Icon/DocumentRun.gif
image create photo DOCDEL      -file $GDefs(Dir)/Resources/Image/Icon/DocumentDelete.gif
image create photo DOCSEL      -file $GDefs(Dir)/Resources/Image/Icon/DocumentTo.gif
image create photo DOCSAVE     -file $GDefs(Dir)/Resources/Image/Icon/DocumentSave.gif
image create photo DOCLOAD     -file $GDefs(Dir)/Resources/Image/Icon/DocumentLoad.gif
image create photo DOCIN       -file $GDefs(Dir)/Resources/Image/Icon/DocumentIn.gif
image create photo DOCOUT      -file $GDefs(Dir)/Resources/Image/Icon/DocumentOut.gif
image create photo DOCNEW      -file $GDefs(Dir)/Resources/Image/Icon/DocumentNew.gif
image create photo DOC         -file $GDefs(Dir)/Resources/Image/Icon/Document.gif
image create photo DOCWRITE    -file $GDefs(Dir)/Resources/Image/Icon/DocumentDraw.gif
image create photo FOLD        -file $GDefs(Dir)/Resources/Image/Icon/Folder.gif
image create photo FOLDIN      -file $GDefs(Dir)/Resources/Image/Icon/FolderIn.gif
image create photo FOLDOUT     -file $GDefs(Dir)/Resources/Image/Icon/FolderOut.gif
image create photo FOLDUP      -file $GDefs(Dir)/Resources/Image/Icon/FolderUp.gif
image create photo FOLDHID     -file $GDefs(Dir)/Resources/Image/Icon/Magnify.gif
image create photo SHEET       -file $GDefs(Dir)/Resources/Image/Icon/Sheet.gif
image create photo BOOK        -file $GDefs(Dir)/Resources/Image/Icon/Book.gif
image create photo BOOKSAVE    -file $GDefs(Dir)/Resources/Image/Icon/BookSave.gif
image create photo GRID        -file $GDefs(Dir)/Resources/Image/Icon/Grid.gif
image create photo GRIDTO      -file $GDefs(Dir)/Resources/Image/Icon/GridTo.gif
image create photo GRIDSAVE    -file $GDefs(Dir)/Resources/Image/Icon/GridSave.gif
image create photo GRIDDEL     -file $GDefs(Dir)/Resources/Image/Icon/GridDelete.gif

image create photo MOUSE       -file $GDefs(Dir)/Resources/Image/Icon/MouseWheel.gif
image create photo MODEZOOM    -file $GDefs(Dir)/Resources/Image/Icon/Magnify.gif
image create photo MODECAM     -file $GDefs(Dir)/Resources/Image/Icon/Camera.gif
image create photo MODEFLY     -file $GDefs(Dir)/Resources/Image/Icon/Airplane.gif
image create photo MODEMAG     -file $GDefs(Dir)/Resources/Image/Icon/DocumentMag.gif
image create photo BINOCULAR   -file $GDefs(Dir)/Resources/Image/Icon/Binocular.gif
image create photo PARAMS      -file $GDefs(Dir)/Resources/Image/Icon/Parameters.gif
image create photo GRAPH       -file $GDefs(Dir)/Resources/Image/Icon/Graph.gif
image create photo GRAPHDATA   -file $GDefs(Dir)/Resources/Image/Icon/SearchRow.gif
image create photo GRAPHZOOM   -file $GDefs(Dir)/Resources/Image/Icon/GraphZoom.gif
image create photo GRAPHRESET  -file $GDefs(Dir)/Resources/Image/Icon/GraphReset.gif
image create photo OPTIONS     -file $GDefs(Dir)/Resources/Image/Icon/Options.gif
image create photo CAMDEL      -file $GDefs(Dir)/Resources/Image/Icon/CameraDelete.gif
image create photo CAMSAVE     -file $GDefs(Dir)/Resources/Image/Icon/CameraSave.gif
image create photo CAMPATH     -file $GDefs(Dir)/Resources/Image/Icon/CameraPath.gif
image create photo ZOOMBACK    -file $GDefs(Dir)/Resources/Image/Icon/Begin.gif
image create photo ZOOMCLR     -file $GDefs(Dir)/Resources/Image/Icon/Left.gif
image create photo CAMDOWN     -file $GDefs(Dir)/Resources/Image/Icon/Down.gif
image create photo PRINT       -file $GDefs(Dir)/Resources/Image/Icon/Print.gif
image create photo SAVE        -file $GDefs(Dir)/Resources/Image/Icon/Save.gif
image create photo BUBBLE      -file $GDefs(Dir)/Resources/Image/Icon/HelpBubble.gif
image create photo BUBBLEGRAPH -file $GDefs(Dir)/Resources/Image/Icon/GraphBubble.gif
image create photo INFO        -file $GDefs(Dir)/Resources/Image/Icon/InfoBubble.gif
image create photo PALETTE     -file $GDefs(Dir)/Resources/Image/Icon/Palette.gif
image create photo COMPUTER    -file $GDefs(Dir)/Resources/Image/Icon/Computer.gif
image create photo LOCATION    -file $GDefs(Dir)/Resources/Image/Icon/Location.gif
image create photo FINGER      -file $GDefs(Dir)/Resources/Image/Icon/FingerDown.gif
image create photo ENVELOPE    -file $GDefs(Dir)/Resources/Image/Icon/Envelope.gif
image create photo ENVELOPE2   -file $GDefs(Dir)/Resources/Image/Icon/NewEnvelope.gif
image create photo ADDRESS     -file $GDefs(Dir)/Resources/Image/Icon/BCard.gif

image create photo VCRPLAYB    -file $GDefs(Dir)/Resources/Image/Icon/VCRPlayBackward.gif
image create photo VCRPLAYF    -file $GDefs(Dir)/Resources/Image/Icon/VCRPlayForward.gif
image create photo VCRFRAMEB   -file $GDefs(Dir)/Resources/Image/Icon/VCRFrameBackward.gif
image create photo VCRFRAMEF   -file $GDefs(Dir)/Resources/Image/Icon/VCRFrameForward.gif
image create photo VCRSTOP     -file $GDefs(Dir)/Resources/Image/Icon/VCRStop.gif
image create photo VCRREWIND   -file $GDefs(Dir)/Resources/Image/Icon/VCRRewind.gif
image create photo VCRFORWIND  -file $GDefs(Dir)/Resources/Image/Icon/VCRForwind.gif
image create photo VCRCYCLE    -file $GDefs(Dir)/Resources/Image/Icon/VCRCycle.gif
image create photo VCRSAVE     -file $GDefs(Dir)/Resources/Image/Icon/VCRSave.gif
image create photo VCRWEB      -file $GDefs(Dir)/Resources/Image/Icon/VCRWeb.gif
image create photo VCRLOCK     -file $GDefs(Dir)/Resources/Image/Icon/Lock.gif
image create photo VCRUNLOCK   -file $GDefs(Dir)/Resources/Image/Icon/UnLock.gif

image create photo ICO_VOLC    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_VOLCANO.gif
image create photo ICO_NUCL    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_NUCLEAR.gif
image create photo ICO_CTBT    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_CTBT.gif
image create photo ICO_FIRE    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_FIRE.gif
image create photo ICO_OTHE    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_OTHER.gif
image create photo ICO_BIO     -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_BIO.gif
image create photo ICO_SPILL   -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_SPILL.gif
image create photo ICO_THERE   -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Type_THERE.gif

image create photo BAD_VOLC    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Bad_VOLCANO.gif
image create photo BAD_NUCL    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Bad_NUCLEAR.gif
image create photo BAD_CTBT    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Bad_CTBT.gif
image create photo BAD_FIRE    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Bad_FIRE.gif
image create photo BAD_OTHE    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Bad_OTHER.gif
image create photo BAD_BIO     -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Bad_BIO.gif
image create photo BAD_SPILL   -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Bad_SPILL.gif
image create photo BAD_THERE   -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Bad_THERE.gif

image create photo ACT_VOLC    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Active_VOLCANO.gif
image create photo ACT_NUCL    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Active_NUCLEAR.gif
image create photo ACT_CTBT    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Active_CTBT.gif
image create photo ACT_FIRE    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Active_FIRE.gif
image create photo ACT_OTHE    -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Active_OTHER.gif
image create photo ACT_BIO     -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Active_BIO.gif
image create photo ACT_SPILL   -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Active_SPILL.gif
image create photo ACT_THERE   -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Active_THERE.gif

image create photo SEL_VOLC  -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Sel_VOLCANO.gif
image create photo SEL_NUCL  -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Sel_NUCLEAR.gif
image create photo SEL_CTBT  -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Sel_CTBT.gif
image create photo SEL_FIRE  -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Sel_FIRE.gif
image create photo SEL_OTHE  -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Sel_OTHER.gif
image create photo SEL_STAT  -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Sel_STATION.gif
image create photo SEL_CITY  -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Sel_CITY.gif
image create photo SEL_THERE -file $GDefs(Dir)/Resources/Image/Symbol/Icon/Sel_THERE.gif
