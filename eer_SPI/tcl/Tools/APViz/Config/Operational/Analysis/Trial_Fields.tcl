#Geography
set Map(Cameras)    {}
set Map(Projection) Orthographic

#Styles
dataspec create GZ 
dataspec create HU
dataspec create TT
dataspec create UV 

#Ranges
variable Range
set Range(Levels) { 1000 850 700 500 250 }
set Range(Vars)   { GZ HU TT UV } 
set Range(Hours)  { 24 36 48 }
set Range(Runs)   { 00 06 12 28 }

#Layers (On:Model:Var:Level:Hour:Interval:Run:Source)
set Layers {
   True:GDPS:<Vars>:<Levels>:<Hours>:-:<Runs>:-
   False:GFS:<Vars>:<Levels>:006:-:-:-
   False:UKMET:<Vars>:<Levels>:006:-:-:-
   False:RAOBS:<Vars>:-:-:-:-:-
}
