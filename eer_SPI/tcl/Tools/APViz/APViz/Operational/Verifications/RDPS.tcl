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
   True:RDPS:<Vars>:<Levels>:<Hours>:-:<Runs>:-
   True:RDPS:PN:0.0:<Hours>:-:<Runs>:-
}