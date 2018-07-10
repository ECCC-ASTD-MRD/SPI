#Geography
set Map(Cameras)    {}
set Map(Projection) Orthographic

#Styles
dataspec create SD 

#Ranges
set Range(Hours)   {  } 

#Layers (On:Model:Var:Level:Hour:Interval:Run:Source)
set Layers {
   True:GDPS:SD:0.0
   True:OBS:SD:0.0
}