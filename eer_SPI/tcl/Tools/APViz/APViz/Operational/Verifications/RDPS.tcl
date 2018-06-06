#Geography
set Map(Cameras)    {}
set Map(Projection) Orthographic

#Styles
set comment {
dataspec create GZ 
dataspec create HU
dataspec create TT
dataspec create UV
}

#Ranges
variable Range
set Range(Levels) 	{1000 850 700 500 250}
set Range(Vars)   	{GZ HU TT UV}
set Range(Vars_Lev0)	{PN PR}
set Range(Hours)  	{000 024 036 048}
set Range(Runs)   	{00 06 12 28}
set Range(Sources)	{pres diag eta hyb}

#Layers (On:Model:Var:Level:Hour:Interval:Run:Source)
set Layers {
   True:RDPS:<Vars>:<Levels>:<Hours>:-:<Runs>:<Sources>
   False:RDPS:<Vars_Lev0>:0.0:<Hours>:-:<Runs>:<Sources>
}