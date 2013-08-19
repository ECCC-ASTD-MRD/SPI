namespace eval Macro::HelloWorld { } {
   variable Param
   variable Data
   variable Error

   set Data(Something)  "Some data value"

   set Error(Something) { "Une erreur quelconque" "Some error" }

   set Param(Info)      { "Bonjour le monde" "Hello world" }
} 

proc Macro::HelloWorld::Execute { } {
   variable Data
   variable Param

   #----- Display a message box

   Macro::Info "Hello World"
}

proc Macro::HelloWorld::Clean { } {
   variable Data
   variable Param

   #----- Nothing to clean
}






