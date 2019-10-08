namespace eval Macro::Quit { } {
   variable Param
   variable Data
   variable Error

  set Param(Info)      { "Test quit" "Test quit" }
} 

proc Macro::Quit::Execute { } {
   variable Data
   variable Param

   #----- Display a message box

   Macro::Info "Quitting"
   SPI::Quit
}

proc Macro::Quit::Clean { } {
   variable Data
   variable Param

   #----- Nothing to clean
}






