namespace eval Macro::HelloWorld { } {
   variable Param
   variable Data
   variable Error

   set Data(Something)  "Some data value"

   set Error(Something) { "Une erreur quelconque" "Some error" }

   set Param(Info)      { "Example: Bonjour le monde" "Example: Hello world" }
} 

proc Macro::HelloWorld::Execute { } {
   variable Data
   variable Param

   #----- Display a message box

   Macro::Info { "Bonjour le monde" "Hello world" }
   
   set f    1
   set size -18
   set fonts { charter courier fix helvetica arial lucida "new century schoolbook" terminal times }

   foreach font $fonts {
    
      font create Font$f -family $font -weight bold -size $size
      $Page::Data(Canvas) create text 10 [expr $f*20] -text "Hello World ($font $size)" -font Font$f -anchor nw
      incr f
   }
   
   if { $SPI::Param(Batch) } {
      PrintBox::Image $Page::Data(Frame) png HelloWorld
      SPI::Quit
   }
}

proc Macro::HelloWorld::Clean { } {
   variable Data
   variable Param

   #----- Nothing to clean
}






