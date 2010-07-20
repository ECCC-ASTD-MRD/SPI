namespace eval EMPTY { }

proc EMPTY::Layout { Frame } {
   variable Data

   Page::Size $Frame 0 0

   Page::UpdateItems $Frame
}

proc EMPTY::LayoutUpdate { Frame } {
   SPI::LayoutUpdate $Frame
}
