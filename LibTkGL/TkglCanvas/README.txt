**************************************
       Tk OpenGL canvas 
**************************************

This is a "mostly" direct conversion of the Tk canvas to use OpenGL instead
of XWindows. You can use it as you would for a regular Tk canvas. The goal of
this package is to be able to create canvas item using harwdare accelerated 
OpenGL in C which.

Here are the addons from a regular canvas.

 * Transparency for every canvas items using -alpha [0,100]. (No postscript)
 * A buffer funciotn which puts the content of the canvas into a Tk image.
 * a magnify parameter which puts a maginfy image into a Tk image.
 * A glrender command allows control over the OpenGL rendering engine.

BUILDING AND INSTALLING THE WIDGET

1. Uncompress and unpack the distribution

   ON UNIX and OS X:
        gzip -cd TkglCanvas<version>.tar.gz | tar xf -

   ON WINDOWS:
        use something like WinZip to unpack the archive.

   ON MACINTOSH:
        use StuffIt Expander to unstuff the archive.
    
   This will create a subdirectory TkglCanvas<version> with all the files in it.

2. Configure

   ON UNIX and OS X:
        cd TkglCanvas<version>
        ./configure

   TkglCanvas uses information left in tkConfig.sh when you built tk.  This
   file will be found in $exec_prefix/lib/.  You might set the --prefix and
   --exec-prefix options of configure if you don't want the default
   (/usr/local).  If building on multiple unix platforms, the following is
   recommended to isolate build conflicts:
        mkdir <builddir>/<platform>
        cd !$
        /path/to/TkglCanvas<version>/configure

   ON WINDOWS:

   This version support building in the cygwin environment on
   Windows based on TEA (http://www.tcl.tk/doc/tea/).  You can retrieve
   cygwin from:
        http://sources.redhat.com/cygwin/

   Inside the cygwin environment, you build the same as on Unix.

   Otherwise, hack makefile.vc until it works and compile.  It has problems
   executing wish from a path with a space in it, but the DLL builds just
   fine.  A DLL should be available where you found this archive.

3. Make and Install

   ON UNIX< OS X or WINDOWS (with cygwin):
        make
        make test (OPTIONAL)
        make demo (OPTIONAL)
        make install

   ON WINDOWS (makefile.vc):
        nmake -f makefile.vc
        nmake -f makefile.vc test (OPTIONAL)
        nmake -f makefile.vc install

   TkglCanvas is built to comply to the latest tcl package conventions.

4. Use it

   Start a regular wish interpreter, 'load' the library, or use package require
   and create a glcanvas instead of a canvas. Use as you would for a standard canvas.
   There are a few test scripts in the demos directory which you can source.
