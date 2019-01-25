/*
 * tkImgSVG.c --
 *
 *      A photo image file handler for SVG (Portable PixMap) files.
 *
 * Copyright (c) 1994 The Australian National University.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution of
 * this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * Author: Paul Mackerras (paulus@cs.anu.edu.au),
 *      Department of Computer Science,
 *      Australian National University.
 */

#include "tkInt.h"

#include <stdio.h>
#include <string.h>
#include <math.h>
#define NANOSVG_ALL_COLOR_KEYWORDS  // Include full list of color keywords.
#define NANOSVG_IMPLEMENTATION      // Expands implementation
#include "nanosvg.h"
#define NANOSVGRAST_IMPLEMENTATION
#include "nanosvgrast.h"

/*
 * The format record for the PPM file format:
 */
static NSVGrasterizer *Rasterizer=NULL;

static int FileMatchSVG(Tcl_Channel chan, const char *fileName, Tcl_Obj *format, int *widthPtr, int *heightPtr, Tcl_Interp *interp);
static int FileReadSVG(Tcl_Interp *interp, Tcl_Channel chan, const char *fileName, Tcl_Obj *format, Tk_PhotoHandle imageHandle, int destX, int destY, int width, int height, int srcX, int srcY);
static int StringMatchSVG(Tcl_Obj *dataObj, Tcl_Obj *format, int *widthPtr, int *heightPtr, Tcl_Interp *interp);
static int StringReadSVG(Tcl_Interp *interp, Tcl_Obj *dataObj, Tcl_Obj *format, Tk_PhotoHandle imageHandle, int destX, int destY, int width, int height, int srcX, int srcY);

Tk_PhotoImageFormat tkImgFmtSVG = {
    "svg",                      /* name */
    FileMatchSVG,               /* fileMatchProc */
    StringMatchSVG,             /* stringMatchProc */
    FileReadSVG,                /* fileReadProc */
    StringReadSVG,              /* stringReadProc */
    NULL,                       /* fileWriteProc */
    NULL,                       /* stringWriteProc */
    NULL
};

/*
 *----------------------------------------------------------------------
 *
 * FileMatchSVG --
 *
 *      This function is invoked by the photo image type to see if a file
 *      contains image data in SVG format.
 *
 * Results:
 *      The return value is >0 if the first characters in file "f" look like
 *      PPM data, and 0 otherwise.
 *
 * Side effects:
 *      The access position in f may change.
 *
 *----------------------------------------------------------------------
 */

static int
FileMatchSVG(
    Tcl_Channel chan,           /* The image file, open for reading. */
    const char *fileName,       /* The name of the image file. */
    Tcl_Obj *format,            /* User-specified format string, or NULL. */
    int *widthPtr, int *heightPtr,
                                /* The dimensions of the image are returned
                                 * here if the file is a valid raw PPM
                                 * file. */
    Tcl_Interp *interp)         /* unused */
{
    char buffer[256];
    if (Tcl_Read(chan, buffer, 255)<=0) {
        return(0);
    }
    buffer[255]='\0';
    return(strstr(buffer,"<svg")?1:0);
}

/*
 *----------------------------------------------------------------------
 *
 * FileReadSVG --
 *
 *      This function is called by the photo image type to read PPM format
 *      data from a file and write it into a given photo image.
 *
 * Results:
 *      A standard TCL completion code. If TCL_ERROR is returned then an error
 *      message is left in the interp's result.
 *
 * Side effects:
 *      The access position in file f is changed, and new data is added to the
 *      image given by imageHandle.
 *
 *----------------------------------------------------------------------
 */

static int
FileReadSVG(
    Tcl_Interp *interp,         /* Interpreter to use for reporting errors. */
    Tcl_Channel chan,           /* The image file, open for reading. */
    const char *fileName,       /* The name of the image file. */
    Tcl_Obj *format,            /* User-specified format string, or NULL. */
    Tk_PhotoHandle imageHandle, /* The photo image to write into. */
    int destX, int destY,       /* Coordinates of top-left pixel in photo
                                 * image to be written to. */
    int width, int height,      /* Dimensions of block of photo image to be
                                 * written to. */
    int srcX, int srcY)         /* Coordinates of top-left pixel to be used in
                                 * image being read. */
{
   NSVGimage *image = NULL;
   Tk_PhotoImageBlock block;
   int w,h;
   float r=1.0;
   
   
   if (!(image=nsvgParseFromFile(fileName,"px",96.0f))) {
      Tcl_SetObjResult(interp,Tcl_ObjPrintf("couldn't read svg file \"%s\"", fileName));
      Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", NULL);
      return TCL_ERROR; 
   }
  
   if (!Rasterizer) {
      if (!(Rasterizer=nsvgCreateRasterizer())) {
         Tcl_SetObjResult(interp,Tcl_ObjPrintf("couldn't initiate rasterizer"));
         Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "RASTERIZER", NULL);
         return TCL_ERROR;
      }
   }

   // Define size/ratio depending on tk image size
   Tk_PhotoGetSize(imageHandle,&w,&h); 
   if (!w || !h) {
      block.width     = image->width;
      block.height    = image->height;
   } else {
      if (image->width && image->height) {
         block.width     = w;
         block.height    = h;
         r=(float)w/image->width<(float)h/image->height?(float)w/image->width:(float)h/image->height;
      } else {
         block.width     = w;
         block.height    = h;
      }
   }

   block.pixelPtr  = ckalloc(block.height*block.width*4);
   block.pixelSize = 4;
   block.pitch     = block.pixelSize * block.width;
   block.offset[0] = 0;
   block.offset[1] = 1;
   block.offset[2] = 2;
   block.offset[3] = 3;

   nsvgRasterize(Rasterizer,image,0,0,r,block.pixelPtr, block.width, block.height, block.width*4);
   nsvgDelete(image);

   if (Tk_PhotoExpand(interp, imageHandle, destX + block.width, destY + block.height) != TCL_OK) {
      return TCL_ERROR;
   }

   if (Tk_PhotoPutBlock(interp, imageHandle, &block, destX, destY, block.width, block.height, TK_PHOTO_COMPOSITE_SET) != TCL_OK) {
      ckfree(block.pixelPtr);
      return TCL_ERROR;
   }    

   ckfree(block.pixelPtr);
   return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * StringMatchPPM --
 *
 *      This function is invoked by the photo image type to see if a string
 *      contains image data in PPM format.
 *
 * Results:
 *      The return value is >0 if the first characters in file "f" look like
 *      PPM data, and 0 otherwise.
 *
 * Side effects:
 *      The access position in f may change.
 *
 *----------------------------------------------------------------------
 */

static int
StringMatchSVG(
    Tcl_Obj *dataObj,           /* The image data. */
    Tcl_Obj *format,            /* User-specified format string, or NULL. */
    int *widthPtr, int *heightPtr,
                                /* The dimensions of the image are returned
                                 * here if the file is a valid raw PPM
                                 * file. */
    Tcl_Interp *interp)         /* unused */
{
    int dataSize;
    char *dataBuffer;

    dataBuffer=Tcl_GetStringFromObj(dataObj, &dataSize);
    dataBuffer[255]='\0';

    return(strstr(dataBuffer,"<svg")?1:0);
}

/*
 *----------------------------------------------------------------------
 *
 * StringReadSVG --
 *
 *      This function is called by the photo image type to read SVG format
 *      data from a string and write it into a given photo image.
 *
 * Results:
 *      A standard TCL completion code. If TCL_ERROR is returned then an error
 *      message is left in the interp's result.
 *
 * Side effects:
 *      New data is added to the image given by imageHandle.
 *
 *----------------------------------------------------------------------
 */

static int
StringReadSVG(
    Tcl_Interp *interp,         /* Interpreter to use for reporting errors. */
    Tcl_Obj *dataObj,           /* The image data. */
    Tcl_Obj *format,            /* User-specified format string, or NULL. */
    Tk_PhotoHandle imageHandle, /* The photo image to write into. */
    int destX, int destY,       /* Coordinates of top-left pixel in photo
                                 * image to be written to. */
    int width, int height,      /* Dimensions of block of photo image to be
                                 * written to. */
    int srcX, int srcY)         /* Coordinates of top-left pixel to be used in
                                 * image being read. */
{
   NSVGimage *image = NULL;
   Tk_PhotoImageBlock block;
   int dataSize;
   int w,h;
   float r;

   
   if (!(image=nsvgParse(Tcl_GetStringFromObj(dataObj,&dataSize),"px",96.0f))) {
      Tcl_SetObjResult(interp,Tcl_ObjPrintf("couldn't read parse svg string"));
      Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", NULL);
      return TCL_ERROR; 
   }
  
   if (!Rasterizer) {
      if(!(Rasterizer=nsvgCreateRasterizer())) {
         Tcl_SetObjResult(interp,Tcl_ObjPrintf("couldn't initiate rasterizer"));
        Tcl_SetErrorCode(interp, "TK", "IMAGE", "SVG", "RASTERIZER", NULL);
         return TCL_ERROR;
      }
   }

   // Define size/ratio depending on tk image size
   Tk_PhotoGetSize(imageHandle,&w,&h); 
   if (!w || !h) {
      block.width     = image->width;
      block.height    = image->height;
      r=1.0;
   } else {
      block.width     = w;
      block.height    = h;
      r=(float)w/image->width<(float)h/image->height?(float)w/image->width:(float)h/image->height;
   }
       
   block.pixelPtr  = ckalloc(block.height*block.width*4);
   block.pixelSize = 4;
   block.pitch     = block.pixelSize * block.width;
   block.offset[0] = 0;
   block.offset[1] = 1;
   block.offset[2] = 2;
   block.offset[3] = 3;
   
   nsvgRasterize(Rasterizer, image, 0,0,r, block.pixelPtr, width, height, width*4);
   nsvgDelete(image);

   if (Tk_PhotoExpand(interp, imageHandle, destX + block.width, destY + block.height) != TCL_OK) {
      return TCL_ERROR;
   }

   if (Tk_PhotoPutBlock(interp, imageHandle, &block, destX, destY, block.width, block.height, TK_PHOTO_COMPOSITE_SET) != TCL_OK) {
      ckfree(block.pixelPtr);
      return TCL_ERROR;
   }    

   ckfree(block.pixelPtr);
   return TCL_OK;

}
/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * End:
 */
