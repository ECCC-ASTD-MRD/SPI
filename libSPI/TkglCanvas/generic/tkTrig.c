#include "tkInt.h"
#include "tkglCanvas.h"

double InterpHermite(double X0,double X1,double X2, double X3,double F,double T,double B) {

  double a0,a1,a2,a3,f0,f1,f2,f3;

   f2=F*F;
   f3=f2*F;

   f0=(X1-X0)*(1+B)*(1-T)/2 + (X2-X1)*(1-B)*(1-T)/2;
   f1=(X2-X1)*(1+B)*(1-T)/2 + (X3-X2)*(1-B)*(1-T)/2;

   a0=2*f3-3*f2+1;
   a1=f3-2*f2+F;
   a2=f3-f2;
   a3=-2*f3+3*f2;

   return(a0*X1+a1*f0+a2*f1+a3*X2);
}

void
TkHermiteScreenPoints(
    Tk_Canvas canvas,           /* Canvas in which curve is to be drawn. */
    double control[],           /* Array of coordinates for four control
                                 * points: x0, y0, x1, y1, ... x3 y3. */
    int numSteps,               /* Number of curve points to generate. */
    register XPoint *xPointPtr) /* Where to put new points. */
{
   int i;
   double x,y;
    
   for (i = 0; i < numSteps; i++, xPointPtr++) {
      x=InterpHermite(control[0],control[2],control[4],control[6],(double)i/numSteps,0.0,0.0);
      y=InterpHermite(control[1],control[3],control[5],control[7],(double)i/numSteps,0.0,0.0);
      Tk_CanvasDrawableCoords(canvas,x,y,&xPointPtr->x,&xPointPtr->y);
   }
}

void
TkHermitePoints(
    double control[],           /* Array of coordinates for four control
                                 * points: x0, y0, x1, y1, ... x3 y3. */
    int numSteps,               /* Number of curve points to generate. */
    register double *coordPtr)  /* Where to put new points. */
{
   int i;
    
   for (i = 0; i < numSteps; i++, coordPtr += 2) {
      coordPtr[0] = InterpHermite(control[0],control[2],control[4],control[6],(double)i/numSteps,0.0,0.0);
      coordPtr[1] = InterpHermite(control[1],control[3],control[5],control[7],(double)i/numSteps,0.0,0.0);
   }
}

   /*
 *--------------------------------------------------------------
 *
 * TkMakeHermiteCurve --
 *
 *      Interpret the given set of points as the raw knots and control points
 *      defining a sequence of cubic Hermite curves. Create a new set of points
 *      that fit these Hermite curves. Output points are produced in either of
 *      two forms.
 *
 * Results:
 *      Either or both of the xPoints or dblPoints arrays are filled in. The
 *      return value is the number of points placed in the arrays.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

int
TkMakeHermiteCurve(
   Tk_Canvas canvas,           /* Canvas in which curve is to be drawn. */
   double *pointPtr,           /* Array of input coordinates: x0, y0, x1, y1,
                                * etc.. */
   int numPoints,              /* Number of points at pointPtr. */
   int numSteps,               /* Number of steps to use for each spline
                                * segments (determines smoothness of
                                * curve). */
   XPoint xPoints[],           /* Array of XPoints to fill in (e.g. for
                                * display). NULL means don't fill in any
                                * XPoints. */
   double dblPoints[])         /* Array of points to fill in as doubles, in
                                * the form x0, y0, x1, y1, .... NULL means
                                * don't fill in anything in this form. Caller
                                * must make sure that this array has enough
                                * space. */
{

   int closed, outputPoints, i,pi;
   int numCoords = numPoints*2;
   double control[8];

   if (!pointPtr) {
       /* Of pointPtr == NULL, this function returns an upper limit of the
        * array size to store the coordinates. This can be used to allocate
        * storage, before the actual coordinates are calculated.
        */
       return 1 + numPoints * numSteps;
   }

   outputPoints = 0;
   closed = 0;
    
   /* If the curve is a closed one */
   if ((pointPtr[0] == pointPtr[numCoords-2]) && (pointPtr[1] == pointPtr[numCoords-1])) {
      closed = 1;
   }

   for (i = 1; i < numPoints; i++) {

      pi=i*2;
        
      control[0] = pointPtr[pi-2];
      control[1] = pointPtr[pi-1];
      control[2] = pointPtr[pi];
      control[3] = pointPtr[pi+1];

      if ((i+1)>=numPoints-1) {
         if (closed) {
            control[4] = pointPtr[(pi+2)-numCoords+2];
            control[5] = pointPtr[(pi+3)-numCoords+2];
         } else {
            control[4] = control[2];
            control[5] = control[3];
         }
      } else {
         control[4] = pointPtr[pi+2];
         control[5] = pointPtr[pi+3];
      }
      if ((i+2)>=numPoints-1 && closed) {
         if (closed) {
            control[6] = pointPtr[(pi+4)-numCoords+2];
            control[7] = pointPtr[(pi+5)-numCoords+2];
         } else {
            control[6] = control[4];
            control[7] = control[5];
         }
      } else {
         control[6] = pointPtr[pi+4];
         control[7] = pointPtr[pi+5];
      }
 
      /* Generate an Hermite spline using the control points */
      if (xPoints != NULL) {
         TkHermiteScreenPoints(canvas, control, numSteps, xPoints);
         xPoints += numSteps;
      }
      if (dblPoints != NULL) {
         TkHermitePoints(control, numSteps, dblPoints);
         dblPoints += 2*numSteps;
      }
      outputPoints += numSteps;
   }
   return outputPoints;
}

/*
 *--------------------------------------------------------------
 *
 * TkMakeHermitePostscript --
 *
 *      This function generates Postscript commands that create a path
 *      corresponding to a given Bezier curve.
 *
 * Results:
 *      None. Postscript commands to generate the path are appended to the
 *      interp's result.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void
TkMakeHermitePostscript(
   Tcl_Interp *interp,         /* Interpreter in whose result the Postscript
                                * is to be stored. */
   Tk_Canvas canvas,           /* Canvas widget for which the Postscript is
                                * being generated. */
   double *pointPtr,           /* Array of input coordinates: x0, y0, x1, y1,
                                * etc.. */
   int numPoints)              /* Number of points at pointPtr. */
{
   int closed, outputPoints, i,pi;
   int numCoords = numPoints*2;
   double *dblPoints,*ptr;
   int numSteps = 12;
   double control[8];
   Tcl_Obj *psObj;

   outputPoints = 0;
   closed = 0;
    
   /* If the curve is a closed one */
   if ((pointPtr[0] == pointPtr[numCoords-2]) && (pointPtr[1] == pointPtr[numCoords-1])) {
      closed = 1;
   }
   ptr=dblPoints=(double*)malloc(numSteps*numCoords*sizeof(double));
   
   for (i = 1; i < numPoints; i++) {

      pi=i*2;
        
      control[0] = pointPtr[pi-2];
      control[1] = pointPtr[pi-1];
      control[2] = pointPtr[pi];
      control[3] = pointPtr[pi+1];

      if ((i+1)>=numPoints-1) {
         if (closed) {
            control[4] = pointPtr[(pi+2)-numCoords+2];
            control[5] = pointPtr[(pi+3)-numCoords+2];
         } else {
            control[4] = control[2];
            control[5] = control[3];
         }
      } else {
         control[4] = pointPtr[pi+2];
         control[5] = pointPtr[pi+3];
      }
      if ((i+2)>=numPoints-1 && closed) {
         if (closed) {
            control[6] = pointPtr[(pi+4)-numCoords+2];
            control[7] = pointPtr[(pi+5)-numCoords+2];
         } else {
            control[6] = control[4];
            control[7] = control[5];
         }
      } else {
         control[6] = pointPtr[pi+4];
         control[7] = pointPtr[pi+5];
      }
 
      /* Generate an Hermite spline using the control points */
      TkHermitePoints(control, numSteps, dblPoints);
      dblPoints += 2*numSteps;
      outputPoints += numSteps;
   }
   
   psObj = Tcl_ObjPrintf("%.15g %.15g moveto\n", ptr[0], Tk_CanvasPsY(canvas, ptr[1]));
   for(i=1; i<outputPoints-1; i++) {
      Tcl_AppendPrintfToObj(psObj,"%.15g %.15g lineto\n", ptr[i<<1], Tk_CanvasPsY(canvas, ptr[(i<<1)+1]));
   }

   Tcl_AppendObjToObj(Tcl_GetObjResult(interp),psObj);
   Tcl_DecrRefCount(psObj);
}

const Tk_SmoothMethod tkHermiteSmoothMethod = {
    "hermite",
    TkMakeHermiteCurve,
    (void (*) (Tcl_Interp *interp, Tk_Canvas canvas, double *coordPtr,
            int numPoints, int numSteps)) TkMakeHermitePostscript,
};

