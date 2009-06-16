/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de rendue donnees.
 * Fichier   : Render.c
 * Creation  : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Fonctions d'affichage de donnees.
 *
 * Remarques :
 *
 * License      :
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation,
 *    version 2.1 of the License.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the
 *    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 *    Boston, MA 02111-1307, USA.
 *
 *==============================================================================
 */

#include "Data_FF.h"

void  Data_RenderBarbule(TDataSpecVECTOR Type,int Flip,float Axis,float Lat,float Lon,float Elev,float Speed,float Dir,float Size,Projection *Proj);
void  Data_RenderMark(Tcl_Interp *Interp,TDataSpec *Spec,ViewportItem *VP,int X,int Y,char *Id,char* Val);

int   Data_GetContour(int Mode,TData *Field,Projection *Proj,int NbInter,float *Inter);
void  Data_RenderContour(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,Projection *Proj);
void  Data_RenderGrid(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,Projection *Proj);
void  Data_RenderLabel(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,Projection *Proj);
int   Data_RenderParticle(TData *Field,ViewportItem *VP,Projection *Proj);
int   Data_RenderStream(TData *Field,ViewportItem *VP,Projection *Proj);
int   Data_RenderStream3D(TData *Field,ViewportItem *VP,Projection *Proj);
int   Data_RenderTexture(TData *Field,ViewportItem *VP,Projection *Proj);
void  Data_RenderValue(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,Projection *Proj,int Tile);
void  Data_RenderVector(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,Projection *Proj);
int   Data_RenderVolume(TData *Field,ViewportItem *VP,Projection *Proj);
int   Data_RenderRange(TData *Field,ViewportItem *VP,Projection *Proj);

/*----------------------------------------------------------------------------
 * Nom      : <Data_GetContour>
 * Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer la liste des segments de contours
 *
 * Parametres :
 *  <Mode>    : Type de referenciel (REF_COOR,REF_GRID,REF_PROJ)
 *  <Field>   : Champs
 *  <Proj>    : Parametres de la projection
 *  <NbInter> : Nombre d'intervalle
 *  <Inter>   : Liste des intervals
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_GetContour(int Mode,TData *Field,Projection *Proj,int NbInter,float *Inter){

   int n,i,j,d,ci,cj,i0,i1,j0,j1,len,t;
   unsigned char *buf;
   TArray *array;

   if (Mode==REF_COOR && !Field->Ref)
      return(0);

   buf=(unsigned char*)malloc(FSIZE2D(Field->Def));

   for (n=0;n<NbInter;n++) {
      /*Is the level within the data interval*/
      if (Inter[n]>=Field->Stat->Max)
         continue;

      memset(buf,0x0,FSIZE2D(Field->Def));

      d=0;t=0;
      i=-1;j=0;
      ci=1;cj=0;

      i0=0;
      j0=1;
      i1=Field->Def->NI-1;
      j1=Field->Def->NJ-1;

      /*As long as we did not check all gridpoint (Worse case)*/
      while(d++<FSIZE2D(Field->Def)) {

         if (i==i1 && ci>0) { ci=0;  cj=1;  i1--; t=1; } /* Check lower right corner */
         if (j==j1 && cj>0) { ci=-1; cj=0;  j1--; t=2; } /* Check upper right corner */
         if (i==i0 && ci<0) { ci=0;  cj=-1; i0++; t=3; } /* Check upper left corner */
         if (j==j0 && cj<0) { ci=1;  cj=0;  j0++; t=0; } /* Check lower left corner */

         i+=ci;
         j+=cj;

         /*When we get to the center, we're done*/
         if (i1<=i0 && j1<=j0) {
            break;
         }

         if (!buf[Field->Def->NI*j+i]) {
            len=FFContour_Triangle(Field->Ref,Field->Def,NULL,buf,i,j,Field->Def->Level,Inter[n],Mode,t);
//            len=FFContour_Quad(Field->Ref,Field->Def,NULL,buf,i,j,Field->Def->Level,Inter[n],Mode);
            if (len>1) {
               if ((array=TArray_Alloc(Inter[n],len))) {
                  Field->Segments=TList_Add(Field->Segments,array);
                  FFContour_Triangle(Field->Ref,Field->Def,array->Data,buf,i,j,Field->Def->Level,Inter[n],Mode,t);
//                  FFContour_Quad(Field->Ref,Field->Def,array->Data,buf,i,j,Field->Def->Level,Inter[n],Mode);
               } else {
                  fprintf(stderr,"(ERROR) Data_GetContour: Unable to alloc memory for contour %f",Inter[n]);
               }
            }
         }
      }
   }
   free(buf);
   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Render>
 * Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage des donnees vectorielles du champs.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <VP>      : Parametres du viewpoint
 *  <Proj>    : Parametres de la projection
 *  <Field>   : Pointeur sur le Champs
 *  <GLMode>  : Mode de rendue
 *  <Mode>    : Type de donnees (GL_ALL,GL_RASTER,GL_VECTOR)
 *
 * Retour:
 *  <nras>    : Nombre de donnees "raster" rendues
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_Grid3D(TData *Field,Projection* Proj) {

   int k,nk;

   if (Field->Ref->LevelNb==1) {
      if (Field->ReadCube) Field->ReadCube(NULL,Field,0);
      Data_PreInit(Field);
   }

   nk=0;
   for(k=0;k<Field->Ref->LevelNb;k++) {
      if (Field->Ref->Pos && Field->Ref->Pos[k]) {
         nk++;
      } else {
         if (Field->Grid(Field,Proj,k)) {
            nk++;
         }
      }
   }
   return(nk==Field->Ref->LevelNb);
}

int Data_Render(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,ClientData Proj,GLuint GLMode,int Mode) {

   int nras=0;

   /*Verifier l'existence du champs*/
   if (!Field || !Field->Ref || !Field->Spec || !Field->Def->Data[0]) {
      return(0);
   }

   Data_PreInit(Field);

   if (!Field->Ref->Pos || !Field->Ref->Pos[Field->Def->Level])
      if (!Field->Grid(Field,Proj,Field->Def->Level))
         return(0);

   glPushName(PICK_FSTDFIELD);

   if (Mode==GL_ALL || Mode==GL_VECTOR) {

      if (!GLRender->GLZBuf) {
         if (Field->Spec->RenderGrid)
            Data_RenderGrid(Interp,Field,VP,(Projection*)Proj);

         if (Field->Spec->RenderContour && !Field->Spec->RenderVol)
            Data_RenderContour(Interp,Field,VP,(Projection*)Proj);

         if (Field->Spec->RenderVector==BARBULE || Field->Spec->RenderVector==ARROW)
            Data_RenderVector(Interp,Field,VP,(Projection*)Proj);

         if (Field->Spec->RenderValue)
            Data_RenderValue(Interp,Field,VP,(Projection*)Proj,Field->Spec->RenderValue);
      }
   }

   if (Mode==GL_ALL || Mode==GL_RASTER) {

      glEnable(GL_DEPTH_TEST);

      if (GLRender->GLZBuf) {
         glEnable(GL_STENCIL_TEST);
         glStencilMask(0x10);
         glStencilFunc(GL_ALWAYS,0x10,0x10);
         glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);

         if (Field->Spec->RenderGrid)
            Data_RenderGrid(NULL,Field,VP,(Projection*)Proj);

         if (Field->Spec->RenderContour && !Field->Spec->RenderVol)
            Data_RenderContour(NULL,Field,VP,(Projection*)Proj);

         if (Field->Spec->RenderVector==BARBULE || Field->Spec->RenderVector==ARROW)
            Data_RenderVector(NULL,Field,VP,(Projection*)Proj);

         if (Field->Spec->RenderValue)
            Data_RenderValue(NULL,Field,VP,(Projection*)Proj,Field->Spec->RenderValue);

         glStencilMask(0xff);
         glStencilFunc(GL_EQUAL,0x0,0x1f);
         glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
      }

      /*Verifier la presence d'une palette de couleur si elle est necessaire*/
      if (Field->Spec->Map) {

         if (Field->Spec->RenderTexture && (!Field->Spec->RenderVol || Field->Ref->Grid[0]=='V')) {
            if (Field->Ref->Grid[0]!='X' && Field->Ref->Grid[0]!='Y' && Field->Ref->Grid[1]!='Y') {
               if (GLRender->ShaderAvailable) {
                  nras+=Data_RenderShaderTexture(Field,VP,(Projection*)Proj);
               } else {
                  nras+=Data_RenderTexture(Field,VP,(Projection*)Proj);
               }
            }
         }

         if (Field->Spec->RenderVector==STREAMLINE) {
            nras+=Data_RenderStream(Field,VP,(Projection*)Proj);
         }

         if (Field->Spec->RenderVector==STREAMLINE3D) {
            if (Field->Def->Data[2]) {
               if (Data_Grid3D(Field,Proj)) {
                  nras+=Data_RenderStream3D(Field,VP,(Projection*)Proj);
               }
            }
         }

         if (Field->Spec->RenderVol) {
            if (Field->Ref->Grid[0]!='X' && Field->Ref->Grid[0]!='V') {
               /*Recuperer les niveaux disponibles*/
               if (Data_Grid3D(Field,Proj)) {
                  nras+=Data_RenderVolume(Field,VP,(Projection*)Proj);
               }
            }
         }

         if (Field->Spec->RenderParticle) {
            if (GLRender->ShaderAvailable) {
               nras+=Data_RenderShaderParticle(Field,VP,(Projection*)Proj);
            } else {
               nras+=Data_RenderParticle(Field,VP,(Projection*)Proj);
            }
         }

         if (Field->Spec->RangeNb) {
            nras+=Data_RenderRange(Field,VP,(Projection*)Proj);
         }

         if (GLRender->GLZBuf) {
            glStencilMask(0xf0);
            glClear(GL_STENCIL_BUFFER_BIT);
         }
         glDisable(GL_DEPTH_TEST);
      }
   }

   glPopName();
   glStencilMask(0xff);
   glStencilFunc(GL_EQUAL,0x0,0xf);
   glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
   return(nras);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderBarbule>
 * Creation : Juin 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage des labels des niveaux.
 *
 * Parametres :
 *  <Type>    : Type d'affichage (1=barbules 2=fleches)
 *  <Axis>    : Rotation autour de l'axe Z
 *  <Lat>     : Latitude
 *  <Lon>     : Latitude
 *  <Elev>    : Elevation
 *  <Speed>   : Vitesse en noeud
 *  <Dir>     : Direction en degree
 *  <Size>    : Dimension
 *  <Proj>    : Parametres de projections
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_RenderBarbule(TDataSpecVECTOR Type,int Flip,float Axis,float Lat,float Lon,float Elev,float Speed,float Dir,float Size,Projection *Proj){

   double y,spd;

   /*Check for unwanted speed values*/
   if (Speed>1e5)
      return;

   glPushMatrix();

   /*Si on projete en 3D, effectue les transformations necessaires*/
   if (Proj) {
      Proj->Type->Locate(Proj,Lat,Lon,1);
      glTranslated(0.0,0.0,ZM(Proj,Elev));
      glRotatef(180,1.0,0.0,0.0);
   } else {
      glTranslatef(Lat,Lon,Elev);
   }

   if (Flip) {
      glScalef(1.0,-1.0,1.0);
   }

   /*Rotation dans le bon axe*/
   if (Axis!=0.0) {
      glRotatef(Axis,0.0,0.0,1.0);
      glRotatef(Dir,0.0,1.0,0.0);
      glRotatef(-90.0,1.0,0.0,0.0);
   } else {
      glRotatef(Dir,0.0,0.0,1.0);
   }

   if (Type==ARROW) {
      glScalef(Size,Size,1.0);
      glDrawArrow(GL_POLYGON);
   } else {
      /*Afficher un cercle pour des vitesses < 1*/
      if (Speed<1) {
         Size*=0.4;
         glScalef(Size,Size,1.0);
         glDrawCircle(64,GL_LINE_STRIP);
      } else {
         glScalef(Size,Size,1.0);
         y=-1.8;
         spd=Speed;

         /*Vecteur de base*/
         glBegin(GL_LINES);
            glVertex2d(0.0,0.0);
            glVertex2d(0.0,y);
         glEnd();

         /*Vitesse >50*/
         while (spd>=50.0) {
            glBegin(GL_TRIANGLES);
            glVertex2d(0.0,y);
            glVertex2d(0.0,y+0.4);
            glVertex2d(0.7,y-0.2);
            spd-=50.0;
            y+=0.5;
            glEnd();
         }

         glBegin(GL_LINES);
         /*Vitesse >10*/
         while (spd>=10.0) {
            glVertex2d(0.0,y);
            glVertex2d(0.7,y-0.2);
            spd-=10.0;
            y+=0.25;
         }

         /*Vitesse >5*/
         if (spd>=5.0 || (Speed<5.0 && Speed>0.0)) {

            /*Positionner la ligne plus loin si c'est la seule*/
            if (Speed<10.0) {
               y+=0.25;
            }
            glVertex2d(0.0,y);
            glVertex2d(0.4,y-0.15);
         }
         glEnd();
      }
   }
   glPopMatrix();
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderContour>
 * Creation : Mars 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage des contours du champs.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Field>   : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_RenderContour(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,Projection *Proj){

   int  c;
   char buf[256];
   TList *list;
   TArray *array;

   if (!Field->Ref || !Field->Ref->Pos || (!Field->Spec->Outline && !Field->Spec->MapAll))
      return;

   /*Les contours sont-ils definit*/
   if (Field->Spec->InterNb && !Field->Segments) {
      Data_GetContour(REF_PROJ,Field,Proj,Field->Spec->InterNb,Field->Spec->Inter);
   }

   /* Render the contours */
   if (Field->Segments) {

      if (Field->Spec->RenderLabel && Interp)
         Tcl_AppendResult(Interp,"gsave\n",(char*)NULL);

      if (Field->Spec->RenderLabel)
         Data_RenderLabel(Interp,Field,VP,Proj);

      glEnableClientState(GL_VERTEX_ARRAY);
//      glEnable(GL_LINE_SMOOTH);

      glStencilMask(0x10);
      glStencilFunc(GL_NOTEQUAL,0x31,0x20);
      glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);

      if (Interp) {
         sprintf(buf,"%% Postscript des contours\n%i setlinewidth 1 setlinecap 1 setlinejoin\n",Field->Spec->RenderContour-1);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
         if (Field->Spec->Outline)
            Tk_CanvasPsColor(Interp,VP->canvas,Field->Spec->Outline);
      }

      if (Interp) {
         glPostscriptDash(Interp,&Field->Spec->Dash,Field->Spec->RenderContour);
      } else {
         glDash(&Field->Spec->Dash);
      }

      if (Field->Spec->Outline)
         glColor3us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue);
      glLineWidth(Field->Spec->RenderContour);

      list=Field->Segments;

      while(list) {
         array=(TArray*)list->Data;

         if (Interp)
            glFeedbackInit(array->Size*6,GL_2D);

         if (Field->Spec->MapAll) {
            VAL2COL(c,Field->Spec,array->Value);
            if (Interp) {
               CMap_PostscriptColor(Interp,Field->Spec->Map,c);
            } else {
               glColor4ubv(Field->Spec->Map->Color[c]);
            }
         }

         Proj->Type->Render(Proj,0,array->Data,NULL,NULL,NULL,GL_LINE_STRIP,array->Size,NULL,NULL);

         if (Interp)
            glFeedbackProcess(Interp,GL_2D);

         list=list->Next;
      }

      glDisableClientState(GL_VERTEX_ARRAY);
      glDisable(GL_LINE_STIPPLE);
      glDisable(GL_LINE_SMOOTH);

      if (GLRender->GLZBuf) {
         glStencilMask(0x10);
         glStencilFunc(GL_ALWAYS,0x10,0x10);
         glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);
      } else {
         glStencilMask(0xf);
         glStencilFunc(GL_EQUAL,0x0,0xf);
         glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
      }

      if (Interp) {
         glPostscriptDash(Interp,NULL,Field->Spec->RenderContour);
      }
   }

   if (Field->Spec->RenderLabel && Interp) {
      Tcl_AppendResult(Interp,"grestore\n",(char*)NULL);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderLabel>
 * Creation : Juin 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage des labels des niveauxle long des lignes de contours.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Field>   : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *  <Mode>    : Mode de rendue (FF_BOX,FF_TXT,FF_ALL)
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_RenderLabel(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,Projection *Proj){

   int  n,c,d,delta;
   char buf[256];
   double th,dx,dy,dnx,dny;
   TList *list;
   TArray *array;
   Vect3d  p1,p0;
   Tk_FontMetrics tkm;
   GLuint s;

   if (GLRender->Resolution>1 || !Field->Ref || !Field->Spec->Font || (!Field->Spec->Outline && !Field->Spec->MapAll) || !Field->Spec->InterNb) {
      return;
   }

   Projection_UnClip(Proj);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glLoadIdentity();

   glMatrixMode(GL_PROJECTION);
   glPushMatrix();
   glLoadIdentity();
   gluOrtho2D(0,VP->Width,0,VP->Height);

   Tk_GetFontMetrics(Field->Spec->Font,&tkm);
   dy=tkm.ascent;

   glDisable(GL_DEPTH_TEST);
   glPolygonMode(GL_FRONT,GL_FILL);
   glReadBuffer(GL_STENCIL);
   glEnable(GL_STENCIL_TEST);
   glStencilFunc(GL_ALWAYS,0xff,0xff);
   glStencilOp(GL_REPLACE,GL_REPLACE,GL_REPLACE);

   if (Field->Segments) {

      if (Interp) {
         if (Field->Spec->Outline)
            Tk_CanvasPsColor(Interp,VP->canvas,Field->Spec->Outline);
         Tk_CanvasPsFont(Interp,VP->canvas,Field->Spec->Font);
         Tcl_AppendResult(Interp,"clippath\n",(char*)NULL);
      } else {
         if (Field->Spec->Outline)
            glColor3us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue);

         glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),Field->Spec->Font);
      }

      list=Field->Segments;
      delta=Field->Spec->RenderLabel*100;

      while(list) {
         array=(TArray*)list->Data;
         if (array->Size<10) {
            list=list->Next;
            continue;
         }

         DataSpec_Format(Field->Spec,VAL2SPEC(Field->Spec,array->Value),buf);
         dx=Tk_TextWidth(Field->Spec->Font,buf,strlen(buf));

         if (Field->Spec->MapAll) {
            VAL2COL(c,Field->Spec,array->Value);
            if (Interp) {
               CMap_PostscriptColor(Interp,Field->Spec->Map,c);
            } else {
               glColor4ubv(Field->Spec->Map->Color[c]);
            }
         }

         /* Check if we need to caclulate this streamline. Will it be visible */
         n=0;
         gluProject(array->Data[n][0],array->Data[n][1],array->Data[n][2],VP->GLModR,VP->GLProj,VP->GLView,&p1[0],&p1[1],&p1[2]);
         d=delta;

         for(++n;n<array->Size;n++) {
            Vect_Assign(p0,p1);
            gluProject(array->Data[n][0],array->Data[n][1],array->Data[n][2],VP->GLModR,VP->GLProj,VP->GLView,&p1[0],&p1[1],&p1[2]);

            /* Test for insidness within viewport */
            if (!VIN(p0[0],dx+5,Proj->Params->VP->Width-dx-5) || !VIN(p0[1],dx+5,Proj->Params->VP->Height-dx-5) || !VIN(p0[2],0,1)) {
               continue;
            }
            if (!VIN(p1[0],dx+5,Proj->Params->VP->Width-dx-5) || !VIN(p1[1],dx+5,Proj->Params->VP->Height-dx-5) || !VIN(p1[2],0,1)) {
               continue;
            }

            d+=hypot(p1[0]-p0[0],p1[1]-p0[1]);
            if (Field->Spec->RenderLabel<0 || d>=delta) {

               /*Calculate the angle of the segment*/
               th=-atan2(p1[0]-p0[0],p1[1]-p0[1])+M_PI2;
               if (th<-M_PI2) th+=M_PI;
               if (th>M_PI2)  th-=M_PI;

               /*We have to translate along the rotation axis to center the label*/
               dnx=dx*0.5*cos(th)-dy*0.5*cos(M_PI2-th);
               dny=dx*0.5*sin(th)+dy*0.5*sin(M_PI2-th);
               th=RAD2DEG(th);

               glReadPixels(p1[0],p1[1],1,1,GL_STENCIL_INDEX,GL_UNSIGNED_INT,&s);
               if (s&0x20) continue;

               glReadPixels(p1[0]+dnx,p1[1]-dny,1,1,GL_STENCIL_INDEX,GL_UNSIGNED_INT,&s);
               if (s&0x20) continue;

               glReadPixels(p1[0]-dnx,p1[1]+dny,1,1,GL_STENCIL_INDEX,GL_UNSIGNED_INT,&s);
               if (s&0x20) continue;

               glReadPixels(p1[0]+dnx,p1[1]+dny,1,1,GL_STENCIL_INDEX,GL_UNSIGNED_INT,&s);
               if (s&0x20) continue;

               p1[0]-=dnx; p1[1]-=dny;
               glReadPixels(p1[0],p1[1],1,1,GL_STENCIL_INDEX,GL_UNSIGNED_INT,&s);
               if (s&0x20) continue;

               /*Draw the bloc in the stencil buffer*/
               glStencilMask(0x20);
               glStencilMaskQuad(p1[0],p1[1],dx,dy,th,4,1);

               if (Interp) {
                  glPostscriptTextBG(Interp,VP->canvas,p1[0],p1[1],th,dx,dy,4,1,VP->BGColor,1);
               }

               /*Draw the text*/
               glStencilMask(0x10);
               glPrint(Interp,VP->canvas,buf,p1[0],p1[1],(Interp?-th:th));
               d=0;
            }

            if (Field->Spec->RenderLabel<0) {
               break;
            }
         }
         list=list->Next;
      }

      if (Interp) {
         Tcl_AppendResult(Interp,"clip newpath\n",(char*)NULL);
      }
   }

   glMatrixMode(GL_PROJECTION);
   glPopMatrix();
   glMatrixMode(GL_MODELVIEW);
   glPopMatrix();

   if (GLRender->GLZBuf)
      glEnable(GL_DEPTH_TEST);
   Projection_Clip(Proj);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderGrid>
 * Creation : Janvier 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue les calculs et l'affichage de la grille.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Field>   : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_RenderGrid(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,Projection *Proj){

   char    buf[128];

   if (!Field->Ref || !Field->Ref->Pos || !Field->Spec->Outline)
      return;

   if (Interp) {
      glFeedbackInit(FSIZE2D(Field->Def)*3,GL_2D);
      sprintf(buf,"%% Postscript de la grille\n%.2f setlinewidth 1 setlinecap 1 setlinejoin\n",Field->Spec->RenderGrid-0.5);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
      Tk_CanvasPsColor(Interp,VP->canvas,Field->Spec->Outline);
   }

   /*Afficher les points*/
   glEnableClientState(GL_VERTEX_ARRAY);
   glPointSize(Field->Spec->RenderGrid+0.1);
   glColor3us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue);

   Proj->Type->Render(Proj,0,Field->Ref->Pos[Field->Def->Level],NULL,NULL,NULL,GL_POINTS,FSIZE3D(Field->Def),NULL,NULL);

   if (Interp)
      glFeedbackProcess(Interp,GL_2D);

   if (Field->Ref->Grid[0]=='M') {
      glPolygonMode(GL_FRONT,GL_LINE);
      glLineWidth(1.0);
      Proj->Type->Render(Proj,0,Field->Ref->Pos[Field->Def->Level],Field->Ref->Idx,NULL,NULL,GL_TRIANGLES,Field->Ref->NIdx,NULL,NULL);
   }
   glDisableClientState(GL_VERTEX_ARRAY);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderMark>
 * Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage d'une marque.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Spec>    : Specifications des donnees
 *  <VP>      : Parametres du viewport
 *  <X>       : Coordonnees en X
 *  <Y>       : Coordonnees en Y
 *  <Id>      : Identification de la marque
 *  <Val>     : Valeur de la marque
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_RenderMark(Tcl_Interp *Interp,TDataSpec *Spec,ViewportItem *VP,int X,int Y,char *Id,char* Val){

   int            txth,dtx;
   Tk_FontMetrics tkm;

   if (!Spec->Font) {
      return;
   }

   Tk_GetFontMetrics(Spec->Font,&tkm);
   txth=tkm.linespace*2;
   dtx=tkm.ascent*0.5;

   if (Interp) {
      glPostscriptText(Interp,VP->canvas,"*",X-Tk_TextWidth(Spec->Font,"*",1)/2,Y-dtx,0,Spec->Outline,0.0,0.5,0.0);
      glPostscriptText(Interp,VP->canvas,Id,X-Tk_TextWidth(Spec->Font,Id,strlen(Id))/2,Y-dtx+txth/2,0,Spec->Outline,0.0,0.5,0.0);
      glPostscriptText(Interp,VP->canvas,Val,X-Tk_TextWidth(Spec->Font,Val,strlen(Val))/2,Y-dtx-txth/3,0,Spec->Outline,0.0,0.5,0.0);
   } else {
      glDrawString(X-Tk_TextWidth(Spec->Font,"*",1)/2,Y-dtx,0,"*",1,0,0);
      glDrawString(X-Tk_TextWidth(Spec->Font,Id,strlen(Id))/2,Y-dtx+txth/2,0,Id,strlen(Id),0,0);
      glDrawString(X-Tk_TextWidth(Spec->Font,Val,strlen(Val))/2,Y-dtx-txth/3,0,Val,strlen(Val),0,0);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_FieldRenderParticle>
 * Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue les calculs et l'affichage pour le type particule.
 *
 * Parametres :
 *  <Champs>  : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_RenderParticle(TData *Field,ViewportItem *VP,Projection *Proj) {

   if (!Field->Ref || !Field->Ref->Pos) {
      return(0);
   }

   if (!Field->Map) {
      FSTD_DataMap(Field,True);
   }

   /*Setup 1D Texture*/
   glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,Field->Spec->Map->NbPixels,0,GL_RGBA,GL_UNSIGNED_BYTE,Field->Spec->Map->Color);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
   glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_REPLACE);
   glEnable(GL_TEXTURE_1D);

   /*Do we need transparency*/
   if (Field->Spec->Map->Alpha) {
      glEnable(GL_BLEND);
   }

   /*Projeter les particules*/
   glPointSize(Field->Spec->RenderParticle+0.1);
   glEnableClientState(GL_VERTEX_ARRAY);
   Proj->Type->Render(Proj,0,Field->Ref->Pos[Field->Def->Level],Field->Ref->Idx,NULL,Field->Map,GL_POINTS,Field->Ref->NIdx,NULL,NULL);
   glDisableClientState(GL_VERTEX_ARRAY);
   glDisable(GL_TEXTURE_1D);
   glDisable(GL_BLEND);

   return 1;
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderStream>
 * Creation : Janvier 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage de streamlines de champs vectoriels.
 *
 * Parametres :
 *  <Field>   : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_RenderStream(TData *Field,ViewportItem *VP,Projection *Proj){

   double i,j,dt;
   int    b,f,len,pi,pj,dz;
   float  step;
   Vect3d pix;
   Coord  coo;

   extern Vect3d GDB_VBuf[];

   if (GLRender->Resolution>2) {
      return(0);
   }

   if (!Field->Ref || !Field->Ref->Pos || !Field->Spec->Outline) {
      return(0);
   }

   /*Setup 1D Texture*/
   glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,256,0,GL_RGBA,GL_UNSIGNED_BYTE,StreamMap);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);
   glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL);
   glEnable(GL_TEXTURE_1D);

   if (!Field->Map) {
      Field->Map=(float*)malloc(sizeof(float)*STREAMLEN);
      for(len=0;len<STREAMLEN;len++) {
         Field->Map[len]=(float)len/STREAMLEN*32;
      }

   }

   glMatrixMode(GL_TEXTURE);
   if (GLRender->Delay<2000) {
      Field->Spec->TexStep+=0.01;
      Field->Spec->TexStep=Field->Spec->TexStep>1.0?0.0:Field->Spec->TexStep;
  }

   glEnable(GL_STENCIL_TEST);
   glStencilMask(0x2);
   glStencilFunc(GL_NOTEQUAL,0x2,0x2);
   glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);

   glReadBuffer(GL_STENCIL);
   glEnableClientState(GL_VERTEX_ARRAY);
   glColor3us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue);
   glEnable(GL_BLEND);

   pi=pj=-1;
   len=512;
   dz=Field->Spec->Sample*10;
   dt=0.0;

   /*Recuperer les latlon des pixels sujets*/
   for (pix[0]=0;pix[0]<VP->Width;pix[0]+=dz) {
      for (pix[1]=0;pix[1]<VP->Height;pix[1]+=dz) {

         Proj->Type->UnProject(VP,Proj->Params,&coo,pix);
         if (coo.Lat==-999.0) {
            continue;
         }

         if (Field->Ref->UnProject(Field->Ref,&i,&j,coo.Lat,coo.Lon,0,1) && i<Field->Def->NI-2 && j<Field->Def->NJ-2) {
            /*Get the cell resolution, if not the same, to use as step size for a constant spacing*/
            if (pi!=(int)i ||  pj!=(int)j) {
               pi=i;
               pj=j;
               step=5.0/FFCellResolution(VP,Proj,Field->Ref->Pos[Field->Def->Level][FIDX2D(Field->Def,pi,pj)],Field->Ref->Pos[Field->Def->Level][FIDX2D(Field->Def,pi+1,pj+1)]);
            }
            /*Get the streamline */
            b=FFStreamLine(Field->Ref,Field->Def,VP,GDB_VBuf,NULL,i,j,Field->Def->Level,len,-step,Field->Spec->Min,0,REF_PROJ,0);
            f=FFStreamLine(Field->Ref,Field->Def,VP,&GDB_VBuf[len],NULL,i,j,Field->Def->Level,len,step,Field->Spec->Min,0,REF_PROJ,0);

            /* If we have at least some part of it */
            glPushMatrix();
            glTranslatef(-Field->Spec->TexStep-(dt+=0.15),0.0,0.0);
            if (b+f>2) {
               glLineWidth(Field->Spec->Width);
               glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
               Proj->Type->Render(Proj,0,&GDB_VBuf[len-b],NULL,NULL,Field->Map,GL_LINE_STRIP,b+f,NULL,NULL);

               glLineWidth(8*Field->Spec->Width);
               glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
               Proj->Type->Render(Proj,0,&GDB_VBuf[len-b],NULL,NULL,NULL,GL_LINE_STRIP,b+f,NULL,NULL);
            }
            glPopMatrix();
         }
      }
   }

   glClear(GL_STENCIL_BUFFER_BIT);
   glStencilMask(0xf);
   glStencilFunc(GL_EQUAL,0x0,0xf);
   glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
   glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
   glDisable(GL_TEXTURE_1D);
   glDisableClientState(GL_VERTEX_ARRAY);

   return(1);
}

int Data_RenderStream3D(TData *Field,ViewportItem *VP,Projection *Proj){

   int    i,j,k,k0,k1;
   int    n,b,f,c,len,dz,idx;
   float  *map=NULL;

   extern Vect3d GDB_VBuf[];

   if (GLRender->Resolution>2) {
      return(0);
   }

   if (!Field->Ref || !Field->Ref->Pos || !Field->Def->Data[2] || !Field->Spec->Outline) {
      return(0);
   }

   if (!Field->Map) {
      Field->Map=(float*)malloc(sizeof(float)*2*STREAMLEN);
   }

   /*Setup 1D Texture*/
   if (Field->Spec->MapAll) {
      if (Field->Spec->Map->Alpha) {
         glEnable(GL_BLEND);
      }
      map=Field->Map;
      glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,Field->Spec->Map->NbPixels,0,GL_RGBA,GL_UNSIGNED_BYTE,Field->Spec->Map->Color);
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP);
      glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_REPLACE);
   } else {
      glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,256,0,GL_RGBA,GL_UNSIGNED_BYTE,StreamMap);
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);
      glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL);
      for(len=0;len<STREAMLEN<<1;len++) {
         Field->Map[len]=(float)len/(STREAMLEN<<1);
      }
   }
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
   glEnable(GL_TEXTURE_1D);

   glEnableClientState(GL_VERTEX_ARRAY);
   glColor3us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue);

   /*Do we need transparency*/
   if (Field->Spec->MapAll && Field->Spec->Map->Alpha) {
      glEnable(GL_BLEND);
   }

   len=STREAMLEN;
   dz=Field->Spec->Sample;
   glLineWidth((float)Field->Spec->Width);
   glMatrixMode(GL_TEXTURE);

   /*Recuperer les latlon des pixels sujets*/
   if (Field->Spec->PosNb) {
      for(n=0;n<Field->Spec->PosNb;n++) {
         i=Field->Spec->Pos[n][0];
         j=Field->Spec->Pos[n][1];
         k=Field->Spec->Pos[n][2];

         /*Get the streamline */
         b=FFStreamLine(Field->Ref,Field->Def,VP,GDB_VBuf,map,i,j,k,len,-Field->Spec->Step,Field->Spec->Min,0,REF_PROJ,1);
         f=FFStreamLine(Field->Ref,Field->Def,VP,&GDB_VBuf[len],(map?&map[len]:map),i,j,k,len,Field->Spec->Step,Field->Spec->Min,0,REF_PROJ,1);
         /* If we have at least some part of it */
         if (b+f>2) {
            if (map) {
               for(c=0;c<b+f;c++) {
                  VAL2COL(idx,Field->Spec,Field->Map[c]);
                  Field->Map[c]=idx/(float)Field->Spec->Map->NbPixels;
               }
            }
            if (!map) {
               glPushMatrix();
               glScalef((float)(len<<1)/(b+f),1.0,1.0);
               Field->Spec->TexStep+=0.000025;
               Field->Spec->TexStep=Field->Spec->TexStep>1.0?0.0:Field->Spec->TexStep;
               glTranslatef(-Field->Spec->TexStep,0.0,0.0);
            }
            Proj->Type->Render(Proj,0,&GDB_VBuf[len-b],NULL,NULL,Field->Map,GL_LINE_STRIP,b+f,NULL,NULL);
            if (!map) {
               glPopMatrix();
            }
         }
      }
   } else {
      for(i=Field->Spec->Cube[0];i<=Field->Spec->Cube[3];i+=dz) {
         for(j=Field->Spec->Cube[1];j<=Field->Spec->Cube[4];j+=dz) {
            for(k=Field->Spec->Cube[2];k<=Field->Spec->Cube[5];k+=dz) {

               /*Get the streamline */
               f=FFStreamLine(Field->Ref,Field->Def,VP,GDB_VBuf,map,i,j,k,len<<1,Field->Spec->Step,Field->Spec->Min,0,REF_PROJ,1);

               /* If we have at least some part of it */
               if (f>2) {
                  if (map) {
                     for(c=0;c<f;c++) {
                        VAL2COL(idx,Field->Spec,Field->Map[c]);
                        Field->Map[c]=idx/(float)Field->Spec->Map->NbPixels;
                     }
                  }
                  if (!map) {
                     glPushMatrix();
                     glScalef((float)(len<<1)/f,1.0,1.0);
                     if (GLRender->Delay<2000) {
                        Field->Spec->TexStep+=0.000025;
                        Field->Spec->TexStep=Field->Spec->TexStep>1.0?0.0:Field->Spec->TexStep;
                        glTranslatef(-Field->Spec->TexStep,0.0,0.0);
                     }
                  }
                  Proj->Type->Render(Proj,0,GDB_VBuf,NULL,NULL,Field->Map,GL_LINE_STRIP,f,NULL,NULL);
                  if (!map) {
                     glPopMatrix();
                  }
               }
            }
         }
      }
   }

   glDisable(GL_TEXTURE_1D);
   glDisable(GL_CULL_FACE);
   glDisableClientState(GL_VERTEX_ARRAY);
   glEnable(GL_BLEND);
   glColor4us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue,7500);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

   Field->Spec->Cube[0]=Field->Spec->Cube[0]<Field->Ref->X0?Field->Ref->X0:Field->Spec->Cube[0]>Field->Ref->X1?Field->Ref->X1:Field->Spec->Cube[0];
   Field->Spec->Cube[3]=Field->Spec->Cube[3]<Field->Ref->X0?Field->Ref->X0:Field->Spec->Cube[3]>Field->Ref->X1?Field->Ref->X1:Field->Spec->Cube[3];
   Field->Spec->Cube[1]=Field->Spec->Cube[1]<Field->Ref->Y0?Field->Ref->Y0:Field->Spec->Cube[1]>Field->Ref->Y1?Field->Ref->Y1:Field->Spec->Cube[1];
   Field->Spec->Cube[4]=Field->Spec->Cube[4]<Field->Ref->Y0?Field->Ref->Y0:Field->Spec->Cube[4]>Field->Ref->Y1?Field->Ref->Y1:Field->Spec->Cube[4];
   Field->Spec->Cube[2]=Field->Spec->Cube[2]<Field->Ref->Z0?Field->Ref->Z0:Field->Spec->Cube[2]>Field->Ref->Z1?Field->Ref->Z1:Field->Spec->Cube[2];
   Field->Spec->Cube[5]=Field->Spec->Cube[5]<Field->Ref->Z0?Field->Ref->Z0:Field->Spec->Cube[5]>Field->Ref->Z1?Field->Ref->Z1:Field->Spec->Cube[5];

   k0=Field->Spec->Cube[2];
   k1=Field->Spec->Cube[5];
   glBegin(GL_QUADS);

      /*Bottom*/
      idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[0];
      glVertex3dv(Field->Ref->Pos[k0][idx]);
      idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[0];
      glVertex3dv(Field->Ref->Pos[k0][idx]);
      idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[3];
      glVertex3dv(Field->Ref->Pos[k0][idx]);
      idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[3];
      glVertex3dv(Field->Ref->Pos[k0][idx]);

      if (k0!=k1) {
         if (Field->Spec->Cube[1]!=Field->Spec->Cube[4]) {
            /*Left*/
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->Ref->Pos[k0][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->Ref->Pos[k0][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->Ref->Pos[k1][idx]);
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->Ref->Pos[k1][idx]);

            /*Right*/
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->Ref->Pos[k0][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->Ref->Pos[k0][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->Ref->Pos[k1][idx]);
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->Ref->Pos[k1][idx]);
         }


         if (Field->Spec->Cube[0]!=Field->Spec->Cube[3]) {
            /*Up*/
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->Ref->Pos[k0][idx]);
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->Ref->Pos[k0][idx]);
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->Ref->Pos[k1][idx]);
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->Ref->Pos[k1][idx]);

            /*Down*/
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->Ref->Pos[k0][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->Ref->Pos[k0][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->Ref->Pos[k1][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->Ref->Pos[k1][idx]);
         }

         /*Top*/
         if (Field->Spec->Cube[0]!=Field->Spec->Cube[3] && Field->Spec->Cube[1]!=Field->Spec->Cube[4]) {
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->Ref->Pos[k1][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->Ref->Pos[k1][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->Ref->Pos[k1][idx]);
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->Ref->Pos[k1][idx]);
         }
      }
   glEnd();
   glDisable(GL_BLEND);
   glEnable(GL_CULL_FACE);

   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderMesh>
 * Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue les calculs et l'affichage pour le type triangle mesh .
 *
 * Parametres :
 *  <Champs>  : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_RenderMesh(TData *Field,ViewportItem *VP,Projection *Proj) {

   int     n,idxk;
   Vect3d  b,p,p0,p1,p2;
   Vect3d *pos;

   if (!Field->Ref || !Field->Ref->Pos) {
      return;
   }

   /*Do we need transparency*/
   if (Field->Spec->Map->Alpha || Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }

   /*Setup 1D Texture*/
   glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,Field->Spec->Map->NbPixels,0,GL_RGBA,GL_UNSIGNED_BYTE,Field->Spec->Map->Color);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
   glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_REPLACE);
   glEnable(GL_TEXTURE_1D);

   pos=Field->Ref->Pos[Field->Def->Level];

   if (!Field->Map)
      FSTD_DataMap(Field,False);

   glLineWidth(1.0);
   if (Field->Spec->InterpDegree[0]=='L') {
      glEnableClientState(GL_VERTEX_ARRAY);
      Proj->Type->Render(Proj,0,pos,Field->Ref->Idx,NULL,Field->Map,GL_TRIANGLES,Field->Ref->NIdx,NULL,NULL);
      glDisableClientState(GL_VERTEX_ARRAY);
   } else {
      glBegin(GL_TRIANGLES);
      for(n=0;n<Field->Ref->NIdx-3;n+=3) {
         Vect_Init(b,1.0/3.0,1.0/3.0,1.0/3.0);
         Bary_Interp(b,p,pos[Field->Ref->Idx[n]],pos[Field->Ref->Idx[n+1]],pos[Field->Ref->Idx[n+2]]);
         Vect_Init(b,0.0,0.5,0.5);
         Bary_Interp(b,p0,pos[Field->Ref->Idx[n]],pos[Field->Ref->Idx[n+1]],pos[Field->Ref->Idx[n+2]]);
         Vect_Init(b,0.5,0.0,0.5);
         Bary_Interp(b,p1,pos[Field->Ref->Idx[n]],pos[Field->Ref->Idx[n+1]],pos[Field->Ref->Idx[n+2]]);
         Vect_Init(b,0.5,0.5,0.0);
         Bary_Interp(b,p2,pos[Field->Ref->Idx[n]],pos[Field->Ref->Idx[n+1]],pos[Field->Ref->Idx[n+2]]);

         glTexCoord1f(Field->Map[Field->Ref->Idx[n]]);
         glVertex3dv(pos[Field->Ref->Idx[n]]);
         glVertex3dv(p);
         glVertex3dv(p1);
         glVertex3dv(pos[Field->Ref->Idx[n]]);
         glVertex3dv(p);
         glVertex3dv(p2);

         glTexCoord1f(Field->Map[Field->Ref->Idx[n+1]]);
         glVertex3dv(pos[Field->Ref->Idx[n+1]]);
         glVertex3dv(p);
         glVertex3dv(p0);
         glVertex3dv(pos[Field->Ref->Idx[n+1]]);
         glVertex3dv(p);
         glVertex3dv(p2);

         glTexCoord1f(Field->Map[Field->Ref->Idx[n+2]]);
         glVertex3dv(pos[Field->Ref->Idx[n+2]]);
         glVertex3dv(p);
         glVertex3dv(p0);
         glVertex3dv(pos[Field->Ref->Idx[n+2]]);
         glVertex3dv(p);
         glVertex3dv(p1);
      }
      glEnd();
   }
   glDisable(GL_TEXTURE_1D);
   glEnable(GL_CULL_FACE);
   glDisable(GL_BLEND);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderTexture>
 * Creation : Septembre 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage de la texture du champs en
 *            Adaptive Meshing.
 *
 * Parametres  :
 *  <Field>    : Champs
 *  <VP>       : Parametres du viewport
 *  <Proj>     : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_RenderTexture(TData *Field,ViewportItem *VP,Projection *Proj){

   int    i,j,col0,col1,col2,col3,idxk,idx0,idx1,idx2,idx3;
   int    ri,rj,ox=0,oi,base=0;
   int    depth;
   double val0,val1,val2,val3;
   Vect3d g0,g1,g2,g3,dim,*pos;
   double dx,dy;

   if (GLRender->Resolution>2) {
      return(0);
   }

   if (!Field->Ref || !Field->Ref->Pos || Field->Ref->Grid[0]=='X' || Field->Ref->Grid[0]=='Y') {
      return(0);
   }

   if (Proj->Type->Def==PROJCYLIN && Field->Ref->Grid[0]!='V') {
      glEnable(GL_CULL_FACE);
   } else {
      glDisable(GL_CULL_FACE);
   }

   if (GLRender->GLDebug) {
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
   } else {
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   }

   /*Afficher les points*/
   if (Field->Ref->Grid[0]=='M') {
      Data_RenderMesh(Field,VP,Proj);
      return(1);
   }

   /*Do we need transparency*/
   if (Field->Spec->Map->Alpha || Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }

   pos=Field->Ref->Pos[Field->Def->Level];

   if (Field->Spec->InterNb || Proj->Type->Def==PROJPLANE || Field->Ref->Grid[0]=='W') {
      ri=1;
      rj=1;
   } else {
      rj=ceil(2.0/FFCellResolution(VP,Proj,pos[Field->Def->NJ/2*Field->Def->NI+Field->Def->NI/2],pos[(Field->Def->NJ/2+1)*Field->Def->NI+Field->Def->NI/2]));
      rj=rj>7?7:rj;
   }

   if (Field->Ref->Grid[0]=='V') {
     ri=1;
   } else {
     ri=rj;
   }

   if (ri==0 || rj==0) {
      return(0);
   }

   if (Field->Spec->InterNb) {
      VAL2COL(base,Field->Spec,Field->Spec->Inter[0]);
   }

   /*Grille avec loop sur la longitude*/
   if (Field->Ref->Type&GRID_WRAP && Proj->Type->Def!=PROJPLANE) {
      ox=1;
   }
   idxk=FSIZE2D(Field->Def)*Field->Def->Level;

   /*Process gridpoints*/
   for(j=0;j<Field->Def->NJ-rj;j+=rj) {

      glBegin(GL_QUADS);

      for(i=0;i<Field->Def->NI-ri+ox;i+=ri) {

         oi=(i+ri)%Field->Def->NI;
         idx0=j*Field->Def->NI+i;
         idx3=(j+rj)*Field->Def->NI+i;

         /*Si l'increment fait manquer le dernier point*/
         if ((i+ri)>=(Field->Def->NI-ri+ox)) {
            oi=(Field->Def->NI+ox-1)%Field->Def->NI;
         }
         idx1=j*Field->Def->NI+oi;
         idx2=(j+rj)*Field->Def->NI+oi;

         Def_GetMod(Field->Def,idxk+idx0,val0);
         Def_GetMod(Field->Def,idxk+idx1,val1);
         Def_GetMod(Field->Def,idxk+idx2,val2);
         Def_GetMod(Field->Def,idxk+idx3,val3);

         VAL2COL(col0,Field->Spec,val0);
         VAL2COL(col1,Field->Spec,val1);
         VAL2COL(col2,Field->Spec,val2);
         VAL2COL(col3,Field->Spec,val3);

         /* Is the cell valid ??? */
         if (col0>-1 || col1>-1 || col2>-1 || col3>-1) {

            Vect_Assign(g0,pos[idx0]);
            Vect_Assign(g1,pos[idx1]);
            Vect_Assign(g2,pos[idx2]);
            Vect_Assign(g3,pos[idx3]);

            /* Is the cell visible ??? */
            if (FFCellProcess(VP,Proj,g0,g1,g2,g3,dim)) {

               if (Field->Spec->InterpDegree[0]=='N') {
                  VertexQuad_Nearest(Field,g0,g1,g2,g3,col0,col1,col2,col3,base);
               } else {

                  dx=ABS(dim[0]);
                  dy=ABS(dim[1]);
                  dx=MAX(dx,dy);
                  depth=1;
                  while (dx>2) {
                     depth++;
                     dx*=0.5;
                  }
                  if (!Field->Spec->InterNb && depth>1) {
                     depth--;
                  }

                  /* Is the cell resolution enough ??? */
                  if (depth>=2 && (ABS(col0-col1)>1 || ABS(col1-col2)>1 || ABS(col2-col3)>1 || ABS(col3-col0)>1)) {
//                     depth>>=1;
                     VertexQuad_Linear(Field,g0,g1,g2,g3,col0,col1,col2,col3,val0,val1,val2,val3,depth,base);
                  } else {
                     VR(g0,col0,base);
                     VR(g1,col1,base);
                     VR(g2,col2,base);
                     VR(g3,col3,base);
                  }
               }
            }
         }
      }
      glEnd();
   }

   glEnable(GL_CULL_FACE);
   glDisable(GL_BLEND);
   return 1;
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderValue>
 * Creation : Octobre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage des bas et des hauts.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Field>   : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *  <Tile>    : Dimension des tuiles d'evaluation
 *
 * Retour:
 *
 * Remarques :
 *     -base sur l'algorithme de Michel Grenier
 *
 *----------------------------------------------------------------------------
*/
void Data_RenderValue(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,Projection *Proj,int Tile){

   double zm,zmm,zn,zv;
   int    high;
   int    ichk,jchk;
   int    is,js,jt,it;
   int    ip,jp,ik,jk;

   int    idx;
   Vect3d pos,g,*posa;
   char   lbl[10];

   if (GLRender->Resolution>1 || !Field->Ref->Pos || !Field->Spec->Font || !Field->Spec->Outline) {
      return;
   }

   Projection_UnClip(Proj);

   glMatrixMode(GL_PROJECTION);
   glPushMatrix();
   glLoadIdentity();
   gluOrtho2D(0,VP->Width,0,VP->Height);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glLoadIdentity();

   glDisable(GL_STENCIL_TEST);
   glColor3us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue);
   glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),Field->Spec->Font);

   posa=Field->Ref->Pos[Field->Def->Level];

   /*Min Max case*/
   if (Tile==1) {
     if (Projection_Pixel(Proj,VP,Field->Stat->MaxLoc,pos)) {
         DataSpec_Format(Field->Spec,VAL2SPEC(Field->Spec,Field->Stat->Max),lbl);
         Data_RenderMark(Interp,Field->Spec,VP,(int)pos[0],(int)pos[1],"Max",lbl);
      }

      if (Projection_Pixel(Proj,VP,Field->Stat->MinLoc,pos)) {
         DataSpec_Format(Field->Spec,VAL2SPEC(Field->Spec,Field->Stat->Min),lbl);
         Data_RenderMark(Interp,Field->Spec,VP,(int)pos[0],(int)pos[1],"Min",lbl);
      }
   } else {

      Tile-=1;

      /* ichk and jchk indicates the covering area under which the extrema is to be evaluated */
      ichk=Field->Def->NI/Tile;ichk=(2>=ichk?2:(2*Tile-1<=ichk?2*Tile-1:ichk));
      jchk=Field->Def->NJ/Tile;jchk=(2>=jchk?2:(2*Tile-1<=jchk?2*Tile-1:jchk));

      /* loop in all the lines */
      for (ip=0;ip<Field->Def->NI;ip++) {

         /* as long as the function is increasing or decreasing move along the y axe */
         Def_GetMod(Field->Def,ip,zm);
         idx=ip+1*Field->Def->NI;
         Def_GetMod(Field->Def,idx,zmm);
         high=(zm<=zmm);
         for (jp=0;jp<Field->Def->NJ-1;jp++) {
            if (jp!=Field->Def->NJ-1) {
               idx=ip+(jp+1)*Field->Def->NI;
               Def_GetMod(Field->Def,idx,zn);
               if (zm==zn || ((zm<zn) && high) || ((zm>zn) && !high)) {
                  zm=zn;
                  continue;
               }
            }

            /* check if it is really a max or a min over the area */
            js=(0>=jp-jchk?0:jp-jchk);
            jt=(Field->Def->NJ-1<=jp+jchk?Field->Def->NJ-1:jp+jchk);
            is=(0>=ip-ichk?0:ip-ichk);
            it=(Field->Def->NI-1<=ip+ichk?Field->Def->NI-1:ip+ichk);

            for (jk=js;jk<=jt;jk++) {
               for (ik=is;ik<=it;ik++) {
                  if (ik!=ip || jk!=jp) {
                     idx=ik+jk*Field->Def->NI;
                     Def_GetMod(Field->Def,idx,zv);
                     if (((zv>=zm) && high) || ((zv<=zm) && !high)) goto nexty;
                  }
               }
            }

            /* an extrema was found */
            idx=jp*Field->Def->NI+ip;
            Vect_Assign(g,posa[idx]);
            PROJCHECK(Proj,g[0]);
            gluProject(g[0],posa[idx][1],posa[idx][2],VP->GLModR,VP->GLProj,VP->GLView,&pos[0],&pos[1],&pos[2]);
            if (VIN(pos[0],1,Proj->Params->VP->Width) && VIN(pos[1],1,Proj->Params->VP->Height) && VIN(pos[2],0,1)) {
               DataSpec_Format(Field->Spec,VAL2SPEC(Field->Spec,zm),lbl);
               if (high) {
                  Data_RenderMark(Interp,Field->Spec,VP,(int)pos[0],(int)pos[1],"H",lbl);
               } else {
                  Data_RenderMark(Interp,Field->Spec,VP,(int)pos[0],(int)pos[1],"L",lbl);
               }
            }
            /* continues the search */
            nexty: high=!high;
            zm=zn;
         }
      }
   }
   Projection_Clip(Proj);

   glPopMatrix();
   glMatrixMode(GL_PROJECTION);
   glPopMatrix();
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderVector>
 * Creation : Avril 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue les calculs et l'affichage d'une partie du champ en
 *            parallele sur plusieur processeurs pour le type vectoriel.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Field>   : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/

void Data_RenderVector(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,Projection *Proj) {

   float   *lat,*lon,*x,*y,size,theta=0.0;
   double  len,u,v,w,i0,j0,i1,j1;
   Vect3d  pix;
   Coord   coo;
   int     n=0,mem,i,j,idx,dz;
   char    buf[32];

   if (!Field->Ref || !Field->Ref->Pos || !Field->Def->Data[1] || !Field->Spec->Outline)
      return;

   /*Calculer la dimension generale*/
   size=VP->Ratio*Field->Spec->Size;
   dz=Field->Spec->Sample*10;

   /*Afficher toutes les barbules*/
   glMatrixMode(GL_MODELVIEW);
   glDisable(GL_CULL_FACE);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   glLineWidth(Field->Spec->Width);
   glColor3us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue);

   if (Field->Spec->MapAll && Field->Spec->Map->Alpha) {
      glEnable(GL_BLEND);
   }

   if (Interp) {
      sprintf(buf,"%i\n",Field->Spec->Width-1);
      Tcl_AppendResult(Interp,"%% Postscript des donnees vectorielles\n",buf," setlinewidth 0 setlinecap 0 setlinejoin\n",(char*)NULL);
      Tk_CanvasPsColor(Interp,VP->canvas,Field->Spec->Outline);
   }

   switch(Field->Ref->Grid[0]) {

      case 'V':
         if (Field->Def->Data[2]) {
            for(i=0;i<Field->Def->NI;i+=Field->Spec->Sample) {
               if (i+1<Field->Def->NI) {
                  if (Field->Ref->RefFrom) {
                     Field->Ref->RefFrom->UnProject(Field->Ref->RefFrom,&i0,&j0,Field->Ref->Lat[i],Field->Ref->Lon[i],1,1);
                     Field->Ref->RefFrom->UnProject(Field->Ref->RefFrom,&i1,&j1,Field->Ref->Lat[i+1],Field->Ref->Lon[i+1],1,1);
                     theta=90.0+RAD2DEG(atan2(i1-i0,j1-j0));
                  } else {
                     theta=90.0;
                  }
               }
               for(j=0;j<Field->Def->NJ;j+=Field->Spec->Sample) {
                  idx=j*Field->Def->NI+i;
                  Def_Get(Field->Def,0,idx,u);
                  Def_Get(Field->Def,2,idx,w);
                  Def_GetMod(Field->Def,idx,len);
                  if (len<=Field->Spec->Max && len>=Field->Spec->Min) {
                     if (Field->Spec->MapAll) {
                        VAL2COL(idx,Field->Spec,len);
                        if (Interp) {
                           CMap_PostscriptColor(Interp,Field->Spec->Map,idx);
                        } else {
                           glColor4ubv(Field->Spec->Map->Color[idx]);
                        }
                     }
                     size=VP->Ratio*VECTORSIZE(Field->Spec,len);
                     if (Interp) glFeedbackInit(256,GL_2D);
                     Data_RenderBarbule(Field->Spec->RenderVector,0,theta,Field->Ref->Lat[i],Field->Ref->Lon[i],Data_Level2Meter(Field->Ref->LevelType,Field->Ref->Levels[j]),len,RAD2DEG(atan2(u,w)),size,Proj);
                     if (Interp) glFeedbackProcess(Interp,GL_2D);
                  }
               }
            }
         }
         break;

      case 'W':
         for(i=0;i<Field->Def->NI;i+=Field->Spec->Sample) {
            for(j=0;j<Field->Def->NJ;j+=Field->Spec->Sample) {
               idx=j*Field->Def->NI+i;
               Def_Get(Field->Def,0,idx,u);
               Def_Get(Field->Def,1,idx,v);
               Def_GetMod(Field->Def,idx,len);
               Field->Ref->Project(Field->Ref,i,j,&coo.Lat,&coo.Lon,1,1);
               if (len<=Field->Spec->Max && len>=Field->Spec->Min) {
                  if (Field->Spec->MapAll) {
                     VAL2COL(idx,Field->Spec,len);
                     if (Interp) {
                        CMap_PostscriptColor(Interp,Field->Spec->Map,idx);
                     } else {
                        glColor4ubv(Field->Spec->Map->Color[idx]);
                     }
                  }
                  size=VP->Ratio*VECTORSIZE(Field->Spec,len);
                  if (Interp) glFeedbackInit(256,GL_2D);
                  Data_RenderBarbule(Field->Spec->RenderVector,0,0.0,coo.Lat,coo.Lon,Data_Level2Meter(Field->Ref->LevelType,Field->Ref->Levels[0]),len,180+RAD2DEG(atan2(u,v)),size,Proj);
                  if (Interp) glFeedbackProcess(Interp,GL_2D);
               }
            }
         }
         break;

      default:
         if (Field->Ref->Id<0)
            return;

         /*Allouer la memoire pour les donnees*/
         mem=(VP->Width*VP->Height)/dz*sizeof(float);
         lat=(float*)malloc(mem);
         lon=(float*)malloc(mem);

         /*Recuperer les latlon des pixels sujets*/
         for (pix[0]=0;pix[0]<VP->Width;pix[0]+=dz) {
            for (pix[1]=0;pix[1]<VP->Height;pix[1]+=dz) {

               Proj->Type->UnProject(VP,Proj->Params,&coo,pix);
               if (coo.Lat!=-999.0) {
                  lat[n]=coo.Lat;
                  lon[n]=(coo.Lon<0?coo.Lon+360:coo.Lon);
                  n++;
               }
            }
         }

         /*Allouer l'espaces memoire pour les retours d'ezscint*/
         mem=n*sizeof(float);
         x=(float*)malloc(mem);
         y=(float*)malloc(mem);

#ifdef LNK_FSTD
         /*Recuperer les informations sur les vents et leurs localisations*/
         c_gdxyfll(Field->Ref->Id,x,y,lat,lon,n);

         mem=0;i=0;
         while (mem<n) {
            if (x[mem]<=Field->Def->NI && y[mem]<=Field->Def->NJ && x[mem]>=1 && y[mem]>=1) {
               lat[i]=lat[mem];
               lon[i]=lon[mem];
               i++;
            }
            mem++;
         }
         n=i;

         mem=FSIZE2D(Field->Def)*Field->Def->Level;

         c_ezsetopt("INTERP_DEGREE",Field->Spec->InterpDegree);

         if (Field->Spec->GeoVector) {
            c_gdllwdval(Field->Ref->Id,x,y,&Field->Def->Data[0][mem],&Field->Def->Data[1][mem],lat,lon,n);
         } else {
            c_gdllvval(Field->Ref->Id,x,y,&Field->Def->Data[0][mem],&Field->Def->Data[1][mem],lat,lon,n);
         }
#endif
         while (n--) {
            if (Field->Spec->GeoVector) {
               if (x[n]<=Field->Spec->Max && x[n]>=Field->Spec->Min) {
                  if (Field->Spec->MapAll) {
                     VAL2COL(idx,Field->Spec,x[n]);
                     if (Interp) {
                        CMap_PostscriptColor(Interp,Field->Spec->Map,idx);
                     } else {
                        glColor4ubv(Field->Spec->Map->Color[idx]);
                     }
                  }
                  size=VP->Ratio*VECTORSIZE(Field->Spec,x[n]);
                  if (Interp) glFeedbackInit(256,GL_2D);
                  Data_RenderBarbule(Field->Spec->RenderVector,0,0.0,lat[n],lon[n],Data_Level2Meter(Field->Ref->LevelType,Field->Ref->Levels[0]),x[n],y[n],size,Proj);
                  if (Interp) glFeedbackProcess(Interp,GL_2D);
               }
            } else {
               len=hypot(x[n],y[n]);
               if (len<=Field->Spec->Max && len>=Field->Spec->Min) {
                  if (Field->Spec->MapAll) {
                     VAL2COL(idx,Field->Spec,len);
                     if (Interp) {
                        CMap_PostscriptColor(Interp,Field->Spec->Map,idx);
                     } else {
                        glColor4ubv(Field->Spec->Map->Color[idx]);
                     }
                  }
                  size=VP->Ratio*VECTORSIZE(Field->Spec,len);
                  if (Interp) glFeedbackInit(256,GL_2D);
                  Data_RenderBarbule(Field->Spec->RenderVector,0,0.0,lat[n],lon[n],Data_Level2Meter(Field->Ref->LevelType,Field->Ref->Levels[0]),len,180+RAD2DEG(atan2(x[n],y[n])),size,Proj);
                  if (Interp) glFeedbackProcess(Interp,GL_2D);
               }
            }
         }

         /*Liberer l'espace memoire temporaire*/
         free(lat);free(lon);
         free(x);free(y);
   }

   glDisable(GL_POLYGON_OFFSET_LINE);
   glDisable(GL_POLYGON_OFFSET_FILL);
   glEnable(GL_CULL_FACE);
   glDisable(GL_BLEND);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderVolume>
 * Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage du volume des contours du champs.
 *
 * Parametres :
 *  <Field>   : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_RenderVolume(TData *Field,ViewportItem *VP,Projection *Proj){

   int i,idx,len;
   TList  *list,*end;
   TArray *array;

   if (Field->Def->NK<=1) {
      return(0);
   }

  /*Creer la liste de vertex par niveaux*/
   if (!Field->Segments) {
      for (i=0;i<Field->Spec->InterNb;i++) {
         len=FFMarchingCube(Field->Ref,Field->Def,Proj,Field->Spec->Inter[i],NULL,0);
         if (len>1) {
            array=TArray_Alloc(Field->Spec->Inter[i],2*len);
            if (array) {
               Field->Segments=TList_Add(Field->Segments,array);
               FFMarchingCube(Field->Ref,Field->Def,Proj,Field->Spec->Inter[i],array->Data,1);
            } else {
               fprintf(stderr,"(ERROR) Data_RenderVolume: Unable to alloc memory for volume data %f",Field->Spec->Inter[i]);
            }
         }
      }
   }

   if (Field->Segments) {

      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);
      glEnable(GL_COLOR_MATERIAL);
      glEnable(GL_DEPTH_TEST);
      glDisable(GL_STENCIL_TEST);
      glEnableClientState(GL_VERTEX_ARRAY);
      glEnableClientState(GL_NORMAL_ARRAY);
      glLineWidth(1.0);

      if (Field->Spec->RenderTexture) {
         glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      } else {
         glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
      }

      /*Gerer l'effet de transparence*/
      if (Field->Spec->Map->Alpha) {
         glEnable(GL_BLEND);
         glEnable(GL_CULL_FACE);
         glCullFace(GL_FRONT);

         list=Field->Segments;
         while(list) {
            end=list;
            list=list->Next;
         }
         list=end;
         while(list) {

            array=(TArray*)list->Data;

            VAL2COL(idx,Field->Spec,array->Value);
            glColor4ubv(Field->Spec->Map->Color[idx]);
            glVertexPointer(3,GL_DOUBLE,2*sizeof(Vect3d),array->Data[1]);
            glNormalPointer(GL_DOUBLE,2*sizeof(Vect3d),array->Data);
            glDrawArrays(GL_TRIANGLES,0,array->Size>>1);

            list=list->Prev;
         }

         glCullFace(GL_BACK);
         list=Field->Segments;
         while(list) {

            array=(TArray*)list->Data;

            VAL2COL(idx,Field->Spec,array->Value);
            glColor4ubv(Field->Spec->Map->Color[idx]);
            glVertexPointer(3,GL_DOUBLE,2*sizeof(Vect3d),array->Data[1]);
            glNormalPointer(GL_DOUBLE,2*sizeof(Vect3d),array->Data);
            glDrawArrays(GL_TRIANGLES,0,array->Size>>1);

            list=list->Next;
         }
         glDisable(GL_BLEND);
      } else {
         glDisable(GL_CULL_FACE);

         list=Field->Segments;
         while(list) {
            array=(TArray*)list->Data;

            VAL2COL(idx,Field->Spec,array->Value);
            glColor4ubv(Field->Spec->Map->Color[idx]);
            glVertexPointer(3,GL_DOUBLE,2*sizeof(Vect3d),array->Data[1]);
            glNormalPointer(GL_DOUBLE,2*sizeof(Vect3d),array->Data);
            glDrawArrays(GL_TRIANGLES,0,array->Size>>1);

            list=list->Next;
         }
      }

      glEnable(GL_CULL_FACE);
      glCullFace(GL_BACK);
      glDisableClientState(GL_VERTEX_ARRAY);
      glDisableClientState(GL_NORMAL_ARRAY);
      glDisable(GL_LIGHTING);
      glDisable(GL_LIGHT0);
      glDisable(GL_COLOR_MATERIAL);
      glEnable(GL_STENCIL_TEST);
   }

   return 1;
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_RenderRange>
 * Creation : Septembre 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'affichage des cercle de distances pour les grilles radar.
 *
 * Parametres :
 *  <Field>   : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_RenderRange(TData *Field,ViewportItem *VP,Projection *Proj){

   int     i,j,r;
   char    buf[128];
   double  h;
   Coord   loc;

   if (Field->Ref->Grid[0]!='R' || !Field->Spec->Outline) {
      return(0);
   }

   if (!Field->Spec->RenderVol) {
      glDisable(GL_DEPTH_TEST);
   }
   glMatrixMode(GL_MODELVIEW);
   glDisable(GL_CULL_FACE);
   glDisable(GL_STENCIL_TEST);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),Field->Spec->Font);
   glLineWidth(2.0);
   glColor3us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue);

   /*Affichage des labels*/
   for(r=0;r<Field->Spec->RangeNb;r++) {
      glPushMatrix();
      Field->Ref->Project(Field->Ref,1,Field->Spec->Range[r]/Field->Ref->ResR,&loc.Lat,&loc.Lon,1,1);
      Proj->Type->Locate(Proj,loc.Lat,loc.Lon,1);

      h=sin(DEG2RAD(Field->Ref->Levels[0]))*Field->Spec->Range[r];

      glTranslated(0.0,0.0,ZM(Proj,h));
      glScalef(VP->Ratio,VP->Ratio,1.0);
      sprintf(buf,"%.0f Km",Field->Spec->Range[r]/1000.0);
      glTranslated(0.0,5.0,0.0);
      glPrint(NULL,VP->canvas,buf,0,0,0);
      glPopMatrix();

/*
      glPushMatrix();
      Field->Ref->Project(Field->Ref,1,-Field->Spec->Range[r],&loc.lat,&loc.lon,1,1);
      Proj->Type->Locate(Proj,loc.lat,loc.lon,1);

      h=sin(DEG2RAD(Field->Ref->Levels[Field->Def->Level]))*Field->Spec->Range[r];
      glTranslated(0.0,0.0,ZM(Proj,h));
      glScalef(VP->Ratio,VP->Ratio,Proj->Params->ZFactor);
      sprintf(buf,"%.0f m",h);
      glTranslated(0.0,-5.0,0.0);
      glPrint(NULL,VP->canvas,buf,0,0,0);
      glPopMatrix();
*/
   }

   /*Affichage des cercles de ranges*/
   for(r=0;r<Field->Spec->RangeNb;r++) {
      j=Field->Spec->Range[r]/Field->Ref->ResR;

      glDisable(GL_BLEND);
      glBegin(GL_LINE_STRIP);
      for(i=0;i<Field->Def->NI;i++) {
         glVertex3dv(Field->Ref->Pos[Field->Def->Level][FIDX2D(Field->Def,i,j)]);
      }
      glVertex3dv(Field->Ref->Pos[Field->Def->Level][FIDX2D(Field->Def,0,j)]);
      glEnd();

      if (Field->Spec->RenderVol) {
         glBegin(GL_LINE_STRIP);
         for(i=0;i<Field->Def->NI;i++) {
            glVertex3dv(Field->Ref->Pos[0][FIDX2D(Field->Def,i,j)]);
         }
         glVertex3dv(Field->Ref->Pos[0][FIDX2D(Field->Def,0,j)]);
         glEnd();

         glEnable(GL_BLEND);
         glColor4us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue,10000);

         glBegin(GL_QUAD_STRIP);
         for(i=0;i<Field->Def->NI;i++) {
            glVertex3dv(Field->Ref->Pos[Field->Def->Level][FIDX2D(Field->Def,i,j)]);
            glVertex3dv(Field->Ref->Pos[0][FIDX2D(Field->Def,i,j)]);
         }
         glVertex3dv(Field->Ref->Pos[Field->Def->Level][FIDX2D(Field->Def,0,j)]);
         glVertex3dv(Field->Ref->Pos[0][FIDX2D(Field->Def,0,j)]);
         glEnd();
      }
   }

   glEnable(GL_CULL_FACE);
   glDisable(GL_STENCIL_TEST);
   return(1);
}
