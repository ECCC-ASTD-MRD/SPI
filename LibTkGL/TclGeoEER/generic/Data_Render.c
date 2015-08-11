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

#include "App.h"
#include "tclUtils.h"
#include "RPN.h"
#include "Triangle.h"

#include "Data_FF.h"

void  Data_RenderBarbule(TDataSpecVECTOR Type,int Flip,float Axis,float Lat,float Lon,float Elev,float Speed,float Dir,float Size,Projection *Proj);
void  Data_RenderMark(Tcl_Interp *Interp,TDataSpec *Spec,ViewportItem *VP,int X,int Y,char *Id,char* Val);

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
void  Data_MapColor(TData *Field,int Idx);
int   Data_Grid3D(TData *Field,Projection* Proj);

extern int Data_RenderShaderParticle(TData *Field,ViewportItem *VP,Projection *Proj);
extern int Data_RenderShaderTexture(TData *Field,ViewportItem *VP,Projection *Proj);
extern int Data_RenderShaderMesh(TData *Field,ViewportItem *VP,Projection *Proj);

// Grid projection forcing wrap-over detection treshold functions
void Grid_ForceLI(Projection *Proj,float Value) {
   
   if (Proj->Ref && Proj->Ref->Type&GRID_PSEUDO) {
      Proj->TLI=Proj->LI;
      Proj->LI=Value;
   }
}
void Grid_ResetLI(Projection *Proj) {
   
   if (Proj->Ref && Proj->Ref->Type&GRID_PSEUDO) {
      Proj->LI=Proj->TLI;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_DataMap>
 * Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Precalcul des index dans la palette de couleur.
 *
 * Parametres   :
 *  <Field>     : Champs de donnees
 *  <Idx>       : Utiliser les index
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_MapColor(TData *Field,int Idx) {

   int    i;
   double v=0.0;

   if (Field->Map) free(Field->Map);

   if ((Field->Map=(float*)malloc(FSIZE2D(Field->Def)*sizeof(float)))) {

      if (Idx) {
         if (Field->GRef->Idx) free(Field->GRef->Idx);
         Field->GRef->Idx=(unsigned int*)malloc(FSIZE2D(Field->Def)*sizeof(unsigned int));
         Field->GRef->NIdx=0;
      }

      for (i=0;i<FSIZE2D(Field->Def);i++) {
         Def_GetMod(Field->Def,i,v);
         VAL2COL(Field->Map[i],Field->Spec,v);
         Field->Map[i]/=(float)Field->Spec->Map->NbPixels;

         if (Idx && Field->GRef->Idx && Field->Map[i]>=0) {
            Field->GRef->Idx[Field->GRef->NIdx++]=i;
         }
      }
   }
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

   if (Field->ZRef->LevelNb==1) {
      if (Field->ReadCube) Field->ReadCube(NULL,Field,0,0.0,0.0,NULL);
      Data_PreInit(Field);
   }

   nk=0;
   for(k=0;k<Field->ZRef->LevelNb;k++) {
      if (Field->GPos && Field->GPos->Pos[k]) {
         nk++;
      } else {
         if (Field->Grid(Field,Proj,k)) {
            nk++;
         }
      }
   }
   return(nk==Field->ZRef->LevelNb);
}

int Data_Render(Tcl_Interp *Interp,TData *Field,ViewportItem *VP,ClientData Proj,GLuint GLMode,int Mode) {

   int    nras=0,u,u0,u1,udef;

   /*Verifier l'existence du champs*/
   if (!Field || !Field->GRef || !Field->Spec || !Field->Def->Data[0]) {
      return(0);
   }

   // Check for U grid (grid index 0=loop on sub grid)
   if (Field->GRef->NbId>1) {
      if (Field->GRef->NId==0) {
         u0=1; u1=Field->GRef->NbId;
      } else {
         u0=u1=Field->GRef->NId;
      }
   } else {
      u0=u1=0;
   }
   udef=Field->GRef->NId;

   Data_PreInit(Field);

   if (!Field->Spec->Active) {
      return(0);
   }

   if (!Field->GPos || !Field->GPos->Pos[Field->Def->Level]) {
       if (!Field->Grid(Field,Proj,Field->Def->Level))
         return(0);
   }

  if (GLRender->GLZBuf) {
      glEnable(GL_DEPTH_TEST);
   }

   for(u=u0;u<=u1;u++) {
      // Point to subgrid data within global data array
      if (Field->SDef) {
         Field->GRef->NId=u;
         Field->Def=Field->SDef[Field->GRef->NId];
      }

      glPushName(PICK_FSTDFIELD);
      if (Mode==GL_ALL || Mode==GL_VECTOR) {

         if (Field->Spec->RenderGrid)
            Data_RenderGrid(Interp,Field,VP,(Projection*)Proj);

         if (Field->Spec->RenderContour && !Field->Spec->RenderVol)
            Data_RenderContour(Interp,Field,VP,(Projection*)Proj);

         if (Field->Spec->RenderVector==BARB || Field->Spec->RenderVector==SPEAR || Field->Spec->RenderVector==ARROW)
            Data_RenderVector(Interp,Field,VP,(Projection*)Proj);

         if (Field->Spec->RenderValue)
            Data_RenderValue(Interp,Field,VP,(Projection*)Proj,Field->Spec->RenderValue);
      }

      if (GLRender->GLZBuf) {
         glEnable(GL_POLYGON_OFFSET_FILL);
         glPolygonOffset(1.0,1.0);
      }

      if (Mode==GL_ALL || Mode==GL_RASTER) {

         /*Verifier la presence d'une palette de couleur si elle est necessaire*/
         if (Field->Spec->Map) {
            if (Field->Spec->RenderTexture && (!Field->Spec->RenderVol || Field->GRef->Grid[0]=='V')) {
               if (Field->GRef->Grid[0]!='Y' && Field->GRef->Grid[1]!='Y') {
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

            glEnable(GL_DEPTH_TEST);
            if (Field->Spec->RenderVector==STREAMLINE3D) {
               if (!Field->Def->Data[2] || Data_Grid3D(Field,Proj)) {
                  nras+=Data_RenderStream3D(Field,VP,(Projection*)Proj);
               }
            }

            if (Field->Spec->RenderVol) {
               if (Field->GRef->Grid[0]!='V') {
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
  }

   // Put back default grid
   if (Field->SDef) {
      Field->GRef->NId=udef;
      Field->Def=Field->SDef[Field->GRef->NId];
   }

   glDisable(GL_POLYGON_OFFSET_FILL);
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

  double y,spd,z=0;

   /*Check for unwanted speed values*/
   if (Speed>1e5)
      return;

   glPushMatrix();

   /*Si on projete en 3D, effectue les transformations necessaires*/
   if (Proj) {
      z=ZM(Proj,Elev);
      Proj->Type->Locate(Proj,Lat,Lon,1);
      glTranslated(0.0,0.0,z);
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

   switch (Type) {
      case BARB:
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
               glVertex3d(0.0,0.0,0.0);
               glVertex3d(0.0,y,0.0);
            glEnd();

            /*Vitesse >50*/
            while (spd>=50.0) {
               glBegin(GL_TRIANGLES);
               glVertex3d(0.0,y,0.0);
               glVertex3d(0.0,y+0.4,0.0);
               glVertex3d(0.7,y-0.2,0.0);
               spd-=50.0;
               y+=0.5;
               glEnd();
            }

            glBegin(GL_LINES);
            /*Vitesse >10*/
            while (spd>=10.0) {
               glVertex3d(0.0,y,0.0);
               glVertex3d(0.7,y-0.2,0.0);
               spd-=10.0;
               y+=0.25;
            }

            /*Vitesse >5*/
            if (spd>=5.0 || (Speed<5.0 && Speed>0.0)) {

               /*Positionner la ligne plus loin si c'est la seule*/
               if (Speed<10.0) {
                  y+=0.25;
               }
               glVertex3d(0.0,y,0.0);
               glVertex3d(0.4,y-0.15,0.0);
            }
            glEnd();
        }
        break;
        
      case SPEAR:
         glScalef(Size,Size,1.0);
         glBegin(GL_LINES);
            glVertex3d(0.0,-1.0,0.0);
            glVertex3d(0.0,0.0,0.0);
            glVertex3d(0.0,0.0,0.0);
            glVertex3d(-0.25,-0.25,0.0);
            glVertex3d(0.0,0.0,0.0);
            glVertex3d(0.25,-0.25,0.0);
         glEnd();
         break;
         
      case ARROW: 
         glScalef(Size,Size,1.0);
         glDrawArrow(GL_POLYGON);
         break;     

      case VNONE: 
      case STREAMLINE: 
      case STREAMLINE3D: break;
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

   int      c=0;
   char     buf[256];
   TList   *list;
   T3DArray *array;

   if (!Field->GRef || !Field->GPos || !Field->Spec->Width || (!Field->Spec->Outline && !Field->Spec->MapAll))
      return;

   /*Les contours sont-ils definit*/
   if (Field->Spec->InterNb && !Field->Def->Segments) {
      if (Field->GRef->Grid[0]=='M') {
         FFContourM(REF_PROJ,Field->GPos,Field->Def,Field->Stat,Proj,Field->Spec->InterNb,Field->Spec->Inter);
      } else {
         FFContour(REF_PROJ,Field->GPos,Field->Def,Field->Stat,Proj,Field->Spec->InterNb,Field->Spec->Inter,4-Field->Spec->RenderContour,0);
      }
   }

   /* Render the contours */
   if (Field->Def->Segments && Field->Spec->Width && Field->Spec->Outline) {

      if (Field->Spec->RenderLabel && Interp)
         Tcl_AppendResult(Interp,"gsave\n",(char*)NULL);

      if (Field->Spec->RenderLabel)
         Data_RenderLabel(Interp,Field,VP,Proj);

      glEnableClientState(GL_VERTEX_ARRAY);
//      glEnable(GL_LINE_SMOOTH);

      glStencilMask(0x10);
//      glStencilFunc(GL_NOTEQUAL,0x31,0x20);
      glStencilFunc(GL_EQUAL,0x00,0xff);
      glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);

      if (Interp) {
         sprintf(buf,"%% Postscript des contours\n%i setlinewidth 1 setlinecap 1 setlinejoin\n",Field->Spec->Width-1);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
         if (Field->Spec->Outline)
            Tk_CanvasPsColor(Interp,VP->canvas,Field->Spec->Outline);
      }

      if (Interp) {
         glPostscriptDash(Interp,&Field->Spec->Dash,Field->Spec->Width);
      } else {
         glDash(&Field->Spec->Dash);
      }

      /*Do we need transparency*/
      if ((Field->Spec->MapAll && Field->Spec->Map && Field->Spec->Map->Alpha) || Field->Spec->Alpha<100) {
         glEnable(GL_BLEND);
      }
      
      glColor4us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue,Field->Spec->Alpha*655.35);
      glLineWidth(Field->Spec->Width);

      list=Field->Def->Segments;

      // For PseudoCylindric projection, keep depth to 1
      Grid_ForceLI(Proj,0.01);
      
      while(list) {
         array=(T3DArray*)list->Data;

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
         Proj->Type->Render(Proj,0,array->Data,NULL,NULL,NULL,Field->GRef->Grid[0]=='M'?GL_LINES:GL_LINE_STRIP,array->Size,0,NULL,NULL);

         if (Interp)
            glFeedbackProcess(Interp,GL_2D);

         list=list->Next;
      }

      // For PseudoCylindric projection, keep depth to 1
      Grid_ResetLI(Proj);
     
      glDisableClientState(GL_VERTEX_ARRAY);
      glDisable(GL_LINE_STIPPLE);
      glDisable(GL_LINE_SMOOTH);
      glDisable(GL_BLEND);

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
         glPostscriptDash(Interp,NULL,Field->Spec->Width);
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
 * But      : Effectue l'affichage des labels des niveaux le long des lignes de contours.
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

   int  n,c,delta;
   char buf[256];
   double th,dx,dy,dnx,dny,d;
   TList *list;
   T3DArray *array;
   Vect3d  p1,p0;

   if (GLRender->Resolution>1 || !Field->GRef || !Field->Spec->Font || (!Field->Spec->Outline && !Field->Spec->MapAll) || !Field->Spec->InterNb) {
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


   dy=Field->Spec->TKM.ascent;

   glPolygonMode(GL_FRONT,GL_FILL);
   glReadBuffer(GL_BACK);
   glEnable(GL_STENCIL_TEST);
   glStencilFunc(GL_EQUAL,0xf0,0x0f);
   glStencilOp(GL_KEEP,GL_REPLACE,GL_REPLACE);
   glDepthMask(GL_FALSE);

   /*Do we need transparency*/
   if ((Field->Spec->MapAll && Field->Spec->Map && Field->Spec->Map->Alpha) || Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }

   if (Field->Def->Segments) {

      if (Interp) {
         if (Field->Spec->Outline)
            Tk_CanvasPsColor(Interp,VP->canvas,Field->Spec->Outline);
         Tk_CanvasPsFont(Interp,VP->canvas,Field->Spec->Font);
         Tcl_AppendResult(Interp,"clippath\n",(char*)NULL);
      } else {
         if (Field->Spec->Outline)
            glColor4us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue,Field->Spec->Alpha*655.35);

         glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),Field->Spec->Font);
      }

      list=Field->Def->Segments;
      delta=Field->Spec->RenderLabel*100;

      while(list) {
         array=(T3DArray*)list->Data;
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
         gluProject(array->Data[0][0],array->Data[0][1],array->Data[0][2],VP->GLModR,VP->GLProj,VP->GLView,&p1[0],&p1[1],&p1[2]);
         d=delta;

         for(n=1;n<array->Size;n++) {
            Vect_Assign(p0,p1);
            gluProject(array->Data[n][0],array->Data[n][1],array->Data[n][2],VP->GLModR,VP->GLProj,VP->GLView,&p1[0],&p1[1],&p1[2]);

            /* Test for insidness within viewport */
            if (!VIN(p0[2],0,1) || !VIN(p1[2],0,1)) {
               continue;
            }

            d+=hypot(p1[0]-p0[0],p1[1]-p0[1]);
            if (Field->Spec->RenderLabel<0 || d>=delta) {

               /*Calculate the angle of the segment*/
               th=-atan2(p1[0]-p0[0],p1[1]-p0[1])+M_PI2;
               if (th<-M_PI2) th+=M_PI;
               if (th>M_PI2)  th-=M_PI;

               /*We have to translate along the rotation axis to center the label*/
               dnx=(dx*cos(th)-dy*cos(M_PI2-th))*0.5;
               dny=(dx*sin(th)+dy*sin(M_PI2-th))*0.5;

               if (ViewportCrowdPush(VP,p1[0]-dnx,p1[1]-dny,p1[0]+dnx,p1[1]+dny,-1)) {

                  p1[0]-=dnx; p1[1]-=dny;
                  th=RAD2DEG(th);

                  /*Draw the bloc in the stencil buffer*/
                  glStencilMask(0x20);
                  glStencilMaskQuad(p1[0],p1[1],dx,dy,th,Field->Spec->TKM.linespace/2,1);

                  if (Interp) {
                     glPostscriptTextBG(Interp,VP->canvas,p1[0],p1[1],th,dx,dy,Field->Spec->TKM.linespace/2,1,VP->BGColor,1);
                  }

                  /*Draw the text*/
                  glStencilMask(0x10);
                  glPrint(Interp,VP->canvas,buf,p1[0],p1[1],th);
                  d=0;
               }
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

   glDisable(GL_BLEND);
   glDepthMask(GL_TRUE);
   glMatrixMode(GL_PROJECTION);
   glPopMatrix();
   glMatrixMode(GL_MODELVIEW);
   glPopMatrix();

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

   char buf[128];
   
   if (!Field->GRef || !Field->GPos || !Field->Spec->Outline)
      return;

   /*Do we need transparency*/
   if (Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }
   
   if (Interp) {
      glFeedbackInit(FSIZE2D(Field->Def)*3,GL_2D);
      sprintf(buf,"%% Postscript de la grille\n%.2f setlinewidth 1 setlinecap 1 setlinejoin\n",Field->Spec->RenderGrid-0.5);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
      Tk_CanvasPsColor(Interp,VP->canvas,Field->Spec->Outline);
   } else {
      glPointSize(Field->Spec->RenderGrid+0.1);
      glColor4us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue,Field->Spec->Alpha*655.35);
   }

   /*Afficher les points*/
   glEnableClientState(GL_VERTEX_ARRAY);
   Proj->Type->Render(Proj,0,&Field->GPos->Pos[Field->Def->Level][Field->Def->Idx],NULL,NULL,NULL,GL_POINTS,FSIZE2D(Field->Def),1,NULL,NULL);

   if (Interp)
      glFeedbackProcess(Interp,GL_2D);

   if (Field->GRef->Grid[0]=='M') {
      glPolygonMode(GL_FRONT,GL_LINE);
      glLineWidth(1.0);
      Proj->Type->Render(Proj,0,&Field->GPos->Pos[Field->Def->Level][Field->Def->Idx],Field->GRef->Idx,NULL,NULL,GL_TRIANGLES,Field->GRef->NIdx,0,NULL,NULL);
   }
   glDisableClientState(GL_VERTEX_ARRAY);
   glDisable(GL_BLEND);

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

   if (!Spec->Font) {
      return;
   }

   txth=Spec->TKM.linespace*2;
   dtx=Spec->TKM.ascent*0.5;

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

   if (!Field->GRef || !Field->GPos) {
      return(0);
   }

   if (!Field->Map) {
      Data_MapColor(Field,True);
   }

   /*Setup 1D Texture*/
   glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,Field->Spec->Map->NbPixels,0,GL_RGBA,GL_UNSIGNED_BYTE,Field->Spec->Map->Color);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
   glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_REPLACE);
   glEnable(GL_TEXTURE_1D);

   /*Do we need transparency*/
   if (Field->Spec->Map->Alpha || Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }

   /*Projeter les particules*/
   glPointSize(Field->Spec->RenderParticle+0.1);
   glEnableClientState(GL_VERTEX_ARRAY);
   Proj->Type->Render(Proj,0,&Field->GPos->Pos[Field->Def->Level][Field->Def->Idx],Field->GRef->Idx,NULL,Field->Map,GL_POINTS,Field->GRef->NIdx,0,NULL,NULL);
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
   int    b,f,len,dz;
   float  step,*map;
   Vect3d pix,*vbuf;
   Coord  coo;

   if (GLRender->Resolution>2) {
      return(0);
   }

   if (!Field->GRef || !Field->GPos || !Field->Def->Data[1] || !Field->Spec->Width || !Field->Spec->Outline) {
      return(0);
   }

  /*Setup 1D Texture*/
   glEnable(GL_TEXTURE_1D);
   map=FFStreamMapSetup1D(0.025);

   glMatrixMode(GL_TEXTURE);
   glEnable(GL_STENCIL_TEST);
   glStencilMask(0x20);
   glStencilFunc(GL_NOTEQUAL,0x20,0x20);
   glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);

   glReadBuffer(GL_BACK);
   glEnableClientState(GL_VERTEX_ARRAY);

   /*Do we need transparency*/
   if (Field->Spec->Alpha<100) {
      glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
      glEnable(GL_BLEND);
   }
   glColor4us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue,Field->Spec->Alpha*655.35);

   // Animation step
   if (GLRender->Delay<GL_STOP) {
      Field->Spec->TexStep+=0.02;
   }

   dz=Field->Spec->Sample*10;
   dt=0.0;
   len=512;
   
   /*Get the cell resolution, to use as step size for a constant spacing*/
   step=Proj->PixDist/Field->GRef->Distance(Field->GRef,Field->Def->NI>>1,Field->Def->NJ>>1,(Field->Def->NI>>1)+1,Field->Def->NJ>>1)*5;

   vbuf=VBuffer_Alloc(len*2+1);

   /*Recuperer les latlon des pixels sujets*/
   for (pix[0]=0;pix[0]<VP->Width;pix[0]+=dz) {
      for (pix[1]=0;pix[1]<VP->Height;pix[1]+=dz) {

         Proj->Type->UnProject(VP,Proj,&coo,pix);
         if (coo.Lat==-999.0) {
            continue;
         }

         if (Field->GRef->UnProject(Field->GRef,&i,&j,coo.Lat,coo.Lon,0,1) && i<Field->Def->NI-2 && j<Field->Def->NJ-2) {

            /*Get the streamline */
            b=FFStreamLine(Field->GPos,Field->Def,VP,vbuf,NULL,i,j,Field->Def->Level,len,-step,Field->Spec->Min,0,REF_PROJ,Field->Spec->SizeRange>1.0?0:-1);
            f=FFStreamLine(Field->GPos,Field->Def,VP,&vbuf[len],NULL,i,j,Field->Def->Level,len,step,Field->Spec->Min,0,REF_PROJ,Field->Spec->SizeRange>1.0?0:-1);

            /* If we have at least some part of it */
            glPushMatrix();
            glScalef(10.0/Field->Spec->Size,1.0,1.0);
            glTranslatef(Field->Spec->TexStep-(dt+=0.015),0.0,0.0);
            if (b+f>2) {
               glLineWidth(Field->Spec->Width);
               glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
               Grid_ForceLI(Proj,0.01);
               
               Proj->Type->Render(Proj,0,&vbuf[len-b],NULL,NULL,map,GL_LINE_STRIP,b+f,0,NULL,NULL);

               if (Field->Spec->SizeRange>1.0) {
                  glLineWidth(Field->Spec->SizeRange*2*Field->Spec->Width);
                  glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
                  Proj->Type->Render(Proj,0,&vbuf[len-b],NULL,NULL,NULL,GL_LINE_STRIP,b+f,0,NULL,NULL);
               }
               Grid_ResetLI(Proj);
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
   glDisable(GL_BLEND);

   return(1);
}

int Data_RenderStream3D(TData *Field,ViewportItem *VP,Projection *Proj){

   int    i,j,k,k0,k1;
   int    n,b,f,c,len,dz,idx;
   float  *map=NULL;
   Vect3d *vbuf;

   if (GLRender->Resolution>2) {
      return(0);
   }

   if (!Field->GRef || !Field->GPos || !Field->Def->Data[1] || !Field->Spec->Width || !Field->Spec->Outline) {
      return(0);
   }

   /*Setup 1D Texture*/
   if (Field->Spec->MapAll && Field->Spec->Map) {
      if (Field->Spec->Map->Alpha) {
         glEnable(GL_BLEND);
      }
      if (!Field->Map) {
         Field->Map=(float*)malloc(sizeof(float)*2*FFSTREAMLEN);
      }
      map=Field->Map;
      glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,Field->Spec->Map->NbPixels,0,GL_RGBA,GL_UNSIGNED_BYTE,Field->Spec->Map->Color);
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
      glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_REPLACE);
   } else {
      map=FFStreamMapSetup1D(1.0);
      
      /*Do we need transparency*/
      if (Field->Spec->Alpha<100) {
         glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
         glEnable(GL_BLEND);
      }
      glColor4us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue,Field->Spec->Alpha*655.35);
   }

   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
   glTexParameteri(GL_TEXTURE_1D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
   glEnable(GL_TEXTURE_1D);
   glEnableClientState(GL_VERTEX_ARRAY);

   len=FFSTREAMLEN;
   dz=Field->Spec->Sample;
   glLineWidth((float)Field->Spec->Width);
   glMatrixMode(GL_TEXTURE);

    vbuf=VBuffer_Alloc(len*2+1);

   /*Recuperer les latlon des pixels sujets*/
   if (Field->Spec->PosNb) {
      for(n=0;n<Field->Spec->PosNb;n++) {
         i=Field->Spec->Pos[n][0];
         j=Field->Spec->Pos[n][1];
         k=Field->Spec->Pos[n][2];

         /*Get the streamline */
         b=FFStreamLine(Field->GPos,Field->Def,VP,vbuf,map,i,j,k,len,-Field->Spec->Step,Field->Spec->Min,0,REF_PROJ,Field->Def->Data[2]?1:-1);
         f=FFStreamLine(Field->GPos,Field->Def,VP,&vbuf[len>>1],(map?&map[len>>1]:map),i,j,k,len,Field->Spec->Step,Field->Spec->Min,0,REF_PROJ,Field->Def->Data[2]?1:-1);
         /* If we have at least some part of it */
         if (b+f>2) {
            glPushMatrix();
            if (Field->Spec->MapAll) {
               for(c=0;c<b+f;c++) {
                  VAL2COL(idx,Field->Spec,Field->Map[c]);
                  map[c]=idx/(float)Field->Spec->Map->NbPixels;
               }
            } else {
               glScalef((float)(len)/(b+f),1.0,1.0);
               // Animation step
               if (GLRender->Delay<GL_STOP) {
                  Field->Spec->TexStep+=0.00001;
                  glTranslatef(Field->Spec->TexStep,0.0,0.0);
               }
            }
            Grid_ForceLI(Proj,0.01);
            Proj->Type->Render(Proj,0,&vbuf[(len>>1)-b],NULL,NULL,map,GL_LINE_STRIP,b+f,0,NULL,NULL);
            Grid_ResetLI(Proj);
            glPopMatrix();
         }
      }
   } else {
      for(i=Field->Spec->Cube[0];i<=Field->Spec->Cube[3];i+=dz) {
         for(j=Field->Spec->Cube[1];j<=Field->Spec->Cube[4];j+=dz) {
            for(k=Field->Spec->Cube[2];k<=Field->Spec->Cube[5];k+=dz) {
               /*Get the streamline */
               f=FFStreamLine(Field->GPos,Field->Def,VP,vbuf,Field->Map,i,j,k,len,Field->Spec->Step,Field->Spec->Min,0,REF_PROJ,Field->Def->Data[2]?1:-1);

               /* If we have at least some part of it */
               if (f>2) {
                  glPushMatrix();
                  if (Field->Spec->MapAll) {
                     for(c=0;c<f;c++) {
                        VAL2COL(idx,Field->Spec,Field->Map[c]);
                        map[c]=idx/(float)Field->Spec->Map->NbPixels;
                     }
                  } else {
                     glScalef(100*1.0/Field->Spec->Size+1,1.0,1.0);
                     // Animation step
                     if (GLRender->Delay<GL_STOP) {
                        Field->Spec->TexStep+=0.00001;
                        glTranslatef(Field->Spec->TexStep,0.0,0.0);
                     }
                  }
                  Grid_ForceLI(Proj,0.01);
                  Proj->Type->Render(Proj,0,vbuf,NULL,NULL,map,GL_LINE_STRIP,f,0,NULL,NULL);
                  Grid_ResetLI(Proj);
                  glPopMatrix();
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

   Field->Spec->Cube[0]=Field->Spec->Cube[0]<Field->GRef->X0?Field->GRef->X0:Field->Spec->Cube[0]>Field->GRef->X1?Field->GRef->X1:Field->Spec->Cube[0];
   Field->Spec->Cube[3]=Field->Spec->Cube[3]<Field->GRef->X0?Field->GRef->X0:Field->Spec->Cube[3]>Field->GRef->X1?Field->GRef->X1:Field->Spec->Cube[3];
   Field->Spec->Cube[1]=Field->Spec->Cube[1]<Field->GRef->Y0?Field->GRef->Y0:Field->Spec->Cube[1]>Field->GRef->Y1?Field->GRef->Y1:Field->Spec->Cube[1];
   Field->Spec->Cube[4]=Field->Spec->Cube[4]<Field->GRef->Y0?Field->GRef->Y0:Field->Spec->Cube[4]>Field->GRef->Y1?Field->GRef->Y1:Field->Spec->Cube[4];
   
   Field->Spec->Cube[2]=Field->Spec->Cube[2]<0?0:Field->Spec->Cube[2]>Field->ZRef->LevelNb-1?Field->ZRef->LevelNb-1:Field->Spec->Cube[2];
   Field->Spec->Cube[5]=Field->Spec->Cube[5]<0?0:Field->Spec->Cube[5]>Field->ZRef->LevelNb-1?Field->ZRef->LevelNb-1:Field->Spec->Cube[5];

   k0=Field->Spec->Cube[2];
   k1=Field->Spec->Cube[5];
   glBegin(GL_QUADS);

      /*Bottom*/
      idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[0];
      glVertex3dv(Field->GPos->Pos[k0][idx]);
      idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[0];
      glVertex3dv(Field->GPos->Pos[k0][idx]);
      idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[3];
      glVertex3dv(Field->GPos->Pos[k0][idx]);
      idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[3];
      glVertex3dv(Field->GPos->Pos[k0][idx]);

      if (k0!=k1) {
         if (Field->Spec->Cube[1]!=Field->Spec->Cube[4]) {
            /*Left*/
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->GPos->Pos[k0][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->GPos->Pos[k0][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->GPos->Pos[k1][idx]);
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->GPos->Pos[k1][idx]);

            /*Right*/
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->GPos->Pos[k0][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->GPos->Pos[k0][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->GPos->Pos[k1][idx]);
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->GPos->Pos[k1][idx]);
         }


         if (Field->Spec->Cube[0]!=Field->Spec->Cube[3]) {
            /*Up*/
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->GPos->Pos[k0][idx]);
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->GPos->Pos[k0][idx]);
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->GPos->Pos[k1][idx]);
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->GPos->Pos[k1][idx]);

            /*Down*/
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->GPos->Pos[k0][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->GPos->Pos[k0][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->GPos->Pos[k1][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->GPos->Pos[k1][idx]);
         }

         /*Top*/
         if (Field->Spec->Cube[0]!=Field->Spec->Cube[3] && Field->Spec->Cube[1]!=Field->Spec->Cube[4]) {
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->GPos->Pos[k1][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[0];
            glVertex3dv(Field->GPos->Pos[k1][idx]);
            idx=Field->Spec->Cube[4]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->GPos->Pos[k1][idx]);
            idx=Field->Spec->Cube[1]*Field->Def->NI+Field->Spec->Cube[3];
            glVertex3dv(Field->GPos->Pos[k1][idx]);
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

   int     n;
   Vect3d  b,p,p0,p1,p2;
   Vect3d *pos;

   if (!Field->GRef || !Field->GPos) {
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

   pos=Field->GPos->Pos[Field->Def->Level];

   if (!Field->Map)
      Data_MapColor(Field,False);

   glLineWidth(1.0);
   if (Field->Spec->InterpDegree[0]=='L') {
      glEnableClientState(GL_VERTEX_ARRAY);
      Proj->Type->Render(Proj,0,pos,Field->GRef->Idx,NULL,Field->Map,GL_TRIANGLES,Field->GRef->NIdx,0,NULL,NULL);
      glDisableClientState(GL_VERTEX_ARRAY);
   } else {
      glBegin(GL_TRIANGLES);
      for(n=0;n<Field->GRef->NIdx-3;n+=3) {
         Vect_Init(b,1.0/3.0,1.0/3.0,1.0/3.0);
         Bary_InterpPos(b,p,pos[Field->GRef->Idx[n]],pos[Field->GRef->Idx[n+1]],pos[Field->GRef->Idx[n+2]]);
         Vect_Init(b,0.0,0.5,0.5);
         Bary_InterpPos(b,p0,pos[Field->GRef->Idx[n]],pos[Field->GRef->Idx[n+1]],pos[Field->GRef->Idx[n+2]]);
         Vect_Init(b,0.5,0.0,0.5);
         Bary_InterpPos(b,p1,pos[Field->GRef->Idx[n]],pos[Field->GRef->Idx[n+1]],pos[Field->GRef->Idx[n+2]]);
         Vect_Init(b,0.5,0.5,0.0);
         Bary_InterpPos(b,p2,pos[Field->GRef->Idx[n]],pos[Field->GRef->Idx[n+1]],pos[Field->GRef->Idx[n+2]]);

         glTexCoord1f(Field->Map[Field->GRef->Idx[n]]);
         glVertex3dv(pos[Field->GRef->Idx[n]]);
         glVertex3dv(p);
         glVertex3dv(p1);
         glVertex3dv(pos[Field->GRef->Idx[n]]);
         glVertex3dv(p);
         glVertex3dv(p2);

         glTexCoord1f(Field->Map[Field->GRef->Idx[n+1]]);
         glVertex3dv(pos[Field->GRef->Idx[n+1]]);
         glVertex3dv(p);
         glVertex3dv(p0);
         glVertex3dv(pos[Field->GRef->Idx[n+1]]);
         glVertex3dv(p);
         glVertex3dv(p2);

         glTexCoord1f(Field->Map[Field->GRef->Idx[n+2]]);
         glVertex3dv(pos[Field->GRef->Idx[n+2]]);
         glVertex3dv(p);
         glVertex3dv(p0);
         glVertex3dv(pos[Field->GRef->Idx[n+2]]);
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

   int          i,j,c0,c1,c2,c3,idxk,idx0,idx1,idx2,idx3;
   int          ox=0,base=0,dp;
   int          depth;
   double       v0,v1,v2,v3;
   Vect3d       g0,g1,g2,g3,dim,*pos;
   unsigned int dx,dy;

   if (GLRender->Resolution>2) {
      return(0);
   }

   if (!Field->GRef || !Field->GPos || Field->GRef->Grid[0]=='Y') {
      return(0);
   }

   if ((Proj->Type->Def!=PROJGLOBE) && Field->GRef->Grid[0]!='V') {
      glEnable(GL_CULL_FACE);
      glCullFace(GL_FRONT);
   } else {
      glDisable(GL_CULL_FACE);
   }

   /*Afficher les points*/
   if (Field->GRef->Grid[0]=='M') {
      Data_RenderMesh(Field,VP,Proj);
      return(1);
   }

   /*Do we need transparency*/
   if (Field->Spec->Map->Alpha || Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }

   if (Field->Spec->InterNb) {
      VAL2COL(base,Field->Spec,Field->Spec->Inter[0]);
   }

   /*Process gridpoints*/
   idxk=FSIZE2D(Field->Def)*Field->Def->Level;
   pos=&Field->GPos->Pos[Field->Def->Level][Field->Def->Idx];

   /*Resolution selon la dimension des cellules (mid-grid) et la vue*/   
   dp=Proj->PixDist/Field->GRef->Distance(Field->GRef,Field->Def->NI>>1,Field->Def->NJ>>1,(Field->Def->NI>>1)+1,Field->Def->NJ>>1);
   
   if (Field->Spec->InterNb) 
      dp>>=2;
   
   dp=(dp<1 || Field->GRef->Grid[0]=='V' || (Proj->Ref && Proj->Ref->Type&GRID_PSEUDO))?1:dp;

   /*Grille avec loop sur la longitude*/
   if (Field->GRef->Type&GRID_WRAP && Proj->Type->Def!=PROJPLANE) {
      ox=1;
      dp=dp>10?10:dp;
   }
   
   idx0=idx1=idx2=idx3=0;
   c0=c1=c2=c3=0;
   v0=v1=v2=v3=0.0;

   /*Render as line to fill the imprecision gaps (only when no transparency)*/
   if (GLRender->TRCon && Proj->Type->Def!=PROJPLANE && (!Field->Spec->Map->Alpha && !Field->Spec->Alpha<100)) {
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
      for(j=0;j<Field->Def->NJ-dp;j+=dp) {

         glBegin(GL_QUADS);

         for(i=0;i<(Field->Def->NI+dp);i+=dp) {

            if (i!=0) {
               idx1=idx0;
               idx2=idx3;
               v1=v0;
               v2=v3;
               c1=c0;
               c2=c3;
            }

            /*If the next index is over the size*/
            if (i>=Field->Def->NI) {
               if (ox) {
                  /*If the grid wraps around, use the first point*/
                  idx0=j*Field->Def->NI;
               } else {
                  /*If not, use the last point*/
                  idx0=(j+1)*Field->Def->NI-1;
               }
            } else {
               idx0=j*Field->Def->NI+i;
            }
            idx3=idx0+dp*Field->Def->NI;

            Def_GetMod(Field->Def,idxk+idx0,v0);
            Def_GetMod(Field->Def,idxk+idx3,v3);
            VAL2COL(c0,Field->Spec,v0);
            VAL2COL(c3,Field->Spec,v3);

            /* Is the cell valid ??? */
            if (i && (c0>-1 || c1>-1 || c2>-1 || c3>-1)) {

               /*Check for mask value*/
               if (Field->Def->Mask && !Field->Def->Mask[idx0] && !Field->Def->Mask[idx1] && !Field->Def->Mask[idx2] && !Field->Def->Mask[idx3]) {
                  glEnd();
                  glBegin(GL_QUADS);
                  continue;
               }

               Vect_Assign(g0,pos[idx0]);
               Vect_Assign(g1,pos[idx1]);
               Vect_Assign(g2,pos[idx2]);
               Vect_Assign(g3,pos[idx3]);

               /* Is the cell visible ??? */
               if (FFCellProcess(VP,Proj,g0,g1,g2,g3,dim)) {
                  if (Field->Spec->InterpDegree[0]=='N') {
                     FFCellQuadNearest(Field->Spec,g0,g1,g2,g3,c0,c1,c2,c3,base);
                  } else {

                     dx=ABS(dim[0]);
                     dy=ABS(dim[1]);
                     dx=MIN(dx,dy);
                     depth=ceil(LOG2(dx));

                     /* Is the cell resolution enough ??? */
                     if (depth>=1 && ((c0!=c1) || (c1!=c2) || (c2!=c3) || (c3!=c0))) {
                        FFCellQuadLinear(Field->Spec,g0,g1,g2,g3,c0,c1,c2,c3,v0,v1,v2,v3,depth,base);
                     } else {
                        VR(Field->Spec,g0,c0,base);
                        VR(Field->Spec,g1,c1,base);
                        VR(Field->Spec,g2,c2,base);
                        VR(Field->Spec,g3,c3,base);
                     }
                  }
               }
            }
      }
      glEnd();
      }
   }

   if (GLRender->GLDebug) {
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
   } else {
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   }
   
   /*Render the polygons over the lines*/
   for(j=0;j<Field->Def->NJ-dp;j+=dp) {

      glBegin(GL_QUADS);

      for(i=0;i<(Field->Def->NI+dp);i+=dp) {

         if (i!=0) {
            idx1=idx0;
            idx2=idx3;
            v1=v0;
            v2=v3;
            c1=c0;
            c2=c3;
         }

         /*If the next index is over the size*/
         if (i>=Field->Def->NI) {
            if (ox) {
               /*If the grid wraps around, use the first point*/
               idx0=j*Field->Def->NI;
            } else {
               /*If not, use the last point*/
               idx0=(j+1)*Field->Def->NI-1;
            }
         } else {
            idx0=j*Field->Def->NI+i;
         }
         idx3=idx0+dp*Field->Def->NI;

         Def_GetMod(Field->Def,idxk+idx0,v0);
         Def_GetMod(Field->Def,idxk+idx3,v3);
         VAL2COL(c0,Field->Spec,v0);
         VAL2COL(c3,Field->Spec,v3);

         /* Is the cell valid ??? */
         if (i && (c0>-1 || c1>-1 || c2>-1 || c3>-1)) {

            /*Check for mask value*/
            if (Field->Def->Mask && !Field->Def->Mask[idx0] && !Field->Def->Mask[idx1] && !Field->Def->Mask[idx2] && !Field->Def->Mask[idx3]) {
               glEnd();
               glBegin(GL_QUADS);
               continue;
            }

            Vect_Assign(g0,pos[idx0]);
            Vect_Assign(g1,pos[idx1]);
            Vect_Assign(g2,pos[idx2]);
            Vect_Assign(g3,pos[idx3]);

            /* Is the cell visible ??? */
            if (FFCellProcess(VP,Proj,g0,g1,g2,g3,dim)) {
               if (Field->Spec->InterpDegree[0]=='N') {
                  FFCellQuadNearest(Field->Spec,g0,g1,g2,g3,c0,c1,c2,c3,base);
               } else {

                  dx=ABS(dim[0]);
                  dy=ABS(dim[1]);
                  dx=MIN(dx,dy);
                  depth=ceil(LOG2(dx));

                  /* Is the cell resolution enough ??? */
                  if (depth>=1 && ((c0!=c1) || (c1!=c2) || (c2!=c3) || (c3!=c0))) {
                     FFCellQuadLinear(Field->Spec,g0,g1,g2,g3,c0,c1,c2,c3,v0,v1,v2,v3,depth,base);
                  } else {
                     VR(Field->Spec,g0,c0,base);
                     VR(Field->Spec,g1,c1,base);
                     VR(Field->Spec,g2,c2,base);
                     VR(Field->Spec,g3,c3,base);
                  }
               }
            }
         }
      }
      glEnd();
   }
 
   glCullFace(GL_BACK);
   glEnable(GL_CULL_FACE);
   glDisable(GL_BLEND);

   return(1);
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

   if (GLRender->Resolution>1 || !Field->GRef || !Field->GPos || !Field->Spec->Font || !Field->Spec->Outline) {
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
   
   /*Do we need transparency*/
   if (Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }
   glColor4us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue,Field->Spec->Alpha*655.35);
   glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),Field->Spec->Font);
   glDisable(GL_STENCIL_TEST);

   posa=Field->GPos->Pos[Field->Def->Level];
   zm=zmm=zn=zv=0.0;

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
            if (VIN(pos[0],1,Proj->VP->Width) && VIN(pos[1],1,Proj->VP->Height) && VIN(pos[2],0,1)) {
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

   glDisable(GL_BLEND);
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

   float   size,theta,thetad,*ll,*xy;
   double  len,dir,u,v,uv,w,i0,j0,i1,j1;
   Vect3d  pix;
   Coord   coo;
   int     n=0,mem,i,j,idx,idc,dz,dn,nn;
   char    buf[32],grtyp;

   if (!Field->GRef || !Field->GPos || !Field->Spec->Width || !Field->Spec->Outline)
      return;

   /*Calculer la dimension generale*/
   size=VP->Ratio*Field->Spec->Size;
   theta=thetad=0.0f;
   u=v=w=len=0.0;
   
   /*Do we need transparency*/
   if ((Field->Spec->MapAll && Field->Spec->Map && Field->Spec->Map->Alpha) || Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }

   /*Afficher toutes les barbules*/
   glMatrixMode(GL_MODELVIEW);
   glDisable(GL_CULL_FACE);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   glLineWidth(Field->Spec->Width);
   glColor4us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue,Field->Spec->Alpha*655.35);

   if (Interp) {
      sprintf(buf,"%i\n",Field->Spec->Width-1);
      Tcl_AppendResult(Interp,"%% Postscript des donnees vectorielles\n",buf," setlinewidth 0 setlinecap 0 setlinejoin\n",(char*)NULL);
      Tk_CanvasPsColor(Interp,VP->canvas,Field->Spec->Outline);
   }

   switch(Field->GRef->Grid[0]) {

      case 'V':
         if (Field->Def->Data[2]) {
            for(i=0;i<Field->Def->NI;i+=Field->Spec->Sample) {
               if (i+1<Field->Def->NI) {
                  /*We calculat the section orientation for rotate the vector accordingly*/
                  if (Field->GRef->RefFrom) {
                     /*If we have the georef it's coming from*/
                     Field->GRef->RefFrom->UnProject(Field->GRef->RefFrom,&i0,&j0,Field->GRef->Lat[i],Field->GRef->Lon[i],1,1);
                     Field->GRef->RefFrom->UnProject(Field->GRef->RefFrom,&i1,&j1,Field->GRef->Lat[i+1],Field->GRef->Lon[i+1],1,1);
                     theta=atan2(i1-i0,j1-j0);
                  } else {
                     /*Otherwise, use the cut orientation*/
                     theta=-COURSE(DEG2RAD(Field->GRef->Lat[i]),DEG2RAD(Field->GRef->Lon[i]),DEG2RAD(Field->GRef->Lat[i+1]),DEG2RAD(Field->GRef->Lon[i+1]));
//                     theta=0.0;
                  }
                  thetad=90.0+RAD2DEG(theta);

               }
               for(j=0;j<Field->Def->NJ;j+=Field->Spec->Sample) {
                  idx=j*Field->Def->NI+i;

                  // Check for mask
                  if (!Field->Def->Mask || Field->Def->Mask[idx]) {
                     /*If the speed if between the defined range*/
                     Def_GetMod(Field->Def,idx,len);
                     if (len<=Field->Spec->Max && len>=Field->Spec->Min) {
                        if (Field->Spec->MapAll && Field->Spec->Map) {
                           VAL2COL(idc,Field->Spec,len);
                           if (Interp) {
                              CMap_PostscriptColor(Interp,Field->Spec->Map,idc);
                           } else {
                              glColor4ubv(Field->Spec->Map->Color[idc]);
                           }
                        }

                        /*Get the components*/
                        Def_Get(Field->Def,0,idx,u);
                        Def_Get(Field->Def,2,idx,w);

                        /*If the horizontal components are not section oriented but geographicaly N-S E-W*/
                        if (!Field->Spec->GridVector) {
                           Def_Get(Field->Def,1,idx,v);
                           uv=hypot(u,v);
                           u=uv*cos(atan2(v,u)-theta);
                        }

                        /*Resize the arrow on the speed*/
                        size=VP->Ratio*VECTORSIZE(Field->Spec,len);
                        if (Interp) glFeedbackInit(256,GL_2D);
                        Data_RenderBarbule(Field->Spec->RenderVector,0,thetad,Field->GRef->Lat[i],Field->GRef->Lon[i],ZRef_Level2Meter(Field->ZRef->Levels[j],Field->ZRef->Type),VAL2SPEC(Field->Spec,len),RAD2DEG(atan2(u,w)),size,Proj);
                        if (Interp) glFeedbackProcess(Interp,GL_2D);
                     }
                  }
               }
            }
         }
         break;

      case 'X':
      case 'Y':
      case 'M':
      case 'W':
         grtyp=Field->GRef->Grid[0];
         size=VP->Ratio*VECTORSIZE(Field->Spec,Field->Spec->Min+(Field->Spec->Max-Field->Spec->Min)*0.5);

         for(i=0;i<Field->Def->NI;i+=(grtyp=='Y'?1:Field->Spec->Sample)) {
            for(j=0;j<Field->Def->NJ;j+=(grtyp=='Y'?1:Field->Spec->Sample)) {
               idx=j*Field->Def->NI+i;
               
               // Check for mask
               if (!Field->Def->Mask || Field->Def->Mask[idx]) {
                  Def_GetMod(Field->Def,idx,len);
                  Field->GRef->Project(Field->GRef,i,j,&coo.Lat,&coo.Lon,1,1);
                  if (len<=Field->Spec->Max && len>=Field->Spec->Min) {
                     if (Field->Spec->MapAll && Field->Spec->Map) {
                        VAL2COL(idc,Field->Spec,len);
                        if (Interp) {
                           CMap_PostscriptColor(Interp,Field->Spec->Map,idc);
                        } else {
                           glColor4ubv(Field->Spec->Map->Color[idc]);
                        }
                     }
                     Def_Get(Field->Def,0,idx,u);
                     
                     if (Field->Def->Data[1]) {
                        Def_Get(Field->Def,1,idx,v);
                        size=VP->Ratio*VECTORSIZE(Field->Spec,len);
                        dir=(grtyp=='Y' || grtyp=='W')?v:180+RAD2DEG(atan2(u,v));
                     } else {
                        dir=u;
                     }
                     if (Interp) glFeedbackInit(256,GL_2D);
                     Data_RenderBarbule(Field->Spec->RenderVector,0,0.0,coo.Lat,coo.Lon,ZRef_Level2Meter(Field->ZRef->Levels[Field->Def->Level],Field->ZRef->Type),VAL2SPEC(Field->Spec,len),dir,size,Proj);
                     if (Interp) glFeedbackProcess(Interp,GL_2D);
                  }
               }
            }
         }
         break;

      default:
#ifdef HAVE_RMN
         if (Field->Spec->SampleType=='P') {
            dz=Field->Spec->Sample*10;

            // Allouer la memoire pour les donnees
            mem=(VP->Width*VP->Height)/dz;
            ll=(float*)malloc(2*mem*sizeof(float));

            if (!ll) {
               App_Log(ERROR,"%s: Unable to allocate temporary coordinate buffer\n",__func__);
               return;
            }

            // Recuperer les latlon des pixels sujets
            for (pix[0]=0;pix[0]<VP->Width;pix[0]+=dz) {
               for (pix[1]=0;pix[1]<VP->Height;pix[1]+=dz) {

                  Proj->Type->UnProject(VP,Proj,&coo,pix);
                  if (coo.Lat!=-999.0) {
                     ll[n]=coo.Lat;
                     ll[mem+n]=(coo.Lon<0?coo.Lon+360:coo.Lon);
                     n++;
                  }
               }
            }

            // Allouer l'espaces memoire pour les retours d'ezscint
            xy=(float*)malloc(3*n*sizeof(float));

            if (!xy) {
               App_Log(ERROR,"%s: Unable to allocate temporary projected buffer\n",__func__);
               return;
            }

            //Recuperer les informations sur les vents et leurs localisations
            c_gdxyfll(Field->GRef->Ids[Field->GRef->NId],xy,&xy[n],ll,&ll[mem],n);
            
            dz=0;i=0;
            while (dz<n) {
               if (xy[dz]<=Field->Def->NI && xy[n+dz]<=Field->Def->NJ && xy[dz]>=1 && xy[n+dz]>=1) {
                  if (!Field->Def->Mask || Field->Def->Mask[FIDX2D(Field->Def,(int)xy[dz],(int)xy[n+dz])]) {
                     ll[i]=ll[dz];
                     ll[mem+i]=ll[mem+dz];
                     i++;
                  }
               }
               dz++;
            }
            n=i;
         } else {

            // Allouer la memoire pour les donnees
            mem=FSIZE2D(Field->Def);
            ll=(float*)malloc(2*mem*sizeof(float));
            
            if (!ll) {
               App_Log(ERROR,"%s: Unable to allocate temporary coordinate buffer\n",__func__);
               return;
            }
            c_gdll(Field->GRef->Ids[Field->GRef->NId],ll,&ll[mem]);

            n=0;
            for(i=0;i<mem;i+=Field->Spec->Sample) {
               if (!Field->Def->Mask || Field->Def->Mask[i]) {
                  ll[n]=ll[i];
                  ll[mem+n]=ll[mem+i];
                  n++;
               }
            }

            xy=(float*)malloc(n*3*sizeof(float));

            if (!xy) {
               App_Log(ERROR,"%s: Unable to allocate temporary projected buffer\n",__func__);
               return;
            }
         }
         
         dz=FSIZE2D(Field->Def)*Field->Def->Level;
         dn=n;
         nn=n+n;

//         RPN_IntLock();
         c_ezsetopt("INTERP_DEGREE",Field->Spec->InterpDegree);
         
         if (Field->Def->Data[1]) {
            if (Field->Spec->GridVector && Proj->Type->Def!=PROJPLANE) {
               c_gdllwdval(Field->GRef->Ids[Field->GRef->NId],xy,&xy[n],(float*)&Field->Def->Data[0][dz],(float*)&Field->Def->Data[1][dz],ll,&ll[mem],n);
            } else {        
               c_gdllvval(Field->GRef->Ids[Field->GRef->NId],xy,&xy[n],(float*)&Field->Def->Data[0][dz],(float*)&Field->Def->Data[1][dz],ll,&ll[mem],n);
            }
            // We have to get the speed from the modulus in case of 3 component vector
            c_gdllsval(Field->GRef->Ids[Field->GRef->NId],&xy[nn],(float*)&Field->Def->Mode[dz],ll,&ll[mem],n);
         } else {
            c_gdllsval(Field->GRef->Ids[Field->GRef->NId],&xy[n],(float*)&Field->Def->Mode[dz],ll,&ll[mem],n); 
            size=Field->Spec->Min+(Field->Spec->Max-Field->Spec->Min)*0.5;
            while (dn--) xy[dn+nn]=size;
            dn=n;
         }
//         RPN_IntUnlock();

         while (dn--) {
            if (xy[dn+nn]<=Field->Spec->Max && xy[dn+nn]>=Field->Spec->Min) {
               if (Field->Spec->MapAll && Field->Spec->Map) {
                  VAL2COL(idc,Field->Spec,xy[dn+nn]);
                  if (Interp) {
                     CMap_PostscriptColor(Interp,Field->Spec->Map,idc);
                  } else {
                     glColor4ubv(Field->Spec->Map->Color[idc]);
                  }
               }
               size=VP->Ratio*VECTORSIZE(Field->Spec,xy[dn+nn]);
               dir=(Field->Spec->GridVector && Proj->Type->Def!=PROJPLANE)?xy[dn+n]:180+RAD2DEG(atan2(xy[dn],xy[dn+n]));
               if (Interp) glFeedbackInit(256,GL_2D);
               Data_RenderBarbule(Field->Spec->RenderVector,0,0.0,ll[dn],ll[mem+dn],ZRef_Level2Meter(Field->ZRef->Levels[Field->Def->Level],Field->ZRef->Type),VAL2SPEC(Field->Spec,xy[dn+nn]),dir,size,Proj);
               if (Interp) glFeedbackProcess(Interp,GL_2D);
            }
         }

         // Liberer l'espace memoire temporaire
         free(ll);
         free(xy);
#else
   App_ErrorSet("%s: Need RMNLIB",__func__);
#endif
   }

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
   T3DArray *array;

   if (!Field->GRef || !Field->GPos || Field->GRef->Grid[0]=='Y') {
      return(0);
   }

   if (Field->Def->NK<=1) {
      return(0);
   }

   list=end=NULL;

   /*Creer la liste de vertex par niveaux*/
   if (!Field->Def->Segments) {
      for (i=0;i<Field->Spec->InterNb;i++) {
         len=FFMarchingCube(Field->GPos,Field->Def,Proj,Field->Spec->Inter[i]);
         if (len>6) {
            array=T3DArray_Alloc(Field->Spec->Inter[i],len);
            if (array) {
               Field->Def->Segments=TList_Add(Field->Def->Segments,array);
               VBuffer_Copy(array->Data,len);
            } else {
               App_Log(ERROR,"%s: Unable to alloc memory for volume data %f",__func__,Field->Spec->Inter[i]);
            }
         }
      }
   }

   if (Field->Def->Segments) {

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

         list=Field->Def->Segments;
         while(list) {
            end=list;
            list=list->Next;
         }
         list=end;
         while(list) {

            array=(T3DArray*)list->Data;

            VAL2COL(idx,Field->Spec,array->Value);
            glColor4ubv(Field->Spec->Map->Color[idx]);
            glVertexPointer(3,GL_DOUBLE,2*sizeof(Vect3d),array->Data[1]);
            glNormalPointer(GL_DOUBLE,2*sizeof(Vect3d),array->Data);
            glDrawArrays(GL_TRIANGLES,0,array->Size>>1);

            list=list->Prev;
         }

         glCullFace(GL_BACK);
         list=Field->Def->Segments;
         while(list) {

            array=(T3DArray*)list->Data;

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

         list=Field->Def->Segments;
         while(list) {
            array=(T3DArray*)list->Data;

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
      if (!Proj->Sun) {
         glDisable(GL_LIGHTING);
         glDisable(GL_LIGHT0);
         glDisable(GL_COLOR_MATERIAL);
      }
      glEnable(GL_STENCIL_TEST);
   }

   VBuffer_Check();
   return(1);
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

   if (Field->GRef->Grid[0]!='R' || !Field->Spec->Outline) {
      return(0);
   }

   if (!Field->Spec->RenderVol) {
      glDisable(GL_DEPTH_TEST);
   }
   glMatrixMode(GL_MODELVIEW);
   glDisable(GL_CULL_FACE);
   glDisable(GL_STENCIL_TEST);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   glLineWidth(2.0);

   /*Do we need transparency*/
   if (Field->Spec->Alpha<100) {
      glEnable(GL_BLEND);
   }

   glColor4us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue,Field->Spec->Alpha*655.35);

   /*Affichage des labels*/
   if (Field->Spec->Font) {
      glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),Field->Spec->Font);
      for(r=0;r<Field->Spec->RangeNb;r++) {
         glPushMatrix();
         Field->GRef->Project(Field->GRef,1,Field->Spec->Range[r]/Field->GRef->ResR,&loc.Lat,&loc.Lon,1,1);
         Proj->Type->Locate(Proj,loc.Lat,loc.Lon,1);

         h=sin(DEG2RAD(Field->ZRef->Levels[0]))*Field->Spec->Range[r];

         glTranslated(0.0,0.0,ZM(Proj,h));
         glScalef(VP->Ratio,VP->Ratio,1.0);
         sprintf(buf,"%.0f Km",Field->Spec->Range[r]/1000.0);
         glTranslated(0.0,5.0,0.0);
         glPrint(NULL,VP->canvas,buf,0,0,0);
         glPopMatrix();
      }
   }

   /*Affichage des cercles de ranges*/
   for(r=0;r<Field->Spec->RangeNb;r++) {
      j=Field->Spec->Range[r]/Field->GRef->ResR;

      glDisable(GL_BLEND);
      glBegin(GL_LINE_STRIP);
      for(i=0;i<Field->Def->NI;i++) {
         glVertex3dv(Field->GPos->Pos[Field->Def->Level][FIDX2D(Field->Def,i,j)]);
      }
      glVertex3dv(Field->GPos->Pos[Field->Def->Level][FIDX2D(Field->Def,0,j)]);
      glEnd();

      if (Field->Spec->RenderVol) {
         glBegin(GL_LINE_STRIP);
         for(i=0;i<Field->Def->NI;i++) {
            glVertex3dv(Field->GPos->Pos[0][FIDX2D(Field->Def,i,j)]);
         }
         glVertex3dv(Field->GPos->Pos[0][FIDX2D(Field->Def,0,j)]);
         glEnd();

         glEnable(GL_BLEND);
         glColor4us(Field->Spec->Outline->red,Field->Spec->Outline->green,Field->Spec->Outline->blue,10000);

         glBegin(GL_QUAD_STRIP);
         for(i=0;i<Field->Def->NI;i++) {
            glVertex3dv(Field->GPos->Pos[Field->Def->Level][FIDX2D(Field->Def,i,j)]);
            glVertex3dv(Field->GPos->Pos[0][FIDX2D(Field->Def,i,j)]);
         }
         glVertex3dv(Field->GPos->Pos[Field->Def->Level][FIDX2D(Field->Def,0,j)]);
         glVertex3dv(Field->GPos->Pos[0][FIDX2D(Field->Def,0,j)]);
         glEnd();
      }
   }

   glEnable(GL_CULL_FACE);
   glDisable(GL_BLEND);
   glDisable(GL_STENCIL_TEST);
   return(1);
}
