/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Projection cylindrique de la carte vectorielle.
 * Fichier   : ProjCylin.c
 * Creation  : Juillet 98 - J.P. Gauthier - CMC/CMOE
*
 * Description: Definit tout ce qui se rapporte a la vue en cours ainsi que
 *              toute les procedures de creations, manipulation et configurations.
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
 * Modification:
 *
 *   Nom         : J.P.Gauthier
 *   Date        : Septembre 2000
 *   Description : Ajout d'une partie qui gere les projections Mercator
 *
 *==============================================================================
 */

#include <math.h>
#include "Projection.h"
#include "GeoData.h"

/*Prototypes*/

int  Cylin_Init(Tcl_Interp *Interp);
void Cylin_DrawFirst(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
void Cylin_DrawLast(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
int  Cylin_Locate(Projection *Proj,double Lat,double Lon,int Undo);
void Cylin_Render(Projection *Proj,GLuint List,Vect3d *Data,unsigned int *Idx,char *Col,float* Tex,int Mode,int Nb,Vect3d V0,Vect3d V1);
void Cylin_Vertex(Vect3d Pix,Vect3d Prev,double Delta,int Mode);
void Merca_DrawFirst(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
int  Merca_Locate(Projection *Proj,double Lat,double Lon,int Undo);

/*Fonctions de transformations*/

unsigned long Cylin_Project(ProjParams *Params,GeoVect *Loc,GeoVect *Pix,long Nb);
int           Cylin_UnProject(ViewportItem *VP,ProjParams *Params,Coord *Loc,Vect3d Pix);
Tcl_Obj*      Cylin_ProjectPoint(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord Pt1,int Any);
Tcl_Obj*      Cylin_ProjectLine(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord *Co,int NCo);
int           Cylin_SegLine(ViewportItem *VP,Projection *Proj,Coord Pt1,Coord Pt2,Vect3d Pix00,Vect3d Pix01,Vect3d Pix10,Vect3d Pix11);
unsigned long Merca_Project(ProjParams *Params,GeoVect *Loc,GeoVect *Pix,long Nb);
int           Merca_UnProject(ViewportItem *VP,ProjParams *Params,Coord *Loc,Vect3d Pix);

/*----------------------------------------------------------------------------
 * Nom      : <Cylin_DrawFirst>
 * Creation : Juillet 98 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Dessine le pourtour du globe.
 *
 * Parametres    :
 *  <Tcl_Interp> : Interpreteur Tcl
 *  <VP>         : Parametres du viewport
 *  <Proj>       : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
void Cylin_DrawFirst(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj){

   double  x,y,incr;
   char    buf[256];
   Vect3d  vr;
   Coord   co;

   if (Proj->Geo->Params.Coast) {
      if (Interp) {
         glFeedbackInit(20,GL_2D);
         if (VP->ColorFLake && VP->ColorFCoast) {
            Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorFLake);
         } else {
            Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorCoast);
         }
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",ABS(Proj->Geo->Params.Coast)-1);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
      }

      /*Pourtour de la carte*/
      if (VP->ColorFLake && VP->ColorFCoast) {
         glColor3us(VP->ColorFLake->red,VP->ColorFLake->green,VP->ColorFLake->blue);
         glDisable(GL_STENCIL_TEST);
         glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
         glBegin(GL_POLYGON);
            glVertex3d(-2.0+Proj->Params->L,-1.0,1.0);
            glVertex3d(2.0+Proj->Params->L,-1.0,1.0);
            glVertex3d(2.0+Proj->Params->L,1.0,1.0);
            glVertex3d(-2.0+Proj->Params->L,1.0,1.0);
            glVertex3d(-2.0+Proj->Params->L,-1.0,1.0);
         glEnd();
         glEnable(GL_STENCIL_TEST);

         if (Interp)
            glFeedbackProcess(Interp,GL_2D);
      }

      glColor3us(VP->ColorCoast->red,VP->ColorCoast->green,VP->ColorCoast->blue);
      glLineWidth(ABS(Proj->Geo->Params.Coast));
      glBegin(GL_LINE_STRIP);
         glVertex3d(-2.0+Proj->Params->L,-1.0,1.0);
         glVertex3d(2.0+Proj->Params->L,-1.0,1.0);
         glVertex3d(2.0+Proj->Params->L,1.0,1.0);
         glVertex3d(-2.0+Proj->Params->L,1.0,1.0);
         glVertex3d(-2.0+Proj->Params->L,-1.0,1.0);
      glEnd();

      if (Interp)
         glFeedbackProcess(Interp,GL_2D);
   }

   /*Latlons*/
   if (Proj->Geo->Params.CoordLoc) {

      glLineWidth(ABS(Proj->Geo->Params.CoordLoc));
      glColor3us(VP->ColorCoord->red,VP->ColorCoord->green,VP->ColorCoord->blue);

      if (Interp) {
         glFeedbackInit(MAXGEOSEG*2,GL_2D);
         Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorCoord);
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",ABS(Proj->Geo->Params.CoordLoc)-1);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
      }

      glBegin(GL_LINES);
         incr=4.0*(Proj->Geo->Params.CoordDef/360.0);
         for(x=-2.0+Proj->Params->L-fmod(Proj->Params->L,incr);x<=2.0+Proj->Params->L;x+=incr){
            glVertex3d(x,-1.0,1.0);
            glVertex3d(x,1.0,1.0);
         }

         for(y=-1.0;y<=1.0;y+=incr){
            glVertex3d(-2.0+Proj->Params->L,y,1.0);
            glVertex3d(2.0+Proj->Params->L,y,1.0);

         }
      glEnd();

      if (Interp)
         glFeedbackProcess(Interp,GL_2D);
   }
}

void Cylin_DrawLast(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj){

   double  x,y,incr;
   char    buf[16];
   Vect3d  vr;
   Coord   co;
   double  ax[2];

   /*Draw 3DAxis*/
   if (Proj->Params->TAxis && Proj->Params->ZAxis.elev!=0.0) {
      glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),VP->tkfont);
      glEnable(GL_DEPTH_TEST);
      glDisable(GL_CULL_FACE);
      glDisable(GL_STENCIL_TEST);
      glEnable(GL_BLEND);
      glPolygonOffset(0.5,1.0);
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glMatrixMode(GL_MODELVIEW);
      glLineWidth(1.0);

      incr=pow(10,ORDER(Proj->Params->ZAxis.elev)-1);
      incr=(Proj->Params->ZAxis.elev/incr)>10?incr*2.5:incr;
      incr=(Proj->Params->ZAxis.elev/incr)>10?incr*2:incr;
      incr=(Proj->Params->ZAxis.elev/incr)>10?incr*2:incr;

      co.lat=Proj->Params->ZAxis.lat;
      co.lon=Proj->Params->ZAxis.lon;
      co.elev=Proj->Params->ZAxis.elev;
      Proj->Type->Project(Proj->Params,&co,&vr,1);
      PROJCHECK(Proj,vr[0])

      switch(Proj->Params->TAxis) {
         case 1: ax[0]=vr[0]; ax[1]=vr[1]; break;
         case 2: ax[0]=-2.0+Proj->Params->L; ax[1]=1.0; break;
      }

      /*Draw Z axis planes*/
      glEnable(GL_POLYGON_OFFSET_FILL);
      glBegin(GL_QUADS);
         glColor3us(0xAAAA,0xAAAA,0xAAAA);
         glVertex3d(vr[0],-1.0,1.0);
         glVertex3d(vr[0],-1.0,vr[2]);
         glVertex3d(vr[0],ax[1],vr[2]);
         glVertex3d(vr[0],ax[1],1.0);

         glColor3us(0xDDDD,0xDDDD,0xDDDD);
         glVertex3d(ax[0],vr[1],1.0);
         glVertex3d(ax[0],vr[1],vr[2]);
         glVertex3d(2.0+Proj->Params->L,vr[1],vr[2]);
         glVertex3d(2.0+Proj->Params->L,vr[1],1.0);
      glEnd();
      glDisable(GL_POLYGON_OFFSET_FILL);

      /*Draw Z axis increments*/
      glColor3us(VP->ColorCoast->red,VP->ColorCoast->green,VP->ColorCoast->blue);
      for(co.elev=0;co.elev<=Proj->Params->ZAxis.elev;co.elev+=incr) {
         glBegin(GL_LINES);
            Proj->Type->Project(Proj->Params,&co,&vr,1);
            PROJCHECK(Proj,vr[0])
            glVertex3d(vr[0],-1.0,vr[2]);
            glVertex3d(vr[0],ax[1],vr[2]);
            glVertex3d(ax[0],vr[1],vr[2]);
            glVertex3d(2.0+Proj->Params->L,vr[1],vr[2]);
         glEnd();

         glPushMatrix();
         Proj->Type->Locate(Proj,co.lat,co.lon,1);
         glTranslated(0.0,0.0,ZM(Proj,co.elev));
         glRotatef(90.0,1.0,0.0,0.0);
         glScalef(VP->Ratio,VP->Ratio,1.0);
         sprintf(buf,"  %i",(int)co.elev);
         glPrint(Interp,VP->canvas,buf,vr[0],vr[1],0);
         glPopMatrix();
      }

      glDisable(GL_BLEND);
      glEnable(GL_CULL_FACE);
      glEnable(GL_STENCIL_TEST);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Cylin_Init>
 * Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Creer la vue, la secen et les commandes.
 *
 * Parametres    :
 *  <Tcl_Interp> : Interpreteur Tcl
 *
 * Retour     :
 *  <TCL_...> : Code de retour standard TCL
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
int Cylin_Init(Tcl_Interp *Interp){

   Projection_CreateType("cylindric",PROJCYLIN,
      Cylin_Locate,
      Cylin_Render,
      Cylin_DrawFirst,
      Cylin_DrawLast,
      Cylin_UnProject,
      Cylin_Project,
      Cylin_ProjectPoint,
      Cylin_ProjectLine);

   Projection_CreateType("mercator",PROJCYLIN,
      Merca_Locate,
      Cylin_Render,
      Merca_DrawFirst,
      Cylin_DrawLast,
      Merca_UnProject,
      Merca_Project,
      Cylin_ProjectPoint,
      Cylin_ProjectLine);

   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <Cylin_Locate>
 * Creation : Aout 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Centrer la projection sur une coordonnee particuliere.
 *
 * Parametres :
 *  <Proj>    : Parametres de la projection
 *  <Lat>     : Latitude
 *  <Lon>     : Longitude
 *  <Undo>    : Localisation inverse
 *
 * Retour     :
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
int Cylin_Locate(Projection *Proj,double Lat,double Lon,int Undo) {

   if (Undo) {
      if ((Lon-Proj->Params->Lon)<180.0) {
         Lon+=360.0;
      }
      if ((Lon-Proj->Params->Lon)>180.0) {
         Lon-=360.0;
      }
      glTranslated(Lon/90.0,Lat/90.0,0.0);
   } else {
      glTranslated(-Lon/90.0,-Lat/90.0,0.0);
   }
   Proj->Params->L=Proj->Params->Lon/90.0;

   return 1;
}

/*----------------------------------------------------------------------------
 * Nom      : <Cylin_Vertex>
 * Creation : Avril 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Positionner correctement un vertex.
 *
 * Parametres :
 *  <Pix>     : Vecteur position
 *  <Prev>    : Vecteur precedente
 *  <Delta>   : Distance du point precedent
 *  <Mode>    : Mode de creation des primitives
 *
 * Retour:
 *
 * Remarques :
 *    -l'algorithme procede segment par segment et les evalues pour la longueur,
 *     la visibilitee et le clipping
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
void Cylin_Vertex(Vect3d Pix,Vect3d Prev,double Delta,int Mode) {

   double d,x;
   Vect3d in0,in1;

   Vect_Assign(in0,Prev);
   Vect_Assign(in1,Pix);

   d=in0[0]-Delta;
   CYLCHECK(d,in0[0]);

   d=in1[0]-Delta;
   CYLCHECK(d,in1[0]);

   /*Si on change de cote +-*/
   if (fabs(in1[0]-in0[0])>2.0) {

      x=(in1[0]-Delta)<0?2.0:-2.0;
      d=DELTA(Prev,Pix);

      glVertex3d(Delta+x,in0[1]+(((Delta+x)-in0[0])*d),in0[2]);
      glEnd();
      glBegin(Mode);
      glVertex3d(Delta-x,in1[1]+(((Delta-x)-in1[0])*d),in1[2]);
   }
   glVertex3dv(in1);
}

void Cylin_RenderArray(Projection *Proj,Vect3d *Data,char *Col,float* Tex,int Mode,int Nb) {

   int    i,stride;
   Vect3d p0;

   if (Nb<0) {
      stride=-Nb-1;
      Nb=-Nb;
   } else {
      stride=1;
   }

   Vect_Assign(p0,Data[0]);

   if (Mode==GL_LINES) {
      for(i=0;i<Nb;i+=stride) {
         glBegin(Mode);
            if (Col) glColor4ubv(&Col[i*4]);
            if (Tex) glTexCoord1f(Tex[i]);
            Cylin_Vertex(Data[i],p0,Proj->Params->L,Mode);
            i++;
            Cylin_Vertex(Data[i],Data[i-1],Proj->Params->L,Mode);
         glEnd();
       }
   } else if (Mode==GL_POINTS) {
      glBegin(Mode);
         for(i=0;i<Nb;i+=stride) {
             if (Col) glColor4ubv(&Col[i*4]);
             if (Tex) glTexCoord1f(Tex[i]);
             Cylin_Vertex(Data[i],p0,Proj->Params->L,Mode);
         }
      glEnd();
   } else {
      glBegin(Mode);
         Cylin_Vertex(Data[0],p0,Proj->Params->L,Mode);
         for(i=stride;i<Nb;i+=stride) {
            if (Col) glColor4ubv(&Col[i*4]);
            if (Tex) glTexCoord1f(Tex[i]);
            Cylin_Vertex(Data[i],Data[i-stride],Proj->Params->L,Mode);
         }
         if (stride>1)
            Cylin_Vertex(Data[Nb-1],Data[Nb-1-stride],Proj->Params->L,Mode);
      glEnd();
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Cylin_Render>
 * Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Affiche un tableau de "vertex".
 *
 * Parametres    :
 *  <Proj>       : Parametres de la projection
 *  <List>       : Liste d'affichage
 *  <Data>       : Listes des vecteurs
 *  <Mode>       : Mode de creation des primitives
 *  <Nb>         : Dimension du tableau
 *  <V0>         : Limite inferieure gauche
 *  <V1>         : Limite superieure droite
 *
 * Retour:
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/

void Cylin_Render(Projection *Proj,GLuint List,Vect3d *Data,unsigned int *Idx,char *Col,float* Tex,int Mode,int Nb,Vect3d V0,Vect3d V1) {

   int    stride=0;
   short  f0=0,f1=0;

   if (!V0 && Data) {
      Cylin_RenderArray(Proj,Data,Col,Tex,Mode,Nb);
      return;
   }

   if (Nb<0) {
      if (Nb<-2) {
         stride=-Nb-1;
         Nb=2;
      } else {
         stride=0;
         Nb=-Nb;
      }
   }

   if (Data) {
      glVertexPointer(3,GL_DOUBLE,stride*sizeof(double)*3,Data);

      /*Activer les couleurs par "vertex"*/
      if (Col) {
         glEnableClientState(GL_COLOR_ARRAY);
         glColorPointer(4,GL_UNSIGNED_BYTE,stride*4,Col);
      }
      if (Tex) {
         glEnableClientState(GL_TEXTURE_COORD_ARRAY);
         glTexCoordPointer(1,GL_FLOAT,stride,Tex);
      }
   }

   /*On verifie si les donnees depasse d'un cote*/
   if (V0) f0=CYLFLIP(Proj->Params->L,V0[0]);
   if (V1) f1=CYLFLIP(Proj->Params->L,V1[0]);

   if (f0!=0)    glTranslated(f0,0.0,0.0);
   if (Data)     { if (Idx) { glDrawElements(Mode,Nb,GL_UNSIGNED_INT,Idx); } else { glDrawArrays(Mode,0,Nb);} }
   if (List)     glCallList(List);
   if (f0!=0)    glTranslated(-f0,0.0,0.0);

   if (f1!=f0) {
      if (f1!=0)  glTranslated(f1,0.0,0.0);
      if (Data)   { if (Idx) { glDrawElements(Mode,Nb,GL_UNSIGNED_INT,Idx); } else { glDrawArrays(Mode,0,Nb);} }
      if (List)   glCallList(List);
      if (f1!=0)  glTranslated(-f1,0.0,0.0);
   }

   if (Data) {
      glDisableClientState(GL_COLOR_ARRAY);
      glDisableClientState(GL_TEXTURE_COORD_ARRAY);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Cylin_ProjectLine>
 * Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue les conversions latlon->pixel et inverses pour une ligne (linestrip).
 *            clipper a l'interieur du viewport
 *
 * Parametres    :
 *  <Tcl_Interp> : Interpreteur Tcl
 *  <VP>         : Parametres du viewport
 *  <Proj>       : Parametres de la projection
 *  <Elev>       : Altitude de la region
 *  <Coord>      : Liste des points de coordonnees en latlon
 *  <NbCoord>    : Nombre de coordonnees dans la liste
 *
 * Retour        :
 *  <liste>      : Retourne la liste { { x y x y ... } { x y xy ... } }
 *                 ou la premiere liste est le polygone droit et le deuxieme,
 *                 le polygone gauche.
 *  <TCL_...>    : Code de reussite TCL
 *
 * Remarques :
 *    -La liste de coordonnees definissant la region doit etre fermee, les premieres et dernieres
 *     coordonnees pareilles
 *
 * Modifications :
 *
 *    Nom         : J.P.Gauthier
 *    Date        : Fevrier 1998
 *    Description : Les segments sont maintenant "clipper" a l'interieur du
 *                  viewport.
 *
 *    Nom         : J.P. Gauthier
 *    Date        : Avril 2000
 *    Description : Simplification de l'algorithme et mise en commmun d'une partie
 *                  avec la fonction de ligne dans SegLine
 *----------------------------------------------------------------------------
*/
Tcl_Obj* Cylin_ProjectLine(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord *Co,int NCo){

   Tcl_Obj *obj,*objf,*objb;
   Coord  prev;
   Vect3d pix00,pix01,pix10,pix11;
   Vect3d pixs[2][2000],pixc[2000];
   int    i=0,side=0,cut,n[2],c;

   obj=Tcl_NewListObj(0,NULL);
   objf=Tcl_NewListObj(0,NULL);
   objb=Tcl_NewListObj(0,NULL);

   n[0]=0;
   n[1]=0;

   /*Boucler sur tout les points definissant la region*/
   prev=Co[0];

   for(i=1;i<NCo;i++) {

      cut=Cylin_SegLine(VP,Proj,prev,Co[i],pix00,pix01,pix10,pix11);
      Vect_Assign(pixs[side][n[side]],pix00);
      n[side]++;
      Vect_Assign(pixs[side][n[side]],pix01);
      n[side]++;

      if (cut) {
         side=side==0?1:0;
         Vect_Assign(pixs[side][n[side]],pix10);
         n[side]++;
         Vect_Assign(pixs[side][n[side]],pix11);
         n[side]++;
      }

      prev=Co[i];

      if (n[0]>2000 || n[1]>2000) {
         Tcl_AppendResult(Interp,"Cylin_ProjectLine: Too many coordinates (Max:2000)",(char*)NULL);
         n[0]=n[1]=0;
         break;
      }
   }

   /*Creer les listes des points*/

   if (n[0]) {
      LiangBarsky_PolygonClip2D(pixs[0],n[0],pixc,&c,0,0,VP->Width,VP->Height);
      for (i=0;i<c;i++){
         Tcl_ListObjAppendElement(Interp,objf,Tcl_NewDoubleObj(pixc[i][0]+VP->x));
         Tcl_ListObjAppendElement(Interp,objf,Tcl_NewDoubleObj(pixc[i][1]+VP->y));
      }
   }

   if (n[1]) {
      LiangBarsky_PolygonClip2D(pixs[1],n[1],pixc,&c,0,0,VP->Width,VP->Height);
      for (i=0;i<c;i++){
         Tcl_ListObjAppendElement(Interp,objb,Tcl_NewDoubleObj(pixc[i][0]+VP->x));
         Tcl_ListObjAppendElement(Interp,objb,Tcl_NewDoubleObj(pixc[i][1]+VP->y));
      }
   }

   Tcl_ListObjAppendElement(Interp,obj,objf);
   Tcl_ListObjAppendElement(Interp,obj,objb);

   return(obj);
}

/*----------------------------------------------------------------------------
 * Nom      : <Cylin_ProjectPoint>
 * Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Projection d'un point.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *  <Pt1>     : Point de depart en latlon
 *  <Any>     : Anywhere (In and Out)
 *
 * Retour:
 *  <TCL_...> : Code de reussite TCL
 *
 * Remarques :
 *    -Retourne une liste de coordonnees si tout est
 *     correct, sinon, retourne une liste vide a Tcl.
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
Tcl_Obj* Cylin_ProjectPoint(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord Pt1,int Any){

   Tcl_Obj *obj;
   Vect3d   pix,in;
   float    d;

   obj=Tcl_NewListObj(0,NULL);

   Proj->Type->Project(Proj->Params,&Pt1,&in,1);

   d=in[0]-Proj->Params->L;   /*Test le depassement*/
   CYLCHECK(d,in[0]);

   gluProject(in[0],in[1],in[2],VP->GLModR,VP->GLProj,VP->GLView,&pix[0],&pix[1],&pix[2]);

   /*Repositionner dans le referentiel de Tcl*/
   pix[1]=VP->Height-pix[1];

   if (Any || INSIDE(pix,0,0,VP->Width,VP->Height)) {
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pix[0]+VP->x));
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pix[1]+VP->y));
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pix[2]));
   }
   return(obj);
}

/*----------------------------------------------------------------------------
 * Nom      : <Cylin_SegLine>
 * Creation : Avril 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Projeter la ligne selon les parametres de la projection et la sectionner
 *            si le besoin est.
 *
 * Parametres :
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *  <Pt1>     : Point de depart en latlon
 *  <Pt2>     : Point final en latlon
 *  <Pix00>   : Pixel de depart du premier segment
 *  <Pix01>   : Pixel de fin du premier segment
 *  <Pix10>   : Pixel de depart du deuxiemer segment
 *  <Pix11>   : Pixel de fin du deuxieme segment
 *
 * Retour:
 *  <int>     : Resultat de segmentation (1=segment, 0=pas segment)
 *
 * Remarques :
 *    -Si il n'y a pas  de segmentation, seulement la premiere paire de point sera
 *     definie.
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
int Cylin_SegLine(ViewportItem *VP,Projection *Proj,Coord Pt1,Coord Pt2,Vect3d Pix00,Vect3d Pix01,Vect3d Pix10,Vect3d Pix11) {

   double m,d,x;
   Vect3d in[2];
   Coord  co[2];

   co[0].lat=Pt1.lat;co[0].lon=Pt1.lon;
   co[1].lat=Pt2.lat;co[1].lon=Pt2.lon;

   CLAMPLAT(co[0].lat);
   CLAMPLAT(co[1].lat);

   /*Localisation des extremites de la ligne*/
   Proj->Type->Project(Proj->Params,co,in,2);

   m=DELTA(in[0],in[1]);

   d=in[0][0]-Proj->Params->L;        /*Test le depassement*/
   CYLCHECK(d,in[0][0]);

   d=in[1][0]-Proj->Params->L;        /*Test le depassement*/
   CYLCHECK(d,in[1][0]);

   /*Si on change de cote +-*/
   if (fabs(in[1][0]-in[0][0])>2.0) {

      x=(in[1][0]-Proj->Params->L)<0.0?2.0:-2.0;

      gluProject(in[0][0],in[0][1],in[0][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix00[0],&Pix00[1],&Pix00[2]);
      Pix00[1]=Proj->Params->VP->Height-Pix00[1];
      gluProject(Proj->Params->L+x,in[0][1]+(((Proj->Params->L+x)-in[0][0])*m),in[0][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix01[0],&Pix01[1],&Pix01[2]);
      Pix01[1]=Proj->Params->VP->Height-Pix01[1];
      gluProject(Proj->Params->L-x,in[1][1]+(((Proj->Params->L-x)-in[1][0])*m),in[1][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix10[0],&Pix10[1],&Pix10[2]);
      Pix10[1]=Proj->Params->VP->Height-Pix10[1];
      gluProject(in[1][0],in[1][1],in[1][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix11[0],&Pix11[1],&Pix11[2]);
      Pix11[1]=Proj->Params->VP->Height-Pix11[1];
      return(1);
   } else {
      gluProject(in[0][0],in[0][1],in[0][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix00[0],&Pix00[1],&Pix00[2]);
      Pix00[1]=Proj->Params->VP->Height-Pix00[1];
      gluProject(in[1][0],in[1][1],in[1][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix01[0],&Pix01[1],&Pix01[2]);
      Pix01[1]=Proj->Params->VP->Height-Pix01[1];
      return(0);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Cylin_Project>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Transforme les coordonnes lat-lon en projection cylindrique.
 *
 * Parametres :
 *  <Params>  : Parametres de la projection
 *  <Loc>     : Coordonnees lat lon du point
 *  <Pix>     : Coordonnees cartesienne du point
 *  <Nb>      : Nombre de coordonnees a transformer
 *
 * Retour:
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
unsigned long Cylin_Project(ProjParams *Params,GeoVect *Loc,GeoVect *Pix,long Nb) {

   long n,e=0;
   Coord   loc;
   Vect3d *out;

   out=(Vect3d*)(Pix?Pix:Loc);

   for(n=0;n<ABS(Nb);n++) {

      if (((Coord*)Loc)[n].lat==-999.0 || ((Coord*)Loc)[n].lon==-999.0) {
         out[n][2]=-999.0;
      } else {
         loc.lat=((Coord*)Loc)[n].lat;
         loc.lon=((Coord*)Loc)[n].lon;
         loc.elev=((Coord*)Loc)[n].elev;
         CLAMPLON(loc.lon);

         out[n][0]=loc.lon/90.0;
         out[n][1]=loc.lat/90.0;
         out[n][2]=1.0+loc.elev*Params->Scale*Params->ZFactor;
         e++;
      }
   }
   return(e);
}

/*----------------------------------------------------------------------------
 * Nom      : <Cylin_UnProject>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Transforme les coordonnes cylindrique en lat-lon.
 *
 * Parametres :
 *  <VP>      : Parametres du viewport
 *  <Params>  : Parametres de la projection
 *  <Loc>     : Coordonne lat lon du point
 *  <Pix>     : Coordonee cartesienne du point
 *
 * Retour:
 *  <int>     : Coordonee Valide ou non
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
int Cylin_UnProject(ViewportItem *VP,ProjParams *Params,Coord *Loc,Vect3d Pix) {

   Vect3d obj;
   double depth=1.0;

   gluUnProject(Pix[0],VP->Height-Pix[1],depth,VP->GLModS,VP->GLProj,VP->GLView,&obj[0],&obj[1],&obj[2]);

   if (Vect_InterPlane(VP->Cam->Basis,obj,1)) {

      Loc->lon=Params->Lon+obj[0]*90.0;
      Loc->lat=Params->Lat+obj[1]*90.0;

      if (Loc->lon<=(180.0+Params->Lon) && Loc->lon>=(-180.0+Params->Lon) && Loc->lat<=90.0 && Loc->lat>=-90.0) {
         return 1;
      }
   }
   Loc->lat=-999.0;
   Loc->lon=-999.0;

   return 0;
}

/*----------------------------------------------------------------------------
 * Nom      : <Merca_DrawFirst>
 * Creation : Juillet 98 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Dessine le pourtour du globe.
 *
 * Parametres    :
 *  <Tcl_Interp> : Interpreteur Tcl
 *  <VP>         : Parametres du viewport
 *  <Proj>       : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
void Merca_DrawFirst(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj){

   double  x,y,incr;
   char    buf[256];

   if (Proj->Geo->Params.Coast) {
      if (Interp) {
         glFeedbackInit(20,GL_2D);
         if (VP->ColorFLake && VP->ColorFCoast) {
            Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorFLake);
         } else {
            Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorCoast);
         }
         Tcl_AppendResult(Interp,"0 setlinewidth 1 setlinecap 1 setlinejoin\n",(char*)NULL);
      }

      /*Pourtour de la carte*/
      if (VP->ColorFLake && VP->ColorFCoast) {
         glColor3us(VP->ColorFLake->red,VP->ColorFLake->green,VP->ColorFLake->blue);
         glDisable(GL_STENCIL_TEST);
         glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
         glBegin(GL_POLYGON);
            glVertex3d(-2.0+Proj->Params->L,-1.0,1.0);
            glVertex3d(2.0+Proj->Params->L,-1.0,1.0);
            glVertex3d(2.0+Proj->Params->L,1.0,1.0);
            glVertex3d(-2.0+Proj->Params->L,1.0,1.0);
            glVertex3d(-2.0+Proj->Params->L,-1.0,1.0);
         glEnd();
         glEnable(GL_STENCIL_TEST);

         if (Interp)
            glFeedbackProcess(Interp,GL_2D);
      }

      glColor3us(VP->ColorCoast->red,VP->ColorCoast->green,VP->ColorCoast->blue);
      glLineWidth(ABS(Proj->Geo->Params.Coast));
      glBegin(GL_LINE_STRIP);
         glVertex3d(-2.0+Proj->Params->L,-1.0,1.0);
         glVertex3d(2.0+Proj->Params->L,-1.0,1.0);
         glVertex3d(2.0+Proj->Params->L,1.0,1.0);
         glVertex3d(-2.0+Proj->Params->L,1.0,1.0);
         glVertex3d(-2.0+Proj->Params->L,-1.0,1.0);
      glEnd();

      if (Interp)
         glFeedbackProcess(Interp,GL_2D);
   }

   /*Latlons*/
   if (Proj->Geo->Params.CoordLoc) {

      if (Interp) {
         glFeedbackInit(MAXGEOSEG*2,GL_2D);
         Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorCoord);
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",ABS(Proj->Geo->Params.CoordLoc)-1);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
      }

      glLineWidth(ABS(Proj->Geo->Params.CoordLoc));
      glBegin(GL_LINES);
         incr=4.0*(Proj->Geo->Params.CoordDef/360.0);
         for(x=-2.0+Proj->Params->L-fmod(Proj->Params->L,incr);x<=2.0+Proj->Params->L;x+=incr){
            glVertex3d(x,-1.0,1.0);
            glVertex3d(x,1.0,1.0);
         }

         for(y=-80;y<=80;y+=Proj->Geo->Params.CoordDef){
            if (y>=-78 && y<=78) {
               incr=log(tan(M_PI4 + y/180.0));
               glVertex3d(-2.0+Proj->Params->L,incr,1.0);
               glVertex3d(2.0+Proj->Params->L,incr,1.0);
            }
         }
      glEnd();

      if (Interp)
         glFeedbackProcess(Interp,GL_2D);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Merca_Locate>
 * Creation : Aout 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Centrer la projection sur une coordonnee particuliere.
 *
 * Parametres :
 *  <Proj>    : Parametres de la projection
 *  <Lat>     : Latitude
 *  <Lon>     : Longitude
 *  <Undo>    : Localisation inverse
 *
 * Retour     :
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
int Merca_Locate(Projection *Proj,double Lat,double Lon,int Undo) {

   if (Undo) {
      if ((Lon-Proj->Params->Lon)<180.0) {
         Lon+=360.0;
      }
      if ((Lon-Proj->Params->Lon)>180.0) {
         Lon-=360.0;
      }
      glTranslated(Lon/90.0,log(tan(M_PI4 + Lat/180.0)),0.0);
   } else {
      glTranslated(-Lon/90.0,-log(tan(M_PI4 + Lat/180.0)),0.0);
   }
   Proj->Params->L=Proj->Params->Lon/90.0;

   return 1;
}

/*----------------------------------------------------------------------------
 * Nom      : <Merca_Project>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Transforme les coordonnes lat-lon en projection mercator.
 *
 * Parametres :
 *  <Params>  : Parametres de la projection
 *  <Loc>     : Coordonnees lat lon du point
 *  <Pix>     : Coordonnees cartesienne du point
 *  <Nb>      : Nombre de coordonnees a transformer
 *
 * Retour:
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
unsigned long Merca_Project(ProjParams *Params,GeoVect *Loc,GeoVect *Pix,long Nb) {

   long n,e=0;
   Coord   loc;
   Vect3d *out;

   out=(Vect3d*)(Pix?Pix:Loc);

   for(n=0;n<ABS(Nb);n++) {

      if (((Coord*)Loc)[n].lat==-999.0 || ((Coord*)Loc)[n].lon==-999.0) {
         out[n][2]=-999.0;
      } else {
         loc.lat=((Coord*)Loc)[n].lat;
         loc.lon=((Coord*)Loc)[n].lon;
         loc.elev=((Coord*)Loc)[n].elev;
         CLAMPLON(loc.lon);

         out[n][0]=loc.lon/90.0;
         out[n][1]=log(tan(M_PI4 + (loc.lat>78.0?78.0:(loc.lat<-78.0?-78.0:loc.lat))/180.0));
         out[n][2]=1.0+loc.elev*Params->Scale*Params->ZFactor;
         e++;
      }
   }
   return(e);
}

/*----------------------------------------------------------------------------
 * Nom      : <Merca_UnProject>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Transforme les coordonnes mercator en lat-lon.
 *
 * Parametres :
 *  <Loc>     : Coordonne lat lon du point
 *  <Pix>     : Coordonee cartesienne du point
 *  <Params>  : Parametres de la projection
 *
 * Retour:
 *  <int>     : Coordonee Valide ou non
 *
 * Remarques :
 *
 * Modifications :
 *
 *    Nom         :
 *    Date        :
 *    Description :
 *----------------------------------------------------------------------------
*/
int Merca_UnProject(ViewportItem *VP,ProjParams *Params,Coord *Loc,Vect3d Pix) {

   Vect3d obj;
   double depth=1.0;

   gluUnProject(Pix[0],VP->Height-Pix[1],depth,VP->GLModS,VP->GLProj,VP->GLView,&obj[0],&obj[1],&obj[2]);

   if (Vect_InterPlane(VP->Cam->Basis,obj,1)) {

      Loc->lon=Params->Lon+obj[0]*90.0;
      Loc->lat=(atan(exp(obj[1]+log(tan(M_PI4 + Params->Lat/180.0))))-M_PI4)*180.0;

      if (Loc->lon<=(180.0+Params->Lon) && Loc->lon>=(-180.0+Params->Lon) && Loc->lat<=78.0 && Loc->lat>=-78.0) {
         return(1);
      }
   }
   Loc->lat=-999.0;
   Loc->lon=-999.0;

   return(0);
}

