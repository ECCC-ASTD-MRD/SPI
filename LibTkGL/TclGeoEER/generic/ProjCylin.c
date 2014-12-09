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
 *==============================================================================
 */

#include <math.h>
#include "Projection.h"
#include "GeoData.h"

/*Prototypes*/
int  Cylin_Init(Tcl_Interp *Interp);
void Cylin_DrawFirst(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
void Cylin_DrawLast(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
void Cylin_DrawGlobe(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
int  Cylin_Locate(Projection *Proj,double Lat,double Lon,int Undo);
void Cylin_Render(Projection *Proj,GLuint List,Vect3d *Data,unsigned int *Idx,char *Col,float* Tex,int Mode,int Nb,int Stride,Vect3d V0,Vect3d V1);
void Merca_DrawFirst(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
int  Merca_Locate(Projection *Proj,double Lat,double Lon,int Undo);

static inline void Cylin_Vertex(Vect3d Pix,Vect3d Prev,double Delta,int Mode);

/*Fonctions de transformations*/
unsigned long Cylin_Project(const Projection* restrict const Proj,GeoVect *Loc,GeoVect *Pix,long Nb);
int           Cylin_UnProject(ViewportItem *VP,Projection *Proj,Coord *Loc,Vect3d Pix);
Tcl_Obj*      Cylin_ProjectPoint(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord Pt1,int Any);
Tcl_Obj*      Cylin_ProjectLine(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord *Co,int NCo);
int           Cylin_SegLine(ViewportItem *VP,Projection *Proj,Coord Pt1,Coord Pt2,Vect3d Pix00,Vect3d Pix01,Vect3d Pix10,Vect3d Pix11);
unsigned long Merca_Project(const Projection* restrict const Proj,GeoVect *Loc,GeoVect *Pix,long Nb);
int           Merca_UnProject(ViewportItem *VP,Projection *Proj,Coord *Loc,Vect3d Pix);

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
 *----------------------------------------------------------------------------
*/
void Cylin_DrawGlobe(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj){

   char    buf[256];

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
         glVertex3d(-2.0+Proj->L,-1.0,1.0);
         glVertex3d(2.0+Proj->L,-1.0,1.0);
         glVertex3d(2.0+Proj->L,1.0,1.0);
         glVertex3d(-2.0+Proj->L,1.0,1.0);
         glVertex3d(-2.0+Proj->L,-1.0,1.0);
      glEnd();
      glEnable(GL_STENCIL_TEST);

      if (Interp)
         glFeedbackProcess(Interp,GL_2D);
   }

   glColor3us(VP->ColorCoast->red,VP->ColorCoast->green,VP->ColorCoast->blue);
   glLineWidth(ABS(Proj->Geo->Params.Coast));
   glBegin(GL_LINE_STRIP);
      glVertex3d(-2.0+Proj->L,-1.0,1.0);
      glVertex3d(2.0+Proj->L,-1.0,1.0);
      glVertex3d(2.0+Proj->L,1.0,1.0);
      glVertex3d(-2.0+Proj->L,1.0,1.0);
      glVertex3d(-2.0+Proj->L,-1.0,1.0);
   glEnd();

   if (Interp)
      glFeedbackProcess(Interp,GL_2D);
}

void Cylin_DrawFirst(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj){

   double  x,y,incr;
   char    buf[256];

   /*Latlons*/
   if (Proj->Geo->Params.CoordLoc && VP->ColorCoord) {

     if (Interp) {
         glFeedbackInit(MAXGEOSEG*2,GL_2D);
         Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorCoord);
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",ABS(Proj->Geo->Params.CoordLoc)-1);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
         glPostscriptDash(Interp,&VP->DashCoord,ABS(Proj->Geo->Params.CoordLoc)-1);
      } else {
         glDash(&VP->DashCoord);
         glLineWidth(ABS(Proj->Geo->Params.CoordLoc));
         glColor3us(VP->ColorCoord->red,VP->ColorCoord->green,VP->ColorCoord->blue);
      }

      glBegin(GL_LINES);
         incr=4.0*(Proj->Geo->Params.CoordDef/360.0);
         for(x=-2.0+Proj->L-fmod(Proj->L,incr);x<=2.0+Proj->L;x+=incr){
            glVertex3d(x,-1.0,1.0);
            glVertex3d(x,1.0,1.0);
         }

      for(y=floor(-1.0/incr)*incr+incr;y<=1.0;y+=incr){
            glVertex3d(-2.0+Proj->L,y,1.0);
            glVertex3d(2.0+Proj->L,y,1.0);

         }
      glEnd();

      if (Interp)
         glFeedbackProcess(Interp,GL_2D);
   }
}

void Cylin_DrawLast(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj){

   char    buf[16];
   Vect3d  vr;
   Coord   co;
   double  incr,ax[2];

   /*Draw 3DAxis*/
   if (Proj->TAxis && Proj->ZAxis.Elev!=0.0) {
      glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),VP->tkfont);
      glEnable(GL_DEPTH_TEST);
      glDisable(GL_CULL_FACE);
      glDisable(GL_STENCIL_TEST);
      glEnable(GL_BLEND);
      glPolygonOffset(0.5,1.0);
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glMatrixMode(GL_MODELVIEW);
      glLineWidth(1.0);

      incr=pow(10,ORDER(Proj->ZAxis.Elev)-1);
      incr=(Proj->ZAxis.Elev/incr)>10?incr*2.5:incr;
      incr=(Proj->ZAxis.Elev/incr)>10?incr*2:incr;
      incr=(Proj->ZAxis.Elev/incr)>10?incr*2:incr;

      co.Lat=Proj->ZAxis.Lat;
      co.Lon=Proj->ZAxis.Lon;
      co.Elev=Proj->ZAxis.Elev;
      Proj->Type->Project(Proj,(GeoVect*)&co,(GeoVect*)&vr,1);
      PROJCHECK(Proj,vr[0])

      ax[0]=ax[1]=0.0;
      switch(Proj->TAxis) {
         case 1: ax[0]=vr[0]; ax[1]=vr[1]; break;
         case 2: ax[0]=-2.0+Proj->L; ax[1]=1.0; break;
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
         glVertex3d(2.0+Proj->L,vr[1],vr[2]);
         glVertex3d(2.0+Proj->L,vr[1],1.0);
      glEnd();
      glDisable(GL_POLYGON_OFFSET_FILL);

      /*Draw Z axis increments*/
      glColor3us(VP->ColorCoast->red,VP->ColorCoast->green,VP->ColorCoast->blue);
      for(co.Elev=0;co.Elev<=Proj->ZAxis.Elev;co.Elev+=incr) {
         glBegin(GL_LINES);
            Proj->Type->Project(Proj,(GeoVect*)&co,(GeoVect*)&vr,1);
            PROJCHECK(Proj,vr[0])
            glVertex3d(vr[0],-1.0,vr[2]);
            glVertex3d(vr[0],ax[1],vr[2]);
            glVertex3d(ax[0],vr[1],vr[2]);
            glVertex3d(2.0+Proj->L,vr[1],vr[2]);
         glEnd();

         glPushMatrix();
         Proj->Type->Locate(Proj,co.Lat,co.Lon,1);
         glTranslated(0.0,0.0,ZM(Proj,co.Elev));
         glRotatef(90.0,1.0,0.0,0.0);
         glScalef(VP->Ratio,VP->Ratio,1.0);
         sprintf(buf,"  %i",(int)co.Elev);
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
 *----------------------------------------------------------------------------
*/
int Cylin_Init(Tcl_Interp *Interp){

   Projection_CreateType(Interp,"cylindric",PROJCYLIN,
      Cylin_Locate,
      Cylin_Render,
      Cylin_DrawFirst,
      Cylin_DrawLast,
      Cylin_DrawGlobe,
      Cylin_UnProject,
      Cylin_Project,
      Cylin_ProjectPoint,
      Cylin_ProjectLine);

   Projection_CreateType(Interp,"mercator",PROJCYLIN,
      Merca_Locate,
      Cylin_Render,
      Merca_DrawFirst,
      Cylin_DrawLast,
      Cylin_DrawGlobe,
      Merca_UnProject,
      Merca_Project,
      Cylin_ProjectPoint,
      Cylin_ProjectLine);

   return(TCL_OK);
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
 *----------------------------------------------------------------------------
*/
int Cylin_Locate(Projection *Proj,double Lat,double Lon,int Undo) {

   if (Undo) {
      if ((Lon-Proj->Lon)<180.0) {
         Lon+=360.0;
      }
      if ((Lon-Proj->Lon)>180.0) {
         Lon-=360.0;
      }
      glTranslated(Lon/90.0,Lat/90.0,0.0);
   } else {
      glTranslated(-Lon/90.0,-Lat/90.0,0.0);
   }
   Proj->L=Proj->Lon/90.0;

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
 *----------------------------------------------------------------------------
*/
static inline void Cylin_Vertex(Vect3d Pix,Vect3d Prev,double Delta,int Mode) {

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

void Cylin_RenderArray(Projection *Proj,Vect3d *Data,char *Col,float* Tex,int Mode,int Nb,int Stride) {

   int    i,n;
   Vect3d p0;

   Vect_Assign(p0,Data[0]);
   Stride=Stride==0?1:Stride;
   n=Nb*Stride;

   if (Mode==GL_LINES) {
      for(i=0;i<n;i+=Stride) {
         glBegin(Mode);
            if (Col) glColor4ubv((GLubyte*)&Col[i*4]);
            if (Tex) glTexCoord1f(Tex[i]);
            Cylin_Vertex(Data[i],p0,Proj->L,Mode);
            i++;
            Cylin_Vertex(Data[i],Data[i-1],Proj->L,Mode);
         glEnd();
       }
   } else if (Mode==GL_POINTS) {
      glBegin(Mode);
         for(i=0;i<n;i+=Stride) {
             if (Col) glColor4ubv((GLubyte*)&Col[i*4]);
             if (Tex) glTexCoord1f(Tex[i]);
             Cylin_Vertex(Data[i],p0,Proj->L,Mode);
         }
      glEnd();
   } else {
      glBegin(Mode);
         Cylin_Vertex(Data[0],p0,Proj->L,Mode);
         for(i=Stride;i<n;i+=Stride) {
            if (Col) glColor4ubv((GLubyte*)&Col[i*4]);
            if (Tex) glTexCoord1f(Tex[i]);
            Cylin_Vertex(Data[i],Data[i-Stride],Proj->L,Mode);
         }
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
 *----------------------------------------------------------------------------
*/

void Cylin_Render(Projection *Proj,GLuint List,Vect3d *Data,unsigned int *Idx,char *Col,float* Tex,int Mode,int Nb,int Stride,Vect3d V0,Vect3d V1) {

   short  f0=0,f1=0;

   if (!V0 && Data) {
      Cylin_RenderArray(Proj,Data,Col,Tex,Mode,Nb,Stride);
      return;
   }

   if (Data) {
      glVertexPointer(3,GL_DOUBLE,Stride*sizeof(double)*3,Data);

      /*Activer les couleurs par "vertex"*/
      if (Col) {
         glEnableClientState(GL_COLOR_ARRAY);
         glColorPointer(4,GL_UNSIGNED_BYTE,Stride*4,Col);
      }
      if (Tex) {
         glEnableClientState(GL_TEXTURE_COORD_ARRAY);
         glTexCoordPointer(1,GL_FLOAT,Stride,Tex);
      }
   }

   /*On verifie si les donnees depasse d'un cote*/
   if (V0) f0=CYLFLIP(Proj->L,V0[0]);
   if (V1) f1=CYLFLIP(Proj->L,V1[0]);

   if (f0)       glTranslated(f0,0.0,0.0);
   if (Data)     { if (Idx) { glDrawElements(Mode,Nb,GL_UNSIGNED_INT,Idx); } else { glDrawArrays(Mode,0,Nb);} }
   if (List)     glCallList(List);
   if (f0)       glTranslated(-f0,0.0,0.0);

   if (f1!=f0) {
      if (f1)     glTranslated(f1,0.0,0.0);
      if (Data)   { if (Idx) { glDrawElements(Mode,Nb,GL_UNSIGNED_INT,Idx); } else { glDrawArrays(Mode,0,Nb);} }
      if (List)   glCallList(List);
      if (f1)     glTranslated(-f1,0.0,0.0);
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
         if (!i || (pixc[i][0]!=pixc[i-1][0] || pixc[i][1]!=pixc[i-1][1])) {
            Tcl_ListObjAppendElement(Interp,objf,Tcl_NewDoubleObj(pixc[i][0]+VP->x));
            Tcl_ListObjAppendElement(Interp,objf,Tcl_NewDoubleObj(pixc[i][1]+VP->y));
         }
      }
   }

   if (n[1]) {
      LiangBarsky_PolygonClip2D(pixs[1],n[1],pixc,&c,0,0,VP->Width,VP->Height);
      for (i=0;i<c;i++){
         if (!i || (pixc[i][0]!=pixc[i-1][0] || pixc[i][1]!=pixc[i-1][1])) {
            Tcl_ListObjAppendElement(Interp,objb,Tcl_NewDoubleObj(pixc[i][0]+VP->x));
            Tcl_ListObjAppendElement(Interp,objb,Tcl_NewDoubleObj(pixc[i][1]+VP->y));
         }
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
 *----------------------------------------------------------------------------
*/
Tcl_Obj* Cylin_ProjectPoint(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord Pt1,int Any){

   Tcl_Obj *obj;
   Vect3d   pix,in;
   float    d;

   obj=Tcl_NewListObj(0,NULL);

   Proj->Type->Project(Proj,(GeoVect*)&Pt1,(GeoVect*)&in,1);

   d=in[0]-Proj->L;   /*Test le depassement*/
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
 *----------------------------------------------------------------------------
*/
int Cylin_SegLine(ViewportItem *VP,Projection *Proj,Coord Pt1,Coord Pt2,Vect3d Pix00,Vect3d Pix01,Vect3d Pix10,Vect3d Pix11) {

   double m,d,x;
   Vect3d in[2];
   Coord  co[2];

   co[0].Lat=Pt1.Lat;co[0].Lon=Pt1.Lon;
   co[1].Lat=Pt2.Lat;co[1].Lon=Pt2.Lon;

   CLAMPLAT(co[0].Lat);
   CLAMPLAT(co[1].Lat);

   /*Localisation des extremites de la ligne*/
   Proj->Type->Project(Proj,(GeoVect*)co,(GeoVect*)in,2);

   m=DELTA(in[0],in[1]);

   d=in[0][0]-Proj->L;        /*Test le depassement*/
   CYLCHECK(d,in[0][0]);

   d=in[1][0]-Proj->L;        /*Test le depassement*/
   CYLCHECK(d,in[1][0]);

   /*Si on change de cote +-*/
   if (fabs(in[1][0]-in[0][0])>2.0) {

      x=(in[1][0]-Proj->L)<0.0?2.0:-2.0;

      gluProject(in[0][0],in[0][1],in[0][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix00[0],&Pix00[1],&Pix00[2]);
      Pix00[1]=Proj->VP->Height-Pix00[1];
      gluProject(Proj->L+x,in[0][1]+(((Proj->L+x)-in[0][0])*m),in[0][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix01[0],&Pix01[1],&Pix01[2]);
      Pix01[1]=Proj->VP->Height-Pix01[1];
      gluProject(Proj->L-x,in[1][1]+(((Proj->L-x)-in[1][0])*m),in[1][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix10[0],&Pix10[1],&Pix10[2]);
      Pix10[1]=Proj->VP->Height-Pix10[1];
      gluProject(in[1][0],in[1][1],in[1][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix11[0],&Pix11[1],&Pix11[2]);
      Pix11[1]=Proj->VP->Height-Pix11[1];
      return(1);
   } else {
      gluProject(in[0][0],in[0][1],in[0][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix00[0],&Pix00[1],&Pix00[2]);
      Pix00[1]=Proj->VP->Height-Pix00[1];
      gluProject(in[1][0],in[1][1],in[1][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix01[0],&Pix01[1],&Pix01[2]);
      Pix01[1]=Proj->VP->Height-Pix01[1];
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
 *  <Proj>    : Parametres de la projection
 *  <Loc>     : Coordonnees lat lon du point
 *  <Pix>     : Coordonnees cartesienne du point
 *  <Nb>      : Nombre de coordonnees a transformer
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
unsigned long Cylin_Project(const Projection* restrict const Proj,GeoVect *Loc,GeoVect *Pix,long Nb) {

   Coord    loc;
   GeoVect *out;
   long     n,e=0;
   double   r;

   out=Pix?Pix:Loc;
   r=Proj->Scale*Proj->ZFactor;

   for(n=0;n<ABS(Nb);n++) {

      if (Loc[n].C.Lat==-999.0 || Loc[n].C.Lon==-999.0) {
         out[n].V[2]=-999.0;
      } else {
         loc.Lat=Loc[n].C.Lat;
         loc.Lon=Loc[n].C.Lon;
         loc.Elev=Loc[n].C.Elev;
         CLAMPLON(loc.Lon);

         out[n].V[0]=loc.Lon/90.0;
         out[n].V[1]=loc.Lat/90.0;
         out[n].V[2]=(loc.Elev==0.0)?1.0:1.0+loc.Elev*r;
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
 *  <Proj>    : Parametres de la projection
 *  <Loc>     : Coordonne lat lon du point
 *  <Pix>     : Coordonee cartesienne du point
 *
 * Retour:
 *  <int>     : Coordonee Valide ou non
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Cylin_UnProject(ViewportItem *VP,Projection *Proj,Coord *Loc,Vect3d Pix) {

   Vect3d obj;
   double depth=0.0;

   gluUnProject(Pix[0],VP->Height-Pix[1],depth,VP->GLModS,VP->GLProj,VP->GLView,&obj[0],&obj[1],&obj[2]);


   if (Vect_InterPlane(VP->Cam->Basis,obj,1)) {

      if (Proj->Perspective) {
         obj[0]*=obj[2];
         obj[1]*=obj[2];
      }
/*   VP->Cam->FOV[0]=atan2(obj[0],obj[2]);
   VP->Cam->FOV[1]=atan2(obj[1],obj[2]);
   obj[0]=tan(VP->Cam->FOV[0]);
   obj[1]=tan(VP->Cam->FOV[1]);*/

      Loc->Lon=Proj->Lon+obj[0]*90.0;
      Loc->Lat=Proj->Lat+obj[1]*90.0;

      if (Loc->Lon<=(180.0+Proj->Lon) && Loc->Lon>=(-180.0+Proj->Lon) && Loc->Lat<=90.0 && Loc->Lat>=-90.0) {
         return 1;
      }
   }
   Loc->Lat=-999.0;
   Loc->Lon=-999.0;

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
      if (VP->ColorFLake && VP->ColorFCoast && !Interp) {
         glColor3us(VP->ColorFLake->red,VP->ColorFLake->green,VP->ColorFLake->blue);
         glDisable(GL_STENCIL_TEST);
         glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
         glBegin(GL_POLYGON);
            glVertex3d(-2.0+Proj->L,-1.0,1.0);
            glVertex3d(2.0+Proj->L,-1.0,1.0);
            glVertex3d(2.0+Proj->L,1.0,1.0);
            glVertex3d(-2.0+Proj->L,1.0,1.0);
            glVertex3d(-2.0+Proj->L,-1.0,1.0);
         glEnd();
         glEnable(GL_STENCIL_TEST);

         if (Interp)
            glFeedbackProcess(Interp,GL_2D);
      }

      glColor3us(VP->ColorCoast->red,VP->ColorCoast->green,VP->ColorCoast->blue);
      glLineWidth(ABS(Proj->Geo->Params.Coast));
      glBegin(GL_LINE_STRIP);
         glVertex3d(-2.0+Proj->L,-1.0,1.0);
         glVertex3d(2.0+Proj->L,-1.0,1.0);
         glVertex3d(2.0+Proj->L,1.0,1.0);
         glVertex3d(-2.0+Proj->L,1.0,1.0);
         glVertex3d(-2.0+Proj->L,-1.0,1.0);
      glEnd();

      if (Interp)
         glFeedbackProcess(Interp,GL_2D);
   }

   /*Latlons*/
   if (Proj->Geo->Params.CoordLoc && VP->ColorCoord) {

      if (Interp) {
         glFeedbackInit(MAXGEOSEG*2,GL_2D);
         Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorCoord);
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",ABS(Proj->Geo->Params.CoordLoc)-1);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
      }

      glLineWidth(ABS(Proj->Geo->Params.CoordLoc));
      glBegin(GL_LINES);
         incr=4.0*(Proj->Geo->Params.CoordDef/360.0);
         for(x=-2.0+Proj->L-fmod(Proj->L,incr);x<=2.0+Proj->L;x+=incr){
            glVertex3d(x,-1.0,1.0);
            glVertex3d(x,1.0,1.0);
         }

         for(y=-80;y<=80;y+=Proj->Geo->Params.CoordDef){
            if (y>=-78 && y<=78) {
               incr=log(tan(M_PI4 + y/180.0));
               glVertex3d(-2.0+Proj->L,incr,1.0);
               glVertex3d(2.0+Proj->L,incr,1.0);
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
 *----------------------------------------------------------------------------
*/
int Merca_Locate(Projection *Proj,double Lat,double Lon,int Undo) {

   if (Undo) {
      if ((Lon-Proj->Lon)<180.0) {
         Lon+=360.0;
      }
      if ((Lon-Proj->Lon)>180.0) {
         Lon-=360.0;
      }
      glTranslated(Lon/90.0,log(tan(M_PI4 + Lat/180.0)),0.0);
   } else {
      glTranslated(-Lon/90.0,-log(tan(M_PI4 + Lat/180.0)),0.0);
   }
   Proj->L=Proj->Lon/90.0;

   return 1;
}

/*----------------------------------------------------------------------------
 * Nom      : <Merca_Project>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Transforme les coordonnes lat-lon en projection mercator.
 *
 * Parametres :
 *  <Proj>    : Parametres de la projection
 *  <Loc>     : Coordonnees lat lon du point
 *  <Pix>     : Coordonnees cartesienne du point
 *  <Nb>      : Nombre de coordonnees a transformer
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
unsigned long Merca_Project(const Projection* restrict const Proj,GeoVect *Loc,GeoVect *Pix,long Nb) {

   Coord    loc;
   GeoVect *out;
   long     n,e=0;
   double   r;

   out=Pix?Pix:Loc;
   r=Proj->Scale*Proj->ZFactor;

   for(n=0;n<ABS(Nb);n++) {

      if (Loc[n].C.Lat==-999.0 || Loc[n].C.Lon==-999.0) {
         out[n].V[2]=-999.0;
      } else {
         loc.Lat=Loc[n].C.Lat;
         loc.Lon=Loc[n].C.Lon;
         loc.Elev=Loc[n].C.Elev;
         CLAMPLON(loc.Lon);

         out[n].V[0]=loc.Lon/90.0;
         out[n].V[1]=log(tan(M_PI4 + (loc.Lat>78.0?78.0:(loc.Lat<-78.0?-78.0:loc.Lat))/180.0));
         out[n].V[2]=(loc.Elev==0.0)?1.0:1.0+loc.Elev*r;
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
 *  <Proj>       : Parametres de la projection
 *
 * Retour:
 *  <int>     : Coordonee Valide ou non
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Merca_UnProject(ViewportItem *VP,Projection *Proj,Coord *Loc,Vect3d Pix) {

   Vect3d obj;
   double depth=1.0;

   gluUnProject(Pix[0],VP->Height-Pix[1],depth,VP->GLModS,VP->GLProj,VP->GLView,&obj[0],&obj[1],&obj[2]);

   if (Vect_InterPlane(VP->Cam->Basis,obj,1)) {

      Loc->Lon=Proj->Lon+obj[0]*90.0;
      Loc->Lat=(atan(exp(obj[1]+log(tan(M_PI4 + Proj->Lat/180.0))))-M_PI4)*180.0;

      if (Loc->Lon<=(180.0+Proj->Lon) && Loc->Lon>=(-180.0+Proj->Lon) && Loc->Lat<=78.0 && Loc->Lat>=-78.0) {
         return(1);
      }
   }
   Loc->Lat=-999.0;
   Loc->Lon=-999.0;

   return(0);
}

