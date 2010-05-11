/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Projection cylindrique de la carte vectorielle.
 * Fichier   : ProjRef.c
 * Creation  : Janvier 2006 - J.P. Gauthier - CMC/CMOE
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

int  Grid_Init(Tcl_Interp *Interp);
void Grid_DrawFirst(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
void Grid_DrawLast(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
void Grid_DrawGlobe(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
int  Grid_Locate(Projection *Proj,double Lat,double Lon,int Undo);
void Grid_Render(Projection *Proj,GLuint List,Vect3d *Data,unsigned int *Idx,char *Col,float* Tex,int Mode,int Nb,Vect3d V0,Vect3d V1);
void Grid_Vertex(Vect3d Pix,Vect3d Prev,double Len,int Mode);
void Grid_Setup(Tcl_Interp *Interp,Projection *Proj);

/*Fonctions de transformations*/

unsigned long Grid_Project(const Projection* restrict const Proj,GeoVect *Loc,GeoVect *Pix,long Nb);
int           Grid_UnProject(ViewportItem *VP,Projection *Proj,Coord *Loc,Vect3d Pix);
int           Grid_ProjectPath(Tcl_Interp *Interp,Projection *Proj,Coord Pt1,Coord Pt2,double Dist);
int           Grid_SegLine(ViewportItem *VP,Projection *Proj,Coord Pt1,Coord Pt2,Vect3d Pix00,Vect3d Pix01,Vect3d Pix10,Vect3d Pix11);
Tcl_Obj*      Grid_ProjectPoint(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord Pt1,int Any);
Tcl_Obj*      Grid_ProjectLine(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord *Co,int NCo);

/*----------------------------------------------------------------------------
 * Nom      : <Grid_DrawFirst>
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
void Grid_DrawGlobe(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj){

   char    buf[256];

   if (Interp) {
      glFeedbackInit(20,GL_2D);
      if (VP->ColorFLake && VP->ColorFCoast) {
         Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorFLake);
      } else {
         Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorCoast);
      }
      Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorCoast);
      sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",ABS(Proj->Geo->Params.Coast)-1);
      Tcl_AppendResult(Interp,buf,(char*)NULL);
   }

   /*Pourtour de la carte*/
   if (VP->ColorFLake && VP->ColorFCoast) {
      glColor3us(VP->ColorFLake->red,VP->ColorFLake->green,VP->ColorFLake->blue);
      glDisable(GL_STENCIL_TEST);
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glBegin(GL_POLYGON);
         glVertex3d(-Proj->LI,-Proj->LJ,1.0);
         glVertex3d(-Proj->LI,Proj->LJ,1.0);
         glVertex3d(Proj->LI,Proj->LJ,1.0);
         glVertex3d(Proj->LI,-Proj->LJ,1.0);
         glVertex3d(-Proj->LI,-Proj->LJ,1.0);
      glEnd();
      glEnable(GL_STENCIL_TEST);

      if (Interp)
         glFeedbackProcess(Interp,GL_2D);
   }

   glColor3us(VP->ColorCoast->red,VP->ColorCoast->green,VP->ColorCoast->blue);
   glLineWidth(ABS(Proj->Geo->Params.Coast));
   glBegin(GL_LINE_STRIP);
      glVertex3d(-Proj->LI,-Proj->LJ,1.0);
      glVertex3d(-Proj->LI,Proj->LJ,1.0);
      glVertex3d(Proj->LI,Proj->LJ,1.0);
      glVertex3d(Proj->LI,-Proj->LJ,1.0);
      glVertex3d(-Proj->LI,-Proj->LJ,1.0);
   glEnd();

   if (Interp)
      glFeedbackProcess(Interp,GL_2D);
}

void Grid_DrawFirst(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj){

   char    buf[256];
   Coord   loc;
   Vect3d  pix,prev;

   if (!Interp) {
      Grid_DrawGlobe(Interp,VP,Proj);
   }

   /*Latlons*/
   if (Proj->Geo->Params.CoordLoc) {

      glLineWidth(ABS(Proj->Geo->Params.CoordLoc));
      glColor3us(VP->ColorCoord->red,VP->ColorCoord->green,VP->ColorCoord->blue);

      if (Interp) {
         glFeedbackInit(MAXGEOSEG*4,GL_2D);
         Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorCoord);
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",ABS(Proj->Geo->Params.CoordLoc)-1);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
      }

      loc.Elev=0.0;

      /*Longitudes*/
      Vect_Init(prev,0.0,0.0,0.0);
      for(loc.Lat=(-90+Proj->Geo->Params.CoordDef);loc.Lat<=(90-Proj->Geo->Params.CoordDef);loc.Lat+=Proj->Geo->Params.CoordDef){
         glBegin(GL_LINE_STRIP);
         for(loc.Lon=-180;loc.Lon<=180;loc.Lon+=1.0){
            if (Grid_Project(Proj,(GeoVect*)&loc,(GeoVect*)&pix,-1)) {
               Grid_Vertex(pix,prev,Proj->LI,GL_LINE_STRIP);
            } else {
               glEnd();
               glBegin(GL_LINE_STRIP);
            }
         }
         glEnd();
      }

      /*Latitudes*/
      Vect_Init(prev,0.0,0.0,0.0);
      for(loc.Lon=-180;loc.Lon<=(180-Proj->Geo->Params.CoordDef);loc.Lon+=Proj->Geo->Params.CoordDef){
         glBegin(GL_LINE_STRIP);
         for(loc.Lat=-90;loc.Lat<=90;loc.Lat+=1.0){
            if (Grid_Project(Proj,(GeoVect*)&loc,(GeoVect*)&pix,-1)) {
               Grid_Vertex(pix,prev,Proj->LI,GL_LINE_STRIP);
            } else {
               glEnd();
               glBegin(GL_LINE_STRIP);
            }
         }
         glEnd();
      }

      if (Interp)
         glFeedbackProcess(Interp,GL_2D);
   }
}

void Grid_DrawLast(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj){

   char    buf[16];
   double  incr;
   Vect3d  vr;
   Coord   co;
   double  ax[2];

   /*Draw 3DAxis*/
   if (Proj->TAxis && Proj->ZAxis.Elev!=0.0) {
      glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),VP->tkfont);
      glEnable(GL_DEPTH_TEST);
      glEnable(GL_BLEND);
      glDisable(GL_CULL_FACE);
      glDisable(GL_STENCIL_TEST);
      glPolygonOffset(0.5,1.0);
      glMatrixMode(GL_MODELVIEW);
      glLineWidth(1.0);
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

      incr=pow(10,ORDER(Proj->ZAxis.Elev)-1);
      incr=(Proj->ZAxis.Elev/incr)>10?incr*2.5:incr;
      incr=(Proj->ZAxis.Elev/incr)>10?incr*2:incr;
      incr=(Proj->ZAxis.Elev/incr)>10?incr*2:incr;

      co.Lat=Proj->ZAxis.Lat;
      co.Lon=Proj->ZAxis.Lon;
      co.Elev=Proj->ZAxis.Elev;
      Proj->Type->Project(Proj,(GeoVect*)&co,(GeoVect*)&vr,1);

      switch(Proj->TAxis) {
         case 1: ax[0]=vr[0]; ax[1]=vr[1]; break;
         case 2: ax[0]=-Proj->LI; ax[1]=Proj->LJ; break;
      }

      /*Draw Z axis planes*/
      glEnable(GL_POLYGON_OFFSET_FILL);
      glBegin(GL_QUADS);
         glColor3us(0xAAAA,0xAAAA,0xAAAA);
         glVertex3d(vr[0],-Proj->LJ,1.0);
         glVertex3d(vr[0],-Proj->LJ,vr[2]);
         glVertex3d(vr[0],ax[1],vr[2]);
         glVertex3d(vr[0],ax[1],1.0);

         glColor3us(0xDDDD,0xDDDD,0xDDDD);
         glVertex3d(ax[0],vr[1],1.0);
         glVertex3d(ax[0],vr[1],vr[2]);
         glVertex3d(Proj->LI,vr[1],vr[2]);
         glVertex3d(Proj->LI,vr[1],1.0);
      glEnd();
      glDisable(GL_POLYGON_OFFSET_FILL);

      /*Draw Z axis increments*/
      glColor3us(VP->ColorCoast->red,VP->ColorCoast->green,VP->ColorCoast->blue);
      for(co.Elev=0;co.Elev<=Proj->ZAxis.Elev;co.Elev+=incr) {
         glBegin(GL_LINES);
            Proj->Type->Project(Proj,(GeoVect*)&co,(GeoVect*)&vr,1);
            glVertex3d(vr[0],-Proj->LJ,vr[2]);
            glVertex3d(vr[0],ax[1],vr[2]);
            glVertex3d(ax[0],vr[1],vr[2]);
            glVertex3d(Proj->LI,vr[1],vr[2]);
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
 * Nom      : <Grid_Init>
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
int Grid_Init(Tcl_Interp *Interp){

  Projection_CreateType("grid",PROJPLANE,
      Grid_Locate,
      Grid_Render,
      Grid_DrawFirst,
      Grid_DrawLast,
      Grid_DrawGlobe,
      Grid_UnProject,
      Grid_Project,
      Grid_ProjectPoint,
      Grid_ProjectLine);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Grid_Locate>
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
int Grid_Locate(Projection *Proj,double Lat,double Lon,int Undo) {

   GeoVect loc;

   loc.C.Lat=Lat;
   loc.C.Lon=Lon;
   loc.C.Elev=0.0;

   if (Grid_Project(Proj,&loc,NULL,1)) {
      if (Undo) {
         glTranslated(loc.V[0],loc.V[1],0.0);
      } else {
         glTranslated(-loc.V[0],-loc.V[1],0.0);
      }
      return(1);
   } else {
      return(0);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Grid_Vertex>
 * Creation : Juillet 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Positionner correctement un vertex.
 *
 * Parametres :
 *  <Pix>     : Vecteur position
 *  <Prev>    : Vecteur position precedent
 *  <Len>     : Longueur maximale
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
void Grid_Vertex(Vect3d Pix,Vect3d Prev,double Len,int Mode) {

   if ((Pix[0]<=0.0 && Prev[0]<=0.0) || (Pix[0]>=0.0 && Prev[0]>=0.0)) {
      glVertex3dv(Pix);
   } else {
      if (fabs(Pix[0]-Prev[0])>Len) {
         if (Pix[0]>0.0) {
            glVertex3d(-Len,Pix[1],Pix[2]);
            glEnd();
            glBegin(Mode);
            glVertex3d(Len,Pix[1],Pix[2]);
         } else {
            glVertex3d(Len,Pix[1],Pix[2]);
            glEnd();
            glBegin(Mode);
            glVertex3d(-Len,Pix[1],Pix[2]);
         }
      }
      glVertex3dv(Pix);
   }

   Vect_Assign(Prev,Pix);
}

/*----------------------------------------------------------------------------
 * Nom      : <Grid_Render>
 * Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Affiche un tableau de "vertex".
 *
 * Parametres    :
 *  <Proj>       : Parametres de la projection
 *  <Data>       : Listes des vecteurs
 *  <Mode>       : Mode de creation des primitives
 *  <Nb>         : Dimension du tableau
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Grid_Render(Projection *Proj,GLuint List,Vect3d *Data,unsigned int *Idx,char *Col,float* Tex,int Mode,int Nb,Vect3d V0,Vect3d V1) {

   Vect3d prev;
   int    i,stride;

   if (Nb<0) {
      stride=-Nb-2;
      Nb=-Nb;
   } else {
      stride=0;
   }

   if (Data) {
      if (Mode==GL_LINE_STRIP) {
         Vect_Init(prev,0.0,0.0,0.0);
         stride++;
         glBegin(Mode);
            for(i=0;i<Nb;i+=stride) {
               if (Tex) glTexCoord1f(Tex[i]);
               Grid_Vertex(Data[i],prev,Proj->LI,Mode);
            }
         glEnd();
      } else {
         /*Traiter les tableau en Array , c'est plus efficace*/
         glVertexPointer(3,GL_DOUBLE,stride*sizeof(float)*3,Data);
         glNormalPointer(GL_DOUBLE,stride*sizeof(float)*3,Data);

         /*Activer les couleurs par "vertex"*/
         if (Col) {
            glEnableClientState(GL_COLOR_ARRAY);
            glColorPointer(4,GL_UNSIGNED_BYTE,stride*4,Col);
         }

         if (Tex) {
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            glTexCoordPointer(1,GL_FLOAT,stride,Tex);
         }

         if (Idx) {
            glDrawElements(Mode,Nb,GL_UNSIGNED_INT,Idx);
         } else {
            glDrawArrays(Mode,0,Nb);
         }

         glDisableClientState(GL_COLOR_ARRAY);
         glDisableClientState(GL_TEXTURE_COORD_ARRAY);
      }
   }

   if (List)
      glCallList(List);
}

/*----------------------------------------------------------------------------
 * Nom      : <Grid_ProjectLine>
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
 *  <Co>         : Liste des points de coordonnees en latlon
 *  <NCo>        : Nombre de coordonnees dans la liste
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
Tcl_Obj* Grid_ProjectLine(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord *Co,int NCo){

   Tcl_Obj *obj,*objf,*objb;
   Coord  prev;
   Vect3d pix00,pix01,pix10,pix11;
   Vect3d pixs[2][2000],pixc[2000];
   int    i=0,side=0,cut,n[2],c;

   obj=Tcl_NewListObj(0,NULL);
   objf=Tcl_NewListObj(0,NULL);
   objb=Tcl_NewListObj(0,NULL);

   /*Boucler sur tout les points definissant la region*/
   prev=Co[0];
   n[0]=0;
   n[1]=0;

   for(i=1;i<NCo;i++){

      cut=Grid_SegLine(VP,Proj,prev,Co[i],pix00,pix01,pix10,pix11);
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
         Tcl_AppendResult(Interp,"Grid_ProjectLine: Too many coordinates (Max:2000)",(char*)NULL);
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
 * Nom      : <Grid_Path>
 * Creation : Septembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Projection d'un path.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Proj>    : Parametres de la projection
 *  <List>    : Liste des coordonnees
 *  <Dist>    : Distance en point de grille entre les samples
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
Tcl_Obj *Grid_Path(Tcl_Interp *Interp,Projection *Proj,Tcl_Obj *List,double Dist){

   Tcl_Obj *obj,*objo;
   Vect3d   p0,p1,p;
   Coord    loc0,loc1;
   double   id,ed;
   int      i=0,nobj;

   objo=Tcl_NewListObj(0,NULL);

   if (!Proj->Ref)
      return(objo);

   Tcl_ListObjLength(Interp,List,&nobj);

   if (nobj<4 || nobj%2!=0) {
      return TCL_OK;
   }

   /* Get the first path coordinate*/
   Tcl_ListObjIndex(Interp,List,i++,&obj);
   Tcl_GetDoubleFromObj(Interp,obj,&loc0.Lat);
   Tcl_ListObjIndex(Interp,List,i++,&obj);
   Tcl_GetDoubleFromObj(Interp,obj,&loc0.Lon);

   while (i<nobj) {

      /* Output start of path segment*/
      Tcl_ListObjAppendElement(Interp,objo,Tcl_NewDoubleObj(loc0.Lat));
      Tcl_ListObjAppendElement(Interp,objo,Tcl_NewDoubleObj(loc0.Lon));

      /* Parse all the path coordinates*/
      Tcl_ListObjIndex(Interp,List,i++,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&loc1.Lat);
      Tcl_ListObjIndex(Interp,List,i++,&obj);
      Tcl_GetDoubleFromObj(Interp,obj,&loc1.Lon);

      Proj->Ref->UnProject(Proj->Ref,&p0[0],&p0[1],loc0.Lat,loc0.Lon,1,1);
      Proj->Ref->UnProject(Proj->Ref,&p1[0],&p1[1],loc1.Lat,loc1.Lon,1,1);

      ed=id=1.0/(hypot(p1[1]-p0[1],p1[0]-p0[0])/Dist);

      /* Iterate on the grid course at the specified step*/
      while(ed<1.0) {
         p[0]=ILIN(p0[0],p1[0],ed);
         p[1]=ILIN(p0[1],p1[1],ed);
         Proj->Ref->Project(Proj->Ref,p[0],p[1],&loc0.Lat,&loc0.Lon,0,1);
         Tcl_ListObjAppendElement(Interp,objo,Tcl_NewDoubleObj(loc0.Lat));
         Tcl_ListObjAppendElement(Interp,objo,Tcl_NewDoubleObj(loc0.Lon));
         ed+=id;
      }
      loc0=loc1;
   }

   /* Output las path coordinate */
   Tcl_ListObjAppendElement(Interp,objo,Tcl_NewDoubleObj(loc1.Lat));
   Tcl_ListObjAppendElement(Interp,objo,Tcl_NewDoubleObj(loc1.Lon));

   return(objo);
}

/*----------------------------------------------------------------------------
 * Nom      : <Grid_ProjectPoint>
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
Tcl_Obj* Grid_ProjectPoint(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord Pt1,int Any){

   Tcl_Obj *obj;
   Vect3d   pix,pos;
   int      in;

   in=Proj->Type->Project(Proj,(GeoVect*)&Pt1,(GeoVect*)&pos,1);
   obj=Tcl_NewListObj(0,NULL);

   /*Si en dehors du domain*/
   if (in) {
      gluProject(pos[0],pos[1],pos[2],VP->GLModR,VP->GLProj,VP->GLView,&pix[0],&pix[1],&pix[2]);

      /*Repositionner dans le referentiel de Tcl*/
      pix[1]=Proj->VP->Height-pix[1];

      if (Any || INSIDE(pix,0,0,Proj->VP->Width,Proj->VP->Height)) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pix[0]+VP->x));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pix[1]+VP->y));
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pix[2]));
      }
   }

   return(obj);
}

/*----------------------------------------------------------------------------
 * Nom      : <Grid_SegLine>
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
int Grid_SegLine(ViewportItem *VP,Projection *Proj,Coord Pt1,Coord Pt2,Vect3d Pix00,Vect3d Pix01,Vect3d Pix10,Vect3d Pix11) {

   Vect3d in[2];
   Coord  co[2];

   co[0].Lat=Pt1.Lat;co[0].Lon=Pt1.Lon;
   co[1].Lat=Pt2.Lat;co[1].Lon=Pt2.Lon;

   CLAMPLAT(co[0].Lat);
   CLAMPLAT(co[1].Lat);

   /*Localisation des extremites de la ligne*/
   Proj->Type->Project(Proj,(GeoVect*)co,(GeoVect*)in,2);

   gluProject(in[0][0],in[0][1],in[0][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix00[0],&Pix00[1],&Pix00[2]);
   Pix00[1]=Proj->VP->Height-Pix00[1];
   gluProject(in[1][0],in[1][1],in[1][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix01[0],&Pix01[1],&Pix01[2]);
   Pix01[1]=Proj->VP->Height-Pix01[1];

   return 0;
}

/*----------------------------------------------------------------------------
 * Nom      : <Grid_Setup>
 * Creation : Aout 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser la projection de grille avec le champs.
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Proj>    : Parametres de la projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Grid_Setup(Tcl_Interp *Interp,Projection *Proj){

   TGeoRef *ref;
   double   ni,nj;

   if (!(ref=Proj->Ref))
      return;

   if (Proj->Geographic) {
      ref->UnProject(ref,&ni,&nj,Proj->Lat,Proj->Lon,0,1);
      Proj->I=ni;
      Proj->J=nj;
   }

   /* Recuperer les parametres de deformations */
   if (ref->Grid[0]=='Z') {
      GeoRef_Expand(ref);
      Proj->LI=ref->AX[ref->X1]-ref->AX[ref->X0];
      Proj->LJ=ref->AY[ref->Y1]-ref->AY[ref->Y0];
   } else {
      Proj->LI=ref->X1-ref->X0;
      Proj->LJ=ref->Y1-ref->Y0;
   }

   Proj->L=Proj->LI>Proj->LJ?Proj->LI:Proj->LJ;
   Proj->LI/=Proj->L;
   Proj->LJ/=Proj->L;

   ViewportClean(Proj->VP,1,1);
   Projection_Clean(Interp,Proj,GDB_FORCE);
}

/*----------------------------------------------------------------------------
 * Nom      : <Grid_Project>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Transforme les coordonnes lat-lon en projection usager.
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
unsigned long Grid_Project(const Projection* restrict const Proj,GeoVect *Loc,GeoVect *Pix,long Nb) {

   TGeoRef *ref;
   GeoVect *out;
   Coord    loc;
   double   d,r;
   int      s;
   long     n,e=0;

   out=(Pix?Pix:Loc);
   r=Proj->Scale*Proj->ZFactor;

   if (!(ref=Proj->Ref))
      return(0);

   for(n=0;n<ABS(Nb);n++) {

      loc.Lat=Loc[n].C.Lat;
      loc.Lon=Loc[n].C.Lon;
      loc.Elev=Loc[n].C.Elev;

      if (Proj->Geographic) {
         if (loc.Lat==-999.0 || loc.Lon==-999.0) {
            out[n].V[2]=-999.0;
            continue;
         } else {
            ref->UnProject(ref,&out[n].V[0],&out[n].V[1],loc.Lat,loc.Lon,1,1);
         }
      } else {
         out[n].V[0]=loc.Lon;
         out[n].V[1]=loc.Lat;
      }

      if (ref->AX && ref->AY) {
         s=floor(out[n].V[0]);
         if (out[n].V[0]<0) {
//            out[e].V[0]=0;
            out[n].V[0]=ref->AX[0]+out[n].V[0]*(ref->AX[1]-ref->AX[0]);
         } else if (out[n].V[0]>ref->X1) {
            out[n].V[0]=ref->AX[ref->X1]+(out[n].V[0]-ref->X1)*(ref->AX[ref->X1]-ref->AX[ref->X1-1]);
         } else {
            out[n].V[0]=ILIN(ref->AX[s],ref->AX[s+1],(out[n].V[0]-s));
         }
         out[n].V[0]-=ref->AX[ref->X0];

         s=floor(out[n].V[1]);
         if (out[n].V[1]<0) {
//            out[e].V[1]=0;
            out[n].V[1]=ref->AY[0]+out[n].V[1]*(ref->AY[1]-ref->AY[0]);
         } else if (out[n].V[1]>ref->Y1) {
            out[n].V[1]=ref->AY[ref->Y1]+(out[n].V[1]-ref->Y1)*(ref->AY[ref->Y1]-ref->AY[ref->Y1-1]);
         } else {
            out[n].V[1]=ILIN(ref->AY[s],ref->AY[s+1],(out[e].V[1]-s));
         }
         out[n].V[1]-=ref->AY[ref->Y0];

         d=Proj->L*0.5;
      } else {
         d=(Proj->L-1)*0.5;
      }

      out[n].V[0]=(out[n].V[0]-ref->X0)/d-Proj->LI;
      out[n].V[1]=(out[n].V[1]-ref->Y0)/d-Proj->LJ;
      out[n].V[2]=(loc.Elev==0.0)?1.0:1.0+loc.Elev*r;

      /*Si en dehors du domain*/
      if (Nb>0 && (out[n].V[0]<-Proj->LI || out[n].V[0]>Proj->LI || out[n].V[1]<-Proj->LJ || out[n].V[1]>Proj->LJ)) {
//         out[e].V[2]=-999.0;
      } else {
         e++;
      }
   }
   return(e);
}

/*----------------------------------------------------------------------------
 * Nom      : <Grid_UnProject>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Transforme les coordonnes usager en lat-lon.
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
int Grid_UnProject(ViewportItem *VP,Projection *Proj,Coord *Loc,Vect3d Pix) {

   TGeoRef *ref;
   Vect3d   obj;
   double   x,y,d,depth=1.0;
   int      s;

   Loc->Lat=-999.0;
   Loc->Lon=-999.0;

   if (!(ref=Proj->Ref))
      return(0);

   gluUnProject(Pix[0],VP->Height-Pix[1],depth,VP->GLModR,VP->GLProj,VP->GLView,&obj[0],&obj[1],&obj[2]);

   if (Vect_InterPlane(VP->Cam->Basis,obj,1)) {

      if (ref->AX) {
         d=Proj->L*0.5;
      } else {
         d=(Proj->L-1)*0.5;
      }

      x=(obj[0]+Proj->LI)*d+ref->X0;
      y=(obj[1]+Proj->LJ)*d+ref->Y0;

      if (ref->AX) {
         s=0;
         x=x+ref->AX[ref->X0];
         while(s<=ref->X1 && x>ref->AX[s]) s++;
         if (s>0 && s<=ref->X1) {
            x=(x-ref->AX[s-1])/(ref->AX[s]-ref->AX[s-1])+s;
         } else {
            x=0;
         }

         s=0;
         y=y+ref->AY[ref->Y0];
         while(s<=ref->Y1 && y>ref->AY[s]) s++;
         if (s>0 && s<=ref->Y1) {
            y=(y-ref->AY[s-1])/(ref->AY[s]-ref->AY[s-1])+s;
         } else {
            y=0;
         }
         x-=1.0;
         y-=1.0;
      }

      if (Proj->Geographic) {
         return(ref->Project(ref,x,y,&Loc->Lat,&Loc->Lon,0,1));
      } else {
         Loc->Lat=y;
         Loc->Lon=x;
      }
   }

   return(0);
}
