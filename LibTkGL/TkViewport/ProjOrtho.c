/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Projection orthographique de la carte vectorielle.
 * Fichier   : ProjOrtho.c
 * Creation  : Avril 98 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Definit tout ce qui se rapporte a la vue en cours ainsi que
 *              toute les procedures de creations, manipulation et configurations.
 *
 * Remarques :
 *   -Base en partie sur la projection cylindrique de Vanh Souvanlasy (Juin 1994)
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

#include "Projection.h"
#include "GeoData.h"
#include <math.h>

/*Prototypes*/
int    Ortho_Init(Tcl_Interp *Interp);
void   Ortho_DrawFirst(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
void   Ortho_DrawLast(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
void   Ortho_DrawGlobe(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
int    Ortho_Locate(Projection *Proj,double Lat,double Lon,int Undo);
void   Ortho_Render(Projection *Proj,GLuint List,Vect3d *Data,unsigned int *Idx,char *Col,float* Tex,int Mode,int Nb,int Stride,Vect3d V0,Vect3d V1);
double CircleIntersect(Coord Pt0,Coord Pt1,int R,Vect3d Mid,Projection *Proj,ViewportItem *VP);

/*Fonctions de transformations*/
unsigned long Ortho_Project(const Projection* restrict const Proj,GeoVect *Loc,GeoVect *Pix,long Nb);
int           Ortho_UnProject(ViewportItem *VP,Projection *Proj,Coord *Loc,Vect3d Pix);
Tcl_Obj*      Ortho_ProjectPoint(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord Pt1,int Any);
Tcl_Obj*      Ortho_ProjectLine(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord *Co,int NCo);

/*----------------------------------------------------------------------------
 * Nom      : <Ortho_CircleIntersect>
 * Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue les conversions latlon->pixel et inverses.
 *
 * Parametres    :
 *  <Pt0>        : Premier point
 *  <Pt1>        : Deuxieme point
 *  <R>          : Rayon de la sphere
 *  <Mid>        : Vecteur du centre de la sphere
 *  <Proj>       : Parametres de la projection
 *  <VP>         : Parameters du viewport
 *
 * Retour        :
 *  <liste>      : Retourne la liste { { x y x y ... } { x y xy ... } }
 *                 ou la premiere liste est le polygone avant et le deuxieme,
 *                 le polygone arriere.
 *  <TCL_...>    : Code de reussite TCL
 *
 * Remarques :
 *    -La liste de coordonnees definissant la region doit etre fermee, les premieres et dernieres
 *     coordonnees pareilles
 *
 *----------------------------------------------------------------------------
*/
double CircleIntersect(Coord Pt0,Coord Pt1,int R,Vect3d Mid,Projection *Proj,ViewportItem *VP){

   Vect3d  inter;
   GeoVect tmp;
   int     rt=-1;
   double  theta,dn,tc,d2;
   double  sinlat0,coslat0,sinlat1,coslat1,sindn,cosdn;

   /*From front to back*/
   Proj->Type->Project(Proj,(GeoVect*)&Pt1,&tmp,1);
   gluProject(tmp.V[0],tmp.V[1],tmp.V[2],VP->GLModR,VP->GLProj,VP->GLView,&inter[0],&inter[1],&inter[2]);
   if (inter[2]<=1.0) {
      tmp.C=Pt0;
      Pt0=Pt1;
      Pt1=tmp.C;
   }

   Pt0.Lat=DEG2RAD(Pt0.Lat);
   Pt0.Lon=DEG2RAD(Pt0.Lon);
   Pt1.Lat=DEG2RAD(Pt1.Lat);
   Pt1.Lon=DEG2RAD(Pt1.Lon);

   sinlat0=sin(Pt0.Lat);
   sinlat1=sin(Pt1.Lat);
   coslat0=cos(Pt0.Lat);
   coslat1=cos(Pt1.Lat);

   /*True course along which to iterate*/
   tc=fmod(atan2(sin(Pt0.Lon-Pt1.Lon)*coslat1,coslat0*sinlat1-sinlat0*coslat1*cos(Pt0.Lon-Pt1.Lon)),M_2PI);

   /*Distance between points*/
   dn=acos(sinlat0*sinlat1+cos(Pt0.Lon-Pt1.Lon)*coslat0*coslat1);
   d2=dn*0.5f;
   dn=d2;

   /*As long as we are not close enough to the circumference*/
   while(rt!=R && d2>M_PI/3600){

      /*Position along true course*/
      sindn=sin(dn);
      cosdn=cos(dn);
      tmp.C.Lat=asin(sinlat0*cosdn+coslat0*sindn*cos(tc));
      tmp.C.Lon=fmod(Pt0.Lon-atan2(sin(tc)*sindn*coslat0,cosdn-sinlat0*sin(tmp.C.Lat))+M_PI,M_2PI)-M_PI;
      tmp.C.Elev=Pt1.Elev;

      /*Project the position*/
      tmp.C.Lat=RAD2DEG(tmp.C.Lat);
      tmp.C.Lon=RAD2DEG(tmp.C.Lon);

      Proj->Type->Project(Proj,&tmp,NULL,1);
      gluProject(tmp.V[0],tmp.V[1],tmp.V[2],Proj->VP->GLModR,Proj->VP->GLProj,Proj->VP->GLView,&inter[0],&inter[1],&inter[2]);

      d2*=0.5f;
      if (inter[2]>1.0){
         dn-=d2;
      } else {
         dn+=d2;
      }

      /*Get the radius*/
      inter[1]=Proj->VP->Height-inter[1];
      Vect_Substract(inter,inter,Mid);
      rt=hypot(inter[0],inter[1]);
   }

   /*Calculate angle from center*/
   theta=atan(inter[1]/inter[0]);
   if (inter[0]<0)
      return (theta+M_PI);
   else
      return (theta);
}

/*----------------------------------------------------------------------------
 * Nom      : <Ortho_DrawFirst>
 * Creation : Avril 98 - J.P. Gauthier - CMC/CMOE
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
 *    -On creer les latlons en utilisant une sphere segmentee selon les degrees des latlons.
 *
 *----------------------------------------------------------------------------
*/
void Ortho_DrawGlobe(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj){

   char    buf[256];
   double  delt;

   if (Proj->Geo->Params.Coast) {

      if (Interp) {
         glFeedbackInit(3000,GL_2D);
         if (VP->ColorFLake && VP->ColorFCoast) {
            Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorFLake);
         } else {
            Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorCoast);
         }
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",ABS(Proj->Geo->Params.Coast)-1);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
      }

      /*Recuperer le rayon du globe*/
      delt=((VP->Width>VP->Height)?VP->Height>>1:VP->Width>>1)/VP->Cam->Aspect;

      /*Afficher les contours du globe*/
      glMatrixMode(GL_PROJECTION);
      glPushMatrix();
      glLoadIdentity();
      gluOrtho2D(0,VP->Width,0,VP->Height);

      glMatrixMode(GL_MODELVIEW);
      glPushMatrix();
      glLoadIdentity();
      glTranslated(Proj->ZPos[0],Proj->ZPos[1],0.0);
      glScaled(delt,delt,1.0);

      if (VP->ColorFLake && VP->ColorFCoast) {
         glColor3us(VP->ColorFLake->red,VP->ColorFLake->green,VP->ColorFLake->blue);
         glDisable(GL_STENCIL_TEST);
         glPolygonMode(GL_FRONT,GL_FILL);
         glDrawCircle(64,GL_POLYGON);
         glEnable(GL_STENCIL_TEST);
      } else {
         glColor3us(VP->ColorCoast->red,VP->ColorCoast->green,VP->ColorCoast->blue);
         glLineWidth(ABS(Proj->Geo->Params.Coast));
         glDrawCircle(64,GL_LINE_STRIP);
      }
      glPopMatrix();
      glMatrixMode(GL_PROJECTION);
      glPopMatrix();

      if (Interp)
         glFeedbackProcess(Interp,GL_2D);
   }
}

void Ortho_DrawFirst(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj){

   char    buf[256];
   double  lat,lon,delt;

   if (!Interp) {
      Ortho_DrawGlobe(Interp,VP,Proj);
   }

   /*Affichage des latlons*/
   if (Proj->Geo->Params.CoordLoc) {

     if (Interp) {
         Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorCoord);
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",ABS(Proj->Geo->Params.CoordLoc)-1);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
      }

      glLineWidth(ABS(Proj->Geo->Params.CoordLoc));
      glColor3us(VP->ColorCoord->red,VP->ColorCoord->green,VP->ColorCoord->blue);

      glMatrixMode(GL_MODELVIEW);
      glPushMatrix();

      /*Trace des latitudes*/
      for(lon=0;lon<180;lon+=Proj->Geo->Params.CoordDef){
         if (Interp) glFeedbackInit(2000,GL_2D);
         glRotated(Proj->Geo->Params.CoordDef,0.0,1.0,0.0);
         glDrawCircle(64,GL_LINE_STRIP);
         if (Interp) glFeedbackProcess(Interp,GL_2D);
      }

      glPopMatrix();
      glPushMatrix();
      glRotated(90.0,1.0,0.0,0.0);

      /*Trace des longitudes*/
      for(lat=-90+Proj->Geo->Params.CoordDef;lat<90;lat+=Proj->Geo->Params.CoordDef){
         if (Interp) glFeedbackInit(2000,GL_2D);
         glPushMatrix();
         delt=DEG2RAD(lat);

         glTranslated(0.0,0.0,sin(delt));
         glScaled(cos(delt),cos(delt),1.0);
         glDrawCircle(64,GL_LINE_STRIP);
         glPopMatrix();
         if (Interp) glFeedbackProcess(Interp,GL_2D);
      }

      glPopMatrix();
   }
}

void Ortho_DrawLast(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj){

   double  incr;
   char    buf[15];
   GeoVect vr,co;
   double  ax[2];

   /*Draw 3DAxis*/
   if (Proj->TAxis && Proj->ZAxis.Elev!=0.0) {
      glFontUse(Tk_Display(Tk_CanvasTkwin(VP->canvas)),VP->tkfont);
      glEnable(GL_DEPTH_TEST);
      glDisable(GL_CULL_FACE);
      glMatrixMode(GL_MODELVIEW);
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glLineWidth(1.0);
      glPolygonOffset(0.5,1.0);
      glEnable(GL_BLEND);

      incr=pow(10,ORDER(Proj->ZAxis.Elev)-1);
      incr=(Proj->ZAxis.Elev/incr)>10?incr*2.5:incr;
      incr=(Proj->ZAxis.Elev/incr)>10?incr*2:incr;
      incr=(Proj->ZAxis.Elev/incr)>10?incr*2:incr;

      switch(Proj->TAxis) {
         case 1: ax[0]=Proj->ZAxis.Lat; ax[1]=0.0; break;
         case 2: ax[0]=90.0; ax[1]=-90.0; break;
      }

      /*Y Axis Filling*/
      glEnable(GL_POLYGON_OFFSET_FILL);
      glColor3us(0xAAAA,0xAAAA,0xAAAA);
      glBegin(GL_QUAD_STRIP);
      co.C.Lon=Proj->ZAxis.Lon;
      for(co.C.Lat=ax[0];co.C.Lat>=-90.0;co.C.Lat-=1.0) {
         co.C.Elev=0.0;
         Proj->Type->Project(Proj,&co,&vr,1);
         glVertex3dv(vr.V);

         co.C.Elev=Proj->ZAxis.Elev;
         Proj->Type->Project(Proj,&co,&vr,1);
         glVertex3dv(vr.V);
      }
      glEnd();
      glDisable(GL_POLYGON_OFFSET_FILL);

      /*Draw Y axis increments*/
      glColor3us(VP->ColorCoast->red,VP->ColorCoast->green,VP->ColorCoast->blue);
      co.C.Lon=Proj->ZAxis.Lon;
      for(co.C.Elev=0;co.C.Elev<=Proj->ZAxis.Elev;co.C.Elev+=incr) {
         glBegin(GL_LINE_STRIP);
         for(co.C.Lat=ax[0];co.C.Lat>=-90.0;co.C.Lat-=1.0){
            Proj->Type->Project(Proj,&co,&vr,1);
            glVertex3dv(vr.V);
         }
         glEnd();
      }

      /*X Axis*/
      glPushMatrix();
      glRotatef(Proj->ZAxis.Lon,0.0,1.0,0.0);
      glRotatef(-Proj->ZAxis.Lat,1.0,0.0,0.0);
      glColor3us(VP->ColorCoast->red,VP->ColorCoast->green,VP->ColorCoast->blue);

      /*X Axis Filling*/
      glEnable(GL_POLYGON_OFFSET_FILL);
      glColor3us(0xDDDD,0xDDDD,0xDDDD);
      glBegin(GL_QUAD_STRIP);
      co.C.Lat=0.0;
      for(co.C.Lon=ax[1];co.C.Lon<=ax[1]+180;co.C.Lon+=1.0){
         co.C.Elev=0.0;
         Proj->Type->Project(Proj,&co,&vr,1);
         glVertex3dv(vr.V);

         co.C.Elev=Proj->ZAxis.Elev;
         Proj->Type->Project(Proj,&co,&vr,1);
         glVertex3dv(vr.V);
      }
      glEnd();
      glDisable(GL_POLYGON_OFFSET_FILL);

      /*X Axis Increment*/
      glColor3us(VP->ColorCoast->red,VP->ColorCoast->green,VP->ColorCoast->blue);
      co.C.Lat=0.0;
      for(co.C.Elev=0;co.C.Elev<=Proj->ZAxis.Elev;co.C.Elev+=incr) {
         glBegin(GL_LINE_STRIP);
         for(co.C.Lon=ax[1];co.C.Lon<=ax[1]+180;co.C.Lon+=1.0){
            Proj->Type->Project(Proj,&co,&vr,1);
            glVertex3dv(vr.V);
         }
         glEnd();

         glPushMatrix();
         co.C.Lon=0.0;
         Proj->Type->Locate(Proj,co.C.Lat,co.C.Lon,1);
         glTranslated(0.0,0.0,ZM(Proj,co.C.Elev));
         glRotatef(90.0,1.0,0.0,0.0);
         glScalef(VP->Ratio,VP->Ratio,1.0);
         sprintf(buf,"  %i",(int)co.C.Elev);
         glPrint(Interp,VP->canvas,buf,vr.V[0],vr.V[1],0);
         glPopMatrix();
      }

      glPopMatrix();

      glDisable(GL_BLEND);
      glEnable(GL_CULL_FACE);
      glEnable(GL_STENCIL_TEST);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Ortho_Init>
 * Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Creer le type de projection.
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
int Ortho_Init(Tcl_Interp *Interp){

   Projection_CreateType("orthographic",PROJGLOBE,
      Ortho_Locate,
      Ortho_Render,
      Ortho_DrawFirst,
      Ortho_DrawLast,
      Ortho_DrawGlobe,
      Ortho_UnProject,
      Ortho_Project,
      Ortho_ProjectPoint,
      Ortho_ProjectLine);

   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <Ortho_Locate>
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
int Ortho_Locate(Projection *Proj,double Lat,double Lon,int Undo) {

   if (Undo) {
      glRotated(Lon,0.0,1.0,0.0);
      glRotated(-Lat,1.0,0.0,0.0);
   } else {
      glRotated(Lat,1.0,0.0,0.0);
      glRotated(-Lon,0.0,1.0,0.0);
   }
   return 1;
}

/*----------------------------------------------------------------------------
 * Nom      : <Ortho_Render>
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
void Ortho_Render(Projection *Proj,GLuint List,Vect3d *Data,unsigned int *Idx,char *Col,float* Tex,int Mode,int Nb,int Stride,Vect3d V0,Vect3d V1) {

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

      if (Idx) {
         glDrawElements(Mode,Nb,GL_UNSIGNED_INT,Idx);
      } else {
         glDrawArrays(Mode,0,Nb);
      }
      glDisableClientState(GL_COLOR_ARRAY);
      glDisableClientState(GL_TEXTURE_COORD_ARRAY);
   }

   if (List)
      glCallList(List);
}

/*----------------------------------------------------------------------------
 * Nom      : <Ortho_ProjectLine>
 * Creation : Aout 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue les conversions latlon->pixel et inverses pour une ligne (linestrip).
 *            clipper a l'interieur du viewport
 *
 * Parametres    :
 *  <Tcl_Interp> : Interpreteur Tcl
 *  <VP>         : Parametres du viewport
 *  <Proj>       : Parametres de la projection
 *  <Co>         : Liste des points de coordonnees en latlon
 *  <NCo>        : Nombre de coordonnees dans la liste
 *
 * Retour        :
 *  <liste>      : Retourne la liste { { x y x y ... } { x y xy ... } }
 *                 ou la premiere liste est le polygone avant et le deuxieme,
 *                 le polygone arriere.
 *  <TCL_...>    : Code de reussite TCL
 *
 * Remarques :
 *    -La liste de coordonnees definissant la region doit etre fermee, les premieres et dernieres
 *     coordonnees pareilles
 *
 *----------------------------------------------------------------------------
*/
Tcl_Obj* Ortho_ProjectLine(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord *Co,int NCo){

   Tcl_Obj *obj,*objf,*objb;
   Vect3d in,mid,pix,pixf[2000],pixb[2000],pixc[2000],arc[11];
   Coord  prev;
   int    i=0,j,inter=1,f=0,b=0,c,r;
   double theta1,theta2,incrt,side=0.0;

   obj=Tcl_NewListObj(0,NULL);
   objf=Tcl_NewListObj(0,NULL);
   objb=Tcl_NewListObj(0,NULL);

   /*Determiner le point central*/
   Vect_Init(in,0.0,0.0,0.0);
   gluProject(in[0],in[1],in[2],VP->GLModR,VP->GLProj,VP->GLView,&mid[0],&mid[1],&mid[2]);
   mid[1]=VP->Height-mid[1];

   /*Determiner le rayon de circonference*/
   r=((VP->Width>VP->Height)?VP->Height>>1:VP->Width>>1)/VP->Cam->Aspect;

   /*Boucler sur tout les points definissant la region*/
   for(i=0;i<NCo;i++) {

      Proj->Type->Project(Proj,(GeoVect*)&Co[i],(GeoVect*)&in,1);
      gluProject(in[0],in[1],in[2],VP->GLModR,VP->GLProj,VP->GLView,&pix[0],&pix[1],&pix[2]);
      pix[1]=VP->Height-pix[1];
      pix[2]=1-pix[2];

      /*Si on change de cote (= -??.??)*/
      if (side*pix[2] < 0.0) {

         /*Trouver le point d'intersection*/
         theta2=CircleIntersect(Co[i],prev,r,mid,Proj,VP);
         theta2=theta2>M_2PI?theta2+M_2PI:theta2;

         /*Si on a traverse le cote du globe creer le segment du pourtour*/
         if (!inter){

            /*Dans les cas ou on passe par dessus l'angle 2Pi ou 0*/
            if (theta2-theta1 > M_PI) theta2-=M_2PI;
            if (theta1-theta2 > M_PI) theta1-=M_2PI;

            /*Definir les increments*/
            incrt=(theta2-theta1)*0.1;

            /*Boucler sur le nombre de segments dans la ligne*/
            for (j=0;j<=10;j++){
               arc[j][0]=mid[0]+r*cos(theta1);
               arc[j][1]=mid[1]+r*sin(theta1);
               theta1+=incrt;
            }

            /*Mettre les coordonnees dans leur liste respective*/
            if (pix[2] < 0.0){
               for (j=10;j>=0;j--) { Vect_Assign(pixf[f],arc[j]); f++; }
               for (j=0;j<=10;j++) { Vect_Assign(pixb[b],arc[j]); b++; }
            } else {
               for (j=10;j>=0;j--) { Vect_Assign(pixb[b],arc[j]); b++; }
               for (j=0;j<=10;j++) { Vect_Assign(pixf[f],arc[j]); f++; }
            }
         } else {
            inter=0;
            theta1=theta2;
         }
      }

      if (pix[2] < 0.0) {
         Vect_Assign(pixb[b],pix);b++;
      } else {
         Vect_Assign(pixf[f],pix);f++;
      }
      side=pix[2];
      prev=Co[i];

      if (b>2000 || f>2000) {
         Tcl_AppendResult(Interp,"Orhto_ProjectLine: Too many coordinates (Max:2000)",(char*)NULL);
         b=f=0;
         break;
      }
   }

   /*Creer la liste des points sur la face avant*/
   if (f) {
      LiangBarsky_PolygonClip2D(pixf,f,pixc,&c,0,0,VP->Width,VP->Height);
      for (i=0;i<c;i++){
         Tcl_ListObjAppendElement(Interp,objf,Tcl_NewDoubleObj(pixc[i][0]+VP->x));
         Tcl_ListObjAppendElement(Interp,objf,Tcl_NewDoubleObj(pixc[i][1]+VP->y));
      }
   }

   /*Creer la liste des points sur la face arriere*/
   if (b) {
      LiangBarsky_PolygonClip2D(pixb,b,pixc,&c,0,0,VP->Width,VP->Height);
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
 * Nom      : <Ortho_ProjectPoint>
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
Tcl_Obj* Ortho_ProjectPoint(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord Pt1,int Any){

   Tcl_Obj *obj;
   GeoVect  in;
   Vect3d   pix;

   obj=Tcl_NewListObj(0,NULL);

   Proj->Type->Project(Proj,(GeoVect*)&Pt1,&in,1);
   gluProject(in.V[0],in.V[1],in.V[2],VP->GLModR,VP->GLProj,VP->GLView,&pix[0],&pix[1],&pix[2]);

   /*Repositionner dans le referentiel de Tcl*/
   pix[1]=VP->Height-pix[1];
   pix[2]=1.0-pix[2];

   if (Any || INSIDE(pix,0,0,VP->Width,VP->Height)) {
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pix[0]+VP->x));
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pix[1]+VP->y));
      Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(pix[2]));
   }
   return(obj);
}

/*----------------------------------------------------------------------------
 * Nom      : <Ortho_Project>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Transforme les coordonnes lat-lon en projection orthographique.
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
unsigned long Ortho_Project(const Projection* restrict const Proj,GeoVect *Loc,GeoVect *Pix,long Nb) {

   long     n,e=0;
   double   r,rd,coslat;
   Coord    loc;
   GeoVect *out;

   out=(Pix?Pix:Loc);
   r=Proj->Scale*Proj->ZFactor;

   for(n=0;n<ABS(Nb);n++) {

      if (Loc[n].C.Lat==-999.0 || Loc[n].C.Lon==-999.0) {
         out[n].V[2]=-999.0;
      } else {

         loc.Lat=DEG2RAD(Loc[n].C.Lat);
         loc.Lon=DEG2RAD(Loc[n].C.Lon);
         loc.Elev=Loc[n].C.Elev;

         /*Calcul des membres repetes*/
         rd=(loc.Elev==0.0)?1.0:(1.0+loc.Elev*r);
         coslat=rd*cos(loc.Lat);

         out[n].V[0]=coslat*sin(loc.Lon);
         out[n].V[1]=(loc.Elev==0.0)?sin(loc.Lat):rd*sin(loc.Lat);
         out[n].V[2]=coslat*cos(loc.Lon);
         e++;
      }
   }
   return(e);
}

/*----------------------------------------------------------------------------
 * Nom      : <Ortho_UnProject>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Transforme les coordonnes de la projection orthographique en lat-lon.
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
 *    - Retourne la valeur 999 si hors du globe
 *
 *----------------------------------------------------------------------------
*/
int Ortho_UnProject(ViewportItem *VP,Projection *Proj,Coord *Loc,Vect3d Pix) {

   Vect3d obj;
   double r,c;
   double depth=0.1;

   gluUnProject(Pix[0],VP->Height-Pix[1],depth,VP->GLModS,VP->GLProj,VP->GLView,&obj[0],&obj[1],&obj[2]);

   /*r=(Loc->Elev*Proj->Scale+EARTHRADIUS)/EARTHRADIUS;r*=r;*/
   r=1.0;

   if (Vect_InterSphere(VP->Cam->Basis,VP->Cam->A,obj,r)) {

      r=hypot(obj[0],obj[1]);

      if (r<=1.0) {

         c=cos(asin(r));

         Loc->Lat=RAD2DEG(asin(c*Proj->SLat + obj[1]*Proj->CLat));
         Loc->Lon=Proj->Lon+RAD2DEG(atan2(obj[0],(Proj->CLat*c - obj[1]*Proj->SLat)));

         return(1);
      }
   }

   Loc->Lat=-999.0;
   Loc->Lon=-999.0;

   return 0;
}
