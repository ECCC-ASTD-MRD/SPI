/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Projection cylindrique de la carte vectorielle.
 * Fichier   : ProjAzimuth.c
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

int  Azimuth_Init(Tcl_Interp *Interp);
void Azimuth_DrawGlobe(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
int  AzimuthDist_Locate(Projection *Proj,double Lat,double Lon,int Undo);
int  AzimuthArea_Locate(Projection *Proj,double Lat,double Lon,int Undo);
void Azimuth_Render(Projection *Proj,GLuint List,Vect3d *Data,unsigned int *Idx,char *Col,float* Tex,int Mode,int Nb,Vect3d V0,Vect3d V1);

/*Fonctions de transformations*/

unsigned long AzimuthDist_Project(ProjParams *Params,GeoVect *Loc,GeoVect *Pix,long Nb);
int           AzimuthDist_UnProject(ViewportItem *VP,ProjParams *Params,Coord *Loc,Vect3d Pix);
unsigned long AzimuthArea_Project(ProjParams *Params,GeoVect *Loc,GeoVect *Pix,long Nb);
int           AzimuthArea_UnProject(ViewportItem *VP,ProjParams *Params,Coord *Loc,Vect3d Pix);
Tcl_Obj*      Azimuth_ProjectPoint(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord Pt1,int Any);
Tcl_Obj*      Azimuth_ProjectLine(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord *Co,int NCo);
int           Azimuth_SegLine(ViewportItem *VP,Projection *Proj,Coord Pt1,Coord Pt2,Vect3d Pix00,Vect3d Pix01,Vect3d Pix10,Vect3d Pix11);

/*----------------------------------------------------------------------------
 * Nom      : <Azimuth_DrawGlobe>
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
void Azimuth_DrawGlobe(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj){

   char    buf[256];
   Coord   co;
   Vect3d  vr;

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

      glTranslated(0.0,0.0,1.0);
      if (VP->ColorFLake && VP->ColorFCoast) {
         glColor3us(VP->ColorFLake->red,VP->ColorFLake->green,VP->ColorFLake->blue);
         glDisable(GL_STENCIL_TEST);
         glPolygonMode(GL_FRONT,GL_FILL);
         glDrawCircle(64,GL_POLYGON);
         if (Interp)
            glFeedbackProcess(Interp,GL_2D);
      }

      glEnable(GL_STENCIL_TEST);
      glColor3us(VP->ColorCoast->red,VP->ColorCoast->green,VP->ColorCoast->blue);
      glLineWidth(ABS(Proj->Geo->Params.Coast));
      glDrawCircle(64,GL_LINE_STRIP);

      glTranslated(0.0,0.0,-1.0);

      if (Interp)
         glFeedbackProcess(Interp,GL_2D);
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

      /*Trace des latitudes*/
      if (Interp) glFeedbackInit(4000,GL_2D);
      for(co.lon=-180;co.lon<180;co.lon+=Proj->Geo->Params.CoordDef){
         glBegin(GL_LINE_STRIP);
         for(co.lat=-90+Proj->Geo->Params.CoordDef;co.lat<=90-Proj->Geo->Params.CoordDef;co.lat+=1){
            Proj->Type->Project(Proj->Params,&co,&vr,1);
            glVertex3dv(vr);
         }
         glEnd();
      }
      if (Interp) glFeedbackProcess(Interp,GL_2D);

      /*Trace des longitudes*/
      if (Interp) glFeedbackInit(4000,GL_2D);
      for(co.lat=-90+Proj->Geo->Params.CoordDef;co.lat<90;co.lat+=Proj->Geo->Params.CoordDef){
        glBegin(GL_LINE_STRIP);
        for(co.lon=-180;co.lon<=180;co.lon+=1){
            Proj->Type->Project(Proj->Params,&co,&vr,1);
            glVertex3dv(vr);
         }
        glEnd();
     }
      if (Interp) glFeedbackProcess(Interp,GL_2D);

   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Azimuth_Init>
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
int Azimuth_Init(Tcl_Interp *Interp){

   Projection_CreateType("azimuthal equidistant",PROJSPHERE,
      AzimuthDist_Locate,
      Azimuth_Render,
      Azimuth_DrawGlobe,
      NULL,
      AzimuthDist_UnProject,
      AzimuthDist_Project,
      Azimuth_ProjectPoint,
      Azimuth_ProjectLine);

   Projection_CreateType("azimuthal equal-area",PROJSPHERE,
      AzimuthArea_Locate,
      Azimuth_Render,
      Azimuth_DrawGlobe,
      NULL,
      AzimuthArea_UnProject,
      AzimuthArea_Project,
      Azimuth_ProjectPoint,
      Azimuth_ProjectLine);
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <AzimuthDist_Locate>
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
int AzimuthDist_Locate(Projection *Proj,double Lat,double Lon,int Undo) {

   Coord  loc;
   Vect3d pix;

   if (Undo) {
      loc.lat=Lat;
      loc.lon=Lon;
      loc.elev=0.0;
      AzimuthDist_Project(Proj->Params,&loc,&pix,1);
      glTranslated(pix[0],pix[1],0.0);
   } else {
   }
   return 0;
}

/*----------------------------------------------------------------------------
 * Nom      : <AzimuthArea_Locate>
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
int AzimuthArea_Locate(Projection *Proj,double Lat,double Lon,int Undo) {

   Coord  loc;
   Vect3d pix;

   if (Undo) {
      loc.lat=Lat;
      loc.lon=Lon;
      loc.elev=0.0;
      AzimuthArea_Project(Proj->Params,&loc,&pix,1);
      glTranslated(pix[0],pix[1],0.0);
   } else {
   }
   return 0;
}

/*----------------------------------------------------------------------------
 * Nom      : <Azimuth_Render>
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

void Azimuth_Render(Projection *Proj,GLuint List,Vect3d *Data,unsigned int *Idx,char *Col,float* Tex,int Mode,int Nb,Vect3d V0,Vect3d V1) {

   int stride=0;

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
 * Nom      : <Azimuth_ProjectLine>
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
Tcl_Obj* Azimuth_ProjectLine(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord *Co,int NCo){

   Tcl_Obj *obj,*objf,*objb;
   Coord  prev;
   Vect3d pix00,pix01,pix10,pix11;
   Vect3d pixs[2000],pixc[2000];
   int    i=0,n=0,c;

   obj=Tcl_NewListObj(0,NULL);
   objf=Tcl_NewListObj(0,NULL);
   objb=Tcl_NewListObj(0,NULL);

   /*Boucler sur tout les points definissant la region*/
   prev=Co[0];

   for(i=1;i<NCo;i++) {

      Azimuth_SegLine(VP,Proj,prev,Co[i],pix00,pix01,pix10,pix11);
      Vect_Assign(pixs[n],pix00);
      n++;
      Vect_Assign(pixs[n],pix01);
      n++;

      prev=Co[i];

      if (n>2000) {
         Tcl_AppendResult(Interp,"Azimuth_ProjectLine: Too many coordinates (Max:2000)",(char*)NULL);
         n=0;
         break;
      }
   }


   /*Creer les listes des points*/

   if (n) {
      LiangBarsky_PolygonClip2D(pixs,n,pixc,&c,0,0,VP->Width,VP->Height);
      for (i=0;i<c;i++){
         Tcl_ListObjAppendElement(Interp,objf,Tcl_NewDoubleObj(pixc[i][0]+VP->x));
         Tcl_ListObjAppendElement(Interp,objf,Tcl_NewDoubleObj(pixc[i][1]+VP->y));
      }
   }

   Tcl_ListObjAppendElement(Interp,obj,objf);
   Tcl_ListObjAppendElement(Interp,obj,objb);

   return(obj);
}

/*----------------------------------------------------------------------------
 * Nom      : <Azimuth_ProjectPoint>
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
Tcl_Obj* Azimuth_ProjectPoint(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord Pt1,int Any){

   Tcl_Obj *obj;
   Vect3d   pix,in;

   obj=Tcl_NewListObj(0,NULL);

   Proj->Type->Project(Proj->Params,&Pt1,&in,1);
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
 * Nom      : <Azimuth_SegLine>
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
int Azimuth_SegLine(ViewportItem *VP,Projection *Proj,Coord Pt1,Coord Pt2,Vect3d Pix00,Vect3d Pix01,Vect3d Pix10,Vect3d Pix11) {

   Vect3d in[2];
   Coord  co[2];

   co[0].lat=Pt1.lat;co[0].lon=Pt1.lon;
   co[1].lat=Pt2.lat;co[1].lon=Pt2.lon;

   CLAMPLAT(co[0].lat);
   CLAMPLAT(co[1].lat);

   /*Localisation des extremites de la ligne*/
   Proj->Type->Project(Proj->Params,co,in,2);

   gluProject(in[0][0],in[0][1],in[0][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix00[0],&Pix00[1],&Pix00[2]);
   Pix00[1]=Proj->Params->VP->Height-Pix00[1];
   gluProject(in[1][0],in[1][1],in[1][2],VP->GLModR,VP->GLProj,VP->GLView,&Pix01[0],&Pix01[1],&Pix01[2]);
   Pix01[1]=Proj->Params->VP->Height-Pix01[1];

   return(0);
}

/*----------------------------------------------------------------------------
 * Nom      : <AzimuthDist_Project>
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
 *----------------------------------------------------------------------------
*/
unsigned long AzimuthDist_Project(ProjParams *Params,GeoVect *Loc,GeoVect *Pix,long Nb) {

   long n,e=0;
   double        k,lon,cllon,cllat,slat;
   Coord         loc;
   Vect3d *out;

   out=(Vect3d*)(Pix?Pix:Loc);

   for(n=0;n<ABS(Nb);n++) {
      if (((Coord*)Loc)[n].lat==-999.0 || ((Coord*)Loc)[n].lon==-999.0) {
         out[n][2]=-999.0;
      } else {

         loc.lat=DEG2RAD(((Coord*)Loc)[n].lat);
         loc.lon=DEG2RAD(((Coord*)Loc)[n].lon);
         loc.elev=((Coord*)Loc)[n].elev;

         lon=DEG2RAD(Params->Lon);
         cllon=cos(loc.lon-lon);
         cllat=cos(loc.lat);
         slat=sin(loc.lat);
         k=acos(Params->SLat*slat+cllon*Params->CLat*cllat);
         k=k/sin(k);

         out[n][0]=0.322580645*(k*cllat*sin(loc.lon-lon));
         out[n][1]=0.322580645*(k*(Params->CLat*slat-Params->SLat*cllat*cllon));
         out[n][2]=1.0+loc.elev*Params->Scale*Params->ZFactor;
         e++;
      }
   }
   return(e);
}

/*----------------------------------------------------------------------------
 * Nom      : <AzimuthArea_Project>
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
 *----------------------------------------------------------------------------
*/
unsigned long AzimuthArea_Project(ProjParams *Params,GeoVect *Loc,GeoVect *Pix,long Nb) {

   long n,e=0;
   double        k,lon,cllon,cllat,slat;
   Coord         loc;
   Vect3d       *out;

   out=(Vect3d*)(Pix?Pix:Loc);

   for(n=0;n<ABS(Nb);n++) {
      if (((Coord*)Loc)[n].lat==-999.0 || ((Coord*)Loc)[n].lon==-999.0) {
         out[n][2]=-999.0;
      } else {

         loc.lat=DEG2RAD(((Coord*)Loc)[n].lat);
         loc.lon=DEG2RAD(((Coord*)Loc)[n].lon);
         loc.elev=((Coord*)Loc)[n].elev;

         lon=DEG2RAD(Params->Lon);
         cllon=cos(loc.lon-lon);
         cllat=cos(loc.lat);
         slat=sin(loc.lat);

         k=sqrt(2.0/(1+Params->SLat*slat+cllon*Params->CLat*cllat));

         out[n][0]=0.5*(k*cllat*sin(loc.lon-lon));
         out[n][1]=0.5*(k*(Params->CLat*slat-Params->SLat*cllat*cllon));
         out[n][2]=(loc.elev*Params->Scale+EARTHRADIUS)/EARTHRADIUS;
         e++;
      }
   }
   return(e);
}

/*----------------------------------------------------------------------------
 * Nom      : <AzimuthDist_UnProject>
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
 *----------------------------------------------------------------------------
*/
int AzimuthDist_UnProject(ViewportItem *VP,ProjParams *Params,Coord *Loc,Vect3d Pix) {

   Vect3d obj;
   double depth=1.0;
   double c,lon,sinc,cosc;

   gluUnProject(Pix[0],VP->Height-Pix[1],depth,VP->GLModS,VP->GLProj,VP->GLView,&obj[0],&obj[1],&obj[2]);

   if (Vect_InterPlane(VP->Cam->Basis,obj,1)) {

      lon=DEG2RAD(Params->Lon);

      obj[0]*=3.1;
      obj[1]*=3.1;

      c=hypot(obj[0],obj[1]);
      sinc=sin(c);
      cosc=cos(c);

      Loc->lat=asin(cosc*Params->SLat+(obj[1]*sinc*Params->CLat)/c);
      if (Loc->lat==DEG2RAD(90.0)) {
           Loc->lon=lon+atan2(obj[0],obj[1]);
      } else if (Loc->lat==-DEG2RAD(90.0)) {
           Loc->lon=lon+atan2(-obj[0],obj[1]);
      } else {
           Loc->lon=lon+atan2(obj[0]*sinc,c*Params->CLat*cosc-obj[1]*Params->SLat*sinc);
      }

      Loc->lat=RAD2DEG(Loc->lat);
      Loc->lon=RAD2DEG(Loc->lon);

      if (Loc->lon<=(180.0+Params->Lon) && Loc->lon>=(-180.0+Params->Lon) && Loc->lat<=90.0 && Loc->lat>=-90.0) {
         return(1);
      }
   }
   Loc->lat=-999.0;
   Loc->lon=-999.0;

   return(0);
}

/*----------------------------------------------------------------------------
 * Nom      : <AzimuthArea_UnProject>
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
 *----------------------------------------------------------------------------
*/
int AzimuthArea_UnProject(ViewportItem *VP,ProjParams *Params,Coord *Loc,Vect3d Pix) {

   Vect3d obj;
   double depth=1.0;
   double c,p,lon,sinc,cosc;

   gluUnProject(Pix[0],VP->Height-Pix[1],depth,VP->GLModS,VP->GLProj,VP->GLView,&obj[0],&obj[1],&obj[2]);

   if (Vect_InterPlane(VP->Cam->Basis,obj,1)) {

      lon=DEG2RAD(Params->Lon);

      obj[0]*=2.0;
      obj[1]*=2.0;

      p=hypot(obj[0],obj[1]);
      c=2*asin(0.5*p);

      sinc=sin(c);
      cosc=cos(c);

      Loc->lat=asin(cosc*Params->SLat+(obj[1]*sinc*Params->CLat)/p);
      if (Loc->lat==DEG2RAD(90.0)) {
           Loc->lon=lon+atan2(obj[0],obj[1]);
      } else if (Loc->lat==-DEG2RAD(90.0)) {
           Loc->lon=lon+atan2(-obj[0],obj[1]);
      } else {
           Loc->lon=lon+atan2(obj[0]*sinc,p*Params->CLat*cosc-obj[1]*Params->SLat*sinc);
      }

      Loc->lat=RAD2DEG(Loc->lat);
      Loc->lon=RAD2DEG(Loc->lon);

      if (Loc->lon<=(180.0+Params->Lon) && Loc->lon>=(-180.0+Params->Lon) && Loc->lat<=90.0 && Loc->lat>=-90.0) {
         return(1);
      }
   }
   Loc->lat=-999.0;
   Loc->lon=-999.0;

   return(0);
}
