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
void Azimuth_DrawFirst(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
void Azimuth_DrawGlobe(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj);
int  Azimuth_Locate(Projection *Proj,double Lat,double Lon,int Undo);
void Azimuth_Render(Projection *Proj,GLuint List,Vect3d *Data,unsigned int *Idx,char *Col,float* Tex,int Mode,int Nb,int Stride,Vect3d V0,Vect3d V1);

/*Fonctions de transformations*/
unsigned long AzimuthDist_Project(const Projection* restrict const Proj,GeoVect *Loc,GeoVect *Pix,long Nb);
int           AzimuthDist_UnProject(ViewportItem *VP,Projection *Proj,Coord *Loc,Vect3d Pix);
unsigned long AzimuthArea_Project(const Projection* restrict const Proj,GeoVect *Loc,GeoVect *Pix,long Nb);
int           AzimuthArea_UnProject(ViewportItem *VP,Projection *Proj,Coord *Loc,Vect3d Pix);
Tcl_Obj*      Azimuth_ProjectPoint(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord Pt1,int Any);
Tcl_Obj*      Azimuth_ProjectLine(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj,Coord *Co,int NCo);
int           Azimuth_SegLine(ViewportItem *VP,Projection *Proj,Coord Pt1,Coord Pt2,Vect3d Pix00,Vect3d Pix01,Vect3d Pix10,Vect3d Pix11);

/*----------------------------------------------------------------------------
 * Nom      : <Azimuth_DrawFirst>
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

void Azimuth_DrawFirst(Tcl_Interp *Interp,ViewportItem *VP,Projection *Proj){

   char    buf[256];
   Coord   co;
   Vect3d  vr;

   /*Affichage des latlons*/
   if (Proj->Geo->Params.CoordLoc && VP->ColorCoord) {

     if (Interp) {
         Tk_CanvasPsColor(Interp,VP->canvas,VP->ColorCoord);
         sprintf(buf,"%i setlinewidth 1 setlinecap 1 setlinejoin\n",ABS(Proj->Geo->Params.CoordLoc)-1);
         Tcl_AppendResult(Interp,buf,(char*)NULL);
      }

      glLineWidth(ABS(Proj->Geo->Params.CoordLoc));
      glColor3us(VP->ColorCoord->red,VP->ColorCoord->green,VP->ColorCoord->blue);

      /*Trace des latitudes*/
      if (Interp) glFeedbackInit(4000,GL_2D);
      for(co.Lon=-180;co.Lon<180;co.Lon+=Proj->Geo->Params.CoordDef){
         glBegin(GL_LINE_STRIP);
         for(co.Lat=-90+Proj->Geo->Params.CoordDef;co.Lat<=90-Proj->Geo->Params.CoordDef;co.Lat+=1){
            Proj->Type->Project(Proj,(GeoVect*)&co,(GeoVect*)&vr,1);
            glVertex3dv(vr);
         }
         glEnd();
      }
      if (Interp) glFeedbackProcess(Interp,GL_2D);

      /*Trace des longitudes*/
      if (Interp) glFeedbackInit(4000,GL_2D);
      for(co.Lat=(floor(-90.0/Proj->Geo->Params.CoordDef)*Proj->Geo->Params.CoordDef)+Proj->Geo->Params.CoordDef;co.Lat<90;co.Lat+=Proj->Geo->Params.CoordDef){
        glBegin(GL_LINE_STRIP);
        for(co.Lon=-180;co.Lon<=180;co.Lon+=1){
            Proj->Type->Project(Proj,(GeoVect*)&co,(GeoVect*)&vr,1);
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

   Projection_CreateType(Interp,"azimuthal equidistant",PROJSPHERE,
      Azimuth_Locate,
      Azimuth_Render,
      Azimuth_DrawFirst,
      NULL,
      Azimuth_DrawGlobe,
      AzimuthDist_UnProject,
      AzimuthDist_Project,
      Azimuth_ProjectPoint,
      Azimuth_ProjectLine);

   Projection_CreateType(Interp,"azimuthal equal-area",PROJSPHERE,
      Azimuth_Locate,
      Azimuth_Render,
      Azimuth_DrawFirst,
      NULL,
      Azimuth_DrawGlobe,
      AzimuthArea_UnProject,
      AzimuthArea_Project,
      Azimuth_ProjectPoint,
      Azimuth_ProjectLine);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Azimuth_Locate>
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
int Azimuth_Locate(Projection *Proj,double Lat,double Lon,int Undo) {

   GeoVect loc;

   if (Undo) {
      loc.C.Lat=Lat;
      loc.C.Lon=Lon;
      loc.C.Elev=0.0;
      AzimuthDist_Project(Proj,&loc,NULL,1);
      glTranslated(loc.V[0],loc.V[1],0.0);
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

   GeoVect loc;

   if (Undo) {
      loc.C.Lat=Lat;
      loc.C.Lon=Lon;
      loc.C.Elev=0.0;
      Proj->Type->Project(Proj,&loc,NULL,1);
      glTranslated(loc.V[0],loc.V[1],0.0);
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

void Azimuth_Render(Projection *Proj,GLuint List,Vect3d *Data,unsigned int *Idx,char *Col,float* Tex,int Mode,int Nb,int Stride,Vect3d V0,Vect3d V1) {

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

   Proj->Type->Project(Proj,(GeoVect*)&Pt1,(GeoVect*)&in,1);
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

   return(0);
}

/*----------------------------------------------------------------------------
 * Nom      : <AzimuthDist_Project>
 * Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Transforme les coordonnes lat-lon en projection cylindrique.
 *
 * Parametres :
 *  <Proj>       : Parametres de la projection
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
unsigned long AzimuthDist_Project(const Projection* restrict const Proj,GeoVect *Loc,GeoVect *Pix,long Nb) {

   Coord    loc;
   GeoVect *out;
   long     n,e=0;
   double   r,k,lon,cllon,cllat,slat;

   out=(Pix?Pix:Loc);
   r=Proj->Scale*Proj->ZFactor;

   for(n=0;n<ABS(Nb);n++) {
      if (Loc[n].C.Lat==-999.0 || Loc[n].C.Lon==-999.0) {
         out[n].V[2]=-999.0;
      } else {

         loc.Lat=DEG2RAD(Loc[n].C.Lat);
         loc.Lon=DEG2RAD(Loc[n].C.Lon);
         loc.Elev=Loc[n].C.Elev;

         lon=DEG2RAD(Proj->Lon);
         cllon=cos(loc.Lon-lon);
         cllat=cos(loc.Lat);
         slat=sin(loc.Lat);
         k=acos(Proj->SLat*slat+cllon*Proj->CLat*cllat);
         k=k/sin(k);

         out[n].V[0]=0.322580645*(k*cllat*sin(loc.Lon-lon));
         out[n].V[1]=0.322580645*(k*(Proj->CLat*slat-Proj->SLat*cllat*cllon));
         out[n].V[2]=(loc.Elev==0.0)?1.0:1.0+loc.Elev*r;
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
unsigned long AzimuthArea_Project(const Projection* restrict const Proj,GeoVect *Loc,GeoVect *Pix,long Nb) {

   Coord    loc;
   GeoVect *out;
   long     n,e=0;
   double   r,k,lon,cllon,cllat,slat;

   out=(Pix?Pix:Loc);
   r=Proj->Scale*Proj->ZFactor;

   for(n=0;n<ABS(Nb);n++) {
      if (Loc[n].C.Lat==-999.0 || Loc[n].C.Lon==-999.0) {
         out[n].V[2]=-999.0;
      } else {

         loc.Lat=DEG2RAD(Loc[n].C.Lat);
         loc.Lon=DEG2RAD(Loc[n].C.Lon);
         loc.Elev=Loc[n].C.Elev;

         lon=DEG2RAD(Proj->Lon);
         cllon=cos(loc.Lon-lon);
         cllat=cos(loc.Lat);
         slat=sin(loc.Lat);

         k=sqrt(2.0/(1+Proj->SLat*slat+cllon*Proj->CLat*cllat));

         out[n].V[0]=0.5*(k*cllat*sin(loc.Lon-lon));
         out[n].V[1]=0.5*(k*(Proj->CLat*slat-Proj->SLat*cllat*cllon));
         out[n].V[2]=(loc.Elev==0.0)?1.0:1.0+loc.Elev*r;
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
int AzimuthDist_UnProject(ViewportItem *VP,Projection *Proj,Coord *Loc,Vect3d Pix) {

   Vect3d obj;
   double depth=1.0;
   double c,lon,sinc,cosc;

   gluUnProject(Pix[0],VP->Height-Pix[1],depth,VP->GLModS,VP->GLProj,VP->GLView,&obj[0],&obj[1],&obj[2]);

   if (Vect_InterPlane(VP->Cam->Basis,obj,1)) {

      lon=DEG2RAD(Proj->Lon);

      obj[0]*=3.1;
      obj[1]*=3.1;

      c=hypot(obj[0],obj[1]);
      sinc=sin(c);
      cosc=cos(c);

      Loc->Lat=asin(cosc*Proj->SLat+(obj[1]*sinc*Proj->CLat)/c);
      if (Loc->Lat==DEG2RAD(90.0)) {
           Loc->Lon=lon+atan2(obj[0],obj[1]);
      } else if (Loc->Lat==-DEG2RAD(90.0)) {
           Loc->Lon=lon+atan2(-obj[0],obj[1]);
      } else {
           Loc->Lon=lon+atan2(obj[0]*sinc,c*Proj->CLat*cosc-obj[1]*Proj->SLat*sinc);
      }

      Loc->Lat=RAD2DEG(Loc->Lat);
      Loc->Lon=RAD2DEG(Loc->Lon);

      if (Loc->Lon<=(180.0+Proj->Lon) && Loc->Lon>=(-180.0+Proj->Lon) && Loc->Lat<=90.0 && Loc->Lat>=-90.0) {
         return(1);
      }
   }
   Loc->Lat=-999.0;
   Loc->Lon=-999.0;

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
int AzimuthArea_UnProject(ViewportItem *VP,Projection *Proj,Coord *Loc,Vect3d Pix) {

   Vect3d obj;
   double depth=1.0;
   double c,p,lon,sinc,cosc;

   gluUnProject(Pix[0],VP->Height-Pix[1],depth,VP->GLModS,VP->GLProj,VP->GLView,&obj[0],&obj[1],&obj[2]);

   if (Vect_InterPlane(VP->Cam->Basis,obj,1)) {

      lon=DEG2RAD(Proj->Lon);

      obj[0]*=2.0;
      obj[1]*=2.0;

      p=hypot(obj[0],obj[1]);
      c=2*asin(0.5*p);

      sinc=sin(c);
      cosc=cos(c);

      Loc->Lat=asin(cosc*Proj->SLat+(obj[1]*sinc*Proj->CLat)/p);
      if (Loc->Lat==DEG2RAD(90.0)) {
           Loc->Lon=lon+atan2(obj[0],obj[1]);
      } else if (Loc->Lat==-DEG2RAD(90.0)) {
           Loc->Lon=lon+atan2(-obj[0],obj[1]);
      } else {
           Loc->Lon=lon+atan2(obj[0]*sinc,p*Proj->CLat*cosc-obj[1]*Proj->SLat*sinc);
      }

      Loc->Lat=RAD2DEG(Loc->Lat);
      Loc->Lon=RAD2DEG(Loc->Lon);

      if (Loc->Lon<=(180.0+Proj->Lon) && Loc->Lon>=(-180.0+Proj->Lon) && Loc->Lat<=90.0 && Loc->Lat>=-90.0) {
         return(1);
      }
   }
   Loc->Lat=-999.0;
   Loc->Lon=-999.0;

   return(0);
}
