/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers raster
 * Fichier      : GeoTex.c
 * Creation     : Avril 2007 - J.P. Gauthier
 *
 * Description  : Fonctions de manipulations et de texture geographique.
 *
 * Remarques    :
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
 *=========================================================
 */

#include "GeoTex.h"
#include "tclGDAL.h"

TCL_DECLARE_MUTEX(MUTEX_GEOTEX)

extern int TD2GDAL[];
extern Vect3d GDB_NMap[181][361];
extern int GDB_Loc(GDB_Box Box,Projection *Proj,float X0,float X1,float Y0,float Y1);
static int GeoTex_TileNb=0;
static int GeoTex_TileNbMax=1024;

int          GeoTex_Get(GDAL_Band *Band,TGeoTexTile *Tile);
int          GeoTex_Texture(GDAL_Band *Band,TGeoTexTile *Tile);
int          GeoTex_Parse(GDAL_Band* Band,TGeoTexTile **Tile,Projection *Proj,ViewportItem *VP,int Resolution,int X0,int Y0);
TGeoTexTile *GeoTex_Pick(TGeoTex *Tex,int Res,int *X,int *Y);
int          GeoTex_Resolution(GDAL_Band *Band,Projection *Proj);
int          GeoTex_Render(GDAL_Band *Band,TGeoTexTile *Tile,Projection *Proj,ViewportItem *VP,int Lock);
int          GeoTex_Limit(GDAL_Band *Band,TGeoTexTile *Tile,Projection *Proj);
void         GeoTex_Sample(GDAL_Band *Band,TGeoTexTile *Tile,Projection *Proj);
void         GeoTex_Qualify(GDAL_Band *Band);
TGeoTexTile *GeoTex_New(GDAL_Band *Band,int Resolution,int X0,int Y0);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_ThreadProc>
 * Creation     : Avril 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Lire les donnees des tuiles et calculer les points d'attaches dans une sceonde thread.
 *
 * Parametres   :
 *   <clientData>: Bande
 *
 * Retour       :
 *   <...>      : Code de reussite
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
Tcl_ThreadCreateType GeoTex_ThreadProc(ClientData clientData) {

   GDAL_Band *band=(GDAL_Band*)clientData;
   Projection *proj=band->Tex.Proj;

   if (proj) {
      proj->Loading+=5;
      GeoTex_Parse(band,&band->Tex.Tile,proj,band->Tex.Proj->VP,band->Tex.ResN,0,0);
      proj->Loading=0;
   }

   band->Tex.ThreadId=(Tcl_ThreadId)NULL;

   Tcl_ExitThread(0);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_Clear>
 * Creation     : Avril 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Liberer les ressources de la geotexture.
 *
 * Parametres   :
 *   <Tex>      : GeoTexture
 *   <Tile>     : Tuile
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GeoTex_Clear(TGeoTex *Tex,char Flags,int Res,int Nb) {

   if (Tex && Tex->Tile>(TGeoTexTile*)0x1) {
      Tex->Tile=GeoTex_ClearTile(Tex->Tile,Flags,Res,Nb);

      /*Reset clear flags*/
      if (Tex->Tile) {
         Tex->Tile->Flag&=~Flags;
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_ClearTile>
 * Creation     : Avril 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Liberer les ressources d'une tuile de geotexture.
 *
 * Parametres   :
 *   <Tile>     : Tuile
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
TGeoTexTile* GeoTex_ClearTile(TGeoTexTile *Tile,char Flags,int Res,int Nb) {

    /*Clear subtiles*/
    if (Tile>(TGeoTexTile*)0x1) {
       Tile->Sub[0]=GeoTex_ClearTile(Tile->Sub[0],Flags,Res,Nb);
       Tile->Sub[1]=GeoTex_ClearTile(Tile->Sub[1],Flags,Res,Nb);
       Tile->Sub[2]=GeoTex_ClearTile(Tile->Sub[2],Flags,Res,Nb);
       Tile->Sub[3]=GeoTex_ClearTile(Tile->Sub[3],Flags,Res,Nb);

      /*If the tile is valid and within the limits*/
      if ((!Res || Tile->Res<Res) && GeoTex_TileNb>Nb) {

         Tcl_MutexLock(&MUTEX_GEOTEX);

         /*Clear GL textures*/
         if (Flags&GEOTEX_CLRTEX) {
            if (Tile->Tx) glDeleteTextures(1,&Tile->Tx); Tile->Tx=0;
         }

         /*Clear coordinates buffer*/
         if (Flags&GEOTEX_CLRCOO) {
            Tile->Flag&=~GEOTEX_COOR;
            if (Tile->Tl)   free(Tile->Tl); Tile->Tl=NULL;
            if (Tile->Nr)   free(Tile->Nr); Tile->Nr=NULL;
         }

         /*Clear band data*/
         if (Flags&GEOTEX_CLRDTA) {
            Tile->Flag&=~GEOTEX_DATA;
            if (Tile->Data) free(Tile->Data); Tile->Data=NULL;
         }

         /*If everything is cleared, get rid of the tile*/
         if (Flags==GEOTEX_CLRALL) {
            free(Tile); Tile=NULL;

            /*Decrement global number of tiles*/
            GeoTex_TileNb--;
         }

         Tcl_MutexUnlock(&MUTEX_GEOTEX);
      }
   }
   return(Tile);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_Texture>
 * Creation     : Avril 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Creer les sous textures de la bande avec les parametres OpenGL.
 *
 * Parametres   :
 *   <Band>     : Bande
 *   <Tile>     : Tuile a texturer
 *
 * Retour       :
 *   <...>      : Code de reussite
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoTex_Texture(GDAL_Band *Band,TGeoTexTile *Tile) {

   int       nc;
   float    *buf;
   GLuint    sc[]={GL_RED_SCALE, GL_GREEN_SCALE, GL_BLUE_SCALE, GL_ALPHA_SCALE };
   GLuint    bc[]={GL_RED_BIAS, GL_GREEN_BIAS, GL_BLUE_BIAS, GL_ALPHA_BIAS };

   if (!Band->Tex.Indexed && Band->Spec->Map) {
      Band->Tex.Scale[0]=Band->Tex.Scale[1]=Band->Tex.Scale[2]=Band->Tex.Scale[3]=-1.0;
      Band->Tex.Bias[0]=Band->Tex.Bias[1]=Band->Tex.Bias[2]=Band->Tex.Bias[3]=0.0;

      if (!GLRender->ShaderAvailable) {
         glEnable(GL_COLOR_TABLE);
         glColorTable(GL_COLOR_TABLE,GL_RGBA,256,GL_RGBA,GL_UNSIGNED_BYTE,(GLvoid*)Band->Spec->Map->Color);

         for (nc=0;nc<Band->Def->NC;nc++) {
            switch(Band->Def->Type) {
               case TD_UByte:  Band->Tex.Scale[nc]=((0x1<<8)-1)/(Band->Spec->Map->Max[nc]-Band->Spec->Map->Min[nc]);
                               Band->Tex.Bias[nc]=-Band->Spec->Map->Min[nc]/((0x1<<8)-1);
                              break;
               case TD_Byte:   Band->Tex.Scale[nc]=((0x1<<7)-1)/(Band->Spec->Map->Max[nc]-Band->Spec->Map->Min[nc]);
                               Band->Tex.Bias[nc]=-Band->Spec->Map->Min[nc]/((0x1<<7)-1);
                               break;
               case TD_UInt16: Band->Tex.Scale[nc]=((0x1<<16)-1)/(Band->Spec->Map->Max[nc]-Band->Spec->Map->Min[nc]);
                               Band->Tex.Bias[nc]=-Band->Spec->Map->Min[nc]/((0x1<<16)-1);
                               break;
               case TD_Int16:  Band->Tex.Scale[nc]=((0x1<<15)-1)/(Band->Spec->Map->Max[nc]-Band->Spec->Map->Min[nc]);
                               Band->Tex.Bias[nc]=-Band->Spec->Map->Min[nc]/((0x1<<15)-1);
                               break;
               case TD_UInt32:
               case TD_Int32:  Band->Tex.Scale[nc]=(((unsigned int)0x1<<31)-1)/(Band->Spec->Map->Max[nc]-Band->Spec->Map->Min[nc]);
                               Band->Tex.Bias[nc]=-Band->Spec->Map->Min[nc]/(((unsigned int)0x1<<31)-1);
                               break;
               case TD_Float32:
               case TD_Float64: Band->Tex.Scale[nc]=1.0/(Band->Spec->Map->Max[nc]-Band->Spec->Map->Min[nc]);
                                Band->Tex.Bias[nc]=-Band->Spec->Map->Min[nc];
                                break;
            }
            glPixelTransferf(sc[nc],Band->Tex.Scale[nc]);
            glPixelTransferf(bc[nc],Band->Tex.Bias[nc]*Band->Tex.Scale[nc]);
            if (GLRender->GLDebug)
               fprintf(stdout,"(DEBUG) GeoTex_Texture: Normalizing factor (%i) Sc=%f Bc=%f\n",nc,Band->Tex.Scale[nc],Band->Tex.Bias[nc]);
         }
      }
   }

   if (Band->Tex.Indexed && Band->Spec->Map) {
      glEnable(GL_COLOR_TABLE);
      glColorTable(GL_COLOR_TABLE,GL_RGBA,256,GL_RGBA,GL_UNSIGNED_BYTE,(GLvoid*)Band->Spec->Map->Color);
   }

   /*Create OpenGL texture*/
   glGenTextures(1,&Tile->Tx);
   if (Tile->Tx<=0) {
      fprintf(stderr,"(ERROR) GeoTex_Texture: Unable to allocate texture memory\n");
      Tile->Tx=0;
   } else {
      glBindTexture(GL_TEXTURE_2D,Tile->Tx);
      glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP);
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP);

      if (Band->Tex.Indexed) {
         Band->Tex.IType=GLRender->Ext[ARB_texture_compression]?GL_COMPRESSED_RGBA_S3TC_DXT3_EXT:GL_RGBA;
      } else {
         if (!GLRender->ShaderAvailable && Band->Def->NC==1) {
            Band->Tex.IType=GL_RGBA;
            glPixelTransferf(sc[1],Band->Tex.Scale[0]);
            glPixelTransferf(bc[1],Band->Tex.Bias[0]*Band->Tex.Scale[0]);
            glPixelTransferf(sc[2],Band->Tex.Scale[0]);
            glPixelTransferf(bc[2],Band->Tex.Bias[0]*Band->Tex.Scale[0]);
            glPixelTransferf(sc[3],Band->Tex.Scale[0]);
            glPixelTransferf(bc[3],Band->Tex.Bias[0]*Band->Tex.Scale[0]);
         }
      }

      /*OpenGL does not manage 64 bit (double data), so we have to use a temporery float buffer*/
      if (Band->Def->Type==TD_Float64) {
         if (buf=(float*)malloc(Tile->Nx*Tile->Ny*sizeof(float))) {
            for(nc=0;nc<Tile->Nx*Tile->Ny;nc++) {
               buf[nc]=((double*)Tile->Data)[nc];
            }
            glTexImage2D(GL_TEXTURE_2D,0,Band->Tex.IType,Tile->Nx,Tile->Ny,0,Band->Tex.Type,GL_FLOAT,(GLvoid*)buf);
            free(buf);
         }
      } else {
         glTexImage2D(GL_TEXTURE_2D,0,Band->Tex.IType,Tile->Nx,Tile->Ny,0,Band->Tex.Type,Band->Tex.Dim,(GLvoid*)Tile->Data);
      }
   }

   for (nc=0;nc<4;nc++) {
      glPixelTransferf(sc[nc],1.0);
      glPixelTransferf(bc[nc],0.0);
   }
   glDisable(GL_COLOR_TABLE);

  return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_Limit>
 * Creation     : Janvier 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Calculer les coordonnes geographiques des limites des tuiles.
 *
 * Parametres   :
 *   <Band>     : Bande
 *   <Proj>     : Projection
 *
 * Retour       :
 *   <...>      : Code de reussite
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoTex_Limit(GDAL_Band *Band,TGeoTexTile *Tile,Projection *Proj) {

   unsigned int x0,y0,x1,y1,r;

   r=Tile->Res*Band->Spec->TexSize;

   x0=Tile->Dx; y0=Tile->Dy; /*Coin superieur gauche*/
   x1=x0+r;     y1=y0+r;     /*Coin inferieur gauche*/

   x1=x1>Band->Ref->X1?Band->Ref->X1:x1;
   y1=y1>Band->Ref->Y1?Band->Ref->Y1:y1;

   Tile->Box.Co[0].Elev=0.0;
   Tile->Box.Co[1].Elev=0.0;
   Tile->Box.Co[2].Elev=0.0;
   Tile->Box.Co[3].Elev=0.0;
   Tile->Box.Co[4].Elev=0.0;
   Tile->Box.Co[5].Elev=0.0;
   Tile->Box.Co[6].Elev=0.0;
   Tile->Box.Co[7].Elev=0.0;

   if (!Proj->Geographic) {
      Tile->Box.Co[0].Lat=y0;Tile->Box.Co[0].Lon=x0;
      Tile->Box.Co[1].Lat=y1;Tile->Box.Co[1].Lon=x0;
      Tile->Box.Co[2].Lat=y1;Tile->Box.Co[2].Lon=x1;
      Tile->Box.Co[3].Lat=y0;Tile->Box.Co[3].Lon=x1;
      Tile->Box.Co[4].Lat=y1;Tile->Box.Co[4].Lon=(x0+x1)*0.5;
      Tile->Box.Co[5].Lat=y0;Tile->Box.Co[5].Lon=(x0+x1)*0.5;
      Tile->Box.Co[6].Lat=(y0+y1)*0.5;Tile->Box.Co[4].Lon=x0;
      Tile->Box.Co[7].Lat=(y0+y1)*0.5;Tile->Box.Co[5].Lon=x1;
   } else {
      Band->Ref->Project(Band->Ref,x0,y0,&Tile->Box.Co[0].Lat,&Tile->Box.Co[0].Lon,1,1);
      Band->Ref->Project(Band->Ref,x0,y1,&Tile->Box.Co[1].Lat,&Tile->Box.Co[1].Lon,1,1);
      Band->Ref->Project(Band->Ref,x1,y1,&Tile->Box.Co[2].Lat,&Tile->Box.Co[2].Lon,1,1);
      Band->Ref->Project(Band->Ref,x1,y0,&Tile->Box.Co[3].Lat,&Tile->Box.Co[3].Lon,1,1);
      Band->Ref->Project(Band->Ref,(x0+x1)*0.5,y0,&Tile->Box.Co[4].Lat,&Tile->Box.Co[4].Lon,1,1);
      Band->Ref->Project(Band->Ref,(x0+x1)*0.5,y1,&Tile->Box.Co[5].Lat,&Tile->Box.Co[5].Lon,1,1);
      Band->Ref->Project(Band->Ref,x0,(y0+y1)*0.5,&Tile->Box.Co[6].Lat,&Tile->Box.Co[6].Lon,1,1);
      Band->Ref->Project(Band->Ref,x1,(y0+y1)*0.5,&Tile->Box.Co[7].Lat,&Tile->Box.Co[7].Lon,1,1);
   }

   /*Projection des coins de la texture*/
   Proj->Type->Project(Proj,Tile->Box.Co,Tile->Box.Vr,-8);
   Tile->Box.Nb=4;

   /*Test for overflow on raster limits*/
   Tile->Rx=(Tile->Dx+r)<=Band->Ref->X1?r:Band->Ref->X1-Tile->Dx+1;
   Tile->Ry=(Tile->Dy+r)<=Band->Ref->Y1?r:Band->Ref->Y1-Tile->Dy+1;
   Tile->Nx=(float)Tile->Rx/r*Band->Spec->TexSize;
   Tile->Ny=(float)Tile->Ry/r*Band->Spec->TexSize;

   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_Sample>
 * Creation     : Janvier 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Calculer les coordonnes geographiques des points d'attache de la tuile.
 *
 * Parametres   :
 *   <Band>     : Bande
 *   <Proj>     : Projection
 *   <Tile>     : Tuile a texturer
 *
 * Retour       :
 *   <...>      : Code de reussite
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GeoTex_Sample(GDAL_Band *Band,TGeoTexTile *Tile,Projection *Proj) {

   double        nlx,nly,dx,dy,x0,y0,x,y;
   int           j=0,tlx,tly,t=2,ix,iy,xy;
   unsigned long handle=0;
   short         z;
   GDAL_Band    *tband=NULL;
   TGeoTexTile  *ttile=NULL;
   Vect3d       *tl;
   Vect3d        nr;

   if (Tile->Res==0) {
      return;
   }

   /*Check for tographic info*/
   if (Band->Spec->Topo) {
      tband=GDAL_BandGet(Band->Spec->Topo);
      if (Band->Spec->Topo[0]=='I') {
         handle=gdb_mapopen(GDB_RES,GDB_MAP_DEM,&t);
      }
   }

   /*Check tile sampling resolution*/
   if (!Band->Spec->Topo) {
      tlx=Band->Spec->TexSample;
   } else {
      tlx=Band->Spec->TexSize/(33-Band->Spec->TexSample)*2;
   }

   /*Make sure its within 2 - 128 and it is odd so as to be able to split exactly at the middle*/
   tlx=(tlx>128?128:tlx<2?2:tlx);
   Tile->Tly=Tile->Tlx=(tlx%2?tlx:tlx+1);

   if (!(tl=(Vect3d*)malloc(Tile->Tlx*Tile->Tly*sizeof(Vect3d)))) {
      fprintf(stderr,"(ERROR) GeoTex_Sample: Unable to allocate sub tile coordinates matrix\n");
      return;
   }

   if (!(Tile->Nr=(Vect3d*)malloc(Tile->Tlx*Tile->Tly*sizeof(Vect3d)))) {
      fprintf(stderr,"(ERROR) GeoTex_Sample: Unable to allocate sub tile normal coordinates matrix\n");
   }

   nlx=(double)Tile->Rx/(Tile->Tlx-1);
   nly=(double)Tile->Ry/(Tile->Tly-1);

   for(tlx=0;tlx<Tile->Tlx;tlx++) {
      x0=tlx*nlx-0.5;
      dx=Tile->Dx+x0;

      for(tly=0;tly<Tile->Tly;tly++) {
         y0=tly*nly-0.5;
         dy=Tile->Dy+y0;

         tl[j][2]=0.0;
         if (!Proj->Geographic) {
            tl[j][0]=dx;
            tl[j][1]=dy;
         } else {
            if (!Band->Ref->Project(Band->Ref,dx,dy,&tl[j][1],&tl[j][0],1,1)) {
               fprintf(stderr,"(ERROR) GeoTex_Sample: Error transforming sub tile coordinates\n");
            }
            if (tl[j][1]>91.0 || tl[j][1]<-91.0 || tl[j][0]<-361 || tl[j][0]>361) {
               fprintf(stderr,"(ERROR) GeoTex_Sample: Invalid transformation\n");
               free(tl);
               free(Tile->Nr);Tile->Nr=NULL;
               Tile->Tl=NULL;
               return;
            }
            if (Band->Spec->Topo) {
               if (tband) {
                  if (tband->Ref->UnProject(tband->Ref,&x,&y,tl[j][1],tl[j][0],0,1)) {
                     ix=x;
                     iy=y;
                     if (tband->Def->Data[0]) {
                        Def_Get(tband->Def,0,FIDX2D(tband->Def,ix,iy),tl[j][2]);
                     } else {
                        if ((ttile=GeoTex_Pick(&tband->Tex,0,&ix,&iy))) {
                           t=iy*ttile->Nx+ix;
                           GeoTex_Val(tband->Tex.Dim,ttile->Data,0,t,tl[j][2]);
                        }
                     }

                  }
               } else {
                  if (Band->Spec->Topo[0]=='I' && handle) {
                     gdb_mapget(handle,tl[j][1],tl[j][0],(void*)&z);tl[j][2]=z;
                  } else {
                     ix=dx;
                     iy=dy;
                     if ((ttile=GeoTex_Pick(&Band->Tex,0,&ix,&iy))) {
                        t=(iy*ttile->Nx+ix)*Band->Def->NC;
                        if (Band->Spec->Topo[0]=='R' && Band->Def->NC>0) {
                           GeoTex_Val(Band->Tex.Dim,ttile->Data,0,t,tl[j][2]);
                        } else if (Band->Spec->Topo[0]=='G' && Band->Def->NC>1) {
                           GeoTex_Val(Band->Tex.Dim,ttile->Data,1,t,tl[j][2]);
                        } else if (Band->Spec->Topo[0]=='B' && Band->Def->NC>2) {
                           GeoTex_Val(Band->Tex.Dim,ttile->Data,2,t,tl[j][2]);
                        } else if (Band->Spec->Topo[0]=='A' && Band->Def->NC>3) {
                           GeoTex_Val(Band->Tex.Dim,ttile->Data,3,t,tl[j][2]);
                        }
                     }
                  }
               }
               tl[j][2]=((isnan(tl[j][2]) || tl[j][2]==Band->Def->NoData)?0.0:tl[j][2]*Band->Spec->TopoFactor);
            }
            if (Tile->Nr)
               Vect_Assign(Tile->Nr[j],GDB_NMap[(int)tl[j][1]+90][(int)tl[j][0]+180]);
         }
         j++;
      }
   }

   /*Generate geolocalized normals*/
   if (Band->Spec->Topo && Tile->Nr) {
      for(iy=0;iy<Tile->Tly;iy++) {
         for(ix=0;ix<Tile->Tlx;ix++) {
            xy=ix*Tile->Tly+iy;
            nr[0]=(tl[ix==Tile->Tlx-1?xy:(xy+Tile->Tly)][2]-tl[ix==0?xy:(xy-Tile->Tly)][2]);
            nr[1]=(tl[iy==Tile->Tly-1?xy:(xy+1)][2]-tl[iy==0?xy:(xy-1)][2]);
            nr[2]=0.0;
            Vect_SMul(nr,nr,0.001);
            Vect_Add(Tile->Nr[xy],Tile->Nr[xy],nr);
            Vect_Normalize(Tile->Nr[xy]);
         }
      }
   }

   Proj->Type->Project(Proj,tl,NULL,-j);

   /*Overwrite limits with sampling coordinates (In case there is height)*/
   Vect_Assign(Tile->Box.Vr[0],tl[0]);
   Vect_Assign(Tile->Box.Vr[1],tl[(Tile->Tlx-1)*Tile->Tly]);
   Vect_Assign(Tile->Box.Vr[2],tl[(Tile->Tlx-1)*Tile->Tly+(Tile->Tly-1)]);
   Vect_Assign(Tile->Box.Vr[3],tl[Tile->Tlx-1]);

   Tile->Tl=tl;
   Tile->Flag|=GEOTEX_COOR;

   if (handle)
      gdb_mapclose(handle);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_Qualify>
 * Creation     : Janvier 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Qualifie les type de donnees necessaire aux texture OpenGL selon les
 *                donnees de la bande.
 *
 * Parametres   :
 *   <Band>     : Bande
 *
 * Retour       :
 *   <...>      : Code de reussite
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GeoTex_Qualify(GDAL_Band *Band) {

  /*Set the kind of image*/
   switch(Band->Def->NC) {
      case 1 : Band->Tex.Type=GL_LUMINANCE; break;
      case 2 :
      case 3 : Band->Tex.Type=GL_RGB; break;
      case 4 : Band->Tex.Type=GL_RGBA; break;
   }

   /*Set the data type of image*/
   switch(Band->Def->Type) {
      case TD_UByte:
         Band->Tex.Dim=GL_UNSIGNED_BYTE;
         Band->Tex.IType=(Band->Tex.Type==GL_LUMINANCE)?GL_LUMINANCE8_EXT:(Band->Tex.Type==GL_RGB)?GL_RGB8_EXT:GL_RGBA8_EXT;
         break;
      case TD_Byte:
         Band->Tex.Dim=GL_BYTE;
         Band->Tex.IType=(Band->Tex.Type==GL_LUMINANCE)?GL_LUMINANCE8_EXT:(Band->Tex.Type==GL_RGB)?GL_RGB8_EXT:GL_RGBA8_EXT;
         break;
      case TD_UInt16:
         Band->Tex.Dim=GL_UNSIGNED_SHORT;
         Band->Tex.IType=(Band->Tex.Type==GL_LUMINANCE)?GL_LUMINANCE16F_ARB:(Band->Tex.Type==GL_RGB)?GL_RGB16F_ARB:GL_RGBA16F_ARB;
         break;
      case TD_Int16:
         Band->Tex.Dim=GL_SHORT;
         Band->Tex.IType=(Band->Tex.Type==GL_LUMINANCE)?GL_LUMINANCE16F_ARB:(Band->Tex.Type==GL_RGB)?GL_RGB16F_ARB:GL_RGBA16F_ARB;
         break;
      case TD_UInt32:
         Band->Tex.Dim=GL_UNSIGNED_INT;
         Band->Tex.IType=(Band->Tex.Type==GL_LUMINANCE)?GL_LUMINANCE32F_ARB:(Band->Tex.Type==GL_RGB)?GL_RGB32F_ARB:GL_RGBA32F_ARB;
         break;
      case TD_Int32:
         Band->Tex.Dim=GL_INT;
         Band->Tex.IType=(Band->Tex.Type==GL_LUMINANCE)?GL_LUMINANCE32F_ARB:(Band->Tex.Type==GL_RGB)?GL_RGB32F_ARB:GL_RGBA32F_ARB;
         break;
      case TD_Float32:
         Band->Tex.Dim=GL_FLOAT;
         Band->Tex.IType=(Band->Tex.Type==GL_LUMINANCE)?GL_LUMINANCE32F_ARB:(Band->Tex.Type==GL_RGB)?GL_RGB32F_ARB:GL_RGBA32F_ARB;
        break;
      case TD_Float64:
         Band->Tex.Dim=GL_DOUBLE;
         Band->Tex.IType=(Band->Tex.Type==GL_LUMINANCE)?GL_LUMINANCE32F_ARB:(Band->Tex.Type==GL_RGB)?GL_RGB32F_ARB:GL_RGBA32F_ARB;
         break;
    }

    if (!GLRender->ShaderAvailable && Band->Tex.IType==GL_LUMINANCE32F_ARB) {
       Band->Tex.IType=GL_LUMINANCE;
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_Get>
 * Creation     : Janvier 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Recuperer les donnees de de la bande pour le tuile.
 *
 * Parametres   :
 *   <Band>     : Bande
 *   <Tile>     : Tuile a texturer
 *
 * Retour       :
 *   <...>      : Code de reussite
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoTex_Get(GDAL_Band *Band,TGeoTexTile *Tile) {

   int    c;

   if (!Tile || Tile->Res==0) {
      return(0);
   }

   /*Allocate tile buffer if not already done*/
   if (!Tile->Data) {
      if (!(Tile->Data=(char*)malloc(Tile->Nx*Tile->Ny*(Band->Tex.Type==GL_LUMINANCE?1:(Band->Tex.Type==GL_RGB?3:4))*TData_Size[Band->Def->Type]))) {
         fprintf(stderr,"(ERROR) GeoTex_Get: Unable to allocate temporaty buffer\n");
         return(0);
      }
   }

   /*Build the image buffer*/
   for(c=0;c<Band->Def->NC;c++) {
      if ((GDALRasterIO(Band->Band[c],GF_Read,Tile->Dx,Tile->Dy,Tile->Rx,Tile->Ry,Tile->Data+c*TData_Size[Band->Def->Type],
         Tile->Nx,Tile->Ny,TD2GDAL[Band->Def->Type],Band->Def->NC>1?Band->Def->NC*TData_Size[Band->Def->Type]:0,0))==CE_Failure) {

         free(Tile->Data);
         Tile->Data=NULL;
         fprintf(stderr,"(ERROR) GeoTex_Get: Unable to read tile data from band %i\n",c);
         return(0);
      }
   }

   Tile->Flag|=GEOTEX_DATA;

   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_New>
 * Creation     : Avril 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Creer la liste des tuiles selon la resolution necessaire.
 *
 * Parametres   :
 *   <Band>     : Band
 *
 * Retour       :
 *   <...>      : Code de reussite
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
TGeoTexTile *GeoTex_New(GDAL_Band *Band,int Resolution,int X0,int Y0) {

   TGeoTexTile *tile;

   if (!(tile=(TGeoTexTile*)calloc(1,sizeof(TGeoTexTile)))) {
      fprintf(stderr,"(ERROR) GeoTex_New: Could not allocate subimage tile\n");
      return(NULL);
   }
   tile->Flag=GEOTEX_NEW;
   tile->Res=Resolution;
   tile->Dx=X0;
   tile->Dy=Y0;
   tile->Tlx=tile->Tly=tile->Tx=tile->Nx=tile->Ny=tile->Rx=tile->Ry=0;
   tile->Sub[0]=tile->Sub[1]=tile->Sub[2]=tile->Sub[3]=NULL;
   tile->Data=NULL;
   tile->Tl=tile->Nr=NULL;

   /*Increment global tile count*/
   GeoTex_TileNb++;

   return(tile);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_Parse>
 * Creation     : Avril 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Parcourir l'arbre des tuiles et recuperer les donnees et les coordonnees.
 *
 * Parametres   :
 *   <Band>       : Bande
 *   <Tile>       : Tuile
 *   <Proj>       : Projection
 *   <VP>         : Viewport
 *   <Resolution> : Resolution a recuperer
 *   <X0>         : X Coin initial
 *   <Y0>         : Y Coin initial
 *
 * Retour       :
 *   <...>      : Code de reussite
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int GeoTex_Parse(GDAL_Band* Band,TGeoTexTile **Tile,Projection *Proj,ViewportItem *VP,int Resolution,int X0,int Y0) {

   VPThreadEvent *event;
   int           r,res,d,x1,y1;

   if (Resolution<=0 || !Band->Tex.Res || !Band->Tex.Proj) {
      return(0);
   }

   if (Band->Tex.Tile && Band->Tex.Tile==*Tile) {

      /*Check for resolution cleanup*/
      if (Band->Tex.Tile->Flag&GEOTEX_CLRDTA) {
         if (GLRender->GLDebug)
            fprintf(stderr,"(DEBUG) GeoTex_Parse: Clearing up to resolution %i\n",Band->Tex.Res);
         GeoTex_Clear(&Band->Tex,GEOTEX_CLRDTA,Band->Tex.Res,GeoTex_TileNbMax);
      }

      /*Check for coordinate cleanup*/
      if (Band->Tex.Tile->Flag&GEOTEX_CLRCOO) {
         if (GLRender->GLDebug)
            fprintf(stderr,"(DEBUG) GeoTex_Parse: Clearing coordinates\n");
         GeoTex_Clear(&Band->Tex,GEOTEX_CLRCOO,0,0);
      }
   }

   if (!(*Tile) && !(*Tile=GeoTex_New(Band,Resolution,X0,Y0))) {
      return(0);
   }

   Proj->Loading+=5;

   if (!(*Tile)->Rx)
      GeoTex_Limit(Band,*Tile,Proj);

   /*Check for visibility*/
   if (GDB_Loc((*Tile)->Box,Proj,1,VP->Width,1,VP->Height)!=GDB_OUT) {

      /*Get tile's data */
      r=0;
      if ((*Tile)->Res>=Band->Tex.Res) {
         if (!(*Tile)->Data) {
            r=1;
            if (GLRender->GLDebug)
               fprintf(stdout,"(DEBUG) GeoTex_Parse: Reading tile (%i) %i - %i\n",Resolution,X0,Y0);
            GeoTex_Get(Band,*Tile);
         }

         /*Calculate coordinate sample mapping*/
         if (!(*Tile)->Tl) {
            r=1;
            GeoTex_Sample(Band,(*Tile),Proj);
         }

         /*Redraw if needed*/
         if (Band->Tex.ThreadId && r) {
            event=(VPThreadEvent*)ckalloc(sizeof(VPThreadEvent));
            event->event.proc=ViewportRefresh_ThreadEventProc;
            event->ptr=(void*)VP;

            Tcl_ThreadQueueEvent(VP->ThreadId,(Tcl_Event*)event,TCL_QUEUE_TAIL);
            Tcl_ThreadAlert(VP->ThreadId);
         }

         /*Process subtile*/
         if ((res=Resolution>>1)) {
            d=res*Band->Spec->TexSize;
            x1=(X0+d)>=Band->Ref->X1?X0:X0+d;
            y1=(Y0+d)>=Band->Ref->Y1?Y0:Y0+d;

            if (!GeoTex_Parse(Band,&(*Tile)->Sub[0],Proj,VP,res,X0,Y0)) return(0);
            if (x1==X0) {
               (*Tile)->Sub[1]=(TGeoTexTile*)0x1;
            } else {
               if (!GeoTex_Parse(Band,&(*Tile)->Sub[1],Proj,VP,res,x1,Y0)) return(0);
            }
            if (y1==Y0) {
               (*Tile)->Sub[3]=(TGeoTexTile*)0x1;
            } else {
               if (!GeoTex_Parse(Band,&(*Tile)->Sub[3],Proj,VP,res,X0,y1)) return(0);
            }
            if (x1==X0 || y1==Y0) {
               (*Tile)->Sub[2]=(TGeoTexTile*)0x1;
            } else {
               if (!GeoTex_Parse(Band,&(*Tile)->Sub[2],Proj,VP,res,x1,y1)) return(0);
            }
         }
      }
   }

   Proj->Loading=0;
   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_Resolution>
 * Creation     : Avril 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Calculer la resolution necessaire selon les parametres de projections et camera.
 *
 * Parametres   :
 *   <Band>     : Bande
 *   <Proj>     : Projection
 *
 * Retour       :
 *   <...>      : Code de reussite
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GeoTex_Signal(TGeoTex *Tex,int Flags) {

   Tcl_MutexLock(&MUTEX_GEOTEX);
   if (Tex->Tile) Tex->Tile->Flag|=Flags;
   Tcl_MutexUnlock(&MUTEX_GEOTEX);
}

int GeoTex_Resolution(GDAL_Band *Band,Projection *Proj) {

   Coord        co[2];
   unsigned int res,d;
   double       l;

   if (Proj->VP->Secondary) {
      return(0);
   }

   /*Figure out geographical resolution*/
   if (Proj->Geographic) {
      Band->Ref->Project(Band->Ref,Band->Def->NI/2,Band->Def->NJ/2,&co[0].Lat,&co[0].Lon,1,1);
      Band->Ref->Project(Band->Ref,Band->Def->NI/2+1,Band->Def->NJ/2,&co[1].Lat,&co[1].Lon,1,1);
      res=Proj->PixDist/DIST(0.0,DEG2RAD(co[0].Lat),DEG2RAD(co[0].Lon),DEG2RAD(co[1].Lat),DEG2RAD(co[1].Lon));
   } else {
      res=Proj->PixDist*MAX((float)Band->Def->NI/Proj->VP->Width,(float)Band->Def->NJ/Proj->VP->Height);
   }

   d=MAX(Band->Def->NI,Band->Def->NJ);
   l=LOG2(d);

   /*Check for factor 2 number*/
   if (System_BitCount(d)==1) {
      Band->Tex.ResN=pow(2,floor(l))/Band->Spec->TexSize;
   } else {
      Band->Tex.ResN=pow(2,ceil(l))/Band->Spec->TexSize;
   }
   /*People like Serge like to do images very small where ResN=0*/
   Band->Tex.ResN=(Band->Tex.ResN<=0?1:Band->Tex.ResN);

   res=pow(2,floor(LOG2(res)))*Band->Spec->TexRes;
   res=res<1?1:(res>Band->Tex.ResN?Band->Tex.ResN:res);

   /*Set flag for cleanup of higher res than needed*/
   if (res>Band->Tex.Res) {
      GeoTex_Signal(&Band->Tex,GEOTEX_CLRDTA);
   }
   Band->Tex.Res=res;

   /*Clear sampling coordinates if projection not the same*/
   if (Band->Tex.Proj!=Proj) {
      GeoTex_Signal(&Band->Tex,GEOTEX_CLRCOO);
   }
   Band->Tex.Proj=Proj;

   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_Render>
 * Creation     : Avril 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Rendu de la texture geographiquer.
 *
 * Parametres  :
 *   <Band>     : Bande a afficher
 *   <Tile>     : Tuile de la GeoTexture
 *   <Proj>     : La projection courante
 *   <VP>       : Le viewport ou le rendu doit etre fait
 *
 * Retour       :
 *   <Done>     : Rendue effectue ou non
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/

int GeoTex_Render(GDAL_Band *Band,TGeoTexTile *Tile,Projection *Proj,ViewportItem *VP,int Lock) {

   int    x,y,nx,ny,tl[4]={ 1,1,1,1 },lt;
   double dx,dy;

   /*Check availability*/
   if (!Band->Tex.Res)
      return(1);

   if (Lock) Tcl_MutexLock(&MUTEX_GEOTEX);

   if (!Tile || Tile->Res<Band->Tex.Res) {
      if (Lock) Tcl_MutexUnlock(&MUTEX_GEOTEX);
      return(0);
   }

   /*Check visibility*/
   if ((Tile->Res==Band->Tex.ResN && Band->Ref->Type&GRID_WRAP) || GDB_Loc(Tile->Box,Proj,1,Proj->VP->Width,1,Proj->VP->Height)!=GDB_OUT) {

      tl[0]=Tile->Sub[0]==(TGeoTexTile*)0x1?1:GeoTex_Render(Band,Tile->Sub[0],Proj,VP,0);
      tl[1]=Tile->Sub[1]==(TGeoTexTile*)0x1?(tl[0]?1:0):GeoTex_Render(Band,Tile->Sub[1],Proj,VP,0);
      tl[2]=Tile->Sub[2]==(TGeoTexTile*)0x1?(tl[0]?1:0):GeoTex_Render(Band,Tile->Sub[2],Proj,VP,0);
      tl[3]=Tile->Sub[3]==(TGeoTexTile*)0x1?(tl[0]?1:0):GeoTex_Render(Band,Tile->Sub[3],Proj,VP,0);

      /*Check for sub tile info availability*/
      if (!tl[0] || !tl[1] || !tl[2] || !tl[3]) {

         /*Setup 2D Data Texture*/
         if (GLRender->Resolution<=2) {
            if (Tile->Flag&GEOTEX_DATA && Tile->Flag&GEOTEX_COOR) {
               if (!Tile->Tx)
                  GeoTex_Texture(Band,Tile);
            }
         }
         if (!Tile->Tx) {
            if (Lock) Tcl_MutexUnlock(&MUTEX_GEOTEX);
            return(0);
         }

         glBindTexture(GL_TEXTURE_2D,Tile->Tx);
         glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,Band->Spec->Interp);
         glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,Band->Spec->Interp);
         glNormal3d(0.0,0.0,0.0);

         /*Light only if topography enabled or Sun is active*/
         lt=(Tile->Nr && (Band->Spec->Topo || Proj->Sun));
         dx=1.0/(Tile->Tlx-1);
         dy=1.0/(Tile->Tly-1);
         nx=(Tile->Tlx+1)>>1;
         ny=(Tile->Tly+1)>>1;

         if (!tl[0]) {
            for(x=0;x<nx-1;x++) {
               glBegin(GL_QUAD_STRIP);
               for(y=0;y<ny;y++) {
                  glTexCoord2f((x+1)*dx,y*dy);
                  if (lt) glNormal3dv(Tile->Nr[(x+1)*Tile->Tly+y]);
                  glVertex3dv(Tile->Tl[(x+1)*Tile->Tly+y]);
                  glTexCoord2f(x*dx,y*dy);
                  if (lt) glNormal3dv(Tile->Nr[x*Tile->Tly+y]);
                  glVertex3dv(Tile->Tl[x*Tile->Tly+y]);
               }
               glEnd();
            }
         }
         if (!tl[1]) {
            for(x=nx-1;x<Tile->Tlx-1;x++) {
               glBegin(GL_QUAD_STRIP);
               for(y=0;y<ny;y++) {
                  glTexCoord2f((x+1)*dx,y*dy);
                  if (lt) glNormal3dv(Tile->Nr[(x+1)*Tile->Tly+y]);
                  glVertex3dv(Tile->Tl[(x+1)*Tile->Tly+y]);
                  glTexCoord2f(x*dx,y*dy);
                  if (lt) glNormal3dv(Tile->Nr[x*Tile->Tly+y]);
                  glVertex3dv(Tile->Tl[x*Tile->Tly+y]);
               }
               glEnd();
            }
         }
         if (!tl[2]) {
            for(x=nx-1;x<Tile->Tlx-1;x++) {
               glBegin(GL_QUAD_STRIP);
               for(y=ny-1;y<Tile->Tly;y++) {
                  glTexCoord2f((x+1)*dx,y*dy);
                  if (lt) glNormal3dv(Tile->Nr[(x+1)*Tile->Tly+y]);
                  glVertex3dv(Tile->Tl[(x+1)*Tile->Tly+y]);
                  glTexCoord2f(x*dx,y*dy);
                  if (lt) glNormal3dv(Tile->Nr[x*Tile->Tly+y]);
                  glVertex3dv(Tile->Tl[x*Tile->Tly+y]);
               }
               glEnd();
            }
         }
         if (!tl[3]) {
            for(x=0;x<nx-1;x++) {
               glBegin(GL_QUAD_STRIP);
               for(y=ny-1;y<Tile->Tly;y++) {
                  glTexCoord2f((x+1)*dx,y*dy);
                  if (lt) glNormal3dv(Tile->Nr[(x+1)*Tile->Tly+y]);
                  glVertex3dv(Tile->Tl[(x+1)*Tile->Tly+y]);
                  glTexCoord2f(x*dx,y*dy);
                  if (lt) glNormal3dv(Tile->Nr[x*Tile->Tly+y]);
                  glVertex3dv(Tile->Tl[x*Tile->Tly+y]);
               }
               glEnd();
            }
         }
      }
   }

   if (Lock) Tcl_MutexUnlock(&MUTEX_GEOTEX);
   return(1);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_Pick>
 * Creation     : Avril 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Recuperer la tuile ou le point X,Y est inclus.
 *
 * Parametres    :
 *   <Interp>   : Interpreteur Tcl
 *   <Tex>      : GeoTexture
 *   <Res>      : Target resolution
 *   <X>        : Coordonnee X
 *   <Y>        : Coordonnee Y
 *
 * Retour       :
 *  <Tile>      : Tuile pointee
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
TGeoTexTile *GeoTex_Pick(TGeoTex *Tex,int Res,int *X,int *Y) {

   TGeoTexTile *next=NULL,*best=NULL,*tile=NULL;

  /*Parse the tree to find best resolution tile*/
   if ((tile=next=Tex->Tile)) {

      if (!tile || !tile->Flag&GEOTEX_DATA || (*X<0 || *X>=Tex->Nx || *Y<0 || *Y>=Tex->Ny)) {
         next=tile=best=NULL;
      }

      while(next) {
         /*If lower thant current resolution, finish*/
         if (next->Res<=Res) {
            break;
         }

         /*Keep last best resolution available*/
         if (next->Flag&GEOTEX_DATA) {
            best=next;
         }

         /*Check for the next best subtile*/
         tile=next->Sub[0];
         if (tile>(TGeoTexTile*)0x1 && *X>=tile->Dx && *X<(tile->Dx+tile->Nx*tile->Res) && *Y>=tile->Dy && *Y<(tile->Dy+tile->Ny*tile->Res)) {
            next=tile;
         } else {
            tile=next->Sub[1];
            if (tile>(TGeoTexTile*)0x1 && *X>=tile->Dx && *X<(tile->Dx+tile->Nx*tile->Res) && *Y>=tile->Dy && *Y<(tile->Dy+tile->Ny*tile->Res)) {
               next=tile;
            } else {
               tile=next->Sub[2];
               if (tile>(TGeoTexTile*)0x1 && *X>=tile->Dx && *X<(tile->Dx+tile->Nx*tile->Res) && *Y>=tile->Dy && *Y<(tile->Dy+tile->Ny*tile->Res)) {
                  next=tile;
               } else {
                  tile=next->Sub[3];
                  if (tile>(TGeoTexTile*)0x1 && *X>=tile->Dx && *X<(tile->Dx+tile->Nx*tile->Res) && *Y>=tile->Dy && *Y<(tile->Dy+tile->Ny*tile->Res)) {
                     next=tile;
                  } else {
                     break;
                  }
               }
            }
         }
      }

      /*If the tile is empty, use the best previous resolution*/
      if (next && !(next->Flag&GEOTEX_DATA)) {
            next=best;
      }

      /*Calculate local tile coordinates*/
      if (!next) {
         *X=*Y=-1;
      } else {
         *X=(*X-next->Dx)/next->Res;
         *Y=(*Y-next->Dy)/next->Res;
         if (*X>=next->Nx)
            *X=next->Nx-1;
         if (*Y>=next->Ny)
            *Y=next->Ny-1;
      }
   }

   return(next);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_AppendValueObj>
 * Creation     : Janvier 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Recuperer les valeurs de la geotexture pou Tcl.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Tex>      : GeoTexture
 *   <X>        : Coordonnee X
 *   <Y>        : Coordonnee Y
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
Tcl_Obj* GeoTex_AppendValueObj(Tcl_Interp *Interp,TGeoTex *Tex,int X,int Y) {

   int          c,t,nc,x,y;
   double       val;
   TGeoTexTile *tile=NULL;
   Tcl_Obj     *obj;

   obj=Tcl_NewListObj(0,NULL);

   x=X;
   y=Y;

   Tcl_MutexLock(&MUTEX_GEOTEX);
   tile=GeoTex_Pick(Tex,Tex->Res,&x,&y);
   nc=Tex->Type==GL_LUMINANCE?1:Tex->Type==GL_RGB?3:4;

   if (tile && tile->Ny) {
      t=(y*tile->Nx+x)*nc;

      for(c=0;c<nc;c++) {
         GeoTex_Val(Tex->Dim,tile->Data,c,t,val);
         if (Tex->Dim<GL_FLOAT) {
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(val));
         } else {
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(val));
         }
      }
   } else {
      for(c=0;c<nc;c++) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
      }
   }
   Tcl_MutexUnlock(&MUTEX_GEOTEX);
   return(obj);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GeoTex_ValueGet>
 * Creation     : Janvier 2007 J.P. Gauthier - CMC/CMOE
 *
 * But          : Recuperer les valeurs de la geotexture pou Tcl.
 *
 * Parametres   :
 *   <Def>      : Definition de la donnnee
 *   <Tex>      : GeoTexture
 *   <C>        : Composante
 *   <X>        : Coordonnee X
 *   <Y>        : Coordonnee Y
 *   <Z>        : Coordonnee Z
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
double GeoTex_ValueGet(TDataDef *Def,TGeoTex *Tex,int Res,int C,double X,double Y,double Z) {

   int          t,x,y,rx,ry,px,py;
   double       v0,v1,cube[2][4]={{0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0}};
   TGeoTexTile *tile=NULL,*tilen;

   /*Check for limits*/
   if (X>Def->NI-1 || Y>Def->NJ-1 || Z>Def->NK-1 || X<0 || Y<0 || Z<0) {
      return(Def->NoData);
   }

   x=px=floor(X);
   y=py=floor(Y);
   tile=GeoTex_Pick(Tex,Res,&x,&y);

   /*Check for data availability, might not be loaded yet*/
   if (tile && tile->Ny) {
      t=(y*tile->Nx+x)*Def->NC;
      GeoTex_Val(Tex->Dim,tile->Data,C,t,cube[0][0]);
      if (px==X && py==Y) {
         /*If we are right on the pixel*/
         return(cube[0][0]);
      } else {

         /*Otherwise, interpolate*/
         if (x==tile->Nx-1) {
            rx=px+1; ry=py;
            tilen=GeoTex_Pick(Tex,Res,&rx,&ry);
            if (tilen && tilen->Ny) {
               t=(ry*tilen->Nx+rx)*Def->NC;
               GeoTex_Val(Tex->Dim,tilen->Data,C,t,cube[0][1]);
            }
         } else {
            t=(y*tile->Nx+(x+1))*Def->NC;
            GeoTex_Val(Tex->Dim,tile->Data,C,t,cube[0][1]);
         }

         if (x==tile->Nx-1 || y==tile->Ny-1) {
            rx=px+1; ry=py+1;
            tilen=GeoTex_Pick(Tex,Res,&rx,&ry);
            if (tilen && tilen->Ny) {
               t=(ry*tilen->Nx+rx)*Def->NC;
               GeoTex_Val(Tex->Dim,tilen->Data,C,t,cube[0][2]);
            }
         } else {
            t=((y+1)*tile->Nx+(x+1))*Def->NC;
            GeoTex_Val(Tex->Dim,tile->Data,C,t,cube[0][2]);
         }

         if (y==tile->Ny-1) {
            rx=px; ry=py+1;
            tilen=GeoTex_Pick(Tex,Res,&rx,&ry);
            if (tilen && tilen->Ny) {
               t=(ry*tilen->Nx+rx)*Def->NC;
               GeoTex_Val(Tex->Dim,tilen->Data,C,t,cube[0][3]);
            }
         } else {
            t=((y+1)*tile->Nx+x)*Def->NC;
            GeoTex_Val(Tex->Dim,tile->Data,C,t,cube[0][3]);
         }

         /*Interpolate over X and Y*/
         X-=px;
         Y-=py;
         v0=ILIN(cube[0][0],cube[0][1],X);
         v1=ILIN(cube[0][3],cube[0][2],X);
         return(ILIN(v0,v1,Y));
      }
   }
   return(Def->NoData);
}
