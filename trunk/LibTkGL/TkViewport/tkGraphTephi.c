/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Creation de graph dans le canvas Tk.
 * Fichier      : tkGraphTephi.c
 * Creation     : Juillet 2007 - J.P. Gauthier - CMC/CMOE
 *
 * Description  : Fichier d'entete du module de graph de type tephigramme.
 *
 * Remarques    : Ce code est derivee du code d'XTEPHI
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
 *
 *=========================================================
 */

#include "tkGraphAxis.h"

extern void Data_RenderBarbule(int Type,int Flip,float Axis,float Lat,float Lon,float Elev,float Speed,float Dir,float Size,void *Proj);

//   thermo_be():   calculates bouyant energy in a layer
double Thermo_Bouyant(double TP_bot,double TP_top,double TE_bot,double TE_top,double P_bot,double P_top) {

   double tp_ave,te_ave;

   tp_ave = 0.5*(TP_bot+TP_top);
   te_ave = 0.5*(TE_bot+TE_top);

   return(R_d*(tp_ave-te_ave)*log(P_bot/P_top));
}

//  Thermo_LatentHeatVapor():  calculates latent heat of vaporization
//         t(deg C)
double Thermo_LatentHeatVapor(double T) {
   double  c=-2.274e3;

   return (L_v+c*T);
}

//   Thermo_VaporPressure():   calculates saturation vapor pressure (over water)
//         t(deg C), vp(mb)
double Thermo_VaporPressure(double T) {

   if (T<T_MIN || T>T_MAX)
      return (MSNGV);

   if (T<t_cut)
      return (6.11*exp(L_s/R_v*(1.0/AZ-1.0/(T+AZ))));

   return (6.11*exp(Thermo_LatentHeatVapor(T)/R_v*(1.0/AZ-1.0/(T+AZ))));
}

//   Thermo_StandardHeight(): calculates height for NACA standard atmosphere
double Thermo_StandardHeight(double P) {

   if (P>p_trop)
      return(c_trop*(1.0-pow(P/1013.25,0.19026)));
   else {
      return(h_trop-c_strat*log(P/p_trop));
    }
}

//  thermo_pt2sh():   calculates specific humidity
//        p(mb), td(deg C), sh(g/kg)
double Thermo_PT2SH(double P,double T) {

   double  e_s;

//   if (P<=0.0 || P>P_MAX || T<T_MIN || T>T_MAX) {
//      return(MSNGV);
//   }
   e_s=Thermo_VaporPressure(T);
   return(1000.0*R_d/R_v*e_s/(P-(1.0-R_d/R_v)*e_s));
}

//   thermo_vt():   calculates virtual temperature
//         p(mb), t(deg C), td(deg C), t*(deg C)
double Thermo_VirtualTemp(double P,double T,double TD) {

   double sh;

//   if (P<=0.0 || P>P_MAX || T<T_MIN || T>T_MAX || TD<T_MIN || TD>T_MAX)
//      return(MSNGV);

   sh = Thermo_PT2SH(P,TD);
   return((1.0+0.001*(R_v/R_d-1.0)*sh)*(T+AZ)-AZ);
}


//   Thermo_PR2T():    calculates t from mixing ratio and pressure
//       p(mb), r(g/kg), t(deg C)
double Thermo_PR2T(double p,double r) {
   double  e_s, t, t1;
   double  l_v, tmp;
   double  c0 = R_d / R_v;
   double  c1 = 1.809927;      /* log (6.11) */

//   if (p <= 0.0 || p > P_MAX || r <= 0.0 || r >= 1000.0)
//      return(MSNGV);

   r*=1.0e-3;
   e_s=p * r / (c0 + r);
   tmp=log(e_s) - c1;
   t = 1.0 / (1.0 / AZ - R_v/L_v * tmp) - AZ;
   l_v = Thermo_LatentHeatVapor(t);
   t = 1.0 / (1.0 / AZ - R_v/l_v * tmp) - AZ;
   if (t<t_cut) {
      t1=1.0/(1.0/AZ-R_v/L_s*tmp)-AZ;
      if (t1<t_cut)
         return(t1);
      else
         return(t_cut);
   } else {
      return(t);
   }
}

//   Thermo_PT2TH():   calculates theta from p and t
//         p(mb), t(deg C), theta(deg K)
double Thermo_PT2TH(TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,double P,double T) {

   double k=R_d/cp_d;

//   if (P<=P_MIN || P>P_MAX || T<=T_MIN || T>T_MAX)
//      return(MSNGV);

   return (pow(1000.0/P,k)*(T + AZ));
}

//   Thermo_PTH2T():   calculates t from p and theta
//         p(mb), theta(deg K), t(deg C)
double Thermo_PTH2T(TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,double P, double TH) {
   double k=R_d/cp_d;

//   if (P<=P_MIN || P>P_MAX || TH<=TH_MIN || TH>TH_MAX)
//      return(MSNGV);

   return (pow(P/1000.0,k)*TH-AZ);
}

//   Thermo_PT2R(): calculates mixing ratio
//         p(mb), t(deg C), r(g/kg)
double Thermo_PT2R(double P,double T) {
   double e_s;

//   if (P<=0.0 || P>P_MAX || T<T_MIN || T>T_MAX)
//      return(MSNGV);

   e_s = Thermo_VaporPressure(T);
   return (1000.0*R_d/R_v*e_s/(P-e_s));
}

//   Thermo_RH2TD():   converts relative humidity to dew point
//        p(mb), t(deg C), rh(%), td(deg C)
double Thermo_RH2TD(double P,double T,double RH) {

   double ratio;

//   if (P <= 0.0 || P > P_MAX || T < T_MIN || T > T_MAX || RH <= 0.0 || RH > 100.0)
//      return(MSNGV);
   ratio=Thermo_PT2R(P,T)*0.01*RH;
   return(Thermo_PT2R(P,ratio));
}

//   Thermo_TD2RH():   converts dew point to relative humidity
//         p(mb), t(deg C), td(deg C), rh(%)
double Thermo_TD2RH(double P,double T,double TD) {

//   if (P<=0.0 || P>P_MAX || T<T_MIN || T>T_MAX || TD<T_MIN || TD>T_MAX)
//      return(MSNGV);
   if (TD>T)
      TD=T;
   return(100.0*Thermo_PT2R(P,TD)/Thermo_PT2R(P,T));
}

//  Thermo_TD2WB():   calculates wet bulb
//         p(mb), t(deg C), td(deg C), tw(deg C)

float Thermo_TD2WB(double P,double T,double TD) {

   int    k;
   double  tw, l_v, r_w, r_d;
   double  fun, fun1;
   double  eps = 0.01;

//   if (P<=0.0 || P>P_MAX || T<T_MIN || T>T_MAX || TD<T_MIN || TD > T_MAX)
//      return(MSNGV);

   if (TD>T)
      TD=T;

   r_d = 0.001 * Thermo_PT2R(P,TD);
   tw = T;
   l_v = Thermo_LatentHeatVapor(tw);
   r_w = 0.001 * Thermo_PT2R(P,tw);
   fun = (r_w - r_d) * l_v / (cp_d + r_w * cv_d);
   fun1 = 1.0 + l_v * l_v * r_w / (cp_d * R_v * (tw + AZ) * (tw + AZ));
   k = 0;
   while (fabs(fun) > eps && k < 10) {
      tw -= fun / fun1;
      l_v = Thermo_LatentHeatVapor(tw);
      r_w = 0.001 * Thermo_PT2R(P,tw);
      fun = tw - T + (r_w - r_d) * l_v / (cp_d + r_w * cv_d);
      fun1 = 1.0 + l_v * l_v * r_w / (cp_d * R_v * (tw + AZ) * (tw + AZ));
      k++;
   }
   return (tw);
}

//  Thermo_WB2TD():   calculates dew point from wet bulb temperature
//         p(mb), t(deg C), tw(deg C), td(deg C)
double Thermo_WB2TD(double P,double T,double TW) {

   double l_v,r_w,r_d;

//   if (P<=0.0 || P>P_MAX || T<T_MIN || T>T_MAX || TW<T_MIN || TW>T_MAX)
//      return(MSNGV);

   if (TW>T)
      TW=T;

   r_w = Thermo_PT2R(P,TW);
   l_v = Thermo_LatentHeatVapor(TW);
   if ((r_d = r_w - (1000.0 * cp_d + r_w * cv_d) * (T-TW) / l_v) <= 0.0)
      return (T_MIN);
   return(Thermo_PR2T(P,r_d));
}

//   Thermo_WB2TF():   calculates frost point from wet bulb temperature
//        p(mb), t(deg C), tw(deg C), tf(deg C)
double Thermo_WB2TF(double P,double T,double TW) {

   double r_w,r_f;

//   if (P<=0.0 || P>P_MAX || T<T_MIN || T>T_MAX || TW<T_MIN || TW>T_MAX)
//       return(MSNGV);

   if (TW>T)
      TW=T;
   r_w = Thermo_PT2R(P,TW);
   if ((r_f = r_w - (1000.0 * cp_d + r_w * cv_d) * (T - TW) / L_s) <= 0.0)
      return (T_MIN);
   return (Thermo_PR2T(P,r_f));
}

//***   Thermo_TTH2P():   calculates p from t and log(theta)
//***         t(deg C), theta(deg K), p(mb)
double Thermo_TTH2P(double T,double TH) {
   double k = cp_d / R_d;

//   if (t <= T_MIN || t > T_MAX)
//      return(MSNGV);
   return(1000.0*pow((T+(double)AZ)/TH,k));
}

//   Thermo_THP():   calculates value of dlnth/dp
//         p(mb), t(deg C), ef (1/1mb)
double Thermo_THP(double P,double Tp,double Te,double Tde,double Ef) {

   double  l_v,r,r2,w,we,entr;
   double  eps=R_d/R_v;

   l_v = Thermo_LatentHeatVapor(Tp);
   r = Thermo_VaporPressure(Tp)/P;
   r2=(1.0-r)*(1.0-r);
   if (Ef==0.0) {
      Tp+=AZ;
      return(1.0-eps*l_v/(cp_d*Tp))/(cp_d*P*Tp*r2/(eps*l_v*r)+eps*P*l_v/(R_d*Tp));
   } else {
      w=Thermo_PT2R(P,Tp);
      we=Thermo_PT2R(P,Tde);
      entr=P*r2/(eps*l_v*r)*(0.001*(w-we)*l_v+cp_d*(Tp-Te))*Ef;
      Tp+=AZ;
      return(1.0-eps*l_v/(cp_d*Tp)+1.0e-3*entr)/(cp_d*P*Tp*r2/(eps*l_v*r)+eps*P*l_v/(R_d*Tp));
   }
}

//   Thermo_eTHP(): does one step of modified Euler method for
//         wet adiabat equation
//         theta(dek K), p(mb), t(deg C)
//         passed values at layer p, returned at p+dp
void Thermo_eTHP(TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,double *pln_th,double *pp,double *pt_p,double t_e,double td_e,double dp,double entrainment) {

   double  y,p,t,th,ln_th;

   p=*pp;
   ln_th=*pln_th=log(*pln_th);
   t=*pt_p;

   y=Thermo_THP(p,t,t_e,td_e,entrainment);
   p+=dp/2.0;
   ln_th+=y*dp/2.0;
   th=exp(ln_th);
   t=Thermo_PTH2T(AxisTH,AxisT,AxisP,p,th);
   y=Thermo_THP(p,t,t_e,td_e,entrainment);
   *pln_th += y*dp;
   *pp += dp;
   *pln_th = exp(*pln_th);
   *pt_p = Thermo_PTH2T(AxisTH,AxisT,AxisP,*pp,*pln_th);
}

//***   GraphTephi_TTH2XY(): converts t-theta coordinates to window's x, y
void GraphTephi_TTH2XY(TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,double T,double TH,double *X,double *Y) {

   T=AXISVALUE(AxisT,T);
   TH=AXISVALUE(AxisTH,TH);

   *X=(T*COSA + TH*SINA)-AxisT->Delta*60;
   *Y=-(T*SINA - TH*COSA)-AxisT->Delta*30;
}

//***   GraphTephi_TP2XY():   converts t-p coordinates to window's x, y
int GraphTephi_TP2XY(TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,double T,double P,double *X,double *Y) {

   double th;

   if (P>=AxisP->T1 && P<=AxisP->T0) {
      th=Thermo_PT2TH(AxisTH,AxisT,AxisP,P,T);
      GraphTephi_TTH2XY(AxisTH,AxisT,AxisP,T,th,X,Y);
      return(1);
   } else {
      return(0);
   }
}

//***   GraphTephi_XY2PT():   converts x-y coordinates to t-p
void GraphTephi_XY2PT(TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,double X,double Y,double *P,double *T) {
   double th;

   X+=(AxisT->DT0*COSA+AxisTH->DT0*SINA)*0.75;
   Y+=(AxisT->DT0*SINA-AxisTH->DT0*COSA)*0.5;

   *T = X*COSA - Y*SINA;
   th = X*SINA + Y*COSA;

   *T = AXISPPOS(AxisT,*T);
   th = AXISPPOS(AxisTH,th);
   *P = Thermo_TTH2P(*T,th);
}

//***   GraphTephi_THP2XY(): converts theta-p coordinates to window's x, y
void GraphTephi_THP2XY(TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,double Theta,double P,double *X,double *Y) {
   double t;

   t = Thermo_PTH2T(AxisTH,AxisT,AxisP,P,Theta);
   GraphTephi_TTH2XY(AxisTH,AxisT,AxisP,t,Theta,X,Y);
}

double GraphTephi_PX2T(TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,double P,int X,int *Y0,int *Y1) {

   int    y,yt,yb;
   double p0,p1,t;

   yb=*Y0;
   yt=*Y1;
   y =(yb+yt)/2;
   GraphTephi_XY2PT(AxisTH,AxisT,AxisP,X,*Y0,&p0,&t);
   GraphTephi_XY2PT(AxisTH,AxisT,AxisP,X,*Y1,&p1,&t);

   do {
      GraphTephi_XY2PT(AxisTH,AxisT,AxisP,X,y,&p1,&t);
      if (p1<P) {
         yt=y;
         y =(yb+yt)/2;
       } else if (p1>P) {
         yb=y;
         y =(yb+yt)/2;
       } else {
         *Y0=yb;
         return(t);
       }
   } while ((yt<yb)&&(y!=yt));
   *Y0=yb;

   return(t);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphTehpi_DisplayIsotherm>
 * Creation     : Juillet 2008 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher un Isotherme du tephigramme
 *
 * Parametres   :
 *   <Graph>    : Item graph
 *   <AxisTH>   : Axe adiabat
 *   <AxisT>    : Axe temperature
 *   <AxisP>    : Axe pression
 *   <GLMode>   : Mode de rendue
 *   <T>        : Isotherme
 *   <Label>    : Libelle a afficher (Si NULL, pas de libelle)
 *   <Width>    : Largeur de la libelle
 *   <Height>   : Hauteur de lal libelle
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void  GraphTehpi_DisplayIsotherm(GraphItem *Graph,TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,GLuint GLMode,double T,Tk_TextLayout Label,int Width,int Height) {

   int     i;
   double  x0,x1,y0,y1;
   double  pt,pb;
   static  double range[20][3]= {{-100,-30,10.0},{-30,-20,10.0},{-20,-15,12.0},{ -15, -10, 17.6},{ -10, -5, 24.3},{ -5, 0, 33.5},
      { 0, 5, 45.2},{ 5, 10, 61},{ 10, 15, 81.5},{ 15, 20, 107.3},{ 20, 25, 140.3},{ 25, 30, 182},{ 30, 35, 233},{ 35, 40, 298},
      { 40, 45, 376},{ 45, 50, 472},{ 50, 55, 585.8},{ 55, 60, 722.2},{ 60, 65, 885},{ 65, 100, 1000}};

   pb=T<-90?200:T<-80?300:T<-60?500:AxisP->T0;
   pt=AxisP->T1;
   for (i=0;i<20;i++) {
      if ((T>range[i][0])&&(T<=range[i][1])) {
         pt=range[i][2];
         break;
      }
   }

   pt=pt<AxisP->T1?AxisP->T1:pt;
   GraphTephi_TP2XY(AxisTH,AxisT,AxisP,T,pb,&x0,&y0);
   GraphTephi_TP2XY(AxisTH,AxisT,AxisP,T,pt,&x1,&y1);

   glBegin(GL_LINES);
      glVertex2d(x0,y0);
      glVertex2d(x1,y1);
   glEnd();

   if (Label) {
      glDisplayTextLayout(Label,(int)-AxisT->Angle+1000,(int)(x0),(int)(y0),0,-1);
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphTehpi_DisplayIsotherms>
 * Creation     : Juillet 2008 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher les Isothermes du tephigramme
 *
 * Parametres   :
 *   <Graph>    : Item graph
 *   <AxisTH>   : Axe adiabat
 *   <AxisT>    : Axe temperature
 *   <AxisP>    : Axe pression
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *   <GLMode>   : Mode de rendue
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphTehpi_DisplayIsotherms(GraphItem *Graph,TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,int X0,int Y0,int X1,int Y1,GLuint GLMode) {

   Tk_TextLayout text;
   Tk_Font       font;
   XColor       *color;
   char          buf[32];
   int           width,height,dx,dy;
   int           t;

   font=AxisT->Font?AxisT->Font:Graph->Font;
   color=AxisT->Color?AxisT->Color:Graph->FGColor;

   if (!font || !color) {
      return;
   }

   glFontUse(Tk_Display(Tk_CanvasTkwin(Graph->canvas)),font);
   if (AxisT->GridWidth) {
      if (AxisT->GridColor)
         glColor3us(AxisT->GridColor->red,AxisT->GridColor->green,AxisT->GridColor->blue);
      glDash(&AxisT->Dash);
      glLineWidth(AxisT->GridWidth);
   }

   if (AxisT->InterNb) {
      for(t=0;t<AxisT->InterNb;t++) {
         if (AxisT->Label) {
            text=Tk_ComputeTextLayout(font,AxisT->Label[t],Tcl_NumUtfChars(AxisT->Label[t],strlen(AxisT->Label[t])),0,TK_JUSTIFY_CENTER,0,&width,&height);
         } else {
            GraphAxis_Print(AxisT,buf,AxisT->Inter[t],0);
            text=Tk_ComputeTextLayout(font,buf,strlen(buf),0,TK_JUSTIFY_CENTER,0,&width,&height);
         }
         GraphTehpi_DisplayIsotherm(Graph,AxisTH,AxisT,AxisP,GLMode,AxisT->Inter[t],text,width,height);
         Tk_FreeTextLayout(text);
     }
   } else {
      t=(int)AxisT->T0;
      while (t<AxisT->T1) {

         GraphAxis_Print(AxisT,buf,(double)t,0);
         text=Tk_ComputeTextLayout(font,buf,strlen(buf),0,TK_JUSTIFY_CENTER,0,&width,&height);
         GraphTehpi_DisplayIsotherm(Graph,AxisTH,AxisT,AxisP,GLMode,(double)t,text,width,height);
         Tk_FreeTextLayout(text);

         t+=AxisT->Incr;
      }
   }

   if (AxisT->HighLightNb) {
      if (AxisT->HighLightWidth)
         glLineWidth(AxisT->HighLightWidth);
      if (AxisT->HighLightColor)
         glColor3us(AxisT->HighLightColor->red,AxisT->HighLightColor->green,AxisT->HighLightColor->blue);

      for(t=0;t<AxisT->HighLightNb;t++) {
         GraphTehpi_DisplayIsotherm(Graph,AxisTH,AxisT,AxisP,GLMode,AxisT->HighLight[t],NULL,0,0);
     }
   }
   glDisable(GL_LINE_STIPPLE);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphTehpi_DisplayIsobar>
 * Creation     : Juillet 2008 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher un Isobar du tephigramme
 *
 * Parametres   :
 *   <Graph>    : Item graph
 *   <AxisTH>   : Axe adiabat
 *   <AxisT>    : Axe temperature
 *   <AxisP>    : Axe pression
 *   <GLMode>   : Mode de rendue
 *   <P>        : Isobar
 *   <Label>    : Libelle a afficher (Si NULL, pas de libelle)
 *   <Width>    : Largeur de la libelle
 *   <Height>   : Hauteur de lal libelle
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void  GraphTehpi_DisplayIsobar(GraphItem *Graph,TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,GLuint GLMode,double P,Tk_TextLayout Label,int Width,int Height) {

   int           dx,dy,l;
   double        x0,y0;
   double        t0,tmax;
   static double range[25][2] = {{10,-18.3},{20,-7.9},{30,-1.6},{40,3.3},{50,7.1},{100,18.7},{150,26.3},{200,32.2},{250,36.6},
      {300,40.6},{350,44.0},{400,46.7},{450,49.6},{500,51.7},{550,53.7},{600,55.8},{650,57.8},{700,59.6},{750,61.1},{800,62.7},
      {850,64.3},{900,65.5},{950,66.9},{1000,68.2},{1050,70.0}};

   t0=AxisT->T0;
   t0=P>500?-60.0:P>300?-80.0:P>200?-90.0:t0;
   GraphTephi_TP2XY(AxisTH,AxisT,AxisP,t0,P,&x0,&y0);
   tmax=AxisT->T1;
   for (l=0;l<25;l++) {
      if (P==range[l][0]) {
         tmax = range[l][1];
         break;
      }
   }
   glBegin(GL_LINE_STRIP);
   glVertex2d(x0,y0);

   while (t0<tmax) {
      t0+=5.0;
      if (t0>tmax) t0=tmax;
      GraphTephi_TP2XY(AxisTH,AxisT,AxisP,t0,P,&x0,&y0);
      glVertex2d(x0,y0);
   }
   glEnd();

   if (Label) {
      glDisplayTextLayout(Label,(int)-AxisP->Angle+1000,(int)(x0),(int)(y0),0,-1);
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphTehpi_DisplayIsobars>
 * Creation     : Juillet 2008 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher les Isobars du tephigramme
 *
 * Parametres   :
 *   <Graph>    : Item graph
 *   <AxisTH>   : Axe adiabat
 *   <AxisT>    : Axe temperature
 *   <AxisP>    : Axe pression
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *   <GLMode>   : Mode de rendue
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphTehpi_DisplayIsobars(GraphItem *Graph,TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,int X0,int Y0,int X1,int Y1,GLuint GLMode) {

   Tk_TextLayout text;
   Tk_Font       font;
   XColor       *color;

   int           i,width,height;
   char          buf[32];
   double        p;

   font=AxisP->Font?AxisP->Font:Graph->Font;
   color=AxisP->Color?AxisP->Color:Graph->FGColor;

   if (!font || !color) {
      return;
   }

   glFontUse(Tk_Display(Tk_CanvasTkwin(Graph->canvas)),font);
   if (AxisP->GridWidth) {
      if (AxisP->GridColor)
         glColor3us(AxisP->GridColor->red,AxisP->GridColor->green,AxisP->GridColor->blue);
      glDash(&AxisP->Dash);
      glLineWidth(AxisP->GridWidth);
   }

   if (AxisP->InterNb) {
      for(i=0;i<AxisP->InterNb;i++) {
         if (AxisP->Label) {
            text=Tk_ComputeTextLayout(font,AxisP->Label[i],Tcl_NumUtfChars(AxisP->Label[i],strlen(AxisP->Label[i])),0,TK_JUSTIFY_CENTER,0,&width,&height);
         } else {
            sprintf(buf,"%d (%d)\n",(int)AxisP->Inter[i],(int)(Thermo_StandardHeight(AxisP->Inter[i])+0.5));
            text=Tk_ComputeTextLayout(font,buf,strlen(buf),0,TK_JUSTIFY_CENTER,0,&width,&height);
         }
         GraphTehpi_DisplayIsobar(Graph,AxisTH,AxisT,AxisP,GLMode,AxisP->Inter[i],text,width,height);
         Tk_FreeTextLayout(text);
     }
   } else {
      p=AxisP->T0;
      while (p>AxisP->T1) {
         sprintf(buf,"%d (%d)\n",(int)p,(int)(Thermo_StandardHeight(p)+0.5));
         text=Tk_ComputeTextLayout(font,buf,strlen(buf),0,TK_JUSTIFY_CENTER,0,&width,&height);
         GraphTehpi_DisplayIsobar(Graph,AxisTH,AxisT,AxisP,GLMode,p,text,width,height);
         Tk_FreeTextLayout(text);
         p-=AxisP->Incr;
      }
   }

   if (AxisP->HighLightNb) {
      if (AxisP->HighLightWidth)
         glLineWidth(AxisP->HighLightWidth);
      if (AxisP->HighLightColor)
         glColor3us(AxisP->HighLightColor->red,AxisP->HighLightColor->green,AxisP->HighLightColor->blue);

      for(i=0;i<AxisP->HighLightNb;i++) {
         GraphTehpi_DisplayIsobar(Graph,AxisTH,AxisT,AxisP,GLMode,AxisP->HighLight[i],NULL,0,0);
     }
   }
   glDisable(GL_LINE_STIPPLE);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphTehpi_DisplayDryAdiabat>
 * Creation     : Juillet 2008 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher un Adiabat Sec du tephigramme
 *
 * Parametres   :
 *   <Graph>    : Item graph
 *   <AxisTH>   : Axe adiabat
 *   <AxisT>    : Axe temperature
 *   <AxisP>    : Axe pression
 *   <GLMode>   : Mode de rendue
 *   <TH>       : Adiabat
 *   <Label>    : Libelle a afficher (Si NULL, pas de libelle)
 *   <Width>    : Largeur de la libelle
 *   <Height>   : Hauteur de lal libelle
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphTehpi_DisplayDryAdiabat(GraphItem *Graph,TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,GLuint GLMode,double TH,Tk_TextLayout Label,int Width,int Height) {

   double x0,x1,y0,y1,pl,tl;
   int    i;

   static double range[33][2] = {{600, 75.6},{590, 81.4},{580, 87.9},{570, 95.2},{560, 103.1},{550, 111.3},{540, 122.0},
      {530, 131.0},{520, 143.7},{510, 157.0},{500, 171.2},{490, 187.8},{480, 206.7},{470, 227.0},{460, 249.7},{450, 277.2},
      {440, 307.0},{430, 340.8},{420, 379.0},{410, 424.4},{400, 475.0},{395, 505.0},{390, 533.1},{385, 570.3},{380, 606.0},
      {375, 642.5},{370, 682.9},{365, 728.6},{360, 775.9},{355, 831.5},{350, 887.2},{345, 948.5},{340, 1018.8}};

   pl=AxisP->T0;
   tl=(TH<260)?-60:(TH<285)?-80:(TH<290)?-90:AxisT->T0;
   for(i=0;i<33;i++) {
      if (TH>=range[i][0]) {
         pl=range[i][1];
         break;
      }
   }
   GraphTephi_THP2XY(AxisTH,AxisT,AxisP,TH,pl,&x0,&y0);
   GraphTephi_TTH2XY(AxisTH,AxisT,AxisP,tl,TH,&x1,&y1);

   glBegin(GL_LINES);
      glVertex2d(x0,y0);
      glVertex2d(x1,y1);
   glEnd();

   if (Label) {
      glDisplayTextLayout(Label,(int)-AxisTH->Angle+1000,(int)(x1-Width),(int)(y1+Height),0,-1);
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphTehpi_DisplayDryAdiabats>
 * Creation     : Juillet 2008 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher les Adiabats Secs du tephigramme
 *
 * Parametres   :
 *   <Graph>    : Item graph
 *   <AxisTH>   : Axe adiabat
 *   <AxisT>    : Axe temperature
 *   <AxisP>    : Axe pression
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *   <GLMode>   : Mode de rendue
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphTehpi_DisplayDryAdiabats(GraphItem *Graph,TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,int X0,int Y0,int X1,int Y1,GLuint GLMode) {

   Tk_TextLayout text;
   Tk_Font      font;
   XColor       *color;

   char          buf[32];
   int           width,height,dx,dy;
   int           theta;
   int           i;

   font=AxisTH->Font?AxisTH->Font:Graph->Font;
   color=AxisTH->Color?AxisTH->Color:Graph->FGColor;

   if (!font || !color) {
      return;
   }

   glFontUse(Tk_Display(Tk_CanvasTkwin(Graph->canvas)),font);
   if (AxisTH->GridWidth) {
      if (AxisTH->GridColor)
         glColor3us(AxisTH->GridColor->red,AxisTH->GridColor->green,AxisTH->GridColor->blue);
      glDash(&AxisTH->Dash);
      glLineWidth(AxisTH->GridWidth);
   }

   if (AxisTH->InterNb) {
      for(i=0;i<AxisTH->InterNb;i++) {
         if (AxisTH->Label) {
            text=Tk_ComputeTextLayout(font,AxisTH->Label[i],Tcl_NumUtfChars(AxisTH->Label[i],strlen(AxisTH->Label[i])),0,TK_JUSTIFY_CENTER,0,&width,&height);
         } else {
            GraphAxis_Print(AxisTH,buf,AxisTH->Inter[i],0);
            text=Tk_ComputeTextLayout(font,buf,strlen(buf),0,TK_JUSTIFY_CENTER,0,&width,&height);
         }
         GraphTehpi_DisplayDryAdiabat(Graph,AxisTH,AxisT,AxisP,GLMode,AxisTH->Inter[i],text,width,height);
         Tk_FreeTextLayout(text);
     }
   } else {
      theta=(int)AxisTH->Min;
      while (theta<AxisTH->Max+1.0) {
         GraphAxis_Print(AxisTH,buf,(double)theta,0);
         text=Tk_ComputeTextLayout(font,buf,strlen(buf),0,TK_JUSTIFY_CENTER,0,&width,&height);
         GraphTehpi_DisplayDryAdiabat(Graph,AxisTH,AxisT,AxisP,GLMode,theta,text,width,height);
         Tk_FreeTextLayout(text);
         theta+=AxisTH->Incr;
      }
   }

   if (AxisTH->HighLightNb) {
      if (AxisTH->HighLightWidth)
         glLineWidth(AxisTH->HighLightWidth);
      if (AxisTH->HighLightColor)
         glColor3us(AxisTH->HighLightColor->red,AxisTH->HighLightColor->green,AxisTH->HighLightColor->blue);

      for(i=0;i<AxisTH->HighLightNb;i++) {
         GraphTehpi_DisplayDryAdiabat(Graph,AxisTH,AxisT,AxisP,GLMode,AxisTH->HighLight[i],NULL,0,0);
     }
   }
   glDisable(GL_LINE_STIPPLE);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphTehpi_DisplayWetAdiabats>
 * Creation     : Juillet 2008 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher les Adiabats humides du tephigramme
 *
 * Parametres   :
 *   <Graph>    : Item graph
 *   <AxisTH>   : Axe adiabat
 *   <AxisT>    : Axe temperature
 *   <AxisP>    : Axe pression
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *   <GLMode>   : Mode de rendue
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphTehpi_DisplayWetAdiabats(GraphItem *Graph,TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,int X0,int Y0,int X1,int Y1,GLuint GLMode) {

   Tk_TextLayout text;
   char          buf[32];
   XColor       *color;
   Tk_Font       font;
   int           width,height,dx,dy;

   int n=0,p;

   double   tmin=AxisT->T0+20;
   double   dp = -25.0;
   double   x0,y0;
   int      i,k;
   int      count, lcount;
   double   p0,t0,th0,t,tp,th,lambda,lth,rdp;
   double   pts[2][64][3];

   t = (double)((int)(tmin/4.0+1)*4.0);
   t = -44;

   font=AxisTH->Font?AxisTH->Font:Graph->Font;
   color=AxisTH->Color?AxisTH->Color:Graph->FGColor;

   if (!font || !color) {
      return;
   }

   glFontUse(Tk_Display(Tk_CanvasTkwin(Graph->canvas)),font);
   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   gluTessBeginPolygon(GLRender->GLTess,NULL);
   gluTessBeginContour(GLRender->GLTess);
   glColor3ub(245,222,179);

   while (t<=AxisT->T1) {
      p=0;n=!n;

      p0 = AxisP->T0; t0 =  t;
      th = Thermo_PT2TH(AxisTH,AxisT,AxisP,p0,t0);

      th0=th;
      Thermo_eTHP(AxisTH,AxisT,AxisP,&th0,&p0,&t0,0.0,0.0,50.0,0.0);
      GraphTephi_TTH2XY(AxisTH,AxisT,AxisP,t0,th0,&x0,&y0);
      pts[n][p][0]=x0;pts[n][p][1]=y0;pts[n][p][2]=0.0;
//      gluTessVertex(GLRender->GLTess,pts[n][p],pts[n][p]);
      p++;

      p0=AxisP->T0;t0= t;
      GraphTephi_TTH2XY(AxisTH,AxisT,AxisP,t0,th0,&x0,&y0);
      pts[n][p][0]=x0;pts[n][p][1]=y0;pts[n][p][2]=0.0;
//      gluTessVertex(GLRender->GLTess,pts[n][p],pts[n][p]);
      p++;


     while (t0>=tmin && p0>AxisP->Max) {
         tp=t0;
         rdp=(p0>150.0)?dp:((p0>30.0)?-10.0:-5.0) ;
         Thermo_eTHP(AxisTH,AxisT,AxisP,&th0,&p0,&t0,0.0,0.0,rdp,0.0);
         t0=t0<tmin?tmin:t0;
         GraphTephi_TTH2XY(AxisTH,AxisT,AxisP,t0,th0,&x0,&y0);
         pts[n][p][0]=x0;pts[n][p][1]=y0;pts[n][p][2]=0.0;
         p++;
      }

      glColor3us(color->red,color->green,color->blue);
      GraphAxis_Print(AxisT,buf,(double)t-4,0);
      text=Tk_ComputeTextLayout(font,buf,strlen(buf),0,TK_JUSTIFY_CENTER,0,&width,&height);
      glDisplayTextLayout(text,(int)-AxisT->Angle+1000,(int)(x0-width),(int)(y0+height),0,-1);
      Tk_FreeTextLayout(text);
      glColor3ub(245,222,179);

      t+=4.0;

      for(i=(n?0:p-1);(n?i<p:i>=0);i+=(n?1:-1)){
         gluTessVertex(GLRender->GLTess,pts[n][i],pts[n][i]);
      }

      if (!n) {
         gluTessEndContour(GLRender->GLTess);
         gluTessEndPolygon(GLRender->GLTess);
         gluTessBeginPolygon(GLRender->GLTess,NULL);
         gluTessBeginContour(GLRender->GLTess);
      }
   }
   gluTessEndContour(GLRender->GLTess);
   gluTessEndPolygon(GLRender->GLTess);

   glDisable(GL_LINE_STIPPLE);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphTehpi_DisplayMixRatio>
 * Creation     : Juillet 2008 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher un Mix Ratios du tephigramme
 *
 * Parametres   :
 *   <Graph>    : Item graph
 *   <AxisTH>   : Axe adiabat
 *   <AxisT>    : Axe temperature
 *   <AxisP>    : Axe pression
 *   <AxisW>    : Axe ratios
 *   <GLMode>   : Mode de rendue
 *   <M>        : Ratio a afficher
 *   <Label>    : Libelle a afficher (Si NULL, pas de libelle)
 *   <Width>    : Largeur de la libelle
 *   <Height>   : Hauteur de lal libelle
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphTehpi_DisplayMixRatio(GraphItem *Graph,TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,TGraphAxis *AxisW,GLuint GLMode,double M,Tk_TextLayout Label,int Width,int Height) {

   double p0,t0,x0,y0;

   p0=AxisP->T0;
   t0=Thermo_PR2T(p0,M);
   GraphTephi_TP2XY(AxisTH,AxisT,AxisP,t0,p0,&x0,&y0);

   if (Label) {
      glDisplayTextLayout(Label,(int)-AxisW->Angle+1000,(int)(x0),(int)(y0-Height),0,-1);
   }

   glBegin(GL_LINE_STRIP);
   glVertex2d(x0,y0);

   while(p0>AxisP->T1) {
      p0=(p0<100)?p0-10:p0-50;
      p0=p0>AxisP->T1?AxisP->T1:p0;
      t0=Thermo_PR2T(p0,M);
      GraphTephi_TP2XY(AxisTH,AxisT,AxisP,t0,p0,&x0,&y0);
      glVertex2d(x0,y0);
   }
   glEnd();
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphTehpi_DisplayMixRatios>
 * Creation     : Juillet 2008 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher les Mix Ratios du tephigramme
 *
 * Parametres   :
 *   <Graph>    : Item graph
 *   <AxisTH>   : Axe adiabat
 *   <AxisT>    : Axe temperature
 *   <AxisP>    : Axe pression
 *   <AxisW>    : Axe ratios
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *   <GLMode>   : Mode de rendue
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphTehpi_DisplayMixRatios(GraphItem *Graph,TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,TGraphAxis *AxisW,int X0,int Y0,int X1,int Y1,GLuint GLMode) {

   Tk_TextLayout text;
   Tk_Font       font;
   XColor       *color;

   char          buf[32];
   int           width,height,dx,dy;
   int           i;
   double        m;

   font=AxisW->Font?AxisW->Font:Graph->Font;
   color=AxisW->Color?AxisW->Color:Graph->FGColor;

   if (!font || !color) {
      return;
   }

   glFontUse(Tk_Display(Tk_CanvasTkwin(Graph->canvas)),font);
   if (AxisW->GridWidth) {
      if (AxisW->GridColor)
         glColor3us(AxisW->GridColor->red,AxisW->GridColor->green,AxisW->GridColor->blue);
      glDash(&AxisW->Dash);
      glLineWidth(AxisW->GridWidth);
   }

   if (AxisW->InterNb) {
      for(i=0;i<AxisW->InterNb;i++) {
         if (AxisW->Label) {
            text=Tk_ComputeTextLayout(font,AxisW->Label[i],Tcl_NumUtfChars(AxisW->Label[i],strlen(AxisW->Label[i])),0,TK_JUSTIFY_CENTER,0,&width,&height);
         } else {
            GraphAxis_Print(AxisW,buf,AxisW->Inter[i],0);
            text=Tk_ComputeTextLayout(font,buf,strlen(buf),0,TK_JUSTIFY_CENTER,0,&width,&height);
         }
         GraphTehpi_DisplayMixRatio(Graph,AxisTH,AxisT,AxisP,AxisW,GLMode,AxisW->Inter[i],text,width,height);
         Tk_FreeTextLayout(text);
     }
   } else {
      m=(int)AxisW->Min;
      while (m<AxisW->Max) {
         GraphAxis_Print(AxisW,buf,(double)m,0);
         text=Tk_ComputeTextLayout(font,buf,strlen(buf),0,TK_JUSTIFY_CENTER,0,&width,&height);
         GraphTehpi_DisplayMixRatio(Graph,AxisTH,AxisT,AxisP,AxisW,GLMode,m,text,width,height);
         Tk_FreeTextLayout(text);
         m+=AxisW->Incr;
      }
   }

   if (AxisW->HighLightNb) {
      if (AxisW->HighLightWidth)
         glLineWidth(AxisW->HighLightWidth);
      if (AxisW->HighLightColor)
         glColor3us(AxisW->HighLightColor->red,AxisW->HighLightColor->green,AxisW->HighLightColor->blue);

      for(i=0;i<AxisTH->HighLightNb;i++) {
         GraphTehpi_DisplayMixRatio(Graph,AxisTH,AxisT,AxisP,AxisW,GLMode,AxisW->HighLight[i],NULL,0,0);
     }
   }
   glDisable(GL_LINE_STIPPLE);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <GraphItem_DisplayXYZ>
 * Creation     : Mai 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But          : Afficher l'item de graph dans le graph en vectoriel
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Graph>    : Item graph
 *   <Item>     : Item de graph
 *   <AxisT>    : Axe en X
 *   <AxisY>    : Axe en Y
 *   <AxisP>    : Axe en Z
 *   <X0>       : Limite inferieure gauche du graph
 *   <Y0>       : Limite inferieure gauche du graph
 *   <X1>       : Limite superieur  droite du graph
 *   <Y1>       : Limite superieur  droite du graph
 *   <GLMode>   : Mode de rendue
 *
 * Retour       :
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
void GraphItem_DisplayTephi(Tcl_Interp *Interp,GraphItem *Graph,TGraphItem *Item,TGraphAxis *AxisT,TGraphAxis *AxisP,TGraphAxis *AxisTH,int X0,int Y0,int X1,int Y1,GLuint GLMode) {

   Tk_FontMetrics tkm;
   Tcl_Obj       *obj;
   TVector       *vdry,*vpres,*vwet,*vdew,*vspd,*vdir,*vspr;
   double         v[2],val,v0[2],t;
   char           buf[32];
   int            i,vn,j,x,y,px,py,pw;

   /*Pressure (YData)*/
   vpres=Vector_Get(Item->YData);

   if (Item->Alpha<100) {
      glEnable(GL_BLEND);
   }

   /* Display graph outline */
   if (Item->Outline && Item->Width && GLMode==GL_RENDER) {
      glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,Item->Alpha*Graph->Alpha*0.01*655);
      glLineWidth(Item->Width);
      glPolygonMode(GL_FRONT,GL_LINE);

      /*DryBulb (XData)*/
      if ((vdry=Vector_Get(Item->XData)) && (vn=vdry->N<vpres->N?vdry->N:vpres->N)) {
         glBegin(GL_LINE_STRIP);
         for(i=0;i<vn;i++) {
            if (vdry->V[i]!=vdry->NoData && vpres->V[i]!=vpres->NoData) {
               if (GraphTephi_TP2XY(AxisTH,AxisT,AxisP,vdry->V[i],vpres->V[i],&v[0],&v[1])) {
                  glVertex2dv(v);
               } else {
                  glEnd();
                  glBegin(GL_LINE_STRIP);
               }
            }
         }
         glEnd();
      }

      /*WetBulb (MinData)*/
      if ((vwet=Vector_Get(Item->MinData)) && (vn=vwet->N<vpres->N?vwet->N:vpres->N)) {
         glDash(&Graph->Dash[1]);
         glBegin(GL_LINE_STRIP);
         for(i=0;i<vn;i++) {
            if (vwet->V[i]!=vwet->NoData && vpres->V[i]!=vpres->NoData) {
               if (GraphTephi_TP2XY(AxisTH,AxisT,AxisP,vwet->V[i],vpres->V[i],&v[0],&v[1])) {
                  glVertex2dv(v);
               } else {
                  glEnd();
                  glBegin(GL_LINE_STRIP);
               }
            }
         }
         glEnd();
      }

      /*DewPoint (MaxData)*/
      if ((vdew=Vector_Get(Item->MaxData)) && (vn=vdew->N<vpres->N?vdew->N:vpres->N)) {
         glDash(&Graph->Dash[0]);
         glBegin(GL_LINE_STRIP);
         for(i=0;i<vn;i++) {
            if (vdew->V[i]!=vdew->NoData && vpres->V[i]!=vpres->NoData) {
               if (GraphTephi_TP2XY(AxisTH,AxisT,AxisP,vdew->V[i],vpres->V[i],&v[0],&v[1])) {
                  glVertex2dv(v);
               } else {
                  glEnd();
                  glBegin(GL_LINE_STRIP);
               }
            }
         }
         glEnd();
      }
      glDisable(GL_LINE_STIPPLE);

      vspd=Vector_Get(Item->Speed);
      vdir=Vector_Get(Item->Dir);
      vspr=Vector_Get(Item->ZData);
      vspr=!vspr?vpres:vspr;

      if (vspd && vdir) {
         GraphTephi_TP2XY(AxisTH,AxisT,AxisP,AxisT->T1,AxisP->T0,&v0[0],&v0[1]);
         for(i=0;i<vspd->N;i++) {
            if (vspd->V[i]!=vspd->NoData && vdir->V[i]!=vdir->NoData && vspr->V[i]!=vspr->NoData) {
               y=Y0;
               t=GraphTephi_PX2T(AxisTH,AxisT,AxisP,vspr->V[i],v0[0],&y,&Y1);
               if (GraphTephi_TP2XY(AxisTH,AxisT,AxisP,t,vspr->V[i],&v[0],&v[1])) {
                  Data_RenderBarbule(1,1,0.0,v0[0],v[1],0.0,vspd->V[i],vdir->V[i],Item->Size,NULL);
               }
            }
         }
      }
   }

   /* Display Values */
   if (Item->Value && Item->Font && GLMode==GL_RENDER) {
      val=-999.0;
      px=py=pw=0;
      glFontUse(Tk_Display(Tk_CanvasTkwin(Graph->canvas)),Item->Font);
      glColor4us(Item->Outline->red,Item->Outline->green,Item->Outline->blue,Item->Alpha*Graph->Alpha*0.01*655);
      Tk_GetFontMetrics(Item->Font,&tkm);

      for(i=0;i<vn;i++) {
         switch(Item->Value) {
            case  1:
            case 14: if (vdry)  val=vdry->V[i]; break;
            case 15: if (vpres) val=vpres->V[i]; break;
            case 16: if (vwet)  val=vwet->V[i]; break;
            case 17: if (vdew)  val=vdew->V[i]; break;
            case 18: if (vdry && vdew) val=vdry->V[i]-vdew->V[i]; break;
//            case 19: val=0.01*ua_rec->h[k]/FOOT; break;
            case 20: if (vpres && vdry && vdew) val=Thermo_TD2RH(vpres->V[i],vdry->V[i],vdew->V[i]); break;
         }
         if (val!=-999.0) {
            if (GraphTephi_TP2XY(AxisTH,AxisT,AxisP,vdry->V[i],vpres->V[i],&v[0],&v[1])) {
               GraphAxis_Print(AxisT,buf,val,-2);
               j=Item->Width+2+(Item->Icon?Item->Size:0);
               y=v[1]+j;
               j=Tk_TextWidth(Item->Font,buf,strlen(buf));
               switch(Item->Anchor) {
                  case TK_ANCHOR_CENTER: x=v[0]-j/2; break;
                  case TK_ANCHOR_W:      x=v[0]+j/strlen(buf);     break;
                  case TK_ANCHOR_E:      x=v[0]-j-j/strlen(buf);   break;
               }
               if (!((y>=py && y<=py+tkm.linespace) || (y+tkm.linespace>=py && y<=py)) || !((x>=px && x<=px+pw) ||  (x+j>=px && x+j<=px+pw))) {
                  px=x;py=y;pw=j;
                  glPrint(Interp,Graph->canvas,buf,x,y,0.0);
               }
            }
         }
      }
   }

   glDisable(GL_BLEND);
}

