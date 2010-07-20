//  te_heights():  recalculates heights
void ws_te_lib::te_heights(UA_REC *ua_rec)
{
   int      k;
        ws_thermo       wth;

   for (k = ua_rec->sfc_level+1; k < ua_rec->ntephi; k++)
      ua_rec->h[k] = ua_rec->h[k-1] + wth.thermo_delta_h(ua_rec->p[k-1],
         ua_rec->t[k-1], ua_rec->p[k], ua_rec->t[k]);
}

//   thermo_delta_h(): calculates height increment
double ws_thermo::thermo_delta_h(double P0,double T0,double P1,double T1) {

   if (p0==p1)
      return(0.0);

   return(0.5*R_d/g_0*(T0+T1+2*AZ)*log(P0/P1));
}

//   te_lcl():   calculates height of LCL
double ws_te_lib::te_lcl(double Ps,int Np,double H) {

   int      k;
   float    lambda;

   if (ua_rec->asc.nlevels == 0)
      return (MSNGV);

   for (k=0;k<Np;k++)
      if (P[k]<ua_rec->asc.p[1])
         break;

   if (k==Np)
      return(MSNGV);

   lambda=log(P[k-1]);
   lambda = (log(ua_rec->asc.p[1])-lambda)/(log(P[k])-lambda);
   return (H[k-1]+lambda*(H[k]-H[k-1]));
}

//     te_level(): returns index to ua_rec at level p
int Thermo_PressureLevel(double Ps,int Np,double P){

   int k;

   for (k=0;k<Np;k++) {
      if (Ps[k]==P)
         return(k);
   }
   return (-1);
}

//   te_cape():  calculates convective available potential energy
double Thermo_ConvectiveAvailablePotentialEnergy(double *Ascent,int Na) {

   int   k;
   double cape=0.0;

   for (k=0;k<Na;k++) {
      if (Ascent[k]>0.0)
         cape+=Ascent[k];
   }
   return (cape);
}

//   te_cix():   calculates Clarke index
double Thermo_ClarkeIndex(double *P,int Np,double *H) {
   int l100,l50,l25;

   if ((l100=Thermo_PressureLevel(P,Np,1000.0))<0)
      return(MSNGV);
   if ((l50=Thermo_PressureLevel(P,Np,500.0))<0)
      return(MSNGV);
   if ((l25=Thermo_PressureLevel(P,Np,250.0))<0)
      return(MSNGV);
   return (0.1*(2.0*H[l50]-H[l25]-H[l100]));
}

//  te_gix():   calculates George's K index
double Thermo_GeorgesKIndex(double *P,int Np,double *T,double *TD) {

   int   l85, l70, l50;
   float t85, td85, t70, td70, t50;

   if ((l85=Thermo_PressureLevel(P,Np,850.0))<0)
      return (MSNGV);
   if ((l70=Thermo_PressureLevel(P,Np,700.0))<0)
      return (MSNGV);
   if ((l50=Thermo_PressureLevel(P,Np,500.0))<0)
      return (MSNGV);
   if ((t85=T[l85])<MSNG || (td85=TD[l85])<MSNG || (t70=T[l70])<MSNG || (td70=TD[l70])<MSNG || (t50=T[l50])<MSNG)
      return(MSNG);

   return(t85+td85-t70+td70-t50);
}

//   te_tix():   calculates Total totals index
double Thermo_TotalIndex(double *P,int Np,double *T,double *TD) {

   int   l85, l50;
   float t85, td85, t50;

   if ((l85 = Thermo_PressureLevel(P,Np,850.0))<0)
      return (MSNGV);
   if ((l50 = Thermo_PressureLevel(P,Np,500.0))<0)
      return (MSNGV);
   if ((t85=T[l85])<MSNG || (td85=TD[l85])<MSNG || (t50=T[l50])<MSNG)
      return (MSNGV);

   return(t85+td85-2.0*t50);
}

//  te_six():   calculates Showalter index
double Thermo_ShowalterIndex(double *P,int Np,double *T,double *TD) {

   int      l85, l50;
   float    t85, td85, t50;
   double   p0, t0, p1, t1, th, ln_th0, lambda;
   static float   dp = -25.0;
   static float   log500 = 6.2146081;
        ws_thermo       wth;

   if ((l85 = Thermo_PressureLevel(P,Np,850.0))<0)
      return (MSNGV);
   if ((l50 = Thermo_PressureLevel(P,Np,500.0))<0)
      return (MSNGV);

   if ((t85=T[l85])<MSNG || (td85=TD[l85])<MSNG || (t50=T[l50])<MSNG)
      return (MSNGV);

   wth.thermo_lcl(850.0,t85,td85,&p0,&t0);
   th=(TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,p0,t0);
   ln_th0 = log(th);

   do {
      p1=p0;
      t1=t0;
      Thermo_eTHP(TGraphAxis *AxisTH,TGraphAxis *AxisT,TGraphAxis *AxisP,&ln_th0,&p0,&t0,0.0,0.0,dp,0.0);
   } while (p0>500.0);

   lambda=log(p0);
   lambda=(log500-lambda)/(log(p1)-lambda);
   t0+=lambda*(t1-t0);

   return(t50-t0);
}

//   te_lix():   calculates lifted index
double Thermo_LiftedIndex(double *P,int Np,double *T) {

   int           k,l50;
   double        lambda, t_high;
   static double log500=6.2146081;

   if ((l50 = Thermo_PressureLevel(P,Np,500.0))<0)
      return (MSNGV);
   if ((t_high = T[l50])<MSNG)
      return (MSNGV);

   for (k=0;k<ua_rec->asc.nlevels;k++) {
      if (ua_rec->asc.p[k]<500.0)
         break;
   }
   if (k==ua_rec->asc.nlevels)
      return (MSNGV);

   lambda=log(ua_rec->asc.p[k]);
   lambda=(log500-lambda)/(log(ua_rec->asc.p[k-1])-lambda);

   return(t_high-ua_rec->asc.t[k]-lambda*(ua_rec->asc.t[k-1]-ua_rec->asc.t[k]));
}

//   te_pcpw():  calculates precipitable water
double Thermo_Precipitable(double *P,int Np,double *T) {

   int      k;
   float    p1, p2, td, pcpw;
   float    s_hum1, s_hum2;
   static float   c = 0.05 / g_0;
        ws_thermo       wth;

   pcpw=0.0;
   for (k=0;k<Np;k++) {
     if ((td=TD[k])>MSNG)
         break;
   }
   p1=P[k];
   s_hum1=Thermo_PT2SH(p1,td);
   for (k++;k<Np;k++) {
      if ((td=TD[k])<MSNG)
         continue;
      }
      p2=P[k];
      s_hum2=Thermo_PT2SH(p2,td);
      pcpw+=c*(s_hum1+s_hum2)*(p1-p2);
      s_hum1=s_hum2;
      p1=p2;
   }
   return((pcpw==0.0)?MSNGV:pcpw);
}

//   te_flevel():   calculates freezing levels
int Thermo_Freezing(double *P,int Np,double *T,double *H,double *Levels) {
   int      k, n, plus, pp;

   plus=(T[ua_rec->sfc_level]>0.0);
   for (n=0,k=ua_rec->sfc_level+1;k<Np k++) {
      if (T[k]<-30.0)
         break;
      pp=(T[k]>0.0);
      if (pp==plus)
         continue;
      plus=pp;
      Levels[n++]=H[k]-(H[k]-H[k-1])/(T[k]-T[k-1])*T[k];
   }
   return(n);
}

//   te_be(): calculates bouyant energy w.r. to t = 0 degC
int Thermo_BouyantEnergy(double *P,int Np,double *T,double *H,double *Levels) {

   int      k;
   float    t0, t1, td0, td1, tv0, tv1, p0, p1;
   float    be, lambda;
        ws_thermo       wth;

   for (k=0;k<Np;k++) {
      if (P[k]<pbot)
         break;
   }
   if (k==0 || k==Np || T[k-1]<MSNG)
      return(MSNGV);

   lambda=log(P[k-1]);
   lambda=(log(pbot)-lambda)/(log(P[k])-lambda);

   p0=pbot;
   t0=T[k-1]+lambda*(T[k]-T[k-1]);
   td0=(TD[k-1]<MSNG)?(TD[k]<MSNG)?t0-10.0:TD[k]:TD[k-1]+lambda*(TD[k]-TD[k-1]);
   tv0=Thermo_VirtualTemp(p0,t0,td0);
   for (be=0.0;k<Np;k++) {
      p1=P[k];
      t1=T[k];
      td1=(TD[k]<MSNG)?t1-10.0:TD[k];
      tv1=Thermo_VirtualTemp(p1,t1,td1);
      if (p1<ptop)
         break;
      be+=Thermo_Bouyant(tv0,tv1,0.0,0.0,p0,p1);
      p0=p1;
      tv0=tv1;
   }
   if (k==Np)
      return (MSNGV);
   lambda = log(P[k-1]);
   lambda = (log(ptop)-lambda)/(log(P[k])-lambda);
   be+=lambda*Thermo_Bouyant(tv0,tv1,0.0,0.0,p0,p1);
   return(be);
}

//   te_tropopause():  finds first tropopause
int Thermo_Tropopause(double *P,int Np,double *T,double *H,double *Levels) {

   int    i,k,is_trop;
   double lapse;

   is_trop=0;
   for (k=ua_rec->sfc_level;k<Np-1;k++) {
      lapse=-(T[k+1]-T[k])/(H[k+1]-H[k]);
      if (lapse<=2.0e-3) {
         is_trop=1;
         for (i=k+2;i<Np;i++) {
            if (H[i]-H[k]>2000.0)
               break;
            lapse=-(T[i]-T[k])/(H[i]-H[k]);
            if (lapse>2.0e-3) {
               is_trop=0;
               break;
            }
         }
      }
      if (is_trop) {
         return(k);
         break;
      }
   }
   return(-1);
}


