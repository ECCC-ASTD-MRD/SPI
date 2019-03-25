
namespace eval RGraph::Tephi {
    variable Data
    variable Param

    set Param(Title)        {"Tephigramme" "Tephigram"}
    set Param(RPackages)    {grid}

    variable Lbl

    set Lbl(Winds)          {"Barbules de vent  " "Wind barbs    "}
    set Lbl(Temp)           {"Température       " "Temperature   "}
    set Lbl(Dewpt)          {"Point de rosée    " "Dew point     "}
    set Lbl(Iso)            {"Isothermes        " "Isotherm      "}
    set Lbl(Pres)           {"Pression          " "Pressure      "}
    set Lbl(Adi)            {"Pseudo-adiabatique" "Pseudo-Adiatic"}
    set Lbl(RS)             {"Rapport de mélange" "Mixing ratio  "}
    set Lbl(Theta)          {"Adiabatique sèche " "Dry adiabatic "}
    set Lbl(Wind)           {"Barbules de vent  " "Wind barbs    "}
    set Lbl(Date)           {"Date              " "Date          "}
    set Lbl(Info)           {"Info              " "Info          "}
    set Lbl(Loc)            {"Localisation      " "Location      "}
    set Lbl(DateSeq)        {"Index date        " "Date index    "}
    set Lbl(PTop)           {"Pression min      " "Min pressure  "}

    set Lbl(Elev)           {"Élévation" "Elevation"}
    set Lbl(WindV)          {"Vent" "Wind"}
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Tephi::RSetup>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors de la première initialisation de ce type de graph
#
# Parametres :
#
# Retour:
#
# Remarques : Optionnel. Cette fonction n'est exécutée qu'une seule fois.
#
#----------------------------------------------------------------------------
proc RGraph::Tephi::RSetup { } {
    R exec {
        #==============================================================================
        #
        # Thepigramme en R, pour iMeteo.ca
        #
        # Author : Andre Plante et Claude Girard juin 2006
        #          Andre Plante fevrier 2014 ajout de vents
        #          Éric Legault-Ouellet 2019 optimiser et adapter aux besoins du RGraph
        #              (use current device,custom rs.col,etc)
        #
        # Note : Code original pris ici : https://gitlab.science.gc.ca/cmdn_utils/tephi
        #==============================================================================
        #------------------------------------------------------------------------------

        tephi <- function() {
            #----- Constants
            t.cpd       <-  0.1005460000000E+04; #  CHAL. SPEC. AIR SEC   J KG-1 K-1
            t.cpv       <-  0.1869460000000E+04; #  CHAL. SPEC. VAP EAU   J KG-1 K-1
            t.rgasd     <-  0.2870500000000E+03; #  CTE GAZ - AIR SEC     J KG-1 K-1
            t.rgasv     <-  0.4615100000000E+03; #  CTE GAZ - VAP EAU     J KG-1 K-1
            t.trpl      <-  0.2731600000000E+03; #  POINT TRIPLE - EAU    K
            t.tcdk      <-  0.2731500000000E+03; #  PASSAGE K A C         C
            t.rauw      <-  0.1000000000000E+04; #  DENSITE EAU LIQ       KG M-3
            t.eps1      <-  0.6219800221014E+00; #  RGASD/RGASV
            t.eps2      <-  0.3780199778986E+00; #  1 - EPS1
            t.delta     <-  0.6077686814144E+00; #  1/EPS1 - 1
            t.cappa     <-  0.2854912179500E+00; #  RGASD/CPD
            t.tgl       <-  0.2731600000000E+03; #  TEMP GLACE DANS ATM   K
            t.consol    <-  0.1367000000000E+04; #  CONSTANTE SOLAIRE     W M-2
            t.grav      <-  0.9806160000000E+01; #  ACC. DE GRAVITE       M S-2
            t.rayt      <-  0.6371220000000E+07; #  RAYON MOY. TERRE      M
            t.stefan    <-  0.5669800000000E-07; #  CTE STEFAN-BOLTZMANN  J M-2 S-1 K-4
            t.pi        <-  0.3141592653590E+01; #  CTE PI=ACOS(-1)
            t.omega     <-  0.7292000000000E-04; #  ROTATION TERRE        S-1
            t.knams     <-  0.5147910000000E+00; #  PASSAGE KT A M/S
            t.stlo      <-  0.6628486583943E-03; #  SCHUMAN-NEWELL L.R.   K S2 M-2
            t.karman    <-  0.3500000000000E+00; #  CTE DE VON KARMAN
            t.ric       <-  0.2000000000000E+00; #  CTE RICHARDSON CRI
            t.chlc      <-  0.2501000000000E+07; #  CH. LA CONDENS.     J KG-1
            t.chlf      <-  0.3340000000000E+06; #  CH. LA FUSION       J KG-1
            t.t1s       <-  0.2731600000000E+03; #  POUR FN HTVOCP        K
            t.t2s       <-  0.2581600000000E+03; #  POUR FN HTVOCP        K
            t.aw        <-  0.3135012829948E+04; #  POUR FN HTVOCP        A VENIR
            t.bw        <-  0.2367075766316E+01; #  POUR FN HTVOCP        A VENIR
            t.ai        <-  0.2864887713087E+04; #  POUR FN HTVOCP        A VENIR
            t.bi        <-  0.1660931315020E+00; #  POUR FN HTVOCP        A VENIR
            t.slp       <-  0.6666666666667E-01; #  POUR FN HTVOCP        A VENIR
            t.gamma_d   <-  0.286              ; #  AJOUTE PAR ANDRE PLANTE MAI 2005 POUR TEPHI

            #----- Params
            p0theta=1000.
            cw=4218.
            ew0=6.1058
            c2=-(t.cpv-cw)/t.rgasv
            c1=t.chlc/t.rgasv+c2*t.tcdk
            c0=log(ew0)+c1/t.tcdk+c2*log(t.tcdk)
            angle=45
            xscale=log(c(230,400))
            #yscale=c(16,-90)
            yscale=c(20,-90)
            vp=viewport(name="tephi.plot",xscale=xscale,yscale=yscale,width=1.,height=1.,angle=angle)

            #----- Runtime vars
            grobs=NULL
            hlgrobs=NULL
            vpz=NULL
            curdata=NULL
            tcol=NULL
            dcol=NULL

            #----- Function to calculate a pressure (hPa), given an ln(thetha) (x pos) and a temperature (°C,y pos)
            tephi.lnthetat2p <- function(lntheta,t) {
                p0theta/exp((lntheta-log(t+t.tcdk))/t.cappa)
            }

            #----- Function to calculate an ln(thetha) (x pos), given a pressure (hPa) and a temperature (°C,y pos)
            tephi.pt2lntheta <- function(p,t) {
                log(t+t.tcdk)+t.cappa*log(p0theta/p)
            }

            #----- Function to calculate a temperature (°C,y pos), given an ln(thetha) (x pos) and a pressure (hPa)
            tephi.lnthetap2t <- function(lntheta,p) {
                exp(lntheta-t.cappa*log(p0theta/p))-t.tcdk
            }

            #----- Function to calculate a theta (°C,y pos), given a theta (exp(x pos)) and a pressure (hPa)
            tephi.pt2theta <- function(p,t) {
                exp(tephi.pt2lntheta(p,t))
            }

            #----- Function to calculate a temperature (°C,y pos), given a theta (exp(x pos)) and a pressure (hPa)
            tephi.thetap2t <- function(theta,p) {
                tephi.lnthetap2t(log(theta),p)
            }

            #----- Function to calculate the position in the graph of a given pressure on a vertical line
            #----- pres: the pressure to convert into a position
            #----- pref: the reference pressure for the vertical line
            #----- tref: the temperature (°C) corresponding to the reference pressure
            tephi.p2constxpos <- function(pres,pref,tref) {
                # Define a 45 degree slop on graph (delat ln(tetha))/delta(T)
                # to plot the wind barbs
                slop=(-70-0)/(log(350/270))
                lntho_pref=tephi.pt2lntheta(pref,tref)
                ordonee=tref-slop*lntho_pref
                theta=320

                mtx <- sapply(pres,function(p){
                    exner=(p/p0theta)^t.cappa
                    # Methode de Raphson-Newton, converge en 3 iterations au centieme
                    for (i in seq(1,100)) {
                        ff=slop*log(theta)-exner*theta+ordonee+t.tcdk
                        ffp=slop/theta -exner
                        thetaP=theta-ff/ffp
                        dif=abs(thetaP-theta)
                        theta=thetaP
                        if(dif < .01){break}
                    }
                    return(c(log(theta),exner*theta-t.tcdk))
                })
                return(list(x=unit(mtx[1,],"native"),y=unit(mtx[2,],"native")))
            }

            tephi.draw <- function() {
                grid.newpage()

                if( !is.null(grobs) ) {
                    if( is.null(vpz) ) {
                        # There seems to be a bug where the first draw errors out
                        tryCatch(grid.draw(grobs),error=function(cond)tephi.draw())
                        if( !is.null(hlgrobs) )
                            grid.draw(hlgrobs)
                    } else {
                        pushViewport(vpz)
                        grid.draw(grobs)
                        if( !is.null(hlgrobs) )
                            grid.draw(hlgrobs)
                        popViewport()
                    }
                }

                invisible(NULL)
            }

            tephi.plot <- function(
                data,
                showWinds=FALSE,
                showElev=FALSE,
                title=NULL,
                title2=NULL,
                title3=NULL,
                title4=NULL,
                barbs_thick=1.5,
                barbs_position_at_1000_hPa=40,
                barbs_length=0.05,
                temp.col='red',
                dewpt.col='blue',
                iso.col='green',
                pres.col='green',
                adi.col='orange',
                rs.col='orange',
                theta.col='orange',
                wind.col='red',
                title.col='black',
                title2.col='black',
                title3.col='black',
                title4.col='black',
                iso.fontsize=12,
                iso.fontface=1,
                iso.fontfamily="",
                pres.fontsize=12,
                pres.fontface=1,
                pres.fontfamily="",
                rs.fontsize=12,
                rs.fontface=1,
                rs.fontfamily="",
                theta.fontsize=12,
                theta.fontface=1,
                theta.fontfamily="",
                title.fontsize=18,
                title.fontface=1,
                title.fontfamily="",
                title2.fontsize=16,
                title2.fontface=1,
                title2.fontfamily="",
                title3.fontsize=16,
                title3.fontface=1,
                title3.fontfamily="",
                title4.fontsize=16,
                title4.fontface=1,
                title4.fontfamily=""
            ) {
                grobs <<- NULL
                hlgrobs <<- NULL
                curdata <<- data
                tcol <<- temp.col
                dcol <<- dewpt.col

                #------------------------------------------------------------------------------
                # Pour tracer isotermes et theta
                trace_tmin=min(-70,floor(min(data$temp-data$spread)/5.0)*5.0)
                trace_tmax=max(50,ceiling(max(data$temp)/5.0)*5.0)
                trace_pmax=1050
                trace_pmin=100
                #==============================================================================
                # Tracer les pseudo-adiabatiques saturees [Iribarne, p143, formule (85)]
                ts_liste=(0:22)*4-40+t.tcdk
                adix <- c()
                adiy <- c()
                adic <- c()
                for (ts in ts_liste) {
                    ews=exp(c0-c1/ts-c2*log(ts))
                    rws=t.eps1*ews/(p0theta-ews)
                    lvs=t.chlc+(t.cpv-cw)*(ts-t.tcdk)
                    Const=t.cpd*log(ts)-t.rgasd*log(p0theta-ews)+rws*lvs/ts
                    lx <- c(log(ts))
                    ly <- c(ts-t.tcdk)
                    for (dir in c(1,-1)){
                        step=dir*20
                        if (step > 0) {
                            pfin=trace_pmax
                        } else {
                            pfin=trace_pmin
                        }
                        t=ts
                        srw=0.
                        t1=ts-t.tcdk
                        theta1=ts
                        pm=1000
                        for (p in seq(1000+step,pfin,step)) {
                            tm=t
                            ewm=exp(c0-c1/tm-c2*log(tm))
                            rwm=t.eps1*ewm/(p-ewm)
                            for ( j in 1:2) {
                                ew=exp(c0-c1/t-c2*log(t))
                                rw=t.eps1*ew/(p-ew)
                                lv=t.chlc+(t.cpv-cw)*(t-t.tcdk)
                                dew=ew*(c1/t^2-c2/t)
                                drw=t.eps1*dew*p/(p-ew)^2
                                dlv=t.cpv-cw
                                f_t=t.cpd*log(t)+cw*(srw+rwm*log(t/tm))-t.rgasd*log(p-ew)+rw*lv/t-Const
                                fp_t=(t.cpd+cw*rwm)/t+t.rgasd/(p-ew)*dew+rw*lv/t*(drw/rw+dlv/lv-1/t)
                                t=t-f_t/fp_t
                            }
                            srw=srw+rwm*log(t/tm)
                            t2=t-t.tcdk
                            theta2=tephi.pt2theta(p,t2)
                            if (t2 < -50) {
                                aa=(theta1-theta2)/(t1-t2)
                                bb=theta1-aa*t1
                                t2=-50
                                theta2=aa*t2+bb
                                stop=1
                                lx <- c(lx,log(theta2))
                                ly <- c(ly,t2)
                                break
                            }
                            lx <- c(lx,log(theta2))
                            ly <- c(ly,t2)
                            t1=t2
                            theta1=theta2
                            pm=p
                        }
                    }
                    o <- order(lx)
                    adix <- c(adix,lx[o])
                    adiy <- c(adiy,ly[o])
                    adic <- c(adic,length(o))
                }
                grobs <<- gList(grobs,polylineGrob(x=adix,y=adiy,id.lengths=adic,gp=gpar(col=adi.col),default.units="native",vp=vp))
                #==============================================================================
                # Tracer les rs eau avec formule empirique pour ew Iribarne pp 68 eq. (53)
                gp<-gpar(col=rs.col,fontsize=rs.fontsize,fontface=rs.fontface,fontfamily=rs.fontfamily,lwd=1)
                rs_liste=c(.02,.03,.05,.15,.2,.3,.4,.6,.8,1.,1.5,2.,3.,4.,5.,6.,7.,8.,9.,10.,12.,14.,16.,18.,20.,25.,30.,35.,40.,50.)
                #%%%%%%%%%%%%%%%%%%%%%%%%%%%
                #%%logew=c0-c1/T+c2*log(T)%%
                #%%%%%%%%%%%%%%%%%%%%%%%%%%%
                rsp2t <- function(rs,p) {
                    const=log(p/(1.+t.eps1/rs))
                    t=t.tcdk
                    for ( j in 1:10){
                        f_t=const-c0+c1/t+c2*log(t)
                        fp_t=-c1/t^2+c2/t
                        t=t-f_t/fp_t
                    }
                    return(t-t.tcdk)
                }
                # Tracer les lignes rs
                rsx <- c()
                rsy <- c()
                rsc <- c()
                for (rs in rs_liste*1.e-3){
                    t <- rsp2t(rs,trace_pmax)
                    if( t > -40 ) {
                        tp <- seq(-40,t,5)+t.tcdk
                        ew <- exp(c0-c1/tp-c2*log(tp))
                        p <- ew*(1.+t.eps1/rs)
                        lntheta <- c(tephi.pt2lntheta(p,tp-t.tcdk),tephi.pt2lntheta(trace_pmax,t))
                        rsx <- c(rsx,lntheta)
                        rsy <- c(rsy,c(tp-t.tcdk,t))
                        rsc <- c(rsc,length(lntheta))
                    }
                }
                grobs <<- gList(grobs,polylineGrob(x=rsx,y=rsy,id.lengths=rsc,gp=gp,default.units ="native",vp=vp))
                # Mettre les valeurs de rs le long de la pression p
                #p <- 1080.
                p <- 1120.
                t <- unlist(lapply(rs_liste*1e-3,rsp2t,p))
                lntheta <- tephi.pt2lntheta(p,t)
                grobs <<- gList(grobs,textGrob(paste(rs_liste),x=lntheta,y=t,gp=gp,default.units ="native",just = "center",rot = -angle,vp=vp))
                #==============================================================================
                # Tracer les theta
                gp <- gpar(col=theta.col,fontsize=theta.fontsize,fontface=theta.fontface,fontfamily=theta.fontfamily)
                vtheta <- seq(200,400,5)
                lntheta <- log(vtheta)
                tt <- tephi.lnthetap2t(lntheta,trace_pmax)
                grobs <<- gList(grobs,polylineGrob(x=rep(lntheta,each=2),y=c(rbind(rep_len(trace_tmin,length(tt)),tt)),id.lengths=rep.int(2,length(tt)),gp=gp,default.units ="native",vp=vp))
                # Metre les valeurs de theta le long de l'isotherme tt
                tt <- -25
                vtheta <- vtheta[vtheta >= tephi.pt2theta(trace_pmax,tt)]
                grobs <<- gList(grobs,textGrob(paste(vtheta),x=log(vtheta),y=tt,gp=gp,default.units ="native",rot = -90,just = "bottom",vp=vp))
                #==============================================================================
                # Tracer les isothermes
                tt <- seq(trace_tmin,trace_tmax,1)
                lwd <- rep.int(.3,length(tt)); lwd[tt%%5==0]<-1; lwd[tt==0]<-4
                lntheta <- lapply(tt,function(ii){c(tephi.pt2lntheta(trace_pmax,ii),log(500))})
                grobs <<- gList(grobs,polylineGrob(x=unlist(lntheta),y=rep(tt,each=2),id.lengths=rep.int(2,length(tt)),gp=gpar(col=iso.col,lwd=lwd),default.units="native",vp=vp))
                # Tracer les valeurs de T le long de theta=vtheta
                gp <- gpar(col=iso.col,fontsize=iso.fontsize,fontface=iso.fontface,fontfamily=iso.fontfamily)
                vtheta=305
                tt <- seq(trace_tmin,tephi.thetap2t(vtheta,trace_pmax),5)
                grobs <<- gList(grobs,textGrob(paste(tt),x=log(vtheta),y=tt,gp=gp,default.units ="native",just = "bottom",vp=vp))
                #==============================================================================
                # Tracer les valeurs de T le long de p trace_pmax
                tt <- seq(trace_tmin,trace_tmax,5)
                lntheta=tephi.pt2lntheta(trace_pmax,tt)
                #grobs <<- gList(grobs,textGrob(paste(tt,"  ",sep=""),x=lntheta,y=tt,gp=gp,default.units ="native",just="right",vp=vp))
                grobs <<- gList(grobs,textGrob(tt,x=lntheta,y=tt,gp=gp,default.units ="native",just="right",vp=vp))
                #==============================================================================
                # Tracer p (Equation 1 pp 100 Iribarne)
                pp <- seq(trace_pmax,trace_pmin,-50.)
                tt <- seq(trace_tmin,trace_tmax,5)
                gp <- gpar(col=pres.col,fontsize=pres.fontsize,fontface=pres.fontface,fontfamily=pres.fontfamily,lwd=(pp%in%c(250,500,700,850,1000))*1.5+1)
                lntheta <- lapply(pp,tephi.pt2lntheta,tt)
                grobs <<- gList(grobs,polylineGrob(x=unlist(lntheta),y=rep.int(tt,length(pp)),id.lengths=rep.int(length(tt),length(pp)),gp=gp,default.units="native",vp=vp))
                # Tracer les valeurs de p le long de l'isotherme tt
                tt <- -50
                lntheta <- tephi.pt2lntheta(pp,tt)
                grobs <<- gList(grobs,textGrob(paste(pp),x=lntheta,y=tt,gp=gp,default.units ="native",rot=-angle,just = "bottom",vp=vp))
                # Tracer les valeur de p en metre
                if( showElev && !is.null(data$elev) ) {
                    pos <- tephi.p2constxpos(data$pres,850,45)
                    grobs <<- gList(grobs,textGrob(sprintf('%.0f m_',data$elev),x=pos$x,y=pos$y,gp=gp,just=c('right','bottom'),rot=-angle,check.overlap=TRUE,vp=vp))
                }
                #==============================================================================
                # Tracer les profiles
                nb=min(length(data$temp),length(data$spread))
                lntheta=tephi.pt2lntheta(data$pres,data$temp)
                grobs <<- gList(grobs,linesGrob(x=lntheta,y=data$temp,gp=gpar(col=temp.col,lwd=3),default.units ="native",vp=vp))
                lntheta=tephi.pt2lntheta(data$pres[1:nb],data$temp[1:nb]-data$spread[1:nb])
                grobs <<- gList(grobs,linesGrob(x=lntheta,y=data$temp[1:nb]-data$spread[1:nb],gp=gpar(col=dewpt.col,lwd=3),default.units ="native",vp=vp))
                #==============================================================================
                # Tracer les vents
                if ( showWinds && is.numeric(data$wspd) && is.numeric(data$wdir) ) {
                    #(0,wy0) \   \
                    #         \   \
                    #          \wxd\
                    #           -------------
                    #       (wx0,0)        (1,0)
                    ###########################
                    aspect <- dev.size()
                    aspect <- aspect[1]/aspect[2]
                    gpp <- gpar(col=wind.col,fill=wind.col)
                    gpl <- gpar(col=wind.col,lwd=barbs_thick)
                    pos <- tephi.p2constxpos(data$pres,1000,barbs_position_at_1000_hPa)
                    for (k in seq(1,length(data$wdir))) {
                        #@@@@@@@@@@@@@@@
                        vpw <- viewport(name=paste("tephi.wnd",k,sep="."),just=c('right','bottom'),x=pos$x[k],y=pos$y[k],xscale=c(0,1),yscale=c(0,1),width=barbs_length/aspect,height=barbs_length,angle=-data$wdir[k]+270-angle)
                        vplst <- vpStack(vp,vpw)
                        #@@@@@@@@@@@@@@@
                        wx0=0.15
                        wy0=0.25
                        wxd=.11
                        xx=wx0
                        wmod=data$wspd[k]
                        # check if we have more than a circle
                        if( wmod >= 2.5 ) {
                            # corps de la barbule
                            wx <- c(wx0,1)
                            wy <- c(0,0)
                            # draw 50 knots flags
                            while( wmod >= 50 ) {
                                grobs <<- gList(grobs,polygonGrob(x=c(xx,xx-wx0/3,xx+wxd*.85),y=c(0,wy0,0),gp=gpp,vp=vplst))
                                xx <- xx+wxd
                                wmod <- wmod-50
                            }
                            # Extra space after the polygons
                            if( xx != wx0 )
                                xx <- xx+wxd
                            # compute 10 knots barbs
                            while( wmod >= 10 ) {
                                wx <- c(wx,xx,xx-wx0)
                                wy <- c(wy,0,wy0)
                                xx <- xx+wxd
                                wmod = wmod-10
                            }
                            # compute 5 knots barbs
                            if( wmod >= 2.5 ) {
                                if(xx==wx0){xx=xx+wxd}
                                wx <- c(wx,xx,xx-wx0/2)
                                wy <- c(wy,0,wy0/2)
                            }
                            # draw body, 10 knots and 5 knots lines
                            grobs <<- gList(grobs,polylineGrob(x=wx,y=wy,id.lengths=rep_len(2,length(wx)/2),gp=gpl,default.units ="npc",vp=vplst))
                        } else {
                            # draw the circle
                            grobs <<- gList(grobs,pointsGrob(x=1,y=0,gp=gpl,default.units ="npc",vp=vplst))
                        }
                    }
                }
                #==============================================================================
                if(is.character(title)  && title!="")   grobs<<-gList(grobs,textGrob(title ,x=.02,y=.98,gp=gpar(col=title.col,fontsize=title.fontsize,fontface=title.fontface),default.units ="npc",just="left"))
                if(is.character(title2) && title2!="")  grobs<<-gList(grobs,textGrob(title2,x=.02,y=.94,gp=gpar(col=title2.col,fontsize=title2.fontsize,fontface=title2.fontface),default.units ="npc",just="left"))
                if(is.character(title3) && title3!="")  grobs<<-gList(grobs,textGrob(title3,x=.02,y=.90,gp=gpar(col=title3.col,fontsize=title3.fontsize,fontface=title3.fontface),default.units ="npc",just="left"))
                if(is.character(title4) && title4!="")  grobs<<-gList(grobs,textGrob(title4,x=.02,y=.86,gp=gpar(col=title4.col,fontsize=title4.fontsize,fontface=title4.fontface),default.units ="npc",just="left"))
                #==============================================================================
                tephi.draw()
            }

            tephi.date <- function(data,date,ptop,...) {
                if( is.character(date) ) {
                    date <- as.POSIXct(date,tz="GMT",format="%Y%m%d%H%M")
                }
                data <- data[data$date==date & data$pres>=ptop,]
                tephi.plot(data,...)
            }
            
            tephi.pick <- function(x,y,vp,units="npc",valueOnly=TRUE) {
                if( is.null(grobs) )
                    return(NULL)
                size <- dev.size("px")
                if( x>=0 && y>=0 && x<=size[1] && y<=size[2] ) {
                    if( is.null(vp) ) {
                        #----- No given viewport, just convert the coords to the given units
                        x <- convertX(unit(x,"native"),units,valueOnly=TRUE)
                        y <- convertY(unit(y,"native"),units,valueOnly=TRUE)
                    } else {
                        #----- Make sure we are at the root viewport
                        while( !is.null(current.parent()) ) {
                            popViewport(recording=FALSE)
                        }
                        #seekViewport("ROOT",recording=FALSE)
                        #----- Get the coordinates from native (pixels) to inches (yes, we need inches) in that root viewport
                        x <- convertX(unit(x,"native"),"inches",valueOnly=TRUE)
                        y <- convertY(unit(y,"native"),"inches",valueOnly=TRUE)
                        #----- Push our specific viewport
                        pushViewport(vp,recording=FALSE)
                        #----- Make the inverse transform (for whatever reason, the units of the transform are in inches)
                        xy <- c(x,y,1) %*% solve(current.transform())
                        xy <- unit(xy/xy[3],"inches")
                        #----- Convert the units from inches to user-specified in the current viewport (native means relative to our xscale/yscale)
                        x <- convertX(xy[1],units,valueOnly=TRUE)
                        y <- convertY(xy[2],units,valueOnly=TRUE)
                        #----- Remove the viewport we pushed
                        popViewport(recording=FALSE)
                    }
                    if( valueOnly ) {
                        return(c(x,y))
                    } else {
                        return(unit(c(x,y),units))
                    }
                }
                return(NULL)
            }

            tephi.pick2val <- function(x,y) {
                if( is.null(grobs) )
                    return(NULL)
                if( is.null(vpz) ) {
                    tephi.pick(x,y,vp,"native")
                } else {
                    xy <- tephi.pick(x,y,vpz,"npc",valueOnly=FALSE)
                    if( is.null(xy) ) {
                        return(NULL)
                    } else {
                        #----- Push the plotting viewport
                        pushViewport(vp,recording=FALSE)
                        #----- Get the coordinates from npc to inches (yes, we need inches) in that root viewport
                        x <- convertX(xy[1],"inches",valueOnly=TRUE)
                        y <- convertY(xy[2],"inches",valueOnly=TRUE)
                        #----- Make the inverse transform (for whatever reason, the units of the transform are in inches)
                        xy <- c(x,y,1) %*% solve(current.transform())
                        xy <- unit(xy/xy[3],"inches")
                        #----- Convert the units from inches to user-specified in the current viewport (native means relative to our xscale/yscale)
                        x <- convertX(xy[1],"native",valueOnly=TRUE)
                        y <- convertY(xy[2],"native",valueOnly=TRUE)
                        #----- Remove the viewport we pushed
                        popViewport(recording=FALSE)
                        return(c(x,y))
                    }
                }
            }

            tephi.details <- function(x,y) {
                #----- Convert the coord from pixels to plot scale
                xy <- tephi.pick2val(x,y)
                if( is.null(xy) )
                    return(NULL)

                x <- xy[1]
                y <- xy[2]
                
                #----- Calculate the pressure
                p <- tephi.lnthetat2p(x,y)

                if( !is.null(curdata) ) {
                    #----- Get the closest index in our data for that pressure
                    idx <- which.min(abs(curdata$pres-p))
                    return(list(iso=y,theta=exp(x),p=p,pres=curdata$pres[idx],temp=curdata$temp[idx],spread=curdata$spread[idx],
                        dewpt=curdata$temp[idx]-curdata$spread[idx],elev=curdata$elev[idx],wspd=curdata$wspd[idx],wdir=curdata$wdir[idx],idx=idx))
                } else {
                    return(list(iso=y,theta=exp(x),p=p,pres=NULL,temp=NULL,spread=NULL,dewpt=NULL,elev=NULL,idx=NULL))
                }
            }

            tephi.highlight <- function(idx=0) {
                if( !is.null(curdata) && idx>0 && idx<=length(curdata$pres) ) {
                    p <- curdata$pres[idx]
                    #----- Temperature
                    t <- curdata$temp[idx]
                    temp <- pointsGrob(x=tephi.pt2lntheta(p,t),y=t,pch=21,size=unit(1.2,"char"),vp=vp,gp=gpar(col='black',fill=tcol,lwd=2))
                    #----- Dew point
                    t <- t-curdata$spread[idx]
                    dewpt <- pointsGrob(x=tephi.pt2lntheta(p,t),y=t,pch=21,size=unit(1.2,"char"),vp=vp,gp=gpar(col='black',fill=dcol,lwd=2))
                    #----- Draw
                    hlgrobs <<- gList(temp,dewpt)
                    tephi.draw()
                } else {
                    #----- Reset highlighting
                    if( !is.null(hlgrobs) ) {
                        hlgrobs <<- NULL
                        tephi.draw()
                    }
                }
            }

            tephi.view <- function(x,y,zoom) {
                #----- Make sure we don't get out of the graph's border
                #----- Since we're dealing with npc and have xscale=yscale=[0,1], we're out if we are outside the [0,1] range
                if( zoom < 1 ) zoom=1
                dlt <- 0.5/zoom
                if( x-dlt < 0 ) x=dlt
                if( y-dlt < 0 ) y=dlt
                if( x+dlt > 1 ) x=1-dlt
                if( y+dlt > 1 ) y=1-dlt
                #----- Draw the plot
                if( zoom==1 && x==0.5 && y==0.5 ) {
                    if( !is.null(vpz) ) {
                        vpz <<- NULL
                        tephi.draw()
                    }
                } else {
                    x <- 0.5-zoom*x
                    y <- 0.5-zoom*y
                    vpz <<- viewport(name="tephi.vp",x=x,y=y,width=zoom,height=zoom,just=c('left','bottom'))
                    tephi.draw()
                }
            }

            tephi.getZoom <- function() {
                if( is.null(vpz) ) {
                    return(1)
                } else {
                    as.numeric(vpz$width)
                }
            }

            tephi.getPos <- function() {
                if( is.null(vpz) ) {
                    return(c(0.5,0.5))
                } else {
                    x <- as.numeric(vpz$x)
                    y <- as.numeric(vpz$y)
                    zoom <- as.numeric(vpz$width)
                    x <- (0.5-x)/zoom
                    y <- (0.5-y)/zoom
                    return(c(x,y))
                }
            }

            tephi.zoomBox <- function(x0,y0,x1,y1) {
                if( is.null(grobs) )
                    invisible(NULL)
                xy0 <- tephi.pick(x0,y0,vpz,"npc")
                xy1 <- tephi.pick(x1,y1,vpz,"npc")
                zoom <- max(1/max(xy1[1]-xy0[1],xy1[2]-xy0[2]),1)
                tephi.view((xy0[1]+xy1[1])*0.5,(xy0[2]+xy1[2])*0.5,zoom)
            }

            tephi.zoomIncr <- function(x,y,incr) {
                if( is.null(grobs) )
                    invisible(NULL)
                xy <- tephi.pick(x,y,vpz,"npc")
                tephi.view(xy[1],xy[2],incr*tephi.getZoom())
            }

            tephi.reset <- function() {
                vpz <<- NULL
                tephi.draw()
            }

            tephi.drag <- function(dx,dy) {
                if( is.null(grobs) )
                    invisible(NULL)
                #----- Get the delta in percentage from the screen, adjusted for the zoom factor
                zoom <- tephi.getZoom()
                dxy <- c(dx,dy)/dev.size('px')/zoom
                dxy <- tephi.getPos()-dxy
                tephi.view(dxy[1],dxy[2],zoom)
            }

            return(list(plot=tephi.plot,date=tephi.date,details=tephi.details,hl=tephi.highlight,pick=tephi.pick2val,
                zoomBox=tephi.zoomBox,zoomIncr=tephi.zoomIncr,reset=tephi.reset,drag=tephi.drag,redraw=tephi.draw))
        }
    }
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Tephi::New>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors de la création d'un graph
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <Var>    : Variable contenant les données du graph
#
# Retour:
#
# Remarques : Optionnel
#
#----------------------------------------------------------------------------
proc RGraph::Tephi::New { Id Var } {
    global GDefs
    variable Param
    upvar #0 $Var graph

    set graph(Winds)        FALSE
    set graph(ColTemp)      "#FF0000"
    set graph(ColDewpt)     "#0000FF"
    set graph(ColIso)       "#000000"
    set graph(ColPres)      "#31B531"
    set graph(ColAdi)       "#FF5B14"
    set graph(ColRS)        "#273D8C"
    set graph(ColTheta)     "#FFA500"
    set graph(ColWind)      "#000000"
    set graph(ColDate)      "#000000"
    set graph(ColInfo)      "#000000"
    set graph(ColLoc)       "#000000"
    set graph(ColDateSeq)   "#000000"
    set graph(Info)         [lindex $Param(Title) $GDefs(Lang)]
    set graph(PTop)         200
    set graph(PTopData)     200

    font create RGraph${Id}Iso      -family courier -size 12 -weight bold
    font create RGraph${Id}Pres     -family courier -size 15 -weight bold
    font create RGraph${Id}RS       -family courier -size 12 -weight bold
    font create RGraph${Id}Theta    -family courier -size 12 -weight bold
    font create RGraph${Id}Date     -family courier -size 18 -weight bold
    font create RGraph${Id}Info     -family courier -size 16 -weight bold
    font create RGraph${Id}Loc      -family courier -size 16 -weight bold
    font create RGraph${Id}DateSeq  -family courier -size 16 -weight bold

    #----- Add the bindings to switch between dates
    set toplvl [winfo toplevel $RGraph::Data(Graph$Id)]
    bind $toplvl <Key-Left> [list ::RGraph::Tephi::DateIdx $Id $Var -1]
    bind $toplvl <Key-Right> [list ::RGraph::Tephi::DateIdx $Id $Var +1]
    bind $toplvl <Shift-Left> [list ::RGraph::Tephi::DateIdx $Id $Var 0]
    bind $toplvl <Shift-Right> [list ::RGraph::Tephi::DateIdx $Id $Var end]
    trace add variable ${Var}(DateIdx) write "RGraph::Tephi::OnDateIdxChange $Id $Var ;#next args are comments#"

    #----- Widgets for graph info
    set c $RGraph::Data(Canvas$Id)
    frame $c.title -background #FFFFFF
        label $c.title.date -textvariable ${Var}(Date) -bg #FFFFFF -font RGraph${Id}Date -anchor w
        entry $c.title.info -textvariable ${Var}(Info) -bg #FFFFFF -font RGraph${Id}Info -relief flat
        label $c.title.loc -textvariable ${Var}(Loc) -bg #FFFFFF -font RGraph${Id}Loc -anchor w
        label $c.title.dateseq -textvariable ${Var}(DateSeq) -bg #FFFFFF -font RGraph${Id}DateSeq -anchor w
        pack $c.title.date $c.title.info $c.title.loc $c.title.dateseq -side top -padx 5 -pady 5 -fill x -expand 1 -anchor w
    $c create window 0 0 -anchor nw -window $c.title
    #----- Prevent focus stealing
    bind $c.title.info <Key-Return> "focus $c"
    $c bind $Id <Button-1> "+focus $c ;#Tephi"
    focus $c

    #----- There seem to be a slight problem with the display list with the way the tephi deals with its grobs
    #----- Which leads to refresh problems. This should take care of this.
    trace add execution RGraph::Resize leave "R exec {if(!exists('tephi$Id'))tephi$Id<-tephi();tephi$Id\$redraw()} ;#Tephi"

    #----- Reset the highlighting whenever we leave the canvas
    $c bind $Id <Leave> "+R exec {if(exists('tephi$Id'))tephi$Id\$hl()} ;#Tephi"
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Tephi::ParamsWindow>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback pour la création de l'interface spécifique à ce type de graph
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <W>      : Frame où ajouter les widgets
#  <Var>    : Variable contenant les données du graph
#
# Retour:
#
# Remarques : Optionnel
#
#----------------------------------------------------------------------------
proc RGraph::Tephi::ParamsWindow { Id W Var } {
    global GDefs
    variable Lbl

    #----- Winds toggle
    frame [set f $W.winds]
        label $f.l -text [lindex $Lbl(Winds) $GDefs(Lang)] -anchor w
        checkbutton $f.b -indicatoron 1 -variable ${Var}(Winds) -command [list RGraph::UpdateGraph $Id 1] -onvalue TRUE -offvalue FALSE
        pack $f.l $f.b -side left
    pack $W.winds -side top -anchor w

    #----- Colors + Font
    foreach key {Date Info Loc DateSeq Iso Pres RS Theta} {
        frame [set f $W.[string tolower $key]]
            label $f.l -text [lindex $Lbl($key) $GDefs(Lang)] -anchor w
            ColorBox::CreateSel $f.col ${Var}(Col$key) [list RGraph::UpdateGraph $Id 1]
            button $f.font -bitmap @$GDefs(Dir)/share/bitmap/font.ico -relief groove \
                -command [list FontBox::Create $f.font "RGraph::UpdateGraph $Id 1" RGraph$Id$key]
            pack $f.l $f.col $f.font -side left
        pack $f -side top -anchor w
    }

    #----- Colors
    foreach key {Temp Dewpt Adi Wind} {
        frame [set f $W.[string tolower $key]]
            label $f.l -text [lindex $Lbl($key) $GDefs(Lang)] -anchor w
            ColorBox::CreateSel $f.col ${Var}(Col$key) [list RGraph::UpdateGraph $Id 1]
            pack $f.l $f.col -side left
        pack $f -side top -anchor w
    }

    #----- Info
    frame [set f $W.title2txt]
        label $f.l -text [lindex $Lbl(Info) $GDefs(Lang)] -anchor w
        entry $f.e -textvariable ${Var}(Info) -relief sunken -bd 1 -bg $GDefs(ColorLight) -width 1
        pack $f.l -side left
        pack $f.e -side left -fill x -expand 1
    pack $f -side top -fill x
    bind $f.e <Key-Return> [list RGraph::UpdateGraph $Id 1]
    
    #----- Top pressure
    frame [set f $W.ptop]
        label $f.l -text [lindex $Lbl(PTop) $GDefs(Lang)] -anchor w
        entry $f.e -textvariable ${Var}(PTop) -relief sunken -bd 1 -bg $GDefs(ColorLight) -width 1 -validate key -validatecommand "string is integer %P"
        pack $f.l -side left
        pack $f.e -side left -fill x -expand 1
    pack $f -side top -fill x
    bind $f.e <Key-Return> "if {\$RGraph::Data(DataValid$Id) } { RGraph::UpdateGraph $Id \[expr {\$${Var}(PTop)>=\$${Var}(PTopData)}\] }"
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Tephi::ProcessFieldList>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Parcours la liste de champs et rempli les array avec les champs
#            voulus pour toute les dates
#
# Parametres :
#  <Flds>   : Liste des champs à par
#  <Ip1s>   : Array contenant les ip1 pour une date donnée
#  <Keys>   : Array contenant les informations des champs pour une clé
#             date_ip1_var
#
# Retour    : Nombre total de niveaux valides
#
# Remarques : Nécessaire
#
#----------------------------------------------------------------------------
proc RGraph::Tephi::SortIP { IPa IPb } {
    lassign [fstdgrid convip $IPa] la ua ta
    lassign [fstdgrid convip $IPb] lb ub tb
    
    if { $ta != $tb } {
        return -code error "$IPa has type $ta whereas $IPb has type $tb"
    }

    if { $la < $lb } {
        set res -1
    } elseif { $la > $lb } {
        set res 1
    } else {
        set res 0
    }

    if { $ua in {sg mb hy th} } {
        set res [expr -1*$res]
    }

    return $res
}
proc RGraph::Tephi::ProcessFieldList { Flds Ip1s Keys } {
    upvar $Ip1s ip1s
    upvar $Keys keys

    unset -nocomplain ip1s
    unset -nocomplain keys

    if { ![llength $Flds] } {
        return 0
    }

    #----- Only keep the vars we are interested in and count them
    foreach fld $Flds {
        if { [lindex $fld 0] in {TT TD ES HR UU VV GZ P0} } {
            #----- Index the field
            if { [lindex $fld 0] == "P0" } {
                set keys([lindex $fld 9]_[lindex $fld 0]) [lrange $fld 10 11]
            } else {
                set keys([lindex $fld 9]_[lindex $fld 12]_[lindex $fld 0]) [lrange $fld 10 11]
                #----- Keep track of dates and ip1s
                if { [lindex $fld 0] == "TT" } {
                    lappend ip1s([lindex $fld 9]) [lindex $fld 12]
                }
            }
        }
    }
    
    set nb 0

    #----- Only keep dates where we have all the variables
    foreach date [array names ip1s] {
        #----- Check for missing fields at the levels
        set lvls {}
        foreach ip1 [lsort -integer -unique $ip1s($date)] {
            if { [info exists keys(${date}_${ip1}_UU)] && [info exists keys(${date}_${ip1}_VV)]
                 && ([info exists keys(${date}_${ip1}_TD)] || [info exists keys(${date}_${ip1}_ES)] || [info exists keys(${date}_${ip1}_HR)])
                 && [info exists keys(${date}_${ip1}_TT)] && [info exists keys(${date}_${ip1}_GZ)] } {
                lappend lvls $ip1
            } else {
                array unset keys ${date}_${ip1}_*
            }
        }
        #----- Only keep levels without missing fields
        if { [llength $lvls] } {
            set ip1s($date) [lsort -command RGraph::Tephi::SortIP $lvls]
            incr nb [llength $lvls]
        } else {
            array unset ip1s $date
        }
    }

    return $nb
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Tephi::GetData>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Collecte les données nécessaires au graph et les transfère à R
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <Var>    : Variable contenant les données du graph
#  <DataSrc>: Source pour les données
#
# Retour:
#
# Remarques : Nécessaire
#
#----------------------------------------------------------------------------
proc RGraph::Tephi::GetData { Id Var DataSrc } {
    global GDefs
    variable Param
    upvar #0 $Var graph

    lassign $DataSrc srctype srcargs

    set cols    [list date temp pres spread wspd wdir elev]
    set colsfmt {STRING 6 DOUBLE}

    set graph(Dates)    {}
    set graph(DateIdx)  ""
    set graph(Loc)      ""

    switch $srctype {
        RData {
            #----- Get the data from a previously generated R dump
            RGraph::DataLoad $Id $srcargs

            set graph(Dates)    [R exec -get "strftime(unique($Id\$date),tz=\"GMT\",format=\"%Y%m%d%H%M\")"]
            set graph(DateIdx)  0
            set graph(PTopData) [R exec -get "min($Id\$pres)"]

            set lat [R exec -get "attributes($Id)\$lat"]
            set lon [R exec -get "attributes($Id)\$lon"]
            if { $lat!="" && $lon!="" } {
                set graph(Loc)      [format "%.5f %.5f" $lat $lon]
            }
        }
        Dict {
            foreach key $cols {
                if { ![dict exists $srcargs $key] } {
                    return -code error "Key \"$key\" missing from given dictionnary"
                }
            }
            R tcl2r [dict set $srcargs attr.class data.frame] $Id
            R exec "$Id\$date <- as.POSIXct(as.character($Id\$date),tz=\"GMT\",format=\"%Y%m%d%H%M\")"
            R exec "$Id <- $Id\[order($Id\$date,$Id\$pres,decreasing=TRUE),\]"
            R exec "rownames($Id) <- seq(1,length($Id\$date))"

            set graph(Dates)    [R exec -get "strftime(unique($Id\$date),tz=\"GMT\",format=\"%Y%m%d%H%M\")"]
            set graph(DateIdx)  0
            set graph(PTopData) [R exec -get "min($Id\$pres)"]
            
            set lat [R exec -get "attributes($Id)\$lat"]
            set lon [R exec -get "attributes($Id)\$lon"]
            if { $lat!="" && $lon!="" } {
                set graph(Loc)      [format "%.5f %.5f" $lat $lon]
            }
        }
        Lst {
            if { [llength [lindex $srcargs 0]] != [llength $cols] } {
                return -code error "Invalid list, should have [llength $cols] items corresponding to the following columns : $cols"
            }
            R tcllst2rdf [list $cols $srcargs] $Id $colsfmt
            R exec "$Id\$date <- as.POSIXct($Id\$date,tz=\"GMT\",format=\"%Y%m%d%H%M\")"
            R exec "$Id <- $Id\[order($Id\$date,$Id\$pres,decreasing=TRUE),\]"
            R exec "rownames($Id) <- seq(1,length($Id\$date))"

            set graph(Dates)    [R exec -get "strftime(unique($Id\$date),tz=\"GMT\",format=\"%Y%m%d%H%M\")"]
            set graph(DateIdx)  0
            set graph(PTopData) [R exec -get "min($Id\$pres)"]
        }
        default {
            set msg "([lindex $Param(Title) $GDefs(Lang)]) [lindex $RGraph::Msg(ReadingFld) $GDefs(Lang)]"
            SPI::Progress 0 $msg

            set fids {}

            set untile [fstdfield autountile]
            fstdfield autountile 1

            try {
                #----- Fill the valid dates/levels/fields array
                switch $srctype {
                    FstdFiles {
                        set flds {}
                        foreach file $srcargs {
                            set fid RGRAPH$Id[incr i]
                            if { [catch {set meta [fstdfile open $fid read $file SPI]}] } {
                                Log::Print WARNING "Error while opening file \"$file\", it will be skipped"
                                fstdfile close $fid
                            } else {
                                lappend fids $fid
                                lappend flds {*}$meta
                            }
                        }
                        set nb [ProcessFieldList $flds ip1s keys]
                        if { !$nb } {
                            return -code error "No valid fields found in specified files"
                        }
                    }
                    FieldBox {
                        if { ![FieldBox::Exist $srcargs] } {
                            return -code error "\"$srcargs\" is not a valid FieldBox id"
                        }
                        set nb [ProcessFieldList [FieldBox::GetContent $srcargs] ip1s keys]
                        if { !$nb } {
                            return -code error "No valid fields found in specified fieldbox"
                        }
                    }
                    default {
                        #----- Loop on all open file boxes
                        set nb 0
                        foreach box [FieldBox::Get] {
                            #----- If something is selected in that box
                            if { [llength [set fld [lindex [FieldBox::GetSelected $box] 0]]] } {
                                set nb [ProcessFieldList [FieldBox::GetContent $box] ip1s keys]

                                #----- Check if we have valid dates
                                if { $nb } {
                                    break
                                }
                            }
                        }
                        if { !$nb } {
                            return -code error "No valid fields found in selected fieldboxes, no data will be shown"
                        }
                    }
                }
                SPI::Progress 5 $msg

                set lat $RGraph::Data(Lat$Id)
                set lon $RGraph::Data(Lon$Id)

                #----- Get the percentage each row processed brings
                set perc [expr {95.0/$nb}]

                #----- Loop on the dates
                set data {}
                foreach date [array names ip1s] {
                    #----- Read P0 if we have it
                    if { [info exists keys(${date}_P0)] } {
                        fstdfield read RGRAPHP0$Id {*}$keys(${date}_P0)
                    }

                    #----- Loop on the levels
                    set nb 0
                    foreach ip1 $ip1s($date) {
                        try {
                            set row [list $date]

                            #----- Get the temperature
                            fstdfield read RGRAPHFLD$Id {*}$keys(${date}_${ip1}_TT)
                            lappend row [fstdfield stats RGRAPHFLD$Id -coordvalue $lat $lon]

                            #----- Calculate the pressure field
                            fstdgrid pressure RGRAPHFLD$Id RGRAPHP0$Id
                            lappend row [fstdfield stats RGRAPHFLD$Id -coordvalue $lat $lon]
                            #----- The ip1s are in order from ground to sky, so all the following pressure values should be below our threshold
                            if { [lindex $row end]<$graph(PTop) } {
                                SPI::Progress +[expr $perc*([llength $ip1s($date)]-$nb-1)] $msg
                                break
                            }

                            #----- Read what we'll use for the dew point
                            if { [info exists keys(${date}_${ip1}_ES)] } {
                                fstdfield read RGRAPHFLD$Id {*}$keys(${date}_${ip1}_ES)
                                lappend row [fstdfield stats RGRAPHFLD$Id -coordvalue $lat $lon]
                            } elseif { [info exists keys(${date}_${ip1}_TD)] } {
                                fstdfield read RGRAPHFLD$Id {*}$keys(${date}_${ip1}_TD)
                                lappend row [expr {[lindex $row 1]-[fstdfield stats RGRAPHFLD$Id -coordvalue $lat $lon]}]
                            } else {
                                fstdfield read RGRAPHFLD$Id {*}$keys(${date}_${ip1}_HR)
                                set hr [fstdfield stats RGRAPHFLD$Id -coordvalue $lat $lon]
                                set tt [lindex $row 1]
                                #----- The procedure was found here : https://iridl.ldeo.columbia.edu/dochelp/QA/Basic/dewpoint.html
                                #----- RH = 100% * (E/Es)
                                #----- E = E0 * exp[(L/Rv)*{(1/T0)-(1/Td)}]
                                #----- Es = E0 * exp[(L/Rv)*{(1/T0)-(1/T)}]
                                #----- Where E0=0.611 kPa, (L/RV)=5423K, T0=273K
                                #----- Ergo: Td = 1/( (1/T) - ln(RH/100)/(L/Rv) )
                                lappend row [expr {$tt-(1./((1./($tt+273.15))-log($hr)/5423) - 273.15)}]
                            }

                            #----- Read the winds
                            fstdfield read RGRAPHFLD$Id {*}$keys(${date}_${ip1}_UU)
                            lappend row {*}[fstdfield stats RGRAPHFLD$Id -coordvalue $lat $lon]

                            #----- Read the elevation
                            fstdfield read RGRAPHFLD$Id {*}$keys(${date}_${ip1}_GZ)
                            lappend row {*}[fstdfield stats RGRAPHFLD$Id -coordvalue $lat $lon]

                            #----- Add to the data pool
                            if { "-" ni $row } {
                                lappend data $row
                                incr nb
                            }
                        } on error {} {
                            continue
                        } finally {
                            SPI::Progress +$perc $msg
                        }
                    }

                    if { !$nb } {
                        array unset ip1s $date
                    }
                }

                if { [llength $fids] } {
                    fstdfile close {*}$fids
                }

                #----- Transfer the data into the R world
                if { [llength $data] } {
                    R tcllst2rdf [list $cols $data] $Id $colsfmt
                    R exec "$Id\$date <- as.POSIXct($Id\$date,tz=\"GMT\",format=\"%Y%m%d%H%M\")"
                    R exec "$Id\$elev <- $Id\$elev*10"
                    R exec "$Id <- $Id\[order($Id\$date,$Id\$pres,decreasing=TRUE),\]"
                    R exec "rownames($Id) <- seq(1,length($Id\$date))"
                    R exec "attributes($Id)\$lat <- $lat"
                    R exec "attributes($Id)\$lon <- $lon"
                } else {
                    return -code error "No data available for the selected field at that coordinate"
                }

                set graph(Dates)    [lsort -ascii [array names ip1s]]
                set graph(DateIdx)  0
                set graph(PTopData) $graph(PTop)
                set graph(Loc)      [format "%.5f %.5f" $lat $lon]
            } finally {
                SPI::Progress 0
                fstdfield free RGRAPHFLD$Id RGRAPHP0$Id
                fstdfield autountile $untile
            }
        }
    }
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Tephi::RCode>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Retourne le code R pour l'affichage du graph
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <Var>    : Variable contenant les données du graph
#
# Retour:
#
# Remarques : Nécessaire
#
#----------------------------------------------------------------------------
proc RGraph::Tephi::Font2R { Font Base } {
    set size [font configure $Font -size]
    set face [expr {1+([font configure $Font -weight]=="bold")+2*([font configure $Font -slant]=="italic")}]
    return "$Base.fontsize=$size,$Base.fontface=$face"
}
proc RGraph::Tephi::RCode { Id Var } {
    global GDefs
    variable Param
    upvar #0 $Var graph

    # TclRSyntax
    return "
if( !exists('tephi$Id') )
    tephi$Id <- tephi()

tephi$Id\$date($Id,'[lindex $graph(Dates) $graph(DateIdx)]',$graph(PTop),showWinds=$graph(Winds),showElev=TRUE,
    temp.col='$graph(ColTemp)',dewpt.col='$graph(ColDewpt)',iso.col='$graph(ColIso)',pres.col='$graph(ColPres)',
    adi.col='$graph(ColAdi)',rs.col='$graph(ColRS)',theta.col='$graph(ColTheta)',wind.col='$graph(ColWind)',
    [Font2R RGraph${Id}Iso iso],[Font2R RGraph${Id}Pres pres],[Font2R RGraph${Id}RS rs],[Font2R RGraph${Id}Theta theta]
)
"
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Tephi::CleanUp>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors de la destruction du graph
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <Var>    : Variable contenant les données du graph
#
# Retour:
#
# Remarques : Optionnel
#
#----------------------------------------------------------------------------
proc RGraph::Tephi::RemoveBind { C Id Event } {
    catch {$C bin $Id $Event [regsub -all {(^|\n)[^\n]+;\#Tephi(\n|$)} [$C bin $Id $Event] ""]}
}
proc RGraph::Tephi::RemoveTrace { Type Name } {
    foreach t [trace info $Type $Name] {
        if { [string match "*;#Tephi" [lindex $t 1]] } {
            trace remove $Type $Name {*}$t
        }
    }
}
proc RGraph::Tephi::CleanUp { Id Var } {
    set toplvl [winfo toplevel $RGraph::Data(Graph$Id)]
    bind $toplvl <Key-Left> ""
    bind $toplvl <Key-Right> ""
    bind $toplvl <Shift-Left> ""
    bind $toplvl <Shift-Right> ""

    #----- Free any font created for that id
    if { [llength [set fonts [lsearch -all -inline -glob [font names] "RGraph$Id*"]]] } {
        font delete {*}$fonts
    }

    #----- Destroy the title widgets and remove the extra handler we put in for the entry
    set c $RGraph::Data(Canvas$Id)
    destroy $c.title
    RemoveBind $c $Id <Button-1>

    #----- Remove our trace on resize
    RemoveTrace execution RGraph::Resize

    #----- Remove the highlighting event we put in
    RemoveBind $c $Id <Leave>
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Tephi::OnMotion>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors du déplacement de la souris à l'intérieur du graph
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <Var>    : Variable contenant les données du graph
#  <X>      : Position en X dans le graphique (en coordonnée rdevice)
#  <Y>      : Position en Y dans le graphique (en coordonnée rdevice)
#
# Retour:
#
# Remarques : Optionnel
#
#----------------------------------------------------------------------------
proc RGraph::Tephi::OnMotion { Id Var X Y } {
    global GDefs
    variable Lbl

    #----- Get the details from R
    set details [R exec -get "if( exists('tephi$Id') ) tephi$Id\$details($X,$Y)"]
    if { [llength $details] } {
        set Page::Data(Coord) [format "%.2fhPa %.2f°C %.2fK" [dict get $details p] [dict get $details iso] [dict get $details theta]]
        set Page::Data(Value) [format "%s:%.1fhPa %s:%.1f°C %s:%.1f°C %s:%.1fm" \
            [string trimright [lindex $Lbl(Pres) $GDefs(Lang)]] [dict get $details pres] \
            [string trimright [lindex $Lbl(Temp) $GDefs(Lang)]] [dict get $details temp] \
            [string trimright [lindex $Lbl(Dewpt) $GDefs(Lang)]] [dict get $details dewpt] \
            [lindex $Lbl(Elev) $GDefs(Lang)] [dict get $details elev]]

        if { [dict exists $details wspd] } {
            append Page::Data(Value) [format " %s:%.1fkt@%.1f°" [lindex $Lbl(WindV) $GDefs(Lang)] [dict get $details wspd] [dict get $details wdir]]
        }

        R exec "tephi$Id\$hl([dict get $details idx])"
    }
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Tephi::OnMouseWheel>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors de l'utilisation de la molette de la souris
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <Var>    : Variable contenant les données du graph
#  <X>      : Position en X dans le graphique (en coordonnée rdevice)
#  <Y>      : Position en Y dans le graphique (en coordonnée rdevice)
#  <D>      : Déplacement de la molette (positif si vers le haut, négatif sinon)
#
# Retour:
#
# Remarques : Optionnel
#
#----------------------------------------------------------------------------
proc RGraph::Tephi::OnMouseWheel { Id Var X Y D } {
    R exec "if( exists('tephi$Id') ) tephi$Id\$zoomIncr($X,$Y,1+$D*0.05)"
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Tephi::OnMouseWheel>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors d'un zoom par limites
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <Var>    : Variable contenant les données du graph
#  <X0>     : Première position en X (en coordonnée rdevice)
#  <Y0>     : Première position en Y (en coordonnée rdevice)
#  <X1>     : Deuxième position en X (en coordonnée rdevice)
#  <Y1>     : Deuxième position en Y (en coordonnée rdevice)
#
# Retour:
#
# Remarques : Optionnel
#
#----------------------------------------------------------------------------
proc RGraph::Tephi::OnBoxZoom { Id Var X0 Y0 X1 Y1 } {
    R exec "if( exists('tephi$Id') ) tephi$Id\$zoomBox($X0,$Y0,$X1,$Y1)"
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Tephi::OnZoomReset>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Réinitialisation du zoom
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <Var>    : Variable contenant les données du graph
#
# Retour:
#
# Remarques : Optionnel
#
#----------------------------------------------------------------------------
proc RGraph::Tephi::OnZoomReset { Id Var } {
    R exec "if( exists('tephi$Id') ) tephi$Id\$reset()"
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Tephi::OnDrag>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors d'un drag
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <Var>    : Variable contenant les données du graph
#  <X0>     : Position initiale du drag en X (en coordonnée rdevice)
#  <Y0>     : Position initiale du drag en Y (en coordonnée rdevice)
#  <X1>     : Position courante du drag en X (en coordonnée rdevice)
#  <Y1>     : Position courante du drag en Y (en coordonnée rdevice)
#  <XL>     : Position précédente du drag en X (en coordonnée rdevice)
#  <YL>     : Position précédente du drag en Y (en coordonnée rdevice)
#
# Retour:
#
# Remarques : Optionnel
#
#----------------------------------------------------------------------------
proc RGraph::Tephi::OnDrag { Id Var X0 Y0 X1 Y1 XL YL } {
    R exec "if( exists('tephi$Id') ) tephi$Id\$drag($X1-$XL,$Y1-$YL)"
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Tephi::OnDateIdxChange>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Callback lors d'un changement de date
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <Var>    : Variable contenant les données du graph
#
# Retour:
#
# Remarques : Appelé par une vtrace*write sur ${Var}(DateIdx)
#
#----------------------------------------------------------------------------
proc RGraph::Tephi::OnDateIdxChange { Id Var } {
    upvar $Var graph

    if { $graph(DateIdx) != "" } {
        set date [lindex $graph(Dates) $graph(DateIdx)]
        scan $date "%4d%2d%2d%2d%2d" yyyy mm dd HH MM

        set graph(Date)     [format "%04d-%02d-%02d %02d:%02d UTC" $yyyy $mm $dd $HH $MM]
        set graph(DateSeq)  "[expr $graph(DateIdx)+1]/[llength $graph(Dates)]"
    } else {
        set graph(Date)     ""
        set graph(DateSeq)  ""
    }
}

#----------------------------------------------------------------------------
# Nom      : <RGraph::Tephi::DateIdx>
# Creation : Mars 2019 - E. Legault-Ouellet - CMC/CMOE
#
# But      : Change la date affichée
#
# Parametres :
#  <Id>     : Identificateur du grapĥ
#  <Var>    : Variable contenant les données du graph
#  <Idx>    : Nouvel index ou incrément p/r à l'index courant
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------
proc RGraph::Tephi::DateIdx { Id Var Idx } {
    if { $RGraph::Data(DataValid$Id) && [focus]!="$RGraph::Data(Canvas$Id).title.info" } {
        upvar $Var graph

        switch -exact [string index $Idx 0] {
            "-"     -
            "+"     {set Idx [expr $graph(DateIdx) $Idx]}
            "end"   {set Idx [expr [llength $graph(Dates)]-1]}
        }

        if { $Idx>=0 && $Idx<[llength $graph(Dates)] && $Idx!=$graph(DateIdx) } {
            set graph(DateIdx) $Idx
            RGraph::UpdateGraph $Id 1
        }
    }
}
