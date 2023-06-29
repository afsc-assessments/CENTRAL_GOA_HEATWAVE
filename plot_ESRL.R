

library(ncdf4)
library(ncdf4.helpers)
library(PCICt)
update.figs<-1
setwd(path.expand("~/Documents/D_AFSC_Files/AFSC_code/R_scripts/PLOT_ESRL"))
#GOA_GFDL_85.nc

#' PLot ESRL climate change data
#'
#' Rcode developed by: Kirstin Holsman
#' kirstin.holsman@noaa.gov
#' Used to plot data from the ESRL cliamte change portal: https://www.esrl.noaa.gov/psd/ipcc/ocn/
#'
#' This function plot_clim() plots netcdf files from the portal
#' dat,col=col2(10)[5],newplot=TRUE,ylimm=ylim1,col_line=FALSE,ylabb="",plotAnom=TRUE,alpha1=100,alpha2=150
#' @param dat is the data file to plot
#' @keywords Temperature, scaling, consumption
#' @export bioE
#' @examples
#' plk_par<-data.frame(RFR=1, Qox=13560,Ceq=2,Req=2,Weq=1,Tco=10,Tcm=15,QC=2.6,CA=0.119,CB=-0.46, 
makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

##' PLot ESRL climate change data
#'
#' Rcode developed by: Kirstin Holsman
#' kirstin.holsman@noaa.gov
#' Used to plot data from the ESRL cliamte change portal: https://www.esrl.noaa.gov/psd/ipcc/ocn/
#'
#' This function plot_clim() plots netcdf files from the portal
#' dat,col=col2(10)[5],newplot=TRUE,ylimm=ylim1,col_line=FALSE,ylabb="",plotAnom=TRUE,alpha1=100,alpha2=150
#' @param dat is the data file to plot
#' @keywords Temperature, scaling, consumption
#' @export bioE
#' @examples
#' plk_par<-data.frame(RFR=1, Qox=13560,Ceq=2,Req=2,Weq=1,Tco=10,Tcm=15,QC=2.6,CA=0.119,CB=-0.46, 
plot_clim<-function(dat,col=col2(10)[5],newplot=TRUE,ylimm=ylim1,col_line=FALSE,ylabb="",plotAnom=TRUE,alpha1=100,alpha2=150){
  if(col_line==FALSE) col_line<-col
  if(plotAnom){
    anomenvel<-dat$anomenvel
    anomaly<-dat$anomaly
  }else{
    anomenvel<-dat$envel
    anomaly<-dat$mean
  }
  yr<-dat$yr
    if(newplot){
      plot(yr,anomenvel[,1],type="l",ylim=ylimm,col="white",ylab=ylabb,xlab="Year",axes=F)
      axis(1);axis(1,c(1500,2500))
      stp<-abs(ylimm[1]/10)
      if(stp==0) stp<-abs(ylimm[2]/30)
      yy<-pretty(seq(ylimm[1],ylimm[2],stp))
      
      axis(2,las=2,at=yy);axis(2,c(-1000,1000))
      axis(4,las=2,at=yy,lab=rep("",length(yy)))
      axis(4,c(-1000,1000))
      axis(3,c(1500,2500))
    }
    dd<-na.omit(data.frame(
      yr=c(yr,rev(yr)),
      dat=c(anomenvel[,1],rev(anomenvel[,2]))))
    # polygon( dd[,1],dd[,2],col=makeTransparent(col,alpha=alpha1),border=FALSE)
    
    dd<-na.omit(data.frame(
      yr=c(yr,rev(yr)),
      dat=c(anomenvel[,3],rev(anomenvel[,4]))))
    polygon( dd[,1],dd[,2],col=makeTransparent(col,alpha=alpha1),border=FALSE)
    
    dd<-na.omit(data.frame(
      yr=c(yr,rev(yr)),
      dat=c(anomenvel[,5],rev(anomenvel[,6]))))
    polygon( dd[,1],dd[,2],col=makeTransparent(col,alpha=alpha2),border=FALSE)
    
    lines(yr,anomaly,col=col_line,lwd=2)

}

getdata<-function(fl){
    climate_output<-nc_open(fl) 
    mean <- ncvar_get(climate_output, "mean")
    envel<-ncvar_get(climate_output, "envel")
    yr <- ncvar_get(climate_output, "year")
    anomaly<-ncvar_get(climate_output, "anomaly")
    anomenvel<-ncvar_get(climate_output, "anomenvel");
    colnames(anomenvel)<-c("min","max", "10%", "90%", "25%", "75%")
    return(list(mean=mean,envel=envel,yr=yr,anomaly=anomaly,anomenvel=anomenvel))
    nc_close(climate_output)
}

  col1<-colorRampPalette(colors()[c(280,320)])
  col2<-colorRampPalette(colors()[c(70,491)])
  col2<-colorRampPalette(colors()[c(114,491)])
  col3<-colorRampPalette(c("yellow","red"))
  
  GOA_BT_gfdlrcp45<-getdata("data/GOA_GFDL_45.nc") # June BT from GFDL_ESM2M
  GOA_BT_gfdlrcp85<-getdata("data/GOA_GFDL_85.nc") # June BT from GFDL_ESM2M
  GOA_BT_gfdlrcp45$anomaly  # the vector of anomolies for GFDL
  GOA_BT_gfdlrcp85$anomaly  # the vector of anomolies for GFDL  
  
  quartz(h=5,w=5)
  par(mar=c(1,1,1,1)) # margins of graph: (bottom,left, top, right)
  par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
  par(oma=c(2,3,2,2))# outer margins of graph: (bottom,left, top, right)
  par(mfrow=c(1,1))
  alpha11<-50
  alpha22<-100
  ylim1<-c(min(as.vector(GOA_BT_gfdlrcp45$anomenvel)),max(as.vector(GOA_BT_gfdlrcp85$anomenvel)))
    plot_clim(dat=GOA_BT_gfdlrcp45,ylimm=ylim1,newplot=T,col=col2(10)[2],
              col_line=col2(10)[3],alpha1=alpha11,alpha2=alpha22,
              ylabb=expression(paste("Anomalies (", C^o,")")))
    plot_clim(dat=GOA_BT_gfdlrcp85,ylimm=ylim1,
              newplot=F,col=col3(10)[4],alpha1=alpha11,alpha2=alpha22,col_line=col3(10)[5])
    legend("topleft",c("GFDL RCP 4.5", "GFDL RCP 8.5"),lwd=2,lty=1,col=c(col2(10)[3],col3(10)[5]),box.lty=0 )
    mtext(side=3,"Change in EBS June BT relative to 1976-2005",font=2,line=1)
    mtext(side=2,expression(bold(paste("Bottom temperature anomalies (", C^o,")"))),line=1.5)
    if(update.figs==1)
      quartz.save(file="AnomGOA_BT.jpg",type="jpg",dpi=500)
 
  ylim1<-c(min(as.vector(GOA_BT_gfdlrcp45$envel)),max(as.vector(GOA_BT_gfdlrcp85$envel)))
    plot_clim(plotAnom=FALSE,dat=GOA_BT_gfdlrcp45,alpha1=alpha11,alpha2=alpha22,ylimm=ylim1,newplot=T,col=col2(10)[2],col_line=col2(10)[3],ylabb=expression(paste("Anomalies (", C^o,")")))
    plot_clim(plotAnom=FALSE,dat=GOA_BT_gfdlrcp85,alpha1=alpha11,alpha2=alpha22,ylimm=ylim1,newplot=F,col=col3(10)[4],col_line=col3(10)[5])
    legend("topleft",c("GFDL RCP 4.5", "GFDL RCP 8.5"),lwd=2,lty=1,col=c(col2(10)[3],col3(10)[5]),box.lty=0 )
    mtext(side=3,"Change in EBS July BT relative to 1976-2005",font=2,line=1)
    mtext(side=2,expression(bold(paste("Bottom temperature (", C^o,")"))),line=1.5)
    if(update.figs==1)
      quartz.save(file="GOA_BT.jpg",type="jpg",dpi=500)
    
    quartz(h=4,w=7)
    par(mar=c(1,1,1,1)) # margins of graph: (bottom,left, top, right)
    par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
    par(oma=c(2,3,2,2))# outer margins of graph: (bottom,left, top, right)
    par(mfrow=c(1,2))
    
    ylim1<-c(min(as.vector(GOA_BT_gfdlrcp45$envel)),max(as.vector(GOA_BT_gfdlrcp85$envel)))
    plot_clim(plotAnom=FALSE,dat=GOA_BT_gfdlrcp45,ylimm=ylim1,newplot=T,col=col2(10)[2],col_line=col2(10)[3],ylabb=expression(paste("Anomalies (", C^o,")")))
    abline(h=mean(GOA_BT_gfdlrcp45$mean[GOA_BT_gfdlrcp45$yr>=1986&GOA_BT_gfdlrcp45$yr<=2005]),lty=2)
    mtext(side=3,"RCP 4.5",font=2,line=0,outer=F)
    mtext(side=2,expression(bold(paste("Temperature (", C^o,")"))),line=1.5)
    legend("topleft",c("GFDL"),lwd=2,lty=1,col=c(col2(10)[3]),box.lty=0 )
    
    plot_clim(plotAnom=FALSE,dat=GOA_BT_gfdlrcp85,ylimm=ylim1,newplot=T,col=col3(10)[4],col_line=col3(10)[5])
   legend("topleft",c("GFDL"),lwd=2,lty=1,col=c(col3(10)[5]),box.lty=0 )
   abline(h=mean(GOA_BT_gfdlrcp45$mean[GOA_BT_gfdlrcp45$yr>=1986&GOA_BT_gfdlrcp45$yr<=2005]),lty=2)
    mtext(side=3,"Eastern Bering Sea July Bottom Temperature",font=2,line=1,outer=T)
    mtext(side=3,"RCP 8.5",font=2,line=0,outer=F)
   
    if(update.figs==1)
      quartz.save(file="GOA_BTv2.jpg",type="jpg",dpi=500)
    
    