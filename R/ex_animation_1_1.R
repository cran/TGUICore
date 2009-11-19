`animation_1_1` <- function() {
	OpenWindow(title="Means (interactive graph)")
    plot_lage <- function(i,j){
    par(bg="white")
    x=c(1,4,5,6,8,10,12)
    y=seq(0,50,by=0.2)
    n=length(y)
    plot(1,1,xlim=c(-1,51),ylim=c(0,5.5),xlab="Daten",ylab="",type="n",yaxt="n",xaxt="n")
    points(x,rep(0.5,length(x)),col=1,cex=1.2,pch=16)
    rect(-1,0.7,31,2.6,col="white",border="white")
    rect(-1,3.9,31,4.6,col="white",border="white")
    points(y[i],0.8,cex=1.2,pch=16,col=1)
    z=c(x,y[i])
    zmed=median(z)
    zm=mean(z)
    segments(zmed,2,zmed,2.5,col=3,lwd=2)
    text(15,3,paste("Median","=",round(median(z),2)),cex=1.5,col=3)
    segments(zm,4,zm,4.5,col=4,lwd=2)
    text(15,5,paste("Arithmetic Mean","=",round(mean(z),2)),cex=1.5,col=4)
  }
  plot_lage_wrapper <- function(){
    if(Texists("il")){
      plot_lage(Tget("il"),Tget("jl"))      
    }else
      plot_lage(1,1)    
  }
  	tkgrid(tklabel(MainFrame, text=""))
  	tkgrid(tklabel(MainFrame, text="Effects of an outlier on median and mean", font=setFont(size="large"), background="white"))
	tkgrid(tklabel(MainFrame, text=""))
  	tkgrid(head <- tklabel(MainFrame, text="animation is running!", font=setFont(size="large"), background="white", fg="red"))
	tkgrid(tklabel(MainFrame, text=""))
	PlotFrame <- tkframe(MainFrame)
  	img <- tkrplot(PlotFrame,fun=plot_lage_wrapper,hscale=2,vscale=2)
  	tkgrid(img)
  	tkgrid(PlotFrame)
  	ExitButton(name=Exit)
  	tkgrid(Exit)
  	for (j in 1:1) {
    	Tassign("jl",j)
    	for (i in 1:251) {
      		Tassign("il",i)
      		tkrreplot(img)
      		Sys.sleep(0.1)     
    	}
    	Sys.sleep(1)
  	}
  	tkconfigure(head,text="animation is finished!")  
}