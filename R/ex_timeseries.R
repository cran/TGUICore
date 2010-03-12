timeseries <- function(eval=FALSE) {
  PlotErg <- function() {
    par(bg="white")
    co <- c(0.7,-0.6)
    set.seed(20)
    series <- rnorm(200)
    y.st <- filter(series,filter=co,method='convolution')
    y.st <- y.st[1:199]
    strata <- c("original","acf","pacf","roots")
    if(Texists("strata1")){
      strata <- strata[as.numeric(evalq(tclvalue(strata1), env = TGUIenv))]    
    }else{
      strata <- "original"
    }
    if (strata=="original") {
      a <- max(y.st)+1
      b <- min(y.st)-1
      plot(y.st,type="l",ylab="value",xlab="time",bty="n")
      axis(1,tick=TRUE,lwd=1.7)
      axis(2,tick=TRUE,lwd=1.7)
      abline(h=0,lty=2,col="blue")
    }
    else if (strata=="acf"){
      x <- acf(y.st,plot=FALSE)
      a <- min(x$acf)-0.1
      acf(y.st,bty="n",ylim=c(a,1),xlim=c(0,25),main="")
      axis(1,tick=TRUE,lwd=1.7)
      axis(2,tick=TRUE,lwd=1.7)
    }
    else if (strata=="pacf"){
      x <- pacf(y.st,plot=FALSE)
      a <- min(x$acf)-0.1
      pacf(y.st,bty="n",ylim=c(a,1),xlim=c(0,25),main="")
      axis(1,tick=TRUE,lwd=1.7)
      axis(2,tick=TRUE,lwd=1.7)
    }
    else if (strata=="roots"){
      polyroot(c(1,-co))
      Mod(polyroot(c(1,-co)))
      root.comp <- Im(polyroot(c(1,-co)))
      root.real <- Re(polyroot(c(1,-co)))
      a <- max(root.comp,root.real)+0.2
      x <- seq(-1,1,length=1000)
      y1 <- sqrt(1-x^2)
      y2 <- -sqrt(1-x^2)
      plot(c(x,x),c(y1,y2),bty="n",xlab='real number',ylab='imaginary number',type='l',main='',
          ylim=c(-a,a),xlim=c(-a,a))
      abline(h=0)
      abline(v=0)
      points(Re(polyroot(c(1,-co))),Im(polyroot(c(1,-co))),pch=19)
      axis(1,tick=TRUE,lwd=1.7)
      axis(2,tick=TRUE,lwd=1.7)
      #legend(-1.7,-1.7,legend="Wurzeln des Prozesses",pch=19)
    }
    
  }
  exerciseTitle <- "Identifying of Moving Average Processes"
  q1 <- "Is the Moving-Average Process stationary?"
  q2 <- c("")
  labs <- c("Yes","No")
  ex <- "timeseries"
  
  
  plotHeader <- 
      if(eval==FALSE) {
        OpenWindow(title=exerciseTitle)
        InteractivePlot(
            plot.function=PlotErg, 
            name=plotLagemasse,
            header="Simulate time series",
            radio_header="selected plot",
            radio=c("strata1"),
            radio_label=c("Original","ACF","PACF",
                "Roots" ),
            radio_value=1:4,
            design="leftright", hscale=1.5, vscale=1.2
        )
        tkgrid(plotLagemasse)
        SingleChoice(frame=MainFrame,
            name=ts,
            question1=q1,
            question2=q2,
            labels=labs,
            filename=ex
        )
        tkgrid(ts)		
      }
      else {
        exerciseTitle <- paste("Evaluation - ", exerciseTitle, sep="")
        OpenWindow(title=exerciseTitle)
        InteractivePlot(
            plot.function=PlotErg, 
            name=plotLagemasse,
            header="Simulate time series",
            radio_header="selected plot",
            radio=c("strata1"),
            radio_label=c("original","ACF","PACF",
                "Roots" ),
            radio_value=1:4,
            design="leftright", hscale=1.2, vscale=1
        )
        tkgrid(plotLagemasse)
        SingleChoice(frame=MainFrame,
            name=ts,
            question1=q1,
            question2=q2,
            labels=labs,
            filename=ex,Answer=TRUE
        )
        tkgrid(ts)		
      }
}
