`interactive_1_2` <- function() {
	OnLeftClick <- function(x,y) {
    	xClick <- x
    	yClick <- y
	    width <- as.numeric(tclvalue(tkwinfo("reqwidth",img)))
	    height <- as.numeric(tclvalue(tkwinfo("reqheight",img)))
	    parPlotSize <- Tget(".parPlotSize")
	    usrCoords <- Tget(".usrCoords")
	    xMin <- parPlotSize[1]*width
	    xMax <- parPlotSize[2]*width
	    yMin <- parPlotSize[3]*height
	    yMax <- parPlotSize[4]*height
	    rangeX <- usrCoords[2]-usrCoords[1]
	    rangeY <- usrCoords[4]-usrCoords[3]
	    xClick <- as.numeric(xClick)+0.5
	    yClick <- as.numeric(yClick)+0.5
	    yClick <- height-yClick
	    xPlotCoord <- usrCoords[1]+(xClick-xMin)*rangeX/(xMax-xMin)
	    yPlotCoord <- usrCoords[3]+(yClick-yMin)*rangeY/(yMax-yMin)
	    x.click <- xPlotCoord
	    Tassign("l1",list(x=x.click,y=0))
	    tkrreplot(img)
  	} 
  	onestep <- function(x, constant=3/1.486) {
	  	mMedian <- median(x)
	  	mMad <- mad(x)
	  	limit1 <- mMedian + constant * mMad
	  	limit2 <- mMedian - constant * mMad
	  	w1 <- which( x > limit1 )
	  	w2 <- which( x < limit2 )
	  	x[w1] <- limit1
	  	x[w2] <- limit2
	  	mean(x)
  	}  
  	hist.refresh <- function() {
    	Methods <- c("Wurzeln", "sturges", "fd")
    	par(bg="white")
    	if(Texists("maxx")) {
      		maxx <- as.numeric(evalq(tclvalue(maxx), env=as.environment(which(search()=="TGUIenv"))))
      		minx <- as.numeric(evalq(tclvalue(minx), env=as.environment(which(search()=="TGUIenv"))))
      		breaks <- Methods[as.numeric(evalq(tclvalue(breaksMethod), env=as.environment(which(search()=="TGUIenv"))))]
		}
		else {
      		maxx <- 5
      		minx <- 0
      		breaks <- Methods[2]
    	} 
    	l <- layout( matrix(c(1,1,1,2,2, 1,1,1,2,2, 1,1,1,2,2, 1,1,1,2,2, 1,1,1,2,2), ncol=5 ) )
    	if(!Texists("l1")) {
      		a <- c(as.numeric(minx),as.numeric(maxx))
      		l <- layout( matrix(c(1,1,1,2,2, 1,1,1,2,2, 1,1,1,2,2, 1,1,1,2,2, 1,1,1,2,2), ncol=5 ) )
      		if(breaks=="Wurzeln")
        		breaks <- ceiling(sqrt(length(a)))
      		h <- hist( x = a, br=breaks, col="skyblue" ) 
      		ind=rep(0,2)
		   	set.seed(123)
		  	ind <- jitter(ind,factor=2)-.2
		  	plot(a, ind, ylim=c(-0.5,0.5), xlim=c(h$breaks[1], h$breaks[length(h$breaks)]))
		  	Tassign("a",a)
    	}
		else if(!is.null(Tget("l1"))) {
      		l1 <- Tget("l1")
      		a <- Tget("a")
      		if(Texists("l1"))
        		Tassign("l1",NULL)
      		a <- as.numeric(c(a, l1$x))
      		Tassign("a",a)
      		if(breaks=="Wurzeln")
        		breaks <- ceiling(sqrt(length(a)))
      		h <- hist( x = a, br=breaks, main="Histogram of your distribution", xlab="Characteristics of variable 'a'", col="skyblue")
      		ind=rep(0,length(a))
      		set.seed(123)
      		ind <- jitter(ind,factor=2)-.2
      		plot(a, ind, ylim=c(-0.5,0.5), xlim=c(h$breaks[1], h$breaks[length(h$breaks)]))
      		s <- summary( a )
      		legend("topright", legend=c(paste("Median = ", s[3], sep=""),
            	paste("Mean = ", s[4], sep=""),
            	paste("OneStep = ", round(onestep(a),3), sep="") ) )
      		legend("topleft", legend=c(paste("StdDev. = ", round(sd(a),3), sep=""),
            	paste("robStdDev. = ", round((quantile(a, 0.75) - quantile(a, 0.25))/1.349,3), sep=""),
            	paste("variance = ", round(var(a),3), sep=""),
            	paste("MAD = ", round(mad(a),3), sep="") ) )
    	}
		else {
      		a <- Tget("a")
      		if(a[1]==minx && a[2]==maxx) {
        		if(breaks=="Wurzeln")
          			breaks <- ceiling(sqrt(length(a)))
        		h <- hist( x = a, br=breaks, main="Histogram of your distribution", xlab="Characteristics of variable 'a'", col="skyblue")
        		ind=rep(0,length(a))
        		set.seed(123)
       	 		ind <- jitter(ind,factor=2)-.2
        		plot(a, ind, ylim=c(-0.5,0.5), xlim=c(h$breaks[1], h$breaks[length(h$breaks)]))
        		s <- summary( a )
        		legend("topright", legend=c(paste("Median = ", s[3], sep=""),
                	paste("Mittel = ", s[4], sep=""),
                	paste("OneStep = ", round(onestep(a),3), sep="") ) )
        		legend("topleft", legend=c(paste("StdAbw. = ", round(sd(a),3), sep=""),
                	paste("robStdA. = ", round((quantile(a, 0.75) - quantile(a, 0.25))/1.349,3), sep=""),
                	paste("Varianz = ", round(var(a),3), sep=""),
                	paste("MAD = ", round(mad(a),3), sep="") ) )
      		}
			else {
        		a <- c(as.numeric(minx),as.numeric(maxx))
        		l <- layout( matrix(c(1,1,1,2,2, 1,1,1,2,2, 1,1,1,2,2, 1,1,1,2,2, 1,1,1,2,2), ncol=5 ) )
        		if(breaks=="Wurzeln")
          			breaks <- ceiling(sqrt(length(a)))
        		h <- hist( x = a, br=breaks, col="skyblue" ) 
        		ind=rep(0,2)
        		set.seed(123)
        		ind <- jitter(ind,factor=2)-.2
        		plot(a, ind, ylim=c(-0.5,0.5), xlim=c(h$breaks[1], h$breaks[length(h$breaks)]))
        		Tassign("a",a)
      		}
    	}
    	Tassign(".parPlotSize",par("plt"))
    	Tassign(".usrCoords",par("usr"))
  	}
  
  	OpenWindow(title="Exercise - interactive histogram")
  	InteractivePlot(plot.function=hist.refresh,name=interactive2,
      	header="In this interactive session, you are now asked to create a histogram.",
      	slider=c("maxx","minx"),slider_start=c(0,5),slider_res=c(1,1),
      	slider_lim=list(c(0,5),c(5,10)),
      	slider_label=c("Please choose a minimal value for your histogram:", "Please choose a maximal value for your histogram:"),
      	radio_header="Please choose the algorithm that is used to calculate the number of bars in your histogram.",
      	radio="breaksMethod",
     	radio_label=c("square root of n n (n=sample size)", "Sturges", "FD"),
      	radio_value=1:3,click.function=OnLeftClick,design="top",hscale=1.5,vscale=1.5
  	)
  	tkgrid(interactive2)
  	ExitButton(name=Exit)
  	tkgrid(Exit)
}
