OpenWindow <- defmacro(window=tt,frame=MainFrame,fullscreen=TRUE,title="",
    image=NULL,image_position="top",
    expr={		
		# image_position in top, bottom
	  	if(!image_position %in% c("top", "bottom"))
			stop("please specify a correct position of the image")
		
		window <- tktoplevel()
	  	tkwm.title(window, title)
	  	if(fullscreen) {
	    	if(Sys.info()[1]=="Windows")
	      		tcl("wm", "state", window,"zoomed")
	    	else
	      		tcl("wm", "attributes", window, "-zoomed", TRUE)
	  	}
	  	frame <- tkframe(window)
	  	if(!is.null(image)) {
	    	imageFrame <- tkframe(window)
	    	image1 <- tclVar()
			
			path <- aTget("pathDoc")
	    	path <- paste(path, image, sep="")
	    	tcl("image","create","photo",image1,file=path)
	    	tkgrid(tklabel(imageFrame,image=image1,bg="grey"), sticky="n")
	    
	    	if(image_position=="top") {
	      		tkpack(imageFrame,side=image_position)
	      		tkpack(frame)
	    	}
			else if(image_position!="top") {
	      		tkpack(frame)
	      		tkpack(imageFrame,side=image_position)
	    	}
	  	}
		else
	    	tkpack(frame)
	}
)