runGUI <- function(){
	callExercise <- function(command) {
    	split_command <- unlist(strsplit(command,"\\("))
    	TF <- length(rL <- readLines(fc <- file(paste(aTget("pathGUI"),"/adminExercise.txt",sep=""))))>0
    	close(fc)
    	if(TF) {
      		if(split_command[1] %in% rL)
        		eval(parse(text=command))
      		else
        		tkmessageBox(title="Exercise is not available",	message="This exercise has not been unlocked yet!")
    	}
		else
      		tkmessageBox(title="Exercise is not available",	message="This exercise has not been unlocked yet!")
  	}
  	pathGUI <- aTget("pathGUI")
  	tt <- tktoplevel(background="white")
	tkwm.title(tt, aTget("usertitle"))
	
  	if(Sys.info()[1]=="Windows") 
    	tcl("wm", "state", tt,"zoomed")
  	else
    	tcl("wm", "attributes", tt, "-zoomed", TRUE)
	
  	topMenu <- tkmenu(tt)
  	tkconfigure(tt,menu=topMenu)
  	
	### Beschriftung leeres Kastl:
	fontHeading 	<- setFont(size=12, bold=TRUE, italic=TRUE)
	fontHeading1 	<- setFont(size=13)
	fontHeading1b 	<- setFont(size=11)
	fontHeading1c 	<- setFont(size=11, bold=TRUE)

  	indKurs <- 1
  	indTeil <- 1
  	KursMenu <- list()
  	TeilMenu <- list()
  	contents <- aTget("contents")
  	for(i in 1:nrow(contents)) {
    	if(i==1) {
      		KursMenu[[indKurs]] <- tkmenu(topMenu,tearoff=FALSE)
          tkadd(KursMenu[[indKurs]],"command",label="Check Yourself!", font=fontHeading1b)
          if(!contents[i,1]%in%aTget("alwaysOn")){
            tkadd(topMenu,"cascade", label=paste("Course ",contents[i,1],"  ",sep=""), font=fontHeading, menu=KursMenu[[indKurs]])
            TeilMenu[[indTeil]] <- tkmenu(topMenu,tearoff=FALSE)
            tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
          }else{
            tkadd(topMenu,"cascade", label=paste(contents[i,1],"  ",sep=""), font=fontHeading, menu=KursMenu[[indKurs]])
            TeilMenu[[indTeil]] <- tkmenu(topMenu,tearoff=FALSE)
            tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
          }
#      		tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
      		ExCommand <- paste("function(){callExercise(\"",as.character(contents[i,3]),"\")}",sep="")
          tkadd(TeilMenu[[indTeil]],"command",label=as.character(contents[i,5]), font=fontHeading1b,
          	command=eval(parse(text=ExCommand)) )
    	}
		else{
      		if(contents[i,1]!=contents[i-1,1]) {
		        indKurs <- indKurs + 1
		        KursMenu[[indKurs]] <- tkmenu(topMenu,tearoff=FALSE)
            tkadd(KursMenu[[indKurs]],"command",label="Check Yourself!", font=fontHeading1b)
            if(!contents[i,1]%in%aTget("alwaysOn")){	
              tkadd(topMenu,"cascade",label=paste("Course ",contents[i,1],"  ",sep=""), font=fontHeading, menu=KursMenu[[indKurs]])
              indTeil <-indTeil + 1
              TeilMenu[[indTeil]] <- tkmenu(topMenu,tearoff=FALSE)
              tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
            }else{
              tkadd(topMenu,"cascade",label=paste(contents[i,1],"  ",sep=""), font=fontHeading, menu=KursMenu[[indKurs]])
              indTeil <-indTeil + 1
              TeilMenu[[indTeil]] <- tkmenu(topMenu,tearoff=FALSE)
              tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
            }
      		}
			else if(contents[i,2]!=contents[i-1,2]) {
        		indTeil <-indTeil + 1
        		TeilMenu[[indTeil]] <- tkmenu(topMenu,tearoff=FALSE)
            if(!contents[i,1]%in%aTget("alwaysOn")){
        		  tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
            }else{
              tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])              
            }
      		}
      		ExCommand <- paste("function(){callExercise(\"",as.character(contents[i,3]),"\")}",sep="")
      		tkadd(TeilMenu[[indTeil]],"command",label=as.character(contents[i,5]), font=fontHeading1b,
          	command=eval(parse(text=ExCommand)) )
    	}
  	}

	# FIXME: Specify correct color, 
	# FIXME: create titlepage()-function in admin-environment
	for (i in 1:7) 
		tkpack(tklabel(tt, text="", background="white"))

	fr1 <- tkframe(tt, background="white")
	lab11 	<- tklabel(fr1, text=substr(aTget("mainTitle"),1, 1), font=setFont(size=50, italic=FALSE, bold=TRUE), foreground="#980037", background="white")
	lab12 	<- tklabel(fr1, text=substr(aTget("mainTitle"),2, nchar(aTget("mainTitle"))), font=setFont(size=50, italic=FALSE, bold=TRUE), background="white")
	
	tkgrid(lab11, lab12) 
	tkpack(fr1)
	
	fr2 <- tkframe(tt, background="white")
	
	lab2 	<- tklabel(fr2, text=aTget("mainSub"), font=setFont(size=14, italic=FALSE), background="white")
	lab3 	<- tklabel(fr2, text=aTget("subTitle"), font=setFont(size=18), background="white")	
	path <- aTget("pathDoc")
	image1 <- tclVar()
	tcl("image","create","photo",image1,file=paste(path, "logoSTAT1.gif", sep=""))
	graph1 	<- tklabel(fr2, image=image1, background="white")

	tkgrid(lab2)
	tkgrid(lab3)
	tkgrid(graph1) 
	tkpack(fr2)	

	for (i in 1:10) 
		tkpack(tklabel(tt, text="", background="white"))		
	
	fr3 <- tkframe(tt, background="white")
	lab41	<- tklabel(fr3, text=substr(aTget("developed1"), 1, 1), font=setFont(size=13, bold=TRUE), foreground="#980037", background="white")	
	lab42	<- tklabel(fr3, text=substr(aTget("developed1"), 2, nchar(aTget("developed1"))), font=setFont(size=13, bold=FALSE), background="white")	
	tkgrid(lab41, lab42) 
	tkpack(fr3)	
	
	fr4 <- tkframe(tt, background="white")
	lab5 	<- tklabel(fr4, text=aTget("developed2"), font=setFont(size=13, bold=FALSE), background="white")	
	lab6 	<- tklabel(fr4, text=aTget("notice"), font=setFont(size=13, bold=FALSE, italic=FALSE), background="white")
	tkgrid(lab5)
	tkgrid(lab6)
	tkpack(fr4)	

	for (i in 1:3) 
		tkpack(tklabel(tt, text="", background="white"))	
	
}