SingleChoiceAnswer <- defmacro(	frame=MainFrame, 
    name,
    question1,
    question2=NULL,
    labels,
    image=NULL,
    hscale=1.5,vscale=1,
    hscaleAns=1,vscaleAns=1,
    plotFunction=NULL,
    filename,
	dynSol=FALSE,
    expr={	
      	name <- tkframe(frame)      
      	contents <- aTget("contents")

		ues <- as.character(sapply(as.character(contents[,3]), function(x) { substr(x, 1, nchar(x)-2) } ))
		for(i in 1:length(ues)) {			
			if(ues[i] == filename) {
				uname <- contents[i,]	
			}				
		}    

      	leftFrame <- tkframe(name)
      	tkgrid(tklabel(leftFrame, text=""));		
      
      	# Draw image if it exists...
      	if(!is.null(image)) {
        	imageFrame <- tkframe(leftFrame)
        	image1 <- tclVar()
			path <- aTget("pathDoc")
        	path <- paste(path, image, ".gif", sep="")
        	tcl("image","create","photo", image1, file=path)
       		tkgrid(tklabel(imageFrame,image=image1, bg="grey"), sticky="n")
        	tkgrid(imageFrame)
        
        	tkgrid(tklabel(leftFrame, text=""));	
      	}
      
      	if(!is.null(plotFunction)) {
      		plotFrame <- tkframe(leftFrame)	
        	tkgrid(plotFrame)
        	img <- tkrplot(plotFrame, fun=plotFunction, hscale=hscale, vscale=vscale)
        	tkgrid(img)
      	}
      
      	# Draw the question
      	QFrame <- tkframe(leftFrame)
      	tkgrid(QFrame)
      	q1 <- tklabel(QFrame,text=question1,font=setFont(size="extralarge", bold=TRUE))
      	tkgrid(q1)	
      	tkgrid.configure(q1, columnspan=2)
      
		if(!is.null(question2)) {
      		q2Working <- question2
			for (j in 1:length(q2Working)) {
          		q2 <- tklabel(QFrame,text=q2Working[j],font=setFont(size="extralarge"))
          		tkgrid(q2)	
          		tkgrid.configure(q2, columnspan=2)					
        	}		
      	}
      	tkgrid(tklabel(leftFrame, text=""));
      
      	# Draw the radiobuttons with labels (mandatory)
      	rb <- list()
      	rb1Frame <- tkframe(leftFrame)
      
      	rbFrame <- tkframe(rb1Frame)
      	tkgrid(rb1Frame)
		
		labelsWorking <- labels
		col <- rep("#cccccc",length(labelsWorking))
      	if(dynSol == FALSE) {
	  		for(i in 1:length(labelsWorking)){
			  	if(i==as.numeric(uname[6])) {
					ll <- tklabel(rbFrame, text=paste(LETTERS[i],") ", labelsWorking[i],sep=""), font=setFont(size="normal"), background="#aaffaa")
		    		col[i] <- "#aaffaa"
				}
		  		else 
					ll <- tklabel(rbFrame, text=paste(LETTERS[i],") ", labelsWorking[i],sep=""), font=setFont(size="normal"))
			
				tkgrid(ll)	
		  		tkgrid.configure(ll, sticky="w")						
			}			  
	  	}
	  	else {
	    	for(i in 1:length(labelsWorking)) {
				ll <- tklabel(rbFrame, text=paste(LETTERS[i],") ", labelsWorking[i],sep=""), font=setFont(size="normal"))
				tkgrid(ll)	
				tkgrid.configure(ll, sticky="w")	
			}	
	  	}      

      	rightFrame <- tkframe(rb1Frame)
      	plotFunction2 <- function() {
			if(dynSol==FALSE) 
				b <- barplot(erg, xlab="", ylab="Frequency", main="", xaxt="n",col=col)
			else 
				b <- barplot(erg, xlab="", ylab="Frequency", main="", xaxt="n",col="cornflowerblue")
        
			axis(1, at=b, labels=LETTERS[1:length(labels)], las=1)
      	}
      
		filename <- paste(filename,".txt", sep="")
		if(file.exists(filename)) {
        	erg <- vector()
        	x <- read.table(filename, sep=",")
        	for(i in 1:length(labels)) {
          		erg[i] <- sum(x==i)
        	}
        	img2 <- tkrplot(rightFrame,fun=plotFunction2,hscale=hscaleAns,vscale=vscaleAns)
        	tkgrid(tklabel(rightFrame,text="Answers:",font=setFont(size="large")))
        	tkgrid(img2)
        	tkgrid(rbFrame,tklabel(rb1Frame,text="  "),rightFrame)
      	}
		else 
        	tkgrid(rbFrame)
     
      	tkgrid(leftFrame)      
      	tkgrid(tklabel(leftFrame, text=""));		
      
      	buttonFrame <- tkframe(name)
      	tkgrid(buttonFrame)		
      	button <- tkbutton(buttonFrame, text="Close evaluation", command=function()tkdestroy(tt),fg="darkgreen")
      	tkgrid(button)		
    }
)
SingleChoice <- defmacro(	frame=MainFrame, 
							name,
							question1,
              				hscale=2,vscale=1.5,
							question2=NULL,
							labels,
							image=NULL,
							plotFunction=NULL,
							note=NULL,
							filename,		
		
	expr={	
		sendRadiobuttonData <- function() {
			setwd(pathGUI)				
			lengthLabels <- length(Tget("labels"))
			erg <- tclvalue(Tget("rbValue"))
			if(erg==0) {
				tkmessageBox(title="Sure?",message="You have not completed the exercise yet!\n")
			}
			else {
				erg <- which(labels==erg)	
				write.table(erg, file=paste(filename,".txt", sep=""), sep=",", append = TRUE, row.names = FALSE, col.names = FALSE)
				tkdestroy(tt)
				rm(list=ls(env=TGUIenv), envir=TGUIenv)   
			}
 		}	
			
		filename <- filename
		name <- tkframe(frame)
		tkgrid(tklabel(name, text=""));		
		
		# Draw image if it exists...
		if(!is.null(image)) {
			imageFrame <- tkframe(name)
			image1 <- tclVar()
			path <- aTget("pathDoc")
			path <- paste(path, image, ".gif", sep="")
			tcl("image","create","photo", image1, file=path)
			tkgrid(tklabel(imageFrame,image=image1, bg="grey"), sticky="n")
			tkgrid(imageFrame)
			tkgrid(tklabel(name, text=""));	
		}
		
		if(!is.null(plotFunction)) {
			plotFrame <- tkframe(name)	
			tkgrid(plotFrame)
			img <- tkrplot(plotFrame, fun=plotFunction, hscale=hscale, vscale=vscale)
			tkgrid(img)
		}
		
		# Draw the question (mandatory)
		QFrame <- tkframe(name)
		tkgrid(QFrame)
		q1 <- tklabel(QFrame,text=question1,font=setFont(size="extralarge", bold=TRUE))
		tkgrid(q1)	
		tkgrid.configure(q1, columnspan=2)
		
		if(!is.null(question2)) {
			q2Working <- question2
			for (j in 1:length(q2Working)) {
				q2 <- tklabel(QFrame,text=q2Working[j],font=setFont(size="extralarge"))
				tkgrid(q2)	
				tkgrid.configure(q2, columnspan=2)					
			}		
		}
		tkgrid(tklabel(name, text=""));
		
		# Draw the radiobuttons with labels (mandatory)
		rb <- list()
		rbValue <- tclVar(as.character("0"))
		rbFrame <- tkframe(name)
		labelsWorking <- labels
				
		tkgrid(rbFrame)
		for(i in 1:length(labelsWorking)){
			rb[[i]] <- tkradiobutton(rbFrame)
			ll <- tklabel(rbFrame, text=paste(LETTERS[i],") ",labelsWorking[i], sep=""), font=setFont(size="normal"))
			tkgrid(rb[[i]], ll) 
			tkgrid.configure(rb[[i]], sticky="e")	
			tkgrid.configure(ll, sticky="w")						
		}		
		
		Tassign("rb",rb)
		Tassign("rbValue", rbValue)		
		Tassign("labelsWorking", labelsWorking)
		for(i in 1:length(labelsWorking)) {			
			Tassign("i", i)
			evalq(tkconfigure(rb[[i]], variable = rbValue, value=as.character(labelsWorking[i])),env=TGUIenv)
		}		

		if(!is.null(note)) {
			tkgrid(tklabel(name, text=""));
			
			noteFrame <- tkframe(name)
			tkgrid(noteFrame)
			nn <- tklabel(noteFrame,text=note, font=setFont(size="small"))
			tkgrid(nn)
			tkgrid.configure(nn, columnspan=2)
		}
		tkgrid(tklabel(name, text=""));		
		
		buttonFrame <- tkframe(name)
		tkgrid(buttonFrame)		
		button <- tkbutton(buttonFrame, text="Submit answer", command=sendRadiobuttonData,fg="darkgreen")
		tkgrid(button)		
	}
)

SingleChoiceGrid <- defmacro(	
	frame=MainFrame, 
	name,
	question1,
	hscale=2, 
	vscale=1.5,
	question2=NULL,
	labels1,
	labels2,
	image=NULL,
	plotFunction=NULL,
	note=NULL,
	filename,		
		
	expr={	
		sendRadiobuttonDataGrid <- function() {
			setwd(pathGUI)	

			rbValue <- Tget("rbValue")
			erg <- NULL
			
			for(i in 1:length(rbValue)) {
				erg <- c(erg, tclvalue(rbValue[[i]]))	
			}					
	
			if(length(which(erg=="0")) > 0) {
				tkmessageBox(title="Sure?",message="You have not completed the exercise yet!\n")
			}
			else {
				write.table(t(erg), file=paste(filename,".txt",sep=""), sep=",", append = TRUE, row.names = FALSE, col.names = FALSE)
				tkdestroy(tt)
				rm(list=ls(env=TGUIenv), envir=TGUIenv)   
			}
		}	
		
		filename <- filename
		name <- tkframe(frame)
		tkgrid(tklabel(name, text=""));		
		
		# Draw image if it exists...
		if(!is.null(image)) {
			imageFrame <- tkframe(name)
			image1 <- tclVar()
			path <- aTget("pathDoc")
			path <- paste(path, image, ".gif", sep="")
			tcl("image","create","photo", image1, file=path)
			tkgrid(tklabel(imageFrame,image=image1, bg="grey"), sticky="n")
			tkgrid(imageFrame)			
			tkgrid(tklabel(name, text=""));	
		}
		
		if(!is.null(plotFunction)) {
			plotFrame <- tkframe(name)	
			tkgrid(plotFrame)
			img <- tkrplot(plotFrame, fun=plotFunction, hscale=hscale, vscale=vscale)
			tkgrid(img)
		}
		
		# Draw the question (mandatory)
		QFrame <- tkframe(name)
		tkgrid(QFrame)
		q1 <- tklabel(QFrame,text=question1,font=setFont(size="extralarge", bold=TRUE))
		tkgrid(q1)	
		tkgrid.configure(q1, columnspan=length(labels1)+1)
		
		if(!is.null(question2)) {
			q2Working <- question2
			for (j in 1:length(q2Working)) {
				q2 <- tklabel(QFrame,text=q2Working[j],font=setFont(size="extralarge"))
				tkgrid(q2)	
				tkgrid.configure(q2, columnspan=length(labels1)+1)					
			}		
		}
		tkgrid(tklabel(name, text=""));

		rbFrame <- tkframe(name)
		tkgrid(rbFrame)
		rbValue <- list()
		
		createQ <- paste("q", 1, sep="")
		assign(createQ, tklabel(rbFrame, text=""))
		befehl <- paste("tkgrid(", createQ, sep="")
		
		labels1Working <- labels1
		labels2Working <- labels2
		for (i in 1:length(labels1Working)) {
			createLL <- paste("ll",i,sep="")
			assign(createLL, tklabel(rbFrame, text=labels1Working[i]))
			befehl <- paste(befehl, ",", createLL, sep="")
		}
		befehl <- paste(befehl, ")", sep="")
		eval(parse(text=befehl))
		
		createRB <- list()
		
		for (i in 1:length(labels1Working)) { 			
			createQ <- paste("q", i, sep="")
			befehl <- "tkgrid("
			assign(createQ, tklabel(rbFrame, text=labels2Working[i]))
			befehl <- paste(befehl, createQ, sep="")
			rbValue[[i]] <- tclVar(as.character("0"))
			createRB[[i]] <- list()
			for (j in 1:length(labels2Working)) { 				
				createRB[[i]][[j]] <- tkradiobutton(rbFrame)
				befehl <- paste(befehl, ",", "createRB[[",i,"]][[",j,"]]", sep="")
				tkconfigure(createRB[[i]][[j]], variable = rbValue[[i]], value=LETTERS[j])
			}
			befehl <- paste(befehl, ")", sep="")
			eval(parse(text=befehl))
			eval(parse(text=paste("tkgrid.configure(", createQ, ", sticky=\"e\")", sep="")))
		}			
		
		Tassign("rbValue", rbValue)
		
		if(!is.null(note)) {
			tkgrid(tklabel(name, text=""));
			
			noteFrame <- tkframe(name)
			tkgrid(noteFrame)
			nn <- tklabel(noteFrame,text=note, font=setFont(size="small"))
			tkgrid(nn)
			tkgrid.configure(nn, columnspan=2)
		}		
		tkgrid(tklabel(name, text=""));		
		
		buttonFrame <- tkframe(name)
		tkgrid(buttonFrame)		
		button <- tkbutton(buttonFrame, text="Submit answer!", command=sendRadiobuttonDataGrid, fg="darkgreen")
		tkgrid(button)		
	}
)

SingleChoiceAnswerGrid <- defmacro(	frame=MainFrame, 
	name,
	question1,
	question2=NULL,
	labels1,
	labels2,
	image=NULL,
	hscale=1.5,vscale=1,
	hscaleAns=1,vscaleAns=1,
	plotFunction=NULL,
	note=NULL,
	filename,
	expr={	
		name <- tkframe(frame)
		
		contents <- aTget("contents")
		ues <- as.character(sapply(as.character(contents[,3]), function(x) { substr(x, 1, nchar(x)-2) } ))
		for(i in 1:length(ues)) {
			if(ues[i] == filename)
				uname <- contents[i,]
		}    
		corrAnswer <- as.integer(unlist(strsplit(as.character(uname[6]),",")))
		corrAnswerLett  <- LETTERS[corrAnswer]

		leftFrame <- tkframe(name)
		tkgrid(tklabel(leftFrame, text=""));		
		
		# Draw image if it exists...
		if(!is.null(image)) {
			imageFrame <- tkframe(leftFrame)
			image1 <- tclVar()
			path <- aTget("pathDoc")
			path <- paste(path,image, ".gif", sep="")
			tcl("image","create","photo", image1, file=path)
			tkgrid(tklabel(imageFrame,image=image1, bg="grey"), sticky="n")
			tkgrid(imageFrame)
			
			tkgrid(tklabel(leftFrame, text=""));	
		}
		
		if(!is.null(plotFunction)) {
			plotFrame <- tkframe(leftFrame)	
			tkgrid(plotFrame)
			img <- tkrplot(plotFrame, fun=plotFunction, hscale=hscale, vscale=vscale)
			tkgrid(img)
		}
		
		# Draw the question
		QFrame <- tkframe(leftFrame)
		tkgrid(QFrame)
		q1 <- tklabel(QFrame,text=question1,font=setFont(size="extralarge", bold=TRUE))
		tkgrid(q1)	
		tkgrid.configure(q1, columnspan=length(labels1)+1)
		
		if(!is.null(question2)) {
			q2Working <- question2
			for (j in 1:length(q2Working)) {
				q2 <- tklabel(QFrame, text=q2Working[j], font=setFont(size="extralarge"))
				tkgrid(q2)	
				tkgrid.configure(q2, columnspan=length(labels1)+1)					
			}		
		}
				
		if(!is.null(note)) {
			tkgrid(tklabel(leftFrame, text=""));
			
			noteFrame <- tkframe(leftFrame)
			tkgrid(noteFrame)
			nn <- tklabel(noteFrame,text=note, font=setFont(size="small"))
			tkgrid(nn)
			tkgrid.configure(nn, columnspan=length(labels1)+1)
		}			
		tkgrid(tklabel(leftFrame, text=""));			
		
		# Draw the radiobuttons with labels (mandatory)
		rb <- list()
		rb1Frame <- tkframe(leftFrame)
		
		rbFrame <- tkframe(rb1Frame)
		tkgrid(rb1Frame)
	
		h1 <- tklabel(rbFrame, text="Question", font=setFont(size="normal", bold=TRUE))
		h2 <- tklabel(rbFrame, text="correct answer", font=setFont(size="normal", bold=TRUE))
		tkgrid(h1, h2)
		tkgrid.configure(h1, sticky="e")
		tkgrid.configure(h2, sticky="w")
		
		createLab <- list()
		
		labels1Working <- labels1
		labels2Working <- labels2		
		
		for (i in 1:length(labels2Working)) { 		
			createLab[[i]] <- list()
			createQ <- paste("q", i, sep="")
			befehl <- "tkgrid("
			assign(createQ, tklabel(rbFrame, text=labels2Working[i], font=setFont(size="normal")))
			befehl <- paste(befehl, createQ, sep="")
			for (j in 1:length(labels2Working)) { 	
				if(corrAnswer[i]==j) {
					createLab[[i]][[j]] <- tklabel(rbFrame, text=labels1Working[j], font=setFont(size="normal"), background="#aaffaa")
					befehl <- paste(befehl, ",", "createLab[[",i,"]][[",j,"]]", sep="")
				}
			}
			befehl <- paste(befehl, ")", sep="")
			eval(parse(text=befehl))
			eval(parse(text=paste("tkgrid.configure(", createQ, ", sticky=\"e\")", sep="")))
		}			
		
		rightFrame <- tkframe(rb1Frame)
		plotFunction2 <- function() {
			b <- barplot(correctPerAnswer, xlab="", ylab="Share", main="Share of correct answers", xaxt="n",col="cornflowerblue")
			axis(1, at=b, labels=LETTERS[1:length(correctPerAnswer)], las=1)
		}
		
		filename <- paste(filename,".txt",sep="")
		if(file.exists(filename)) {
			erg <- read.table(filename, sep=",")
			nc <- length(erg[1,])
			nr <- length(erg[,1])
			erg <- apply(erg, 2, as.character)
			
			correctPerAnswer <- NULL			
			for (i in 1:nc) {
				if(nr > 1) 
					correctPerAnswer[i] <- length(which(erg[,i] == corrAnswerLett[i])) / nrow(erg)
				else 
					correctPerAnswer[i]	<- ifelse(erg[i] == corrAnswerLett[i], 1, 0)
			}
			
			img2 <- tkrplot(rightFrame, fun=plotFunction2, hscale=hscaleAns, vscale=vscaleAns)
			tkgrid(tklabel(rightFrame,text="Answers:", font=setFont(size="large")))
			tkgrid(img2)
			tkgrid(rbFrame,tklabel(rb1Frame,text="  "), rightFrame)
		} else 
			tkgrid(rbFrame)
		tkgrid(leftFrame)      
		tkgrid(tklabel(leftFrame, text=""));		
		
		buttonFrame <- tkframe(name)
		tkgrid(buttonFrame)		
		button <- tkbutton(buttonFrame, text="Close evaluation", command=function()tkdestroy(tt),fg="darkgreen")
		tkgrid(button)		
	}
)