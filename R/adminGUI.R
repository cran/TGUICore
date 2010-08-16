pw <- function() { return("defaultPW") }
enableExercise <- defmacro( frame=MainFrame,
							name,
							exercise,
							default="0",
							titles,
	expr={	
		fileExists <- file.exists(paste(aTget("pathGUI"),"/adminExercise.txt",sep="")) & length(readLines(paste(aTget("pathGUI"),"/adminExercise.txt",sep="")) > 0)
		if(fileExists==FALSE) {
			file.create(paste(aTget("pathGUI"),"/adminExercise.txt",sep=""))
			write.table(file=paste(aTget("pathGUI"),"/adminExercise.txt",sep=""),exercise[[1]],row.names = FALSE,col.names = FALSE,quote=FALSE)
		}		
		
		contents <- aTget("contents")	
		contents$Funktion2 <- as.character(contents$Funktion)
		contents$Funktion2 <- as.vector(sapply(contents$Funktion2, function(x) { substr(x, 1, nchar(x)-2) } ) )
		contentsSpl <- split(contents, as.factor(contents[,1]))

	  	examplesAE <- read.table(paste(aTget("pathGUI"),"/adminExercise.txt",sep=""), sep=",", header=F)		
	  	examplesAE <- as.character(examplesAE[,1])				  
  		vorhExamples <- returnExamplesWithAusw() 	
		
      	name <- tkframe(frame)
      	ExFrame <- ExCheck <- ExVar <- list()

      	fontHeading <- setFont(size="small")
      	fontHeading1 <- setFont(size="normal")
      
		ExVector <- CourseCheck <- CourseVar <- list()
  	
    	for(i in 1:length(exercise)) {
	        ExVector[[i]] <- vector()
	        ExFrame[[i]] <- tkframe(name)
	        ExCheck[[i]] <- list()
	        fun <- paste("function()setAll(",i,")",sep="")
	        CourseCheck[[i]] <- tkcheckbutton(ExFrame[[i]], command = eval(parse(text=fun)))
	        CourseVar[[i]] <- tclVar(default)
	        tkconfigure(CourseCheck[[i]], variable = CourseVar[[i]])
	        tkgrid(tklabel(ExFrame[[i]], text=" "))
	        tkgrid(tklabel(ExFrame[[i]], text=names(exercise)[i], font=fontHeading1), CourseCheck[[i]])
	        tkgrid(tklabel(ExFrame[[i]], text=" "))
			
        	for(j in 1:length(exercise[[i]])) {
          		ExCheck[[i]][[j]] <- tkcheckbutton(ExFrame[[i]], command = writeExercise)
          		tkgrid(tklabel(ExFrame[[i]],text=exerciseTitle[[i]][j],font=fontHeading),ExCheck[[i]][[j]])        
        	}
        	aTassign("ExCheck",ExCheck)
			
			for(j in 1:length(ExCheck[[i]])) {
				tmp <- as.character(contentsSpl[[i]][j,"Funktion"])
				tmp <- substr(tmp, 1, nchar(tmp)-2)
				#if(tmp %in% vorhExamples)
				#TODO: stimmt das wirklich so??
				if(tmp %in% vorhExamples | default==1)
					ExVar[[length(ExVar)+1]]  <- tclVar("1")
				else 
					ExVar[[length(ExVar)+1]]  <- tclVar("0") 
				
          		ExVector[[i]] <- c(ExVector[[i]],length(ExVar))          
          		tkconfigure(ExCheck[[i]][[j]], variable =ExVar[[length(ExVar)]])        
      		}
		}
		
		aTassign("CourseVar", CourseVar)
      	aTassign("ExVar", ExVar)
      	aTassign("ExVector", ExVector)
      	command <- "tkgrid("
      	
		for(i in 1:length(ExFrame)) {
        	if(i!=length(ExFrame))
        		command <- paste(command,"ExFrame[[",i,"]],",sep="")
       	 	else
          		command <- paste(command,"ExFrame[[",i,"]])",sep="")
      	}
      	eval(parse(text=command))
     	
		for(i in 1:length(ExFrame))	{
        	command <- paste("tkgrid.configure(ExFrame[[",i,"]],sticky=\"n\")",sep="")
        	eval(parse(text=command))
      	}
 	}
)

`adminGUI` <- function(pwd="defaultPW",evaluierung = FALSE, enableAll=FALSE) {
  	if(pwd==pw()) {
		returnExamplesWithAusw <- function() {
			contents <- aTget("contents")	
		  	examplesC <- as.character(contents$Funktion)
		  	examplesC <- as.vector(sapply(examplesC, function(x) { substr(x, 1, nchar(x)-2) } ) )
		  
		  	examplesAE <- read.table(paste(aTget("pathGUI"),"/adminExercise.txt",sep=""), sep=",", header=F)
		  	examplesAE <- as.character(examplesAE[,1])			
		  	examples <- examplesC[which(examplesC %in% examplesAE)]
		    return(examples)
	  	}
		callAns <- function(command) {
      		split_command <- unlist(strsplit(command,"\\("))
			command <- paste(split_command[1],"(eval=TRUE",split_command[2],sep="")
      		eval(parse(text=command))
    	}
    	callExercise <- function(command) {
      		split_command <- unlist(strsplit(command,"\\("))
      		eval(parse(text=command))
    	}
      	writeExercise <- function() {
        	exercise <- aTget("exercise")
        	ExVar <- aTget("ExVar")
        	TF_ex <- vector()
        	for(i in 1:length(ExVar)){
          		TF_ex[i] <- as.numeric(tclvalue(ExVar[[i]]))==1            
        	}
        	out <- exercise[TF_ex]
        	attributes(out) <- list()
        	if(!is.null(aTget("alwaysOnExercise")))
          		out <- c(out,aTget("alwaysOnExercise"))
        	write.table(out,file=paste(aTget("pathGUI"),"/adminExercise.txt",sep=""),row.names=FALSE,col.names=FALSE,quote=FALSE)
      	}
		setAll <- function(i) {
      		CourseValue <- tclvalue(aTget("CourseVar")[[i]])
      		ExVector <- aTget("ExVector")[[i]]
      		ExVar <- aTget("ExVar")
      		for(e in ExVector){
        		tclvalue(ExVar[[e]]) <- CourseValue
      		}
      		writeExercise()
    	}
		
    	pathGUI <- aTget("pathGUI")
    	tt <- tktoplevel()
		tkwm.title(tt,aTget("admintitle"))
    	if(Sys.info()[1]=="Windows")
      		tcl("wm", "state", tt,"zoomed")
    	else
      		tcl("wm", "attributes", tt, "-zoomed", TRUE)

    	topMenu <- tkmenu(tt)
    	tkconfigure(tt,menu=topMenu)
    
	    fontHeading <- tkfont.create(family="tahoma",size=12,weight="bold",slant="italic")
	    fontHeading1 <- tkfont.create(family="tahoma",size=13)
	    fontHeading1b <- tkfont.create(family="tahoma",size=11)
	    fontHeading1c <- tkfont.create(family="tahoma",size=11,weight="bold")
	    fontHeading2 <- tkfont.create(family="tahoma",size=16)
	    fontHeading3 <- tkfont.create(family="tahoma",size=25)
	    fontHeading4 <- tkfont.create(family="tahoma",size=55)
    	indKurs <- 1
    	indTeil <- 1
		
    	KursMenu <- TeilMenu <- list()

    	contents <- aTget("contents")
    	for(i in 1:nrow(contents)) {
	      	if(i==1) {
		        KursMenu[[indKurs]] <- tkmenu(topMenu,tearoff=FALSE)
            if(!contents[i,1]%in%aTget("alwaysOn")){
              tkadd(topMenu,"cascade",label=paste("Course ",contents[i,1],"  ",sep=""), font=fontHeading, menu=KursMenu[[indKurs]])
		          TeilMenu[[indTeil]] <- tkmenu(topMenu,tearoff=FALSE)
		          tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
            }else{
              tkadd(topMenu,"cascade",label=paste(contents[i,1],"  ",sep=""), font=fontHeading, menu=KursMenu[[indKurs]])
              TeilMenu[[indTeil]] <- tkmenu(topMenu,tearoff=FALSE)
              tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
            }
            ExCommand <- paste("function(){callExercise(\"",as.character(contents[i,3]),"\")}",sep="")
            tkadd(TeilMenu[[indTeil]],"command",label=as.character(contents[i,5]), font=fontHeading1b,
	            command=eval(parse(text=ExCommand)) ) 
	      	} 
			else {
	        	if(contents[i,1]!=contents[i-1,1]) {
	          		indKurs <- indKurs + 1
	          		KursMenu[[indKurs]] <- tkmenu(topMenu,tearoff=FALSE)
                	if(!contents[i,1]%in%aTget("alwaysOn")){
                  		tkadd(topMenu,"cascade",label=paste("Course ",contents[i,1],"  ",sep=""), font=fontHeading, menu=KursMenu[[indKurs]])
	          		  	indTeil <-indTeil + 1
	          		  	TeilMenu[[indTeil]] <- tkmenu(topMenu,tearoff=FALSE)
	          		  	tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
                	}
					else {
                  		tkadd(topMenu,"cascade",label=paste(contents[i,1],"  ",sep=""), font=fontHeading, menu=KursMenu[[indKurs]])
                  		indTeil <-indTeil + 1
                  		TeilMenu[[indTeil]] <- tkmenu(topMenu,tearoff=FALSE)
                  		tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents[i,2]," ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
                	}
	        	}
				else if(contents[i,2]!=contents[i-1,2]) {
	          		indTeil <-indTeil + 1
	          		TeilMenu[[indTeil]] <- tkmenu(topMenu,tearoff=FALSE)
                	if(!contents[i,1]%in%aTget("alwaysOn")) 
	          		  	tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
                	else
                  		tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
	        	}
	        	ExCommand <- paste("function(){callExercise(\"",as.character(contents[i,3]),"\")}",sep="")
	        	tkadd(TeilMenu[[indTeil]],"command",label=as.character(contents[i,5]), font=fontHeading1b, command=eval(parse(text=ExCommand)) )
	      	}
		}
	    contents1 <- contents[contents[,4]==1,]
	    for(i in 1:nrow(contents1)) {
	     	if(i==1) {
	        	KursMenu[[indKurs]] <- tkmenu(topMenu,tearoff=FALSE)
            	tkadd(topMenu,"cascade",label=paste("Evaluation ",contents1[i,1],"  ",sep=""), font=fontHeading, menu=KursMenu[[indKurs]])
	        	TeilMenu[[indTeil]] <- tkmenu(topMenu,tearoff=FALSE)
	        	tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents1[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
	        	ExCommand <- paste("function(){callAns(\"",as.character(contents1[i,3]),"\")}",sep="")
	        	tkadd(TeilMenu[[indTeil]],"command",label=as.character(contents1[i,5]), font=fontHeading1b, command=eval(parse(text=ExCommand)) )
	      	}
			else {
	        	if(contents1[i,1]!=contents1[i-1,1]) {
	          		indKurs <- indKurs + 1
	          		KursMenu[[indKurs]] <- tkmenu(topMenu,tearoff=FALSE)
                	tkadd(topMenu,"cascade",label=paste("Evaluation ",contents1[i,1],"  ",sep=""), font=fontHeading, menu=KursMenu[[indKurs]])
	          		indTeil <-indTeil + 1
	          		TeilMenu[[indTeil]] <- tkmenu(topMenu,tearoff=FALSE)
	          		tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents1[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
	        	}
				else if(contents1[i,2]!=contents1[i-1,2]) {
	          		indTeil <-indTeil + 1
	          		TeilMenu[[indTeil]] <- tkmenu(topMenu,tearoff=FALSE)
	          		tkadd(KursMenu[[indKurs]],"cascade",label=paste("  ",contents1[i,2],"  ",sep=""), font=fontHeading1c, menu=TeilMenu[[indTeil]])
	        	}
	        	ExCommand <- paste("function(){callAns(\"",as.character(contents1[i,3]),"\")}",sep="")
	        	tkadd(TeilMenu[[indTeil]],"command",label=as.character(contents1[i,5]), font=fontHeading1b, command=eval(parse(text=ExCommand)) )
	      	}
	    }
		
		### add the Counter
		counter <- tkmenu(topMenu, tearoff=FALSE)		
		tkadd(counter,"command",label="Show dynamic counter", command=dynCounterCall) 
	    tkadd(topMenu,"cascade",label="Counter",menu=counter)
		
		### add checkboxes
	    exercise <- exerciseTitle <- list()
	    ex_vec <- ex_vec_title <- vector()
      	for(i in 1:nrow(contents)) {
        	if(!contents[i,1]%in%aTget("alwaysOn")){
          		if(i>1) {
            		if(contents[i,1]!=contents[i-1,1]){
#              			exercise[[as.character(contents[i-1,1])]] <- ex_vec
#              			exerciseTitle[[as.character(contents[i-1,1])]] <- ex_vec_title
              			ex_vec <- vector()
              			ex_vec_title <- vector()
            		}
          		}
          		ex_vec <- c(ex_vec,unlist(strsplit(as.character(contents[i,3]),"\\("))[1])
          		ex_vec_title <- c(ex_vec_title,sub("Check ","",as.character(contents[i,5])))
        	}
        	if(!contents[i,1]%in%aTget("alwaysOn")){
          		exercise[[as.character(contents[i,1])]] <- ex_vec
          		exerciseTitle[[as.character(contents[i,1])]] <- ex_vec_title
        	}
		}
      	ex_on <- unlist(strsplit(contents[contents[,1]%in%aTget("alwaysOn"),3],"\\("))
      
      	if(length(ex_on)==0)
        	ex_on <- NULL
		else 
			ex_on <- ex_on[seq(1,length(ex_on),by=2)]
		
      	aTassign("alwaysOnExercise",ex_on)
    	aTassign("exercise",unlist(exercise))
    	aTassign("exerciseTitle",unlist(exerciseTitle))
	
	    if(enableAll)
	    	default <- "1"
	    else
	      	default <- "0"

    	enableExercise(frame=tt,name=adminExer,exercise=exercise,default=default,titles=exerciseTitle)
    	tkgrid(adminExer)
		writeExercise()
  	}
	else
    	tkmessageBox(title="For course instructors only!", message="This function is only available to course instructors.")
}