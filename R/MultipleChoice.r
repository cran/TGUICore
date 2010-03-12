MultipleChoice <- defmacro(
    frame=MainFrame, 
    name,
    question1,
    question2=NULL,
    labels,
    image=NULL,
    hscale=1.5,vscale=1,
    hscaleAns=1,vscaleAns=1,
    plotFunction=NULL,Answer=FALSE,note=NULL,
    dynSol=FALSE,
    filename,
    expr={
      if(Answer){
        name <- tkframe(frame)
        fn <- paste(filename,".txt",sep="")
        fileExists <- TRUE
        if(file.exists(fn)==TRUE) {
          x <- read.table(fn, sep=",")
          colnames(x) <- c("Question", "Answer")
          start <- min(x[,1])
          ende <- max(x[,1])
          erg <- rep(0,length(start:ende))
          for(i in start:ende){
            erg[i] <- sum(x[seq(i,nrow(x),by=ende),2]==1)
          }		  
        }
        else 
          fileExists <- FALSE
        
        contents <- aTget("contents")
        ues <- as.character(sapply(as.character(contents[,3]), function(x) { substr(x, 1, nchar(x)-2) } ))
        for(i in 1:length(ues)) {
          if(ues[i] == filename)
            uname <- contents[i,]
        }      
        
        leftFrame <- tkframe(name)
        tkgrid(tklabel(leftFrame, text=""));		
        
        # Draw image if it exists...
        if(!is.null(image)) {
          imageFrame <- tkframe(leftFrame)
          image1 <- tclVar()
          path <- aTget("pathDoc")
          path <- paste(path, image, ".gif", sep="")
          tcl("image", "create", "photo", image1, file=path)
          tkgrid(tklabel(imageFrame, image=image1, bg="grey"), sticky="n")
          tkgrid(imageFrame)
          
          tkgrid(tklabel(name, text=""))
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
        
        # Draw the queckboxes with labels
        cb1Frame <- tkframe(leftFrame)
        cbFrame <- tkframe(cb1Frame)
        tkgrid(cb1Frame)
        labelsWorking <- labels
        
        if(dynSol==FALSE) {
          col <- rep("#cccccc",length(labelsWorking))
          for(i in 1:length(labelsWorking)) {
            if(eval(parse(text=paste(i,"%in%c(",as.character(uname[6]),")",sep="")))){
              ll <- tklabel(cbFrame,text=paste(LETTERS[i],") ",labelsWorking[i],sep=""), font=setFont(size="normal", bold=TRUE),background="#aaffaa")
              col[i] <- "#aaffaa"
            }
            else 
              ll <- tklabel(cbFrame,text=paste(LETTERS[i],") ",labelsWorking[i],sep=""), font=setFont(size="normal"))
            
            tkgrid( ll) 
            tkgrid.configure(ll, sticky="w")						
          }
        }
        else {
          for(i in 1:length(labelsWorking)){
            ll <- tklabel(cbFrame,text=paste(LETTERS[i],") ",labelsWorking[i],sep=""), font=setFont(size="normal"))
            tkgrid(ll)	
            tkgrid.configure(ll, sticky="w")				  
          }
        }
        rightFrame <- tkframe(cb1Frame)
        
        plotFunction2 <- function() {
          if(dynSol==FALSE) 
            b <- barplot(erg, xlab="", ylab="Frequency", main="", xaxt="n",col=col)
          else
            b <- barplot(erg, xlab="", ylab="Frequency", main="", xaxt="n",col="cornflowerblue")
          
          axis(1, at=b, labels=LETTERS[start:ende], las=1)
        }
        
        if(fileExists==TRUE) {
          img2 <- tkrplot(rightFrame,fun=plotFunction2,hscale=hscaleAns,vscale=vscaleAns)
          tkgrid(tklabel(rightFrame,text="Answers:",font=setFont(size="large")))
          tkgrid(img2)
          tkgrid(cbFrame,tklabel(cb1Frame,text="  "), rightFrame)
          tkgrid(leftFrame)
          tkgrid.configure(rightFrame,sticky="e")  
        }
        else {
          tkgrid(cbFrame,tklabel(cb1Frame,text="  "))
          tkgrid(leftFrame)
          
          tkgrid(tklabel(name, text=""));		
          tkgrid(tklabel(name, text=""));		
          tkgrid(tklabel(name, text="No answers are available yet for this question!", font=setFont(size="large", bold=TRUE)))
        }   
        
        tkgrid(tklabel(name, text=""));		
        buttonFrame <- tkframe(name)
        tkgrid(buttonFrame)		
        button <- tkbutton(buttonFrame, text="Close Evaluation", command=function()tkdestroy(tt),fg="darkgreen")
        tkgrid(button)		  
      }else if(!Answer){
        sendCheckboxData <- function() {
          setwd(aTget("pathGUI"))			
          
          lengthLabels <- length(labels)
          erg <- NULL
          
          for (i in 1:lengthLabels) {
            vars <- c(vars, paste("V",i,sep=""))
            erg <- c(erg, as.numeric(tclvalue(Tget(vars[i])))) 
          }
          
          x <- data.frame(frage=1:lengthLabels, antwort=erg)	
          write.table(x, file=filename, sep=",", append = TRUE, row.names = FALSE, col.names = FALSE)
          tkdestroy(tt)
          rm(list=ls(env=TGUIenv),envir=TGUIenv)
        }	
        
        filename <- paste(filename, ".txt", sep="")
        name <- tkframe(frame)
        
        tkgrid(tklabel(name, text=""));		
        
        # Draw image if it exists...
        if(!is.null(image)) {
          imageFrame <- tkframe(name)
          image1 <- tclVar()
          path <- aTget("pathDoc")
          path <- paste(path, image, ".gif", sep="")
          tcl("image","create","photo",image1,file=path)
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
        
        # Draw the question
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
        
        # Draw the queckboxes with labels
        cb <- list()
        vars <- NULL
        cbFrame <- tkframe(name)
        tkgrid(cbFrame)
        labelsWorking <- labels
        for(i in 1:length(labelsWorking)){
          vars <- c(vars, paste("V",i,sep=""))
          cb[[i]] <- tkcheckbutton(cbFrame)
          
          ll <- tklabel(cbFrame,text=paste(LETTERS[i],") ",labelsWorking[i]), font=setFont(size="normal"))
          tkgrid(cb[[i]], ll) 
          tkgrid.configure(cb[[i]], sticky="e")	
          tkgrid.configure(ll, sticky="w")						
        }		
        
        Tassign("cb",cb)
        Tassign("vars",vars)
        for(i in 1:length(labels)){
          Tassign(vars[i], tclVar("0"))
          Tassign("i", i)
          evalq(tkconfigure(cb[[i]], variable = eval(parse(text=vars[i]))),env=TGUIenv)
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
        button <- tkbutton(buttonFrame, text="Submit answer", command=sendCheckboxData,fg="darkgreen")
        tkgrid(button)  
        
      }
    }
)
