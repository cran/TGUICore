SingleChoice <- defmacro(frame=MainFrame, 
    name,
    question1,
    question2=NULL,
    labels,
    image=NULL,
    hscale=1.5,vscale=1,
    hscaleAns=1,vscaleAns=1,
    plotFunction=NULL,
    filename,note=NULL,Answer=FALSE,
    dynSol=FALSE,
    expr={
      if(Answer){
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
      else if(!Answer){
        sendRadiobuttonData <- function() {
          setwd(aTget("pathGUI"))				
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
    }
)

