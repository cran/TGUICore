Feedback <- defmacro(
    frame=MainFrame, 
    name,
    question1,
    question2=NULL,
    image=NULL,Answer=FALSE,
    filename,
    expr={
      if(!Answer){
        sendFeedback <- function(){
          setwd(aTget("pathGUI"))
          zz <- file(paste(filename,".txt",sep=""), "a")
          cat(1,"\n",file=zz)
          close(zz)
          for(j in 1:length(q2.text)){
            zz <- file(paste(filename,j,".txt",sep=""), "a")
            cat(tclvalue(tkget(q2.text[[j]],"0.0","end")),file=zz)
            cat("----------------------------------------------- \n",file=zz)
            close(zz)
          }
          tkdestroy(tt)
          rm(list=ls(env=TGUIenv),envir=TGUIenv)
        }
        
        name <- tkframe(frame)
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
        heights <- c(20,10,6,4,3)
        if(length(question2)>5){
          stop("More than 5 questions are not possible!!!")
        }else{
          height <- rep(heights[length(question2)],5)
        }
        # Draw the question
        QFrame <- tkframe(leftFrame)
        tkgrid(QFrame)
        q1 <- tklabel(QFrame,text=question1,font=setFont(size="extralarge", bold=TRUE))
        tkgrid(q1)
        tkgrid(tklabel(QFrame,text="   "))
        tkgrid.configure(q1, columnspan=2)
        q2Working <- question2
        q2.text <- list()
        for (j in 1:length(q2Working)) {
          q2 <- tklabel(QFrame,text=q2Working[j],font=setFont(size="extralarge"))
          tkgrid(q2)	
          tkgrid.configure(q2, columnspan=2)
          q2.text[[j]] <- tktext(QFrame,width=100,height=height[j],font=setFont(size="large"))
          tkgrid(q2.text[[j]])
          tkgrid.configure(q2.text[[j]], columnspan=2)
        }
        tkgrid(leftFrame)
        
        buttonFrame <- tkframe(name)
        tkgrid(tklabel(buttonFrame,text="  "))
        tkgrid(buttonFrame)		
        button <- tkbutton(buttonFrame, text="Submit answer", command=sendFeedback,fg="darkgreen")
        tkgrid(button)		
        
      }else if(Answer){
        closeFeedback <- function(){
          tkdestroy(tt)
          rm(list=ls(env=TGUIenv),envir=TGUIenv)
        }
        heights <- c(39,19,12,8,6)
        if(length(question2)>5){
          stop("More than 5 questions are not possible!!!")
        }else{
          height <- rep(heights[length(question2)],5)
        }
        name <- tkframe(frame)
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
        
        # Draw the question
        QFrame <- tkframe(leftFrame)
        tkgrid(QFrame)
        q1 <- tklabel(QFrame,text=question1,font=setFont(size="extralarge", bold=TRUE))
        tkgrid(q1)
        tkgrid(tklabel(QFrame,text="   "))
        q2Working <- question2
        q2.text <- list()
        scroll <- list()
        scrollbar <- list()
        for (j in 1:length(q2Working)) {
          q2 <- tklabel(QFrame,text=q2Working[j],font=setFont(size="extralarge"))
          tkgrid(q2)	
          if(file.exists(paste(filename,j,".txt",sep=""))){
            zz <- file(paste(filename,j,".txt",sep=""), "r")
            q2.answer <- readLines(zz)
            close(zz)
            eval(parse(text=paste(
                        "q2.text",j," <- tktext(QFrame,width=100,height=",height[j],",font=setFont(size=\"normal\"))"
                        ,sep="")))
            eval(parse(text=paste(
                        "scroll",j," <- function(...) tkyview(q2.text",j,",...)"
                        ,sep="")))
            eval(parse(text=paste(
                        "scrollbar",j," <- tkscrollbar(QFrame, repeatinterval = 5, command = scroll",j," )"
                        ,sep="")))
            
            eval(parse(text=paste(
                        "tkconfigure(q2.text",j,", yscrollcommand = function(...)  tkset(scrollbar",j,",...))"
                        ,sep="")))
            eval(parse(text=paste(
                        "tkgrid(q2.text",j,",scrollbar",j,")"
                        ,sep="")))
            for(i in 1:length(q2.answer)){
              cat(substring(q2.answer[i],1,1))
              eval(parse(text=paste(
                          "tkinsert(q2.text",j,",\"end\",paste(q2.answer[",i,"],\"\\n\"))"
                          ,sep="")))
            }
          }
        }
        tkgrid(leftFrame)
        
        buttonFrame <- tkframe(name)
        tkgrid(tklabel(buttonFrame,text="  "))
        tkgrid(buttonFrame)		
        button <- tkbutton(buttonFrame, text="Close feedback", command=closeFeedback,fg="blue")
        tkgrid(button)
        
        
        
      }
    }
)
