.onAttach <- function(lib, pkg) {
	#if(!file.exists("adminExercise.txt")){
   	#	fco <- file("adminExercise.txt","w")
    #	close(fco)
  	#}
	### Some strings that can be used throughout the GUI's
	mainTitle 	<- "TGUI system"
	mainSub 	<- "Teaching GUI system"
	subTitle 	<- "Interactive Feedback- and Training-Tool for Teaching & Learning"
	usertitle 	<- "Student Interface"
	admintitle 	<- "Trainer Interface"
	developed1 	<- "TGUI Basic system developed by G. Dinges, M. Templ (2005)"
	developed2 	<- "Advanced and rewritten by G. Dinges, A. Kowarik, B. Meindl, M. Templ (2009)" 
	notice 		<- "Feedback is welcome! Visit www.statistik.at"	
	
	#setwd(tclvalue(tkchooseDirectory())) 
	##should be active in build for external TODO: tkchooseDirectory aktivieren
    version <- packageDescription("TGUICore")$Version
	
	#TGUIenv should be trashed after use by an exercise
    if (!any(search()=="TGUIenv")){
		  TGUIenv <- new.env()
      attach(TGUIenv)
    }
	
	#adminTGUIenv should never be trashed after use by an exercise
  if (!any(search()=="adminTGUIenv")){
    adminTGUIenv <- new.env()
    attach(adminTGUIenv)
  }
	cat("\n TGUI (Core) version",version,"has been loaded \n\n")

  	pathGUI <- getwd()
    aTassign("pathGUI",getwd())
	
	if(Sys.info()[1]=="Windows") {
		pathEtc <- paste(searchpaths()[grep("TGUICore", searchpaths())], "\\etc", sep="")
		pathDoc <- paste(searchpaths()[grep("TGUICore", searchpaths())], "\\doc\\", sep="")
		pathContents <- paste(pathEtc, "\\contents.csv", sep="")
	}			
	else {
		pathEtc <- paste(searchpaths()[grep("TGUICore", searchpaths())], "/etc", sep="")
		pathDoc <- paste(searchpaths()[grep("TGUICore", searchpaths())], "/doc/", sep="")
		pathContents <- paste(pathEtc, "/contents.csv", sep="")
	}

    contents <- read.table(pathContents,header=FALSE,sep=";",stringsAsFactors=FALSE)
    colnames(contents) <- c("Kurs","Teil","Funktion","Auswertung","Titel","Antwort")
	
	aTassign("contents",contents)
	aTassign("pathEtc", pathEtc);			aTassign("pathDoc", pathDoc)
	aTassign("mainTitle",mainTitle);		aTassign("subTitle", subTitle)
	aTassign("mainSub", mainSub);
	aTassign("usertitle", usertitle); 		aTassign("admintitle", admintitle)	
	aTassign("developed1", developed1); 	aTassign("developed2", developed2)
	aTassign("notice", notice)
  	aTassign("alwaysOn",vector())

	# activate all examples by default
	if(!file.exists("adminExercise.txt")) {
		dat <- as.character(read.table(file=paste(aTget("pathEtc"), "/contents.csv", sep=""), sep=";")[,3])
		dat <- as.character(sapply(dat, function(x) { substr(x, 1, nchar(x)-2) } ))
		write.table(dat, file="adminExercise.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)			
	}		
	
	  cat("Start the GUI for the participants with runGUI()\n and the GUI for the course leader with adminGUI()\n")
    cat("The window of the R console should be minimized while using the GUI.\n")
#  	runGUI()
}