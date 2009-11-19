# environment functions
Tassign <- function(x,value) assign(x,value,env=TGUIenv)
Tget <- function(x) get(x,env=TGUIenv)
Trm <- function(x) rm(list=x,envir=TGUIenv)
Texists <- function(x) exists(x,env=TGUIenv)
aTassign <- function(x,value) assign(x,value,env=adminTGUIenv)
aTget <- function(x) get(x,env=adminTGUIenv)
aTexists <- function(x) exists(x,env=adminTGUIenv)
aTrm <- function(x) rm(list=x,envir=adminTGUIenv)

# show an exit button
ExitButton <- defmacro(window=tt,frame=MainFrame,name,text="Close",
	expr={
		name <- tkframe(MainFrame)
		button <- tkbutton(	name, text=text,
							command= function() {            
										tkdestroy(window)
										rm(list=ls(env=TGUIenv),envir=TGUIenv)
									},
							fg="red")
		tkgrid(tklabel(name,text="  "))
		tkgrid(button)
	}
)

# set font by size, weight or slant
setFont <- function(family="tahoma", size, italic=FALSE, bold=FALSE) {
	if(is.character(size)) {
		if(!size %in% c("small", "normal", "large", "extralarge", "huge")) 
			stop("please specify a valid size!\n")
		else {
			if(size=="small") 		fontsize <- 10
			if(size=="normal") 		fontsize <- 12
			if(size=="large")  		fontsize <- 14
			if(size=="extralarge")  fontsize <- 16
			if(size=="huge")  		fontsize <- 18			
		}
	}
	# size is numeric
	else
		fontsize <- size
	
	if(italic==FALSE & bold==FALSE) 
		font <- tkfont.create(family=family, size=fontsize)
	if(italic==TRUE & bold==FALSE)
		font <- tkfont.create(family=family, size=fontsize, slant="italic")
	if(italic==FALSE & bold==TRUE) 
		font <- tkfont.create(family=family, size=fontsize, weight="bold")
	if(italic==TRUE & bold==TRUE)
		font <- tkfont.create(family=family, size=fontsize, slant="italic", weight="bold")
	return(font)
}

# open a pdf in the browser
showPDF <- function(filename) {
	filename <- paste(aTget("pathDoc"), fileName, sep="")
	if(Sys.info()[1]=="Windows")
		system(paste("rundll32 SHELL32.DLL,ShellExec_RunDLL \"", filename,"\"", sep=""), wait = FALSE)
	else
		system(paste("open", fileName, sep=" "), wait=FALSE)
}