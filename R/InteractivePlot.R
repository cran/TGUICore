InteractivePlot <- defmacro(
	frame=MainFrame, name, plot.function, click.function=NULL,
	hscale=2, vscale=2,
    slider=NULL, slider_lim=NULL, slider_start=NULL, slider_res=NULL, slider_label=NULL, slider_header=NULL,
    checkbox=NULL, checkbox_start=NULL, checkbox_header=NULL, checkbox_label=NULL, checkbox_twoCols=FALSE,
    radio=NULL, radio_label=NULL, radio_header=NULL, radio_value=NULL,
    header=NULL,
	design="leftright",
    
	expr={      
		if(is.list(checkbox)) {
			if(length(checkbox) != length(checkbox_header))
				stop("Falsche Dimensionen\n")
		}
		
    	fontHeading 	<- setFont(size="normal")
		fontHeadingBold <- setFont(size="normal", bold=TRUE)
    	fontHeading1 	<- setFont(size="large")
		
    	name <- tkframe(frame)
		tkgrid(tklabel(name, text=""))
    	if(!is.null(header))
      		tkgrid(tklabel(name,text=header,font=fontHeading1,background="white"))
    
		PlotFrame <- tkframe(name)
    	img <- tkrplot(PlotFrame,plot.function,hscale=hscale,vscale=vscale)
    	tkgrid(img)
    
		if(!is.null(click.function)) {
      		tkbind(img, "<Button-1>",click.function)
      		tkconfigure(img,cursor="hand2")     
    	}
      
    	ControlFrame <- tkframe(name)
		
		### slider
    	if(!is.null(slider)) {
      		SliderFrame <- tkframe(ControlFrame)
      		if(!is.null(slider_header))
        		tkgrid(tklabel(SliderFrame,text=slider_header,font=fontHeadingBold))
      		sl <- list()
      		sf <- list()
      		for(i in 1:length(slider)) {
        		sl[[i]] <- tkscale(SliderFrame, command = function(...)tkrreplot(img), from = slider_lim[[i]][1],
            	to = slider_lim[[i]][2], orient = "horiz", resolution = slider_res[i], showvalue = T)
        		if(!is.null(slider_label)) 
          			tkgrid(tklabel(SliderFrame,text=slider_label[i],font=fontHeading),tklabel(SliderFrame,text="   "),sl[[i]]) 
        		else
          			tkgrid(sl[[i]])
      		}
      		Tassign("sl",sl)
      		for(i in 1:length(slider)) {
        		Tassign(slider[i],tclVar(slider_start[i]))
       		 	Tassign("i",i)
        		evalq(tkconfigure(sl[[i]], variable = eval(parse(text=slider[i]))),env=as.environment(which(search()=="TGUIenv")))
      		}
      		tkgrid(SliderFrame)
    	}
		
		### checkboxes
    	if(!is.null(checkbox)) {
			# blocks 
			if(is.list(checkbox)) {
				CheckboxFrame <- tkframe(ControlFrame)
				tkgrid(tklabel(CheckboxFrame, text="   "))
				cl <- list()	
				tl <- list()
				cFrame <- tkframe(CheckboxFrame)
				for(i in 1:length(checkbox)) {
					cl[[i]] <- list()
					tl[[i]] <- list()					
					
					# two columns
					if(checkbox_twoCols==TRUE) {
						if(!is.null(checkbox_header[i])) 
							tkgrid(tklabel(cFrame, text=checkbox_header[i], font=fontHeadingBold), columnspan=4)					
						
						nrBoxes <- length(checkbox_label[[i]])
						nrRows <- ceiling(nrBoxes/2)
						fullLastRow <- ifelse(nrBoxes %%2 == 0, TRUE, FALSE)
						counter <- 1
						for (z in 1:nrRows) {
							cl[[i]][[counter]] <- tkcheckbutton(cFrame, command = function(...)tkrreplot(img))
							tl[[i]][[counter]] <- tklabel(cFrame, text=checkbox_label[[i]][counter], font=fontHeading)
							
							counter <- counter + 1
							if(z==nrRows & fullLastRow==FALSE) {
								cl[[i]][[counter]] <- tklabel(cFrame, text="", font=fontHeading)
								tl[[i]][[counter]] <- tklabel(cFrame, text="", font=fontHeading)
							}
							else {
								cl[[i]][[counter]] <- tkcheckbutton(cFrame, command = function(...)tkrreplot(img))
								tl[[i]][[counter]] <- tklabel(cFrame,text=checkbox_label[[i]][counter], font=fontHeading)
							}
							
							tkgrid(tl[[i]][[counter-1]], cl[[i]][[counter-1]], tl[[i]][[counter]], cl[[i]][[counter]])
							tkgrid.configure(tl[[i]][[counter-1]], column=0, columnspan=1, sticky="e")
							tkgrid.configure(cl[[i]][[counter-1]], column=1, columnspan=1, sticky="w")
							tkgrid.configure(tl[[i]][[counter]], column=2, columnspan=1, sticky="e")
							tkgrid.configure(cl[[i]][[counter]], column=3, columnspan=1, sticky="w")
							
							counter <- counter + 1
						}						
					}					
					# one column
					else {
						if(!is.null(checkbox_header[i])) 
							tkgrid(tklabel(cFrame, text=checkbox_header[i], font=fontHeadingBold), columnspan=2)					
						for (j in 1:length(checkbox_label[[i]])) {
							cl[[i]][[j]] <- tkcheckbutton(cFrame, command = function(...)tkrreplot(img))
							tl[[i]][[j]] <- tklabel(cFrame,text=checkbox_label[[i]][[j]], font=fontHeading)
							tkgrid(tl[[i]][[j]], cl[[i]][[j]])					
						}
						tkgrid(tklabel(cFrame, text="   "))						
					}					
				}
			
				Tassign("cl",cl)
				for(i in 1:length(checkbox)) {
					for (j in 1:length(checkbox[[i]])) {
						Tassign(checkbox[[i]][[j]], tclVar(checkbox_start[[i]][[j]]))
						Tassign("i",i)
						Tassign("j",j)
						evalq(tkconfigure(cl[[i]][[j]], variable = eval(parse(text=checkbox[[i]][[j]]))),env=as.environment(which(search()=="TGUIenv")))
					}
				}
				tkgrid(cFrame)
				tkgrid(CheckboxFrame)
			}		
			
			# no blocks
			else {
				CheckboxFrame <- tkframe(ControlFrame)
				tkgrid(tklabel(CheckboxFrame,text="   "))

				cl <- list()
				tl <- list()
				cFrame <- tkframe(CheckboxFrame)				
				
				# two columns
				if(checkbox_twoCols==TRUE) {
					if(!is.null(checkbox_header))
						tkgrid(tklabel(CheckboxFrame, text=checkbox_header, font=fontHeadingBold), columnspan=4)
					
					nrBoxes <- length(checkbox)
					nrRows <- ceiling(nrBoxes/2)
					fullLastRow <- ifelse(nrBoxes %%2 == 0, TRUE, FALSE)						
					counter <- 1
					for (z in 1:nrRows) {
						cl[[counter]] <- tkcheckbutton(cFrame, command = function(...)tkrreplot(img))
						tl[[counter]] <- tklabel(cFrame, text=checkbox_label[counter], font=fontHeading)
						
						counter <- counter + 1
						if(z==nrRows & fullLastRow==FALSE) {
							cl[[counter]] <- tklabel(cFrame, text="", font=fontHeading)
							tl[[counter]] <- tklabel(cFrame, text="", font=fontHeading)
						}
						else {
							cl[[counter]] <- tkcheckbutton(cFrame, command = function(...)tkrreplot(img))
							tl[[counter]] <- tklabel(cFrame,text=checkbox_label[counter], font=fontHeading)
						}
						
						tkgrid(tl[[counter-1]], cl[[counter-1]], tl[[counter]], cl[[counter]])
						tkgrid.configure(tl[[counter-1]], column=0, columnspan=1, sticky="e")
						tkgrid.configure(cl[[counter-1]], column=1, columnspan=1, sticky="w")
						tkgrid.configure(tl[[counter]], column=2, columnspan=1, sticky="e")
						tkgrid.configure(cl[[counter]], column=3, columnspan=1, sticky="w")

						counter <- counter + 1
					}
					tkgrid(tklabel(cFrame, text="   "))		
				}				
				# One Column
				else {
					if(!is.null(checkbox_header))
						tkgrid(tklabel(CheckboxFrame, text=checkbox_header, font=fontHeadingBold), columnspan=2)
	
					for(i in 1:length(checkbox)) {
						tl[[i]] <- tklabel(cFrame, text=checkbox_label[i], font=fontHeading)
						cl[[i]] <- tkcheckbutton(cFrame, command = function(...) tkrreplot(img))
						tkgrid(tl[[i]],cl[[i]])    
						tkgrid.configure(tl[[i]], column=0, columnspan=1, sticky="e")
						tkgrid.configure(cl[[i]], column=1, columnspan=1, sticky="e")
					}					
				}
					
				Tassign("cl",cl)
				for(i in 1:length(checkbox)) {
					cat("i:", i,"\n")
					Tassign(checkbox[i],tclVar(checkbox_start[i]))
					Tassign("i",i)
					evalq(tkconfigure(cl[[i]], variable = eval(parse(text=checkbox[i]))), env=as.environment(which(search()=="TGUIenv")))
				}
				tkgrid(cFrame)
				tkgrid(CheckboxFrame)				
			}
	   	}
		
		### radio-buttons
    	if(!is.null(radio)) {
			radio2 <- radio
			Tassign("radio2", radio2)
      		RadioFrame <- tkframe(ControlFrame)
		
      		if(length(radio2)==1) {
        		if(!is.null(radio_header))
          			tkgrid(tklabel(RadioFrame,text=radio_header,font=fontHeadingBold), columnspan=2)
        		
				rl <- list()
				rLab <- list()
        		Tassign(radio2,tclVar(1))
        		for(i in 1:length(radio_label)) {
					rLab[[i]] 	<- tklabel(RadioFrame, text=radio_label[i], font=fontHeading)
					rl[[i]] 	<- tkradiobutton(RadioFrame, text="", font=fontHeading,value=radio_value[i],command=function(...)tkrreplot(img))
					tkgrid(rLab[[i]], rl[[i]])
					tkgrid.configure(rLab[[i]], columnspan=1, column=0, sticky="e")	
					tkgrid.configure(rl[[i]], 	columnspan=1, column=1, sticky="w")	
        		}
				
        		Tassign("rl",rl)
        		for(i in 1:length(radio_label)) {
          			Tassign("i",i)
          			evalq(tkconfigure(rl[[i]], variable = eval(parse(text=radio2))),env=as.environment(which(search()=="TGUIenv")))
        		}
      		}		
			
			else {				
        		for(r in 1:length(radio2)) {
          			if(!is.null(radio_header[r]))
            			tkgrid(tklabel(RadioFrame,text=radio_header[r],font=fontHeadingBold), columnspan=2)
          			
					rl <- list()
					rLab <- list()					
          			Tassign(radio2[r], tclVar(1))
          			for(i in 1:length(radio_label[[r]])) {
						rl[[i]] 	<- tkradiobutton(RadioFrame,text="", font=fontHeading, value=radio_value[[r]][i], command=function(...)tkrreplot(img))
						rLab[[i]]	<- tklabel(RadioFrame, text=radio_label[[r]][i], font=fontHeading)
						
						tkgrid(rLab[[i]], rl[[i]])
						tkgrid.configure(rLab[[i]], columnspan=1, column=0, sticky="e")	
						tkgrid.configure(rl[[i]], 	columnspan=1, column=1, sticky="w")	
          			}
          			Tassign("rl",rl)
          			for(i in 1:length(radio_label[[r]])) {
            			Tassign("i",i)
            			Tassign("r",r)
            			evalq(tkconfigure(rl[[i]], variable = eval(parse(text=radio2[[r]]))),env=as.environment(which(search()=="TGUIenv")))
          			}
        		}                
      		}	
      		tkgrid(RadioFrame)
    	}
		
		### design
    	if(design=="leftright")
      		tkgrid(PlotFrame,tklabel(name,text="  "), ControlFrame)     
   		else {
      		tkgrid(PlotFrame)
      		tkgrid(ControlFrame)      
    	}
	})
