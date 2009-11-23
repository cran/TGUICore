`interactive_2_1` <- function(eval=FALSE) {
	plotInteractive3 <- function() {
		qq.plot <- function (x, distribution = "norm", ylab = deparse(substitute(x)),
				xlab = paste(distribution, "quantiles"), main = "", las = par("las"),
				envelope = 0.95, labels = FALSE, col = palette()[2], lwd = 2,
				pch = 1, line = c("quartiles", "robust", "none"), ...) {
			
			result <- NULL
			line <- match.arg(line)
			good <- !is.na(x)
			ord <- order(x[good])
			ord.x <- x[good][ord]
			q.function <- eval(parse(text = paste("q", distribution, sep = "")))
			d.function <- eval(parse(text = paste("d", distribution, sep = "")))
			n <- length(ord.x)
			P <- ppoints(n)
			z <- q.function(P, ...)
			plot(z, ord.x, xlab = xlab, ylab = ylab, main = main, las = las, col = col, pch = pch)
			if (line == "quartiles") {
				Q.x <- quantile(ord.x, c(0.25, 0.75))
				Q.z <- q.function(c(0.25, 0.75), ...)
				b <- (Q.x[2] - Q.x[1])/(Q.z[2] - Q.z[1])
				a <- Q.x[1] - b * Q.z[1]
				abline(a, b, col = col, lwd = lwd)
			}
			if (line == "robust") {
				coef <- coefficients(rlm(ord.x ~ z))
				a <- coef[1]
				b <- coef[2]
				abline(a, b)
			}
			if (line != "none" & envelope != FALSE) {
				zz <- qnorm(1 - (1 - envelope)/2)
				SE <- (b/d.function(z, ...)) * sqrt(P * (1 - P)/n)
				fit.value <- a + b * z
				upper <- fit.value + zz * SE
				lower <- fit.value - zz * SE
				lines(z, upper, lty = 2, lwd = lwd/2, col = col)
				lines(z, lower, lty = 2, lwd = lwd/2, col = col)
			}
			if (labels[1] == TRUE & length(labels) == 1)
				labels <- seq(along = z)
			if (labels[1] != FALSE) {
				selected <- identify(z, ord.x, labels[good][ord])
				result <- seq(along = x)[good][ord][selected]
			}
			if (is.null(result))
				invisible(result)
			else 
				sort(result)
		}
		
		par(bg="white")
		type <- c("norm","lnorm","exp")
		if(Texists("distr")){
			distr <- type[as.numeric(evalq(tclvalue(distr), env = TGUIenv))]
			data <- as.numeric(evalq(tclvalue(data), env = TGUIenv))
			n <- as.numeric(evalq(tclvalue(n), env = TGUIenv))
		} 
		else {
			distr <- type[1]
			data <- 1
			n <- 100
		}
		z <- list()
    
		if(data==1) {
      set.seed(12232451)
      z[[1]] = rnorm(n)
			attr(z[[1]], "message") = "normal"
		}
		if(data==2) {
      set.seed(12341)
      z[[1]] = exp(rnorm(n))
			attr(z[[1]], "message") = "log-normal"
		}
		if(data==3) {
      set.seed(1556232541)
      z[[1]] = rexp(n)#, runif(1,0.5,1.5))
			attr(z[[1]], "message") = "exp"
		}
		z[[1]] = round(z[[1]], 4)
		z <- data.frame(z)
		qq.plot(z, dist=distr)
	}

	exerciseTitle <-"Exercise: interactive QQ-plots"
	plotHeader <- "Please try to specify the distribution of three empirical datasets given above!"
	plotSliderLabel <- "Sample size:"
	plotSliderLim <- list(c(10,1000))
	plotRadioHeader <- c(	"Please choose one of the three available empirical datasets!",
							"Base-distribution of the QQ-plot")
	plotRadio <- c("data", "distr")
	plotRadioLabel <- list(1:3, c("Normal","Log-Normal","Exponential")) 
	plotValues <- list(1:3,1:3)
	plotDesign <- "leftright"
	
	q1 <- "Which mapping of the empircal datasets with available distributions is correct?"
	q2 <- "Use the QQ-Plots to find the correct answer!"
	labs <- c(	"1=Exponential, 2=Normal, 3=Log-Normal",
			"1=Normal, 2=Normal,3=Normal",
			"1=Normal, 2=Log-Normal, 3=Exponential",
			"1=Log-Normal, 2=Exponential, 3=Normal")
	fileExample <- "interactive_2_1"
	
	if(eval==FALSE) {
		OpenWindow(title=exerciseTitle)
		InteractivePlot(plot.function=plotInteractive3, name=interactive31,
				header=plotHeader,
				slider=c("n"),slider_start=c(100),slider_res=c(1),
				slider_lim=plotSliderLim,
				slider_label=plotSliderLabel,
				radio_header=plotRadioHeader,
				radio=plotRadio,
				radio_label=plotRadioLabel,
				radio_value=plotValues,click.function=NULL,
				design=plotDesign,hscale=1.5,vscale=1.5
		)  
		SingleChoice(frame=MainFrame, 
				interactive32,
				question1=q1,
				question2=q2,
				labels=labs,
				filename=fileExample)
		
		tkgrid(interactive31)
		tkgrid(interactive32)
		ExitButton(name=Exit)
		tkgrid(Exit)	
	}
	else {
		OpenWindow(title="Evaluation: interactive QQ-plots")
		InteractivePlot(plot.function=plotInteractive3, name=interactive31,
				header=plotHeader,
				slider=c("n"),slider_start=c(100),slider_res=c(1),
				slider_lim=plotSliderLim,
				slider_label=plotSliderLabel,
				radio_header=plotRadioHeader,
				radio=plotRadio,
				radio_label=plotRadioLabel,
				radio_value=plotValues,click.function=NULL,
				design=plotDesign,hscale=1.3,vscale=1.3
		)
		SingleChoiceAnswer(frame=MainFrame, 
				interactive32,
				question1=q1,
				question2=q2,
				labels=labs,
				filename=fileExample)
		
		tkgrid(interactive31)
		tkgrid(interactive32)
		ExitButton(name=Exit)
		tkgrid(Exit)		
	}
}