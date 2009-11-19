`radiobutton_1_1` <- function(eval=FALSE) {
	exerciseTitle <- "Exercise: scales"
	q1 <- "Variable 'household income'"
	q2 <- c(	"with values ranging from 0 to 50000 Euro",
				"is measured on a:")
	labs <- c(	"nominal scale",
				"ordinal scale",
				"interval scale",
				"ratio scale")
	notice <- "In this example, only one choice is possible!"
	fileExample <- "radiobutton_1_1"	
	
	plotIncome <- function() {
		par(bg="white")
		dat <- c(0, 5000, 7000, 7500, 10000, 13000,12400,14120,15340, 17000, 17900, 13620, 11700, 14400, 13800, 12790, 15950, 18290, 18990, 23620, 50000)
		set.seed(123)
		y <- jitter(rep(0, length(dat)))
		hist(dat, breaks=15, xlab="Available household income (in EURO))", main="", pch=4, col="skyblue")
	}
	
	if(eval==FALSE) {
		OpenWindow(title=exerciseTitle)
		SingleChoice(frame=MainFrame, 
				radiobutton1,
				question1=q1,
				question2=q2,
				labels=labs,
				plotFunction=plotIncome,
				note=notice,
				filename=fileExample)	
		tkgrid(radiobutton1)		
	}
	else {
		OpenWindow(title=paste("Evaluation - ", exerciseTitle, sep=""))
		SingleChoiceAnswer(frame=MainFrame, 
				radiobutton1,
				question1=q1,
				question2=q2,
				labels=labs,
				plotFunction=plotIncome,
				filename=fileExample)	
		tkgrid(radiobutton1)		
	}
}