ex_feedback_1 <- function(eval=FALSE){
  exerciseTitle <- "Feedback: TopicXXX"
  qu1 <- "Please help us improve your experience"
  qu2 <- c("Any further questions?",
      "More details on something?")
  fileExample <- "ex_feedback1"
  OpenWindow(title=exerciseTitle)
  if(!eval){
    Feedback(frame=MainFrame, 
        name=feed1,
        question1=qu1,
        question2=qu2,
        filename=fileExample)	
  }
  else{
    FeedbackAnswer(frame=MainFrame, 
        name=feed1,
        question1=qu1,
        question2=qu2,
        filename=fileExample)
  }
  tkgrid(feed1)
  
}
