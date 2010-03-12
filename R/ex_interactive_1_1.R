`interactive_1_1` <- function () {
    binom.refresh <- function() {
      if(Texists("n")&&Texists("sn")){
        n <- as.numeric(evalq(tclvalue(n), env = TGUIenv))
        p <- as.numeric(evalq(tclvalue(p), env = TGUIenv))
        sn <- as.numeric(evalq(tclvalue(sn), env = TGUIenv))==1
        sp <- as.numeric(evalq(tclvalue(sp), env = TGUIenv))==1
      }else{
        n <- 10
        p <- .5
        sn<-0==1
        sp<-0==1
        
      }
      par(bg="white")
      mu <- p * n
      sd <- sqrt(n * p * (1 - p))
      if (sn) {
        xx <- seq(-1, n + 1, length = 250)
        plot(xx, dnorm(xx, mu, sd), type = "l", col = "green",
            ylim = range(0, dnorm(mu, mu, sd), dbinom(seq(0,
                        n), n, p)), xlab = "x", ylab = "Probability",bg="white")
        if (sp) {
          points(seq(0, n), dpois(seq(0, n), mu), type = "h",
              col = "blue")
          points(seq(0, n), dpois(seq(0, n), mu), pch = "-",
              col = "blue", cex = 2)
        }
        abline(h = 0)
        lines(xx, dnorm(xx, mu, sd), col = "green")
        points(seq(0, n), dbinom(seq(0, n), n, p), type = "h")
        points(seq(0, n), dbinom(seq(0, n), n, p), type = "p")
      }
      else {
        if (sp) {
          plot(seq(0, n), dpois(seq(0, n), mu), type = "h",
              col = "blue", xlim = c(-1, n + 1), xlab = "x",
              ylab = "Probability", ylim = range(0, dpois(seq(0,
                          n), mu), dbinom(seq(0, n), n, p)),bg="white")
          points(seq(0, n), dpois(seq(0, n), mu), pch = "-",
              col = "blue", cex = 2)
          points(seq(0, n), dbinom(seq(0, n), n, p), type = "h")
        }
        else {
          plot(seq(0, n), dbinom(seq(0, n), n, p), type = "h",
              xlim = c(-1, n + 1), xlab = "x", ylab = "Probability",bg="white")
        }
        abline(h = 0)
        points(seq(0, n), dbinom(seq(0, n), n, p))
      }
      title(paste("Mean =", round(mu, 3), "Std. Dev. =", round(sd,
                  3)))
    }
   
    OpenWindow(title="Exercise: visualization of the binomial distribution")
    InteractivePlot(plot.function=binom.refresh,name=interactive1,
        header="Try to construct a binomial distribution using the slider 
            with p=0.2 and the smallest possible sample size that is well
            approximated by a normal distribution",
        slider=c("n","p"),slider_start=c(10,.5),slider_res=c(1,1/100),
        slider_lim=list(c(1,100),c(0,1)),slider_label=c("Sample size", "Probability"),
        checkbox=c("sn","sp"),checkbox_start=c(0,0),
        checkbox_label=c("Show Normal Approximation", "Show Poisson Approximation")
        )
    tkgrid(interactive1)
    ExitButton(name=Exit)
    tkgrid(Exit)
}

