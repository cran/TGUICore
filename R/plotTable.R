plotTable <- function (table, lwd = par("lwd"), bg = par("bg"), 
    cex=1, xjust = 0, yjust = 1, box.col = par("fg"), text.col = par("fg"), 
    display.colnames = TRUE, display.rownames = TRUE, hlines = TRUE, vlines=TRUE,mar=c(0,0,1,0), 
    title = NULL,col1="red",col2="orange",column1=NULL,column2=NULL,mwidth=0.02539063,
    cellwidth=0.08398439,cellheight=0.04251454) {
  par(mar=mar)
  tabdim <- dim(table)
  column.names <- colnames(table)
  if (is.null(column.names) && display.colnames) 
    column.names <- 1:tabdim[1]
  row.names <- rownames(table)
  if (is.null(row.names) && display.rownames) 
    row.names <- 1:tabdim[1]
  
  if (par("xlog")) 
    x <- log10(x)
  if (display.colnames) {
    cellwidth <- cellwidth+mwidth
    nvcells <- tabdim[1] + 1
  }
  else {
    nvcells <- tabdim[1]
    cellwidth <- cellwidth+mwidth
  }
  if (display.rownames) 
    nhcells <- tabdim[2] + 1
  else nhcells <- tabdim[2]
  if (par("ylog")) 
    y <- log10(y)
  xleft <- xjust * nhcells * cellwidth
  ytop <-  yjust * nvcells * cellheight
  oldpar <- par(ylog = FALSE, ylog = FALSE, xpd = TRUE)
  ylim=nvcells*cellheight
  xlim=nhcells*cellwidth
  plot(1,xlim=c(0,xlim),ylim=c(0,ylim),type="n",xaxt="n",yaxt="n",bty="n",main=title,xlab="",ylab="")
  rect(xleft, ytop - nvcells * cellheight, xleft + nhcells * cellwidth, ytop, lwd = lwd, col = bg, border = box.col)
  if(!is.null(column1)) {
    for(ic in 1:nrow(column1)){
      aktcell<-c(NA,NA)
      aktcell[2] <- column1[ic,2] 
      aktcell[1] <- tabdim[1]  - column1[ic,1]
      rect(cellwidth*aktcell[2],cellheight*aktcell[1], cellwidth*(aktcell[2]+1), cellheight*(aktcell[1]+1), lwd = lwd, col = col1, border = col1)      
    }
  }
  if(!is.null(column2)){
    for(ic in 1:nrow(column2)){
      aktcell<-c(NA,NA)
      aktcell[2] <- column2[ic,2] 
      aktcell[1] <- tabdim[1]  - column2[ic,1]
      rect(cellwidth*aktcell[2],cellheight*aktcell[1], cellwidth*(aktcell[2]+1), cellheight*(aktcell[1]+1), lwd = lwd, col = col2, border = col2)      
    }
  }
  for (row in 1:tabdim[1]) {
    if (row <= nvcells - 1 && hlines) 
      segments(xleft, ytop - row * cellheight, xleft + nhcells * cellwidth, ytop - row * cellheight, lwd = lwd, col = box.col)
    if (display.rownames) 
      text(xleft + 0.5 * cellwidth, ytop - (row + display.colnames - 0.5) * cellheight, row.names[row], cex = cex, col = text.col)
    for (col in 1:tabdim[2]) 
      text(xleft + (col + display.rownames - 0.5) * cellwidth, ytop - (row + display.colnames - 0.5) * cellheight, table[row, col], cex = cex, col = text.col)
  }
  if(vlines) {
    for(col in 1:tabdim[2])
      segments(xleft+col*cellwidth, ytop - nvcells * cellheight, xleft+col*cellwidth, ytop, lwd = lwd, col = box.col)
  }
  if (display.colnames) {
    for (col in 1:tabdim[2]) 
      text(xleft + (col + display.rownames - 0.5) * cellwidth, ytop - 0.5 * cellheight, column.names[col], cex = cex, col = text.col)
    if (!hlines) 
      segments(xleft, ytop - cellheight, xleft + nhcells * cellwidth, ytop - cellheight, lwd = lwd, col = box.col)
  }
  Tassign("cellheight",cellheight)
  Tassign("cellwidth",cellwidth)
  par(oldpar)
}