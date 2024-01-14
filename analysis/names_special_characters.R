# reproducible data
plates <- data.frame(WatexCl = rnorm(100), ConcuM = rnorm(100), Depth = rnorm(100))

# alter the default plot margins so the 
# superscript in the y-axis label is completely displayed
par(mar=c(5,5,4,2))

# draw the plot
plot(WatexCl ~ ConcuM, data = plates,
     col = as.numeric(1), 
     pch = as.numeric(Depth), 
     xlab = bquote("Concentration Cl ("*mu~"moles/g dry wt)"), 
     ylab = bquote("Average Conc of S- on plates ("~mu~"Moles/cm"^"2"*")"))



alpha = rnorm(1e3)
hist(alpha,cex.main=2,cex.axis=1.2,cex.lab=1.2,main=NULL )

title <- list( bquote( paste( "Histogram of " , hat(mu) ) ) ,
               bquote( paste( "Bootstrap samples, Allianz" ) ) )


mtext(do.call(expression, title ),side=3, line = c(1,-1) , cex = 2 )
w <- readline()
a=3+2
log(-20)
