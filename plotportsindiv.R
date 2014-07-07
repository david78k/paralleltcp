#!/usr/bin/Rscript

#options(echo=TRUE)
# trailingOnly=TRUE means that only your arguments are returned
args <- commandArgs(trailingOnly = TRUE)
print(args)
#src = "4vms-r1.dest.dstat.csv"
src <- args[1]
#N = 4 # number of parallel migrations
#N <- as.numeric(args[2])

#prefix = src
prefix = paste0(src, ".indiv")

# for average
startcol = 9
#integer: the number of lines of the data file to skip before beginning to read data.
startrow = 6

# figure size in pixel
fheight = 200
fwidth = 300

linewidth = 2.5

# second line color
secondlc = "red"

# second line type: 2 for dashed, 3 for dotted, 4 for dotdash, 5 for longdash, 6 for twodash
#secondlty = 5 # best
#secondlty = 2 # good 
#secondlty = 3 # too pale
secondlty = 1
secondlwd = 2

# legend position
legendpos = "bottom"
#legendpos = "center"
#legendpos = "topright"

# cex
fontsize = 1
#fontsize = 1.5 
#fontsize = 2 

library(ggplot2)
library(reshape)
library(grid)
require(devEMF)

# filter missing values
data <- read.table(src, na.strings = "NA", fill = TRUE)
#data <- data.frame(src)
#data <- read.csv(src, sep=",", skip = startrow, header=1)
#aapl = aapl[nrow(aapl):1, ]
#print(data)

# replace missing values to zero
data[is.na(data)] <- 0
#print(data)

# select odd columns only and convert to percentile for link utilization (%)
data <- 100*data[, seq(1, ncol(data), by = 2)]/125
#print(data)

#data <- reshape(data,
#                varying = list(names(data)), 
#                v.names = "throughput",
#                timevar = "VM",
#		times = names(data),
#                direction = "long")
 
# remove id column (last column)
#data <- subset(data, select = -c(id))
#print(data)

genplot <- function (type) {
	#rm(list = ls())      # Clear all variables
	#graphics.off()    # Close graphics windows

	if(type == "png") {
		#type(paste0(prefix, sep = ".", type))
		png(paste0(prefix, ".png"), height=fheight, width=fwidth)
		#png(paste0(prefix, ".png"), height=300, width=400)
	#	png(paste0(prefix, ".png"))
	} else if (type == "pdf") {
		pdf(paste0(prefix, ".pdf"), height=1.5*fheight/100.0, width=1.5*fwidth/100.0)
	} else if (type == "eps") {
		#png("aapl.png")
		#postscript(paste0(prefix, ".eps"), res = resolution)
		postscript(paste0(prefix, ".eps"))
	} else if (type == "emf") {
		#postscript("aapl.eps")
		emf(paste0(prefix, ".emf"), height=1.5*fheight/100.0, width=1.5*fwidth/100.0)
		#emf('aapl.emf')
	}

	# margins: oma for the number of lines in outer margin, mar for the number of lines in inside margin
	# c(bottom, left, top, right)
	#par(oma=c(0,0,0,0))               # Set outer margin areas (only necessary in order to plot extra y-axis)
	# mgp for space between label and axis
	# default mgp = c(3, 1, 0)
	par(mar=c(4,4,1,1) + 0.1, mgp=c(2.5,1,0)) # good fit
	#par(mar=c(4,4,1,1)) # good fit
	#par(mar=c(5,5,1,1)) # good fit
	#par(mar=c(4,5,0,0))  # both too tight

	x <- as.numeric(rownames (data))*2
	#print (x)
	#y <- cbind(data, rowMeans(data))
	y <- cbind(data)
	#print (y)
	matplot(x, y, type = "l", 
        	xlab = "TIME (SEC)", 
        	ylab = "LINK UTILIZATION (%)"
	)
        #mtext ("LINK UTILIZATION (%)", side = 2, line = 3)
	
	# link utilization (%)
	# individual throughput
	#plot(data[,startcol]/1024.0/1024.0,            # Data to plot - x, y
#	plot(data,            # Data to plot - x, y
	#   type="b",                    # Plot lines and points. Use "p" for points only, "l" for lines only
	#     type="l",                    # Plot lines and points. Use "p" for points only, "l" for lines only
	#     main="Time series plot",     # Main title for the plot
	#     xlab="TIME (SEC)",                 # Label for the x-axis
	#	ylab = "LINK UTILIZATION (%)",
#	     font.lab=2,                  # Font to use for the axis labels: 1=plain text, 2=bold, 3=italic, 4=bold italic
#	     cex.axis = fontsize,
#	     cex.lab = fontsize,
	#     ylim=c(0,20),                # Range for the y-axis; "xlim" does same for x-axis
	#     xaxp=c(0,50,5),              # X-axis min, max and number of intervals; "yaxp" does same for y-axis
	#     bty="l",                     # Box around plot to contain only left and lower lines
#	     las = 1                      # labels are parallel (=0) or perpendicular(=2) to axis, 1 for x-axis = 0 and y-axis = 1
#	)

	# Add y2 data to the same plot
	# average throughput
	avg <- rowMeans(data)
	
	#points(avg,
	points(x = x, y = avg,
	       type="l",                  # Plot lines and points
	       lty=secondlty,                     # Line type: 0=blank, 1=solid, 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash
	       lwd=secondlwd,                     # Line width
	#       pch=20,                    # Point type: pch=19 - solid circle, pch=20 - bullet (smaller circle), pch=21 - circle, pch=22 - square, pch=23 - diamond, pch=24 - triangle point-up, pch=25 - triangle point down.
	#       pch=19,                    # Point type: pch=19 - solid circle, pch=20 - bullet (smaller circle), pch=21 - circle, pch=22 - square, pch=23 - diamond, pch=24 - triangle point-up, pch=25 - triangle point down.
	       col=secondlc)                 # Color of the plotted data

	#ggplot(data, aes(x = x, y = y)) + geom_line()
	#ggplot(data, aes(x=data[,0]*2, y=100*data/125)) + geom_line()
		#+ geom_line(aes(colour=variable))
#	graph = ggplot(data, aes(x=id*2, y=100*throughput/125, fill=VM)) +
#		geom_area(position = 'stack') +
 #       	labs(x = "TIME (SEC)", 
  #      	     y = "LINK UTILIZATION (%)"
        	     #y = "THROUGHPUT (MB/S)"
        	     #title = "Composition of Natural Gas Pipeline Material in the United States"
#		) +
 #       	scale_fill_discrete(name = "VM", 
  #      	                    breaks = names(data),
        	                    #breaks = c("plastic", "steel", "castiron"),
   #     	                    labels = names(data) 
        	                    #labels = c("Plastic", "Steel", "Cast Iron")
#		) +
		      #plot.margin = unit(c(0.5,0.5,0,0), "cm"),
#		      plot.margin = unit(c(2,2,4,4), "mm"),

	# Add a legend to the plot
	legend(legendpos,                       # x-y coordinates for location of the legend
	       legend=c("AVERAGE"),      # Legend labels
	       col=c(secondlc),   # Color of points or lines
	#       col=c("black", secondlc),   # Color of points or lines
	      #pch=c(NA,20),                 # Point type
	      #pch=c(21,19),                 # Point type
	       lty=c(secondlty),                    # Line type
	       lwd=c(secondlwd),                    # Line width
	#       cex = fontsize,
		bty = 'n'
	)
}

genplot("png")
genplot("pdf")
#genplot("eps")
genplot("emf")

