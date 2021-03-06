#!/usr/bin/Rscript

#args <- commandArgs(trailingOnly = TRUE)
#print(args)
#src <- args[1]
#prefix = src
prefix = "models"

xlabel = "NUMBER OF PARALLEL STREAMS"
ylabel = "THROUGHPUT (Mbps)"
#ylabel = "THROUGHPUT (MB/S)"

# figure size in pixel
#fheight = 200
fheight = 300
#fwidth = 300
fwidth = 400

# in ms
RTT = 190
MSS = 1500
# sqrt(3/2)
c = sqrt(3/2)
#c = 1.22 
n = 1:40
# packet loss rate
#p = 0.001
#p = 0.01
n1 = 1
n2 = 10
Th1 = 100
Th2 = 350
a = ((n1^2/Th1^2) - (n2^2/Th2^2))/(n1^2 - n2^2)
b = (n1^2/Th1^2) - a*(n1^2)
#p = a*(n^2)+b

cat(a, b, "\n")

# partial second order
# an2 + b
#p = 

#data <- th

# filter missing values
#data <- read.table(src, na.strings = "NA", fill = TRUE, header=1)
#data <- read.table(src, na.strings = "NA", fill = TRUE)

# replace missing values to zero
#data[is.na(data)] <- 0
#print(data)

# transpose
#data <- t(data[,-1]/1000)
#print(data)

require(devEMF)

# aggregate throughput
getthroughput <- function (p) {
#	print (p)
	return (n/sqrt(p))
	#return (c*MSS*n/(RTT * sqrt(p)))
}

genplot <- function (type) {
        #rm(list = ls())      # Clear all variables
        #graphics.off()    # Close graphics windows

        imgfile = paste(prefix, sep = ".", type)

        if(type == "png") {
                png(imgfile, height = fheight, width = fwidth)
                #png(paste0(prefix, ".png"), height=fheight, width=fwidth)
        #       png(paste0(prefix, ".png"))
        } else if (type == "pdf") {
                pdf(imgfile, height=1.5*fheight/100.0, width=1.5*fwidth/100.0)
                #pdf(paste0(prefix, ".pdf"), height=1.5*fheight/100.0, width=1.5*fwidth/100.0)
        } else if (type == "eps") {
                postscript(imgfile)
                #postscript(paste0(prefix, ".eps"))
                #postscript(paste0(prefix, ".eps"), res = resolution)
        } else if (type == "emf") {
                emf(imgfile, height=1.5*fheight/100.0, width=1.5*fwidth/100.0)
                #emf(paste0(prefix, ".emf"), height=1.5*fheight/100.0, width=1.5*fwidth/100.0)
        }

	# c(bottom, left, top, right)
	par(mar = c(5, 5, 1, 1) + 0.1)

#	df <- data.frame(values = c(data[1,8], data[1:2,7], data[1:3,6], data[1:4,5], data[1:5,4], data[1:6,3], data[1:7,2], data[1:8,1]), 
#		vars = rep(c("1", "2", "3", "4", "5", "6", "7", "8"), times = c(1,2,3,4,5,6,7,8)))
	
	#print(df)
	
	# packet loss rate
	p = a*(n^2)+b

	data <- getthroughput(p)
	ylim <- range(data + data/n)
#	print (ylim)
#	print (ylim[0])

	# las = 2 to rotate xlabels
	#boxplot(values ~ vars, data = df,
	plot(data,
		las = 1, 
		xlab = xlabel,
		ylab = ylabel,
		type = "l",
		lty = 2
		#names = c("1", "2", "3", "4", "5", "6", "7", "8"),
			#"9", "10", "12", "14", "16", "18", "20", "30", 
			#"40", "50", "60", "70", "80", "90", "100"),
		,ylim = c(0, ylim[2])
	)

	# add individual throughput divided total throughput by n
	#p = 0.001	
	#p = 0.01
	#data <- getthroughput(p)
	points(data/n, type = "l")

	# sum of normalized throughputs
	lines(data + data/n, type = "l", lty = 3)
	#lines(data + data/n, type = "b", lty = 3, pch = 8)

	# Add a legend to the plot
        legend("right",                       # x-y coordinates for location of the legend
               legend=c(paste("a=", a), paste("b=", b)),      # Legend labels
               #col=c(secondlc),   # Color of points or lines
        #       col=c("black", secondlc),   # Color of points or lines
              #pch=c(NA,20),                 # Point type
              #pch=c(21,19),                 # Point type
               #lty=c(secondlty),                    # Line type
               #lwd=c(secondlwd),                    # Line width
        #       cex = fontsize,
                #bty = 'n'
        )
}

genplot("png")
genplot("pdf")
genplot("emf")
