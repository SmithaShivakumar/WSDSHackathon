library(maps)

setwd("C:/Users/Maggie/Desktop/WSDS")

poverty <- read.csv("data/child_poverty.csv")

data(state)

fig.path <- "plots/child_poverty.jpeg"
jpeg(fig.path, height = 480, width = 640)

# things to plot
nlevels = 11
levels = pretty(poverty$X2015.ACS.Child.Poverty.Rate, nlevels)
color.palette = heat.colors
col = color.palette(length(levels) - 1)

mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
on.exit(par(par.orig))
w <- (3 + mar.orig[2L]) * par("csi") * 2.54
layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    
# plot key
mar <- mar.orig
mar[4L] <- mar[2L]
mar[2L] <- 1
par(mar = mar)
plot.new()
plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
            yaxs = "i")
rect(0, levels[-length(levels)], 1, levels[-1L], col = color.palette(length(levels) - 1))
axis(4, las=1, cex.axis = 1)

# plot key
mar <- mar.orig
mar[4L] <- 1
mar[2L] <- 2.1
par(mar = mar)
map("state",
    regions = poverty$State,
    lty = 1, lwd =1,
    boundary=TRUE,
    fill=TRUE,
    col=heat.colors(10)
    )
title("2015 ACS Child Poverty Rate (%)")
dev.off()