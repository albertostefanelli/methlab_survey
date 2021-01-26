data <- read.csv("data.csv")
data <- data[-2,]

main <- data[1, ]

colnames <- NA
for (i in which(colnames(main)=="Q4_1"):which(colnames(main)=="Q5_11")) {
  colnames[i] <- sub("\\.*On ", "", sub("\\ .*? - ", " ", main[i][[1]]))
}

## ------------------------------------------------------------------------
# Interests in following topics
## ------------------------------------------------------------------------

data[, which(colnames(data)=="Q4_1"):which(colnames(data)=="Q4_11")] <- lapply(data[, which(colnames(data)=="Q4_1"):which(colnames(data)=="Q4_11")], function(x){
  levels(x)[c(1, 2, 5)] <- "NA"
  x
})

data[, which(colnames(data)=="Q5_1"):which(colnames(data)=="Q5_11")] <- lapply(data[, which(colnames(data)=="Q5_1"):which(colnames(data)=="Q5_11")], function(x){
  levels(x)[c(1, 2, 5)] <- "NA"
  x
})

data[, which(colnames(data)=="Q4_1"):which(colnames(data)=="Q4_11")] <- lapply(data[, which(colnames(data)=="Q4_1"):which(colnames(data)=="Q4_11")], as.integer)
data[, which(colnames(data)=="Q5_1"):which(colnames(data)=="Q5_11")] <- lapply(data[, which(colnames(data)=="Q5_1"):which(colnames(data)=="Q5_11")], as.integer)

tiff("quali_pref.tiff", res = 600, compression = "lzw", height = 8, width = 12, units = "in")
par(mar = c(2.5, 2.5, 2.5, 1.5))
layout(matrix(c(1:15), 5, 3, byrow = TRUE))

plot.new()
plot.new()
text(0.5,0.5,"Interests topic (qualitative)",cex=2,font=2)
plot.new()

for(i in which(colnames(data)=="Q4_1"):which(colnames(data)=="Q4_11")){
  hist(data[2:nrow(data), i],
       main = sub("\\.*On ", "", sub("\\ .*? - ", " ", main[i][[1]])),
       ylim = c(0, 40),
       xaxt='n')
  axis(side=1, at=seq(1, 5, 1), labels=c("NA", "1", "2", "3", "4"))
}

dev.off()

tiff("quanti_pref.tiff", res = 600, compression = "lzw", height = 8, width = 12, units = "in")
par(mar = c(2.5, 2.5, 2.5, 1.5))
layout(matrix(c(1:15), 5, 3, byrow = TRUE))

plot.new()
plot.new()
text(0.5,0.5,"Interests topic (quantitative)",cex=2,font=2)
plot.new()

for(i in which(colnames(data)=="Q5_1"):which(colnames(data)=="Q5_11")){
  hist(data[2:nrow(data), i],
       main = sub("\\.*On ", "", sub("\\ .*? - ", " ", main[i][[1]])),
       ylim = c(0, 40),
       xaxt='n')
  axis(side=1, at=seq(1, 5, 1), labels=c("NA", "1", "2", "3", "4"))
}

dev.off()

## ------------------------------------------------------------------------
# Knowledgeable
## ------------------------------------------------------------------------

data[, which(colnames(data)=="Q8_1"):which(colnames(data)=="Q8_11")] <- lapply(data[, which(colnames(data)=="Q8_1"):which(colnames(data)=="Q8_11")], function(x){
  levels(x)[c(1, 2, 5)] <- "NA"
  x
})

data[, which(colnames(data)=="Q9_1"):which(colnames(data)=="Q9_11")] <- lapply(data[, which(colnames(data)=="Q9_1"):which(colnames(data)=="Q9_11")], function(x){
  levels(x)[c(1, 2, 5)] <- "NA"
  x
})

data[, which(colnames(data)=="Q8_1"):which(colnames(data)=="Q8_11")] <- lapply(data[, which(colnames(data)=="Q8_1"):which(colnames(data)=="Q8_11")], as.integer)
data[, which(colnames(data)=="Q9_1"):which(colnames(data)=="Q9_11")] <- lapply(data[, which(colnames(data)=="Q9_1"):which(colnames(data)=="Q9_11")], as.integer)

tiff("quali_pref.tiff", res = 600, compression = "lzw", height = 8, width = 12, units = "in")
par(mar = c(2.5, 2.5, 2.5, 1.5))
layout(matrix(c(1:15), 5, 3, byrow = TRUE))

plot.new()
plot.new()
text(0.5,0.5,"Knowledge (qualitative)",cex=2,font=2)
plot.new()

for(i in which(colnames(data)=="Q8_1"):which(colnames(data)=="Q8_11")){
  hist(data[2:nrow(data), i],
       main = sub("\\.*On ", "", sub("\\ .*? - ", " ", main[i][[1]])),
       ylim = c(0, 50),
       xaxt='n')
  axis(side=1, at=seq(1, 6, 1), labels=c("NA", "1", "2", "3", "4", "5"))
}

dev.off()

tiff("quanti_pref.tiff", res = 600, compression = "lzw", height = 8, width = 12, units = "in")
par(mar = c(2.5, 2.5, 2.5, 1.5))
layout(matrix(c(1:15), 5, 3, byrow = TRUE))

plot.new()
plot.new()
text(0.5,0.5,"Knowledge (quantitative)",cex=2,font=2)
plot.new()

for(i in which(colnames(data)=="Q9_1"):which(colnames(data)=="Q9_11")){
  hist(data[2:nrow(data), i],
       main = sub("\\.*On ", "", sub("\\ .*? - ", " ", main[i][[1]])),
       ylim = c(0, 60),
       xaxt='n')
  axis(side=1, at=seq(1, 6, 1), labels=c("NA", "1", "2", "3", "4", "5"))
}

dev.off()

## ------------------------------------------------------------------------
# Interests training
## ------------------------------------------------------------------------

data[, which(colnames(data)=="Q11_1"):which(colnames(data)=="Q11_11")] <- lapply(data[, which(colnames(data)=="Q11_1"):which(colnames(data)=="Q11_11")], function(x){
  levels(x)[c(1, 2, 5)] <- "NA"
  x
})

data[, which(colnames(data)=="Q11_1"):which(colnames(data)=="Q11_11")] <- lapply(data[, which(colnames(data)=="Q11_1"):which(colnames(data)=="Q11_11")], as.integer)

tiff("train_pref.tiff", res = 600, compression = "lzw", height = 8, width = 12, units = "in")
par(mar = c(2.5, 2.5, 2.5, 1.5))
layout(matrix(c(1:15), 5, 3, byrow = TRUE))

plot.new()
plot.new()
text(0.5,0.5,"Interests topic",cex=2,font=2)
plot.new()

for(i in which(colnames(data)=="Q11_1"):which(colnames(data)=="Q11_11")){
  hist(data[2:nrow(data), i],
       main = sub("\\.*On ", "", sub("\\ .*? - ", " ", main[i][[1]])),
       ylim = c(0, 40),
       xaxt='n')
  axis(side=1, at=seq(1, 5, 1), labels=c("NA", "1", "2", "3", "4"))
}

dev.off()

## ------------------------------------------------------------------------
# Interests training
## ------------------------------------------------------------------------

qual <- as.data.frame(lapply(data[, which(colnames(data)=="Q4_1"):which(colnames(data)=="Q4_11")], function(x){length(which(x == 3 | x == 4))/nrow(data)}))
colnames(qual) <- colnames[which(colnames(data)=="Q4_1"):which(colnames(data)=="Q4_11")]

quant <- as.data.frame(lapply(data[, which(colnames(data)=="Q5_1"):which(colnames(data)=="Q5_11")], function(x){length(which(x == 3 | x == 4))/nrow(data)}))
colnames(quant) <- colnames[which(colnames(data)=="Q5_1"):which(colnames(data)=="Q5_11")]

perc <- cbind(qual, quant)
perc <- t(perc)
perc <- as.data.frame(perc)
perc$names <- rownames(perc)
colnames(perc)[1] <- "values"



barplot(sort(perc$values, decreasing = T),
        xaxt = "n")

axis(side = 1, labels = FALSE)

text(x = 1:nrow(perc),
     ## Move labels to just below bottom of chart.
     y = par("usr")[3],
     ## Use names from the data list.
     labels = perc[order(perc$names, perc$values, decreasing = T), "names"],
     ## Change the clipping region.
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965)
