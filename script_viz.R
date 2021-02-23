## ------------------------------------------------------------------------
# Load data and libraries
## ------------------------------------------------------------------------

library(ggplot2)
data <- read.csv("data_viz.csv")
data <- data[-2,]

main <- data[1, ]

head(main)

for (i in 1:90) {
  main[i] <- sub("\\.*On ", "", sub("\\ .*? - ", " ", main[i][[1]]))
}

## ------------------------------------------------------------------------
# Cleaning
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

qual <- as.data.frame(lapply(data[, which(colnames(data)=="Q4_1"):which(colnames(data)=="Q4_11")], function(x){length(which(x == 3 | x == 4))/nrow(data)}))
colnames(qual) <- main[which(colnames(data)=="Q4_1"):which(colnames(data)=="Q4_11")]

quant <- as.data.frame(lapply(data[, which(colnames(data)=="Q5_1"):which(colnames(data)=="Q5_11")], function(x){length(which(x == 3 | x == 4))/nrow(data)}))
colnames(quant) <- main[which(colnames(data)=="Q5_1"):which(colnames(data)=="Q5_11")]

perc <- cbind(qual, quant)
perc <- t(perc)
perc <- as.data.frame(perc)
perc$names <- rownames(perc)
colnames(perc)[1] <- "values"

perc$meth <- append(rep("qual", 11), rep("quant", 11))

# ---

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

qual_know <- as.data.frame(lapply(data[, which(colnames(data)=="Q8_1"):which(colnames(data)=="Q8_11")], function(x){length(which(x == 4 | x == 5))/nrow(data)}))
colnames(qual_know) <- main[which(colnames(data)=="Q8_1"):which(colnames(data)=="Q8_11")]

quant_know <- as.data.frame(lapply(data[, which(colnames(data)=="Q9_1"):which(colnames(data)=="Q9_11")], function(x){length(which(x == 4 | x == 5))/nrow(data)}))
colnames(quant_know) <- main[which(colnames(data)=="Q9_1"):which(colnames(data)=="Q9_11")]

perc_know <- cbind(qual_know, quant_know)
perc_know <- t(perc_know)
perc_know <- as.data.frame(perc_know)
perc_know$names <- rownames(perc_know)
colnames(perc_know)[1] <- "values"
perc_know$meth <- append(rep("qual_know", 11), rep("quant_know", 11))

# ---

data[, which(colnames(data)=="Q11_1"):which(colnames(data)=="Q11_11")] <- lapply(data[, which(colnames(data)=="Q11_1"):which(colnames(data)=="Q11_11")], function(x){
  levels(x)[c(1, 2, 5)] <- "NA"
  x
})

data[, which(colnames(data)=="Q11_1"):which(colnames(data)=="Q11_11")] <- lapply(data[, which(colnames(data)=="Q11_1"):which(colnames(data)=="Q11_11")], as.integer)

train_crs <- as.data.frame(lapply(data[, which(colnames(data)=="Q11_1"):which(colnames(data)=="Q11_11")], function(x){length(which(x == 3 | x == 4))/nrow(data)}))
colnames(train_crs) <- main[which(colnames(data) == "Q11_1"):which(colnames(data)=="Q11_11")]

train_crs <- t(train_crs)
train_crs <- as.data.frame(train_crs)
train_crs$names <- rownames(train_crs)
colnames(train_crs)[1] <- "values"

## ------------------------------------------------------------------------
# Interests in following topics
## ------------------------------------------------------------------------

tiff("quali_pref.tiff", res = 600, compression = "lzw", height = 8, width = 12, units = "in")
par(mar = c(2.5, 2.5, 2.5, 1.5))
layout(matrix(c(1:15), 5, 3, byrow = TRUE))

plot.new()
plot.new()
text(0.5,0.5,"Frequencies of respondents' \n interest in qualitative topics",cex=2,font=2)
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
par(mar = c(2.5, 3, 2.5, 1.5))
layout(matrix(c(1:15), 5, 3, byrow = TRUE))

plot.new()
plot.new()
text(0.5,0.5,"Frequencies of respondents' \n interest in quantitative topics",cex=2,font=2)
plot.new()

for(i in which(colnames(data)=="Q5_1"):which(colnames(data)=="Q5_11")){
  hist(data[2:nrow(data), i],
       main = sub("\\.*On ", "", sub("\\ .*? - ", " ", main[i][[1]])),
       ylim = c(0, 40),
       xaxt='n',
       xlab = NULL)
  axis(side=1, at=seq(1, 5, 1), labels=c("NA", "1", "2", "3", "4"))
}

dev.off()

tiff("interest.tiff", res = 300, compression = "lzw", height = 6, width = 10, units = "in")
ggplot(perc, aes(x=reorder(names, values), y=values, fill = meth)) + 
  geom_bar(stat = "identity") +
  ylab("Proportion of respondents") +
  ggtitle("Comparison between topics with highest levels of interests \n (somewhat and very interested)") +
  theme_classic() +
  theme(axis.title.y = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0.01, .1)),
                     breaks = round(seq(0, 0.7, by = 0.05), 2)) +
  scale_fill_manual("Method family", values = c("qual" = "#00CACC", "quant" = "#FF7875"), labels = c("Qualitative", "Quantitative")) +
  coord_flip()
dev.off()

## ------------------------------------------------------------------------
# Knowledgeable
## ------------------------------------------------------------------------

tiff("quali_know.tiff", res = 300, compression = "lzw", height = 8, width = 12, units = "in")
par(mar = c(2.5, 2.5, 2.5, 1.5))
layout(matrix(c(1:15), 5, 3, byrow = TRUE))

plot.new()
plot.new()
text(0.5,0.5,"Frequencies of respondents' \n knowledge in qualitative topics",cex=2,font=2)
plot.new()

for(i in which(colnames(data)=="Q8_1"):which(colnames(data)=="Q8_11")){
  hist(data[2:nrow(data), i],
       main = sub("\\.*On ", "", sub("\\ .*? - ", " ", main[i][[1]])),
       ylim = c(0, 50),
       xaxt='n')
  axis(side=1, at=seq(1, 6, 1), labels=c("NA", "1", "2", "3", "4", "5"))
}

dev.off()

tiff("quanti_know.tiff", res = 600, compression = "lzw", height = 8, width = 12, units = "in")
par(mar = c(2.5, 2.5, 2.5, 1.5))
layout(matrix(c(1:15), 5, 3, byrow = TRUE))

plot.new()
plot.new()
text(0.5,0.5,"Frequencies of respondents' \n knowledge in quantitative topics",cex=2,font=2)
plot.new()

for(i in which(colnames(data)=="Q9_1"):which(colnames(data)=="Q9_11")){
  hist(data[2:nrow(data), i],
       main = sub("\\.*On ", "", sub("\\ .*? - ", " ", main[i][[1]])),
       ylim = c(0, 60),
       xaxt='n')
  axis(side=1, at=seq(1, 6, 1), labels=c("NA", "1", "2", "3", "4", "5"))
}

dev.off()

tiff("knowledge.tiff", res = 300, compression = "lzw", height = 6, width =10, units = "in")
ggplot(perc_know, aes(x=reorder(names, values), y=values, fill = meth)) + 
  geom_bar(stat = "identity") +
  ylab("Proportion of respondents") +
  ggtitle("Comparison between topics with highest levels of knowledge \n (somewhat and very knowledgeable)") +
  theme_classic() +
  theme(axis.title.y = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0.01, .1)),
                     breaks = round(seq(0, 0.4, by = 0.05), 2)) +
  scale_fill_manual("Method family", values = c("qual_know" = "#00CACC", "quant_know" = "#FF7875"), labels = c("Qualitative", "Quantitative")) +
  coord_flip()
dev.off()

## ------------------------------------------------------------------------
# Interests training
## ------------------------------------------------------------------------

tiff("train_pref.tiff", res = 300, compression = "lzw", height = 6, width = 12, units = "in")
par(mar = c(2.5, 2.5, 2.5, 1.5))
layout(matrix(c(1:15), 5, 3, byrow = TRUE))

plot.new()
plot.new()
text(0.5,0.5,"Frequencies of respondents \n expressing interest in training",cex=2,font=2)
plot.new()

for(i in which(colnames(data)=="Q11_1"):which(colnames(data)=="Q11_11")){
  hist(data[2:nrow(data), i],
       main = sub("\\.*On ", "", sub("\\ .*? - ", " ", main[i][[1]])),
       ylim = c(0, 40),
       xaxt='n')
  axis(side=1, at=seq(1, 5, 1), labels=c("NA", "1", "2", "3", "4"))
}

dev.off()

tiff("train.tiff", res = 300, compression = "lzw", height = 6, width = 10, units = "in")
ggplot(train_crs, aes(x=reorder(names, values), y=values)) + 
  geom_bar(stat = "identity", colour="black", fill="#C5C6C5") +
  ylab("Proportion of respondents") +
  ggtitle("Comparison between courses with highest levels of interests \n (somewhat and very interested)") +
  theme_classic() +
  theme(axis.title.y = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0.01, .1)),
                     breaks = round(seq(0, 0.6, by = 0.05), 2)) + 
  coord_flip()
dev.off()