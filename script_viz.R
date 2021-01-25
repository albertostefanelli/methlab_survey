data <- xlsx::read.xlsx2("data.xlsx", 1)

main <- data[1, 22:46]
data[, 22:46] <- lapply(data[, 22:46], as.numeric)

tiff("quali_pref.tiff", res = 600, compression = "lzw", height = 8, width = 12, units = "in")
par(mar = c(2.5, 2.5, 2.5, 1.5))
layout(matrix(c(1:15), 5, 3, byrow = TRUE))

plot.new()
plot.new()
text(0.5,0.5,"Qualitative",cex=2,font=2)
plot.new()

for(i in 1:9){
  hist(data[2:nrow(data), 21+i],
       main = sub("\\.*On ", "", sub("\\ .*? - ", " ", main[i][[1]])),
       ylim = c(0, 40))
}

dev.off()

tiff("quanti_pref.tiff", res = 600, compression = "lzw", height = 10, width = 14, units = "in")
par(mar = c(2.5, 2.5, 2.5, 1.5))
layout(matrix(c(1:18), 6, 3, byrow = TRUE))

plot.new()
plot.new()
text(0.5,0.5,"Quantitative",cex=2,font=2)
plot.new()

for(i in 10:24){
  hist(data[2:nrow(data), 21+i],
       main = sub("\\.*On ", "", sub("\\ .*? - ", " ", main[i][[1]])),
       ylim = c(0, 40))
}

dev.off()