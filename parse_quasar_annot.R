

library(png)
library(ggplot2)

#### This is a script to parse pixinsight annotation text files and retrieve pixel values from an image for each one





annot_text_file <- "~/../Downloads/M13 v02.objects.txt"
image_file <- "~/../Downloads/M13 v02.png"

annot_text <- read.delim(annot_text_file, stringsAsFactors = F, skip = 2, sep = ";")

img <- readPNG(image_file)


# pix.top.left <- img[1,1,]     # row 1, column 1
# pix.bottom.left <- img[3,1,]  # row 3, column 1
# pix.top.right <- img[1,3,]    # row 1, column 3

annot_text$R <- NA
annot_text$G <- NA
annot_text$B <- NA

for(i in 1:nrow(annot_text)){
  annot_text$R[i] <- img[round(annot_text$PixelY[i]), round(annot_text$PixelX[i]),1]
  annot_text$G[i] <- img[round(annot_text$PixelY[i]), round(annot_text$PixelX[i]),2]
  annot_text$B[i] <- img[round(annot_text$PixelY[i]), round(annot_text$PixelX[i]),3]
}

annot_text$RGBsum <- rowSums(annot_text[,c("R","G","B")])

#### calculate median values based on n samples
n <- 10000

sample_values <- data.frame(matrix(data = NA, nrow = n, ncol = 4))
colnames(sample_values) <- c("R","G","B","RGBsum")

rand_x <- sample(x = 1:dim(img)[2], size = n, replace = T)
rand_y <- sample(x = 1:dim(img)[1], size = n, replace = T)

for(i in 1:n){
  sample_values[i,] <- c(
    img[rand_y[i],rand_x[i],1:3],
    sum(img[rand_y[i],rand_x[i],1:3])
  )
}



#### HSV values from RGB


hsv_df <- data.frame(t(rgb2hsv(t(as.matrix(annot_text[,c("R","G","B")])))))
annot_text <- cbind(annot_text, hsv_df)

sample_values <- cbind(sample_values, data.frame(t(rgb2hsv(t(as.matrix(sample_values[,c("R","G","B")]))))))


#### Annotate quasar of interest


annot_text$QOI <- FALSE

annot_text$QOI[grep("364114", annot_text$Name)] <- TRUE

#### log transform v values

annot_text$logV <- log10(annot_text$v)
sample_values$logV <- log10(sample_values$v)

####

cols <- rev(rainbow(7)[-7])



ggplot(annot_text, aes(x = Bmag, y = v)) +
  #scale_y_continuous(trans = "log10") +
  #scale_x_continuous(trans = "reverse") +
  geom_hline(yintercept = median(sample_values$v), col = "darkblue") +
  geom_hline(yintercept = 0, col = "black") +
  #geom_vline(xintercept = 0, col = "black") +
  #geom_boxplot(data = sample_values, aes(y=v, x=max(annot_text$Rmag)*1.1)) +
  geom_violin(data = sample_values, aes(y=v, x=max(annot_text$Bmag)*1.1), fill="light blue", col = "darkblue") +
  geom_point(aes(col=Redshift, shape = QOI)) +
  scale_color_gradientn(colours = rainbow(5,rev = T)) +
  theme_minimal()

ggsave(filename = "linearV_vs_Bmag.png", device = "png", bg = "white")

ggplot(annot_text, aes(x = Bmag, y = logV)) +
  #scale_y_continuous(trans = "log10") +
  #scale_x_continuous(trans = "reverse") +
  geom_hline(yintercept = median(sample_values$logV), col = "darkblue") +
  #geom_vline(xintercept = 0, col = "black") +
  #geom_boxplot(data = sample_values, aes(y=v, x=max(annot_text$Rmag)*1.1)) +
  geom_violin(data = sample_values, aes(y=logV, x=max(annot_text$Bmag)*1.1), fill="light blue", col = "darkblue") +
  geom_point(aes(col=Redshift, shape = QOI)) +
  scale_color_gradientn(colours = rainbow(5,rev = T)) +
  theme_minimal()

ggsave(filename = "logV_vs_Bmag.png", device = "png", bg = "white")





ggplot(annot_text, aes(x = Bmag, y = h)) +
  geom_hline(yintercept = median(sample_values$h), col = "blue") +
  geom_hline(yintercept = 0, col = "black") +
  #geom_boxplot(data = sample_values, aes(y=v, x=max(annot_text$Rmag)*1.1)) +
  geom_violin(data = sample_values, aes(y=h, x=max(annot_text$Bmag)*1.1), fill="light blue") +
  geom_point(aes(col=Redshift)) +
  scale_color_gradientn(colours = rainbow(5,rev = T)) +
  theme_minimal()



ggplot(annot_text, aes(x = Bmag, y = RGBsum)) +
  geom_hline(yintercept = median(sample_values$RGBsum), col = "blue") +
  geom_hline(yintercept = 0, col = "black") +
  #geom_boxplot(data = sample_values, aes(y=v, x=max(annot_text$Rmag)*1.1)) +
  geom_violin(data = sample_values, aes(y=RGBsum, x=max(annot_text$Bmag)*1.1), fill="light blue") +
  geom_point(aes(col=Redshift)) +
  scale_color_continuous(type = "viridis") +
  theme_minimal()



ggplot(annot_text, aes(x = Redshift, y = B)) +
  geom_point() +
  geom_hline(yintercept = median(sample_values$RGBsum), col = "blue") +
  #geom_boxplot(data = sample_values, aes(y=RGBsum, x=max(annot_text$Redshift)*1.1)) +
  geom_violin(data = sample_values, aes(y=B, x=max(annot_text$Redshift, na.rm = T)*1.1), fill="light blue") +
  theme_minimal()



##### find the new one

subdf <- annot_text[annot_text$Bmag < 20,]
subdf <- subdf[which(subdf$Redshift == max(subdf$Redshift, na.rm = T)),]



### find the even dimmer one


subdf2 <- annot_text[annot_text$Bmag > 21,]
subdf2 <- subdf2[which(subdf2$Redshift == max(subdf2$Redshift, na.rm = T)),]


##### Redshift vs hue

ggplot(annot_text, aes(x=Redshift, y=v)) +
  geom_point()


