#' Load useful packages
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)

#' Load data
df2 <- read.csv("B892.csv")

#fix up columns 
df2$age <- as.numeric(as.character(df2$age))
agelabs <- c("A", "B", "C", "D", "E",
             "F","G")
df2$age_gr <- cut(df2$age,breaks = c(0, 20, 25, 30, 35,
                                  45, 65,100), right=FALSE, labels=agelabs)

df2$RS <- ifelse(df2$TYPE == "R", "Y", "N")
df2$GPA_2 <- ifelse(df2$GPA >= 2.0, "P", "F")
df2$RS <- as.factor(df2$RS)
df2$GPA_2 <- as.factor(df2$GPA_2)
df2$ETHN <- as.factor(df2$ETHN)
#check it out
df2 %>% top_n(2)
#subset
df <- subset(df2,,select=c(FT_PT,ETHN, INTENT, RTN_IND,age_gr,RS,GPA_2))

#Do the clustering

#' Compute Gower distance
gower_dist <- daisy(df, metric = "gower")

gower_mat <- as.matrix(gower_dist)

#' Print most similar clients
df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]

#' Print most dissimilar clients
df[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

sil_width <- c(NA)
for(i in 2:10){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

k <- 8
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- df %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = X, y = Y), data = tsne_data) +
 geom_point(aes(color = cluster))+
             # geom_point(stat = "identity" ) + 
     scale_colour_manual(values=cbbPalette)


