library(factoextra)





uni <- read.csv("Universities.csv", header = TRUE)
head(uni)

names(uni)


univ_complete <- na.omit(uni)
nrow(univ_complete)

str(univ_complete)

names(univ_complete)

head(univ_complete)

univ_cont <- univ_complete[, -c(1:3)]
names(univ_cont)

str(univ_cont)

univ_cont_norm <- sapply(univ_cont, scale)
head(univ_cont_norm)

univ_norm_dist_m <- dist(univ_cont_norm, method = "manhattan")

univ_hc_m <- hclust(univ_norm_dist_m, method = "complete")
plot(univ_hc_m, hang = -100, ann = TRUE)

univ_hc_m_memb <- cutree(univ_hc_m, k = 3)
head(univ_hc_m_memb)

univ_hc_m_memb_df <- as.data.frame(univ_hc_m_memb)
head(univ_hc_m_memb_df)

names(univ_hc_m_memb_df)[names(univ_hc_m_memb_df) == 
                           "univ_hc_m_memb"] <- "Cluster"
head(univ_hc_m_memb_df)


fviz_nbclust(univ_cont_norm, 
             hcut, method = "silhouette") +
  labs(subtitle = "Silhouette method")

univ_complete_w_cluster <- cbind(univ_complete, univ_hc_m_memb_df)

head(univ_complete_w_cluster)

names(univ_complete_w_cluster)

aggregate(univ_complete_w_cluster[, c(5:20)],
          by = univ_complete_w_cluster[21],
          FUN = mean)

table(univ_complete_w_cluster$Public.vs.Private, univ_complete_w_cluster$Cluster)
table(univ_complete_w_cluster$State, univ_complete_w_cluster$Cluster)