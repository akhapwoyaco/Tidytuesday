#
library(tidyverse)
library(janitor)
tuesdata <- tidytuesdayR::tt_load(2023, week = 33)
spam <- tuesdata$spam |> clean_names()
#
head(spam)
#
library(cluster) # gower similarity and pam
library(Rtsne) # t-SNE plot
library(ggplot2) # for visualization
#
gower_dist <- daisy(
  spam[,-7],
  metric = "gower") #“Gower's distance” is chosen by metric "gower"
# Check attributes to ensure the correct methods used
# (I = interval, N = nominal)
summary(gower_dist)
#
gower_mat <- as.matrix(gower_dist)
# Output most similar pair
similar_pair_car <- spam[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ] # subset car based on minimum dissimaliryt
# Output most dissimilar pair
dissimilar_pair_car <- spam[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ] # subset car based on maximum dissimaliryt
#
knitr::kable(
  t(similar_pair_car), caption = "Most Similar observations"
)
knitr::kable(
  t(dissimilar_pair_car), caption = "Most Dissimilar observations"
)

# PAM Clusterin
#
# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist,  diss = TRUE, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
# sihouette width plot
plot(1:10, sil_width,
     xlab = "Number of Clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
grid()
#
### Cluster Summary
pam_fit <- pam(gower_dist, diss = TRUE, k = 2)
#
pam_results <- spam |>
  mutate(cluster = pam_fit$clustering) |>
  group_by(cluster) |>
  do(the_summary = summary(.))
#
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y |>
  data.frame() |>
  setNames(c("X", "Y")) |>
  mutate(cluster = factor(pam_fit$clustering),
         Model = spam$yesno)
#
cluster_plot1 = ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) +
  theme_bw() +
  labs(caption = "https://github.com/akhapwoyaco") +
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.15,0.9),
    legend.background = element_blank()
  )
cluster_plot2 = ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = Model)) +
  labs(caption = "https://github.com/akhapwoyaco") +
  theme_bw() +
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.15,0.9),
    legend.background = element_blank()
  )
#
ggsave(
  plot = cluster_plot1, filename = "cluster_plot1.jpeg",
  width = 25, height = 25, units = "cm", dpi = 650)
ggsave(
  plot = cluster_plot2, filename = "cluster_plot2.jpeg",
  width = 25, height = 25, units = "cm", dpi = 650)
#