#
#
tuesdata <- tidytuesdayR::tt_load(2020, week = 31)
penguins <- tuesdata$penguins
penguins_raw <- tuesdata$penguins_raw
#
head(penguins)
#
library(ggplot2)
#
penguins |>
  ggplot(aes(x = bill_length_mm, flipper_length_mm, colour = island)) +
  geom_point() + 
  geom_smooth(
    method="lm", se=T)+ 
  scale_color_brewer(palette = 'Dark2') +
  theme_light() +
  labs(
    color = "Island", x = "Bill Length (mm)",
    y = "Flipper Length (mm)"
  ) +
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.15, 0.85),
    legend.background = element_blank()
  )
#
#
bill_len_body_mass = penguins |>
  ggplot(aes(x = bill_length_mm, y = body_mass_g, colour = island)) +
  geom_point() + 
  geom_smooth(
    method="lm", se=T)+ 
  scale_color_brewer(palette = 'Dark2') +
  theme_light() +
  labs(
    color = "Island", x = "Bill Length (mm)",
    y = "Body Mass (g)"
  ) +
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.15, 0.85),
    legend.background = element_blank()
  )
#
table(penguins$species, penguins$island)
#

ggsave(
  "bill_len_body_mass.png",plot = bill_len_body_mass, 
  width = 22, height = 15, unit = "cm", dpi = 450)
#
#
#
#
head(penguins)
#
#
#
jpeg("penguins_boxplots.jpeg", res = 500, height = 35, width = 35, units = 'cm')
par(mfrow = c(2,2))
b1 <- boxplot(bill_length_mm~droplevels(interaction(species, island, sex)), 
              data = penguins, horizontal = T, #col = factor(penguins$sex),
              main = 'Bill Length (mm)', ylab = '', xlab = '',
              las = 1, boxwex = 0.4, yaxt = 'n',
              medlwd = 2, whiskcol = "black", staplecol = "black",
              outcol="red", cex = 0.5, outpch = 19)
axis(side = 2, at = 1:length(b1$names), cex.axis = 0.7,
     labels = paste(
       b1$names |> gsub(pattern = '\\.', replacement = '\n', x = b1$names),
       # b1$names,
       "\n(n=", b1$n,")", sep=""),  
     las = 1)
#
b2 <- boxplot(bill_depth_mm~droplevels(interaction(species, island, sex)), 
              data = penguins, horizontal = T,
              main = 'Bill Depth (mm)', ylab = '', xlab = '',
              las = 1, boxwex = 0.4, yaxt = 'n',
              medlwd = 2, whiskcol = "black", staplecol = "black",
              outcol="red", cex = 0.5, outpch = 19)
axis(side = 2, at = 1:length(b2$names), cex.axis = 0.7,
     labels = paste(
       b2$names |> gsub(pattern = '\\.', replacement = '\n', x = b2$names),
       # b2$names,
       "\n(n=", b2$n,")", sep=""), 
     las = 1)
#
b3 <- boxplot(flipper_length_mm~droplevels(interaction(species, island, sex)),
              data = penguins, horizontal = T,
              main = 'Flipper Length (mm)', ylab = '', xlab = '',
              las = 1, boxwex = 0.4, yaxt = 'n',
              medlwd = 2, whiskcol = "black", staplecol = "black",
              outcol="red", cex = 0.5, outpch = 19)
axis(side = 2, at = 1:length(b3$names), cex.axis = 0.7,
     labels = paste(
       b3$names |> gsub(pattern = '\\.', replacement = '\n', x = b3$names),
       # b3$names, 
       "\n(n=", b3$n,")", sep=""), 
     las = 1)
#
b4 <- boxplot(body_mass_g~droplevels(interaction(species, island, sex)),
              data = penguins, horizontal = T,
              main = 'Body Mass (g)', ylab = '', xlab = '',
              las = 1, boxwex = 0.4, yaxt = 'n',
              medlwd = 2, whiskcol = "black", staplecol = "black",
              outcol="red", cex = 0.5, outpch = 19)
axis(side = 2, at = 1:length(b4$names), cex.axis = 0.7,
     labels = paste(
       b4$names |> gsub(pattern = '\\.', replacement = '\n', x = b4$names),
       # b4$names,
       "\n(n=", b4$n,")", sep=""), 
     las = 1)
par(mfrow = c(1,1))
#
dev.off()
#

