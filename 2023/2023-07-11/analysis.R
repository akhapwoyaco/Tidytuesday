#
# tuesdata <- tidytuesdayR::tt_load('2023-07-11')
tuesdata <- tidytuesdayR::tt_load(2023, week = 28)
global_temps <- tuesdata$global_temps
nh_temps <- tuesdata$nh_temps
sh_temps <- tuesdata$sh_temps
zonann_temps <- tuesdata$zonann_temps
#
head(global_temps)
#
jpeg("global_temp_box_plot.jpeg", width = 35, height = 20, units = "cm", res = 350)
box_plot = boxplot(
  global_temps[,2:13], 
  xaxt = "n", border = "white", col = "black", 
  boxwex = 0.3, medlwd = 1, whiskcol = "black", 
  staplecol = "black", outcol = "red", cex = 0.2, 
  outpch = 19, 
  main = "Global Temperatures by Month")
axis(
  side = 1, at = 1:length(box_plot$names), 
  labels = paste(box_plot$names, "\n(n=", box_plot$n, ")",sep = ""),
     mgp = c(3,2,0))
mtext("https://github.com/akhapwoyaco", side = 1, line = 3, padj = 1, adj = 0)
dev.off()
#
jpeg("nh_temps_box_plot.jpeg", width = 35, height = 20, units = "cm", res = 350)
box_plot = boxplot(
  global_temps[,2:13], 
  xaxt = "n", border = "white", col = "black", 
  boxwex = 0.3, medlwd = 1, whiskcol = "black", 
  staplecol = "black", outcol = "red", cex = 0.2, 
  outpch = 19, 
  main = "NH Temperatures by Month")
axis(
  side = 1, at = 1:length(box_plot$names), 
  labels = paste(box_plot$names, "\n(n=", box_plot$n, ")",sep = ""),
  mgp = c(3,2,0))
mtext("https://github.com/akhapwoyaco", side = 1, line = 3, padj = 1, adj = 0)
dev.off()
#
jpeg("sh_temps_box_plot.jpeg", width = 35, height = 20, units = "cm", res = 350)
box_plot = boxplot(
  global_temps[,2:13], 
  xaxt = "n", border = "white", col = "black", 
  boxwex = 0.3, medlwd = 1, whiskcol = "black", 
  staplecol = "black", outcol = "red", cex = 0.2, 
  outpch = 19, 
  main = "SH Temperatures by Month")
axis(
  side = 1, at = 1:length(box_plot$names), 
  labels = paste(box_plot$names, "\n(n=", box_plot$n, ")",sep = ""),
  mgp = c(3,2,0))
mtext("https://github.com/akhapwoyaco", side = 1, line = 3, padj = 1, adj = 0)
dev.off()
#
jpeg("zonann_temps_box_plot.jpeg", width = 35, height = 20, units = "cm", res = 350)
box_plot = boxplot(
  global_temps[,2:13], 
  xaxt = "n", border = "white", col = "black", 
  boxwex = 0.3, medlwd = 1, whiskcol = "black", 
  staplecol = "black", outcol = "red", cex = 0.2, 
  outpch = 19, 
  main = "zonann Temperatures by Month")
axis(
  side = 1, at = 1:length(box_plot$names), 
  labels = paste(box_plot$names, "\n(n=", box_plot$n, ")",sep = ""),
  mgp = c(3,2,0))
mtext("https://github.com/akhapwoyaco", side = 1, line = 3, padj = 1, adj = 0)
dev.off()
#