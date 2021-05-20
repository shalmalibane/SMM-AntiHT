install.packages('pheatmap') 
install.packages("viridis")

library(pheatmap)
library(grid)    
library(viridis)
setwd("/Users/shalmali/Desktop/Repro Epi Research/6. Optum Comorbidities and SMM/Figures")

## data
data <- read.csv("R_heatmap_grid_05.08.21.csv", header = TRUE)
row_names <- list("Labetalol",	"Methyldopa",	"Nifedipine",	"Other",	"ACE",	"ARB",	"Other",	"Safe/NR mix")

## Edit body of pheatmap:::draw_colnames, customizing it to your liking
draw_colnames_45 <- function (coln, ...) {
  m = length(coln)
  x = (1:m)/m - 1/2/m
  grid.text(coln, x = x, y = unit(0.96, "npc"), vjust = .5, 
            hjust = 1, rot = 45, gp = gpar(...)) ## Was 'hjust=0' and 'rot=270'
}

draw_colnames_45 <- function (coln, gaps, ...) {
  coord = pheatmap:::find_coordinates(length(coln), gaps)
  x = coord$coord - 0.5 * coord$size
  res = textGrob(coln, x = x, y = unit(1, "npc") - unit(3,"bigpts"), vjust = 0.5, hjust = 1, rot = 45, gp = gpar(...))
  return(res)}

## 'Overwrite' default draw_colnames with your own version 
assignInNamespace(x="draw_colnames", value="draw_colnames_45",
                  ns=asNamespace("pheatmap"))

## Single color 
pheatmap(data, 
         display_numbers = T, 
         color = colorRampPalette(c('white','purple'))(100), 
         cluster_rows = F, 
         cluster_cols = F, 
         number_format = "%.1f",
         number_color = "grey10",
         labels_row = row_names,
         labels_col = row_names,
         fontsize_number = 14)

## Viridis
pheatmap(data, 
         display_numbers = T, 
         color = viridis(100, direction = -1, option = "D"), 
         cluster_rows = F, 
         cluster_cols = F, 
         number_format = "%.1f",
         number_color = "white",
         labels_row = row_names,
         labels_col = row_names,
         fontsize_number = 13)



