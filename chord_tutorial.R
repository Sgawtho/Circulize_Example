#===CHORD DIAGRAMS USING THE CIRCULIZE PACKAGE IN R===###############################

library(dplyr)
library(circlize)
library(reshape2)
#####################################################################################
df = read.csv('toy_data.csv') #The data included here is fake trafficking flows data 

#The data is often not tidy for graphing so I thought I would
#introduce a few common steps I undertake for initial set up.
#Labeling chords can be tricky as they often overlap the nodes
levels(df$from) = c("Caribbean","Central America","E. & SE Asia","East Europe",
                     "N. & W.Europe", "North Africa", "North America", "Oceania", 
                     "Rest World", "South America","South Europe", 
                     "W.& Mid-Africa", "West Asia", "Balkans") 
levels(df$to) = c("Caribbean","Central America","E. & SE Asia","East Europe",
                   "N. & W.Europe", "North Africa", "North America", "Oceania", 
                   "Rest World", "South America","South Europe", 
                   "W.& Mid-Africa", "West Asia", "Balkans") 
#Cube root to non-linearly scale the numbers down
#This can be a useful replacement for log transform to fit data  
math.cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}

df$value = math.cbrt(df$value)
#Below helps transform a dataframe into a matrix
#circulize works with dataframes but I find that
#converting to a matrix is by far easier to work with the code
library(reshape2)
df = acast(df, from~to, value.var = 'value', sum)
#
#I use this loop below to remove all recusive sectors and set them to zero#####

for(cn in intersect(rownames(df), colnames(df))) {
  df[cn, cn] = 0
}
#####################################################################################
#======================PLOTTING THE CHORD DIAGRAM===================================# 
circos.clear()
circos.par(start.degree = 0, 
           gap.degree = .6, # +/- gap degree value adjusts node spacing & helps if labels overlap
           track.margin = c(-0.1, 0.1), 
           points.overflow.warning = FALSE)

chordDiagram(df, directional = 1,
             direction.type = c("arrows", "diffHeight"), #settings here show direction of flow 
             transparency = 0.15, #helps see overlapping flows
             link.sort = TRUE,
             link.arr.width = 0.05,
             link.arr.type = 'big.arrow', 
             link.lwd = .5,
             annotationTrack = 'grid', 
             link.border = 'darkgrey', 
             self.link = 1, 
             big.gap = 3)

circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    # Add names to the sector. 
    circos.text(
      x = mean(xlim), 
      y = 1.9, 
      labels = sector.index, 
      facing = "bending", #Change this setting for label placement
      niceFacing = FALSE,
      cex = 0.5
    )
  }
)
#####################################################################################
