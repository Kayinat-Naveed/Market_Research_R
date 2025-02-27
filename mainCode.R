# install required packages 
install.packages("tidyverse")
install.packages("ggplot2")

# use libraries 
library(tidyverse, ggplot2)

# set directory 
setwd("D:/Msc/Semester 3/market research for innovation/marketResearch/")
getwd()



#import table 
surveyTable <- read.csv("D:/Msc/Semester 3/market research for innovation/marketResearch/MarketResearchClean.csv")

# remove french language 
surveyClean <- data.frame(lapply(surveyTable,function(col){
  gsub("/.*"," ", col)
}))

#____________________________________________________________________________________________________#

# Calculate total count and percentages
likeMost <- table(unlist(strsplit(as.character(surveyClean$Like.most), ";")))
total <- sum(likeMost)
percentages <- (likeMost / total) * 100 

# Adjust the margins to accommodate lengthy y-axis labels
par(mar = c(5, 15, 4, 2) , oma = c(0, 0, 2, 0)) # Bottom, Left, Top, Right margins

# Create the bar plot
bar_positions <- barplot(
  sort(percentages, decreasing = TRUE),  # Use percentages instead of counts
  horiz = TRUE,
  col = "red",
  border = "black",
  xlim = c(0, max(percentages) + 5),  # Adjust x-axis limit
  las = 1,                            # Rotate y-axis labels for better readability
  cex.names = 0.8                     # Adjust size of axis labels
)

# Add text labels with percentages
text(
  x = sort(percentages, decreasing = TRUE) + 2,  # Adjust label position slightly
  y = bar_positions,
  labels = paste0(round(sort(percentages, decreasing = TRUE), 1), "%"),  # Add percentage labels
  col = "black",
  cex = 0.8                                     # Adjust label size
)

# Add a centered title in the outer margin
title(
  main = "Most Appreciated Feature Of Coke Zero",
  outer = TRUE,  # Place the title in the outer margin
  cex.main = 1.5 # Adjust font size of the title
)

#___________________________________________________________________________________________________________#

# Calculate total count and percentages
improve_raw <- table(unlist(strsplit(as.character(surveyClean$Improve.appeal), ";")))

improve_clean <- trimws(tolower(improve_raw))

improve <- table(improve_clean)

totalimp <- sum(improve)                       # Total count of responses
percentimp <- (improve / totalimp) * 100       # Calculate percentages

# Adjust the margins to accommodate lengthy y-axis labels and provide space for the title
par(mar = c(5, 18, 4, 2), oma = c(0, 0, 2, 0)) # Adjusted margins for better spacing

# Create the horizontal bar chart
imp_positions <- barplot(
  sort(percentimp, decreasing = TRUE),  # Use sorted percentages for bar lengths
  horiz = TRUE,                         # Set bars to horizontal
  col = "red",
  border = "black",
  xlim = c(0, max(percentimp) + 5),     # Adjust x-axis limit for label placement
  cex.names = 0.8,                      # Adjust size of axis labels
  las = 1                               # Rotate y-axis labels for better readability
)

# Add text labels with percentages at the end of each bar
text(
  x = sort(percentimp, decreasing = TRUE) + 3,  # Position labels slightly beyond bar end
  y = imp_positions,                           # Align labels with bar positions
  labels = paste0(round(sort(percentimp, decreasing = TRUE), 1), "%"),  # Add percentage labels
  col = "black",
  cex = 0.8                                     # Adjust label size
)

# Add a centered title in the outer margin
title(
  main = "Most Appealing Improvements Of Coke Zero",
  outer = TRUE,  # Place the title in the outer margin
  cex.main = 1.5 # Adjust font size of the title
)
