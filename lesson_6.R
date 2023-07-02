install.packages("plotly")
install.packages("maps")
install.packages("mapproj")
library(tidyverse)
library(plotly)
library(maps)
library(mapproj)


df <- read.csv("healthcare-dataset-stroke-data.csv")


View(df)

df2 <- df


hist(df2$age)
