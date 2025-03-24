list.of.packages <- c("lubridate", "zoo", "binom", "devtools",
                      "foreign", "tidyverse", "readxl", "ggpubr", "scales",
                      "viridis", "plotly", "plyr", "ggpattern", 
                      "magick", "ggh4x", "here")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#loads:
lapply(list.of.packages, require, character.only = TRUE)

