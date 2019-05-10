
suppressWarnings({
    
  library(knitr)
  library(car)
  library(data.table)
  library(forcats)
  library(doBy)
  library(stringr)
  library(lubridate)
  library(DT)
  library(magrittr)
  library(thief)
  library(TSA)
  library(ggplot2)
  library(plotly)
  library(mgcv)
  #library(MTS)
  #library(hts)
  #library(fields) detach("package:fields", unload = T)
  #library(xlsx)
  
  library(tidyverse)
})

knitr::opts_chunk$set(
  echo = F, 
  warning = F, 
  fig.align = 'center', 
  fig.height = 6, 
  fig.width = 10
)

