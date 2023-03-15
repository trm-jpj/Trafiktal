

library(readxl)
library(stringr)
library(gridExtra)
library(grid)
library(cowplot)
library(officedown)
library(officer)
library(zoo)
library(lubridate)
library(openxlsx)
library(statbank)
library(tidyverse)
library(highcharter)
library(ggthemes)
library(ggpp)
library(gpclib)
library(tidyverse)
library(scales)
library(knitr)
library(TRMvisual)
library(extrafont)
library(tidyquant)
library(TTR)

Sys.setlocale("LC_TIME", "Danish")

aar <- 2023

# Kører program der henter og danner rejsekort data
source("rejsekort.R")
# Kører program der henter og danner DSB data
source("DSB data.R")


rmarkdown::render('rapport.Rmd',
                  output_file = str_c('Trafikdata for kollektiv trafik ', Sys.Date() |> format("%d%m%Y"), '.docx'))

