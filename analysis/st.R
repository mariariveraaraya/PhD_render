## ---- tb-st

library(knitr)
library(here)
library(bookdown)
library(tidyverse)
library(kableExtra)

st<-read.csv(here("data","statement_contribution_2.csv"))

st2<- st %>%rename('Nature of assistance'= Nature.of.Assistance, 'Names of contributors'= Names..Titles..if.relevant..and.Affiliations.of.Co.Contributors)
