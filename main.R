require(RSelenium)
require(rvest)
require(tidyverse)
require(plotly)
require(magrittr)

rm(list = ls())
source("code/functions.R")


# Getting Dodge Journey
all_info2 = rbind(
  get_cars_rvest("fiat","freemont")
  ,get_cars_rvest("dodge","journey")
)

all_info2 %<>%
  filter(km < 500000)

p = ggplot(all_info2,aes(x=km,y=price,color=as.factor(year),year=year,brand=brand,model=model)) +
  geom_point()

p
pp = ggplotly(p, tooltip=c("brand","model","year","km","price"))

pp