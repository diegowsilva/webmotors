require(RSelenium)
require(rvest)
require(tidyverse)
require(plotly)
rm(list=ls())

eCap <- list(chromeOptions=list(binary="/Applications/Google\ Chrome\ Canary.app/Contents/MacOS/Google\ Chrome\ Canary"))

brand = "fiat"
model = "freemont"
url = paste0("https://www.webmotors.com.br/carros/sp-sao-paulo/",brand,"/",model)


get_cars = function(brand,model){
  brand = "fiat"
  model = "freemont"
  url = paste0("https://www.webmotors.com.br/carros/sp-sao-paulo/",brand,"/",model)
  rD <- rsDriver(extraCapabilities = eCap)
  remDr <- rD[["client"]]
  
  remDr$navigate(url)
  
  ### n_anuncios
  # //*[@id="box-seo-count"]/span
  
  n_pages = remDr$getPageSource() %>%
    .[[1]] %>%
    read_html() %>%
    html_nodes(.,xpath = '//*[@id="box-seo-count"]/span') %>%
    html_text() %>%
    str_extract(.,"\\d+") %>%
    as.numeric %>%
    "/"(36) %>%
    ceiling
  
  
  remDr$close()
  # stop the selenium server
  rD[["server"]]$stop() 
  all_info = c()
  for(page in 1:1){
    rD <- rsDriver(extraCapabilities = eCap)
    remDr <- rD[["client"]]
    page=1
    url = paste0("https://www.webmotors.com.br/carros/sp-sao-paulo/",brand,"/",model,"?qt=36&p=",page)
    remDr$navigate(url)
    
    source = remDr$getPageSource()
    
    # prices 
    price = source %>%
      .[[1]] %>%
      read_html() %>%
      html_nodes(.,xpath = '//*[@itemprop="price"]') %>%
      html_text()
    
    # other_infos
    info = source %>%
      .[[1]] %>%
      read_html() %>%
      html_nodes(.,xpath = '//*[@class="info-veiculo-detalhe"]') %>%
      html_text() %>%
      matrix(.,ncol = 3,byrow = T)
    
    all_info=rbind(all_info,data.frame(cbind(price,info),stringsAsFactors = F))
    remDr$close()
    # stop the selenium server
    rD[["server"]]$stop() 
    
  }
  
  names(all_info) = c('price','year','km','transmission')
  
  all_info$price %<>%
    gsub(.,pattern = '[^0-9]',replacement = '') %>%
    as.numeric
  
  
  all_info$year %<>%
    gsub(.,pattern = '.+/',replacement = '') %>%
    as.numeric
  
  all_info$km %<>%
    gsub(.,pattern = '[^0-9]',replacement = '') %>%
    as.numeric
  all_info$brand = brand
  all_info$model = model
  return(all_info)
}

get_cars("dodge","journey")



all_info2 = all_info %>%
  filter(year >= 2012)

all_info2$year %<>% 
  as.factor

p = ggplot(all_info2,aes(x=km,y=price,color=as.factor(year))) +
  geom_point()

p = ggplotly(p, tooltip="text")

p

