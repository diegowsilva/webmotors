# get car by brand and name

get_cars_rvest = function(brand,model){
  brand = "fiat"
  model = "freemont"
  url = paste0("https://beta.webmotors.com.br/carros/sp-sao-paulo/",brand,"/",model)
  
  ### n_anuncios
  # //*[@id="box-seo-count"]/span
  
  n_pages = url %>%
    read_html() %>%
    html_nodes(.,xpath = '//*[@id="box-seo-count"]/span') %>%
    html_text() %>%
    str_extract(.,"\\d+") %>%
    as.numeric %>%
    "/"(36) %>%
    ceiling
  
  all_info = c()
  for(page in 1:n_pages){
    message(paste0("Getting - ",brand," - ",model," at page ",page,"/",n_pages))
    url = paste0("https://www.webmotors.com.br/carros/sp-sao-paulo/",brand,"/",model,"?qt=36&p=",page)
    
    source = url %>%
      read_html()
    # prices 
    price = source  %>%
      html_nodes(.,xpath = '//*[@itemprop="price"]') %>%
      html_text()
    
    # other_infos
    info = source %>%
      html_nodes(.,xpath = '//*[@class="info-veiculo-detalhe"]') %>%
      html_text() %>%
      matrix(.,ncol = 3,byrow = T)
    
    all_info=rbind(all_info,data.frame(cbind(price,info),stringsAsFactors = F))
  }
  
  names(all_info) = c('price','year','km','transmission')
  
  all_info$price %<>%
    gsub(.,pattern = '[^0-9]',replacement = '') %>%
    as.numeric
  
  
  all_info$year %<>%
    gsub(.,pattern = '.+/',replacement = '') %>%
    as.numeric
  
  all_info$transmission %<>%
    iconv(.,to="ASCII//TRANSLIT") %>%
    gsub(.,pattern = '[^a-zA-Z]',replacement = '')
  
  all_info$km %<>%
    gsub(.,pattern = '[^0-9]',replacement = '') %>%
    as.numeric
  all_info$brand = brand
  all_info$model = model
  return(all_info)
}

get_cars_rselenium = function(brand,model){
  eCap <- list(chromeOptions=list(binary="/Applications/Google\ Chrome\ Canary.app/Contents/MacOS/Google\ Chrome\ Canary"))
  url = paste0("https://www.webmotors.com.br/carros/sp-sao-paulo/",brand,"/",model)
  
  rD <- rsDriver(extraCapabilities = eCap)
  remDr <- rD[["client"]]
  remDr$navigate(url)
  source = remDr$getPageSource() %>%
    .[[1]] %>%
    read_html()
  
  n_pages = source %>%
    html_nodes(.,xpath = '//*[@id="box-seo-count"]/span') %>%
    html_text() %>%
    str_extract(.,"\\d+") %>%
    as.numeric %>%
    "/"(36) %>%
    ceiling
  
  remDr$close()
  rD[["server"]]$stop() 
  
  all_info = c()
  for(page in 1:n_pages){
    message(paste0("Getting - ",brand," - ",model," at page ",page,"/",n_pages))
    url = paste0("https://www.webmotors.com.br/carros/sp-sao-paulo/",brand,"/",model,"?qt=36&p=",page)
    remDr <- rsDriver(extraCapabilities = eCap)[["client"]]
    remDr$navigate(url)
    source = remDr$getPageSource() %>%
      .[[1]] %>%
      read_html()
    
    # prices 
    price = source  %>%
      html_nodes(.,xpath = '//*[@itemprop="price"]') %>%
      html_text()
    
    # other_infos
    info = source %>%
      html_nodes(.,xpath = '//*[@class="info-veiculo-detalhe"]') %>%
      html_text() %>%
      matrix(.,ncol = 3,byrow = T)
    
    all_info=rbind(all_info,data.frame(cbind(price,info),stringsAsFactors = F))
    remDr$close()
    rD[["server"]]$stop() 
  }
  
  names(all_info) = c('price','year','km','transmission')
  
  all_info$price %<>%
    gsub(.,pattern = '[^0-9]',replacement = '') %>%
    as.numeric
  
  
  all_info$year %<>%
    gsub(.,pattern = '.+/',replacement = '') %>%
    as.numeric
  
  all_info$transmission %<>%
    iconv(.,to="ASCII//TRANSLIT") %>%
    gsub(.,pattern = '[^a-zA-Z]',replacement = '')
  
  all_info$km %<>%
    gsub(.,pattern = '[^0-9]',replacement = '') %>%
    as.numeric
  all_info$brand = brand
  all_info$model = model
  return(all_info)
}