# Real-Estate-data-web-scraping
These are my raw and unpolished R codes to capture data from property websites.   
The reasons that I do this is because: 
1. I want to practise my skills of playing Rselenium and rvest packages 
2. Most real estate webs do not provide sufficient options to filter keywords such as "heating" and "air conditioning".  
3. Efficiently select the property pool that I want to rent/buy 
4. Collect data to analyse and model regression to evaluate the price.  I am still working on the 4th object.  

This project would also work as an entry-level tutorial for people who are interested in Rselenium and rvest. 

The following codes are just an example to capture 80 properties for rent. Rselenium and rvest both do the work. However, domain.com is not a website that performs massive dynamic javascript, so that I can mainly rely on rvest rather than Rselenium. 

Rvest can handle the most entry-level tasks and it is faster than Rselenium. However, rvest cannot capture data from dynamic webs, it is where we have to use Rselenium. 

Rselenium mimics human behaviours to capture information like a real person. It builds a remote server and mimics our behaviour by codes which include click button, type text and copy. Rselenium is slower and inefficient comparing to rvest, but it is the best option to scrape the dynamic web for entry-level users. 

Some useful tutorials reference: 
1. https://thatdatatho.com/tutorial-web-scraping-rselenium/
2. https://github.com/yusuzech/r-web-scraping-cheat-sheet/blob/master/README.md

```{r packages}
library(tidyverse)
library(RSelenium)
library(rvest)
library(xml2)
```

```{r server pack}
# Check the available chrome version, the default is latest
verlist <- binman::list_versions("chromedriver") %>% flatten_chr()
# check my chrome version. wmic: WMI command-line 
version <- system2(command = "wmic",args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',stdout = TRUE,stderr = TRUE)
version <- str_sub(version[3],start = 9,end = -2)
# select the latest version earlier the current chrome version 
ver <- max(verlist[version > verlist])
# driver setting
driver <- rsDriver(browser = "chrome", chromever = ver)
driver$client$close()
redr <- driver[["client"]]
redr$open()
# redr$close()
```

```{r port kill}
# to cleanup the port, by kill the java instance(s) inside Rstudio
# refer: https://github.com/ropensci/RSelenium/issues/228
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
# check if ports still open
pingr::ping_port("localhost", 4567)
```

```{r url}
url <- "https://www.domain.com.au/rent/clayton-vic-3168/?ptype=duplex,free-standing,new-home-designs,new-house-land,semi-detached,terrace,town-house,villa&bedrooms=2-any&price=300-400&excludedeposittaken=1&carspaces=1-any&page=1"
```

```{r selenium}
redr$navigate(url)

urls <- sapply(1:4, function(x){
  url <- "https://www.domain.com.au/rent/clayton-vic-3168/?ptype=duplex,free-standing,new-home-designs,new-house-land,semi-detached,terrace,town-house,villa&bedrooms=2-any&price=300-400&excludedeposittaken=1&carspaces=1-any&page="
  urls <- paste0(url,x)
})

# sample check
each_prop_links <- redr$findElements(using = "xpath", value = "//*[@class='address is-two-lines css-1y2bib4']")
all_links_a_page <- sapply(each_prop_links,function(x){x$getElementAttribute("href")})
all_links_a_page <- data.frame(link = unlist(all_links_a_page))

# rvest
test_response <- GET(urls[1])
test_page <- content(test_response , as = "parsed")
test_page_links <- test_page %>% html_nodes(".address.is-two-lines.css-1y2bib4") %>% html_attr("href")


store_links <- data.frame()
pagelinks <- function(urls){
  response <- GET(urls)
  page <- content(response, as = "parsed")
  one_page_links <- page %>% html_nodes(".address.is-two-lines.css-1y2bib4") %>% 
    html_attr("href") %>% data.frame(links = .)
  store_links <<- rbind(store_links, one_page_links)
}

sapply(urls,pagelinks)

library(data.table)
# check if there are duplicates
store_links %>% uniqueN()


# full links
links_all <- data.frame()
for(i in 1:(length(urls))){
  redr$navigate(paste0(urls[[i]]))
  Sys.sleep(3)
  links <- redr$findElements(using = "xpath", value = "//*[@class='address is-two-lines css-1y2bib4']")
  df <- sapply(links,function(x){x$getElementAttribute("href")})
  df <- data.frame(link = unlist(df))
  Sys.sleep(1)
  links_all = rbind(links_all, df)
}
```

```{r test sample}
library(httr)
links <- sapply(links_all, as.character)
links <- sapply(store_links, as.character)
redr$navigate(links[73])

response <- GET(links[1])
page <- content(response, as = "parsed")
# price
price <- page %>% html_nodes(".css-1texeil") %>% 
  html_text() %>% 
  str_extract(.,"\\-*\\d+\\.*\\d*")

# address
page %>% html_nodes(".css-164r41r") %>% 
  html_text() %>% 
  word(.,-1)
# house type
page %>% html_nodes(".css-in3yi3") %>% 
  html_text() %>% 
  .[[2]]
# bedroom
bed <- page %>% html_nodes(".css-lvv8is") %>% 
  html_text() %>% 
  .[[1]] %>% word(.,1,1)

bath <- page %>% html_nodes(".css-lvv8is") %>% 
  html_text() %>% 
  .[[2]] %>% word(.,1)

park <- page %>% html_nodes(".css-lvv8is") %>% 
  html_text() %>% 
  .[[3]] %>% word(.,1)

# description
decp <- page %>% html_nodes(".css-bq4jj8 p") %>% 
  html_text() %>% 
  paste(collapse = " ")
# test if there is heating information
decp %>% 
  str_detect("heat")

ifelse(str_detect(decp,"heat"),1,0)
# test if there is condition
ifelse(str_detect(decp,"condition"),1,0)
```

```{r rvest}

df_all_data <- data.frame()

scraper <- function(links){
  # single url 
  url <- links
  # PARSE PAGE URL
  response <- GET(url)
  page <- content(response, as = "parsed")
  Sys.sleep(0.5)
  # rent price
  rent_price <- 
    page %>% html_nodes(".css-1texeil") %>% 
    html_text() %>% 
    str_extract(.,"\\-*\\d+\\.*\\d*")
  # address
  address <- 
    page %>% html_nodes(".css-164r41r") %>% 
    html_text()
  #postcode
  postcode <- 
    address %>% word(.,-1)
  # property type
  type <- page %>% html_nodes(".css-in3yi3") %>% 
    html_text() %>% 
    .[[2]]
  # bedroom
  bedroom <- page %>% html_nodes(".css-lvv8is") %>% 
    html_text() %>% 
    .[[1]] %>% word(.,1,1)
  #bathroom
  bathroom <- page %>% html_nodes(".css-lvv8is") %>% 
    html_text() %>% 
    .[[2]] %>% word(.,1)
  # park
  parkspace <- page %>% html_nodes(".css-lvv8is") %>% 
    html_text() %>% 
    .[[3]] %>% word(.,1)
  # description
  description <- 
    page %>% html_nodes(".css-bq4jj8 p") %>% 
    html_text() %>% 
    paste(collapse = " ")
  # heat related information 
  heat <- ifelse(str_detect(description," heat"),1,0)
  # air condition information
  ac <- ifelse(str_detect(description," air "),1,0)
  # store single property information
  df_one_page <- data.frame(rent_price = rent_price,
                            address = address,
                            postcode = postcode,
                            type = type,
                            bedroom = bedroom,
                            bathroom = bathroom,
                            parkspace = parkspace,
                            description = description,
                            heat = heat,
                            ac = ac)
  df_all_data <<- rbind(df_all_data, df_one_page)
}

sapply(links, scraper)

df_all_data <- df_all_data %>% data.table()
df_all_data[, links := links]
