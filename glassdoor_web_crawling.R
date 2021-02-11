rm(list=ls())
#read_html(baseurl)
library(xml2)
library(XML)
library(rvest)

baseurl <- "https://www.glassdoor.com/Reviews/Company-Reviews-"
companyNum <- "E1259396"
sort <- ".htm?sort.sortType=RD&sort.ascending=true"


full_url <- paste(baseurl, companyNum, sep = "")

df <- read_html(paste(baseurl, companyNum, sort, sep = ""))

total_review_count <- html_nodes(df, ".mt+ div strong")
num_review_count <- gsub('<strong>',"",total_review_count)
num_review_count <- gsub('</strong>',"",num_review_count)
num_review_count_numeric <- as.numeric (num_review_count)
total_pages_to_read <- round(num_review_count_numeric/10)

#html_nodes(selected_page,".tightVert.floatLt strong, .margRtSm.minor")
#html_nodes(selected_page, "p")

#### Pros Data Extraction ##############
#####################################################  Page 1 Pros data extraction ###############################################
Pros <- html_nodes(df, ".mb-0+ .common__EiReviewTextStyles__allowLineBreaks .strong+ p")
html_text(Pros)
#####################################################  Page 2 to all page reading Pros data extraction   #######################################
pros_list <- list()
  
for(i in 2:total_pages_to_read )  #####  total_pages_to_read-60
{
  print(i)
  baseurl <- "https://www.glassdoor.com/Reviews/Company-Reviews-"
  companyNum <- "E1259396_P"
  page_number <- as.character(i)
  sort <- ".htm?sort.sortType=RD&sort.ascending=true&countryRedirect=true"
  
  df_page <- read_html(paste(baseurl, companyNum,page_number, sort, sep = ""))
  #df_page
  print(paste(baseurl, companyNum,page_number, sort))
  #Pros_oth <- html_nodes(df_page, ".mb-0+ .common__EiReviewTextStyles__allowLineBreaks .strong+ p")
  #Pros_oth <- html_nodes(df_page, "#ReviewsFeed .pl-0")'
  Pros_oth <- html_nodes(df_page,".mb-0+ .common__EiReviewTextStyles__allowLineBreaks .strong+ p")
  pros_list <- html_text(Pros_oth)
  print(pros_list)
  
}


#### Cons Data Extraction ##############
#####################################################  Page 1 Pros data extraction ###############################################
cons <- html_nodes(df, ".common__EiReviewTextStyles__allowLineBreaks:nth-child(7) .strong+ p")
html_text(cons)
#####################################################  Page 2 to all page reading Pros data extraction   #######################################
cons_list <- list()

for(i in 2:total_pages_to_read )  #####  total_pages_to_read-60
{
  print(i)
  baseurl <- "https://www.glassdoor.com/Reviews/Company-Reviews-"
  companyNum <- "E1259396_P"
  page_number <- as.character(i)
  sort <- ".htm?sort.sortType=RD&sort.ascending=true&countryRedirect=true"
  
  df_page_cons <- read_html(paste(baseurl, companyNum,page_number,sort, sep = ""))
  #df_page
  print(paste(baseurl, companyNum,page_number, sort, sep = ""))

  cons_oth <- html_nodes(df_page_cons,".common__EiReviewTextStyles__allowLineBreaks:nth-child(7) .strong+ p")
  cons_list <- html_text(cons_oth)
  print(cons_list)
  
}

