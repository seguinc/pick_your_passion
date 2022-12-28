url<- "https://www.goodreads.com/choiceawards/best-books-2021"

urlNYT <- "https://www.nytimes.com/books/best-sellers/2022/09/25/"

# The easiest way to get rvest is to install the whole tidyverse:
install.packages("tidyverse")
# Alternatively, install just rvest:
install.packages("rvest")
```

## Usage

#```{r, message = FALSE}
library(rvest)
# Start by reading a HTML page with read_html():
test <- read_html(urlNYT)
# Then find elements that match a css selector or XPath expression
# using html_elements(). In this example, each <section> corresponds
# to a different film
test2 <- test %>% html_elements("body")
test3<-test2[[1]]


sections <- test3 %>% html_elements("section")

titles <- test3 %>% 
  html_elements("h3") %>% 
  html_text2()
titles


categories <- test3 %>% 
  html_elements("h2") %>% 
  html_text2()
categories

episode <- test3 %>% 
  html_element("h2") %>% 
  html_attr("class") %>% 
  readr::parse_integer()
episode


authors <- test %>%
  html_nodes(xpath = '//p[@itemprop="author"]') %>% 
  html_text2()
  #html_attr('content') 
authors

BookNames <- test %>%
  html_nodes(xpath = '//h3[@itemprop="name"]') %>% 
  html_text2()
BookNames

BookDescription  <- test %>%
  html_nodes(xpath = '//p[@itemprop="description"]') %>% 
  html_text2()

BookDescription[19]



--------------------
# Start again from subpage dedicated to combined-print-and-e-book-fiction
urlNYT_9_25_fiction<-"https://www.nytimes.com/books/best-sellers/2022/09/25/combined-print-and-e-book-fiction/"

content <- read_html(urlNYT_9_25_fiction)

Author <- content  %>%
  html_nodes(xpath = '//p[@itemprop="author"]') %>% 
  html_text2()%>%
  as.data.frame() 
#html_attr('content') 
Author

#extract book titles
Title <- content  %>%
  html_nodes(xpath = '//h3[@itemprop="name"]') %>% 
  html_text2()%>%
  as.data.frame() 
Title

#extract book descriptions
Description  <- content  %>%
  html_nodes(xpath = '//p[@itemprop="description"]') %>% 
  html_text2()%>%
  as.data.frame() 
Description

#extract publisher
Publisher  <- content  %>%
  html_nodes(xpath = '//p[@itemprop="publisher"]') %>% 
  html_text2()%>%
  as.data.frame() 
Publisher

#extract book cover images
CoverImageURL_prep  <- content  %>%
  html_elements("img") %>% 
  html_attr("src")
CoverImageURL <-CoverImageURL_prep [1:15]%>% #only keep the first 15 values 
  as.data.frame()  
CoverImageURL 


#extract rank
RankPrep  <- content  %>%
  html_nodes(xpath = '//article[@itemprop="itemListElement"]') %>% 
  html_text2() %>%
  as.data.frame() 

Rank <- row(RankPrep)%>%
  as.data.frame() 

  as.data.frame() %>%
  mutate(RankIndex = row(Rank))%>%
  select(RankIndex)
Rank

#extract weeks on list
WeeksOnList <- content  %>%
  html_elements(".css-1o26r9v")%>% 
  html_text2()%>%
  as.data.frame() 
WeeksOnList

#Combine all together in one data frame

data_9_25_22 <- cbind(Title, Author, Description, Publisher, CoverImageURL, WeeksOnList, Rank) 
names(data_9_25_22) <-c("Title", "Author", "Description", "Publisher", "CoverImageURL", "WeeksOnList"  , "Rank")
  


#Try it for the following week's data
urlNYT_10_2_fiction<-"https://www.nytimes.com/books/best-sellers/2022/10/02/combined-print-and-e-book-fiction/"

content_10_2 <- read_html(urlNYT_10_2_fiction)

Author_10_2 <- content_10_2  %>%
  html_nodes(xpath = '//p[@itemprop="author"]') %>% 
  html_text2()%>%
  as.data.frame() 
#html_attr('content') 
Author_10_2

#extract book titles
Title_10_2 <- content_10_2  %>%
  html_nodes(xpath = '//h3[@itemprop="name"]') %>% 
  html_text2()%>%
  as.data.frame() 
Title_10_2

#extract book descriptions
Description_10_2  <- content_10_2  %>%
  html_nodes(xpath = '//p[@itemprop="description"]') %>% 
  html_text2()%>%
  as.data.frame() 
Description_10_2

#extract publisher
Publisher_10_2  <- content_10_2  %>%
  html_nodes(xpath = '//p[@itemprop="publisher"]') %>% 
  html_text2()%>%
  as.data.frame() 
Publisher_10_2

#extract book cover images
CoverImageURL_prep_10_2  <- content_10_2  %>%
  html_elements("img") %>% 
  html_attr("src")
CoverImageURL_10_2 <-CoverImageURL_prep_10_2 [1:15]%>% #only keep the first 15 values 
  as.data.frame()  
CoverImageURL_10_2 


#extract rank
RankPrep_10_2  <- content_10_2  %>%
  html_nodes(xpath = '//article[@itemprop="itemListElement"]') %>% 
  html_text2() %>%
  as.data.frame() 

Rank_10_2 <- row(RankPrep_10_2)%>%
  as.data.frame() 

Rank_10_2

#extract weeks on list
WeeksOnList_10_2 <- content_10_2  %>%
  html_elements(".css-1o26r9v")%>% 
  html_text2()%>%
  as.data.frame() 
WeeksOnList_10_2

#Combine all together in one data frame

data_10_2_22 <- cbind(Title_10_2, Author_10_2, Description_10_2, Publisher_10_2, CoverImageURL_10_2, WeeksOnList_10_2, Rank_10_2) 
names(data_10_2_22) <-c("Title", "Author", "Description", "Publisher", "CoverImageURL", "WeeksOnList"  , "Rank")


#create list of all Sundays in 2022
ListDates <-seq(as.Date("2022-01-02"), as.Date("2022-12-25"), by="weeks")%>%
  as.data.frame()%>%
  rename("Date"=".") %>%
  mutate(Day = format(as.Date(Date, format="%d/%m/%Y"),"%d"))%>%
  mutate(Month = format(as.Date(Date, format="%d/%m/%Y"),"%m"))%>%
  mutate(Year = format(as.Date(Date, format="%d/%m/%Y"),"%Y"))%>%
  mutate(URL = urlNYT_10_2_fiction<-gsub(" ","",paste("https://www.nytimes.com/books/best-sellers/",Year,"/",Month,"/",Day,"/combined-print-and-e-book-fiction/")))

#Create a function that take a url and returns a data frame with all the data from that url

ExtractData<-function(url){
    #Extract data
  content_date <- read_html(url)
  
  #Extract authors
  Author <- content_date  %>%
    html_nodes(xpath = '//p[@itemprop="author"]') %>% 
    html_text2()
  
  Author<-    substr(Author,4,nchar(Author)) %>%
    as.data.frame() 
  
  #extract book titles
  Title <- content_date  %>%
    html_nodes(xpath = '//h3[@itemprop="name"]') %>% 
    html_text2()%>%
    as.data.frame() 
  
  
  #extract book descriptions
  Description  <- content_date  %>%
    html_nodes(xpath = '//p[@itemprop="description"]') %>% 
    html_text2()%>%
    as.data.frame() 

  #extract publisher
  Publisher  <- content_date  %>%
    html_nodes(xpath = '//p[@itemprop="publisher"]') %>% 
    html_text2()%>%
    as.data.frame() 
  
  #extract book cover images
  CoverImageURL_prep  <- content_date  %>%
    html_elements("img") %>% 
    html_attr("src")
  CoverImageURL <-CoverImageURL_prep [1:15]%>% #only keep the first 15 values 
    as.data.frame()  

    #extract rank
  RankPrep  <- content_date  %>%
    html_nodes(xpath = '//article[@itemprop="itemListElement"]') %>% 
    html_text2() %>%
    as.data.frame() 
  
  Rank <- row(RankPrep)%>%
    as.data.frame() 

  #extract weeks on list
  WeeksOnList <- content_date  %>%
    html_elements(".css-1o26r9v")%>% 
    html_text2()%>%
    as.data.frame() 

  #Combine all together in one data frame
  #and rename the columns
  data <- cbind(Title, Author, Description, Publisher, CoverImageURL, WeeksOnList, Rank) 
  names(data) <-c("Title", "Author", "Description", "Publisher", "CoverImageURL", "WeeksOnList"  , "Rank")
  
  #Add the url as a column
  data <- mutate(data, URL = url)
  
  #Return the data frame
  data
}

#Test it for the first week of the year
data_01_02 <- ExtractData(ListDates[1,5])


#Create an empty data frame
df <- data.frame(matrix(ncol = 8, nrow = 0))

#Extract for each week on the list and then union
for(i in 1:nrow(ListDates)) {
    df<-rbind(df,ExtractData(ListDates[i,5]))
  }

#Join the list of dates with the extracted data
FinalData <- inner_join(ListDates, df, by=c("URL"="URL"))

#Export data to csv
write.csv(FinalData,"H:/Personal/Data Viz PYP/PYP Books/FinalData_2022_NYTBestSellers.csv")

#-----------------------------
