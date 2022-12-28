pacman::p_load(dplyr, tidyr, rvest, tidyverse)

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
#data_01_02 <- ExtractData(ListDates[1,5])


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
