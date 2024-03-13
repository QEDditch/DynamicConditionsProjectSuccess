library(readr)
library(tidyr)
library(tidyverse)
library(dplyr)
library(tm)
library(wordcloud2)                
library(networkD3)

mybib = read_csv("science progress bibliometrics.csv")
data.frame(colnames(mybib))
dim(mybib)

mybib %>% 
  filter(`Item Type` == "journalArticle") %>% 
  group_by(`Publication Title`) %>% 
  tally(sort = TRUE) %>% 
  mutate(Percentage = 100*(n / sum(n))) %>% 
  rename("Number of Studies" = n) %>% 
  rename("Publication Title" = `Publication Title`) %>% 
  write_csv(file = "Journal Counts.csv")


mybib$`Publication Year` <- as.integer(mybib$`Publication Year`)
p <- ggplot(mybib, aes(x = `Publication Year`))
p + geom_histogram(binwidth = 5, fill = "blue")
ggsave("articles by year.png")

mybib2 = read_csv("C:/Users/David Eggleton/Downloads/diamond.csv")
data.frame(colnames(mybib2))
dim(mybib2)

mybib2 %>% 
  filter(`Item Type` == "journalArticle") %>% 
  group_by(`Publication Title`) %>% 
  tally(sort = TRUE) %>% 
  mutate(Percentage = 100*(n / sum(n))) %>% 
  rename("Number of Studies" = n) %>% 
  rename("Publication Title" = `Publication Title`) %>% 
  write_csv(file = "Journal Counts diamond.csv")


mybib2$`Publication Year` <- as.integer(mybib$`Publication Year`)
p <- ggplot(mybib, aes(x = `Publication Year`))
p + geom_histogram(binwidth = 5, fill = "blue")
ggsave("articles by year diamond.png")