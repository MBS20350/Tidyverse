library(lubridate)
library(tidyverse)
library(tm)

data <- read_csv("data_complaints.csv")

data$`Date received` <- as.Date(data$`Date received`, "%m/%d/%y")
data$`Date sent to company` <- as.Date(data$`Date sent to company`, "%m/%d/%y")
data$State <- as.factor(data$State)
data$`Submitted via` <- as.factor(data$`Submitted via`) 

q1date <- as.Date("01/01/18", "%m/%d/%y")
q1 <- data %>%
	filter(`Date received` > q1date) %>%
	filter(Product == "Student loan") %>%
	group_by(State) %>%
	summarise(count = n()) %>%
	arrange(desc(count))
## 12661  WRONG // 1327

q2 <- data %>%
	filter(`Submitted via` == "Phone") %>%
	mutate(gap = interval(`Date sent to company`, `Date received`)) %>%
	summarise(avgDelay dd= mean(gap))
q2/86400
## 2.6 (assuming days) RIGHT

q3 <- data %>%
	filter(Product == "Mortgage") %>%
	filter(str_detect(`Consumer complaint narrative`, "student")) %>%
	summarise(count = n())
## 135 WRONG

q3 <- data %>%
	filter(Product == "Credit card or prepaid card") %>%
	mutate(count = str_count(`Consumer complaint narrative`, "student")) %>%
	filter(!is.na(count)) %>%
	summarise(sum = sum(count)) 
## 222


q4 <- data %>%
	filter(Product == "Vehicle loan or lease") %>%
	group_by(Issue) %>%
	mutate(charLnth = str_length(`Consumer complaint narrative`)) %>%
	filter(!is.na(charLnth)) %>%
	summarise(lnthMean = mean(charLnth)) %>%
	arrange(desc(lnthMean))
## Getting a loan or lease RIGHT

q4 <- data %>%
	filter(Product == "Student loan") %>%
	group_by(Issue) %>%
	mutate(charLnth = str_length(`Consumer complaint narrative`)) %>%
	filter(!is.na(charLnth)) %>%
	summarise(lnthMean = mean(charLnth)) %>%
	arrange(desc(lnthMean))
## Dealing with your lender or servicer

q5 <- data %>%
	filter(Product == "Credit card or prepaid card") %>%
	select(Product, `Consumer complaint narrative`) %>%
	filter(!is.na(`Consumer complaint narrative`)) %>%
	filter(!str_detect(`Consumer complaint narrative`, "XX"))
q5$ID <- c(1:4892)

documents <- q5$`Consumer complaint narrative` 

documents <- Corpus(VectorSource(documents))
documents = tm_map(documents, content_transformer(tolower))
documents = tm_map(documents, removePunctuation)
documents = tm_map(documents, removeWords, stopwords("english"))
set <- documents$content

tbl <- table(unlist(strsplit(set, "\\s+")))
ans <- sort(tbl, decreasing = TRUE)
## credit card account RIGHT
