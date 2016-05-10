
---
title: "Initial loan book analysis"
author: "Eryk Walczak"
date: "6 May 2016"
output: html_document
---

```{r load, echo=FALSE, warning=FALSE, message=FALSE}
library(choroplethr)
library(choroplethrMaps)
library(DescTools)
library(dplyr)
library(ggplot2)
library(readr)

options(scipen=999)
loanbook <- read_csv("../input/loan.csv")

# load the state names
data(state.regions)

# merge the loan book with the state names
loanbook <- merge(loanbook, state.regions, by.x = "addr_state", by.y = "abb")
```

## Data overview

After loading the entire data set I'd like to check its structure and summarise it. 

```{r, echo = TRUE}
# print dimentions
dim(loanbook)

# print column names
colnames(loanbook)
```

I'm not printing these results for conciseness:

```{r, eval=FALSE}
str(loanbook)
summary(loanbook)
```

## Loan amounts

I focused on several variables and I plotted them using DescTools package.
Here I created the density plot, box plot, and empirical distribution function plot.
```{r, echo=TRUE}
Desc(loanbook$loan_amnt, main = "Loan amount distribution", plotit = TRUE)
```

Here's how the loan book was growing:
```{r, echo=TRUE}
loanbook$issue_d <- as.Date(gsub("^", "01-", loanbook$issue_d), format="%d-%b-%Y")

amnt_df <- loanbook %>% 
  select(issue_d, loan_amnt) %>% 
  group_by(issue_d) %>% 
  summarise(Amount = sum(loan_amnt))

ts_amnt <- ggplot(amnt_df, 
                  aes(x = issue_d, y = Amount))
ts_amnt + geom_line() + xlab("Date issued")
```

## Loan statuses

The loanbook consists of loans in various statuses so I started with exploring their frequency.
```{r, echo=TRUE}
Desc(loanbook$loan_status, plotit = T)
```

Then I checked the distribution of loan amounts by status.
```{r, echo=TRUE}
box_status <- ggplot(loanbook, aes(loan_status, loan_amnt))
box_status + geom_boxplot(aes(fill = loan_status)) +
  theme(axis.text.x = element_blank()) +
  labs(list(
    title = "Loan amount by status",
    x = "Status",
    y = "Amount"))  
```

Here's how the value of loans of different grades was changing over time
```{r, echo=TRUE}
amnt_df_grade <- loanbook %>% 
  select(issue_d, loan_amnt, grade) %>% 
  group_by(issue_d, grade) %>% 
  summarise(Amount = sum(loan_amnt))

ts_amnt_grade <- ggplot(amnt_df_grade, 
                  aes(x = issue_d, y = Amount))
ts_amnt_grade + geom_area(aes(fill=grade)) + xlab("Date issued")
```

## Maps

In order to create maps of loans I initially joined the loan book 
with the names of states and then I created a data frame required by choroplethr.

### Loans by value
```{r, echo=TRUE}
state_by_value <-
loanbook %>% group_by(region) %>%
  summarise(value = sum(loan_amnt, na.rm=TRUE))

state_choropleth(state_by_value, title = "Value by State")
```

### Loans by volume
```{r, echo=TRUE}
state_by_volume <-
loanbook %>% group_by(region) %>%
  summarise(value = n())

state_choropleth(state_by_volume, title = "Volume by State")
```

## Loan reasons

### What's the reason for taking a loan with LendingClub?

```{r, echo=TRUE}
Desc(loanbook$title, main = "Loan types", plotit = TRUE)
```

## Word cloud

Word cloud gives a good overview of the loan titles (gives by borrowers). 
This information should explain what kind of loans are being funded by LendingClub.

In order to create a word cloud, first I loaded the necessary libraries, 
then I preprocessed the 'title' column by removing punctuation and transforming it to lower case.
This analysis was run on the first 10000 rows to speed up the process.

```{r, echo=TRUE, warning=FALSE, message=FALSE}
library(tm)
library(RColorBrewer)
library(wordcloud)

loan_descriptions.corpus <- Corpus(DataframeSource(data.frame(head(loanbook[,23], n=10000))))
loan_descriptions.corpus <- tm_map(loan_descriptions.corpus, removePunctuation)
loan_descriptions.corpus <- tm_map(loan_descriptions.corpus, content_transformer(tolower))

wordcloud(loan_descriptions.corpus,
          max.words = 100,
          random.order=FALSE, 
          rot.per=0.30, 
          use.r.layout=FALSE, 
          colors=brewer.pal(8, "Paired"))
```

## Loan grades

Here is the overview of the occurrence of loans of different grades:
```{r, echo=TRUE}
Desc(loanbook$grade, main = "Loan grades", plotit = TRUE)
```

The last step (so far) was checking whether the interest rates are dependent on the loan grade.
```{r, echo=TRUE}
Desc(int_rate ~ grade, loanbook, digits = 1, main = "Interest rate by grade", plotit = TRUE)
```

Unsurprisingly, the higher the grade (more risky loan), the higher the interest rates.

## Missing variables

I've also noticed that several key fields described in the data dictionary were missing from the loan book:

```{r, echo=TRUE}
library(readxl)

dataDictionary <- read_excel("../input/LCDataDictionary.xlsx")

# fields available in the data dictionary
dd_names <- as.character(na.omit(dataDictionary$LoanStatNew))

# fields available in the loan book
loanbook_names <- names(loanbook)

# show the fields described in data dictionary but not in the loan book
setdiff(dd_names, loanbook_names)
```
