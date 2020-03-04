---
title: "Data Mining Project"
author: "Darshan Durve & Akash Chitrey"
date: "April 14, 2019"
output:
  pdf_document: default
  html_document:
    df_print: paged
  word_document: default
---
#Appendix
##Loading the data and data cleaning
```{r, results="hide", fig.show='hide'}
library(readr)
library(dplyr)
library(plotly)
library(ggplot2)
library(purrr)
library(readr)

blackfriday <- read.csv(file.choose())
summary(blackfriday)
colSums(is.na(blackfriday))
blackfriday$Product_Category_2[is.na(blackfriday$Product_Category_2)] <- mean(blackfriday$Product_Category_2, na.rm = T)
blackfriday$Product_Category_3[is.na(blackfriday$Product_Category_3)] <- mean(blackfriday$Product_Category_3, na.rm = T)
colSums(is.na(blackfriday))
str(blackfriday)
```
#Exploratory Data Analysis
##Total Purchase Distribution
```{r, results="hide", fig.show='hide'}
blackfriday %>%
  group_by(User_ID) %>%
  summarise(total_purchase = sum(Purchase)) %>%
  ggplot(aes(x = total_purchase)) + 
  geom_histogram(col = 'black', fill = 'blue', binwidth = 300000, center = 150000) +
                   theme_linedraw() + 
                   theme(panel.background = element_rect(fill = "gainsboro", colour = "white", size = 0.7, linetype = "solid"), 
                         plot.background = element_rect(fill = "gainsboro"), 
                         panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "red"), 
                         panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "red"), 
                         plot.title = element_text(hjust = 0, face = 'bold',color = 'black'), 
                         plot.subtitle = element_text(face = "italic")) + 
                   labs(x = 'Total Purchase', y = 'Number of Consumers', title = "Black Friday Sales",
                        subtitle = "Distribution of total purchase by consumers") + 
                   scale_y_continuous(limits = c(0,2000), breaks = c(0,500,1000,1500,2000)) +
                   scale_x_continuous(labels = scales::comma) 
```

##Total Purchase by City
```{r, results="hide", fig.show='hide'}
blackfriday %>%
  group_by(User_ID, City_Category) %>%
  summarise(total_purchase = sum(Purchase)) %>%
  ggplot(aes(x = total_purchase, group = City_Category)) + 
  geom_histogram(aes(fill=City_Category),col = 'black', binwidth = 300000, center = 150000) +
  theme_linedraw() + 
  theme(legend.box.background	= element_rect(colour = "black"),
        legend.background = element_rect(fill = "darkgrey"),
        panel.background = element_rect(fill = "gainsboro", colour = "white", size = 0.5, linetype = "solid"), 
        plot.background = element_rect(fill = "gainsboro"), 
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "red"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "red"), 
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black'), 
        plot.subtitle = element_text(face = "italic")) + 
  labs(x = 'Total Purchase ($)', y = 'Number of Consumers', title = "Black Friday", 
       subtitle = "Distribution of total purchasing by consumers") + 
  guides(fill=guide_legend(title = "City")) +
  scale_y_continuous(limits = c(0,2000), breaks = c(0,500,1000,1500,2000)) +
  scale_x_continuous(labels = scales::comma) 
```

##Total Purchase by Gender
```{r, results="hide", fig.show='hide'}
gender <- blackfriday %>%
  group_by(Gender) %>%
  distinct(User_ID) %>%
  summarise(Total=n())
plot_ly(gender, labels = ~Gender, values = ~Total, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste(Total, 'People'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 4)), showlegend = FALSE) %>%
  layout(title = 'Gender', titlefont = list(size = 18, color = 'black'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
```

##Total Purchase by Age
```{r, results="hide", fig.show='hide'}
age <- blackfriday %>%
  group_by(Age) %>%
  summarise(Total=n())

#make a pie chart with plotly package
plot_ly(age, labels = ~Age, values = ~Total, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste(Total, 'People'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE) %>%
  layout(title = 'Age Distribution', titlefont = list(size = 18, color = 'black'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
```

##Total Purchase by Marital Status
```{r, results="hide", fig.show='hide'}
marital_status <- blackfriday %>%
  group_by(Marital_Status) %>%
  summarise(Total=n())
plot_ly(marital_status, labels = ~Marital_Status, values = ~Total, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste(Total, 'People'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE) %>%
  layout(title = 'Marital Status', titlefont = list(size = 18, color = 'black'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
```

##Total Purchase by Occupation
```{r, results="hide", fig.show='hide'}
customers_total_purchase_amount = blackfriday %>%
  group_by(User_ID) %>%
  summarise(Purchase_Amount = sum(Purchase))

customers_Occupation =  blackfriday %>%
  select(User_ID, Occupation) %>%
  group_by(User_ID) %>%
  distinct() %>%
  left_join(customers_total_purchase_amount, Occupation, by = 'User_ID')


totalPurchases_Occupation = customers_Occupation %>%
  group_by(Occupation) %>%
  summarise(Purchase_Amount = sum(Purchase_Amount)) %>%
  arrange(desc(Purchase_Amount))

totalPurchases_Occupation$Occupation = as.character(totalPurchases_Occupation$Occupation)
typeof(totalPurchases_Occupation$Occupation)

occupation = ggplot(data = totalPurchases_Occupation) +
  geom_bar(mapping = aes(x = reorder(Occupation, -Purchase_Amount), y = Purchase_Amount, fill = Occupation), stat = 'identity') +
  scale_x_discrete(name="Occupation", breaks = seq(0,20, by = 1), expand = c(0,0)) +
  scale_y_continuous(name="Purchase Amount ($)", expand = c(0,0), limits = c(0, 300000000)) +
  theme(panel.background = element_rect(fill = "gainsboro", colour = "white", size = 0.5, linetype = "solid"), 
        plot.background = element_rect(fill = "gainsboro"), 
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "red"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "red"), 
        plot.title = element_text(hjust = 0, face = 'bold',color = 'black'), 
        plot.subtitle = element_text(face = "italic")) + 
  labs(x = "Occupation", y = "Purchase Amount", title = "Black Friday", 
       subtitle = "Distribution of total purchasing by occupation")
```

#Data Clustering
```{r, results="hide", fig.show='hide'}

blackfridayclustering <-  blackfriday %>%
                          select(Purchase)
```
##Determine the number of Clusters
```{r, results="hide", fig.show='hide'}
tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x = blackfridayclustering, centers = k)
  model$tot.withinss
})
```
##Generating a data frame containing both k and tot_withinss
```{r, results="hide", fig.show='hide'}
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)
```
##Plotting the elbow plot
```{r, results="hide", fig.show='hide'}
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)
```

##Cluster Model
```{r, results="hide", fig.show='hide'}
model_km3 <- kmeans(blackfridayclustering, centers = 3)
clust_km3 <- model_km3$cluster
blackfriday_clust <- mutate(blackfriday, cluster = clust_km3)
blackfriday_clust_node <- blackfriday_clust %>%
                          group_by(cluster) %>%
                          summarise(min_purchase = min(Purchase),
                                   max_purchase = max(Purchase),
                                   avg_purchase = round(mean(Purchase),0))
```
##Determining how many people in each cluster
```{r, results="hide", fig.show='hide'}
blackfriday_clust %>%
group_by(City_Category, cluster) %>%
summarise(n = n()) %>%
ggplot(aes(x=City_Category, y = n)) +
geom_col(aes(fill = as.factor(cluster))) +
theme_linedraw() + 
theme(legend.box.background	= element_rect(colour = "black"),
      legend.background = element_rect(fill = "gainsboro"),
      panel.background = element_rect(fill = "gainsboro", colour = "white", size = 0.5, linetype = "solid"), 
      plot.background = element_rect(fill = "gainsboro"), 
      panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "red"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "red"), 
      plot.title = element_text(hjust = 0, face = 'bold',color = 'black'),
      plot.subtitle = element_text(face = "italic")) + 
labs(x = 'City Category', y = 'Total Purchase (dollars)', title = "Black Friday",
     subtitle = "Total people in each cluster by city") + 
guides(fill=guide_legend(title = "Cluster")) + 
scale_y_continuous(labels = scales::comma) 
```
#Linear Regression
```{r, results="hide", fig.show='hide'}
blackfriday <- blackfriday[,-c(1,2)]
blackfriday <- as.data.frame(unclass(blackfriday))
str(blackfriday)
```
##Splitting the data
```{r, results="hide", fig.show='hide'}
library(caTools)
set.seed(123)
split=sample.split(blackfriday$Purchase, SplitRatio = 0.6)
training_set = subset(blackfriday, split == TRUE)
test_set = subset(blackfriday, split == FALSE)
```
##Designing the model
```{r, results="hide", fig.show='hide'}
regressor = lm(formula = Purchase ~ ., data = training_set)
summary(regressor)

y_pred = predict(regressor, test_set)
prediction_lm = data.frame(cbind(actual = test_set$Purchase, predicted = y_pred))
correlation_accuracy_lm = cor(prediction_lm)
n = sum(correlation_accuracy_lm)
diag = diag(correlation_accuracy_lm)
accuracy_lm = sum(diag)/n
accuracy_lm
```
#Random Forest
```{r, results='hide', fig.show='hide'}
library(randomForest)
regressor_rf<-randomForest(x = training_set[,-10], y = training_set$Purchase, ntree = 100)

y_pred_rf = predict(regressor_rf, test_set)
prediction_rf = data.frame(cbind(actual = test_set$Purchase, predicted = y_pred_rf))
correlation_accuracy_rf = cor(prediction_rf)
n = sum(correlation_accuracy_rf)
diag = diag(correlation_accuracy_rf)
accuracy_rf = sum(diag)/n
accuracy_rf
```