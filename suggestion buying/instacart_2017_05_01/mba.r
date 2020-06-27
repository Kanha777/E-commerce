rm(list=ls(all=TRUE))

#load libraries#
library(tidyverse)
library(data.table)
library(dplyr)
library(arules)
library(arulesViz)
library(plotly)

#set directory##
setwd("C:/Users/User/Desktop/data science project/ecommerce/suggestion buying/instacart_2017_05_01")

#load file#
order=read.csv(file="order_products__prior.csv",header=T,sep=",")
product=read.csv(file="products.csv",header=T,sep=",")


#join file#
order=order[1:1000,]
basket_data = left_join(order, product, by='product_id')

#avoid insginficant coloumn##
basket_data$product_id=NULL
basket_data$add_to_cart_order=NULL
basket_data$reordered=NULL
basket_data$aisle_id=NULL
basket_data$department_id=NULL


#create main data frame and transactions#
basket_data = group_by(basket_data, order_id)
basket_data = summarise(basket_data,itens=as.vector(list(product_name)))
head(basket_data)


transactions=as(basket_data$itens, 'transactions')
head(transactions)





##model_buildig#


myrules=apriori(transactions, list(support=0.000003, confidence=0.2, maxlen= 5))
inspect(myrules)

#plotting model##
plot(myrules)