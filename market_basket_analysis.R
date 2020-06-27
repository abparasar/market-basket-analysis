
# set current working directory
setwd('C:/Users/HP/Desktop/edu/Market-Basket-Analysis-20200423T211140Z-001/Market-Basket-Analysis/data')

#-----------------------import the file---------------------
transactions_data = read.csv('Online_Retail.csv')
str(transactions_data)
View(transactions_data)

summary(transactions_data)
n_distinct(transactions_data$InvoiceNo)


#-------------------------- Data Preparation----------------------------

# sorting the data based on customerId
transactions_data_sorted <- transactions_data[order(transactions_data$CustomerID),]
View(transactions_data_sorted)

library(plyr)
library(dplyr)

# making an intermediate table based on customerId where rows contain the names of comma separated items bought by the customer 
itemList <- ddply(transactions_data_sorted,c("CustomerID"), function(df1)paste(df1$Description, collapse = ","))

View(itemList)

itemList$CustomerID <- NULL

itemList[3,]


# write the data in csv file 
write.csv(itemList,"market_basketdata.csv", quote = FALSE, row.names = FALSE)

library("arules")

#reads the file and cverts the data into sparce matrix
tran_data_modified <- read.transactions('market_basketdata.csv', format = 'basket', sep=',')

summary(tran_data_modified)
inspect(tran_data_modified[1:20])



# ----------------------------- Run the algo ----------------------

rules <- apriori(tran_data_modified, parameter = list(supp=0.01, conf=0.8,minlen=2,maxlen=4))

summary(rules)

inspect(rules[1:2])




#-------------------- get pattern -------------------
# sorting rules by a parameter- Confidence
rules <- sort(rules, by='confidence', decreasing = TRUE)
# Viewing the top 20 rules 
inspect(rules[1:20])


library(arulesViz)
plot(rules,jitter=0)

plot(rules,method="graph", engine ='interactive')

