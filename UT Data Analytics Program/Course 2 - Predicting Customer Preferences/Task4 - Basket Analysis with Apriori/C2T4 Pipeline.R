# Title: C2T4 Pipeline

# Last update: 2020.09

# File/project name: C2T4 Pipeline.R
# RStudio Project name: Task4R.proj

###############
# Project Notes
###############

# Blackwell Electronics' board of directors is considering acquiring 
# Electronidex, a start-up electronics online retailer. The board of 
# directors has asked us to help them better understand the clientele 
# that Electronidex currently is serving and if it would be an optimal 
# partnership.
# 
# They need our help to identify purchasing patterns that will provide 
# insight into Electronidex's clientele. Attached is a CSV file that 
# contains one month’s (30 days’ worth) of Electronidexes online 
# transactions and a file containing all the electronics that they 
# currently sell. Due to their lack of funding, Electronidex is only 
# able to pull data on the items that customers purchased per their 
# transactions.
# 
# I would like you to to use R to conduct a market basket analysis. 
# You will be discovering any interesting relationships (or associations) 
# between customer’s transactions and the item(s) they’ve purchased. 
# These associations can then be used to drive sales-oriented initiatives 
# such as recommender systems like the ones used by Amazon and other 
# eCommerce sites. 
# 
# To help Blackwell’s board of directors form a clearer picture of 
# Electronidex's customer buying patterns, please consider the following
# questions while you’re performing your analysis:
# 
# Are there any interesting patterns or item relationships within 
# Electronidex's transactions?
#   
# Would Blackwell benefit from selling any of Electronidex's items?
# 
# In your opinion, should Blackwell acquire Electronidex?
# 
# If Blackwell does acquire Electronidex, do you have any recommendations for Blackwell? (Ex: cross-selling items, sale promotions, should they remove items, etc.)

# Comment multiple lines

# WIN: CMD + SHIFT + C


###############
# Housekeeping
###############

# Clear objects if necessary
rm(list = ls())

# get working directory
wd <- getwd()
# set working directory
setwd(wd)
dir()

# set a value for seed (to be used in the set.seed function)
seed <- 123


################
# Load packages
################

if(!require(arules)){
  install.packages("arules")
  library(arules)
}


if(!require(arulesViz)){
  install.packages("arulesViz", dependencies = TRUE)
  library(arulesViz)
}

if(!require(readr)){
  install.packages("readr", dependencies = TRUE)
  library(readr)
}

#####################
# Parallel processing
#####################

#--- for WIN ---#

if(!require(doParallel)){
  install.packages("doParallel")
  library(doParellel)
}
detectCores()  # detect number of cores
cl <- makeCluster(2)  # select number of cores; 2 in this example
registerDoParallel(cl) # register cluster
getDoParWorkers()  # confirm number of cores being used by RStudio
# Stop Cluster. After performing your tasks, make sure to stop your cluster. 
#stopCluster(cl)



###############
# Import data
##############

#### --- Load raw datasets --- ############################

# --- Load Transaction Data requiring read.transactions to create transactions object ############

oobTrans <- read.transactions(file = "ElectronidexTransactions2017.csv", 
                  format = c("basket"), 
                  header = FALSE, sep = ",", 
                  cols = NULL, rm.duplicates = TRUE, 
                  quote = "\"'", skip = 0, 
                  encoding = "unknown")

# results of removing duplicates
# distribution of transactions with duplicates:
#   items
# 1   2 
# 191  10 

class(oobTrans)  # "transactions"

################
# Evaluate data
################

inspect(head(oobTrans, 5)) # You can view the transactions. Is there a way to see a certain # of transactions?
inspect(oobTrans[4:5])     #can see a certain set of transactions by using matrix/array methods
length (oobTrans) # Number of transactions.
size (oobTrans) # Number of items per transaction
LIST(oobTrans) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(oobTrans)# To see the item labels
summary(oobTrans)

######### plot data  #######################
graphics.off()
par("mar")
par("cin")
par("lheight")
par(mar=c(1,1,1,1))
#plot items with relative frequency (support) >= .10 (10%)

itemFrequencyPlot(oobTrans, type = c("absolute"),
                  weighted = FALSE, support = .10, topN = 10,
                  lift = FALSE, horiz = TRUE, 
                  names = TRUE, xlab = "number of occurences",
                  mai = c(1,3,.25,1))


image(sample(oobTrans, 250)) #plot a sample set of 250 transactions and thier items

image(head(oobTrans, 100)) #plot the first 100 transactions and thier items

itemInfo(oobTrans)

##############
# Run Apriori algorithm on transaction data
##############

RulesName<- apriori (oobTrans, parameter = list(supp = 0.015, conf = 0.1, minlen = 2))

#################
# Evaluate Rules results
#################

inspect(RulesName)

summary(RulesName)

inspect(head(sort(RulesName, by = "confidence")))
inspect(head(sort((RulesName), by = "support")))

ItemRules <- subset(RulesName, items %in% "Keyboard")
ItemRules
inspect(sort(ItemRules, by = "confidence"))

is.redundant(RulesName)
is.redundant(ItemRules)
plot(RulesName, method="graph")
plot(RulesName, method="two-key plot", measure = "confidence")

##--- Conclusion ---##




