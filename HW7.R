library(dplyr)
library(plyr)

load("camp.Rdata")

head(camp)

"//prod/userdata/FR_Data/WickhamJ/Desktop/R Working Directory/ccl_header_file.csv"

#Creating functions to add in data

add_names <- function(df, file) {
  names(df) <- names(read.csv(file))
  df
}

read_fec <- function(file) {
  read.csv(file, sep = '|', skipNul = T, na.strings = '',
           header = F, quote = "")
}

# Reading in the candidate data
#==============================

'//prod/userdata/FR_Data/WickhamJ/Desktop/R Working Directory/cn.txt' %>%
  read_fec %>%
  add_names('//prod/userdata/FR_Data/WickhamJ/Desktop/R Working Directory/cn_header_file.csv') -> cand

# Reading in the linking data
# ===========================

'//prod/userdata/FR_Data/WickhamJ/Desktop/R Working Directory/ccl.txt' %>%
  read_fec %>%
  add_names('//prod/userdata/FR_Data/WickhamJ/Desktop/R Working Directory/ccl_header_file.csv') %>%
  select(CAND_ID, CAND_ELECTION_YR, CMTE_ID) -> link

#Joining the candidate and linking data 
#======================================

cand %>%
  inner_join(link, by = c("CAND_ID", "CAND_ELECTION_YR")) -> working_data

#Creating the working data
#=========================

working_data %>%
  inner_join(camp, by = "CMTE_ID") -> working_data

#Looking for missing values
#==========================

working_data %>%
  filter(is.na(TRANSACTION_DT))

working_data %>%
  filter(is.na(TRANSACTION_AMT))

working_data %>%
  filter(is.na(CAND_NAME))

# Found two rows with missing transaction dates, so get rid of them
# We can see that the number of observations drops by two
#=================================================================

working_data %>%
  filter(!is.na(TRANSACTION_DT)) -> working_data_noNA
  
#2. Aggreagating data by candidate

table(working_data$CAND_ELECTION_YR)

library(dplyr)
library(plyr)
library(magrittr)

working_data_noNA %>%
  group_by(CAND_NAME) %>%
  dplyr::summarize(total_donations = n()) -> wd_sum

aggregate(working_data_noNA$TRANSACTION_AMT, by=list(working_data_noNA = working_data_noNA$CAND_NAME), FUN=sum) -> wd_don_amt

wd_don_amt %>%
  dplyr::rename (CAND_NAME = working_data_noNA,
          TOT_AMT = x) -> wd_don_amt

wd_don_amt %>%
  inner_join(wd_sum, by="CAND_NAME") -> popular

head(wd_findata)

#3. Creating data frames "weekly_d" and "weekly_r" that aggregate
#   the total amount contributed to each candidate each week, for 
#   democrats and republicans, respectively. 


library(dplyr)
library(lubridate)
library(zoo)
library(xts)


working_data_noNA %>%
  filter(!is.na(TRANSACTION_DT)) %>%
  mutate(date = as.Date(mdy(TRANSACTION_DT))) -> wb_better_dates

wb_better_dates %>%
  dplyr::group_by (date) %>%
  dplyr::summarise(n=n())

#I think this will end up being the solution

wd_better_dates$week <- format(wd_better_dates$date, format="%Y-%U")

by_weeks <- ddply(wd_better_dates, .(week), summarize, sum_don =sum(TRANSACTION_AMT))

#Okay, now just need to aggregate by candidate somehow, then you can choose top
# top candidates and figure out the plot 

by_candname_week <- aggregate(wd_better_dates$TRANSACTION_AMT, by=list(wd_better_dates = wd_better_dates$CAND_NAME, wd_better_dates$week), FUN=sum)

#Now just need to group by sumtotal and throw all of the others into an 
# "other candidates" category, then can plot

agg_cand <- aggregate(by_candname_week$x, by=list(by_candname_week$wd_better_dates), FUN = sum)

agg_cand %>%
  arrange(desc(x)) -> arranged_cand

arranged_cand %>%
  top_n(9,x) -> most_popular

most_popular %>%
  dplyr::rename(name = Group.1) -> most_popular

by_candname_week %>%
  dplyr::rename(name = wd_better_dates) -> by_candname_week

by_candname_week %>%
  semi_join(most_popular,by = 'name') -> popular_candidates_by_week

by_candname_week %>%
  anti_join(most_popular, by= 'name') -> other_candidates_by_week

other_agg_by_week <- aggregate(other_candidates_by_week$x, by = list(other_candidates_by_week$week), FUN= sum)


#So we also have to separate out into just president, Republican and Democrat, 
# etc, and combine the other_candidates 

# For now, though, I'll do a ggplot model using popular_candidates_by_week, 
# which is basically the same structure as I'll have with any data set

# We want x to be week, y to be contributions, and each separate line to be
# based on name 

library(ggplot2)

ggplot(data=popular_candidates_by_week, aes(x=Group.2, y=x, color=name)) + geom_point() 


# So it's working-- I think that the main issue will be subsetting the data in order
# to get a more manageable x-axis. the problem is that the week data is still discrete, so you're
# going to have to make it continuous, which will likely require converting the week data
# to integers, but we'll see
  
# So there's still going to be an issue with the x-axis -- move on for now
  
# 4 First, this creates the data set of funds aggregated by candidate and state

agg_by_cand_and_state <- aggregate(working_data_noNA$TRANSACTION_AMT, by = list(working_data_noNA$CAND_NAME, working_data_noNA$CAND_OFFICE_ST), FUN = sum)

# Then it renames the fields to be more useable

agg_by_cand_and_state %>%
  dplyr::rename(name = Group.1, state= Group.2) -> agg_by_cand_and_state

#Next it aggregates the data by state, so we can see state totals

agg_by_state <- aggregate(working_data_noNA$TRANSACTION_AMT, by = list(working_data_noNA$CAND_OFFICE_ST), FUN = sum)

agg_by_state %>%
  dplyr::rename(state_total=x, state = Group.1) -> agg_by_state

#It inner joins the data sets, so that fin_agg has the state_total data 

agg_by_cand_and_state %>%
  inner_join(agg_by_state, by=("state")) -> fin_agg

#It adds a new column, which is the percentage of funding raised in a state, 
# based on the calculation of the other two columns 

fin_agg$cand_perc = fin_agg$cand_total / fin_agg$state_total

# Finally, it joins this data with primary vote totals 

primary_results %>%
  dplyr::rename(name = candidate) -> primary_results


library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(ggplot2)
library(stringr)

fin_agg$name %>%
  str_split(", ") %>%
  map_chr(~paste(.[[2]], .[[1]])) -> fin_agg$name


primary_results_by_state <- aggregate(primary_results$votes, by=list(primary_results$state,primary_results$name), FUN=sum)

primary_results_by_state$name <- toupper(primary_results_by_state$Group.2)

fin_agg %>%
  inner_join(primary_results_by_state, by=("name")) -> wprim_res 

# May have to go in manually and change the names around, to make sure that they merge, 
# e.g. if (primary_results$name == "Donald Trump) {
#primary_results$name = "DONALD TRUMP"
#}
#}
  
#5 For Decision trees, your going to want to model off of the following code: 

The following is the section of the code you need to do the decision tree and
model: 

set.seed(123)

test_idx  <- sample(length(diamonds$price),2000)
test_set  <- diamonds[ test_idx,  ]
train_set <- diamonds[-test_idx,  ]

forest_model <- train(train_set[,-7],
                      train_set$price,
                      method="ranger",
                      tuneGrid=data.frame(mtry=3,splitrule="variance"),
                      trControl=trainControl(method="none"))

forest_model

tree_model <- train(train_set[,-7],
                    train_set$price,
                    method="rpart")

tree_model

forest_predict <- predict(forest_model, newdata = test_set[,-7])
tree_predict   <- predict(tree_model, newdata = test_set[,-7])

errors <- data.frame(rf = forest_predict,dt = tree_predict, actual = test_set$price)

ggplot(errors,aes(x=actual,color=log(abs(actual-rf))))+geom_point(aes(y=actual-rf))

ggplot(errors,aes(x=actual,color=log(abs(actual-dt))))+geom_point(aes(y=actual-dt))

mse(forest_predict,test_set$price)
rmse(forest_predict,test_set$price)

mse(tree_predict,test_set$price)
rmse(tree_predict,test_set$price)

