setwd("/Users/chriswickham/Desktop/R Working Directory")
getwd()

# Research question: What information can we glean from data representing individual 
# contributions to the 2016 presidential race? 

# Question 1 
# ==========

# The following lines will load the campaign data and link it to candidate names, 
# using the link file. Note that my computer had problems loading camp.Rdata, so I downloaded
# and will be using the itcon.txt file directly from the FEC website. 

#Call the appropriate libraries and load the data

options(stringsAsFactors = FALSE)
library(dplyr)
library(e1071)
library(rpart)
library(rpart.plot)
library(ranger)
library(caret)
library(Metrics)
library(magrittr)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(ggplot2)
library(stringr)
library(bit64)
library(readr)
library(data.table)
library(sqldf)

camp <- fread('itcont.txt', sep = '|', header = F, na.strings = '')

# Note: I got a warning message, about bumping one entry's type to character type. 
# That warning message won't alter the data for our purposes, so ignore it if you get it
# as well, 

# Installing the header file, so we know the variable names

names(camp) <- names(read.csv('indiv_header_file.csv'))

# The original file contains 20,347,312 observations and 21 variables. 

# I'm only interested the following six categories, and have an interest in subsetting 
# at this stage, since it will make subsequent operations easier: 

# CMTE_ID - to facilitate the link
# ENTITY_TP - to be able to focus on individuals 
# TRANSACTION_DT - to the date of contributions 
# STATE - the state in which those contributions were made
# TRANSACTION_AMT - the amount of contributions 
# TRANSACTION_TP - the type of contributions, eg loans, contributions to a political committee, etc

# This means dropping the following categories, which were either redundant or irrelevant 
# to our research question (see data dictionary http://classic.fec.gov/finance/disclosure/metadata/DataDictionaryContributionsbyIndividuals.shtml
# for definitions): AMNDT_IND, RPT_TP, TRANSACTION_PGI, IMAGE_NUM, NAME, CITY, MEMO_CD, 
# ZIP_CODE, EMPLOYER, OCCUPATION, OTHER_ID, TRAN_ID, FILE_NUM, MEMO_TEXT, SUB_ID

camp %>%
  select(CMTE_ID, TRANSACTION_TP, ENTITY_TP, 
         TRANSACTION_DT, TRANSACTION_AMT, STATE) -> camp_red

#The file now has 20,347,213 observations, but only 6 variables. 

# We want to only focus on contributions from individuals, which corresponds to TRANSACTION_TP = 15. 
# So we filter by that transaction type, then drop the variable. 

camp_red %>%
  filter(TRANSACTION_TP == '15') %>%
  select(-TRANSACTION_TP) -> camp_ind

# The file now has 12,934,687 observations and 5 variables. 

# To load the other files, I'll be relying on the following two functions:

# This function adds names to data frames. It takes a data frame and a .csv file with header names as an argument, and 
# returns the data frame with the header names as its variable names. 

add_names <- function(df, file) {
  names(df) <- names(read.csv(file))
  df
}

# This function reads in files. It takes a .csv file as an argument and reads it into a dataframe
# under the following specifications:

read_fec <- function(file) {
  read.csv(file, sep = '|', skipNul = T, na.strings = '',
           header = F, quote = "")
}

#Reading in the candidate data. I'm interested in selecting the following four categories: 

# CAND_ID - a 9-digit code identifying each candidate
# CAND_NAME - canddiate name
# CAND_ELECTION_YR - their election year from the Statement of Candidacy
# CAND_OFFICE - the office for where they're running 

#This means dropping the following categories, which were either irrelevant or redudant 
# to our research question (see data dictionary, http://classic.fec.gov/finance/disclosure/metadata/DataDictionaryCandidateMaster.shtml,
# for definitions): CAND_ELECTION_YR, CAND_OFFICE_ST, CAND_OFFICE_DISTRICT, CAND_ICI, 
# CAND_STATUS, CAND_PCC, CAND_ST1, CAND_ST2, CAND_CITY, CAND_ST, CAND_ZIP

# Since I'm only interested in presidential races, I also filtered CAND_OFFICE by "P", 
# which indicates a presidential race, then dropped the variable. 

'cn.txt' %>%
  read_fec %>%
  add_names('cn_header_file.csv') %>%
  select(CAND_ID, CAND_NAME, CAND_ELECTION_YR, CAND_OFFICE) %>%
  filter(CAND_OFFICE == 'P') %>%
  select(-CAND_OFFICE) -> cand

# This file has 2300 obsevations, for 2300 candidates, and the three variables. 

# Load the link data, using the two functions above. I'm interested in selecting only
# those fields which will facilitate the link: 

# CAND_ID - the unique identifier for each candidate
# CAND_ELECTION_YR - their election year according to the statement of candidacy
# CMTE_ID - the unique code assigned to each committee

# This means dropping the following variables (see data dictionary http://classic.fec.gov/finance/disclosure/metadata/DataDictionaryCandCmteLinkage.shtm
# for definitions): FEC_ELECTION_YR, CMTE_TP, DMTE_DSGN, LINKAGE_ID

'ccl.txt' %>%
  read_fec %>%
  add_names('ccl_header_file.csv') %>%
  select(CAND_ID, CAND_ELECTION_YR, CMTE_ID) -> link

# To join the candidate information and contribution information, first joining 
# the candidate file to the link file by CAND_ID and CAND_ELECTION_YR: 

cand %>%
  inner_join(link, by = c("CAND_ID", "CAND_ELECTION_YR")) -> joined_data

# Then joining that file to camp_ind, by CMTE_ID:

joined_data %>%
  inner_join(camp_ind, by = "CMTE_ID") -> joined_data

# This file has 3,958,012 observations and 8 variables. 

# Now that we have the joined data, I'm going to continue winnowing down the data
# to focus on our specific research question, namely the 2016 presidential election. 

# The following step filters data to only include data from the 2016 election year, 
# using the variable CAND_ELECTION_YR, then drops the variable. 

# Finally, it selects only those fields that we are interested in (and defined above), 
# namely CAND_NAME, TRANSACTION_AMT, TRANSACTION_DT, and STATE. This means dropping the joining
# variables -- CMTE_ID and CAND_ID --  and dropping ENTITY_TP, which we aren't interested in. We'll
# also be eliminating negative campaign donations, since these are likely typos. 

joined_data %>%
  filter(CAND_ELECTION_YR == 2016) %>%
  filter(TRANSACTION_AMT > 0) %>%
  select(CAND_NAME, TRANSACTION_AMT, TRANSACTION_DT, STATE) -> wd_16

# The data now has 3,934,905 observations and 4 variables, so two rows were dropped for not
# pertaining to the 2016 election year. 

# The following step renames the variables to make them more readable and intuitive:

wd_16 %>%
  rename (name = CAND_NAME, 
          amount = TRANSACTION_AMT,
          date = TRANSACTION_DT,
          state = STATE) -> wd_ready

# Finally, the following three steps to make sure there are no rows that are missing either
# a date, candidate, or amount

wd_ready %>%
  filter(is.na(name))

wd_ready %>%
  filter(is.na(amount))

wd_ready %>%
  filter(is.na(date))

# Each of these filters found nothing (any null-valued rows were likely eliminated
# during the earlier filtering steps), so our working data is ready! 

# And finally our working data is ready! It has 3,934,905 observations and 4 variables

# Question 2 
# ==========

# The following steps create a data frame "popular" that aggregates data by candidate and 
# includes columns for the total number of donations made to acandidate and the total 
# amount awwarded to each candidate. 

# First, we create a data frome "tot_by_can", which is the aggregate of the amount field 
# from our working dataframe "wd_ready" by name. Then we rename the fields to reflect their
# and join to a new dataframe, tot_by_can_rn. 

tot_by_can <- aggregate(wd_ready$amount, by=list(wd_ready$name), FUN=sum)

tot_by_can %>%
  rename(name = Group.1,
         total_amount = x) -> tot_by_can_rn

unique(tot_by_can_rn$name)

# Next, we create a data frame, "wd_sum", which indicates the total number of donations 
# received by each candidate. 

wd_ready %>%
  group_by(name) %>%
  summarize(number_donations = n()) -> wd_sum

# Finally, we inner join the two dataframes together, creating the dataframe popular, 
# which aggregates data by candidate and includes columns for the total number of donations 
# made to candidate (number donations) and the total amount donated to each candidate
# (total_amount_contributed). 

tot_by_can_rn %>%
  inner_join(wd_sum, by="name") -> popular

#"popular" contains 49 observations, representing 49 distinct candidates, and 3 variables, 
# for name, total amount received, and total number of donations. 

# Question 3 
# ==========

# The following steps create data frames "weekly_d" and "weekly_r" that aggregate the total 
# amount contributed to each candidate each week (with Democratic candidates in weekly_d
# and Republican candidates being listed in weekly_r). They will only include the candidates 
# who receive the top 9 most in terms of dollars, with all other candidates aggreaged into 
# candidate "other". Finally, they will plot each of these weekly contributions over the course
# of the dates presented in the data frame. 

# First, we need to develop a partisan idenitifier. We know from the size of the popular data 
# frame that our data currently contains 34 candidates. However, the only data set that contains
# a partisan identifier field -- 'primary_results.csv' -- only accounts for 14 candidates (see 
# the candidates listed below). 

# Note: I hard-code names in during this question, and at few other points in the code, 
# since, the the differences in candidate names in primary_results and the wd_ready 
# are irregular. I know this exposes the program to typo errors, so I've minimized these
# with table statements and other tests after each hardcoding section to show that 
# I haven't lost data. 

primary_results <- fread('primary_results.csv', sep = ',', header = T, na.strings = '')

# Thus, we will have to separate our candidates into Republicans and Democrats by 'hard-coding', 
# using outside information. 

# The following list shows the 49 candidates that we will have to account for: 

unique(wd_ready$name)

# In order to classify candidates, I will be using information from the FEC website, https://www.fec.gov/. 

# The following 21 candidates are neither Democrat nor Republican, representing a number of 
# minor parties, independents, or a surprising number of football players. 
# They can be dropped from the dataset: 

# Andrew Daniel Basiago 
# Liza Dawn Cherricks
# Dale Christensen
# Scott Cubbler 
# Colin Doyle 
# Larry Duncan
# Jeff George
# Gary Johnson
# Elijah Manley
# John David McAffee
# Kent Philip Mesplay
# Joseph Schriner
# Jill Stein
# Judd Weiss
# Robert Carr Wells
# Bob Whitaker
# William Kreml
# Joseph Maldonado
# Austin Wade Petersen
# Peter Alan Skewes
# Willie Wilson

# The following 21 candidates are Republicans

# Jeb Bush 
# Ben Carson
# Chris Christie 
# Ted Cruz 
# Rocky de la Fuente
# Carly Fiorina
# Jame S. Gilmore III
# Mike Huckabee
# Evan McMullin
# George Pataki
# Rand Paul
# Rick Perry
# Marco Rubio
# Rick Santorum
# Scott Walker
# Mark Everson
# Lindsey Graham
# Bobby Jindal
# John Kasich
# Jefferson Woodson Sherman
# Donald J. Trump

# The following step pulls Republicans from the working data set, into the dataframe wd_r:

wd_ready %>%
  filter(name == "BUSH, JEB" 
         | name == "CARSON, BENJAMIN S SR MD" 
         | name == "CHRISTIE, CHRISTOPHER J"
         | name == 'CRUZ, RAFAEL EDWARD "TED"' 
         | name == "DE  LA  FUENTE, ROQUE ROCKY"
         | name == "FIORINA, CARLY" 
         | name == "GILMORE, JAMES S III" 
         | name == "HUCKABEE, MIKE"
         | name == "MCMULLIN, EVAN / MINDY FINN" 
         | name == "PATAKI, GEORGE E" 
         | name == "PAUL, RAND" 
         | name == "PERRY, JAMES R (RICK)" 
         | name == "RUBIO, MARCO" 
         | name == "SANTORUM, RICHARD J." 
         | name == "WALKER, SCOTT"
         | name == "EVERSON, MARK" 
         | name == "GRAHAM, LINDSEY O" 
         | name == "JINDAL, BOBBY"
         | name == "KASICH, JOHN R" 
         | name == "SHERMAN, JEFFERSON WOODSON"
         | name == "TRUMP, DONALD J. / MICHAEL R. PENCE ") -> wd_r

#To make sure there were no typos or observations lost, checked the table(wd_r) and found that
# there were, in fact, 21 Republican candidates, all with the same number of contributions as 
# in table(wd_ready$name)

table(wd_ready$name)

unique(wd_r$name)
table(wd_r$name)

# wd_r has 980,335 observations and 4 variables. 

# The following 7 candidates are Democrats: 

# Hillary Clinton 
# Martin O'Malley
# Bernie Sanders
# Lincoln Chafee
# Wanda Gayle Duckwald
# Lawrence Lessig
# James Webb


# The following step pulls Democrats from the working data set, into the dataframe wd_d: 

wd_ready %>% 
  filter (  name == "CLINTON, HILLARY RODHAM / TIMOTHY MICHAEL KAINE"
          | name == "O'MALLEY, MARTIN JOSEPH"
          | name == "SANDERS, BERNARD"
          | name == "CHAFEE, LINCOLN DAVENPORT MR."
          | name == "DUCKWALD, WANDA GAYLE"
          | name == "LESSIG, LAWRENCE" 
          | name == "WEBB, JAMES") -> wd_d

# As with above, check to make sure that there are seven candidates, all of whom have the same 
# number of contributions as in wd_ready, to ensure that there was no data lost. 

unique(wd_d$name)
table(wd_d$name)
table(wd_ready$name)

#wd_d has 2,952,395 observations and 4 variables. It may seem odd that wd_d has fewer candidates
# but more total donations -- this is mainly explained by the massive amount of donations 
# to the Hillary Clinton campaign (2,448,957, more than twice the number of contributions 
# to the entire Republican field). 

# Now we have our Republican and Democrat data sets. 

# First, we create a new column, newdate, that expresses 

wd_r %>%
  mutate(nicedate = as.Date(mdy(date))) -> wd_r_nd

wd_d %>%
  mutate(nicedate = as.Date(mdy(date))) -> wd_d_nd

# Next, we create a new variable "week" that aggregates data by week, eg 2015-1 
# represents the first week of 2015. 

wd_r_nd$week <- format(wd_r_nd$nicedate, format = "%Y-%U")

wd_d_nd$week <- format(wd_d_nd$nicedate, format = "%Y-%U")

head(wd_r_nd)
head(wd_d_nd)

#Democrats have fewer than 9 candidates, so we don't have to pull out the top 9. We can just
# aggregate by name and date to get to weekly_d. 

weekly_d <- aggregate(wd_d_nd$amount, by=list(wd_d_nd$name, wd_d_nd$week), FUN=sum)

weekly_d%>%
  rename(name = Group.1,
         week = Group.2, 
         amount = x) -> weekly_d

# For weekly_r, however, we want to focus on the top 9 candidates, and aggregate the others into a category
# "other candidates, so we must first aggregate data by candidate, then sort by 
# total amount. 

wd_r_agg <- aggregate(wd_r_nd$amount, by = list(wd_r_nd$name), FUN= sum)

wd_r_agg %>%
  arrange(desc(x)) -> wd_r_ranked

wd_r_ranked %>%
  rename (name = Group.1, 
          amount = x) -> wd_r_ranked

# Once we print wd_r_ranked, we can see the top 9 candidates The amount raised 
# by the 9th candidate, Scott Walker, is 4,859,189 so we can filter out all of the candidates
# who raised more than than and the group them as the "other" candidates, and group the rest
# as the top 9 candidates. 

wd_r_ranked %>%
  filter(!amount >= 4859189) -> wd_r_other 

wd_r_ranked %>%
  filter(amount >= 4859189) -> wd_r_top_9

print(wd_r_other)

# Finally, we can return to the weekly data, and aggregate all of the candidates whose 
# names are in the "other category" by week. 

wd_r_nd %>%
  filter(name == "PAUL, RAND"
         | name == "GRAHAM, LINDSEY O"
         | name == "HUCKABEE, MIKE"
         | name == "JINDAL, BOBBY"
         | name == "PERRY, JAMES R (RICK)"
         | name == "SANTORUM, RICHARD J."
         | name == "MCMULLIN, EVAN / MINDY FINN"
         | name == "PATAKI, GEORGE E"
         | name == "GILMORE, JAMES S III"
         | name == "EVERSON, MARK"
         | name == "DE  LA  FUENTE, ROQUE ROCKY"
         | name == "SHERMAN, JEFFERSON WOODSON") -> wd_r_other_weekly

wd_r_nd %>%
  filter(!name == "PAUL, RAND"
         & !name == "GRAHAM, LINDSEY O"
         & !name == "HUCKABEE, MIKE"
         & !name == "JINDAL, BOBBY"
         & !name == "PERRY, JAMES R (RICK)"
         & !name == "SANTORUM, RICHARD J."
         & !name == "MCMULLIN, EVAN / MINDY FINN"
         & !name == "PATAKI, GEORGE E"
         & !name == "GILMORE, JAMES S III"
         & !name == "EVERSON, MARK"
         & !name == "DE  LA  FUENTE, ROQUE ROCKY"
         & !name == "SHERMAN, JEFFERSON WOODSON") -> wd_r_top9_weekly

# Called up unique names, just to make sure I hadn't dropped any
           
unique(wd_r_other_weekly$name)
unique(wd_r_top9_weekly$name)

# Finally, aggregate the data by week, rename all candidates in the "other" dataframe
# "other", then inner join with the top 9 candidates. 

wd_r_other_aggregated <- aggregate(wd_r_other_weekly$amount, by = list(wd_r_other_weekly$week), FUN=sum)

wd_r_other_aggregated %>%
  rename (amount = x,
          week = Group.1) -> wd_r_other_aggregated

wd_r_other_aggregated$name <- "OTHER"

# Eliminating unnecessary variables fromw wd_r_top9_weekly to facilite a vertical merge: 

wd_r_top9_weekly %>%
  select(name, amount, week) -> wd_r_top9_pared

# Merging the top 9 data set by week

wd_r_top9_premerge <- aggregate(wd_r_top9_pared$amount, by = list(wd_r_top9_pared$week, wd_r_top9_pared$name), FUN = sum)

wd_r_top9_premerge %>%
  rename (week = Group.1,
          name = Group.2,
          amount =x) -> wd_r_top9_premerge

# Finally, merging the two data sets by week

weekly_r <- rbind(wd_r_other_aggregated, wd_r_top9_premerge)

#So we have a dataframe "weekly_r" that aggregates Republican candidates by week and amount contributed! 

#Finally, we plot both weekly_d and weekly_r:

min(weekly_d$week)
max(weekly_d$week)

#Since weeks are a discrete variable, I list the time range in the title and 
# just specify 'Time' as the x-axis

ggplot(weekly_d, aes(x=week, y=amount, color=name)) + geom_density(position = "stack") + ggtitle("Weekly Contributions to Democratic 2016 \n Presidential Candidates (1/2014 - 11/2016)") +
  xlab("Time") + ylab("Amount, Dollars") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

min(weekly_r$week)
max(weekly_r$week)

ggplot(weekly_r, aes(x=week, y=amount, color = name)) + 
  geom_density(position = "stack") + ggtitle("Weekly Contributions of Republican 2016 \n Presidential Candidates (11/2014 - 12/2016)") + xlab("Time") + ylab("Amount, Dollars") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) 

# Question 4
# ==========

# The following steps will create a dataframe, called "final_primary", that includes primary votes, primary vote percentages, 
# contributions, and contribution percentages, all aggregated by candidte and state. 

# First import the primary data. 

primary_results <- fread('primary_results.csv', sep = ',', header = T, na.strings = '')

# Then aggregate the primary results by state (rather than county), and rename the results
# to match our other dataframes.

primary_results_by_state <- aggregate(primary_results$votes, by = list(primary_results$candidate, primary_results$state_abbreviation), FUN = sum)

primary_results_by_state %>%
  rename(name = Group.1,
         state = Group.2,
         votes = x) -> primary_results_by_state

# FYI, as the code below demonstrates, there are only 49 distinct states in this data set, so when we see 49 states later, 
# it won't be an indicator that we've lost data.

length(unique(primary_results_by_state$state))

# Next, we take wd_ready, the working data set, and aggregate it by name and state as well

wd_amount_state <- aggregate(wd_ready$amount, by = list(wd_ready$name, wd_ready$state), FUN=sum)

wd_amount_state %>%
  rename (name = Group.1, 
          state = Group.2, 
          amount= x) -> wd_amount_state

#In order to prepare to join, we're going to have to change several names. See the table 
# below for the names in the primary data set. 

table(primary_results_by_state$name)

#Since these names are easier to read, I'll replace the names in wd_amount_state with those in 
# the primary results: 

wd_st_rn <- wd_amount_state

wd_st_rn[wd_st_rn == "CARSON, BENJAMIN S SR MD"] <- "Ben Carson"
wd_st_rn[wd_st_rn == "SANDERS, BERNARD"] <- "Bernie Sanders"
wd_st_rn[wd_st_rn == "FIORINA, CARLY"] <- "Carly Fiorina"
wd_st_rn[wd_st_rn == "CHRISTIE, CHRISTOPHER J"] <- "Chris Christie"
wd_st_rn[wd_st_rn == "TRUMP, DONALD J. / MICHAEL R. PENCE "] <- "Donald Trump"
wd_st_rn[wd_st_rn == "CLINTON, HILLARY RODHAM / TIMOTHY MICHAEL KAINE"] <- "Hillary Clinton"
wd_st_rn[wd_st_rn == "BUSH, JEB"] <- "Jeb Bush"
wd_st_rn[wd_st_rn == "KASICH, JOHN R"] <- "John Kasich"
wd_st_rn[wd_st_rn == "RUBIO, MARCO"] <- "Marco Rubio"
wd_st_rn[wd_st_rn == "O'MALLEY, MARTIN JOSEPH"] <- "Martin O'Malley"
wd_st_rn[wd_st_rn == "HUCKABEE, MIKE"] <- "Mike Huckabee"
wd_st_rn[wd_st_rn == "PAUL, RAND"] <- "Rand Paul"
wd_st_rn[wd_st_rn == "SANTORUM, RICHARD J."] <- "Rick Santorum"
wd_st_rn[wd_st_rn == 'CRUZ, RAFAEL EDWARD "TED"'] <- "Ted Cruz"

# Now we inner join on name. You can verify that all 14 candidate names have been preserved by looking at table(prim_vot_cont$name) 
# and verify that all states have been preserved by looking at length(unique(prim_vot_cont$state)). 

primary_results_by_state %>%
  inner_join(wd_st_rn, by=c("name", "state")) -> prim_vot_cont

table(prim_vot_cont$name)
length(unique(prim_vot_cont$state))

# First, find the totals for the Democratic candidates in the primary data, namely
# Martin O'Malley, Hillary Clinton, and Bernie Sanders. The state vote totals for Democrats will be stored in 
# the dataframe state_vot_tot_d, and the state contribution totals for Democrats will be stored in state_cont_tot_d. 

prim_vot_cont %>%
  filter(name == "Martin O'Malley" | name == "Hillary Clinton" | name == "Bernie Sanders") -> prim_vot_cont_d

state_vot_tot_d <- aggregate(prim_vot_cont_d$votes, by=list(prim_vot_cont_d$state), FUN = sum)
state_cont_tot_d <- aggregate(prim_vot_cont_d$amount, by=list(prim_vot_cont_d$state), FUN = sum)

state_vot_tot_d %>%
  rename(state = Group.1,
         democratic_votes = x) -> state_vot_tot_d

state_cont_tot_d %>%
  rename(state = Group.1, 
         democratic_contributions = x) -> state_cont_tot_d

# Now we do the same for Republican candidates (and to save time, I just select by NOT the democrats, and then verified 
# that the 11 candidates are present by looking at the table). Republican state vote totals are stored in state_vot_tot_r, 
# and Republican state contribution totals are stored in state_cont_tot_r. 

prim_vot_cont %>%
  filter(!name == "Martin O'Malley" & !name == "Hillary Clinton" & !name == "Bernie Sanders") -> prim_vot_cont_r

table(prim_vot_cont_r$name)

state_vot_tot_r <- aggregate(prim_vot_cont_r$votes, by=list(prim_vot_cont_r$state), FUN = sum)
state_cont_tot_r <- aggregate(prim_vot_cont_r$amount, by=list(prim_vot_cont_r$state), FUN = sum)

state_vot_tot_r %>%
  rename(state = Group.1,
         republican_votes = x) -> state_vot_tot_r

state_cont_tot_r %>%
  rename(state = Group.1, 
         republican_contributions = x) -> state_cont_tot_r

# Now join both of these back onto the primary votes and contributions dataframes, then calculate
# the percentage and votes for each candidate by dividing the number of state votes by 
# state party votes for each candidate, and percentage of contributions for each candidate by 
# candidate contributions per state by party contributions per state. These values are stored in 
# perc_vote and perc_cont, respectively. 

prim_vot_cont_d %>%
  inner_join(state_vot_tot_d, by = "state") %>%
  inner_join(state_cont_tot_d, by = "state") -> dem_prim

dem_prim$perc_vote <- dem_prim$votes / dem_prim$democratic_votes
dem_prim$perc_cont <- dem_prim$amount / dem_prim$democratic_contributions

prim_vot_cont_r %>%
  inner_join(state_vot_tot_r, by = "state") %>%
  inner_join(state_cont_tot_r, by = "state") -> rep_prim

rep_prim$perc_vote <- rep_prim$votes / rep_prim$republican_votes
rep_prim$perc_cont <- rep_prim$amount / rep_prim$republican_contributions

# Finally, we drop the columns that don't match, then merge them vertically and arrange by state to get the final 
# data set, final_primary. Check out the first few rows with the head(final_primary). 

dem_prim %>%
  select(-democratic_votes, -democratic_contributions) -> dem_premerge

rep_prim %>%
  select(-republican_votes, -republican_contributions) -> rep_premerge

final_primary_dataset <- rbind(dem_premerge, rep_premerge)

final_primary_dataset %>%
  arrange(state) -> final_primary

head(final_primary)

#Question 5 

# I'm interested in examining the effect of the following 5 variables on vote count:

# AGE135214 - percent of persons above 18 years  
# AGE295214 - percent of persons between 18 years
# SEX255214 - percent of female persons
# VET605213 - Veterans, 2009-2013
# INC10213 - Median Household Income

# First, load the county facts 

county_facts <- fread('county_facts.csv', sep = ',', header = T, na.strings = '')

# Then join these onto the primary results. 

primary_results %>%
  inner_join(county_facts, by="fips") -> wd_random_votes

# Finally, join the contribution on as well, and select those variables that I'm 
# interested in. I'm going to use perc_votes and perc_cont rather than votes and amount, 
# since the former categories account for differences in the size of the electorate and
# total amount contributed. 

final_primary %>%
  rename(candidate = name, state_abbreviation.x = state) -> final_primary_ready

wd_random_votes %>%
  inner_join(final_primary_ready, by = c("candidate", "state_abbreviation.x")) %>%
  select (candidate, state_abbreviation.x, perc_vote, perc_cont, AGE135214, 
          AGE295214, SEX255214, VET605213, INC110213) %>%
  rename (state = state_abbreviation.x,
          above_18 = AGE135214,
          under_18 = AGE295214,
          perc_female = SEX255214,
          perc_vets = VET605213,
          med_inc = INC110213) -> wd_random

str(wd_random)

# First, I'm going to do decision trees including all of the variables, for both percentage
# of votes and percentage of contributions. 

percent_votes.model <- rpart(perc_vote~., data = wd_random)
rpart.plot(percent_votes.model)

summary(percent_votes.model)

# For the model to predict percentage votes, we find that the candidate's name, the state, 
# and the percentage of contributions they gain are the greatest indicators towards the 
# percentage votes. This shouldn't be suprising, since different candidates had different 
# levels of success (and note that all of the republicans plus Martin o'Malley are inlcuded
# the first fork, which directs us towards a lower branch of perc_vote-- this is reflective 
# of how crowded the Republican race was vs. the Democratic race), since states have significantly 
# different political environments, and since the percentage of contributions gained is a 
# decent proxy for the promise attributed to attention paid to a candidate. 

percent_cont.model <- rpart(perc_cont~., data = wd_random)
rpart.plot(percent_cont.model)

summary(percent_cont.model)

# For percent contributions, the best predictors were name and state, and no other 
# variables. This is likely reflective, again, of different levels of donor interest
# in different candidates, and in states' varying political environments. 

# It's possible that this model could be overfitting the data, since that remains the 
# main pitfall of decision trees. Thus, let's construct a random forest model to verify 
# significance, using the data with our five variables of interest.  

# First, construct a random forest model for the percentage of votes, perc_vote: 

test_idx  <- sample(length(wd_random$perc_vote),2000)
wd_test  <- wd_random[ test_idx,  ]
wd_train <- wd_random[-test_idx,  ]

forest_model <- train(wd_train,
                      wd_train$perc_vote,
                      method="ranger",
                      tuneGrid=data.frame(mtry=3,splitrule="variance"),
                      trControl=trainControl(method="none"))

forest_predict <- predict(forest_model, newdata = wd_test)

errors <- data.frame(rf = forest_predict, actual = wd_test$perc_vote)

ggplot(errors,aes(x=actual,perc_vote=log(abs(actual-rf))))+geom_point(aes(y=actual-rf))

# To see the summary of random forest model:

summary(forest_model)

# Finally, construct a random forest model for the percentage of contributions, 
# perc_cont: 

test_idx  <- sample(length(wd_random$perc_cont),2000)
wd_test  <- wd_random[ test_idx,  ]
wd_train <- wd_random[-test_idx,  ]

forest_model <- train(wd_train,
                      wd_train$perc_cont,
                      method="ranger",
                      tuneGrid=data.frame(mtry=3,splitrule="variance"),
                      trControl=trainControl(method="none"))

forest_predict <- predict(forest_model, newdata = wd_test)

errors <- data.frame(rf = forest_predict, actual = wd_test$perc_vote)

ggplot(errors,aes(x=actual,perc_cont=log(abs(actual-rf))))+geom_point(aes(y=actual-rf))

# To see the summary of random forest model: 

summary(forest_model)


