rm(list=ls())


# Load packages
library(rvest)
library(tidyverse)
library(utils)
library(survPen)


# Event_Links -------------------------------------------------------------

event_page <- 6
pb <- txtProgressBar(min = 0, max = event_page, style = 3)
event_links_table = data.frame()

for (i in 1:event_page) {

Sys.sleep(0.1)
    
fighter_page <- read_html(paste0("http://www.sherdog.com/organizations/Ultimate-Fighting-Championship-UFC-2/recent-events/",
                                 i))
event_links <- as.vector(fighter_page %>%
  html_nodes("td:nth-child(2) a") %>%
  html_attr("href"))

df <- data.frame(event_links)
event_links_table <- rbind(event_links_table,df)
setTxtProgressBar(pb, i)

}


Total_Events <- nrow(event_links_table)

Last_Fight <- which(grepl("/events/UFC-Fight-Night-181-Hall-vs-Silva-87364",event_links_table$event_links))

event_links_table <- event_links_table$event_links[Last_Fight:Total_Events]

event_links_table <- setdiff(event_links_table, event_links_table[100:111])

rm(df,event_links,event_page,pb)


# Event_Table -------------------------------------------------------------

### Variables definition

x <- 0
event_id <- 0
event_name <- 0
event_date <- 0
event_local <- 0
event_table = data.frame()
pb <- txtProgressBar(min = 0, max = length(event_links_table), style = 3)

for (i in event_links_table) {

Sys.sleep(0.1)

fighter_page <- read_html(paste0("http://www.sherdog.com",i))

fights_table <- fighter_page %>%
  # extract fight history
  html_nodes(".author span , .date , h1 span") %>%
  # not a well-behaved table so it is extracted as strings
  html_text()
  
# Counter and Fight ID
  x <- x + 1

  event_id[x] <- x    
  event_name[x] <- fights_table[2]  
  event_date[x] <- fights_table[3]
  event_local[x] <- fights_table[4]

  setTxtProgressBar(pb, x)
  
}  
    
  event_table <- tibble (event_id = event_id, 
                event_name = event_name,
                event_date = event_date,
                event_local = event_local)
  
  
  event_table

rm(event_id,event_date, event_name, event_local)

write.csv(event_table,"event_table.csv")

# Event_Main_Fight --------------------------------------------------------


###Variables

x <- 0
R_fighter <- 0
B_fighter <- 0
R_result <- 0
B_result <- 0
decision_method <- 0
event_referee <- 0
round_time <- 0
event_round <- 0
event_match <- 0
event_id <- 0
main_fights = data.frame()
pb <- txtProgressBar(min = 0, max = length(event_links_table), style = 3)

for (i in event_links_table) {
  
  fighter_page <- read_html(paste0("http://www.sherdog.com",i))
  
  fights_table <- fighter_page %>%
    # extract fight history
    html_nodes(".resume td , .right_side span , .left_side span") %>%
    # not a well-behaved table so it is extracted as strings
    html_text()

  # Counter and Fight ID
  x <- x + 1
  
  event_id[x] <- x    
  event_match[x] <- fights_table[7]
  R_fighter[x] <- fights_table[1]
  B_fighter[x] <- fights_table[4]
  R_result[x] <- fights_table[2]
  B_result[x] <- fights_table[5]
  decision_method[x] <- fights_table[8]
  event_referee[x] <- fights_table[9]
  event_round[x] <- fights_table[10]
  round_time[x] <- fights_table[11]
  
  setTxtProgressBar(pb, x)
  
}

main_fights <- tibble (event_id = event_id, 
                       event_match = event_match,
                       R_fighter = R_fighter,
                       B_fighter = B_fighter,
                       R_result = R_result,
                       B_result = B_result,
                       decision_method = decision_method,
                       event_referee = event_referee,
                       event_round = event_round,
                       round_time = round_time)

 main_fights
 write.csv(main_fights,"main_fights.csv")
 
 rm(event_id,event_match,
    B_fighter,R_fighter, decision_method, 
    B_result, R_result, round_time, event_round, event_referee)


# Event_Table_Fight_4_Columns -------------------------------------------------------

fighter_page <- read_html("https://www.sherdog.com/events/UFC-1-The-Beginning-7")
  
 x <- 1
 other_fights = data.frame()
 pb <- txtProgressBar(min = 0, max = length(event_links_table), style = 3)
 
 for (i in event_links_table) {
   
   if (x %in% c(256,327)) {
     
     x <- x + 1
     
   }else{
      
     fighter_page <- read_html(paste0("http://www.sherdog.com",i))
     
     fight_extract <- fighter_page %>%
       # extract fight history
       html_nodes(".col_one, .table td:nth-child(1),
    .table td:nth-child(2),
    .table td:nth-child(4),
    .table td:nth-child(5)") %>%
       # html_nodes(".col_four , td:nth-child(6), .col_five , td:nth-child(7)") %>% 
       # not a well-behaved table so it is extracted as strings
       html_text() %>% 
       # wrap text to reform table
       matrix(ncol = 4, byrow = T)
     
     # Add column names from first entries of table
     colnames(fight_extract) <- fight_extract[1,]
     fight_extract <- fight_extract[-1,, drop = F]
     
     fight_extract <- fight_extract %>%
       as.data.frame(stringsAsFactors = F) %>%
       mutate(fight_extract, event_id = x)
     
     other_fights <- rbind(other_fights,fight_extract)
     x <- x + 1
     
     }
      
   setTxtProgressBar(pb, x) 
    
   } 
 
  
write.csv(other_fights,"other_fights_part1.csv")

rm(fight_extract, pb) 


# Event_Table_2_Columns -------------------------------------------------

x <- 1
other_fights = data.frame()
pb <- txtProgressBar(min = 0, max = length(event_links_table), style = 3)

for (i in event_links_table) {
  
  if (x %in% c(256,327)) { #445
    
    x <- x + 1
    
  }else{
    
    fighter_page <- read_html(paste0("http://www.sherdog.com",i))
    
    fight_extract <- fighter_page %>%
      # extract fight history
      html_nodes(".col_four , td:nth-child(6), .col_five , td:nth-child(7)") %>% 
      # not a well-behaved table so it is extracted as strings
      html_text() %>% 
      # wrap text to reform table
      matrix(ncol = 2, byrow = T)
    
    # Add column names from first entries of table
    colnames(fight_extract) <- fight_extract[1,]
    fight_extract <- fight_extract[-1,, drop = F]
    
    fight_extract <- fight_extract %>%
      as.data.frame(stringsAsFactors = F) %>%
      mutate(fight_extract, event_id = x)
    
    other_fights <- rbind(other_fights,fight_extract)
    x <- x + 1
    
  }
  
  setTxtProgressBar(pb, x) 
  
} 

write.csv(other_fights,"other_fights_part2.csv")

read.csv("other_fights_part2.csv")


# Cleanning_Transformation  -----------------------------------------------


###### EVENT TABLE #########################

event_table <- read.csv("event_table.csv")

library(stringr)

event_table$event_name

### Remove spaces between the columns
event_table$event_name <- gsub("([[:digit:]]+\\.*[[:digit:]]*)([A-Z])", "\\1 \\2" , event_table$event_name)
event_table$event_name <- gsub("UFCUltimate", "UFC Ultimate", event_table$event_name)
event_table$event_name <- gsub("UFCThe", "UFC The", event_table$event_name)

rem_dup.one <- function(x){
  paste(unique(trimws(unlist(strsplit(x,split="(?!')[ [:punct:]]",fixed=F,perl=T)))),collapse = " ")
}

x <- 1
for (i in event_table$event_name) {
  event_table$event_name[x] <- rem_dup.one(i)
  x <- x + 1
}

event_table$event_date <- AsDate(event_table$event_date)

event_table <- separate( event_table , event_local , "," ,
          into = c("event_gynasium","event_city","event_state" , "event_country"))

event_table$event_country <- trimws(event_table$event_country)
event_table$event_gynasium <- trimws(event_table$event_gynasium)
event_table$event_country <- trimws(event_table$event_country)
event_table$event_state <- trimws(event_table$event_state)

write.csv(event_table,"event_table_clean.csv")

view(event_table)

################ MAIN FIGHTS ###############

main_fights <- read.csv("main_fights_clean.csv")

view(main_fights)

main_fights <- subset(main_fights, select = -X)

main_fights$event_match <- as.numeric(gsub("Match ","",main_fights$event_match))
main_fights$event_round <- as.numeric(gsub("Round ","",main_fights$event_round))
main_fights$decision_method <- gsub("Method ","",main_fights$decision_method)
main_fights$round_time <- gsub("Time ","",main_fights$round_time)

write.csv(main_fights,"main_fights_clean.csv")


############# OTHER FIGHTS ################


other_fights <- subset(other_fights, select = -X)

other_fights <- other_fights %>% rename(event_match = Match,
                       R_fighter = Fighters,
                       B_fighter = Round)

other_fights$event_match <- as.numeric(trimws(gsub("\n","", other_fights$event_match)))
other_fights$R_fighter <- trimws(gsub("\n","", other_fights$R_fighter))
other_fights$B_fighter <- trimws(gsub("\n","", other_fights$B_fighter))


event_referee <- 0
decision_method <- 0

for (i in other_fights$Time){
  
  event_referee[x] <- substr(i,
                             instr(i,")") + 1,
                             nchar(i))
  decision_method[x] <-substr(i,1,instr(i,")"))
  
  x <- x + 1
}

other_fights <- subset(other_fights, select = -c(fight_extract,Time))

other_fights$decision_method <- decision_method 
other_fights$event_referee <- event_referee

write.csv(other_fights,"other_fights_Part1_Clean.csv")


# OTHER FIGHTS 2 ----------------------------------------------------------

other_fights <- read.csv("other_fights_part2.csv")

head(other_fights,4)

other_fights <- subset(other_fights, select = -c(X,fight_extract.Round,fight_extract.Time))


# Joins -------------------------------------------------------------------



fights_table <- read.csv("other_fights_Part1_Clean.csv")

fights_table$event_round <- other_fights$Round
fights_table$round_time <- other_fights$Time

head(fights_table,30)


rm(list=ls())

main_fights <- read.csv("main_fights_clean.csv")
other_fights <- read.csv("other_fights.csv")  
event_table <- read.csv("event_table_clean.csv")

view(fights_table)
view(main_fights)

main_fights <- main_fights %>% rename(fight_winner  = R_fighter,
                                      fight_looser = B_fighter)

main_fights <- subset(main_fights, select = -c(R_result,B_result,X))
event_table <- subset(event_table, select = -X)

fight_table <- union(main_fights,other_fights)

write.csv(fight_table,"fight_table.csv")

UFC_DataSet <- merge(event_table,fight_table, by = "event_id")

head(UFC_DataSet,10)

write.csv(UFC_DataSet, "UFC_Data.csv")


