
library(tidyverse)
library(rvest)
library(xml2)

allYearHTML <- c(
  'http://www.baseball-reference.com/register/league.cgi?code=JPPL&class=Fgn', #パ
  'http://www.baseball-reference.com/register/league.cgi?code=JPCL&class=Fgn' #セ
  )

# functions ####
splitLeft <- function(str,separ){
  # str    target string     対象文字列
  # separ  separator         分割する文字列
  #splitLeft("aaa/rrr","/") -> "aaa"
  #
  strsplit(str,separ)[[1]][[1]]
}

splitRight <- function(str,separ){
  # str    target string     対象文字列
  # separ  separator         分割する文字列
  #splitRight("aaa/rrr","/") -> "rrr"
  pos<-regexpr(separ,str)[1]+nchar(separ)
  substr( str, pos, nchar( str ))
}


# read ####

reading_player <- function(){
  
  if(!dir.exists("data")){
    dir.create("data")
    dir.create("./data/batting")
    dir.create("./data/pitching")
    dir.create("./data/fielding")
  }
  
  for(league in allYearHTML){
    allYearHtml <- read_html(league)
    
    for(yearID in 2:74){ # 2
      Year <- (2023 - yearID + 1)  
      Year %>% print
      yearID %>% print
      
      linkTeamList <- 
        allYearHtml %>% 
        html_nodes(
          xpath = paste0('//*[@id="lg_history"]/tbody/tr[', yearID, ']/td/a')
        )
      linkList <- linkTeamList %>% html_attr("href")
      teamList <- linkTeamList %>% html_text()
      teamList %>% print
      
      teamStatsURL <- paste0("http://www.baseball-reference.com", linkList)
      
      for(i in 1:length(teamList)){
        teamList[i] %>% print
        teamName <- teamList[i] %>% gsub(pattern = " ",  replacement = "")
        
        txt <- read_html(teamStatsURL[i]) %>% 
          gsub("<!--", "", .) %>% gsub("-->", "", .)
        html <- read_html(txt)
        batting_stats <- html %>% 
          html_node(xpath = '//*[@id="team_batting"]') %>% html_table 
        playerID <- 
          html %>%
          html_nodes(xpath = '//*[@id="team_batting"]/tbody/tr/td[1]') %>% 
          lapply(FUN = function(x){gsub("<b>", "", x)}) %>% unlist %>% 
          lapply(FUN = function(x){gsub("</b>", "", x)}) %>% unlist %>% 
          lapply(FUN = function(x){strsplit(x, split="\\?")[[1]][2]}) %>% 
          lapply(FUN = function(x){strsplit(x, split=">")[[1]][1]}) %>% 
          lapply(FUN = function(x){gsub("id=", "", x)}) %>% 
          lapply(FUN = function(x){gsub("\"", "", x)}) %>% unlist %>% 
          lapply(FUN = function(x){splitLeft(x, " data-stat=player")}) %>% unlist
        fileName <- paste0("./data/batting/", Year, teamName, ".csv")
        fileName %>% print
        tryCatch({
          batting_stats[,-27] %>% 
            filter(Rk != "") %>% 
            mutate(Team = teamList[i]) %>% 
            mutate(Year = Year) %>% 
            mutate(PlayerID = playerID) %>% 
            write_csv(file = fileName)
          },
          error = function(e){
            message(paste("Error writing file:", fileName))
            message("Error message:", e)
          }
        )
        
        # pitching
        pitching_stats <- html %>% 
          html_node(xpath = '//*[@id="team_pitching"]') %>% html_table
        playerID <- 
          html %>%
          html_nodes(xpath = '//*[@id="team_pitching"]/tbody/tr/td[1]') %>% 
          lapply(FUN = function(x){gsub("<b>", "", x)}) %>% unlist %>% 
          lapply(FUN = function(x){gsub("</b>", "", x)}) %>% unlist %>% 
          lapply(FUN = function(x){strsplit(x, split="\\?")[[1]][2]}) %>% 
          lapply(FUN = function(x){strsplit(x, split=">")[[1]][1]}) %>% 
          lapply(FUN = function(x){gsub("id=", "", x)}) %>% 
          lapply(FUN = function(x){gsub("\"", "", x)}) %>% unlist %>% 
          lapply(FUN = function(x){splitLeft(x, " data-stat=player")}) %>% unlist
        
        fileName <- paste0("./data/pitching/", Year, teamName, ".csv")
        fileName %>% print
        tryCatch({
          pitching_stats[,-32] %>% 
            filter(Rk != "") %>% 
            mutate(Team = teamList[i]) %>% 
            mutate(Year = Year) %>% 
            mutate(PlayerID = playerID) %>% 
            write_csv(file = fileName)
          },
          error = function(e){
            message(paste("Error writing file:", fileName))
            message("Error message:", e)
          }
        )
        
        # all_team_fielding
        PATH <- '//*[@id="team_fielding_'
        fielding_stats <- c()
        
        for(pos in list("1B", "2B", "3B", "SS", "OF", "C", "P")){
          tryCatch({
            fileName <- paste("./data/fielding/", Year, teamName, "-", pos, ".csv", sep = "")
            fileName %>% print
            field <- html %>% 
              html_node(xpath = paste0(PATH, pos, '"]')) %>% html_table
            playerID <- 
              html %>% 
              html_nodes(xpath = paste0(PATH, pos, '"]/tbody/tr/th[1]')) %>% 
              lapply(FUN = function(x){gsub("<b>", "", x)}) %>% unlist %>% 
              lapply(FUN = function(x){gsub("</b>", "", x)}) %>% unlist %>% 
              lapply(FUN = function(x){strsplit(x, split="\\?")[[1]][2]}) %>% 
              lapply(FUN = function(x){strsplit(x, split=">")[[1]][1]}) %>% 
              lapply(FUN = function(x){gsub("id=", "", x)}) %>% 
              lapply(FUN = function(x){gsub("\"", "", x)}) %>% unlist %>% 
              lapply(FUN = function(x){splitLeft(x, " data-stat=player")}) %>% unlist
            field %>% select(-Notes) %>% 
              mutate(
                Team = teamList[1], Year = Year, 
                PlayerID = playerID, Position = pos
              ) %>% 
              relocate(PlayerID, .after = Name) %>% 
              relocate(Team, .after = PlayerID) %>% 
              relocate(Year, .after = Team) %>%
              relocate(Position, .after = Year) %>% 
              write_csv(file = fileName)
            },
            error = function(e){
              message(paste("Error writing file:", fileName))
              message("Error message:", e)
            }
          )
        }
        
        Sys.sleep(6) # Wait for 
      }
    }    
  }
}

reading_player()










