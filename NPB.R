
library(rvest)
library(xml2)
library(tidyverse)
library(romaji)


'http://www.baseball-reference.com/register/league.cgi?code=JPCL&class=Fgn' #セ
'http://www.baseball-reference.com/register/league.cgi?code=JPPL&class=Fgn' #パ


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
  pos <- regexpr(separ,str)[1]+nchar(separ)
  substr( str, pos, nchar( str ))
}

#####

# stats b ####
"team_batting"
STATS_b <- function(filepath){
  
  if(!dir.exists(filepath)){
    dir.create(filepath)
  }
  
  for(yearID in 2:74){
    Year <- (2023 - yearID + 1)  
    Year %>% print
    
    linkTeamList <- 
      allYearHtml %>% 
      html_nodes(
        xpath = paste(
          '//*[@id="lg_history"]/tbody/tr[', yearID, ']/td/a', sep = ""
          )
        )
    linkList <- linkTeamList %>% html_attr("href")
    teamList <- linkTeamList %>% html_text()
    teamList %>% print
    
    teamStatsURL <- paste("http://www.baseball-reference.com", linkList, sep = "")
    
    for(i in 1:length(teamList)){
      teamList[i] %>% print
      
      html <- read_html(teamStatsURL[i]) 
      
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
      
      
      teamName <- teamList[i] %>% gsub(pattern = " ",  replacement = "")
      fileName <- paste("./",filepath,"/", Year, teamName, ".csv", sep = "")
      fileName %>% print
      
      if(!file.exists(fileName)){
        tryCatch(
          {
            batting_stats[,-27] %>% 
              filter(Rk != "") %>% 
              mutate(Team = teamList[i]) %>% 
              mutate(Year = Year) %>% 
              mutate(PlayerID = playerID) %>% 
              write.csv(fileName, row.names = FALSE, quote = FALSE)
          },
          error = function(e) {
            message(paste("Error writing file:", fileName))
            message("Error message:", e)
          }
        )
      }
      
      Sys.sleep(5) # Wait for 
    }
  }
}
#

STATS_b("battingStats")

#####

#stats p ####
STATS_p <- function(filepath){
  
  if(!dir.exists(filepath)){
    dir.create(filepath)
  }
  
  for(yearID in 2:74){
    Year <- (2023 - yearID + 1)  
    Year %>% print
    
    linkTeamList <- allYearHtml %>% 
      html_nodes(
        xpath = paste(
          '//*[@id="lg_history"]/tbody/tr[', yearID, ']/td/a', sep = "")
      )
    
    linkList <- linkTeamList %>% html_attr("href")
    teamList <- linkTeamList %>% html_text()
    teamList %>% print
    
    teamStatsURL <- 
      paste("http://www.baseball-reference.com", linkList, sep = "")
    
    for(i in 1:length(teamList)){
      teamList[i] %>% print
      
      txt <- read_html(teamStatsURL[i]) %>% 
        gsub("<!--", "", .) %>% gsub("-->", "", .)
      html <- read_html(txt)
      
      pitching_stats <- html %>% 
        html_node(xpath = '//*[@id="team_pitching"]') %>% 
        html_table()
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
      
      teamName <- teamList[i] %>% gsub(pattern = " ",  replacement = "")
      fileName <- paste("./",filepath,"/", Year, teamName, ".csv", sep = "")
      fileName %>% print
      
      if(!file.exists(fileName)){
        tryCatch(
          {
            pitching_stats[,-32] %>% 
              filter(Rk != "") %>% 
              mutate(Team = teamList[i]) %>% 
              mutate(Year = Year) %>% 
              mutate(PlayerID = playerID) %>% 
              write.csv(fileName, row.names = FALSE, quote = FALSE)
          },
          error = function(e) {
            message(paste("Error writing file:", fileName))
            message("Error message:", e)
          }
        )
      }
      Sys.sleep(5) # Wait for 
    }
  }
}
#

STATS_p("pitchingStats")

#####

# teamstats ####

teamSTATS <- function(folder){
  
  if(!dir.exists(folder)){
    dir.create(folder)
    dir.create(paste("./",folder,"/l", sep = ""))
    dir.create(paste("./",folder,"/b", sep = ""))
    dir.create(paste("./",folder,"/p", sep = ""))
    dir.create(paste("./",folder,"/f", sep = ""))
  }
  
  ## セリーグ
  central <- read_html('http://www.baseball-reference.com/register/league.cgi?code=JPCL&class=Fgn')
  Sys.sleep(2)
  ## パ・リーグ
  pacific <- read_html('http://www.baseball-reference.com/register/league.cgi?code=JPPL&class=Fgn')
  leagues <- list(central,pacific)
  lg <- c("central", "pacific")
  
  i = 1
  
  for(allYearHtml in leagues){
    lg[i] %>% print
    for(yearID in 2:74){
      Year <- (2023 - yearID + 1)  
      Year %>% print
      
      link <- 
        allYearHtml %>% 
        html_nodes(
          xpath = paste(
            '//*[@id="lg_history"]/tbody/tr[', yearID, ']/th/a', 
            sep = ""
            )
          ) %>% 
        html_attr("href")
      url <- paste("http://www.baseball-reference.com",link, sep = "")
      
      txt <- read_html(url) %>% 
        gsub("<!--", "", .) %>% gsub("-->", "", .)
      html <- read_html(txt)
      
      XPATH1 <- '//*[@id="regular_season"]'
      XPATH2 <- '//*[@id="league_batting"]'
      XPATH3 <- '//*[@id="league_pitching"]'
      XPATH4 <- '//*[@id="league_fielding"]'
      
      filePath1 <- paste("./",folder,"/l/", Year, lg[i], "l.csv", sep = "")
      filePath2 <- paste("./",folder,"/b/", Year, lg[i], "b.csv", sep = "")
      filePath3 <- paste("./",folder,"/p/", Year, lg[i], "p.csv", sep = "")
      filePath4 <- paste("./",folder,"/f/", Year, lg[i], "f.csv", sep = "")
      #
      bpFinFun <- function(){
        
        html %>% html_node(xpath = XPATH2) %>% html_table  %>% 
          select(-Aff) %>% 
          rename(
            "team" = Finals, "ageB" = BatAge, "games" = G, "HIT" = H, "second" = `2B`, 
            "third" = `3B`, "RUN" = R, "steal" = SB, "SB" = SH 
          ) %>% 
          mutate(
            year = Year, league = lg[i], single = HIT - (second + third + HR)
          ) %>% 
          write.csv(filePath2, row.names = FALSE, quote = FALSE)
        
        html %>% html_node(xpath = XPATH3) %>% html_table %>% 
          select(-Aff) %>% 
          rename(
            "team" = Finals, "ageP" = PAge, "Ra/G" = `R/G`, "win" = W, "lose" = L, 
            "WL%" = `W-L%`, "games" = G, "start" = GS, "finish" = GF, 
            "complete" = CG, "shutouts" = SHO, "save" = SV, "outs" = IP, 
            "HITallow" = H, "RUNallow" = R, "HRallow" = HR, "BBallow" = BB, 
            "IBBallow" = IBB, "K" = SO, "HBPallow" = HBP
          ) %>% 
          mutate(year = Year, league = lg[i]) %>% 
          write.csv(filePath3, row.names = FALSE, quote = FALSE)
      }
      #
      bpTmFun <- function(){
        
        html %>% html_node(xpath = XPATH2) %>% html_table  %>% 
          select(-Aff) %>% 
          rename(
            "team" = Tm, "ageB" = BatAge, "games" = G, "HIT" = H, "second" = `2B`, 
            "third" = `3B`, "RUN" = R, "steal" = SB, "SB" = SH 
          ) %>% 
          mutate(
            year = Year, league = lg[i], single = HIT - (second + third + HR)
          ) %>% 
          write.csv(filePath2, row.names = FALSE, quote = FALSE)
        
        html %>% html_node(xpath = XPATH3) %>% html_table %>% 
          select(-Aff) %>% 
          rename(
            "team" = Tm, "ageP" = PAge, "Ra/G" = `R/G`, "win" = W, "lose" = L, 
            "WL%" = `W-L%`, "games" = G, "start" = GS, "finish" = GF, 
            "complete" = CG, "shutouts" = SHO, "save" = SV, "outs" = IP, 
            "HITallow" = H, "RUNallow" = R, "HRallow" = HR, "BBallow" = BB, 
            "IBBallow" = IBB, "K" = SO, "HBPallow" = HBP
          ) %>% 
          mutate(year = Year, league = lg[i]) %>% 
          write.csv(filePath3, row.names = FALSE, quote = FALSE)
      }
      #
      ffun <- function(){
        
        table <- html %>% html_node(xpath = XPATH4) %>% html_table
        colnames(table) <- 
          c(
            "team","Aff","games","CG","PO","assists","E","DP","FLD%","PB",
            "stolen","getCS","CS%","lgCS%","pickoffs","Attendance","Maneger"
          )
        table %>% 
          select(-Aff) %>% 
          mutate(year = Year, league = lg[i]) %>% 
          write.csv(filePath4, row.names = FALSE, quote = FALSE)
      }
      #
      html %>% html_node(xpath = XPATH1) %>% html_table  %>% 
        rename(
          "team" = Tm, "wins" = W, "loses" = L, "WL%" = `W-L%`
        ) %>% 
        mutate(year = Year, league = lg[i]) %>% 
        write.csv(filePath1, row.names = FALSE, quote = FALSE)
      
      if(Year <= 2003){
        bpTmFun()
      } else if(2013 <= Year){
        bpTmFun()
        ffun()
      } else if(2004 <= Year && Year <= 2012){
        if(i == 1){
          if(2007 <= Year && Year <= 2012){
            bpFinFun()
            ffun()
          } else if(2005 <= Year && Year <= 2006){
            bpTmFun()
            ffun()
          } else if(Year == 2004){
            bpTmFun()
          }
        } else if(i == 2){
          if(2005 <= Year && Year <= 2012){
            bpFinFun()
            ffun()
          }else if(Year == 2004){
            bpFinFun()
          }
          
        }
      }
      print(paste(Year,"fin", sep = "-"))
      Sys.sleep(5) # Wait for 
    }
    i = 2
  }
}
#

teamSTATS("teamStats")

#####

#
##
#

# NBP data ####

url <- "https://npb.jp/bis/players/all/index.html"
npbPlayerData <- function(url){
  npb <- read_html(url)
  
  kanaList <- list()
  wordList <- list()
  for(i in 1:10){
    pb <- txtProgressBar(min=1, max=nrow(10), style=3)
    if(i <= 7){
      for(j in 1:5){
        path <- npb %>% 
          html_node(
            xpath = paste('//*[@id="pl_lk_unit"]/ul[',i,']/li[',j,']/a', sep = "")
            )
        kana <- path %>% html_attr("href")
        word <- path %>% html_text()
        kanaList[[5*(i-1) + j]] <- kana
        wordList[[5*(i-1) + j]] <- word
      }
    } else if(i == 8){
      for(j in 1:3){
        path <- npb %>% 
          html_node(
            xpath = paste(
              '//*[@id="pl_lk_unit"]/ul[8]/li[',2*j-1,']/a', 
              sep = ""
              )
            )
        kana <- path %>% html_attr("href")
        word <- path %>% html_text()
        kanaList[[7*5 + j]] <- kana
        wordList[[7*5 + j]] <- word
      }
    } else if(i == 9){
      for(j in 1:5){
        path <- npb %>% 
          html_node(xpath = paste('//*[@id="pl_lk_unit"]/ul[9]/li[',j,']/a', sep = ""))
        kana <- path %>% html_attr("href")
        word <- path %>% html_text()
        kanaList[[5*7 + 3 + j]] <- kana
        wordList[[5*7 + 3 + j]] <- word
      }
    } else if(i == 10){
      path <- npb %>% 
      html_node(xpath = '//*[@id="pl_lk_unit"]/ul[10]/li/a')
      kana <- path %>% html_attr("href")
      word <- path %>% html_text()
      kanaList[[44]] <- kana
      wordList[[44]] <- word
    }
    setTxtProgressBar(pb, i)
  }
  #
  allKanaURL <- paste("https://npb.jp/bis/players/all/",kanaList, sep = "")
  result <- c()
  for(i in 40:44){
    wordList[i] %>% print
    kanalink <- read_html(allKanaURL[i])
    
    index <- kanalink %>% html_node(xpath = '//*[@id="pl_result_list"]/div[1]')
    player <- index %>% html_children() %>% html_attr("href")
    #カナごとの選手一覧URL
    allPlayerURL <- paste("https://npb.jp/",player, sep = "")
    table <- c()
    for(j in 1:length(allPlayerURL)){
      playerindex <- read_html(allPlayerURL[j])
      name1 <- playerindex %>% 
        html_node(xpath = '//li[@id="pc_v_name"]') %>% html_text
      name2 <- playerindex %>% 
        html_node(xpath = '//li[@id="pc_v_kana"]') %>% html_text
      table <- playerindex %>% 
        html_node(xpath = '//*[@id="pc_bio"]/table') %>% html_table %>% 
        as.data.frame() %>% column_to_rownames("X1")%>% t() %>% 
        bind_cols(nameJP = name1, kana = name2) %>% bind_rows(table)
      print(paste(j,"/",length(allPlayerURL)))
      Sys.sleep(1)
    }
    result <- rbind(result,table)
    wordList[i] %>% print
    Sys.sleep(2)
  }
}

###

#
thrhit <- function(vec){
  vec = case_when(
    vec == "右" ~ "R",
    vec == "左" ~ "L",
    vec == "両" ~ "B",
    TRUE ~ NA
  )
}
#

## wrangling ####
result111 <- result
result112 <- result111 %>% 
  mutate(
    nameJP = str_replace_all(nameJP, "　", " "),
    nameJP = str_replace_all(nameJP, "                                    ",""),
    kana = str_replace_all(kana, "・", " "),
    birthday = as.Date(生年月日, format = "%Y年%m月%d日"),
    height = str_split(`身長／体重`, "／", simplify = TRUE)[,1],
    weight = str_split(`身長／体重`, "／", simplify = TRUE)[,2],
    height = str_replace(height, "cm", ""),
    weight = str_replace(weight, "kg", ""),
    height = as.integer(na_if(height, "- ")),
    weight = as.integer(na_if(weight, "- ")),
    throwing = str_split(投打, "投", simplify = TRUE)[,1],
    hitting = str_split(投打, "投", simplify = TRUE)[,2],
    throwing = str_replace(throwing, "-右打","--"),
    hitting = str_replace_all(hitting, "打",""),
    throwing = thrhit(throwing),
    hitting = thrhit(hitting),
    draftYear = if_else(ドラフト != "", str_sub(ドラフト,1,4), NA),
    draft = if_else(ドラフト != "", str_sub(ドラフト,6,-1), NA),
    draft = str_replace(draft, "ドラフト", ""),
    career1 = str_split(経歴, " - ", simplify = TRUE)[,1],
    career2 = str_split(経歴, " - ", simplify = TRUE)[,2],
    career3 = str_split(経歴, " - ", simplify = TRUE)[,3],
    career4 = str_split(経歴, " - ", simplify = TRUE)[,4],
    career5 = str_split(経歴, " - ", simplify = TRUE)[,5],
    career6 = str_split(経歴, " - ", simplify = TRUE)[,6],
    career7 = str_split(経歴, " - ", simplify = TRUE)[,7],
    career8 = str_split(経歴, " - ", simplify = TRUE)[,8],
    career9 = str_split(経歴, " - ", simplify = TRUE)[,9],
    career10 = str_split(経歴, " - ", simplify = TRUE)[,10],
    career11 = str_split(経歴, " - ", simplify = TRUE)[,11],
    career1 = na_if(career1, ""),
    career2 = na_if(career2, ""),
    career3 = na_if(career3, ""),
    career4 = na_if(career4, ""),
    career5 = na_if(career5, ""),
    career6 = na_if(career6, ""),
    career7 = na_if(career7, ""),
    career8 = na_if(career8, ""),
    career9 = na_if(career9, ""),
    career10 = na_if(career10, ""),
    career11 = na_if(career11, ""),
    
    
    ) %>% 
  select(-投打,-`身長／体重`,-生年月日,-経歴,-ドラフト)


### 日本語 ####
##
library(reticulate)
py_install("pykakasi")
pykakasi <- import("pykakasi")
convert <- pykakasi$kakasi()$convert
##

result111 %>% 
  mutate(
    kana = str_replace_all(kana, "・", " ")
    ) %>% 
  select(kana)
  mutate(
    draftYear = if_else(ドラフト != "", str_sub(ドラフト,1,4), NA),
    draft = if_else(ドラフト != "", str_sub(ドラフト,6,-1), NA),
    draft = str_replace(draft, "ドラフト", "")
    
    ) %>% 
  select(ドラフト,draftYear,draft) %>% tail(20)
  




