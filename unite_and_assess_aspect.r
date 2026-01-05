#Admin
library("factoextra")
library("FactoMineR")
library("ggcorrplot")
library("corrr")
library("xlsx")
library("stringr")
library(stringi)
library("readxl")
library("tidyverse")
library(rcompanion)
library("dplyr")
#rm(list=ls()) # nolint

currentwd <- 'C:/Users/mg/OneDrive/Dokumente/_studium/diss/mds_cda'
setwd(currentwd)

initialize_ttllex <- function(workingdir){#ansteuerung der Daten in einem ordner
  setwd(workingdir)
  allfiles <- list.files(getwd(), pattern="*.xlsx", all.files=FALSE,
                         full.names=FALSE)#Alle excels in Liste 
  alldf <- list()#df, die sämtliche wörter enthält
  
  for(i in allfiles){#jetzt werden die einzelnen Tabellen nacheinander aneinandergehangen
    alldf <- append(alldf, list(read_excel(i)), 0)
  }
  
  alldf
}

ttllex_zusammensetzung <- function(alldf){#nimmt liste, die aus allen excelfiles extrahiert wurde
  
  totaldf <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("lex", "hit"))))#empty df with two rows
  currentword <- ""#initialisierg. laufvariablen
  currenthit <- 0
  
  for(i in alldf){#i = einzelne dataframes in der Liste
    for(j in 1:nrow(data.frame(i))){#j: zeilnen der dataframes
      currentword <- i$word_0[j]
      currenthit <- i$hits[j]
      if(currentword %in% totaldf$lex){
        print("duplicate at")#leider findet er keine duplikate!!!!!
        print(currentword)
      } else {
        totaldf <- rbind(totaldf,c(currentword,currenthit))
      }
    }
  } 
  
  totaldf
}

ttllex <- function(workingdir, nameoffile){
  alldf <- initialize_ttllex(workingdir)#erstellt eine liste, die alle excels als df enthält
  print('directory accessed succesully')
  totaldf <- ttllex_zusammensetzung(alldf)
  colnames(totaldf) <- c("lex", "hit")
  totaldf$hit <- as.numeric(totaldf$hit)
  print('dataframe written')
  totaldf <- unique(totaldf, by = "lex")
  print('dataframe cleaned from duplicate lexemes')
  setwd('..')
  write.xlsx2(totaldf, file = nameoffile, sheetName = "Sheet1",
              col.names = TRUE, row.names = TRUE, append = FALSE)
  print('successfully written')
}

do_nothing <- function(){
  
}

go_through_ipf_leftovers <- function(res, df_ipf){
  for(i in 1:nrow(df_ipf)){
    current_lex_ipf <- df_ipf$lex[i]
    if(current_lex_ipf %in% res$lex){
      do_nothing()
    }else{
      res <- rbind(res, c(current_lex_ipf, 'ipf'))
    }
  }
  res
}

go_through_ipf_pf_lists<-function(df_ipf, df_pf){#kann durchaus zuordnen, aber die ipf liste der wörter, welche sich nicht in df_pf finden, muss noch angehängt w
  res <- data.frame(
    lex <- NA,
    asp <- NA
  )
  for(i in 1:nrow(df_pf)){#jetzt wird pf durchgegangen
    current_lex_pf <- df_pf$lex[i]
    if(current_lex_pf %in% res$lex){
      do_nothing()
    }else{
      current_asp <- "pf"#asp default pf
      current_hit_pf <- df_pf$hit[i]
      current_hit_ipf <- 0#assume RNC never listed word as ipf
      if(current_lex_pf %in% df_ipf$lex){#if RNC actually listed word as ipf
        j <- which(df_ipf$lex == current_lex_pf)#where is it in the df_ipf
        current_hit_ipf <- df_ipf$hit[j]#get nr of hits
        #df_ipf[-j,]#yo, shit's not workin
        if(current_hit_pf - current_hit_ipf < 0){#and there is more ipf
          current_asp <- 'ipf'
        } else if(current_hit_pf - current_hit_ipf == 0){#equal number
          current_asp <- '?'
        } else {#more pf
          do_nothing()
        }
      } else {#if current lex is never listed as ipf
        do_nothing()
      }
      res <- rbind(res, c(current_lex_pf, current_asp))
    }
  }
  res
}

decide_asp <- function(excelfile_ipf, excelfile_pf, newfilename){
  df_ipf <- read_excel(excelfile_ipf)
  df_pf <- read_excel(excelfile_pf)
  res <- go_through_ipf_pf_lists(df_ipf, df_pf)
  print('pf list done')
  res <- go_through_ipf_leftovers(res, df_ipf)
  print('ipf list done')
  colnames(res)[colnames(res) == 'lex....NA'] <- 'lex'#i dont get where this is coming from
  colnames(res)[colnames(res) == 'asp....NA'] <- 'asp' 
  print('colnames readjusted')
  write.xlsx2(res, file = newfilename, sheetName = "Sheet1",
              col.names = TRUE, row.names = TRUE, append = FALSE)
  print('file written')
  print(newfilename)
}

newspelling <- function(oldspellingstr){
  oldspellingstr <- str_replace_all(oldspellingstr, 'ѣ', 'е')#erstmal die yats weg
  if (endsWith(oldspellingstr, "ати")){#und jetzt die Infinitivendungen updaten
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "ати", 'ать')
  } else if(endsWith(oldspellingstr, "ети")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "ети", 'еть')
  } else if(endsWith(oldspellingstr, "ыти")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "ыти", 'ыть')
  } else if(endsWith(oldspellingstr, "очи")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "очи", 'очь')
  } else if(endsWith(oldspellingstr, "ечи")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "ечи", 'ечь')
  } else if(endsWith(oldspellingstr, "ути")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "ути", 'уть')
  } else if(endsWith(oldspellingstr, "ити")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "ити", 'ить')
  } else if(endsWith(oldspellingstr, "оти")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "оти", 'оть')
  } else if(endsWith(oldspellingstr, "яти")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "яти", 'ять')
  } else if(endsWith(oldspellingstr, "атися")){#selbiges für mediae
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "атися", 'аться')
  } else if(endsWith(oldspellingstr, "етися")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "етися", 'еться')
  } else if(endsWith(oldspellingstr, "ытися")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "ытися", 'ыться')
  } else if(endsWith(oldspellingstr, "ечися")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "ечися", 'ечься')
  } else if(endsWith(oldspellingstr, "очися")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "очися", 'очься')
  } else if(endsWith(oldspellingstr, "утися")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "утися", 'уться')
  } else if(endsWith(oldspellingstr, "итися")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "итися", 'иться')
  } else if(endsWith(oldspellingstr, "отися")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "отися", 'оться')
  } else if(endsWith(oldspellingstr, "ятися")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "ятися", 'яться')
  } else if(endsWith(oldspellingstr, "итися")){
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "итися", 'иться')
  } else if(oldspellingstr == "итти"){#kleine besonderheit
    oldspellingstr <- "идти"
  }
  
  if(endsWith(oldspellingstr, "тися")){#zB nestisja -> nestis'
    oldspellingstr <- stri_replace_last_fixed(oldspellingstr, "тися", 'тись')
  }
  res <- oldspellingstr
  res
}

newspellingindf <- function(df, colnamesarelexhit = FALSE){#takes completely raw row with colnames word_1 and hits and returns one with lex and hit
  res <- data.frame(
    lex <- NA,
    hit <-NA
  )
  if(colnamesarelexhit){
    for(i in 1:nrow(df)){
      res <- rbind(res, c(newspelling(df$lex[i]), df$hit[i]))
    }
  } else {
    for(i in 1:nrow(df)){
      res <- rbind(res, c(newspelling(df$word_1[i]), df$hits[i]))
    }
  }
  
  colnames(res)[colnames(res) == 'lex....NA'] <- 'lex'#i dont get where this is coming from
  colnames(res)[colnames(res) == 'hit....NA'] <- 'hit'
  res$hit <- as.numeric(res$hit)
  res
}

scramble_because_grammar_is_ignored <- function(df){
  res <- data.frame(
    lex <- NA,
    hit <-NA
  )
  
  for(i in 1:nrow(df)){
    
    colnames(res)[colnames(res) == 'lex....NA'] <- 'lex'#i do fckin hate this
    colnames(res)[colnames(res) == 'hit....NA'] <- 'hit'
    res$hit <- as.numeric(res$hit)
    current_lex <- df$lex[i]
    
    if(current_lex %in% res$lex){
      
      j <- which(res$lex == current_lex)
      res$hit[j] <- res$hit[j] + df$hit[i]
      
    } else {
      
      res <- rbind(res, c(current_lex, df$hit[i]))

    }
  }
  res
}

delete_all_pf <- function(df, database){
  res <- data.frame(
    lex <- '',
    hit <- 0
  )

  
  for(i in 1:nrow(df)){
    colnames(res)[colnames(res) == 'lex....NA'] <- 'lex'
    colnames(res)[colnames(res) == 'hit....NA'] <- 'hit'
    res$hit <- as.numeric(res$hit)
    
    current_lex <- df$lex[i]
    pf <- 0
    
    if(current_lex %in% database$lex){
      
      j <- which(database$lex == current_lex)
      asp <- database$asp[j]
      
      if(asp == 'pf'){
        pf <- pf + 1
      } else {
        res <- rbind(res, c(current_lex, df$hit[i]))
      }
    } else {
      res <- rbind(res, c(current_lex, df$hit[i]))
    }
  }
  print('pfs deletes')
  print(pf)
  res
}

messy_colnames_cleanup<-function(df){
  res <- df
  colnames(res)[colnames(res) == 'lex......'] <- 'word_1'#this is to harmonize this, all of this is such a f mess
  colnames(res)[colnames(res) == 'hit....0'] <- 'hits'
  res$hit <- as.numeric(res$hit)
  res <- na.omit(res)
  res <- subset(res, select=-c(hit))
  res <- res[-1,]
  res
}

cleanfromperfectives <- function(table_to_be_cleaned, table_database, newfilename, isstilloldspelling=TRUE, needstobescrambled = TRUE){
  res <- data.frame(
    lex <- NA,
    hit <- NA
  )
  datadf <- read_excel(table_to_be_cleaned)
  datadf <- na.omit(datadf)
  df_database <- read_excel(table_database)#database needs to be unique, or else this is not bound to go well
  df_database <- na.omit(df_database)
  if(isstilloldspelling){
    datadf <- newspellingindf(datadf)
    print('spellingupdate done')
  }
  
  if(needstobescrambled){
    datadf <- scramble_because_grammar_is_ignored(datadf)
    print('scrambled')
  }
  
  res <- delete_all_pf(datadf, df_database)
  res <- subset(res, select=-c(hit))
  #print(colnames(res))
  colnames(res)[colnames(res) == 'lex......'] <- 'word_1'#this is to harmonize this, all of this is such a f mess
  colnames(res)[colnames(res) == 'hit....0'] <- 'hits'
  res$hit <- as.numeric(res$hit)
  res <- na.omit(res)
  res <- subset(res, select=-c(hit))
  res <- res[-1,]
  
  write.xlsx2(res, file = newfilename, sheetName = "Sheet1",
              col.names = TRUE, row.names = TRUE, append = FALSE)
}


ttl_pf_removal <- function(ttldf, database = 'INSERT EXCEL HERE', updatespelling = FALSE){
  ttldf$asp <- 'ipf'#assume all are ipf
  aspects <- read_excel(database)
  for(i in 1:nrow(ttldf)){
    current_lex <- newspelling(ttldf$lex[i])#first update spelling, bc will be hard to find in database if not
    if(updatespelling){#check whether renaiming lex
      ttldf$lex[i] <- current_lex
    }
    if(current_lex %in% aspects$lex){#if v is pres in database
      j <- which(aspects$lex == current_lex)#enquire its asp
      current_asp <- aspects$asp[j]
      if(current_asp == 'pf'){#and change default setting 'ipf' only if necessary
        ttldf$asp[i] <- current_asp
      }
    }
  }
  ttldf <- filter(ttldf, grepl('ipf', asp))#filter for asp
  ttldf <- ttldf[,-which(names(ttldf) %in% c("asp"))]#remove asp col
  ttldf
}
