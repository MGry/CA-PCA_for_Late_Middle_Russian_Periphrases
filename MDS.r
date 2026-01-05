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
library(rgl)
library(plotly)
library(ggfortify)

source('unite_and_assess_aspect.r')

#-------------functions-------------
cleanspelling <-function(df){
  for (i in 1:nrow(df)){
    df$word_1[i] <- str_replace_all(df$word_1[i], 'ѣ', 'е')#erstmal die yats weg
    if (endsWith(df$word_1[i], "ати")){#und jetzt die Infinitivendungen updaten
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "ати", 'ать')
    } else if(endsWith(df$word_1[i], "ети")){
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "ети", 'еть')
    } else if(endsWith(df$word_1[i], "ечи")){
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "ечи", 'ечь')
    } else if(endsWith(df$word_1[i], "ути")){
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "ути", 'уть')
    } else if(endsWith(df$word_1[i], "ити")){
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "ити", 'ить')
    } else if(endsWith(df$word_1[i], "оти")){
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "оти", 'оть')
    } else if(endsWith(df$word_1[i], "яти")){
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "яти", 'ять')
    } else if(endsWith(df$word_1[i], "атися")){#selbiges für mediae
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "атися", 'аться')
    } else if(endsWith(df$word_1[i], "етися")){
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "етися", 'еться')
    } else if(endsWith(df$word_1[i], "ечися")){
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "ечися", 'ечься')
    } else if(endsWith(df$word_1[i], "утися")){
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "утися", 'уться')
    } else if(endsWith(df$word_1[i], "итися")){
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "итися", 'иться')
    } else if(endsWith(df$word_1[i], "отися")){
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "отися", 'оться')
    } else if(endsWith(df$word_1[i], "ятися")){
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "ятися", 'яться')
    } else if(endsWith(df$word_1[i], "итися")){
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "итися", 'иться')
    } else if(df$word_1[i] == "итти"){#kleine besonderheit
      df$word_1[i] <- "идти"
    }
    
    if(endsWith(df$word_1[i], "тися")){#zB nestisja -> nestis'
      df$word_1[i] <- stri_replace_last_fixed(df$word_1[i], "тися", 'тись')
    }
  }
  
  df
}

sumup <- function(df, nameofconstruction = "hits for construction1"){#aggregiert duplikate, die durch zusammenfassg verschiedener morph. Formen entstehen, leider klappt die eingebaute fkt nicht
  #df <- cleanspelling(df)#orthografie normieren = updaten
  v1 <- unique(df$word_1)#einen vektor der types in der tabelle erstellen
  v2 <- integer(length(unique(df$word_1)))# einen vektor für die hits, voller nullen
  dfunique <- data.frame(#zu df zusammensetzen
    word_1 <- v1,
    hits <- v2
  )
  
  for (i in 1:nrow(df)){#jetzt über die ausgangstabelle iterieren
    j <- which(dfunique$word_1 == df$word_1[i])#jedes token in der liste der types identifizieren
    dfunique$hits[j] <- dfunique$hits[j] + as.numeric(df$hits[i])#und hits addieren
  }
  dfunique <- dfunique[-2]#warum auch immer, bei diesem vorgehen entsteht eine geisterzeile, die weg muss
  colnames(dfunique)[colnames(dfunique) == 'word_1....v1'] <- 'lex'#und sehr seltsame colnames
  colnames(dfunique)[colnames(dfunique) == 'hits'] <- nameofconstruction #ästhetik
  dfunique
  
}

colllist <- function(exceltable, containsfilter, nameofconstruction = "hits for construction1"){#damit es schneller geht
  oneormorefilters <- class(containsfilter)
  df1 <- read_excel(exceltable)#tabelle einlesen
  if (class(containsfilter)== "list"){
    for (i in containsfilter){
      df1 <- filter(df1, grepl(i, word_0))
    } 
  } else {
    df1 <- filter(df1, grepl(containsfilter, word_0))
  }
  #df2 <- filter(df1, grepl(containsfilter, word_0))#erstmal grammatisch filtern
  df3 <- select(df1, word_1, hits)#jetzt nur das relevante rausnehmen
  df4 <- sumup(df3, nameofconstruction)#duplikate aggregieren, funzt nicht mit der eingebauten fkt
  df4
}

expandvectors <- function(olddf, addeddata, nameofnewvector){
  resdf <- olddf
  resdf$new <- NA#Spalte für die neuen Daten
  j <- 0#bezeichnet die Position wiedergefundener Lexeme
  for (i in 1:nrow(resdf)){# die neue Spalte muss für Addition nutzbar gemacht werden
    resdf$new[i] <- 0
  }
  
  for(i in 1:nrow(addeddata)){#jetzt wird über neue Daten iteriert
    if(addeddata$lex[i] %in% resdf$lex){#Lexem wiederfinden
      j <- which(resdf$lex == addeddata$lex[i])#Position bestimmen
      resdf$new[j] <- resdf$new[j] + addeddata[i,2]#das Feld der Position um den Wert der hits in neuen Daten erhöhen
    } else {#neues Lexem
      resdf[nrow(resdf)+1,] <- NA#neue Zeile anlegen
      resdf[nrow(resdf), 1] <- addeddata$lex[i]#lexem aus neuen Daten übernehmen
      resdf[nrow(resdf), ncol(resdf)] <- addeddata[i,2]#hits aus neuen Daten übernehmen
      for (k in 1:ncol(resdf)){#und jetzt alle NAs beheben
        if(is.na(resdf[nrow(resdf), k])){
          resdf[nrow(resdf), k] <- 0
        }
      }
    }
  }
  names(resdf)[names(resdf) == 'new'] <- nameofnewvector#rename to avoid confusion
  resdf
}

addnewvector <- function(initialdf, exceltable, byfilter, namenewcolumn){
  expandvectors(initialdf, colllist(exceltable, byfilter, namenewcolumn), namenewcolumn)
}

comprise_all_verbtypes_in_corpus <-function(writeexcel = TRUE, returndf = TRUE, excelname = "allverbsincorpus.xlsx", dir_constructions = 'INSERT DIR HERE', dir_allverbtypes = 'INSERT DIR HERE'){#takes a bunch of excel files and makes one big excel file, also returns this as data frame
  setwd(dir_allverbtypes)#first enter the correct directory
  allfiles <- list.files(getwd(), pattern="*.xlsx", all.files=FALSE,#get a list of all files
                         full.names=FALSE)
  alldf <- list()#make a new list where to put them in
  for(i in allfiles){#iterate over files and add the content of the files as list
    alldf <- append(alldf, list(read_excel(i)), 0)
  }
  totaldf <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("lex", "hit"))))#empty df with two rows
  currentword <- ""
  currenthit <- 0
  for(i in alldf){#i = einzelne dataframes
    for(j in 1:nrow(data.frame(i))){#j: zeilen der dataframes
      currentword <- i$word_0[j]
      currenthit <- i$hits[j]
      if(currentword %in% totaldf$lex){#warn if duplicate
        print("duplicate at")
        print(currentword)
      } else {#else: bind to total dataframe
        totaldf <- rbind(totaldf,c(currentword,currenthit))
      }
    }
  } 
  colnames(totaldf) <- c("lex", "hit")#make useful names
  totaldf$hit <- as.numeric(totaldf$hit)#somehow the values were turned to characters in the process
  setwd(dir_constructions)#now put the file into the actual working directory
  if(writeexcel){#if it is to be written down in excel
    write.xlsx2(totaldf, file = excelname, sheetName = "Sheet1",#remember, first col is trash
                col.names = TRUE, row.names = TRUE, append = FALSE)
  }
  if(returndf){#if it is to be returned into the environment
    totaldf
  }
}

get_total_type_hits <-function(thisdata){
  if(class(thisdata) == "character"){
    total_type_hits <- read_excel(thisdata)[,-1]#comprise_all--- generiert seltsame zusatzreihe
  } else {
    total_type_hits <- thisdata
  }
  total_type_hits
}

get_occurrences <-function(thisdata){
  if(class(thisdata) == "character"){
    occurrences <- read_excel(thisdata)[,-1]
  } else {
    occurrences <- thisdata
  }
  occurrences
}

integrate_total_type_hits<- function(allconstructions, allverbs){
  verbtypes <- get_total_type_hits(allverbs)
  constructions <- get_occurrences(allconstructions)
  constructions$corpustotal <- NA
  j <- 0
  for(i in 1:nrow(constructions)){
    if(constructions$lex[i] %in% verbtypes$lex){
      j <- which(verbtypes$lex == constructions$lex[i])
      constructions$corpustotal[i] <- verbtypes$hit[j]
    } else {
      print("not found")
      print(constructions$lex[i])
    }
  }
  constructions
}

pointwisemutualinformation <- function(df, corpusvolume){
  print(head(df))
  w1 <- 0
  w2 <- 0
  w1w2 <-0
  for(i in 1:(ncol(df)-1)){#lex wird schon in der übergeordneten fkt mdsscatterplot bereinigt
    w1 <- sum(df[,i])
    for(j in 1:nrow(df)){
      w2 <- df[j, ncol(df)]
      w1w2 <- df[j, i]
      df[j, i] <- max(0, log((as.numeric(w1w2)/corpusvolume)/((as.numeric(w1)/corpusvolume)*(as.numeric(w2)/corpusvolume)), 2))
    }
  }
  df
}

mdsscatterplot <- function(df, corpusvolume, plottitle, showcos = TRUE){
  if(class(df[,1])=='character'){
    df <- df[,-1]  #erste spalte ist lex, wird erfolgreich gelöscht
  }
  df <- pointwisemutualinformation(df, corpusvolume)
  df <- df[,1:(ncol(df)-1)]#letzte spalte ist corpustotal, muss gelöscht werden
  df <- scale(df)#scale ist default TRUE####!!!!!!
  df <- t(df)
  data.pca <- prcomp(df)
  if(showcos){
    fviz_pca_ind(data.pca, title = plottitle, repel = TRUE, col.ind="cos2") +
       scale_color_gradient2(low="red", high="blue", midpoint=0.6)
  }else{
    fviz_pca_ind(data.pca, title = plottitle, repel = TRUE)
  }
}

mds3d <- function(df, corpusvolume, plottitle){
  df <- df[,-1]  #erste spalte ist lex, wird erfolgreich gelöscht
  df <- pointwisemutualinformation(df, corpusvolume)
  df <- df[,1:(ncol(df)-1)]#letzte spalte ist corpustotal, muss gelöscht werden
  df <- scale(df)#scale ist default TRUE
  df <- t(df)
  data.pca <- prcomp(df, scale. = TRUE)
  print(summary(data.pca))
  #otto <- plot3d(data.pca$scores[,1:3])
  p <- autoplot(data.pca)
  ggplotly(p)
}

#--------------- For my personal use -----------------
quick_miru_ic <- function(
    dir_constructions,
    dir_allverbtypes,
    corpustotal,
    RemovePfiveInfinitives = TRUE,
    CompriseVerbsFirst = FALSE,
    ExcludeLowOcc = FALSE,
    ExclusionThreshold = 50,
    IncludePast = TRUE,
    IncludeBudu = TRUE,
    IncludeModals = TRUE,
    IncludeImamImeju = FALSE,
    IncludeOnlyKhochuFromModals = FALSE,
    returnocc = FALSE,
    IncludePochnu = FALSE,
    IncludeNachnu = FALSE,
    Specialset = FALSE,
    LeaveNames = FALSE
){
  if(CompriseVerbsFirst){
    comprise_all_verbtypes_in_corpus(writeexcel = TRUE, returndf = FALSE, excelname = "allverbsincorpus.xlsx", dir_constructions, dir_allverbtypes)#funktioniert nicht
  }
  #always
  firstv <- colllist("stanu.xlsx", 'fut', "stanuFut")
  print("stanu included")
  totaldata <- addnewvector(firstv, "uchnu.xlsx", "fut", "uchnuFut")
  
  if(Specialset){
    totaldata <- addnewvector(totaldata, "uchnu.xlsx", "praet", "uchnuPst")
    totaldata <- addnewvector(totaldata, "stanu.xlsx", "praet", "stanuPst")
    totaldata <- addnewvector(totaldata, "nachnu.xlsx", "praet", "nachnuPst")
    totaldata <- addnewvector(totaldata, "pochnu.xlsx", "praet", "pochnuPst")
  }
  
  #now add the remaining inchoatives
  if(IncludeNachnu){
    totaldata <- addnewvector(totaldata, "nachnu.xlsx", "fut", "nachnuFut")
  }
  if(IncludePochnu){
    totaldata <- addnewvector(totaldata, "pochnu.xlsx", "fut", "pochnuFut")#zu selten
  }

  #now their praet
  if(IncludePast){
    totaldata <- addnewvector(totaldata, "stanu.xlsx", "praet", "stanuPst")
    totaldata <- addnewvector(totaldata, "uchnu.xlsx", "praet", "uchnuPst")
    if(IncludeNachnu){
      totaldata <- addnewvector(totaldata, "nachnu.xlsx", "praet", "nachnuPst")
    }
    if(IncludePochnu){
      totaldata <- addnewvector(totaldata, "pochnu.xlsx", "praet", "pochnuPst")#zu selten
    }
  }
  
  #now get the star
  if(IncludeBudu){
    totaldata <- addnewvector(totaldata, "budu.xlsx", "fut", "buduFut")
  }
  #now come the modals

  if(IncludeOnlyKhochuFromModals){#small programm: khochu only
    totaldata <- addnewvector(totaldata, "khochu.xlsx", "praes", "khochuPraes")
    if(IncludePast){
      totaldata <- addnewvector(totaldata, "khochu.xlsx", "praet", "khochuPst")
    }
  } else {#but if big programm, that is khochu, mogu and maybe (!) imam/imeju
      if(IncludeModals){
        totaldata <- addnewvector(totaldata, "khochu.xlsx", "praes", "khochuPraes")
        totaldata <- addnewvector(totaldata, "mogu.xlsx", "praes", "moguPraes")
        if(IncludeImamImeju){
          totaldata <- addnewvector(totaldata, "im.xlsx", "praes", "imPraes")
        }
        #and now their past
        if(IncludePast){
          totaldata <- addnewvector(totaldata, "khochu.xlsx", "praet", "khochuPst")
          totaldata <- addnewvector(totaldata, "mogu.xlsx", "praet", "moguPst")
          if(IncludeImamImeju){
            totaldata <- addnewvector(totaldata, "im.xlsx", "praet", "imPst17")
          }
        }
      }
      
  }
  occ <- integrate_total_type_hits(totaldata, "allverbsincorpus.xlsx")
  if(RemovePfiveInfinitives){
    occ <- ttl_pf_removal(occ)
  }
  if(LeaveNames){
    return(occ)
  }
  occ <- occ[,-1]#ohne lex, damit später gerechnet werden kann
  if(ExcludeLowOcc){
    
    print("Hi!")
    occ <- occ[,!sapply(occ, function(x) sum(x))<ExclusionThreshold]
    #df = df[,!sapply(df, function(x) mean(is.na(x)))>0.5]
    print("Ok")
  }
  if(returnocc){
    return(occ)
  }
  if(IncludeModals){#Naming
    nameofscatterplot = "Periphrastic Constructions"
  }else{
    nameofscatterplot = "Inchoative Constructions"
  }
  mdsscatterplot(occ, corpustotal, nameofscatterplot)
}

quickc <- function(
    #----Corpus Treatment Properties----
    dir_constructions,
    dir_allverbtypes,
    corpustotal,
    RemovePfiveInfinitives = TRUE,
    CompriseVerbsFirst = FALSE,
    #----search Properties----
    Alle = FALSE,
    AlleOhnePst = FALSE,
    OhneModals = FALSE,
    #----Detailes Search Properties----
    IncludePast = TRUE,
    IncludeBudu = TRUE,
    IncludeModals = TRUE,
    IncludeImamImeju = FALSE,
    IncludeOnlyKhochuFromModals = FALSE,
    IncludePochnu = FALSE
){
  #to be done later
}
