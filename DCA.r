ibrary("tidyverse")
library(rcompanion)
source('MDS.R')

individual_fisher <- function(occaux, occfull, occtogether, corpussize){
  r1 <- c(occtogether, occaux-occtogether)
  r2 <- c(occfull-occtogether, corpussize-occfull-occaux+(2*occtogether))
  ft <- data.frame(r1, r2)
  fisher.test(ft)$p
}

fisher_for_all <- function(vectorofocctogether, auxtotals){#takes the hits for a word with a list of its hits for a list of aux
  df <- data.frame(matrix(ncol = 2, nrow = 0))
  for(i in 1:length(vectorofocctogether)){
    df <- rbind(df, c(vectorofocctogether[i], auxtotals[i+1]-vectorofocctogether[i]))
  }
  fisher.test(df)$p
}

get_auxtotals <-function(occurrences){
  auxtotals <- c(0)#dient dazu, dass gesamtvorkommen der aux nur einmal berechnet w muss
  for (i in 2:ncol(occurrences)){
    auxtotals <- append(auxtotals, sum(occurrences[,i]))
  }
  auxtotals
}

initialize_dca_dataframe <- function(occurrences){
  distinctiveness_matrix <- matrix(ncol=ncol(occurrences))
  colnames(distinctiveness_matrix) <- c(names(occurrences))
  dca_dataframe <- data.frame(distinctiveness_matrix)
  dca_dataframe
}

dca_raw <- function(occurrences, corpussize, onlysignificantlydistinctive = TRUE){#occurrences ist eine occurrence liste: lex (str), occ für aux 1, occ für aux 2, 3,..., gesamtvorkommen von lex in corpus
  dca_dataframe <- initialize_dca_dataframe(occurrences)
  auxtotals <- get_auxtotals(occurrences)

  for(i in 1:nrow(occurrences)){#reihen der occurrences
    p_individual <- c()#vector für current vollverb, wahrscheinlichkeiten für aux
    v_i_occtogether <- c()#vector, der die occurrences von current vollverb für jedes aux registriert 
    i_occfull <- occurrences$corpustotal[i]#dessen gesamtvorkommen
    for(j in 2:(ncol(occurrences)-1)){#jetzt wird jedes aux durchgegangen, letzte row ist corpustotal!
      i_occaux <- auxtotals[j]#hier werden die ttlen vorkommen der aux abgerufen
      i_occtogether <- occurrences[i,j]#die hits für vollverb i mit aux j können aus occurrences abgerufen w
      prob_aux_full <- individual_fisher(i_occaux, i_occfull, i_occtogether, corpussize)#jetzt wird collanalysis durchgeführt
      p_individual <- append(p_individual, prob_aux_full)#das ergebnis wird an den v angehangen
      
      v_i_occtogether <- append(v_i_occtogether, i_occtogether)#zum schluss die hits registrieren
    }
    
    p_all <- fisher_for_all(v_i_occtogether, auxtotals)#somit kann jetzt die eigentliche dca durchgeführt w
    
    completerow <- c(occurrences$lex[i], p_individual,p_all)#jetzt wird die reihe zusammengesetzt
    dca_dataframe <- rbind(dca_dataframe, completerow)#und angehängt
  }
  df <- filter(dca_dataframe, !is.na(lex))#irgendwie, irgendwo ist ne leere Zeile entstanden
  df[, c(2:ncol(df))] <- sapply(df[, c(2:ncol(df))], as.numeric)#sind alles char geworden
  if(onlysignificantlydistinctive){
    df <- filter(df, df$corpustotal<0.05)#nur significante differenz
  }
  df
}

dca_mostdistinctivefor <- function(df, colnrforsorting){#df = dca frame, es muss die Nr der col, die Untersucht w soll, angegeben w
  collectionofwords <- character()
  df <- df[order(df[,colnrforsorting], decreasing = FALSE),]#sortiert, welche am signifikantesten für das V sind
  for(i in 1:nrow(df)){#jetzt jede der Reihen, von hoch sign zu wenig sign durchgehen
    occs <- c()
    for(j in 2:(ncol(df)-1)){#die relevanten occs sind alle außer die erste (namen der lexeme) und der letzten (corpustotal)
      occs <- append(occs, df[i,j])#jede Reihe als vektor
    }
    if(df[i,colnrforsorting]==min(occs)){#wenn die untersuchte col das minimum darstellt, bdtet das, dass es gerade für dieses, nicht die alternativen, constructions distinktiv ist
      collectionofwords <- append(collectionofwords, df$lex[i])#und dann kann es gesammelt w
    }
  }
  collectionofwords
}


combineddca <- function(occ, cols1, corpussize, normalizemass = FALSE){#occurrence liste mit lex, indiv. occurrences, ctotal; cols1: vector mit nummern der cols der gruppe, die gegen die andere getestet werden soll
  if(normalizemass){
    print('Normalizing not implemented yet')
  }
  
  cat1 <- integer()
  cat2 <- integer()
  for(i in 1:nrow(occ)){#jede einzelne reihe durchgehen
    addv1 <- 0
    addv2 <- 0
    
    for(j in 2:(ncol(occ)-1)){#dann spaltenweise, wobei natürlich lex und corpustotal ignoriert werden
      if(j %in% cols1){#falls die nr der spalte in gruppe 1 ist
        addv1 <- addv1 + df[i,j]
      }else{
        addv2 <- addv2 + df[i,j]
      }
    }
    
    cat1 <- append(cat1, addv1)
    cat2 <- append(cat2, addv2)
  }
  lex <- occ[,1]
  corpustotal <- occ[,ncol(occ)]
  df <- data.frame(lex, cat1, cat2, corpustotal)
  dcadf <- dca_raw(df, corpussize)
  distlist <- list(dca_mostdistinctivefor(dcadf, 2), dca_mostdistinctivefor(dcadf, 3))
  distlist
}

numberofaux <- function(df){#I understand that this is not elegant
  a<-1
  for(i in colnames(df)){
    print(a)
    a <- a+1
    print(i)
  }
}
