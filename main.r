rm(list=ls()) 
#specify corpus
library(ca)
library(ggplot2)
library(ggrepel)


#general processing
NoLowOcc = FALSE#only takes frequent verbs
thresholdexcl = 30#with a threshold
ShowWithRemovedPf = TRUE#removes perfective infinitives

#method
CA = TRUE#if FALSE: pca
CAthreshold = 25#number of infinitives included

CAtoggplot = FALSE#sophisticated 

#Visualize cos
coslabel = FALSE

#specify corpus
AlleGenres1600_1699 = FALSE
Del1600_1699 = FALSE
Gib1600_1699 = FALSE
Ver1600_1699 = FALSE

Del1500_1699 = TRUE
KontrolleAlle1617 = FALSE

#specify search
Alle = FALSE
AlleOhnePst = FALSE
AlleOhneInch = FALSE
AlleOhneInchOhnePst = FALSE
AlleOhneModale = FALSE
KleinesSample = TRUE
SpecialSample = FALSE


upperdir <- 'INSERT MAIN FOLDER HERE'
setwd(upperdir)
source('DCA.r')

#------ INITIALIZATION OF CORPUS ------#

if(AlleGenres1600_1699){
  codedir <- 'INSERT FOLDER HERE'
  vtypedir <- 'INSERT FOLDER HERE'
  totalc <- 4620567
  BuduFileExists <- TRUE
  ImamImejuFileExists <- TRUE
  plttitle <- "RNC, 17th ct."
}

if(Del1500_1699){
  codedir <- 'INSERT FOLDER HERE'
  vtypedir <- 'INSERT FOLDER HERE'
  totalc <- 5032854 
  BuduFileExists <- TRUE
  ImamImejuFileExists <- FALSE
  plttitle <- "Business texts, 16-17th ct."
}

if(Del1600_1699){
  codedir <- 'INSERT FOLDER HERE'
  vtypedir <- 'INSERT FOLDER HERE'
  totalc <- 3366818 
  BuduFileExists <- TRUE
  ImamImejuFileExists <- TRUE
  plttitle <- "Business texts, 17th ct."
}

if(Gib1600_1699){
  codedir <- 'INSERT FOLDER HERE'
  vtypedir <- 'INSERT FOLDER HERE'
  totalc <- 1038796
  BuduFileExists <- TRUE
  ImamImejuFileExists <- TRUE
  plttitle <- "Hybrid Register, 17th ct."
}

if(Ver1600_1699){
  codedir <- 'INSERT FOLDER HERE'
  vtypedir <- 'INSERT FOLDER HERE'
  totalc <- 335127
  BuduFileExists <- TRUE
  ImamImejuFileExists <- TRUE
  plttitle <- "Vernacular Register, 17th ct."
}

if(KontrolleAlle1617){
  codedir <- 'INSERT FOLDER HERE'
  vtypedir <- 'INSERT FOLDER HERE'
  totalc <- 4620567 
  BuduFileExists <- TRUE
  ImamImejuFileExists <- TRUE
  plttitle <- "RNC, 17th ct."
}

setwd(codedir)

#------ INITIALIZATION OF CHOSEN VARIABLES ------#
if(KleinesSample){#alle, keine Past
  df <- quick_miru_ic(
    dir_constructions = codedir, 
    dir_allverbtypes = vtypedir,
    corpustotal = totalc,
    ExcludeLowOcc = NoLowOcc,
    ExclusionThreshold = thresholdexcl,
    IncludeBudu = BuduFileExists,
    IncludeModals = FALSE,
    IncludePast = FALSE,
    IncludePochnu = FALSE,
    IncludeNachnu = FALSE,
    LeaveNames = CA,
    returnocc = TRUE
  )
}

if(AlleOhnePst){#alle, keine Past
  df <- quick_miru_ic(
    dir_constructions = codedir, 
    dir_allverbtypes = vtypedir,
    corpustotal = totalc,
    ExcludeLowOcc = NoLowOcc,
    ExclusionThreshold = thresholdexcl,
    IncludeBudu = BuduFileExists,
    IncludeImamImeju = ImamImejuFileExists,
    IncludePast = FALSE,
    IncludePochnu = TRUE,
    IncludeNachnu = TRUE,
    LeaveNames = CA,
    returnocc = TRUE
  )
}

if(AlleOhneInch){
  df <- quick_miru_ic(
    dir_constructions = codedir, 
    dir_allverbtypes = vtypedir,
    corpustotal = totalc,
    ExcludeLowOcc = NoLowOcc,
    ExclusionThreshold = thresholdexcl,
    IncludeBudu = BuduFileExists,
    IncludeImamImeju = ImamImejuFileExists,
    IncludePochnu = FALSE,
    IncludeNachnu = FALSE,
    LeaveNames = CA,
    returnocc = TRUE
  )
}

if(AlleOhneInchOhnePst){#alle, keine Past
  df <- quick_miru_ic(
    dir_constructions = codedir, 
    dir_allverbtypes = vtypedir,
    corpustotal = totalc,
    ExcludeLowOcc = NoLowOcc,
    ExclusionThreshold = thresholdexcl,
    IncludeBudu = BuduFileExists,
    IncludeImamImeju = ImamImejuFileExists,
    IncludePast = FALSE,
    IncludePochnu = FALSE,
    IncludeNachnu=FALSE,
    LeaveNames = CA,
    returnocc = TRUE
  )
}

if(Alle){#alle, keine Past
  df <-quick_miru_ic(
    dir_constructions = codedir, 
    dir_allverbtypes = vtypedir,
    corpustotal = totalc,
    ExcludeLowOcc = TRUE,
    ExclusionThreshold = thresholdexcl,
    IncludeBudu = BuduFileExists,
    IncludeImamImeju = ImamImejuFileExists,
    IncludePochnu = TRUE,
    IncludeNachnu = TRUE,
    LeaveNames = CA,
    returnocc = TRUE,
    RemovePfiveInfinitives = ShowWithRemovedPf
  )
}

if(AlleOhneModale){#alle, keine Past
  df <-quick_miru_ic(
    dir_constructions = codedir, 
    dir_allverbtypes = vtypedir,
    corpustotal = totalc,
    ExcludeLowOcc = NoLowOcc,
    ExclusionThreshold = thresholdexcl,
    IncludeBudu = BuduFileExists,
    IncludeImamImeju = ImamImejuFileExists,
    IncludeModals = FALSE,
    RemovePfiveInfinitives = ShowWithRemovedPf,
    IncludePochnu = TRUE,
    IncludeNachnu = TRUE,
    LeaveNames = CA,
    returnocc = TRUE
  )
}

if(SpecialSample){
  df <-quick_miru_ic(
    dir_constructions = codedir, 
    dir_allverbtypes = vtypedir,
    corpustotal = totalc,
    ExcludeLowOcc = NoLowOcc,
    ExclusionThreshold = thresholdexcl,
    IncludeBudu = BuduFileExists,
    IncludeImamImeju = FALSE,
    IncludeModals = FALSE,
    IncludePast = FALSE,
    Specialset = TRUE,
    RemovePfiveInfinitives = ShowWithRemovedPf,
    LeaveNames = CA
  )
}

#------ VISUALIZATION ------#
#There still is a bug, it only works if each of these lines is run individually. This will be fixed in the new version
if(CA){
  #plot(ca(subset(df, select = -corpustotal)))
  #View(df[,2:(ncol(df)-1)])
  #apply(mtcars, 1, sum)
  df$sums <- apply(df[,2:(ncol(df)-1)], 1, sum)#eigentlich muss df[,2:(ncol...)]
  df <- df[order(df$sums, decreasing = TRUE),]
  row.names(df) <- df$lex
  
  #Below older
  df <- df[df$lex %in% mostdistinctive_fv,]
  #df <- df[1:CAthreshold,]#selects a set of rows
  
  df <- subset(df, select = - c(sums, corpustotal, lex))#deletes unnecessary cok
  if(CAtoggplot){
    #largely copied from https://www.r-bloggers.com/2019/08/correspondence-analysis-visualization-using-ggplot/
    ca.fit <- ca(df)
    ca.plot <- plot(ca.fit)
    ca.plot.df <- data.frame(Label = c(rownames(ca.plot$rows),
                                rownames(ca.plot$cols)),
                      Dim1 = c(ca.plot$rows[,1], ca.plot$cols[,1]),
                      Dim2 = c(ca.plot$rows[,2], ca.plot$cols[,2]),
                      Variable = c(rep("Infinitive", nrow(ca.plot$rows)),
                                   rep("Auxiliary", nrow(ca.plot$cols))))
    ca.plot.df$Size <- ifelse(ca.plot.df$Variable == "Construction", 2, 1)
    
    p <- ggplot(ca.plot.df, aes(x = Dim1, y = Dim2,
                                col = Variable, shape = Variable, 
                                label = Label, size = Size)) +
      geom_vline(xintercept = 0, lty = "dashed", alpha = .5) +
      geom_hline(yintercept = 0, lty = "dashed", alpha = .5) +
      geom_point(size = 1, show.legend = FALSE) +
      scale_x_continuous(limits = range(ca.plot.df$Dim1) + c(diff(range(ca.plot.df$Dim1)) * -0.2,
                                                             diff(range(ca.plot.df$Dim1)) * 0.2)) +
      scale_y_continuous(limits = range(ca.plot.df$Dim2) + c(diff(range(ca.plot.df$Dim2)) * -0.2,
                                                             diff(range(ca.plot.df$Dim2)) * 0.2)) +
      scale_size(range = c(4, 7), guide = F) +
      geom_text_repel(size = 3, show.legend = FALSE, max.overlaps = Inf)
      #geom_label_repel(show.legend = F, segment.alpha = .05, point.padding = unit(5, "points"), max.overlaps = Inf)
    p
      

    
  }else{
    #plot(ca(df))
    #View(ca(df))
    if(coslabel){
      fviz_ca_biplot(ca(df), col.col = "cos2",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
    } else {
      fviz_ca_biplot(ca(df), repel = TRUE, title = plttitle)
    }
    
  }
  colSums(df)

} else {
  mdsscatterplot(df, totalc, plttitle, coslabel)
  colSums(df)
  nrow(df)
}

if(FALSE){
  df2 <- df
  drop <- c("pochnuFut", "nachnuPst", "nachnuFut", "imPraes", "moguPst", "imPst17")
  df2 = df[,!names(df) %in% drop]
  df2$sums <- apply(df2[,2:(ncol(df2)-1)], 1, sum)
  df2 <- df2[order(df2$sums, decreasing = TRUE),]
  #row.names(df) <- df$lex
  df2 <- subset(df2, select = - c(sums))
  CAthreshold = 50
  df2 <- df2[1:CAthreshold,]
  
  fviz_ca_biplot(ca(df2), repel = TRUE, title = plttitle, label = "col")
  colSums(df2)
}
