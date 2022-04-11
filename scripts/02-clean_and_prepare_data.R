library(tesseract)
library(tidyverse)
library(reader)

d2 <- n.readLines("outfile.txt", 
                  header = FALSE,
                  n = 13,
                  skip = 9)

y2 <- gsub('©', '', d2, perl = TRUE)
y2 <- gsub('~', '', y2, perl = TRUE)
y2 <- gsub('$9', '5.9', y2, perl = TRUE)
y2 <- gsub("=", '', y2, perl = TRUE)
y2 <- gsub("a3", '', y2, perl = TRUE)
y2 <- gsub(',', '', y2, perl = TRUE)
y2 <- gsub(':', '', y2, perl = TRUE)
y2 <- gsub('90M44', '', y2, perl = TRUE)

MaleAgeTable <- read.table(text = y2,sep = ' ',fill = TRUE,
                           col.names = c("Age","No Education", "Primary incomplete", "Primary complete",
                                         "Secondary incomplete", "Secondary complete",
                                         "Don't know/missing","Total", "Number", 
                                         "Median Number of Years","Other"),header = FALSE)

MaleAgeTable <-
  MaleAgeTable |>
  mutate(No.Education = as.numeric(No.Education),
         Primary.incomplete = as.numeric(Primary.incomplete),
         Primary.complete = as.numeric(Primary.complete),
         Secondary.incomplete = as.numeric(Secondary.incomplete),
         Secondary.complete = as.numeric(Secondary.complete),
         Don.t.know.missing = as.numeric(Don.t.know.missing),
         Total = as.numeric(Total),
         Number = as.numeric(Number),
         Median.Number.of.Years = as.numeric(Median.Number.of.Years))

# correct row names
MaleAgeTable[1,1] <- "6-9"
MaleAgeTable[7,1] <- "35-39"
MaleAgeTable[8,1] <- "40  9OM44"
MaleAgeTable[9,1] <- "45-49"

# correct commonly mistaken values
MaleAgeTable$Total <-100
MaleAgeTable$No.Education<-MaleAgeTable$No.Education/10
MaleAgeTable$Primary.incomplete[3:13] <- MaleAgeTable$Primary.incomplete[3:13]/10
MaleAgeTable$Primary.complete[5:13] <- MaleAgeTable$Primary.complete[5:13]/10
MaleAgeTable$Secondary.incomplete <- MaleAgeTable$Secondary.incomplete/10
MaleAgeTable$Secondary.complete <- MaleAgeTable$Secondary.complete/10
MaleAgeTable$Don.t.know.missing <- MaleAgeTable$Don.t.know.missing/10
MaleAgeTable$Median.Number.of.Years <- MaleAgeTable$Median.Number.of.Years/10

# correct values 1
MaleAgeTable$No.Education[1] <- 27.1
MaleAgeTable$Don.t.know.missing[1] <- 1.5


# correct values 2
MaleAgeTable$Secondary.incomplete[2] <- 41.3
MaleAgeTable$Secondary.complete[2] <- 0.4

# correct values 3
MaleAgeTable$Secondary.incomplete[3] <- 45.5
MaleAgeTable$Don.t.know.missing[3] <- 0.1
MaleAgeTable$Number[3] <- 1757
MaleAgeTable$Median.Number.of.Years[3] <- 7.8

# correct values 4
# MaleAgeTable[i,]
i<-4
MaleAgeTable$No.Education[i] <- 1.5
MaleAgeTable$Secondary.incomplete[i] <- 21.8
MaleAgeTable$Don.t.know.missing[i] <- 0.1
MaleAgeTable$Median.Number.of.Years[i] <- 7.7

# correct values 5
i<-5
MaleAgeTable$No.Education[i] <- 2.1
MaleAgeTable$Primary.complete[i] <- 41.6
MaleAgeTable$Secondary.incomplete[i] <- 22.4
MaleAgeTable$Don.t.know.missing[i] <- 0.1

# correct values 6
i<-6
MaleAgeTable$No.Education[i] <- 3.5
MaleAgeTable$Secondary.incomplete[i] <- 21.2
MaleAgeTable$Secondary.complete[i] <- 28.6
MaleAgeTable$Median.Number.of.Years[i] <- 5


# correct values 7
# MaleAgeTable[i,]
i<-7
MaleAgeTable$Primary.complete[i] <- 48.9
MaleAgeTable$Secondary.incomplete[i] <- 16.3
MaleAgeTable$Secondary.complete[i] <- 28.8

# correct values 8
# MaleAgeTable[i,]
i<-8
MaleAgeTable$No.Education[i] <- 4.8
MaleAgeTable$Primary.incomplete[i] <- 2
MaleAgeTable$Primary.complete[i] <- 48.8
MaleAgeTable$Secondary.incomplete[i] <- MaleAgeTable$Secondary.complete[i]
MaleAgeTable$Secondary.complete[i] <- 27
MaleAgeTable$Don.t.know.missing[i] <- 0
MaleAgeTable$Number[i] <- 978
MaleAgeTable$Median.Number.of.Years[i] <- 4.9

# correct values 9
# MaleAgeTable[i,]
i<-9
MaleAgeTable$Primary.complete[i] <- 10*MaleAgeTable$Primary.complete[i]
MaleAgeTable$Secondary.incomplete[i] <- 100*MaleAgeTable$Secondary.incomplete[i]
MaleAgeTable$Secondary.complete[i] <- 10*MaleAgeTable$Secondary.complete[i]
MaleAgeTable$Don.t.know.missing[i] <- 0
MaleAgeTable$Number[i] <- 10*MaleAgeTable$Median.Number.of.Years[i]
MaleAgeTable$Median.Number.of.Years[i] <- 4.7

# correct values 10
# MaleAgeTable[i,]
i<-10
MaleAgeTable$Primary.incomplete[i] <- 10*MaleAgeTable$Primary.incomplete[i]
MaleAgeTable$Secondary.incomplete[i] <- 100*MaleAgeTable$Secondary.incomplete[i]
MaleAgeTable$Secondary.complete[i] <- 10*MaleAgeTable$Secondary.complete[i]
MaleAgeTable$Number[i] <- 10*MaleAgeTable$Median.Number.of.Years[i]

# correct values 11
# MaleAgeTable[i,]
i<-11
MaleAgeTable$No.Education[i] <- 10*MaleAgeTable$No.Education[i]
MaleAgeTable$Primary.complete[i] <- 10*MaleAgeTable$Primary.complete[i]
MaleAgeTable$Secondary.incomplete[i] <- 100*MaleAgeTable$Secondary.incomplete[i]
MaleAgeTable$Secondary.complete[i] <- 10*MaleAgeTable$Secondary.complete[i]

# correct values 12
# MaleAgeTable[i,]
i<-12
MaleAgeTable$No.Education[i] <- 10*MaleAgeTable$No.Education[i]
MaleAgeTable$Secondary.incomplete[i] <- 5.9
MaleAgeTable$Secondary.complete[i] <- 5.9
MaleAgeTable$Don.t.know.missing[i] <- 1.5

# correct values 13
# MaleAgeTable[i,]
i<-13
MaleAgeTable$No.Education[i] <- 10*MaleAgeTable$No.Education[i]
MaleAgeTable$Primary.complete[i] <- 10*MaleAgeTable$Primary.complete[i]
MaleAgeTable$Secondary.incomplete[i] <- 6

MaleAgeTable <- MaleAgeTable[1:10]

MaleAgeTable[,11] = "Male Age"

d3 <- n.readLines("outfile.txt", 
                  header = FALSE,
                  n = 2,
                  skip = 23)

MaleResidenceTable <- read.table(text = d3,sep = ' ',fill = TRUE,
                                 col.names = c("Age","No Education", "Primary incomplete", "Primary complete",
                                               "Secondary incomplete", "Secondary complete",
                                               "Don't know/missing","Total", "Number", 
                                               "Median Number of Years","Other"),header = FALSE)

MaleResidenceTable <-
  MaleResidenceTable |>
  mutate(No.Education = as.numeric(No.Education),
         Primary.incomplete = as.numeric(Primary.incomplete),
         Primary.complete = as.numeric(Primary.complete),
         Secondary.incomplete = as.numeric(Secondary.incomplete),
         Secondary.complete = as.numeric(Secondary.complete),
         Don.t.know.missing = as.numeric(Don.t.know.missing),
         Total = as.numeric(Total),
         Number = as.numeric(Number),
         Median.Number.of.Years = as.numeric(Median.Number.of.Years),
         Number = str_remove_all(Number, ","))


# correct values 1
# MaleResidenceTable[i,]
i<-1
MaleResidenceTable$No.Education[i] <- 7.5
MaleResidenceTable$Secondary.incomplete[i] <- 24.9
MaleResidenceTable$Don.t.know.missing[i] <- 0.5
MaleResidenceTable$Number[i] <- 9214
MaleResidenceTable$Median.Number.of.Years[i] <-4.9

# correct values 1
# MaleResidenceTable[i,]
i<-2
MaleResidenceTable$No.Education[i] <- MaleResidenceTable$No.Education[i]/10
MaleResidenceTable$Primary.incomplete[i] <- MaleResidenceTable$Primary.incomplete[i]/10
MaleResidenceTable$Secondary.complete[i] <- MaleResidenceTable$Secondary.complete[i]/10
MaleResidenceTable$Don.t.know.missing[i] <- MaleResidenceTable$Don.t.know.missing[i]/10
MaleResidenceTable$Number[i] <- 5124
MaleResidenceTable$Median.Number.of.Years[i] <-4.4


MaleResidenceTable <- MaleResidenceTable[1:10]

MaleResidenceTable[,11] = "Male Residence"

d4 <- n.readLines("outfile.txt", 
                  header = FALSE,
                  n = 5,
                  skip = 26)

y2 <- gsub('©', '', d4, perl = TRUE)
y2 <- gsub('~', '', y2, perl = TRUE)
y2 <- gsub("=", '', y2, perl = TRUE)
y2 <- gsub(',', '', y2, perl = TRUE)
y2 <- gsub(':', '', y2, perl = TRUE)

MaleRegionTable <- read.table(text = y2,sep = ' ',fill = TRUE,
                              col.names = c("Age","No Education", "Primary incomplete", "Primary complete",
                                            "Secondary incomplete", "Secondary complete",
                                            "Don't know/missing","Total", "Number", 
                                            "Median Number of Years","Other","oo"),header = FALSE)


MaleRegionTable <-
  MaleRegionTable |>
  mutate(No.Education = as.numeric(No.Education),
         Primary.incomplete = as.numeric(Primary.incomplete),
         Primary.complete = as.numeric(Primary.complete),
         Secondary.incomplete = as.numeric(Secondary.incomplete),
         Secondary.complete = as.numeric(Secondary.complete),
         Don.t.know.missing = as.numeric(Don.t.know.missing),
         Total = as.numeric(Total),
         Number = as.numeric(Number),
         Median.Number.of.Years = as.numeric(Median.Number.of.Years),
         Number = str_remove_all(Number, ","))

# correct values
MaleRegionTable$No.Education <- MaleRegionTable$No.Education/10
MaleRegionTable$Primary.incomplete <- MaleRegionTable$Primary.incomplete/10
MaleRegionTable$Secondary.complete <- MaleRegionTable$Secondary.complete/10
MaleRegionTable$Median.Number.of.Years <- MaleRegionTable$Median.Number.of.Years/10
MaleRegionTable$Don.t.know.missing <- MaleRegionTable$Don.t.know.missing/10
MaleRegionTable$Total <-100
# correct values 1
# MaleRegionTable[i,]
i<-1
MaleRegionTable$Don.t.know.missing[i] <- 0.5
MaleRegionTable$Number[i] <- 5337
MaleRegionTable$Median.Number.of.Years[i] <-4.9

# correct values 2
# MaleRegionTable[i,]
i<-2
MaleRegionTable$Primary.incomplete[i] <- 15.4
MaleRegionTable$Number[i] <- 3250
MaleRegionTable$Median.Number.of.Years[i] <-4.8

# correct values 3
# MaleRegionTable[i,]
i<-3
MaleRegionTable$Secondary.incomplete[i] <- 22.2
MaleRegionTable$Number[i] <- 1130
MaleRegionTable$Median.Number.of.Years[i] <-4.6

# correct values 4
# MaleRegionTable[i,]
i<-4
MaleRegionTable$No.Education[i] <- 12.1
MaleRegionTable$Primary.incomplete[i] <- 15
MaleRegionTable$Secondary.incomplete[i] <- 22.2
MaleRegionTable$Number[i] <- 2517
MaleRegionTable$Median.Number.of.Years[i] <-4.3

# correct values 5
# MaleRegionTable[i,]
i<-5
MaleRegionTable$No.Education[i] <- 23.4
MaleRegionTable$Secondary.incomplete[i] <- 15.4
MaleRegionTable$Secondary.complete[i] <- 13.9
MaleRegionTable$Number[i] <- 2103
MaleRegionTable$Median.Number.of.Years[i] <-4.7

MaleRegionTable <- MaleRegionTable[1:10]

MaleRegionTable[,11] = "Male Region"

d5 <- n.readLines("outfile.txt", 
                  header = FALSE,
                  n = 14,
                  skip = 34)


y2 <- gsub('©', '', d5, perl = TRUE)
y2 <- gsub('~', '', y2, perl = TRUE)
y2 <- gsub("=", '', y2, perl = TRUE)
y2 <- gsub(',', '', y2, perl = TRUE)
y2 <- gsub(':', '', y2, perl = TRUE)
y2 <- gsub('—', '', y2, perl = TRUE)
y2 <- gsub('ul', '', y2, perl = TRUE)
y2 <- gsub('‘701', '', y2, perl = TRUE)
y2 <- gsub('‘1061', '', y2, perl = TRUE)
y2 <- gsub("483", '', y2, perl = TRUE)

FemaleAgeTable <- read.table(text = y2,sep = ' ',fill = TRUE)
FemaleAgeTable[,3] <-FemaleAgeTable[,3]/10
FemaleAgeTable[,4] <-FemaleAgeTable[,4]/10
FemaleAgeTable[,6] <-FemaleAgeTable[,6]/10
FemaleAgeTable[,8] <-100
FemaleAgeTable[1,1] <- c("6-9")
FemaleAgeTable[1,4] <- 1.1
FemaleAgeTable[1,10] <- 0.6
FemaleAgeTable[3,9] <- 1907
FemaleAgeTable[3,10] <- 4.9
FemaleAgeTable[4,2] <- 30.3
FemaleAgeTable[4,9] <- 1701
FemaleAgeTable[4,10] <- 4.7
FemaleAgeTable[5,7] <- 1
FemaleAgeTable[,7] <- as.numeric(FemaleAgeTable[,7])/10
FemaleAgeTable[5,9] <- 1483
FemaleAgeTable[5,10] <- 4.7
FemaleAgeTable[6,1:10] <- c("30-34",15.9,3.6,54.2,8.3,17.9, 0.0,100.0,1256,4.6)
FemaleAgeTable[7,1:10] <- c("35-39",22.1,6.3,49.5,6.3,15.6, 0.2,100.0,1154,4.4)
FemaleAgeTable[8,1:10] <- c("40-44",30.3,9.4,39.8,7.1,13.4, 0.0,100.0,975,4.3)
FemaleAgeTable[9,1:10] <- c("45-49",39.5,8.6,36.5,5.4,9.6, 0.5,100.0,787,4.0)
FemaleAgeTable[10,1:10] <- c("50-54",50.0,10.0,31.5,2.6,4.5, 1.4,100.0,684,0.0)
FemaleAgeTable[11,1:10] <- c("55-59",56.6,12.1,25.8,1.7,3.7, 0.0,100.0,619,0.0)
FemaleAgeTable[12,1:10] <- c("60-64",61.3,12.4,20.1,2.1,3.7, 0.4,100.0,471,0.0)
FemaleAgeTable[13,1:10] <- c("65+",75.0,8.3,11.6,2.7,1.5, 0.9,100.0,1061,0.0)

FemaleAgeTable <- FemaleAgeTable[,1:10] 
colnames(FemaleAgeTable) <- c("Age","No.Education", "Primary.incomplete", "Primary.complete",
                              "Secondary.incomplete", "Secondary.complete",
                              "Don.t.know.missing","Total", "Number", 
                              "Median.Number.of.Years")


FemaleAgeTable[,11] = "Female Age"

FemaleAgeTable <-
  FemaleAgeTable |>
  mutate(No.Education = as.numeric(No.Education),
         Primary.incomplete = as.numeric(Primary.incomplete),
         Primary.complete = as.numeric(Primary.complete),
         Secondary.incomplete = as.numeric(Secondary.incomplete),
         Secondary.complete = as.numeric(Secondary.complete),
         Don.t.know.missing = as.numeric(Don.t.know.missing),
         Total = as.numeric(Total),
         Number = as.numeric(Number),
         Median.Number.of.Years = as.numeric(Median.Number.of.Years),
         Number = str_remove_all(Number, ","))

FemaleResidenceTable <- read.csv("raw_data.csv")

FemaleResidenceTable <- FemaleResidenceTable[24:25,1:10]

colnames(FemaleResidenceTable) <- c("Age","No.Education", "Primary.incomplete", "Primary.complete",
                              "Secondary.incomplete", "Secondary.complete",
                              "Don.t.know.missing","Total", "Number", 
                              "Median.Number.of.Years")

FemaleResidenceTable <- FemaleResidenceTable %>%
  mutate(No.Education = as.numeric(No.Education),
         Primary.incomplete = as.numeric(Primary.incomplete),
         Primary.complete = as.numeric(Primary.complete),
         Secondary.incomplete = as.numeric(Secondary.incomplete),
         Secondary.complete = as.numeric(Secondary.complete),
         Don.t.know.missing = as.numeric(Don.t.know.missing),
         Total = as.numeric(Total),
         Number = as.numeric(Number),
         Median.Number.of.Years = as.numeric(Median.Number.of.Years),
         Number = str_remove_all(Number, ","))

FemaleResidenceTable[,10] = FemaleResidenceTable[,10]/10
FemaleResidenceTable[,6] = FemaleResidenceTable[,6]/10
FemaleResidenceTable[,8] = 100
FemaleResidenceTable[1,2] = 20.9
FemaleResidenceTable[1,7] = 0.3
FemaleResidenceTable[1,9] = 9662
FemaleResidenceTable[2,3] = 15.4
FemaleResidenceTable[2,4] = 40.8
FemaleResidenceTable[2,7] = 0.4
FemaleResidenceTable[2,9] = 5707
FemaleResidenceTable[2,10] = 4.0

FemaleResidenceTable[,11] = "Female Residence"

cleaned_data <- rbind(MaleAgeTable,MaleResidenceTable,MaleRegionTable,
                      FemaleAgeTable,FemaleResidenceTable)

colnames(cleaned_data)[1] <- "characteristics" 

write.csv(cleaned_data,"cleaned_data.csv", row.names = FALSE)
