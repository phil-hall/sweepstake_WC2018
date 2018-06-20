library(readxl)
library(jsonlite)
library(httr)
library(plyr)
library(xlsx)

#Get draw data

Draw <- read_xlsx("Sweepstake Draw.xlsx","Draw",col_names = FALSE)
names(Draw) <- c("Team","Name")

#Get result data

url <- "http://worldcup.sfg.io/"

path <- "matches"

raw.result <- GET(url = url, path = path)
raw.content <- rawToChar(raw.result$content)
content <- fromJSON(raw.content)

#Extract completed games and scores

home <- content$home_team[content$status == "completed",c(1,2,3)]
away <- content$away_team[content$status == "completed",c(1,2,3)]

home <- cbind(home,away$goals,content$winner[content$status == "completed"])
away <- cbind(away,home$goals,content$winner[content$status == "completed"])

#Generate figures for table

varNames <- c("Team","code","F","A","Winner")

names(home) <- varNames
names(away) <- varNames

results <- rbind(home,away)

results$P <- 1
results$W <- as.integer(results$Team == results$Winner)
results$D <- as.integer(results$Winner == "Draw")
results$L <- results$P - results$W - results$D
results$GD <- results$F - results$A
results$Pts <- results$W * 3 + results$D * 1

#Merge results with draw to aggregate scores by each person

results <- merge(results,Draw)

Draw$P <- 0
Draw$W <- 0
Draw$D <- 0
Draw$L <- 0
Draw$F <- 0
Draw$A <- 0
Draw$GD <- 0
Draw$Pts <- 0

results <- rbind(results[c("Name","P","W","D","L","F","A","GD","Pts")],Draw[c("Name","P","W","D","L","F","A","GD","Pts")])

results <- ddply(results, .(Name), summarise, 
                 P = sum(P), 
                 W = sum(W), 
                 D = sum(D), 
                 L = sum(L), 
                 F = sum(F),
                 A = sum(A),
                 GD = sum(GD),
                 Pts = sum(Pts))

#Place results in a table ordered by points, goal difference, goals for and name.

table <- results[with(results,order(-Pts,-GD,-F,Name)),]

write.xlsx(table,"Sweepstake Table.xlsx",sheetName="Table")