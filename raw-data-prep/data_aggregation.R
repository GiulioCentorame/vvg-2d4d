# TO DO: Check & discard subjects according to gameplay vars

# how do i aggregate all this data from this disparate spreadsheets?
# closest I can think of is my loop below which retrieves for a filename

# Subject 80 "bad"?

#install.packages("xlsx")
library(xlsx)

dataSheets = c("Debrief", "Distraction_Assignment", "Note_sheet", 
               "Post-Questionnaire", "Writing_Task_Evaluation")

outList = vector("list", length=length(dataSheets))
names(outList) = dataSheets
dat = data.frame(NULL)
for (i in 1:length(dataSheets)) {
  path = "./raw_data/"
  RAs = substr(list.files(path = path, pattern=dataSheets[i]), nchar(dataSheets[i])+2, nchar(dataSheets[i])+3)
  for (j in 1:length(RAs)) {
    fileName = paste(path, dataSheets[i], "_", RAs[j], ".xlsx", sep="")
    temp = read.xlsx(fileName, 1, stringsAsFactors=F)
    temp$Entrant = RAs[j]
    print(fileName); print(names(temp))
    dat = rbind(dat, temp)
    # Save to an object with variable name
    # I'm going to use eval(parse()) and you don't get to pretend you're better than me
    }
  outList[[i]] = dat
  dat = data.frame(NULL)
}

# fix subno name
names(outList[[2]])[1] = "Subject"

# checking for duplicates (e.g. two different subs entered w/ same ID)
# Note that it is right that "digits" has frequent duplicates!
for (i in 1:length(dataSheets)) {
  print(names(outList)[i])
  print(subset(table(outList[[i]]$Subject), table(outList[[i]]$Subject)>1))
}

debrief = outList[[1]]
debrief[debrief$Subject == 168,] # bad
debrief = debrief[!(debrief$Subject %in% c("165 or 166?", "168")),] 
# Gonna put thse back into list b/c I think that makes reshaping easier 
outList[[1]] = debrief ###

distract = outList[[2]]
distract = distract[!(distract$Subject %in% c(168, 170, 203, 33)),] # ??? doesn't remove rows?
distract = distract[!(distract$Subject == "165 or 167?"),]
distract = distract[complete.cases(distract),]
#
outList[[2]] = distract ###

notes = outList[[3]]
notes[notes$Subject == 150,]
notes = notes[-265,] # deleting duplicate row
#
outList[[3]] = notes ###

postQ = outList[[4]]

eval = outList[[5]]
eval = eval[!(eval$Subject %in% c(140, 63, 82)),]
#
outList[[5]] = eval ###

# Bring in digits data, previously cleaned in 2d4d.R
digits = read.delim("./cleaned_data/2d4d.txt")
names(digits)[1] = "Subject"
outList[[6]] = digits

# Aggregate everything into a single huge spreadsheet
require(reshape2)
molten = melt(outList, id.var = "Subject")
outDat = dcast(molten, Subject ~ ...)
# If it's aggregating, you've done it wrong!

# I really must fix the column names before I write more code.

# Fix up some misc data entry screwups
dat$Good.Session_Note_sheet[]

# assign factor codes based on condition
outDat$Violence = ifelse(outDat$Condition_Note_sheet == 1 |
                          outDat$Condition_Note_sheet == 2, "Violent", "Nonviolent")

outDat$Difficulty = ifelse(outDat$Condition_Note_sheet == 2 |
                             outDat$Condition_Note_sheet == 4, "Hard", "Easy")

table(outDat$Condition_Note_sheet, outDat$Violence, outDat$Difficulty, useNA='always')

# may tidy up column order here, make more columns, prettier names, etc.
# version for browsing in Excel and reading feedback
write.table(outDat, "./cleaned_data/aggregated_data_wNotes.txt", sep="\t", row.names=F)
# All this ugly text data is ruining things so i'm making a smaller version for analysis
noteCols = c("julNotes_", "racNotes_", "tayNotes_","Notes_Note_sheet","Why._Debrief")
outDat = outDat[,!(names(outDat) %in% noteCols)]
write.table(outDat, "./cleaned_data/aggregated_data.txt", sep="\t", row.names=F)


