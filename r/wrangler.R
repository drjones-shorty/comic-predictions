# init
libs <- c("tm","plyr","data.table","class", "caret")
lapply(libs, require, character.only = TRUE)
options(stringsAsFactors = FALSE)
options(mc.cores=4)
# FORMAT & CAST: functions to clean upon import
comicsDf <- read.csv("/Users/petertamisin/Github/comic-predictions/data/all_issues_new.csv", encoding = "UTF-8", strip.white = TRUE)
comicsDf <- unique(comicsDf)

# Remove commas
comicsDf$EST_SALES <- gsub("[^[:digit:]///' ]", "",comicsDf$EST_SALES)
#sum( is.na( comicsDf$EST_SALE ) ) 


comicsDf <- transform(comicsDf, MONTH_RANK = as.numeric(MONTH_RANK))
comicsDf <- transform(comicsDf, EST_SALES = as.numeric(EST_SALES))
comicsDf <- transform(comicsDf, ISSUENUMBER = as.numeric(ISSUENUMBER))

comicsDf <- subset(comicsDf, !is.null(ISSUENUMBER) &
                     !is.null(EST_SALES) &
                     !is.null(MONTH_RANK) &
                     !is.na(ISSUENUMBER) &
                     !is.na(EST_SALES) &
                     !is.na(MONTH_RANK)
)

comicsDf$ISSUE_RANGE <- cut(comicsDf$ISSUENUMBER, breaks=(0:1000)*10,dig.lab=10)
# Add pk and concatenated text field
comicsDf$UID <- paste(comicsDf$Id,comicsDf$EST_SALES,comicsDf$MONTH_RANK)
comicsDf$TEXT <- paste(comicsDf$TITLE,comicsDf$REL_EVENTS,comicsDf$CHARACTERS,comicsDf$DESCR)
comicsDf <- comicsDf[!duplicated(comicsDf[,c('UID')]),]

# number of columns up to this point
tcols <- 14

# RESTRUCTURE:  Create columns for each creator and populate as true if they contributed to the issue
cr = unlist(strsplit(comicsDf$CREATORS, '\\|'))
cr = sapply(strsplit(cr, ":"), "[", 2)
cr = paste(unique(cr), collapse = ', ')
cr = unlist(strsplit(cr, split=", "))
cr = sapply(cr, sort)
numc <- max(sapply(cr, length))
l.names <- lapply(cr, function(X) c(X, rep(NA, numc - length(X))))
augmented.df = data.frame(do.call(cbind, l.names))
temp.df = cbind(comicsDf,augmented.df)
temp.df[-(1:(tcols))] <- mapply(grepl, pattern=names(temp.df)[-(1:(tcols))], x=list(temp.df$CREATORS))+0
comicsDf <- temp.df


# FILTER: Remove rows will incomplete data
textOnly <- subset(comicsDf, !is.null(UID) &
                     !is.null(TEXT) &
                     !is.null(ISSUE_RANGE) &
                     (MONTH_RANK <= 25 | MONTH_RANK > 125), select = c(UID,TEXT))

metaOnly <- subset(comicsDf, !is.null(UID) &
                                !is.null(TEXT) &
                                !is.null(ISSUE_RANGE) &
                                (MONTH_RANK <= 25 | MONTH_RANK >125), select = -c(Id,CREATORS,TEXT,DESCR,TITLE,CHARACTERS,EST_SALES,REL_EVENTS, DATE_INFO, PRICE_INFO,NA.))

# Create corpus for text data
textReader <- readTabular(mapping=list(id="UID",content="TEXT"))
textCorpus <- Corpus(DataframeSource(textOnly), readerControl = list(reader=textReader))

# Clean Corpuses
stripNonAcsii <- function (x) {
  x <- gsub("[^[:alnum:]///' ]", " ", x)
  return(x)
}
removeTags <- function(x) {
  return(gsub("<.*?>", "", x))
}
cleanCorpus <- function(c, stoplist) {
  c <- tm_map(c, content_transformer(removeTags))
  c <- tm_map(c, content_transformer(stripNonAcsii))
  c <- tm_map(c, content_transformer(removeNumbers))
  c <- tm_map(c, content_transformer(tolower))
  c <- tm_map(c, removeWords, c(stopwords("english"), stoplist))
  c <- tm_map(c, content_transformer(stripWhitespace))
  return(c)
}
textCorpus <- cleanCorpus(textCorpus,  c("will", "can", "hes", "shes", "may","get","issue","take", "come", "dont", "going", "make", "thats", "also", "goes", "many", "seen","see","taken","yet","another","anyone","around","artist","away","back","become","band","begins","behind","bring","brings","X.bring.us.","brought","call","called","change","choice","character","comes","comics","coming"))
# Group Terms
#dtm.text <- DocumentTermMatrix(textCorpus)
BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3))}
dtm.text <- DocumentTermMatrix(textCorpus, control = list(tokenize=BigramTokenizer))

# Return pruned DF from DTM
restructureDtm <- function(d) {
  d <- removeSparseTerms(d, 0.99)
  d.m <- as.matrix((d))
  d.df <- as.data.frame(d.m)
  setDT(d.df, keep.rownames = TRUE)[]
  d.df$UID <- d.df$rn
  d.df <- subset(d.df, select = - c(rn))
  return(d.df)
}
text.df <- restructureDtm(dtm.text)

# Combine metadata columns and text columns
combined.df <- merge(text.df,metaOnly,by="UID")
# Remove uneeded column & flatten factors into boolean columns
combined.df <- subset(combined.df, select = - c(UID))
dmy <- dummyVars(" ~ .", data = combined.df)
combined.df <- data.frame(predict(dmy, newdata = combined.df))
# Default empties to Zero
combined.df[is.na(combined.df)] <- 0


# ENHANCE: classify 
highlow <- function(x) {
  retVal <- 'So-so'
  if (x <= 25) retVal <- 'Superstar'
  if (x > 25 & x < 125) retVal <- 'So-so'
  if (x >= 125 ) retVal <- 'Dud'
  return(retVal)
}
combined.df$RANK_INDICATOR <- sapply(combined.df$MONTH_RANK, highlow)
count(combined.df, "RANK_INDICATOR")
# Add prediction column
combined.df$EST_RANK_INDICATOR <-combined.df$RANK_INDICATOR

# Separate data for training and test samples
train.rows <- sample(nrow(combined.df), ceiling(nrow(combined.df)*.8))
test.rows <- (1:nrow(combined.df)) [- train.rows]
# Specificy columns to be evaluted (train) and column to predict(test)
train.cols <- subset(combined.df, select = -c(EST_RANK_INDICATOR,MONTH_RANK,RANK_INDICATOR))
test.cols <- subset(combined.df, select = c(EST_RANK_INDICATOR))


# K Nearest Neigbor
cl <- test.cols[train.rows,]
train <- train.cols[train.rows, ]
test <-  train.cols[test.rows, ]
knn.pred <- knn(train,test, cl)

# View Prediction Results
conf.m <- table("Predicted Rank"= knn.pred, Actual = test.cols[test.rows,])
print(conf.m)

# Pct Correct
print(mean(knn.pred==test.cols[test.rows,]))
