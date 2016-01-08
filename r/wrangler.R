# Todo: Refactor to change creator logic. No need for ngram.  Names and roles are explicitly specified
# Todo: Add prices
# Todo: Rewrite as functions so you can pass in test set
# Todo: Add predictor for sales
# Todo: Add functions to show variables with highest abs(correlation)

# init
libs <- c("tm","plyr","data.table","class", "caret","RWeka")
lapply(libs, require, character.only = TRUE)
options(stringsAsFactors = FALSE)
# FORMAT & CAST: functions to clean upon import
comicsDf <- read.csv("/Users/petertamisin/r_files/comic-predictions/data/all_issues.csv",
                     colClasses = c('character', 'character', 'numeric', 'character',
                                    'numeric','character','character', 'character',
                                    'character','character','character'), strip.white = TRUE)
# Remove commas
comicsDf$EST_SALES <- gsub("[^[:alnum:]///' ]", "",comicsDf$EST_SALES)
comicsDf <- transform(comicsDf, EST_SALES = as.numeric(EST_SALES))

# ENHANCE: Band values into ranges 
comicsDf$RANK_RANGE <- floor(comicsDf$MONTH_RANK/50) #Not using cut to preserve numeric. don't want to flatten factors later
comicsDf$SALES_RANGE <- cut(comicsDf$EST_SALES,breaks=(0:100)*10000,dig.lab=10)
comicsDf$ISSUE_RANGE <- cut(comicsDf$ISSUENUMBER, breaks=(0:1000)*10,dig.lab=10)
# Add pk and concatenated text field
comicsDf$UID <- paste(comicsDf$ID,comicsDf$EST_SALES)
comicsDf$TEXT <- paste(comicsDf$TITLE,comicsDf$REL_EVENTS,comicsDf$CHARACTERS,comicsDf$DESCR)

# FILTER: Remove rows will incomplete data
textOnly <- subset(comicsDf, !is.null(UID), select = c(UID,TEXT))
metaOnly <- subset(comicsDf, (!is.null(UID) &
                                !is.null(ISSUE_RANGE) &
                                !is.null(RANK_RANGE) &
                                !is.null(SALES_RANGE)), select = c(UID,CREATORS,SALES_RANGE,ISSUE_RANGE,RANK_RANGE))
colnames(metaOnly) <- c("UID","CREATORS","SALES_RANGE","ISSUE_RANGE","RANK_RANGE")

# Create one Corpus for text data and another for creators (ngrams are different)
metaReader <- readTabular(mapping=list(id="UID",content="CREATORS"))
textReader <- readTabular(mapping=list(id="UID",content="TEXT"))
metaCorpus <- Corpus(DataframeSource(metaOnly), readerControl = list(reader=metaReader))
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
textCorpus <- cleanCorpus(textCorpus,  c("will", "can", "hes", "shes", "may","get","issue","take", "come", "dont", "going", "make", "thats", "also", "goes", "many", "seen","see","taken","yet"))
metaCorpus <- cleanCorpus(metaCorpus, c("penciller", "writer", "letterer", "editor", "cover", "artist", "colorist", "penciler"))

# Group Terms
dtm <- DocumentTermMatrix(textCorpus)
BigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 1, max = 2))}
TrigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 3))}
dtm.text <- DocumentTermMatrix(textCorpus, control = list(tokenize=BigramTokenizer))
dtm.meta <- DocumentTermMatrix(metaCorpus, control = list(tokenize=TrigramTokenizer))

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
meta.df <- restructureDtm(dtm.meta)

# Combine metadata columns and text columns
combined.df <- merge(text.df,meta.df,by="UID")
combined.df <- merge(combined.df, subset(metaOnly, select= c(UID,SALES_RANGE,ISSUE_RANGE,RANK_RANGE)),by="UID")
# Remove uneeded column & flatten factors into boolean columns
combined.df <- subset(combined.df, select = - c(UID))
dmy <- dummyVars(" ~ .", data = combined.df)
combined.df <- data.frame(predict(dmy, newdata = combined.df))
# Default empties to Zero
combined.df[is.na(combined.df)] <- 0

# Add prediction column
#combined.df$EST_RANK_RANGE <- combined.df$RANK_RANGE
combined.df$EST_RANK_RANGE <- paste((combined.df$RANK_RANGE * 50), (combined.df$RANK_RANGE * 50) + 50, sep = "-")


# Separate data for training and test samples
train.rows <- sample(nrow(combined.df), ceiling(nrow(combined.df)*.9))
test.rows <- (1:nrow(combined.df)) [- train.rows]
# Specificy columns to be evaluted (train) and column to predict(test)
train.cols <- subset(combined.df, select = - c(EST_RANK_RANGE,RANK_RANGE))
test.cols <- subset(combined.df, select = c(EST_RANK_RANGE))


# K Nearest Neigbor
cl <- test.cols[train.rows,]
train <- train.cols[train.rows, ]
test <-  train.cols[test.rows, ]
knn.pred <- knn(train,test, cl)

# View Prediction Results
conf.m <- table("Predicted Rank"= knn.pred, Actual = test.cols[test.rows,])
conf.m
