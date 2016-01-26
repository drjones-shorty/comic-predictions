# init
libs <- c("tm","plyr","data.table","class", "caret")
lapply(libs, require, character.only = TRUE)
options(stringsAsFactors = FALSE)
options(mc.cores=1)
# FORMAT & CAST: functions to clean upon import
comicsDf <- read.csv("../data/all_issues.csv",
                     colClasses = c('character', 'character', 'numeric', 'character',
                                    'numeric','character','character', 'character',
                                    'character','character','character'), strip.white = TRUE)
# Remove commas
comicsDf$EST_SALES <- gsub("[^[:alnum:]///' ]", "",comicsDf$EST_SALES)
comicsDf <- transform(comicsDf, EST_SALES = as.numeric(EST_SALES))

# ENHANCE: Band values into ranges 
comicsDf$RANK_INDICATOR <- floor(comicsDf$MONTH_RANK/150) #Not using cut to preserve numeric. don't want to flatten factors later
comicsDf$ISSUE_RANGE <- cut(comicsDf$ISSUENUMBER, breaks=(0:1000)*10,dig.lab=10)
# Add pk and concatenated text field
comicsDf$UID <- paste(comicsDf$ID,comicsDf$EST_SALES)
comicsDf$TEXT <- paste(comicsDf$TITLE,comicsDf$REL_EVENTS,comicsDf$CHARACTERS,comicsDf$DESCR)

# RESTRUCTURE:  Create columns for each creator and populate as true if they contributed to the issue
pr = unlist(strsplit(comicsDf$PRICE_INFO, '\\|'))
pr = sapply(strsplit(pr, ":"), "[", 2)
pr = gsub(" ", "",pr)
pr = paste(unique(pr), collapse = ', ')
pr = sort(pr)
pr = unlist(strsplit(pr, split=", "))
pr = sapply(pr, sort)
nprices <- max(sapply(pr, length))
l.prices <- lapply(pr, function(X) c(X, rep(NA, nprices - length(X))))
augmented.df = data.frame(do.call(cbind, l.prices))
temp.df = cbind(comicsDf,augmented.df)
temp.df[-(1:16)] <- mapply(grepl, pattern=temp.df[-(1:16)], x=list(temp.df$PRICE_INFO))+0
comicsDf <- temp.df

# RESTRUCTURE:  Create columns for each creator and populate as true if they contributed to the issue
dr = unlist(strsplit(comicsDf$DATE_INFO, '\\|'))
dr = sapply(strsplit(dr, ":"), "[", 2)
dr = gsub(" ", "",dr)
dr = substr(dr, 1, 7)
dr = paste(unique(dr), collapse = ', ')
dr = sort(dr)
dr = unlist(strsplit(dr, split=", "))
dr = sapply(dr, sort)
ndate <- max(sapply(dr, length))
l.dates <- lapply(dr, function(X) c(X, rep(NA, ndate - length(X))))
augmented.df = data.frame(do.call(cbind, l.dates))
temp.df = cbind(comicsDf,augmented.df)
temp.df[-(1:(nprices + 16))] <- mapply(grepl, pattern=temp.df[-(1:(nprices + 16))], x=list(temp.df$DATE_INFO))+0
comicsDf <- temp.df

tcols <- 16 + nprices + ndate

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
                     !is.null(RANK_INDICATOR) &
                     MONTH_RANK <= 300, select = c(UID,TEXT))
metaOnly <- subset(comicsDf, !is.null(UID) &
                                !is.null(TEXT) &
                                !is.null(ISSUE_RANGE) &
                                !is.null(RANK_INDICATOR) &
                                MONTH_RANK <= 300, select = -c(ID,CREATORS,TEXT,DESCR,TITLE,CHARACTERS,EST_SALES,MONTH_RANK,REL_EVENTS, DATE_INFO, PRICE_INFO,NA.))

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
textCorpus <- cleanCorpus(textCorpus,  c("will", "can", "hes", "shes", "may","get","issue","take", "come", "dont", "going", "make", "thats", "also", "goes", "many", "seen","see","taken","yet"))
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

# Add prediction column
combined.df$EST_RANK_INDICATOR <- paste((combined.df$RANK_INDICATOR * 150), (combined.df$RANK_INDICATOR * 150) + 150, sep = "-")

# Separate data for training and test samples
train.rows <- sample(nrow(combined.df), ceiling(nrow(combined.df)*.8))
test.rows <- (1:nrow(combined.df)) [- train.rows]
# Specificy columns to be evaluted (train) and column to predict(test)
train.cols <- subset(combined.df, select = -c(EST_RANK_INDICATOR,RANK_INDICATOR))
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
