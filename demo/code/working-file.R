
# PART 1 (LDA)

## Load Data

library(quanteda); library(tidyverse); library(RColorBrewer); library(quanteda.textplots)

dataset <- read_csv("https://raw.githubusercontent.com/manika-lamba/workshop-iim/main/demo/data.csv")

## Create Corpus
myCorpus <- corpus(dataset$Abstract)

## Text Pre-Processing -- Creating Document Feature Matrix

dfm <- dfm(myCorpus,
           remove = c(stopwords("english")),
           ngrams=1L,
           stem = F,
           remove_numbers = TRUE, 
           remove_punct = TRUE,
           remove_symbols = TRUE)

vdfm <- dfm_trim(dfm, min_count = 10, min_docfreq = 5)

# min_count = remove words used less than x
# min_docfreq = remove words used in less than x docs

## Let's explore the top 50 words.

topfeatures(vdfm, n = 50)

## Let's plot two word clouds

### one with the raw term frequencies 

textplot_wordcloud(vdfm,  scale=c(3.5, .75), colors=brewer.pal(8, "Dark2"), 
                   random.order = F, rot.per=0.1, max.words=250, main = "Raw Counts")

### one with TF-IDF
textplot_wordcloud(dfm_tfidf(vdfm),  scale=c(3.5, .75), colors=brewer.pal(8, "Dark2"), 
                   random.order = F, rot.per=0.1, max.words=250, main = "TF-IDF")


## Excercise
### Let's now create a dendogram to get an idea of how the words are clustering.

## Topic Modeling (LDA)

library(topicmodels)

# we now export to a format that we can run the topic model with
dtm <- convert(vdfm, to="topicmodels")

# estimate LDA with K topics
K <- 20  
lda <- LDA(dtm, k = K, method = "Gibbs", 
           control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))

# Visualizations Example: LDAVis

#Create Json for LDAVis
library(LDAvis)

topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}


result <- LDA(dtm, 5)
serVis(topicmodels2LDAvis(result))

## Let's view the topics

term <- terms(lda, 10)
term

## Exercise

## Visualize the topics using ggplot2

##################################################
##################################################

# PART 2 (STM)
  
# Remove the pre-created list of “generic” words to our original stop list

library("quanteda.textstats")

stopWords <- c("can","use","uses","used","using","study","studies","first","second","third","also","across","results","result","resulted","may","however","one","two","three","four","five","among","well","within","many","related","i.e","e.g","find","finding","finds","found","increase","increases","increasing","increased","decreased","decrease","decreases","decreasing","propose","proposal","proposals","proposes","proposed","new","old","differ","differs","different","difference","differences","positive","negative","findings","reports","report","reported","state","states","article","articles","examines","examine","suggest","research","researches","researchers","need","needs","show","shows","association","associations","associated","discuss","discusses","discussed","will","likely","unlikely","paper","method","methods","methodology","compared","specifically","approach","impact","impacts","examine","examined","examines","includes","include","included","including","measure","measures","measured","analysis","analyze","analyses","complete","completes","completed","indicate","indicated","indicates","high","higher","low","lower","follow","follows","following","significant","significance","approach","approaches","approached","model","models","demonstrate","demonstrated","demonstrates","yet","best","worst","better","large","small","larger","smaller","several","few","much","less","given","via","long","short","often","years","along","whether","potential","significantly","influence","influenced","influences","develop","develops","developed","good","bad","based","p","group","groups","effect","affect","affects","effects","sample","samples","relationship","relationships","change","changes","m","k","conclusion","conclusions","present","presents")

dfm <- dfm(myCorpus,
           remove = c(stopwords("english"), stopWords),
           ngrams= 1L,
           stem = F,
           remove_numbers = TRUE, 
           remove_punct = TRUE,
           remove_symbols = TRUE)

vdfm <- dfm_trim(dfm, min_count = 10, min_docfreq = 5)

## Let's explore the top 50 words

topfeatures(vdfm, n = 50)


## Exercise
### Plot two word clouds: one with the raw term frequencies and one with TF-IDF
## And observe if you see any difference from Part 1

## Structural Topic Model (STM) with stm package

library(stm)

# use quanteda converter to convert our Dfm
stmdfm <- convert(dfm, to = "stm", docvars = docvars(myCorpus))

## Unlike the topicmodels packages, stm has built in features to help analysts reduce sparse terms (minDoc or minCount)

plotRemoved(stmdfm$documents, lower.thresh = seq(1, 100, by = 10))

out <- prepDocuments(stmdfm$documents, stmdfm$vocab, stmdfm$meta, lower.thresh = 5)

## Topic Modeling using STM

k <- 40

load("./stmFit.RData")

## Exploring the results through stm’s visualizations

plot(stmFit, 
     type = "summary", 
     xlim = c(0,.16), 
     n = 5, 
     labeltype = "prob",
     main = "UNCC Research Topics", 
     text.cex = 0.8)

## Let’s examine one of the topics to interpret its meaning. Let’s consider topic 25 using the labels type.

plot(stmFit, # model results
     type = "labels", # type of plot
     labeltype="prob", # label type for the words
     n = 30, # number of words to show
     topics = 25, # the topic we've selected
     text.cex = 1.2, # this increases the font by 20% (1.2 = 120%)
     width = 50) # this increases the width of the box

topicNames <- labelTopics(stmFit, n = 5)
topic <- data.frame(
  TopicNumber = 1:k,
  TopicProportions = colMeans(stmFit$theta))

## Exploring the effects of the covariates: Subject
Result <- plot(
  prep,
  "Subject",
  method = "difference",
  cov.value1 = "Social Science",
  cov.value2 = "Computing",
  verbose.labels = F,
  ylab = "Expected Difference in Topic Probability by Subject (with 95% CI)",
  xlab = "More Likely Computing                           Not Significant                       More Likely Social Science",
  main = "Effect of Subject on Topic Prevelance for UNCC Research",
  xlim = c(-0.1, 0.1)
)

## Let’s redo this plot but rank the topics.
# order based on Expected Topic Proportion
rank = order(unlist(Result$means))
topicRnk <- topic[rank, ]

plot(
  prep,
  "Subject",
  method = "difference",
  cov.value1 = "Social Science",
  cov.value2 = "Computing",
  verbose.labels = F,
  topics = topicRnk$TopicNumber,
  #labeltype = "custom",
  #custom.labels  = apply(topicNames$prob, 1, function(x) paste0(x, collapse = " + ")),
  ylab = "Expected Difference in Topic Probability by Subject (with 95% CI)",
  xlab = "More Likely Computing                           Not Significant                       More Likely Social Science",
  main = "Effect of Subject on Topic Prevelance for UNCC Research",
  xlim = c(-0.1, 0.1)
)


# Effect of time
par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))
i <- c(9, 18)
plot(
  prep,
  "Year",
  method = "continuous",
  topics = i,
  main = "Topics 9 and 18 by Year",
  printlegend = T,
  ylab = "Exp. Topic Prob",
  xlab = "Year",
  ylim = c(-0.01, 0.16)
)


