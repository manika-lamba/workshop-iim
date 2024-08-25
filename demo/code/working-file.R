# PART 1

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

## Let’s explore the top 50 words.

topfeatures(vdfm, n = 50)

## Let’s plot two word clouds

### one with the raw term frequencies 

textplot_wordcloud(vdfm,  scale=c(3.5, .75), colors=brewer.pal(8, "Dark2"), 
                   random.order = F, rot.per=0.1, max.words=250, main = "Raw Counts")

### one with TF-IDF
textplot_wordcloud(dfm_tfidf(vdfm),  scale=c(3.5, .75), colors=brewer.pal(8, "Dark2"), 
                   random.order = F, rot.per=0.1, max.words=250, main = "TF-IDF")

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

## Let’s view the topics

term <- terms(lda, 10)
term

##################################################
##################################################

# PART 2
  
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

## Let’s explore the top 50 words

topfeatures(vdfm, n = 50)



