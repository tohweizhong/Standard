# quick script to create a wordcloud

library(tm)
library(wordcloud)
library(RColorBrewer)


createWC <- function(vector, minFreq, scale, toRemove = "", rot.per = .15){
    
    feedback <- vector
    
    # custom functions to take care of contractions and plurals
    fix_contractions <- function(doc) {
        # "won't" is a special case as it does not expand to "wo not"
        doc <- gsub("won't", "will not", doc)
        doc <- gsub("n't", " not", doc)
        doc <- gsub("'ll", " will", doc)
        doc <- gsub("'re", " are", doc)
        doc <- gsub("'ve", " have", doc)
        doc <- gsub("'m", " am", doc)
        # 's could be is or possessive: it has no expansion
        doc <- gsub("'s", "", doc) 
        return(doc)
    }
    aggregate.plurals <- function (v) {
        aggr_fn <- function(v, singular, plural) {
            if (! is.na(v[plural])) {
                v[singular] <- v[singular] + v[plural]
                v <- v[-which(names(v) == plural)]
            }
            return(v)
        }
        for (n in names(v)) {
            n_pl <- paste(n, 's', sep='')
            v <- aggr_fn(v, n, n_pl)
            n_pl <- paste(n, 'es', sep='')
            v <- aggr_fn(v, n, n_pl)
        }
        return(v)
    }
    
    #create Corpus
    feedback.corpus <- Corpus(VectorSource(feedback))
    feedback.corpus <- tm_map(feedback.corpus, removePunctuation)
    feedback.corpus <- tm_map(feedback.corpus, tolower)
    feedback.corpus <- tm_map(feedback.corpus, function(x) removeWords(x, stopwords("SMART")))
    feedback.corpus <- tm_map(feedback.corpus, function(x) removeWords(x, stopwords("english")))
    feedback.corpus <- tm_map(feedback.corpus, stemDocument)
    feedback.corpus <- tm_map(feedback.corpus, fix_contractions)
    
    
    #word counts in a matrix
    feedback.corpus <- tm_map(feedback.corpus, PlainTextDocument)
    tdm <- TermDocumentMatrix(feedback.corpus)
    m <- as.matrix(tdm)
    
    #print(rownames(m))
    #remove some redundant words
    tmp <- NULL
    for(tr in toRemove){
        print(tr)
        tmp <- c(tmp, which(rownames(m) == tr))
    }
    #print(tmp)
    m <- m[-tmp,]
#     foo<-c(which(rownames(m)=="questions"),
#            which(rownames(m)=="well"),
#            which(rownames(m)=="extra"),
#            which(rownames(m)=="lecturer"),
#            which(rownames(m)=="gives"),
#            which(rownames(m)=="lot"))
#     
#     m<-m[-foo,]
#     
    #str(m)
    
    v <- sort(rowSums(m),decreasing=TRUE)
    v <- aggregate.plurals(v)
    d <- data.frame(word = names(v),freq=v)
    pal <- brewer.pal(8, "Dark2")
    
    
    #tiff("wordcloud for teach feedback/wordcloud.tiff", width=1280,height=800)
    #wordcloud(d$word, d$freq, min.freq=100)
    
    wordcloud(d$word,d$freq, scale=scale ,min.freq= minFreq ,max.words=Inf, random.order=T, rot.per=rot.per, colors=pal, vfont=c("sans serif","plain"))
    #dev.off()
    
}
