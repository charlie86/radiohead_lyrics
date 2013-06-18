library(tm)

batch_wordstats <- function() {

  dirs <- list.dirs()[-1];
  
  for (dir in dirs) {
    
    print(dir)
    d <- DirSource(dir, pattern="*.txt")
    c <- Corpus(d)
    ws <- wordstats(c)
    
    # write to file
    filename <- paste(basename(dir),"_stats.csv",sep="")
    print(filename)
    write.table(ws, file=filename, sep=",")
  }
}

# Calculate statistics on words
wordstats <- function(c) {
  
  c <- tm_map(c, tolower)
  c <- tm_map(c, removePunctuation)
  tdm <- TermDocumentMatrix(c)
  m <- as.matrix(tdm)
  stopwords <- stopwords('english')
  
  # stopword indices
  sti <- (rownames(m)%in%stopwords)
  nsti <- (!rownames(m)%in%stopwords)
  
  # Total number of words
  wordcount <- colSums(m)
  # Stopwords
  stopwordcount <- colSums(m[sti,])
  # Non-stopwords
  nonstopwordcount <- colSums(m[nsti,])
  # Compute lexical density
  lexdensity <- nonstopwordcount/wordcount*100.0
    
  df <- data.frame(stopwordcount, nonstopwordcount, 
                   lexdensity, wordcount)
  
  return(df)
  
}

rh_boxplots <- function() {
  
  par(mar=c(5, 9, 4, 2)+0.1, las=1, xaxs='i')
  data$Album <- ordered(data$Album, rev(unique(data$Album)))
  
  # Length in seconds
  boxplot(data$Length..seconds. ~ data$Album, horizontal=TRUE, xlab='Length (s)',
          main='Distribution of Track Length by Album', xaxp=c(0,480,4), ylim=c(0,480))
  points(data$Length..seconds, data$Album, pch=16, col=rgb(1,0,0,0.5))
  
  par(xaxs='r')
  
  # Total words
  boxplot(data$Total ~ data$Album, horizontal=TRUE, xlab='Number of words',
          main='Distribution of Number of Words by Album')
  points(data$Total, data$Album, pch=16, col=rgb(1,0,0,0.5))  
  
  # Lyrical density
  boxplot(data$LyricD..words.s. ~ data$Album, horizontal=TRUE, xlab='Lyrical density (words/s)',
          main='Track Lyrical Density by Album', ylim=c(0, 2))
  points(data$LyricD..words.s., data$Album, pch=16, col=rgb(1,0,0,0.5))
  
  # Lexical density
  boxplot(data$LexD ~ data$Album, horizontal=TRUE, ylim=c(0, 100), 
          xlab='Lexical Density',main='Track Lexical Density by Album')
  points(data$LexD, data$Album, pch=16, col=rgb(1,0,0,0.5))
  
  
}




