----------------------------------------------------------




news.con <- file("capstone2/final/en_US/en_US.news.txt","rb")
newsblogstwitter <- readLines(news.con, encoding="UTF-8")
close(news.con)
rm(news.con)

blogs.con <- file("capstone2/final/en_US/en_US.blogs.txt","rb")
newsblogstwitter <- readLines(blogs.con,encoding="UTF-8")
close(blogs.con)
rm(blogs.con)

twitter.con <- file("capstone2/final/en_US/en_US.twitter.txt","rb")
newsblogstwitter <- readLines(twitter.con,skipNul=TRUE, encoding="UTF-8")
close(twitter.con)
rm(twitter.con)




newsblogstwitter <- gsub('"',"",newsblogstwitter)
newsblogstwitter <- gsub(',',"",newsblogstwitter)
newsblogstwitter <- gsub('â',"",newsblogstwitter)



library(quanteda)
sent <- tokens(paste0(newsblogstwitter, collapse=" "), what="sentence", remove_numbers=FALSE,
               remove_punct=FALSE, remove_symbols=FALSE, remove_separators=FALSE,
               remove_twitter=FALSE, remove_hyphens=FALSE, remove_url=FALSE, ngrams=1L)[[1]]
rm(newsblogstwitter)


library(tokenizers)
library(tm)
library(data.table)

d1 <- data.table(ngrams=sent, freq=0)
rm(sent)
gc()

system.time(
d3 <- d1[  ,  tokenize_skip_ngrams(ngrams, n=4, n_min=4, k=0)  ,  by=ngrams  ]
)
# 4g
# news                         blogs                         twitter
#    user  system elapsed         user  system elapsed          user  system elapsed
# 1165.97   43.67 1213.48      1277.19   46.54 1327.05       1283.96   50.64 1339.31
# 1151.95   39.81 1194.81      1277.12   46.31 1327.80       1286.77   53.25 1344.52

# 3g
# news                         blogs                         twitter
#    user  system elapsed         user  system elapsed          user  system elapsed
# 1069.78   38.30 1111.69      1236.13   43.67 1284.27       1286.59   53.14 1344.80


# 2g
# news                         blogs                         twitter
#    user  system elapsed         user  system elapsed          user  system elapsed
#  997.89   40.92 1042.64      1116.17   44.80 1164.92       1136.61   52.22 1192.73


rm(d1)
gc()
d3[,ngrams:=NULL]
colnames(d3) <- "ngrams"
d3[,freq:=is.na(ngrams)]
d3 <- d3[freq==FALSE]
d3[,freq:=NULL]
d3[,freq:=1]

#d3[  ,  freq:=sum(freq)  ,  by=ngrams  ]
#d3 <- d3[!duplicated(d3)]#  d3[duplicated(d3),.N]


d3[ , "split":=stringr::str_count(ngrams, " ") ]
d3[ order(-split) ]
d3[split>3]
d3[split<3]
d3[,split:=NULL]


   data.table::fwrite(d3,"capstone2/raw4-gram.txt")
ngram_2 <- fread("capstone2/raw4-gram.txt", showProgress=FALSE, encoding="UTF-8")
colnames(ngram_2) <- c("rnn","wdf")
d3[  ngrams!=ngram_2[,rnn]  ]
ngram_2[  rnn!=d3[,ngrams]  ]

d3 <- d3[  ngrams==ngram_2[,rnn]  ]
   data.table::fwrite(d3,"capstone2/raw4-gram.twitter.txt")



library(data.table)
news <- fread("capstone2/raw4-gram.news.txt", showProgress=FALSE, encoding="UTF-8")
blogs <- fread("capstone2/raw4-gram.blogs.txt", showProgress=FALSE, encoding="UTF-8")
twitter <- fread("capstone2/raw4-gram.twitter.txt", showProgress=FALSE, encoding="UTF-8")

#gc()
d3 <- rbindlist(     list( blogs , twitter , news )     )
#gc()
#rm(news,blogs,twitter)
#gc()
#d3[  ,  freq:=sum(freq)  ,  by=ngrams  ]# Error: memory exhausted (limit reached?)
d3[  ,  freq:=.N  ,  by=ngrams  ]# Error: memory exhausted (limit reached?)
d3 <- d3[!duplicated(d3)]

   data.table::fwrite(d3,"capstone2/raw4-gram.ALL.txt")
