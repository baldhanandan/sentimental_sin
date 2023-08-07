# load packages
library(twitteR)
library(tm)
library(cluster)
library(FactoMineR)
library(RColorBrewer)
library(ggplot2)

??twitteR
#install.packages("FactoMineR")

#collecttweet

# harvest tweets from REI
rei_tweets = userTimeline("AmitShah", n=3000)

# dump tweets information into a data frame
rei_df = twListToDF(rei_tweets)

# get the text
rei_txt = rei_df$text


#Clean Tweets

# remove retweet entities
rei_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", rei_txt)
# remove Atpeople
rei_clean = gsub("@\\w+", "", rei_clean)
# remove punctuation symbols
rei_clean = gsub("[[:punct:]]", "", rei_clean)
# remove numbers
rei_clean = gsub("[[:digit:]]", "", rei_clean)
# remove links
rei_clean = gsub("http\\w+", "", rei_clean)


#Create Corpus

# corpus
rei_corpus = VCorpus(VectorSource(rei_clean))

# convert to lower case
rei_corpus = tm_map(rei_corpus, tolower)
# remove stoprwords
rei_corpus = tm_map(rei_corpus, removeWords, c(stopwords("english"), "vivekg86"))
# remove extra white-spaces
rei_corpus = tm_map(rei_corpus, stripWhitespace)

# term-document matrix
rei_corpus <- tm_map(rei_corpus, PlainTextDocument)
tdm = TermDocumentMatrix(rei_corpus)

# convert as matrix
m = as.matrix(tdm)


# remove sparse terms (word frequency > 95% percentile)
wf = rowSums(m)
m1 = m[wf>quantile(wf,probs=0.95), ]

# remove columns with all zeros
m1 = m1[,colSums(m1)!=0]

# for convenience, every matrix entry must be binary (0 or 1)
m1[m1 > 1] = 1


# step 6: Cluster Analysis

# distance matrix with binary distance
m1dist = dist(m1, method="binary")

# cluster with ward method
cluster = hclust(m1dist, method="ward.D")
clus1=hclust(m1dist, method="ward.D")
# plot dendrogram
dev.new(width=5, height=4, unit="in")
plot(1:20)
plot(clus1, cex=0.7)



# correspondance analysis
rei_ca = CA(m1, graph=FALSE)

# default plot of words
dev.new(width=5, height=4, unit="in")
plot(1:200)
plot(rei_ca$row$coord, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
text(rei_ca$row$coord[,1], rei_ca$row$coord[,2], labels=rownames(m1),
     col=hsv(0,0,0.6,0.9))
title(main="@Zpamsam Correspondence Analysis of tweet words", cex.main=1)

#ggplot


# create data frame
rei_words_df = data.frame(
  words = rownames(m1),
  dim1 = rei_ca$row$coord[,1],
  dim2 = rei_ca$row$coord[,2],
  freq = rowSums(m1))
 
dim1 = rei_ca$row$coord[,1]
dim2 = rei_ca$row$coord[,2]
freq = rowSums(m1)
length(words)
length(dim1)
length(dim2)
length(freq)
length(cluster)
#length(clusters)


# plot
dev.new(width=5, height=4, unit="in")
plot(1:200)
ggplot(rei_words_df, aes(x=dim1, y=dim2, label=words,colour=factor(skew[product == 'p1']))) +
  geom_text(aes(size=freq, colour=cluster), alpha=0.7) +
  scale_size_continuous(breaks=seq(20,80,by=10), range=c(3,8)) +
  scale_colour_manual(values=brewer.pal(9, "YlOrRd")) +
  labs(x="", y="") +
  theme(title = "What does @zpamsam tweet about?",
       plot.title = element_text(size=12),
       axis.ticks=element_blank(),
       legend.position = "none",
       axis.text.x = element_blank(),
       axis.text.y = element_blank()
  )




# partitioning around medoids iwth 6 clusters
k = 6

# pam clustering
rei_pam = pam(rei_ca$row$coord[,1:2], k)

# get clusters
clusters = rei_pam$clustering



# first we need to define a color palette
gbrew = brewer.pal(8, "Dark2")

# I like to use hsv encoding
gpal = rgb2hsv(col2rgb(gbrew))

# colors in hsv (hue, saturation, value, transparency)
gcols = rep("", k)
for (i in 1:k) {
  gcols[i] = hsv(gpal[1,i], gpal[2,i], gpal[3,i], alpha=0.65)
}

# plot with frequencies
wcex = log10(rowSums(m1))
plot(rei_ca$row$coord, type="n", xaxt="n+1", yaxt="n-1", xlab="", ylab="")
title("user's Correspondence Analysis of tweet words", cex.main=1)
for (i in 1:k)
{
  tmp <- clusters == i
  text( rei_ca$row$coord[tmp,1], rei_ca$row$coord[tmp,2],
       labels=rownames(m1)[tmp], cex=wcex[tmp],
       col=gcols[i])
}

