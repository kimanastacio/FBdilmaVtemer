library(readr)
library(magrittr)
library(tm)
library(wordcloud)
library(igraph)

dilma = read_tsv("caminhoprodoc.tab")
names(dilma)
posts_dilma = dilma$post_message %>% unique
posts_dilma[1:10]

#wordclouds
f <- content_transformer(function(x) iconv(x, to='latin1', sub='byte'))

cdilma = Corpus(VectorSource(posts_dilma))
cdilma <- tm_map(cdilma, removePunctuation)
cdilma <- tm_map(cdilma, function(x)removeWords(x,stopwords("pt")))
cdilma <- tm_map(cdilma, content_transformer(tolower))
cdilma <- tm_map(cdilma, function(x)removeWords(x,'mais'))
cdilma <- tm_map(cdilma, function(x)removeWords(x,'agora'))
cdilma <- tm_map(cdilma, function(x)removeWords(x,'com'))
cdilma <- tm_map(cdilma, function(x)removeWords(x,'desde'))
cdilma <- tm_map(cdilma, function(x)removeWords(x,'minha'))
cdilma <- tm_map(cdilma, function(x)removeWords(x,'sobre'))
cdilma <- tm_map(cdilma, function(x)removeWords(x,'disse'))
cdilma <- tm_map(cdilma, function(x)removeWords(x,'o'))
cdilma <- tm_map(cdilma, function(x)removeWords(x,'a'))
cdilma <- tm_map(cdilma, function(x)removeWords(x,'é'))

pal = brewer.pal(5, "Set2")

wordcloud(cdilma, min.freq = 4, max.words = 112, random.order = F, colors = pal)
title(xlab = "")

#palavras frequentes
tdm <- TermDocumentMatrix(cdilma, control = list(wordLengths = c(1, Inf)))
tdm

(freq.terms <- findFreqTerms(tdm, lowfreq=8))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=8)
df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Termos") + ylab("Contagem") +coord_flip()

#Cluster
tdm <- TermDocumentMatrix(cdilma)
tdm <- removeSparseTerms(tdm, sparse = 0.9)
df <- as.data.frame(inspect(tdm))
dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")

fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2, main="Clusterização Hierárquica\nFacebook - Dilma", xlab = "De 31/08/2016 a 10/11/2016")
rect.hclust(fit.ward2, k=2)

#Semântica
(freq.terms <- findFreqTerms(tdm, lowfreq=3))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=3)
df <- data.frame(term = names(term.freq), freq = term.freq)

library(graph)
library(Rgraphviz)

plot(tdm, term = freq.terms, corThreshold = 0.12, weighting = T)

#Rede
matriz <- as.matrix(df)

g = graph_from_incidence_matrix(matriz)
p = bipartite_projection(g, which = "FALSE")
V(p)$shape = "none"
deg = degree(p)
p = delete_vertices(p, 'presidenta')
p = delete_vertices(p, 'dilma')
p = delete_vertices(p, 'rousseff')


plot(p, vertex.label.cex=deg/5, edge.width=(E(p)$weight)/2, 
     edge.color=adjustcolor("grey60", .5),
     vertex.label.color=adjustcolor("red", .7),
     main = "Facebook - Dilma", xlab = "De 31/08/2016 a 10/11/2016")

