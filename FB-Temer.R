library(readr)
library(magrittr)
library(tm)
library(wordcloud)
library(igraph)

#separa o arquivo
temer = read_tsv("caminhoprodocumento.tab")

#identifica a parte a ser analisada (post_message)
#publica dez exemplos
names(temer)
posts_temer = temer$post_message %>% unique
posts_temer[1:10]

#wordcloud
f <- content_transformer(function(x) iconv(x, to='latin1', sub='byte'))

#limpa os dados
ctemer = Corpus(VectorSource(posts_temer))
#coloca em minúsculas
ctemer <- tm_map(ctemer, content_transformer(tolower))
#remove a pontuação
ctemer <- tm_map(ctemer, removePunctuation)
#remove as stopwords
ctemer <- tm_map(ctemer, function(x)removeWords(x,stopwords("pt")))
# + palavras removidas
ctemer <- tm_map(ctemer, function(x)removeWords(x,"após"))
ctemer <- tm_map(ctemer, function(x)removeWords(x,"onde"))
ctemer <- tm_map(ctemer, function(x)removeWords(x,"nesta"))
ctemer <- tm_map(ctemer, function(x)removeWords(x,"ainda"))
ctemer <- tm_map(ctemer, function(x)removeWords(x,"todo"))
ctemer <- tm_map(ctemer, function(x)removeWords(x,"todos"))
ctemer <- tm_map(ctemer, function(x)removeWords(x,"disse"))
ctemer <- tm_map(ctemer, function(x)removeWords(x,"afirmou"))
ctemer <- tm_map(ctemer, function(x)removeWords(x,"anunciou"))
ctemer <- tm_map(ctemer, function(x)removeWords(x,"destacou"))
ctemer <- tm_map(ctemer, function(x)removeWords(x,"faz"))
ctemer <- tm_map(ctemer, function(x)removeWords(x,"ser"))
ctemer <- tm_map(ctemer, function(x)removeWords(x,"é"))

#seleciona a cor (set1, 2 ou 3)
pal = brewer.pal(5, "Set2")

wordcloud(ctemer, min.freq = 6, max.words = 112, random.order = F, colors = pal)

#palavras frequentes
tdm <- TermDocumentMatrix(ctemer, control = list(wordLengths = c(1, Inf)))
tdm

#inspeciona as palavras frequentes
(freq.terms <- findFreqTerms(tdm, lowfreq=15))

term.freq <- rowSums(as.matrix(tdm))
#seleciona apenas as com frequência >=20
term.freq <- subset(term.freq, term.freq >=20)
df <- data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2)

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Termos") + ylab("Contagem") +coord_flip()

#Cluster
tdm <- TermDocumentMatrix(ctemer)
#Remove os termos esparsos
tdm <- removeSparseTerms(tdm, sparse = 0.85)
df <- as.data.frame(inspect(tdm))
dim(df)

df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")

fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2, main="Clusterização Hierárquica\nFacebook - Temer", xlab = " 31/08/2016 a 10/11/2016")
rect.hclust(fit.ward2, k=5)

#Semântica
(freq.terms <- findFreqTerms(tdm, lowfreq=3))

term.freq <- rowSums(as.matrix(tdm))
#seleciona apenas as com frequência >=3
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
#Retira palavras indesejadas
p = delete_vertices(p, 'presidente')
p = delete_vertices(p, 'temer')
p = delete_vertices(p, 'michel')

plot(p, vertex.label.cex=deg/7, edge.width=(E(p)$weight)/2, 
     edge.color=adjustcolor("grey60", .5),
     vertex.label.color=adjustcolor("blue", .7),
     main = "Facebook - Temer", xlab = " 31/08/2016 a 10/11/2016")