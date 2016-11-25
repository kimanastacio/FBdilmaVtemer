#analise em R de postagens no FB - adaptar para dados da Dilma

library(readr)
library(magrittr)
library(tm)
library(wordcloud)
library(igraph)
library(graph)
library(Rgraphviz)
library(ggplot2)

#separa o arquivo
temer = read_tsv("caminhoprodocumento.tab")

#identifica a parte a ser analisada (post_message)
#publica dez exemplos
names(temer)
posts_temer = temer$post_message %>% unique
posts_temer[1:10]

#Wordcloud
f <- content_transformer(function(x) iconv(x, to='latin1', sub='byte'))

#transforma os posts em corpus de analise
ctemer = Corpus(VectorSource(posts_temer))
#coloca em minusculas
ctemer <- tm_map(ctemer, content_transformer(tolower))
#remove a pontuacao
ctemer <- tm_map(ctemer, removePunctuation)
#remove as stopwords
ctemer <- tm_map(ctemer, function(x)removeWords(x,stopwords("pt")))
# + palavras removidas
ctemer <- tm_map(ctemer, function(x)removeWords(x,"após", "onde", "nesta","ainda", "todo", "todos", "disse", "afirmou", "anunciou", "destacou", "faz", "ser", "é"))

#seleciona a cor (set1, 2 ou 3)
pal = brewer.pal(5, "Set2")

#monta nuvem
wordcloud(ctemer, min.freq = 6, max.words = 112, random.order = F, colors = pal)

#Palavras frequentes

#monta matriz termo-documento
tdm <- TermDocumentMatrix(ctemer, control = list(wordLengths = c(1, Inf)))
tdm

#inspeciona as palavras frequentes
(freq.terms <- findFreqTerms(tdm, lowfreq=15))

term.freq <- rowSums(as.matrix(tdm))

#seleciona apenas as com frequência >=20
term.freq <- subset(term.freq, term.freq >=20)
df <- data.frame(term = names(term.freq), freq = term.freq)

# grafico com as frequencias
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Termos") + ylab("Contagem") +coord_flip()

#Cluster
tdm <- TermDocumentMatrix(ctemer)

#remove os termos esparsos
tdm <- removeSparseTerms(tdm, sparse = 0.85)

#transforma matriz em banco de dados
df <- as.data.frame(inspect(tdm))
dim(df)

#calcula a distancia euclidiana
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")

#grafico de clusterização hierarquica
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2, main="Clusterização Hierárquica\nFacebook - Temer", xlab = " 31/08/2016 a 10/11/2016")
rect.hclust(fit.ward2, k=5)

#Semantica
(freq.terms <- findFreqTerms(tdm, lowfreq=3))

term.freq <- rowSums(as.matrix(tdm))

#seleciona apenas as com frequencia >=3
term.freq <- subset(term.freq, term.freq >=3)
df <- data.frame(term = names(term.freq), freq = term.freq)

# grafico com a rede semantica
plot(tdm, term = freq.terms, corThreshold = 0.12, weighting = T)

#Rede
matriz <- as.matrix(df)

g = graph_from_incidence_matrix(matriz)
p = bipartite_projection(g, which = "FALSE")
V(p)$shape = "none"
deg = degree(p)

#retira palavras indesejadas
p = delete_vertices(p, 'presidente', 'temer', 'michel')

#grafico da rede
plot(p, vertex.label.cex=deg/7, edge.width=(E(p)$weight)/2, 
     edge.color=adjustcolor("grey60", .5),
     vertex.label.color=adjustcolor("blue", .7),
     main = "Facebook - Temer", xlab = " 31/08/2016 a 10/11/2016")