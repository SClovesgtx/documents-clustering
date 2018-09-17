# https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html
# https://anythingbutrbitrary.blogspot.com/2013/03/build-search-engine-in-20-minutes-or.html

library(textmineR)
library(tm)
library(wordcloud)

# Fazendo o load do data set 
# nativo da library textmineR
data(nih_sample)
View(nih_sample)
?nih_sample


# vamos criar uma matriz
# de quatidade de termos
dtm <- CreateDtm(doc_vec = nih_sample$ABSTRACT_TEXT, # vetor de caracteres dos documentos
                 doc_names = nih_sample$APPLICATION_ID, # nomes dos documentos
                 ngram_window = c(1, 2), # comprimento mínimo e máximo dos n-gramas
                 stopword_vec = c(tm::stopwords("english"), # stopwords do tm (library text miner)
                                  tm::stopwords("SMART")), # este é o valor padrão
                 lower = TRUE, 
                 remove_punctuation = TRUE, 
                 remove_numbers = TRUE, 
                 verbose = FALSE, # desliga o status bar para essa demonstração
                 cpus = 2) 

# construir a matriz de frequência dos termos para obter o vetor de IDF
tf_mat <- TermDocFreq(dtm)

# Precisamos re-ponderar as contagens 
# de palavras na matriz de termos do documento. 
# Fazemos isso multiplicando o termo frequência 
# (neste caso, contagem de palavras em documentos) 
# por um vetor de frequência de documento inverso (IDF).
 
tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf
tfidf <- t(tfidf)

# O próximo passo é calcular a similaridade do cosseno e 
# alterá-lo para uma distância. Vamos usar alguma álgebra 
# linear para fazer isso. O produto escalar de dois vetores 
# de tamanho unitário de valor positivo 
# é a semelhança de cosseno entre os dois vetores.
csim <- tfidf / sqrt(rowSums(tfidf * tfidf))
csim <- csim %*% t(csim)

# Várias funções de agrupamento de R trabalham com distâncias, 
# não semelhanças. Nós convertemos a similaridade do cosseno 
# em distância do cosseno, subtraindo-o de 1. 
# Isso funciona porque a semelhança de cosseno é limitada entre 0 e 1.
cdist <- as.dist(1 - csim)

# O último passo é o agrupamento. Existem muitos algoritmos de clustering por aí.
hc <- hclust(cdist, "ward.D")

clustering <- cutree(hc, 10)

plot(hc, main = "Hierarchical clustering of 100 NIH grant abstracts",
     ylab = "", xlab = "", yaxt = "n")

rect.hclust(hc, 10, border = "red")


# Função para 

p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[ clustering == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})

# create a summary table of the top 5 words defining each cluster
cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)
cluster_summary

# plot a word cloud of one cluster as an example
wordcloud::wordcloud(words = names(cluster_words[[ 5 ]]), 
                     freq = cluster_words[[ 5 ]], 
                     max.words = 50, 
                     random.order = FALSE, 
                     colors = c("red", "yellow", "blue"),
                     main = "Top words in cluster 100")
