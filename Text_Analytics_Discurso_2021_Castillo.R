
# Owner  : Arnold Simson Condor Cirineo
# Load libraries
library(tm)
library(colorspace)
library(wordcloud)
library(SnowballC)
library(gsubfn)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(forcats)
library(data.table)
library(arules)
library(arulesViz)

library(visNetwork)
library(igraph)
# ---------------------------------
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Rgraphviz")
# ---------------------------------
# Set directory
getwd()
wd<-"D:/ACADEMY/ANALYTICS/TEXT MINING/Text_Analytics_Discurso_2021_Castillo"
setwd(wd)
# ---------------------------------
# Paths

# file discourse
file_path_discurso<-"discurso.txt"
# file stopwords
file_path_stopwords<-"stopwords.es.txt"
# ---------------------------------
# Load


# Load discourse
txt_    <- readLines(file_path_discurso,encoding="UTF-8")
#txt = iconv(txt, to="ASCII//TRANSLIT")

# Load stopwords
sw_txt <- readLines(file_path_stopwords,encoding="UTF-8")

# ---------------------------------
# Pre processing

# clean text
unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='Ni',  'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='ni', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

txt=gsubfn(paste(names(unwanted_array),collapse='|'), unwanted_array,txt_)


# buid  corpus
corpus <- Corpus(VectorSource(txt))

# text to tolower 
d  <- tm_map(corpus, tolower)

# clean white spaces
d  <- tm_map(d, stripWhitespace)

# clean la scores/numbers
d <- tm_map(d, removePunctuation)


# clean  generic stopwords
d <- tm_map(d, removeWords, stopwords("spanish"))

# clean  generic stopwords from list
sw <- c(stopwords("spanish")[-162],'u','f')
sw = iconv(sw, to="ASCII//TRANSLIT")
d <- tm_map(d, removeWords, sw)

# Make document matrix
tdm <- TermDocumentMatrix(d)

# List minimum ocurrences
findFreqTerms(tdm, lowfreq=20)


m <- as.matrix(tdm)

v <- sort(rowSums(m),decreasing=TRUE)

df <- data.frame(word = names(v),freq=v)

wordcloud(df$word,df$freq,min.freq=6)
wordcloud(df$word,df$freq,min.freq=3,colors = brewer.pal(8, "Dark2"),rot.per = 0.5)

wordcloud(words = df$word, 
          freq  = df$freq,
          min.freq = 4, 
          max.words = Inf, 
          colors = brewer.pal(n = 8, name = 'Dark2'),
          scale=c(3,.2))
text(x=0.5, y=0.975, " Términos más empleados en el primer discurso del Presidente Pedro Castillo")


top_n=20
df_top<-df[1:top_n, ]
df_top$word <- factor(df_top$word, levels = df_top$word[order(df_top$freq)])


df_top%>%
  mutate(name = fct_reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = freq)) + 
  coord_flip() + 
  labs(title = "Palabras más frecuentes",  x = "Palabras", y = "Número de usos")




str(m)
m <- as.matrix(tdm)

tdm_lite <- removeSparseTerms(tdm, 0.99)
tdm_lite = t(as.matrix(tdm_lite))


# assocation rules with function apriori 
rules = apriori(tdm_lite, parameter=list(support=0.01, confidence=0.02))
rules <- sort(rules, by = "lift")

rules_dt <- data.table( lhs = labels( lhs(rules) ), 
                        rhs = labels( rhs(rules) ), 
                        quality(rules) )[ order(-lift), ]

rules_dt
n_valid_lhs_rhs <-nrow(rules_dt[rules_dt$lift> 1,]) 

valid_rules <- head(sort(rules, by="lift"), n_valid_lhs_rhs)

# Methods : matrix, mosaic, doubledecker, graph, paracoord, scatterplot, grouped matrix, two-key plot, matrix3D

plot(rules, method = "help")
plot(rules, method = "graph", engine = "help")
plot(rules, method = "graph", engine = "igraph",control = "help")
#-------------------------------------------------------
plot(valid_rules, method = "graph", engine = "igraph")
#-------------------------------------------------------
plot(valid_rules, method = "graph")
#-------------------------------------------------------

plot(valid_rules, engine = "ggplot2", main = NULL, limit = 100) + 
  scale_color_gradient2(low = "red", mid = "gray90", high = "blue", 
                        midpoint = 1, limits = c(0,12)) +
  labs(x = "Supp.", y = "Conf.", color = "Lift") + 
  theme_classic()
#-------------------------------------------------------
# Interactive scatter plot using the grid engine (selected rules are returned)
if(interactive()) {
  sel <- plot(valid_rules, engine = "interactive")
  
  # Create a html widget for interactive visualization (uses plotly)
  plot(valid_rules, engine = "htmlwidget")}
#-------------------------------------------------------
plot(valid_rules, method = "two-key plot", limit = 100)
#-------------------------------------------------------
# works better with small sets of rules
subrules <- subset(rules, lift > 1)
subrules

# 2D matrix with shading (ggplot2). The LHS and RHS are reordered so 
# that rules with similar lift are displayed close to each other.
plot(subrules, method = "matrix")
#-------------------------------------------------------
if(interactive()) {
  plot(subrules, method = "matrix", engine = "interactive")
  plot(subrules, method = "matrix", engine = "htmlwidget")
}
#-------------------------------------------------------
plot(subrules, method = "graph", 
     control = list(
       edges = ggraph::geom_edge_link(
         end_cap = ggraph::circle(4, "mm"),
         start_cap = ggraph::circle(4, "mm"),
         color = "black",
         arrow = arrow(length = unit(2, "mm"), angle = 20, type = "closed"),
         alpha = .2
       ),
       nodes = ggraph::geom_node_point(aes_string(size = "support", color = "lift")),
       nodetext = ggraph::geom_node_label(aes_string(label = "label"), alpha = .8, repel = TRUE)
     ),
     limit = 10
) + 
  scale_color_gradient(low = "yellow", high = "red") + 
  scale_size(range = c(2, 10)) 
#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------
plot(subrules, method = "graph", asEdges = TRUE, limit = n_valid_lhs_rhs)
plot(subrules, method = "graph", asEdges = TRUE, circular = FALSE, limit = n_valid_lhs_rhs)
#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------
plot(subrules, method = "graph", engine = "igraph", limit = n_valid_lhs_rhs)
plot(subrules, method = "graph", engine = "igraph",
     nodeCol = grey.colors(10), edgeCol = grey(.7), alpha = 1,
     limit = n_valid_lhs_rhs)
#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------

plot(subrules, method = "graph", engine = "igraph",
     plot_options = list(
       edge.lty = 2, 
       vertex.label.cex = .6, 
       margin = c(.1,.1,.1,.1), 
       asp = .5),
     limit = n_valid_lhs_rhs)

#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------
plot(subrules, method="graph", engine = "igraph", layout = igraph::in_circle(), limit = n_valid_lhs_rhs)
#-------------------------------------------------------
plot(subrules, method = "graph", engine = "graphviz", limit = n_valid_lhs_rhs)
#-------------------------------------------------------
if(interactive()) {
  # Default interactive plot (using igraph's tkplot)
  plot(subrules, method = "graph", engine = "interactive", limit = n_valid_lhs_rhs)
  
  # Interactive graph as a html widget (using igraph layout)
  plot(subrules, method = "graph", engine = "htmlwidget", limit = n_valid_lhs_rhs)
  plot(subrules, method = "graph", engine = "htmlwidget", 
       igraphLayout = "layout_in_circle", limit = n_valid_lhs_rhs)}
#-------------------------------------------------------
oneRule <- sample(rules, 1)
inspect(oneRule)
plot(oneRule, method = "doubledecker", data = Groceries)
#-------------------------------------------------------
p <- plot(subrules, engine = "html")
htmlwidgets::saveWidget(p, "arules.html", selfcontained = FALSE)

browseURL("arules.html")
# clean up
unlink(c("arules.html", "arules_files"), recursive = TRUE)











