## ---- echo=FALSE, message=FALSE------------------------------------------
library(ggplot2)
library(ggnetwork)

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("ggnetwork")

## ---- eval=FALSE---------------------------------------------------------
#  devtools::install_github("briatte/ggnetwork")

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("ggplot2")
#  library(ggplot2)

## ---- message=FALSE------------------------------------------------------
library(network)
library(sna)
n <- network(rgraph(10, tprob = 0.2), directed = FALSE)

## ------------------------------------------------------------------------
n %v% "family" <- sample(letters[1:3], 10, replace = TRUE)
n %v% "importance" <- sample(1:3, 10, replace = TRUE)

## ------------------------------------------------------------------------
e <- network.edgecount(n)
set.edge.attribute(n, "type", sample(letters[24:26], e, replace = TRUE))
set.edge.attribute(n, "day", sample(1:3, e, replace = TRUE))

## ------------------------------------------------------------------------
theme_blank

## ---- eval=FALSE---------------------------------------------------------
#  ggnetwork(n, layout = "fruchtermanreingold", cell.jitter = 0.75)
#  ggnetwork(n, layout = "target", niter = 100)

## ------------------------------------------------------------------------
head(ggnetwork(n))

## ------------------------------------------------------------------------
tail(ggnetwork(n))

## ---- eval=FALSE---------------------------------------------------------
#  ggplot(n)

## ---- eval=FALSE---------------------------------------------------------
#  ggplot(ggnetwork(n))

## ------------------------------------------------------------------------
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = type), color = "grey50") +
  theme_blank()

## ------------------------------------------------------------------------
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = type), color = "grey50", curvature = 0.1) +
  theme_blank()

## ------------------------------------------------------------------------
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = type), color = "grey50") +
  geom_nodes(aes(color = family, size = importance)) +
  theme_blank()

## ---- eval=FALSE---------------------------------------------------------
#  ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
#    geom_edges(aes(color = type)) +
#    geom_nodes(aes(color = family)) +
#    theme_blank()

## ------------------------------------------------------------------------
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black") +
  geom_nodes(color = "black", size = 8) +
  geom_nodetext(aes(color = family, label = LETTERS[ vertex.names ]),
                fontface = "bold") +
  theme_blank()

## ------------------------------------------------------------------------
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black") +
  geom_nodelabel(aes(color = family, label = LETTERS[ vertex.names ]),
                 fontface = "bold") +
  theme_blank()

## ------------------------------------------------------------------------
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black") +
  geom_nodelabel_repel(aes(color = family, label = LETTERS[ vertex.names ]),
                       fontface = "bold", box.padding = unit(1, "lines")) +
  geom_nodes(color = "black", size = 8) +
  theme_blank()

## ------------------------------------------------------------------------
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = type), color = "grey75") +
  geom_nodes(color = "gold", size = 8) +
  geom_nodetext(aes(label = LETTERS[ vertex.names ])) +
  geom_edgetext(aes(label = day), color = "white", fill = "grey25") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "grey25"),
        panel.grid = element_blank())

## ------------------------------------------------------------------------
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = type), color = "grey75") +
  geom_nodes(color = "gold", size = 8) +
  geom_nodetext(aes(label = LETTERS[ vertex.names ])) +
  geom_edgetext_repel(aes(label = day), color = "white", fill = "grey25",
                      box.padding = unit(1, "lines")) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "grey25"),
        panel.grid = element_blank())

## ------------------------------------------------------------------------
data(emon)
emon[[1]]

## ---- echo=FALSE---------------------------------------------------------
ggplot(emon[[1]], aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_nodes(color = "tomato", size = 4) +
  theme_blank()

## ------------------------------------------------------------------------
ggplot(emon[[1]], aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed")) +
  geom_nodes(color = "tomato", size = 4) +
  theme_blank()

## ---- eval=FALSE---------------------------------------------------------
#  ggnetwork(emon[[1]], weights = "Frequency")

## ------------------------------------------------------------------------
ggplot(ggnetwork(emon[[1]], arrow.gap = 0.04, by = "Frequency"),
       aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed"),
             aes(color = Sponsorship)) +
  geom_nodes(aes(color = Sponsorship), size = 4) +
  facet_wrap(~ Frequency) +
  theme_facet()

## ------------------------------------------------------------------------
theme_facet

## ------------------------------------------------------------------------
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = type), color = "grey50") +
  geom_nodes(aes(x, y, color = family, size = 1.5 * importance)) +
  geom_nodetext(aes(label = LETTERS[ vertex.names ], size = 0.5 * importance)) +
  geom_edgetext(aes(label = day), color = "grey25") +
  scale_color_brewer(palette = "Set2") +
  scale_size_area("importance", breaks = 1:3, max_size = 9) +
  theme_blank()

## ------------------------------------------------------------------------
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50", alpha = 0.5) +
  geom_nodes(aes(x, y, color = family, size = 5.5 * importance), alpha = 0.5) +
  geom_nodes(aes(x, y, color = family, size = 1.5 * importance)) +
  scale_color_brewer(palette = "Set1") +
  guides(size = FALSE) +
  theme_blank()

## ------------------------------------------------------------------------
ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50", alpha = 0.5) +
  geom_nodes(aes(x, y, color = family), size = 3) +
  geom_nodelabel_repel(aes(label = vertex.names),
                       box.padding = unit(1, "lines"),
                       data = function(x) { x[ x$family == "a", ]}) +
  scale_color_brewer(palette = "Set1") +
  theme_blank()

## ---- results='asis', echo=FALSE-----------------------------------------
cat("Last printed on ", gsub("\\s+", " ", format(Sys.time(), "%b %e, %Y")), 
    ", using ggnetwork version ", as.character(packageVersion("ggnetwork")),
    ".", sep = "")

