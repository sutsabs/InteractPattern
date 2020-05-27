
# ---- C1 ----
# Load Libraries
library(ggplot2)
library(GGally)
library(igraph)
library(ergm)
library(scales)
library(dplyr)
library(network)
library(latticeExtra)
library(intergraph)
library(sand)
library(png)
#getwd()

source('lab2-utils.R')
source('invlogit.R')
source('mycugtest.R')
source('myqaptest.R')

# ---- C2 ----
#import nodes and edges for day 1 and day 2
links_day1 <- read.csv("edgelist.csv", stringsAsFactors=F)
nodes_day1 <- read.csv("nodelist.csv", stringsAsFactors=F)
links_day2 <- read.csv("edgelist-1.csv", stringsAsFactors=F)
nodes_day2 <- read.csv("nodelist-1.csv", stringsAsFactors=F)
# ---- C3 ----
# create graph file
day1 <- graph.data.frame(links_day1, vertices=nodes_day1, directed=F)
day2 <- graph.data.frame(links_day2, vertices=nodes_day2, directed=F)
# ---- C4 ----
summary(day1)
summary(day2)
summary(E(day1)$Weight)
summary(E(day2)$Weight)
# ---- C5 ----
degr <- read.csv("grddeg.csv") #degree by grade table
degr$Day <- as.factor(degr$Day)
degr$Gender <- as.factor(degr$Gender)
# ---- C6 ----
g <- ggplot(degr, aes(x=Label, y=Degree,fill=Day ))
g <- g + geom_boxplot(outlier.color = 'red')
g <- g + xlab("Grade")+ylab("Degree") +ggtitle("Degree distribution by Grade")
print(g)
g <- ggplot(degr, aes(x=Gender, y=Degree,fill=Day ))
g <- g + geom_boxplot(outlier.color = 'red')
g <- g + xlab("Gender")+ylab("Degree") +
  ggtitle("Degree distribution by Gender")+ theme_bw()
print(g)
ggplot(degr, aes(y=Degree, x=Label)) +
  geom_col(aes(fill = Day), width =0.4,position = "dodge") +
  xlab("Grades")+ylab("Number of Contacts") +ggtitle("Total Interactions by Different Grade Students")

# ---- C7 ----
# distribution of number of contacts by grades heatmap
dat <- read.csv("dat.csv", header=TRUE,sep=",",stringsAsFactors=F)
# plot
p <- ggplot(dat, aes(x = x, y = y)) + geom_bin2d(bins = 10)
# Get data - this includes counts and x,y coordinates
newdat <- ggplot_build(p)$data[[1]]
# add in text labels
p + geom_text(data=newdat, aes((xmin + xmax)/2, (ymin + ymax)/2,
                               label=count), col="white")+
  scale_fill_gradientn(limits=c(0,300), breaks=seq(0, 300, by=50),colors=c(high = "#132B43", low = "#56B1F7"))+
  scale_x_discrete(position = "top") +xlab("Grades")+ylab("Grades")+
  theme_bw()
# ---- C8 ----
# Delete edges with weight less than 1
edges_d1 <- E(day1)[E(day1)$Weight < 1]
edges_d2 <- E(day2)[E(day2)$Weight < 1]
day1 <- delete_edges(day1, edges_d1)
day2 <- delete_edges(day2, edges_d2)
# Delete isolate degree
day1 <- delete_vertices(day1,V(day1)[degree(day1)==0])
day2 <- delete_vertices(day2,V(day2)[degree(day2)==0])


## add grade and age attribute
ageTable <- data.frame(Grade=c("1A", "1B", "2A", "2B", "3A", "3B", "4A", "4B", "5A","5B","Teachers"),
                       Age=c(1,1,2,2,3,3,4,4,5,5,20))
grade_1 <- data.frame(Grade=as.factor(V(day1)$X0))
grade_2 <- data.frame(Grade=as.factor(V(day2)$X0))

grade_1 <- grade_1 %>% 
  inner_join(ageTable, by ="Grade")

grade_2 <- grade_2 %>% 
  inner_join(ageTable, by ="Grade")


day_1<-set_vertex_attr(day1, "Age", index = V(day1),value=as.numeric(grade_1$Age))
V(day1)$Age <- as.numeric(grade_1$Age)

day_1<-set_vertex_attr(day1, "Grade", index = V(day1),value=as.character(grade_1$Age))
V(day1)$Grade <- as.character(grade_1$Age)
V(day1)$Grade[V(day1)$Grade=="10"]="Teacher"

day_2<-set_vertex_attr(day2, "Age", index = V(day2),value=as.numeric(grade_2$Age))
V(day2)$Age <- as.numeric(grade_2$Age)

day2<-set_vertex_attr(day2, "Grade", index = V(day2),value=as.character(grade_2$Age))
V(day2)$Grade <- as.character(grade_2$Age)
V(day2)$Grade[V(day2)$Grade=="10"]="Teacher"



day1.deg <- data.frame(Degree=graph.strength(day1), Grade=V(day1)$Grade)
day1.deg <- day1.deg[day1.deg$Grade != 20,]
day1.deg$Grade <- as.factor(day1.deg$Grade)

day2.deg <- data.frame(Degree=graph.strength(day2), Grade=V(day2)$Grade)
day2.deg <- day2.deg[day2.deg$Grade != 20,]
day2.deg$Grade <- as.factor(day2.deg$Grade)


# ---- C9 ----
# Degree distribution histogram
day1.deg <- data.frame(Degree=graph.strength(day1), Sex=V(day1)$X1)
day1.deg <- day1.deg[day1.deg$Sex != "Unknown",]
day1.deg$Sex <- as.factor(day1.deg$Sex)


g <- ggplot(data=day1.deg, aes(x=Degree))
g <- g + geom_histogram(breaks=seq(0,25, by=1), col="black",aes(fill=Sex)) 
g <- g + scale_fill_manual("Grade", values = c("pink","darkslateblue")) + facet_wrap(~Sex,ncol = 1)
g <- g + ggtitle ("Distribution of Weighted Degree by Gender") + theme_bw()
print(g)


day2.deg <- data.frame(Degree=graph.strength(day2), Sex=V(day2)$X1)
day2.deg <- day2.deg[day2.deg$Sex != "Unknown",]
day2.deg$Sex <- as.factor(day2.deg$Sex)


g <- ggplot(data=day2.deg, aes(x=Degree))
g <- g + geom_histogram(breaks=seq(0,25, by=1), col="black",aes(fill=Sex)) 
g <- g + scale_fill_manual("Grade", values = c("pink","darkslateblue")) + facet_wrap(~Sex,ncol = 1)
g <- g + ggtitle ("Distribution of Weighted Degree by Gender") + theme_bw()
print(g)

# ---- C10 ----
## connected

is.connected(day1)
is.connected(day2)

is.simple(day1)
is.simple(day2)

diameter(day1)
diameter(day2)

# ---- C11 ----
# ggplot boxplot
g <- ggplot(day1.deg, aes(x=Sex, y=Degree ))
g <- g + geom_boxplot()+ggtitle("Boxplot Degree by Gender Day1")+theme_bw()
print(g)

g <- ggplot(day2.deg, aes(x=Sex, y=Degree ))
g <- g + geom_boxplot()+ggtitle("Boxplot Degree by Gender Day2")+theme_bw()
print(g)


# ---- C12 ----
# decompose into smaller communities
components <- decompose(day1, min.vertices = 2)
sapply(components,vcount)
giant_1 <- components[[1]]
summary(giant_1)

components <- decompose(day2, min.vertices = 2)
sapply(components,vcount)
giant_2 <- components[[1]]
summary(giant_2)

# ---- C13 ----
# Centrality measures
giant_1.dg <- degree(giant_1, normalized = TRUE)
giant_1.indg <-degree(giant_1, mode=c("in"),normalized = TRUE)
giant_1.outdg <-degree(giant_1, mode=c("out"),normalized = TRUE)
giant_1.wtdg <-graph.strength(giant_1)
giant_1.bet <- betweenness(giant_1, normalized =TRUE, weights = NULL)
giant_1.clo <- closeness(giant_1, normalized = TRUE,weights = NULL)
giant_1.pr <- page_rank(giant_1)
giant_1.eig <- eigen_centrality(giant_1, directed = FALSE)

giant_1.cent <- data.frame(
                         wtdg=giant_1.wtdg,
                         pr=giant_1.pr$vector,
                         eig=giant_1.eig$vector,
                         betw=giant_1.bet,
                         clo=giant_1.clo)


# ggcorr
ggcorr(giant_1.cent, label = TRUE)+ggtitle("Day 1 Centrality Measures Corrplot")



giant_2.dg <- degree(giant_2, normalized = TRUE)
giant_2.wtdg <-graph.strength(giant_2)
giant_2.bet <- betweenness(giant_2, normalized =TRUE,weights = NULL)
giant_2.clo <- closeness(giant_2, normalized = TRUE, weights = NULL)
giant_2.pr <- page_rank(giant_2)
giant_2.eig <- eigen_centrality(giant_2, directed = FALSE)

giant_2.cent <- data.frame(
                           wtdg=giant_2.wtdg,
                           pr=giant_2.pr$vector,
                           eig=giant_2.eig$vector,
                           betw=giant_2.bet,
                           clo=giant_2.clo)

# ggcorr
ggcorr(giant_2.cent, label = TRUE)+ggtitle("Day 2 Centrality Measures Corrplot")


# ---- C14 ----
# Betweeness/Closeness distribution
Grade <- factor(V(giant_1)$Grade)
giant_1.df <-data.frame(Betweeness=giant_1.bet,Closeness=giant_1.clo, Grade=Grade)
giant_1.df<-giant_1.df[giant_1.df$Grade!=20,]
g <- ggplot(data=giant_1.df,aes(x=Betweeness, fill=Grade))
g <- g + geom_histogram(position= "dodge", binwidth = .01, aes(y=..count..),color="black")
g <- g + scale_x_continuous(breaks = seq(0,0.5,by=0.01))
g <- g + scale_fill_manual("Grade", values = c("gray", "blue", "dark green","#990000", "light blue", "light green"))
g <- g + ggtitle ("Betweeness Distribution by Grade") + theme_bw()
print(g)

g <- ggplot(data=giant_1.df,aes(x=Closeness, fill=Grade))
g <- g + geom_histogram(position= "dodge", binwidth = .05, aes(y=..count..),color="black")
g <- g + scale_x_continuous(breaks = seq(0,0.5,by=0.05))
g <- g + scale_fill_manual("Grade", values = c("red", "darkslateblue", "dark green","#990000", "light blue", "light green", "orange", "yellow","purple","gray","plum"))
g <- g + ggtitle ("Closeness Distribution by Grade") + theme_bw()
print(g)

Grade <- factor(V(giant_2)$Grade)
giant_2.df <-data.frame(Betweeness=giant_2.bet,Closeness=giant_2.clo, Grade=Grade)
giant_2.df<-giant_2.df[giant_2.df$Grade!=20,]

g <- ggplot(data=giant_2.df,aes(x=Betweeness, fill=Grade))
g <- g + geom_histogram(position= "dodge", binwidth = .01, aes(y=..count..),color="black")
g <- g + scale_x_continuous(breaks = seq(0,0.5,by=0.01))
g <- g + scale_fill_manual("Grade", values = c("gray", "blue", "dark green","#990000", "light blue", "light green"))
g <- g + ggtitle ("Betweeness Distribution by Grade") + theme_bw()
print(g)

g <- ggplot(data=giant_2.df,aes(x=Closeness, fill=Grade))
g <- g + geom_histogram(position= "dodge", binwidth = .05, aes(y=..count..),color="black")
g <- g + scale_x_continuous(breaks = seq(0,0.5,by=0.05))
g <- g + scale_fill_manual("Grade", values = c("red", "darkslateblue", "dark green","#990000", "light blue", "light green", "orange", "yellow","purple","gray","plum"))
g <- g + ggtitle ("Closeness Distribution by Grade") + theme_bw()
print(g)

# ---- C15 ----
# ERGM Modelling
# Load saved Model files
par(mfcol=c(1,1))
## save(ga_1.net,ga_2.net,file="network-ga.Rdata")
## ga_1.net <- asNetwork(giant_1)
## ga_2.net <- asNetwork(giant_2)

load("network-ga.Rdata")
load("day1-model-ii.Rdata")

# ---- C16 ----
## ga_1.m1 <- ergm(ga_1.net ~ edges + nodemix("X1", base=c(-1,-2,-3)))
## ga_1.m1.gof <- gof(ga_1.m1)
summary(ga_1.m1)
plot(ga_1.m1.gof)

# ---- C17 ----
## ga_1.m2 <- ergm(ga_1.net ~ edges + nodemix("X1", base=c(-1,-2,-3))+
##                absdiff("Age"),
##                control=control.ergm(MCMC.burnin = 50000,
##                                     MCMC.interval = 5000))
## ga_1.m2.gof <- gof(ga_1.m2)
plot(ga_1.m2.gof)
summary(ga_1.m2)


# ---- C18 ----
## ga_1.m3 <- ergm(ga_1.net ~ edges + nodemix("X1", base=c(-1,-2,-3)) + 
##                degree(1) + absdiff("Age"),
##              control=control.ergm(MCMC.burnin = 50000,
##                                   MCMC.interval = 5000))
## ga_1.m3.gof <- gof(ga_1.m3)
summary(ga_1.m3)
plot(ga_1.m3.gof)

# ---- C19 ----
## ga_1.m4 <- ergm(ga_1.net ~ edges + nodemix("X1", base=c(-1,-2,-3)) + 
##                  degree(2:4) + absdiff("Age")+ gwesp(cutoff=7),
##                control=control.ergm(MCMC.burnin = 50000,
##                                     MCMC.interval = 5000))
## ga_1.m4.gof <- gof(ga_1.m4)
summary(ga_1.m4)
mcmc.diagnostics(ga_1.m4, center=F)
plot(ga_1.m4.gof)


# ---- C20 ----
## ga_1.m5 <- ergm(ga_1.net ~ edges + nodemix("X1", base=c(-1,-2,-3)) +
##                  nodematch("X0")+
##                  degree(1) + absdiff("Age")+ gwesp(cutoff=7),
##                control=control.ergm(MCMC.burnin = 50000,
##                                     MCMC.interval = 5000))
## ga_1.m5.gof <- gof(ga_1.m5)
summary(ga_1.m5)
mcmc.diagnostics(ga_1.m5)
plot(ga_1.m5.gof)

# ---- C21 ----
## ga_1.m6 <- ergm(ga_1.net ~ edges + nodemix("X1", base=c(-1,-2,-3)) +
##                  nodematch("X0")+degree(2:4)+degree(9),
##                control=control.ergm(MCMC.burnin = 100000,
##                                     MCMC.interval = 5000))
## ga_1.m6.gof <- gof(ga_1.m6)
summary(ga_1.m6)
mcmc.diagnostics(ga_1.m6)
plot(ga_1.m6.gof)

# ---- C22 ----
## ga_2.m1 <- ergm(ga_2.net ~ edges + nodemix("X1") +
##                  nodematch("X0"))

summary(ga_2.m1)


# ---- C23 ----

## ga_2.m2 <- ergm(ga_2.net ~ edges + nodemix("X1", base=c(-1,-2,-3)) +
##                absdiff("Age"),
##                control=control.ergm(MCMC.burnin = 50000,
##                                     MCMC.interval = 5000))
## ga_2.m2.gof <- gof(ga_2.m2)
summary(ga_2.m2)
plot(ga_2.m2.gof)


## ga_2.m3 <- ergm(ga_2.net ~ edges + nodemix("X1", base=c(-1,-2,-3)) + 
##                  nodematch("X0")+
##                  absdiff("Age") + degree(2:5)+degree(8)+degree(12),
##                control=control.ergm(MCMC.burnin = 100000,
##                                     MCMC.interval = 5000))

## ga_2.m3.gof <- gof(ga_2.m3)
summary(ga_2.m3)
plot(ga_2.m3.gof)
mcmc.diagnostics(ga_2.m3)

# ---- C24 ----
## ga_2.m4 <- ergm(ga_2.net ~ edges + nodematch("X1", diff = T) + 
##                  nodematch("X0",diff = T) + absdiff("Age"), 
##                  control=control.ergm(MCMC.burnin = 100000,MCMC.interval = 5000))
summary(ga_2.m4)
## ga_2.m4.gof <- gof(ga_2.m4)
plot(ga_2.m4.gof)

# ---- C25 ----
# ga_2.m5 <- ergm(ga_2.net ~ edges + nodematch("X1", diff = T) + 
#                  nodematch("X0") + absdiff("Age"), control=control.ergm(MCMC.burnin = 100000,
#                                                                                  MCMC.interval = 5000))

# ga_2.m5.gof <- gof(ga_2.m5)
summary(ga_2.m5)
plot(ga_2.m5.gof)

# ---- C26 ----
## ga_2.m6 <- ergm(ga_2.net ~ edges + nodematch("X1", diff = T) + 
##                   nodematch("X0") + absdiff("Age")+degree(1:5), 
##                 control=control.ergm(MCMC.burnin = 100000,MCMC.interval = 5000))
## ga_2.m6.gof <- gof(ga_2.m6)
summary(ga_2.m6)
mcmc.diagnostics(ga_2.m6)
plot(ga_2.m6.gof)
 
# ---- C27 ----
## ga_2.m7 <- ergm(ga_2.net ~ edges + nodematch("X1", diff = T) + 
##                  nodematch("X0") + absdiff("Age")+degree(1:3)+ gwesp(0.25, fixed = T))
## ga_2.m7.gof <- gof(ga_2.m7)

summary(ga_2.m7)
mcmc.diagnostics(ga_2.m7)
## plot(ga_2.m7.gof)


# ---- C28 ----
# CUG test
attr.sex <- as.factor(V(giant_1)$X0)
attr.grade <- as.factor(V(giant_1)$Grade)
assortativity(giant_1,types1 = attr.sex)
assortativity(giant_1,types1 = attr.grade)
sex.cug <- mycugtest(giant_1, assortativity, cmode = "edges",
                      types1=attr.sex)
grade.cug <- mycugtest(giant_1, assortativity, cmode = "edges",
                     types1=attr.grade)

plot.cug.test(sex.cug)
print.cug.test(sex.cug)
plot.cug.test(grade.cug)
print.cug.test(grade.cug)

# ---- C29 ----
# QAP test
sex.qap <- myqaptest(giant_1, assortativity, types1=attr.sex)
plot.qaptest(sex.qap)
summary.qaptest(sex.qap)

grade.qap <- myqaptest(giant_1, assortativity, types1=attr.grade)
plot.qaptest(grade.qap)
summary.qaptest(grade.qap)

# ---- C30 ----
edges <- c(1)
nodemixFF <- c(0,1,0,0)
nodemixFM <- c(1,0,0,0)
nodemixMM <- c(0,0,1,0)
nodematch.XO <-c(0,0,0,1)

cases <- c("F-M", "F-F", "M-M","Grade-Match")

cases.df <- data.frame(Case = cases, edges = edges,
                       nodemixFF,nodemixFM,nodemixMM,nodematch.XO)

logodds <- ga_1.m6$coef[1] * cases.df$edges +
  ga_1.m6$coef[2] * cases.df$nodemixFF +
  ga_1.m6$coef[3] * cases.df$nodemixFM +
  ga_1.m6$coef[4] * cases.df$nodemixMM +
  ga_1.m6$coef[5] * cases.df$nodematch.XO
cases.df$logodds <- logodds

cases.df$cond_prob <- invlogit(cases.df$logodds)
cases.df[order(cases.df$cond_prob),]




##save(ga_1.m1,ga_1.m1.gof,ga_1.m2,ga_1.m2.gof,ga_1.m3,ga_1.m3.gof,ga_1.m4,ga_1.m4.gof,
##     ga_1.m5,ga_1.m5.gof,ga_1.m6,ga_1.m6.gof,
##     ga_2.m1,ga_2.m2,ga_2.m2.gof,ga_2.m3,ga_2.m3.gof,ga_2.m4,ga_2.m4.gof,
##     ga_2.m5,ga_2.m5.gof,ga_2.m6,ga_2.m6.gof,ga_2.m7,ga_2.m7.gof, file = "day1-model-ii.Rdata")

