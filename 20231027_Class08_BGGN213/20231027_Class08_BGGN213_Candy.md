---
title: "October 27, 2023 Class 08 Halloween Candy Project"
author: "Savannah Bogus A69027475"
format: pdf
---

# Importing Candy Data

```{r}
read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/candy-power-ranking/candy-data.csv")
candy_file<-"candy_data.csv"
candy=read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/candy-power-ranking/candy-data.csv",row.names=1)
head(candy)
```

# Q1 How many different candy types are in this dataset?

```{r}
nrow(candy)
dim(candy)
```
 There are 85 candies in this dataset. 

```{r}
table(candy$fruity)
```
38 candies have a 1 in fruity, so 38 candies are fruity. 

## Favorite candy

# Q3 

```{r}
candy["Snickers",]$winpercent
```
The win percent value is 76.67%, which is wayyyy too low in my opinion for snickers. I cannot BELIEVE Twix is higher. Snickers is like elevated Twix. 

# Q4 and Q5

```{r}
candy["Kit Kat",]$winpercent
candy["Tootsie Roll Snack Bars",]$winpercent
```

Kit Kat=76.76 (HIGHER THAN SNICKERS? REALLY?)
Tootsie Roll Snack Bars=49.65

```{r}

library("skimr")
skim(candy)
```

# Q6 

win percent is on a different scale to the other columns, I believe.

# Q7

0=not chocolatey, 1=chocolatey. ie. Hershey's would have a 1 and Starbursts would have a 0. 

# Q8

```{r}
library(ggplot2)
ggplot(candy)+
       aes(winpercent)+
    geom_histogram()

```

# Q9

The distribution is not symmetrical.

# Q10

```{r}
mean(candy$winpercent)
```


The center of the distribution is just above 50%. 

# Q11 

```{r}
mean(candy$winpercent[as.logical(candy$chocolate)])

mean(candy$winpercent[as.logical(candy$fruity)])
```

The mean win percent for chocolate candy is higher than the mean win percent for fruity candy. 

# Q11 

```{r}
choc<-as.logical(candy$chocolate)
fruity<-as.logical(candy$fruity)

t.test(candy$winpercent[as.logical(candy$chocolate)],candy$winpercent[as.logical(candy$fruity)])
```

Yes, the difference is statistically significant

## Overall Candy Rankings

```{r}
order(candy$winpercent)
head(candy[order(candy$winpercent),],n=5)
tail(candy[order(candy$winpercent),],n=5)

```

# Q13

Least liked 5 candies are Nik L Nip (what on earth is that), boston baked beans (tell me this isn't actually baked beans and that it's a candy), chiclets, super bubble, and jawbusters.

# Q14

Top 5 candies are Reese's Peanut Butter cup, Reese's Miniatures (1, the miniatures ARE worse. 2, this is the same candy), Twix, Kit Kat, Snickers. 

# Q15

```{r}

ggplot(candy)+
  aes(winpercent, reorder(rownames(candy),winpercent))+
  geom_bar(stat="identity",fill="darkcyan")
 
```

Trying again with new colors that you prefer. (Except we're still using my faves)

```{r}
my_colors=rep("violet",nrow(candy))
my_colors[as.logical(candy$chocolate)]="brown"
my_colors[as.logical(candy$bar)]="darkcyan"
my_colors[as.logical(candy$fruity)]="pink"

ggplot(candy)+
  aes(winpercent, reorder(rownames(candy),winpercent))+
  geom_bar(stat="identity",fill=my_colors)
```
I'm watching you make this graph in class right now (I'm on Q23 in real time) and it looks like I did mine differently. But, I guess it still works. 

# Q17 and Q18 

Worst ranked chocolate candy is sixlets. The best ranked fruity candy is starbursts. 

## Taking a look at pricepercent

```{r}
library(ggrepel)

ggplot(candy)+
  aes(winpercent,pricepercent,label=rownames(candy))+
  geom_point(col=my_colors)+
  geom_text_repel(col=my_colors,size=1,max.overlaps=5)
```

# Q19

```{r}
order(candy$pricepercent)
cheap<-order(candy$pricepercent,decreasing=TRUE)
head(candy[cheap,c(11,12)],n=5)
tail(candy[cheap,c(11,12)],n=5)
```


Reese's miniatures (not as good as fullsized) is ranked very highly and is very cheap, by eye, but out of the 5 cheapest candies, Tootsie roll midgies (what are these) are the best and cheapest.

# Q20

Why is Nik L Nip expensive but also garbage. That's the least popular. Ring pops, Smarties, Pop Rocks, and Sugar Babies are also very very pricey, and apparently not all that great. (Except Ring pops are fantastic, or so they should be ranked)

# Q21

```{r}
ggplot(candy)+
  aes(pricepercent,reorder(rownames(candy),pricepercent))+
  geom_segment(aes(xend=0,yend=reorder(rownames(candy),pricepercent)),col="darkcyan")+
  geom_point()
```

## Exploring correlation structure

```{r}
library(corrplot)
cij<-cor(candy)
corrplot(cij,col=colorRampPalette(c("pink","violet","darkcyan"))(200))
```

I spent a good 7 minutes figuring out how to implement my color scheme. I had never heard of the `colorRampPalette()` function.

# Q22

Chocolate and fruity and pluribus and bars are negatively correlated. It's weird because I LOVE fruity chocolate (chocolate covered strawberries?? cmon) but I guess it's not very good in candy. 

# Q23

Winpercent and chocolate are positively correlated :( as well as chocolate and bar. 

## Principal Component Analysis

```{r}
pca<-prcomp(candy,scale=TRUE)
summary(pca)

```
```{r}
pca$rotation[,1]
```
```{r}
df_candy<-as.data.frame(pca$x)
ggplot(df_candy)+
  aes(PC1,PC2)+
  geom_point(col=my_colors)
```
I realize now that you wanted us to use a base R plot, but I started working in ggplot right away because usually you say "use base R OR use this package" so I wanted to start using the package right away. Please don't take off points.

Now, we're going to use ggrepel. 

```{r}
ggplot(df_candy)+
  aes(PC1,PC2)+
  geom_point(col=my_colors,size=candy$winpercent/100)+
  geom_text_repel(col=my_colors, size=2, max.overlaps=7,label=rownames(candy))+
  theme(legend.position="none")+
  labs(title="Halloween Candy PCA",
       subtitle="Colored by type: chocolate bar(dark cyan), chocolate other (brown), fruity(pink), other(black)",
       caption="Data from 538")
```
The interactive labeling seems like a great idea. I'm going to try that. However, when I try to render it, it doesn't work because I'm rendering a PDF. I'll just put my code in as the backtick code chunk and not as code to be read. 

`
library(plotly)

ggplotly(ggplot(df_candy)+
  aes(PC1,PC2)+
  geom_point(col=my_colors,label=rownames(candy))+
  geom_text_repel(col=my_colors, size=2, max.overlaps=7,label=rownames(candy))+
  theme(legend.position="none")+
  labs(title="Halloween Candy PCA",
       subtitle="Colored by type: chocolate bar(dark cyan), chocolate other (brown), fruity(pink), other(black)",
       caption="Data from 538"))
`
Now, we're going to look at the PCA loadings. 
```{r}
barplot(pca$rotation[,1],las=2,ylab="PC1 contribution",col="darkcyan")
```

# Q24

If you're chocolate, you're probably a bar, and if you're fruity, you're probably a hard pluribus. 
