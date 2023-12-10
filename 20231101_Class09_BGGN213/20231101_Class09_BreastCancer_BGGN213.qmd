---
title: "November 1, 2023: Class 09 Breast Cancer"
author: "Savannah Bogus A69027475"
format: pdf
---

## Exploration of Data

```{r}
fna.data<-"WisconsinCancer.csv"
wisc.df<-read.csv("WisconsinCancer.csv",row.names=1)
```
We're going to delete the diagnosis since that's the answer we're actually looking for. 
```{r}
wisc.data<-wisc.df[,-1]
```

Now, we're going to make a diagnosis vector for later. 
```{r}
diagnosis<-as.factor(wisc.df$diagnosis)
```

# Q1

```{r}
dim(wisc.data)
```

There are 569 patient samples/observations in the dataset and 31 variables in the dataset (excluding diagnosis).

# Q2

```{r}
table(diagnosis)
```

There are 357 benign and 212 malignant diagnoses. 

# Q3

```{r}
colnames(wisc.data)
length(grep("_mean$",colnames(wisc.data)))
```
10 variables are suffixed with "_mean". 

```{r}
colMeans(wisc.data)
```
```{r}
apply(wisc.data,2,sd)
```
## Principle Component Analysis


```{r}

wisc.pr<-prcomp(wisc.data[,colnames(wisc.data)!="X"],scale=TRUE)
summary<-summary(wisc.pr)
summary
```

# Q4 

~44% of the variance is captured by the first PC. 

# Q5

You need PC1-3 to describe at least 70% of the variance in the original data. 

# Q6

You need PC1-PC7 to describe at least 90% of the variance in the original data. 

```{r}
biplot(wisc.pr)
```

What stands out to me is a lot of the pink variables are not very centered and a lot of the black variables are very centered. This plot is a mess, so w're going to make a better one below.

```{r}
plot(wisc.pr$x,col=diagnosis,
     xlab="PC1",ylab="PC2")
```

# Q8

```{r}
plot(wisc.pr$x[,1],wisc.pr$x[,3],col=diagnosis,
     xlab="PC1",ylab="PC3")
```
In general, these two plots seem similar in that they both have one more dense cluster and one less dense cluster that segregate the benign and malignant samples, although PC3 had a less dense cluster than PC2. There is also less overlap between red and black in the PC1 vs PC2 plot as opposed to the PC1 vs PC3 plot.

Now, we're going to move into ggplot.

```{r}
df<-as.data.frame(wisc.pr$x)
df$diagnosis<-diagnosis
library(ggplot2)

```

```{r}
ggplot(df)+
  aes(PC1, PC2, col=diagnosis)+
  geom_point()+
  scale_color_manual(values=c("M"="cyan3","B"="violet"))
```
Variance explained:
```{r}
pr.var<-wisc.pr$sdev^2
head(pr.var)
```

```{r}
pve<-pr.var/sum(pr.var)
pve
plot(pve,xlab="Principle Component",ylab="Proportion of Variance",ylim=c(0,1),type="o")
```
```{r}
#install.packages("factoextra")
library(factoextra)
fviz_eig(wisc.pr, addlabels = TRUE)
```

# Q9

```{r}
wisc.pr$rotation[,1]['concave.points_mean']
```
This tells us that 26% of the variance is due to concave.points_mean. 

## Hierarchal Clustering


```{r}
data.scaled<-scale(wisc.data)
data.dist<-dist(data.scaled)
wisc.hclust<-hclust(data.dist,method="complete")
```

# Q10

```{r}
plot(wisc.hclust)
abline(h=19,col="darkcyan",lty=2)
```
```{r}
wisc.hclust.clusters<-cutree(wisc.hclust,k=4)
table(wisc.hclust.clusters,diagnosis)
```
# Q11 

```{r}
wisc.hclust.clust<-cutree(wisc.hclust,k=10)
table(wisc.hclust.clust,diagnosis)
```

# Q12

```{r}
wisc.hclust.s<-hclust(data.dist,method="single")
plot(wisc.hclust.s)
```
Single certainly isn't my favorite because it looks very heavy on one side.

```{r}
wisc.hclust.a<-hclust(data.dist,method="average")
plot(wisc.hclust.a)
```
Average looks like it groups better than single, for sure, and potentially slightly worse than complete.

```{r}
wisc.hclust.w<-hclust(data.dist,method="ward.D2")
plot(wisc.hclust.w)
```

Either ward.D2 or complete look like they're the most all encompassing

## Combining methods

```{r}
d.dist<-dist(wisc.pr$x[,1:7])
wisc.pr.hclust<-hclust(d.dist,method="ward.D2")
plot(wisc.pr.hclust)
```

```{r}
grps<-cutree(wisc.pr.hclust,k=2)
table(grps,diagnosis)
g<-as.factor(grps)
levels(g)
g<-relevel(g,2)
levels(g)
plot(wisc.pr$x[,1:2],col=grps)
plot(wisc.pr$x[,1:2],col=diagnosis)
plot(wisc.pr$x[,1:2],col=g)
```
I edited the group level as I went, so it's all in one code chunk instead of spread out. 



# Q13 

```{r}
wisc.pr.hclust.clusters<-cutree(wisc.pr.hclust,k=2)
table(wisc.pr.hclust.clusters,diagnosis)
```
There's still a lot of overlap in our clusters between diagnoses. There's around 50 people here who have the opposite diagnosis to the majority in their cluster. 

# Q14 

```{r}
table(wisc.hclust.clusters,diagnosis)
table(wisc.pr.hclust.clusters,diagnosis)
```
It seems that the PCA has slightly better clustering. It also takes 4 clusters before PCA to get similar data to what can be done in 2 clusters after PCA. 

```{r}
table(diagnosis)
```
Sensitivity pre PCA=0.877
Specificit ypre PCA=1.07
Sensitivity post PCA=1.01
Specificity post PCA=.0989

After PCA, sensitivty and specificity are both closer to 1, indicating they're better.

## Prediction

```{r}
url<-"https://tinyurl.com/new-samples-CSV"
new<-read.csv(url)
npc<-predict(wisc.pr,newdata=new)
npc
```

```{r}
plot(wisc.pr$x[,1:2], col=g)
points(npc[,1], npc[,2], col="blue", pch=16, cex=3)
text(npc[,1], npc[,2], c(1,2), col="white")
```
# Q16

We should follow up with patient 2. 
