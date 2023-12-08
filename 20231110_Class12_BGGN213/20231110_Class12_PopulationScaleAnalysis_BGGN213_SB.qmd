---
title: "November 10, 2023 Class 12 Population Scale Analysis"
author: "Savannah Bogus A69027475"
format: pdf
---

## Population Scale Analysis

```{r}
data<-read.table("rs8067378_ENSG00000172057.6.txt")
```

```{r}
summary(data)

```
```{r}
table(data$geno)
```

There are 108 of A/A genotype, 233 of A/G genotype, and 121 of G/G genotype. 


```{r}
library(dplyr)

AA<-data%>%
  filter(geno=="A/A")

AG<-data%>%
  filter(geno=="A/G")
GG<-data%>%
  filter(geno=="G/G")

boxplot(AA$exp,AG$exp,GG$exp,names=c("A/A","A/G","G/G"),col="cyan")
```
```{r}
median(AA$exp)
median(AG$exp)
median(GG$exp)
```

The median expression levels for the A/A genotype is 31.24847, 25.06486 for A/G, and 20.07363 for the G/G genotype. Based on my boxplot, that looks correct. 

# Q14

This SNP does affect the expression of ORMDL3, with A/A having a higher expression level, G/G having a lower expression level, and A/G being somewhere in between. 


