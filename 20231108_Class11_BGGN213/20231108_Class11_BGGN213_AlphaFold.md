---
title: "Novembe 08, 2023 Class 11 Alpha Fold"
author: "Savannah Bogus"
format: pdf
---

AlphaFold2 stuff! AF

My results from AF live in the folder/directory `hivprodimer_23119`

```{r}
results_dir<-"hivprdimer_23119"

pdb_files<-list.files(results_dir, pattern=".pdb", full.names=T)
```
Oh boy, gang it looks like we'er doing align and superimpose using `pdbaln()`

```{r}
library(bio3d)
pdbs<-pdbaln(pdb_files,fit=TRUE,exefile="msa")
```

## The RMSD matrix

A common measure of structural dis-similarity is called RMSD (root mean square distance).

```{r}
rd<- rmsd(pdbs)
rd
```




```{r}
library(pheatmap)

rownames(rd)<-paste0("m",1:5)
colnames(rd)<-paste0("m",1:5)
pheatmap(rd)
```


Let's view this in Mol*

```{r}
xyz<-pdbfit(pdbs,outfile="fitted")
```

A full atom based fitting or superposition did not work very well because we have multiple chains that are in different conformations. 

We want to focus our superposition on the most invariant part. (That's the "core", so to say)

```{r}
core<- core.find(pdbs)
```

```{r}
core.inds<-core
```

```{r}
xyz<-pdbfit(pdbs,inds=core.inds,outpath="core_fitted")
```

To evaluate how good our multi-chain or multi-domain models are, we need to look at the PAE scores (predicted alignment error)

There are output as JSON format files. Let's find all their file names:

```{r}
pae_files<-list.files(results_dir,pattern="000.json",full.names=T)
pae_files
```
```{r}
library(jsonlite)

pae1<- read_json(pae_files[1],simplifyVector=TRUE)
attributes(pae1)
```
 
```{r}
pae1$max_pae
```

```{r}
pae5<- read_json(pae_files[5],simplifyVector=TRUE)
pae5$max_pae
```

The 5th has 2x the error that the 1st one has.

```{r}
plot.dmat(pae5$pae,
          xlab="Residue No.",
          ylab="Residue No.",
          zlim=c(0,30))
```

This plot above shows low error in the first chain and high error in the second chain.

We could put this in an `apply()` function to read all 5 pae for each of the alignments. 

```{r}
plot.dmat(pae1$pae,
          xlab="Residue No.",
          ylab="Residue No.",
          zlim=c(0,30))
```

Note that this has far less red, which indicates less error. I also added zlim to limit the z to be plotting the same axis size (or intensity/error size)

We can run AF on google compute infrastructure which is very very helpful.
We can read these results into R, and process to help make sense of the results in R, which would help us make sense of the PLDDT and PAE scores. 




