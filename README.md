# Bam2Bigwig_Rshiny

## DESCRIPTION


Allows the transformation of a Bam file into a Bigwig file. The app is based on "bamcoverage" of the Deeptools tool.
At the top of the pages a link to the bamcoverage tutorial is available.
Options are available to the user to allow adaptation of the output file to his needs. 
App is divided into two parts giving access to a simple conversion (unstranded) or specific to each strand (stranded).
On the stranded page, two files are downloadable, one per strand, and, on the unstranded page, only one corresponding to the complete bigwig.
Note that the app will use half of the threads (core) of your PC.
In addition, it is not possible to launch several conversions simultaneously or to load several BAM files,
conversions must be done one by one.

## SOFTWARE AND PACKAGES

R version 4 and packages : 

    - shiny

    - shinydashboard

    - rtracklayer

    - BiocManager

    - Parallel
    

For install in R4: 

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("rtracklayer")

install.packages(c("shiny", "shinydashboard", "parallel"))
```

## PRESENTATION OF THE USER INTERFACE (UI)

### Options

At the top of pages, you can find all the converting options : 

0 - Only for the Stranded page, give the choice of stranded methods / Library type 

1 - Here you can choose one normalization strategy (RPKM, CPM, BPM or RPGC). It is also possible to define a
precise normalization factor.
If you dont want to use one of this normalization strategy, you can give a specific factor, the calculation carried out will be :
bigwig file * (factor / number of read)

note : 
RPKM = Reads Per Kilobase per Million mapped reads; 
CPM = Counts Per Million mapped reads, same as CPM in RNA-seq; 
BPM = Bins Per Million mapped reads, same as TPM in RNA-seq; 
RPGC = reads per genomic content (1x normalization)

2 - ignoreForNormalization option of bamcoverage : A list of space-delimited chromosome names containing those chromosomes 
that should be excluded for computing the normalization. This is useful when considering samples with unequal 
coverage across chromosomes, like male samples.

3 - Exact scaling option of bamcoverage : Instead of computing scaling factors based on a sampling of the reads, 
process all of the reads to determine the exact number that will be used in the output. 
This requires significantly more time to compute, but will produce more accurate scaling factors in cases where 
alignments that are being filtered are rare and lumped together. In other words, this is only needed when 
region-based sampling is expected to produce incorrect results.

4 - binSize option of bamcoverage : Size of the bins, in bases, for the output of the bigwig/bedgraph file.

5 - region option of bamcoverage : Region of the genome to limit the operation to - this is useful when testing parameters to reduce the computing time. 
The format is chr:start:end, for example –region chr10 or –region chr10:456700:891000.

6 - skipNonCoveredRegions of bamcoverage : This parameter determines if non-covered regions (regions without overlapping reads) 
in a BAM file should be skipped. The default is to treat those regions as having a value of zero. 
The decision to skip non-covered regions depends on the interpretation of the data. 
Non-covered regions may represent, for example, repetitive regions that should be skipped.

### UPLOAD/CONVERSION/DOWNLOAD

At the bottom of the page you can find the conversion pannel : 

7 - You can upload your BAM file here, wait the message "Upload complete" to continue.

8 - You can give a specific name at your bigwig file(s). If you used the stranded page, the words "reverse" and "forward
are automaticaly added to your file name. 

9 - This is the button of conversion "BAM TO BIGWIG". Make sure you have chosen options you need before clicking on them. 

10/11 - Download button. Only one for the Unstranded page and two for stranded (one by strand). 

## ACKNOWLEDGEMENT

- **deepTools2: a next generation web server for deep-sequencing data analysis :** _"Fidel Ramírez, Devon P Ryan, Björn Grüning, Vivek Bhardwaj, Fabian Kilpert, Andreas S Richter, Steffen Heyne, Friederike Dündar, Thomas Manke, deepTools2: a next generation web server for deep-sequencing data analysis, Nucleic Acids Research, Volume 44, Issue W1, 8 July 2016, Pages W160–W165, https://doi.org/10.1093/nar/gkw257"_

- **Samtools : a suite of programs for interacting with high-throughput sequencing data :** _"Danecek P, Bonfield JK, Liddle J, Marshall J, Ohan V, Pollard MO, Whitwham A, Keane T, McCarthy SA, Davies RM, Li H, Twelve years of SAMtools and BCFtools, GigaScience (2021) 10(2) giab008 [33590861]"_
