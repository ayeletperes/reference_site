---
title: "Functional groups"
output:
  distill::distill_article:
    toc: true
    toc_float: true
    keep_md: yes
---



<div class="layout-chunk" data-layout="l-body">


</div>


## Sequence pre-processing

Summary statistics for each of the functional groups declared in the app.

The app includes the P1 and P11 naive datasets and the P4 non-naive dataset.

For P1 and P11 the following filtration criteria were applied:

* Functional sequence, no stop codons or frame shifts.
* Sequences which start from position 1 of the V gene.
* Sequences which didn't have gaps open (-) and didn't include any N's
* After changing into group annotations, sequences which had more than a single assignment in naive repertoires were remove.

The groups were created with similarity of 95% based on complete linkage and functional sequences and up to position 318.

## Functional groups

<div class="layout-chunk" data-layout="l-body">
<img src="index_files/figure-html5/unnamed-chunk-2-1.png" width="1000%" />

</div>



## Projects sequence depth

<div class="layout-chunk" data-layout="l-body">
![](index_files/figure-html5/unnamed-chunk-3-1.png)<!-- -->

</div>


```{.r .distill-force-highlighting-css}
```
