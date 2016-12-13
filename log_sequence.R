# Created March 2016 by Schneider.DW@Gmail.com.
# A function for creating a logarithmic sequence in R. Similar to seq() can create by interval or by set length.

log.seq.r <- function(from,to,n,method) if(method=="by") round(n^seq(log(from,n),log(to,n))) else if(method=="length") round(exp(seq(log(from),log(to),length.out=n))) else stop("Third argument not acceptable. Method 'by' or 'length' needed.")
