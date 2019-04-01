#+ message=F,echo=F
debug <- 0;
if(debug>0) source('global.R') else {
  .junk<-capture.output(source('global.R',echo=F))};
.depends <- 'dictionary.R';
.depdata <- paste0(.depends,'.rdata');
.currentscript <- parent.frame(2)$ofile;
if(is.null(.currentscript)) .currentscript <- 'RUN_FROM_INTERACTIVE_SESSION';
tself(scriptname=.currentscript);
if(!file.exists(.depdata)) system(sprintf('R -e "source(\'%s\')"',.depends));
.loadedobjects <- tload(.depdata);

dat0 <- read.xlsx(inputdata00,startRow = 6,
check.names = T);
(names(dat0));
dct0 <- data.frame(origname=names(dat0),
                  class=sapply(dat0,class));
#' These are the OTU names from the xlsx sheet
dat0$Names.in.OTU.table;
#' These are the names from fasta second genome sheet
#' The first `r length(grep('dnOTU_',names(fasta), value = T))` start with dnOTU_
#' The last `r length(grep('t__',names(fasta), value = T))` start with t__
head(names(fasta), 30);
tail(names(fasta));
c()