#' ---
#' title: "%1$s"
#' author: "%2$s"
#' date: "%3$s"
#' ---
#' 
#+ message=F,echo=F
# init -----
# %1$s %2$s %3$s %4$s %5$s %6$s
.packages <- %4$s;
.deps <- %6$s;
debug <- 0;
if(debug>0) source('global.R') else {
  .junk<-capture.output(source('global.R',echo=F))};
if(length(.packages) > 1 || .packages != '') instrequire(.packages);
.currentscript <- "%5$s";
tself(scriptname=.currentscript);
.origfiles <- ls();
.loadedobjects <- c();
if(length(.deps)>1 || .deps != ''){
  for(ii in .deps){
    .depdata <- paste0(ii,'.rdata');
    if(!file.exists(.depdata)) {
      system(sprintf('R -e "source(\'%%s\')"',ii))};
    .loadedobjects <- union(.loadedobjects,tload(.depdata));
  }
}
#+ echo=F
#############################################################
# Your code goes below, content provided only as an example #
#############################################################

#' ### Data Characterization
#' 
#' Quality control, descriptive statistics, etc.

#+ echo=F
# characterization ----
pander(summary(dat0)); 

#' ### Data Analysis
#' 
#' Fitting the actual statistical models.
#+ echo=F
# analysis ----

#+ echo=F
#############################################################
# End of your code, start of boilerplate code               #
#############################################################

# save out with audit trail ----
# Saving original file-list so we don't keep exporting functions and 
# environment variables to other scripts. Could also replace .origfiles
# with the expression c(.origfiles,.loadedobjects) and then, nothing
# get inherited from previous files, only the stuff created in this 
# script gets saved. If this doesn't make sense, don't worry about it.
tsave(file=paste0(.currentscript,'.rdata'),list=setdiff(ls(),.origfiles)
      ,verbose=F);

#' ### Audit Trail
#+ echo=F
.wt <- walktrail();
pander(.wt[order(.wt$sequence),-5],split.tables=Inf,justify='left',missing='');
#+ echo=F,eval=F
c()
