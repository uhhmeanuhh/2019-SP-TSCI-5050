dat0 <- read.xlsx(inputdata00,startRow = 6,
check.names = T);
(names(dat0));
dct0 <- data.frame(origname=names(dat0),
                  class=sapply(dat0,class));
c()