AminaTemplate <- function(file, template='Template.R'
                          ,deps=c('dictionary.R'), packages=c()
                          ,title='TITLE', author='AUTHOR'
                          ,date=Sys.Date())
out <- sprintf(readLines(template)
               ,title # The title that will appear in the header
               ,author # Author, ditto
               ,format(date,'%d/%m/%Y')) # Date, ditto
file # So file knows its own name
map0 <- t_autoread(inputdatasec, file_args=file_args);
browser()
