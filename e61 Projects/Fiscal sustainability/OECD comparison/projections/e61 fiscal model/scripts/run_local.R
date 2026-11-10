args<-commandArgs(FALSE);script<-sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
shiny::runApp('.',host='127.0.0.1',port=7461,launch.browser=FALSE)
