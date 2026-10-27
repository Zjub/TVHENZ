# Backwards-compatible validation entry point. v1 tests are in archive/v1/tests.
args<-commandArgs(FALSE)
script<-sub('^--file=','',args[grepl('^--file=',args)][1])
source(file.path(dirname(script),'validate_update.R'),encoding='UTF-8')
