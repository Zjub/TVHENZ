# Render the README faithfully, then print via local Chrome (no network sources).
args <- commandArgs(FALSE)
script <- sub('^--file=','',args[grepl('^--file=',args)][1])
setwd(normalizePath(file.path(dirname(script),'..'),mustWork=TRUE))
markdown <- paste(readLines('README.md',encoding='UTF-8',warn=FALSE),collapse='\n')
body <- commonmark::markdown_html(markdown,extensions=TRUE)
html <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8">',
  '<title>e61 fiscal model - README</title><style>',
  '@page{size:A4;margin:18mm 18mm 20mm}body{font-family:Arial,sans-serif;color:#183343;',
  'font-size:10pt;line-height:1.48}h1{font-size:23pt;line-height:1.15;color:#007e80;',
  'margin:0 0 18pt}h2{font-size:14pt;margin:22pt 0 8pt;color:#007e80;',
  'break-after:avoid}p{margin:7pt 0}a{color:#007e80;text-decoration:none}',
  'pre{white-space:pre-wrap;overflow-wrap:anywhere;font-size:8pt;padding:10pt;',
  'background:#f1f5f4;border-left:3px solid #007e80;break-inside:avoid}',
  'code{font-family:Consolas,monospace;font-size:.9em;overflow-wrap:anywhere}',
  'li{margin:4pt 0}ul,ol{padding-left:20pt}strong{color:#122b3f}',
  'h1,h2,h3{break-inside:avoid}h3{font-size:11pt;break-after:avoid}p,li{orphans:3;widows:3}',
  'table{width:100%;border-collapse:collapse;font-size:8.3pt;line-height:1.3;margin:12pt 0}',
  'th,td{padding:6pt;border:1px solid #d5dfdc;text-align:left;vertical-align:top}',
  'th{background:#eaf2ef}thead{display:table-header-group}tr{break-inside:avoid}',
  '</style></head><body>',body,'</body></html>')
dir.create('outputs',showWarnings=FALSE)
writeLines(enc2utf8(html),'outputs/README_print.html',useBytes=TRUE)
Sys.setenv(CHROMOTE_CHROME='C:/Program Files/Google/Chrome/Application/chrome.exe')
browser <- chromote::ChromoteSession$new()
frame <- browser$Page$getFrameTree()$frameTree$frame$id
browser$Page$setDocumentContent(frameId=frame,html=html)
ready <- FALSE
for (attempt in 1:50) {
  ready <- isTRUE(browser$Runtime$evaluate(
    "document.readyState === 'complete' && document.body.innerText.includes('Discussion questions')",
    returnByValue=TRUE)$result$value)
  if (ready) break
  Sys.sleep(0.1)
}
if (!ready) {
  print(browser$Runtime$evaluate("JSON.stringify({state:document.readyState,text:document.body.innerText.slice(0,200),length:document.body.innerText.length,tail:document.body.innerText.slice(-200)})",returnByValue=TRUE))
  browser$close(); stop('README HTML did not finish loading')
}
browser$Runtime$evaluate('document.fonts.ready.then(()=>true)',awaitPromise=TRUE)
pdf <- browser$Page$printToPDF(printBackground=TRUE,preferCSSPageSize=TRUE)
writeBin(jsonlite::base64_dec(pdf$data),'README.pdf')
browser$close()
info <- pdftools::pdf_info('README.pdf')
text <- paste(pdftools::pdf_text('README.pdf'),collapse='\n')
stopifnot(info$pages>0,grepl('Discussion questions',text,fixed=TRUE),
          grepl('Starting data and calibration',text,fixed=TRUE),
          grepl('NGDP',text,fixed=TRUE))
cat('Created README.pdf:',info$pages,'pages. Verified key sections and readable text.\n')
