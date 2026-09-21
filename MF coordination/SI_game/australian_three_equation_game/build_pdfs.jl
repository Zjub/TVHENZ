#!/usr/bin/env julia

# Convert the Markdown documentation to PDFs using Julia's Markdown standard
# library and a locally installed Chromium-family browser. Display equations are
# left for MathJax to typeset before the browser prints each document.

using Markdown


const ROOT = @__DIR__
const DOCUMENTS = [
    "README.md" => "README.pdf",
    "MODEL_NOTES.md" => "MODEL_NOTES.pdf",
    "THREE_EQUATION_MODEL.md" => "THREE_EQUATION_MODEL.pdf",
]

const CSS = """
@page { size: A4; margin: 18mm 17mm 20mm 17mm; }
* { box-sizing: border-box; }
html { font-size: 10.5pt; }
body {
  color: #17212b;
  font-family: Georgia, "Times New Roman", serif;
  line-height: 1.46;
  margin: 0 auto;
  max-width: 178mm;
}
h1, h2, h3 { color: #123f60; font-family: Arial, Helvetica, sans-serif; }
h1 { border-bottom: 2px solid #2f6f97; font-size: 22pt; margin: 0 0 8mm; padding-bottom: 3mm; }
h2 { border-bottom: 1px solid #b9cbd8; font-size: 15.5pt; margin: 8mm 0 3mm; padding-bottom: 1mm; }
h3 { font-size: 12pt; margin: 5mm 0 2mm; }
p { margin: 0 0 3mm; }
a { color: #145f8c; text-decoration: none; }
code {
  background: #eef3f6;
  border-radius: 2px;
  font-family: Consolas, "Courier New", monospace;
  font-size: 9pt;
  padding: 0.2mm 0.7mm;
}
pre {
  background: #eef3f6;
  border-left: 3px solid #5b8ba8;
  line-height: 1.35;
  overflow-wrap: anywhere;
  padding: 3mm;
  white-space: pre-wrap;
}
pre code { padding: 0; }
table { border-collapse: collapse; font-size: 9.1pt; margin: 4mm 0 5mm; width: 100%; }
thead { background: #dfeaf0; }
th, td { border: 1px solid #9fb5c3; padding: 1.7mm 2mm; text-align: left; vertical-align: top; }
tr { break-inside: avoid; }
ul, ol { margin: 1.5mm 0 4mm 6mm; padding-left: 5mm; }
li { margin: 1mm 0; }
.equation { font-size: 112%; margin: 4mm auto; overflow: hidden; text-align: center; }
.source-note {
  border-top: 1px solid #b9cbd8;
  color: #52616b;
  font-family: Arial, Helvetica, sans-serif;
  font-size: 8pt;
  margin-top: 10mm;
  padding-top: 2mm;
}
blockquote { border-left: 3px solid #9fb5c3; color: #3e4d56; margin-left: 0; padding-left: 4mm; }
"""


"""Locate Edge, Chrome or Chromium, with an optional explicit command-line path."""
function find_browser()
    explicit_index = findfirst(==("--browser"), ARGS)
    if !isnothing(explicit_index)
        explicit_index < length(ARGS) || error("--browser requires a path")
        path = abspath(ARGS[explicit_index + 1])
        isfile(path) || error("Browser not found: $path")
        return path
    end

    candidates = [
        raw"C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe",
        raw"C:\Program Files\Microsoft\Edge\Application\msedge.exe",
        raw"C:\Program Files\Google\Chrome\Application\chrome.exe",
        raw"C:\Program Files (x86)\Google\Chrome\Application\chrome.exe",
    ]
    for path in candidates
        isfile(path) && return path
    end
    for command in ["chromium", "chromium-browser", "google-chrome", "msedge"]
        path = Sys.which(command)
        !isnothing(path) && return path
    end
    error("No Edge, Chrome or Chromium executable was found")
end


"""
Render Markdown to HTML while protecting display LaTeX from Markdown emphasis
parsing. MathJax receives the equations after the Markdown conversion.
"""
function markdown_html(path::AbstractString)
    source = read(path, String)
    equations = String[]
    protected = replace(source, r"(?s)\\\[(.*?)\\\]" => function (matched)
        push!(equations, String(matched))
        return "\n\nMATHPLACEHOLDER$(length(equations))\n\n"
    end)

    parsed = Markdown.parse(protected)
    buffer = IOBuffer()
    show(buffer, MIME("text/html"), parsed)
    body = String(take!(buffer))
    for (index, equation) in enumerate(equations)
        body = replace(
            body,
            "<p>MATHPLACEHOLDER$(index)</p>" =>
                "<div class=\"equation\">$(equation)</div>",
        )
    end

    # Cross-document links should continue to work in the PDF copies.
    body = replace(body, "THREE_EQUATION_MODEL.md" => "THREE_EQUATION_MODEL.pdf")
    body = replace(body, "MODEL_NOTES.md" => "MODEL_NOTES.pdf")
    body = replace(body, "README.md" => "README.pdf")
    return body
end


function file_url(path::AbstractString)
    normalised = replace(abspath(path), '\\' => '/')
    escaped = replace(normalised, " " => "%20", "#" => "%23")
    return "file:///" * escaped
end


function render_pdf(
    source::AbstractString,
    destination::AbstractString,
    browser::AbstractString,
)
    body = markdown_html(source)
    title = replace(splitext(basename(source))[1], '_' => ' ')
    document = """<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>$title</title>
  <style>$CSS</style>
  <script>
    MathJax = { tex: { inlineMath: [['\$', '\$']], displayMath: [['\\\\[','\\\\]']] } };
  </script>
  <script defer src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js"></script>
</head>
<body>
$body
<p class="source-note">Generated from $(basename(source)). Model and outputs are
reproduced by <code>julia --project=. run_model.jl</code>.</p>
</body>
</html>
"""

    mktempdir(prefix = "mf-notes-pdf-") do temporary
        html_path = joinpath(temporary, title * ".html")
        profile = joinpath(temporary, "browser-profile")
        write(html_path, document)
        command = `$(browser) --headless --disable-gpu --no-pdf-header-footer --virtual-time-budget=10000 --user-data-dir=$(profile) --print-to-pdf=$(abspath(destination)) $(file_url(html_path))`
        run(command)
    end

    isfile(destination) || error("PDF was not created: $destination")
    filesize(destination) > 1_000 || error("PDF is unexpectedly small: $destination")
    open(destination, "r") do io
        read(io, 5) == Vector{UInt8}(codeunits("%PDF-")) || error("Invalid PDF header")
    end
    println("Wrote $(basename(destination))")
end


function main()
    browser = find_browser()
    for (source_name, destination_name) in DOCUMENTS
        render_pdf(
            joinpath(ROOT, source_name),
            joinpath(ROOT, destination_name),
            browser,
        )
    end
end


main()
