using tectonic_jll

# Build the native LaTeX paper with the reproducible Tectonic binary supplied
# by Julia's artifact system.  Run this file from any working directory.
const PROJECT_DIR = @__DIR__
const TEX_FILE = joinpath(PROJECT_DIR, "paper.tex")

isfile(TEX_FILE) || error("Cannot find $TEX_FILE")

mktempdir() do build_directory
    tectonic() do executable
        # Compile outside the source directory.  On Windows, PDF viewers often
        # hold PAPER.pdf open; an isolated output keeps compilation itself from
        # failing when that happens.
        cd(PROJECT_DIR) do
            run(`$executable --outdir $build_directory $TEX_FILE`)
        end
    end

    source_pdf = joinpath(build_directory, "paper.pdf")
    target_pdf = joinpath(PROJECT_DIR, "PAPER.pdf")
    fallback_pdf = joinpath(PROJECT_DIR, "PAPER_UPDATED.pdf")
    isfile(source_pdf) || error("Tectonic did not create $source_pdf")
    try
        cp(source_pdf, target_pdf; force=true)
        isfile(fallback_pdf) && rm(fallback_pdf; force=true)
        println("Built: $target_pdf")
    catch exception
        # Preserve the completed build if another program has locked PAPER.pdf.
        # Re-running after the viewer closes will restore the canonical name.
        cp(source_pdf, fallback_pdf; force=true)
        @warn "PAPER.pdf is locked; wrote the updated paper to $fallback_pdf" exception
        println("Built: $fallback_pdf")
    end
end
