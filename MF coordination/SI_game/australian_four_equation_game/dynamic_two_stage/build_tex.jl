using tectonic_jll

const ROOT = @__DIR__
const TEX = joinpath(ROOT, "paper.tex")
isfile(TEX) || error("Cannot find $TEX")

mktempdir() do build_directory
    tectonic() do executable
        cd(ROOT) do
            run(`$executable --keep-logs --outdir $build_directory $TEX`)
        end
    end
    source = joinpath(build_directory, "paper.pdf")
    target = joinpath(ROOT, "DYNAMIC_STRATEGIC_INVESTMENT.pdf")
    isfile(source) || error("Tectonic did not produce the PDF")
    cp(source, target; force=true)
    println("Built: $target")
end
