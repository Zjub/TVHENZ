using tectonic_jll

const ROOT = @__DIR__
const SOURCE = joinpath(ROOT, "slides.tex")
const TARGET = joinpath(ROOT, "ANALYTICAL_STRATEGIC_INVESTMENT_MPE_SLIDES.pdf")

isfile(SOURCE) || error("Cannot find $SOURCE")
mktempdir() do build_directory
    tectonic() do executable
        cd(ROOT) do
            run(`$executable --keep-logs --outdir $build_directory $SOURCE`)
        end
    end
    built = joinpath(build_directory, "slides.pdf")
    isfile(built) || error("Tectonic did not create the slide PDF")
    cp(built, TARGET; force=true)
end
println("Built: $TARGET")
