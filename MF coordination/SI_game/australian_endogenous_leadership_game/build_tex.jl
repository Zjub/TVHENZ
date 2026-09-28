using tectonic_jll

const ROOT = @__DIR__
const SOURCE = joinpath(ROOT, "paper.tex")
const TARGET = joinpath(ROOT, "STRATEGIC_INVESTMENT_POLICY_GAME.pdf")

isfile(SOURCE) || error("Cannot find $SOURCE")
mktempdir() do build_directory
    tectonic() do executable
        cd(ROOT) do
            run(`$executable --keep-logs --outdir $build_directory $SOURCE`)
        end
    end
    built = joinpath(build_directory, "paper.pdf")
    isfile(built) || error("Tectonic did not create the PDF")
    cp(built, TARGET; force=true)
end
println("Built: $TARGET")
