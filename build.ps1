$progs =
[pscustomobject]@{
    prog     = "ghc"
    required = "9.2"
},
[pscustomobject]@{
    prog     = "cabal"
    required = "3.6"
},
[pscustomobject]@{
    prog     = "elm"
    required = "0.19"
}

foreach ($prog in $progs) {
    $installed = ((
            Invoke-expression ($prog.prog + ' --version')
        ).Split([Environment]::NewLine) |
        Select-Object -First 1
    ).Split(' ') |
    Select-Object -Last 1
    if ([version]$prog.required -gt [version]$installed) {
        Write-Host "$($prog.prog) version >= $($prog.required) required - $installed found"
        exit 1
    }
}

ghc -fno-code Build.hs 2>&1 | out-null
if ($?) {
    ghc -ignore-dot-ghci --run Build.hs
}
else {
    # TODO environment files no longer written - https://github.com/haskell/cabal/issues/6999#issuecomment-1052953251
    cabal run Build.hs --project-file cabal.project --builddir .build/hs --write-ghc-environment-files=always
}
