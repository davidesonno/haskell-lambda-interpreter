# This script launches GHCi with the source directory included in the search path.
# Usage: .\ghci.ps1 <File.hs> (usually Main.hs or a Test.hs)
# If no arguments are provided, it displays this help message.

if ($args.Count -eq 0) {
    Write-Host "Usage: .\ghci.ps1 <File.hs> (usually Main.hs or a Test.hs)"
    Write-Host "Launches GHCi with the ./src directory in the search path."
    Write-Host "The executed command is: ghci ""-i./src"" <File.hs>"
    exit 1
}

ghci "-i./src" $args[0]