param([string] $InputFile)

function Get-Parts {
    $lines = cat $InputFile
    $parts = for ($i = 0; $i -lt $lines.Length; $i++) { 
        Get-Part $i $($lines.Length -1) $($lines[0].Length -1)
    }
    $parts
}

function Get-Part($rownum, $lastrow, $lastcol) {
    $above = [Math]::Max(0, $rownum - 1)
    $below = [Math]::Min($lastrow, $rownum + 1)
    $matchlist = $lines[$rownum] | sls "(\d+)" -AllMatches | % { $_.Matches | % { @{ Number = $_.Value; Index = $_.Index } } }
    $matchlist | % {
        $num = $_.Number
        $leftcol = $_.Index
        $rightcol = $leftcol + $num.Length - 1
        $startcol = [Math]::Max(0, $leftcol - 1)
        $endcol = [Math]::Min($lastcol, $rightcol + 1)
        $ispart = $false
        $gearpart = $false
        $gearPositions = @()
        for ($c = $startcol; $c -le $endcol; $c++) {
            for ($r = $above; $r -le $below; $r++) {
                if (($lines[$r][$c] | sls "[\d\.]").Matches.Length -eq 0) {
                    $ispart = $true
                }
                if ($lines[$r][$c] -eq "*") {
                    $gearpart = $true
                    $gearPositions += "$r`:$c"
                }
            }
        }
        @{
            Number = [int]$num;
            IsPart = $ispart;
            GearPart = $gearpart;
            GearPositions = $gearPositions
        }       
    } | ? { $_.IsPart -eq $true }
}

function Solve-Part1 {

    $parts = Get-Parts
    $parts | select -ExpandProperty Number | measure -Sum | select -ExpandProperty Sum
}

function Solve-Part2 {  
    $parts = Get-Parts
    $gearParts = $parts | ? { $_.GearPart }
    $gearlinks = $gearParts | select -ExpandProperty GearPositions | group | ? Count -eq 2 | select -ExpandProperty Name
    $gearlinks | % {
        $link = $_
        ($gearParts | ? { $_.GearPositions -eq $link } | % { $_.Number }) -join "*" | iex
    } | measure -Sum | select -ExpandProperty Sum
}

Solve-Part1
Solve-Part2