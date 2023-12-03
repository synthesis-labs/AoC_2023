param([string] $InputFile)

function Get-Parts {
    $lines = cat $InputFile
    $lines | % {
        $rownum = $lines.IndexOf($_);
        $_ | sls "(\d+)" -AllMatches | % { $_.Matches | % { @{ Number = $_.Value; Index = $_.Index } } } | % {
            $ispart = $false
            $gearpart = $false
            $links = @()
            for ($col = [Math]::Max(0, $_.Index - 1); $col -le [Math]::Min($lines[0].Length -1, $_.Index + $_.Number.Length); $col++) {
                for ($row = [Math]::Max(0, $rownum - 1); $row -le [Math]::Min($lines.Length -1, $rownum + 1); $row++) {
                    $ispart = $ispart -or $(($lines[$row][$col] | sls "[\d\.]").Matches.Length -eq 0)
                    $gearpart = $gearpart -or ($lines[$row][$col] -eq "*")
                    $links += ($lines[$row][$col] -eq "*") ? "$row`:$col" : ""
                }
            }
            @{ Number = $_.Number; IsPart = $ispart; GearPart = $gearpart; Links = $links }       
        } | ? IsPart
    }
}

function Solve-Part2 {  
    $gearParts = Get-Parts | ? GearPart
    $gearlinks = $gearParts | select -ExpandProperty Links | group | ? Count -eq 2 | select -ExpandProperty Name
    $gearlinks | % {($gearParts | ? Links -eq $_ | % { $_.Number }) -join "*" | iex } | measure -Sum | select -ExpandProperty Sum
}

echo "Part 1: $(Get-Parts | select -ExpandProperty Number | measure -Sum | select -ExpandProperty Sum)"
echo "Part 2: $(Solve-Part2)"