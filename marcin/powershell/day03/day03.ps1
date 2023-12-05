param([string] $InputFile)

$lines = cat $InputFile
$parts = $lines | % {
    $rownum = $lines.IndexOf($_);
    $_ | sls "(\d+)" -AllMatches | % { $_.Matches | % { @{ Number = $_.Value; Index = $_.Index } } } | % {
        $item = @{ Number = $_.Number; IsPart = $false; GearPart = $false; Links = @() }
        for ($col = [Math]::Max(0, $_.Index - 1); $col -le [Math]::Min($lines[0].Length -1, $_.Index + $_.Number.Length); $col++) {
            for ($row = [Math]::Max(0, $rownum - 1); $row -le [Math]::Min($lines.Length -1, $rownum + 1); $row++) {
                $item.IsPart = $item.IsPart -or $(($lines[$row][$col] | sls "[\d\.]").Matches.Length -eq 0)
                $item.GearPart = $item.GearPart -or ($lines[$row][$col] -eq "*")
                $item.Links += ($lines[$row][$col] -eq "*") ? "$row`:$col" : ""
            }
        }
        $item       
    } | ? IsPart
}

$gearParts = $parts | ? GearPart
$gearlinks = ($gearParts.Links | group | ? Count -eq 2).Name

echo "Part 1: $( ($parts.Number | measure -Sum).Sum )"
echo "Part 2: $( ($gearlinks | % { ($gearParts | ? Links -eq $_).Number -join "*" | iex } | measure -Sum).Sum)"