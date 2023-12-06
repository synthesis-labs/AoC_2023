param([string] $InputFile)

$cards = cat $InputFile | sls ": (.+) \| (.+)" | % {
    $groups = $_.Matches.Groups | ? Name -gt 0 | % { ,@( $_.Value -split '\s+' ) }
    $count = ($groups[1] | ? { $groups[0] -Contains $_ } | measure).Count
    $points = $count -eq 0 ? 0 : [Math]::Pow(2, $count - 1)
    @{ Count = $count; Points = $points; Instances = 1; }
}

for ($i = 0; $i -lt $cards.Length - 1; $i++) {
    for ($c = $i+1; $c -le [Math]::Min($cards.Length - 1, $i + $cards[$i].Count); $c++) {
        $cards[$c].Instances += $cards[$i].Instances
    }
}

echo "Part 1: $( ($cards | measure -Property Points -Sum).Sum )"
echo "Part 2: $( ($cards | measure -Property Instances -Sum).Sum )"