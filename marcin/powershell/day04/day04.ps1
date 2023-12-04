param([string] $InputFile)

$cards = cat $InputFile | sls ": (.+) \| (.+)" | % {
    $winningNumbers = $_.Matches.Groups[1].Value -split '\s+'
    $card = $_.Matches.Groups[2].Value -split '\s+'
    $count = ($_.Matches.Groups[2].Value -split '\s+' | ? { $winningNumbers -Contains $_ } | measure).Count
    $points = $count -eq 0 ? 0 : [Math]::Pow(2, $count - 1)
    @{ Count = $count; Points = $points; Instances = 1; }
}

for ($i = 0; $i -lt $cards.Length - 1; $i++) {
    for ($d = 0; $d -lt $cards[$i].Instances; $d++) {
        for ($c = $i+1; $c -le [Math]::Min($cards.Length - 1, $i + $cards[$i].Count); $c++) {
            $cards[$c].Instances++
        }
    }
}

echo "Part 1: $($cards | measure -Property Points -Sum | select -ExpandProperty Sum )"
echo "Part 2: $($cards | measure -Property Instances -Sum | select -ExpandProperty Sum)"