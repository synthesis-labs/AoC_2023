param([string] $InputFile)

function Get-Cards {
    cat $InputFile | sls "Card\s+(\d+): (.+) \| (.+)" | % {
        $winningNumbers = $_.Matches.Groups[2].Value -split ' ' | % { [int]$_ } | ? { $_ -ne 0 }
        $card = $_.Matches.Groups[3].Value -split ' ' | % { [int]$_ } | % { [int]$_ } | ? { $_ -ne 0 }
        $count = ($card | ? { $winningNumbers -Contains $_ } | measure).Count
        $points = $count -eq 0 ? 0 : [Math]::Pow(2, $count - 1)
        @{ Count = $count; Points = $points; Instances = 1; }
    }
}

function Solve-Part2 {
    $cards = Get-Cards
    for ($i = 0; $i -lt $cards.Length - 1; $i++) {
        for ($d = 0; $d -lt $cards[$i].Instances; $d++) {
            for ($c = $i+1; $c -le [Math]::Min($cards.Length - 1, $i + $cards[$i].Count); $c++) {
                $cards[$c].Instances++
            }
        }
    }
    $cards | measure -Property Instances -Sum | select -ExpandProperty Sum
}

echo "Part 1: $(Get-Cards | measure -Property Points -Sum | select -ExpandProperty Sum )"
echo "Part 2: $(Solve-Part2)"







         
        # for ($d = 0; $d -lt $cards[$i].Instances; $d++) {
        #     echo "$i`:$d"
        # }