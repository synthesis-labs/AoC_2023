param([string] $InputFile)

$strengths = @{
    'A' = 'E';
    'K' = 'D';
    'Q' = 'C';
    'J' = 'B';
    'T' = 'A';
}

function Get-CardInfo($cards, $bid, $useJoker=$false) {
    $cardCounts = @{}
    $jokers = 0
    $cardOrder = ($cards.toCharArray() | % { $strengths[$_.ToString()] ?? $_.ToString() }) -join ""

    foreach ($card in $cards.toCharArray()) {
        $jokers += $card -eq 'J' -and $useJoker ? 1 : 0

        if (-not $useJoker -or ($useJoker -and $card -ne 'J')) {
            if ($cardCounts.ContainsKey($card)) {
                $cardCounts[$card]++
            } else {
                $cardCounts[$card] = 1;
            }
        }
    }
    if ($cardCounts.Count -eq 0 -and $useJoker) { $cardCounts['A'] = 5 }

    $sets = @()
    foreach ($entry in $cardCounts.GetEnumerator()) {
        $sets += @{
            Card      = $entry.Key
            Instances = $entry.Value
            Strength  = $strengths[$entry.Key.ToString()]
        }
    }

    $sets = $sets | sort Instances,Strength -Descending
    $strongestCard = $sets | select -first 1
    $strongestCard.Instances += $jokers
    $sets[0] = $strongestCard

    $type = 0
    if ($sets.Length -eq 1) { $type = 7 } #5 of a kind
    if ($sets.Length -eq 2 -and $sets.Instances -contains 4) { $type = 6 } #4 of a kind
    if ($sets.Length -eq 2 -and $sets.Instances -contains 3) { $type = 5 } #fh
    if ($sets.Length -eq 3 -and $sets.Instances -contains 3) { $type = 4 } #3 of a kind
    if ($sets.Length -eq 3 -and ($sets | ? Instances -eq 2).Count -eq 2) { $type = 3 } #2 pair
    if ($sets.Length -eq 4) { $type = 2 } #1 pair
    if ($sets.Length -eq 5) { $type = 1 } #high card

    @{
        Hand = $cards;
        CardOrder = "${type}${cardOrder}";
        Bid = $bid;
    }
}

function Get-Winnings($useJoker) {
    $hands = cat $InputFile
    $strengths['J'] = $useJoker ? '1' : 'B'
    $rankings = $(foreach ($hand in $hands) {
        $captures = $hand | sls "(\w+)\s(\d+)" | %{ $_.Matches.Groups }
        $cards = $captures[1].Value
        $bid = [int]$captures[2].Value
        Get-CardInfo $cards $bid $useJoker
    }) | sort CardOrder

    $(foreach ($_ in $rankings) { 
        $rank = $rankings.IndexOf($_) + 1
        $rank * $_.Bid
    }) -join "+" | iex
}

Get-Winnings
Get-Winnings $true