param([string] $InputFile)

$pieces = @{ red = 12; green = 13; blue = 14; }
$games = (cat $InputFile | sls "Game\s+(\d+):\s(.+)").Matches | % {
    $invalid = $false
    $mincolours = @{ red = 0; green = 0; blue = 0; }
    $turns = $_.Groups[2].Value -split '\s*;\s*'
    ($turns | sls "(\d+)\s(red|green|blue)" -AllMatches).Matches | % {
        $cubes = [int]$_.Groups[1].Value
        $colour = $_.Groups[2].Value
        $invalid = $invalid -or ($cubes -gt $pieces[$colour]) 
        $mincolours[$colour] = [Math]::Max($mincolours[$colour], $cubes)   
    }
    @{
        Game = $_.Groups[1].Value;
        Valid = -not $invalid;
        Power = $mincolours["red"] * $mincolours["green"] * $mincolours["blue"]
    }
}

echo "Part 1: $( (($games | ? Valid).Game | measure -Sum).Sum )"
echo "Part 2: $( (($games).Power | measure -Sum).Sum )"