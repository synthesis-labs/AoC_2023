param([string] $InputFile)

function Solve {
    $pieces = @{ red = 12; green = 13; blue = 14; }
    cat $InputFile | % {
        $invalid = $false
        $mincolours = @{ red = 0; green = 0; blue = 0; }
        $game = sls -InputObject $_ "Game\s(\d+):\s(.+)"
        $turns = $game.Matches.Groups[2].Value -split '; '
        $turns | % {
            $turn = $_ | sls "(\d+)\s(red|green|blue)" -AllMatches
            $turn.Matches | % {
                $cubes = [int]$_.Groups[1].Value
                $colour = $_.Groups[2].Value
                $invalid = $invalid -or ($cubes -gt $pieces[$colour]) 
                $mincolours[$colour] = [Math]::Max($mincolours[$colour], $cubes)   
            }
        }
        @{
            Game = $game.Matches.Groups[1].Value;
            Valid = -not $invalid;
            Power = $mincolours["red"] * $mincolours["green"] * $mincolours["blue"]
        }
    }
}

echo "Part 1: $(Solve | ? Valid | select -ExpandProperty Game | measure -Sum | select -ExpandProperty Sum )"
echo "Part 2: $(Solve | select -ExpandProperty Power | measure -Sum | select -ExpandProperty Sum)"