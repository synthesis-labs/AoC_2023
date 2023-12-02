param([string] $InputFile)

function Solve-Part1 {

    $pieces = @{
        red = 12;
        green = 13;
        blue = 14;
    }

    cat $InputFile | % {
        $game = sls -InputObject $_ "Game\s(\d+):\s(.+)"
        $num = $game.Matches.Groups[1].Value
        $valid = $true
        $turns = $game.Matches.Groups[2].Value -split '; '
        $turns | % {
            $turn = $_ | sls "(\d+)\s(red|green|blue)" -AllMatches
            $turn.Matches | % {
                $cubes = [int]$_.Groups[1].Value
                $colour = $_.Groups[2].Value
                if ($cubes -gt $pieces[$colour]) {
                    $valid = $false
                }
            }
        }
        @{
            Game = $num;
            Valid = $valid
        }

    } | ? { $_.Valid -eq $true } | select -ExpandProperty Game | measure -Sum | select -ExpandProperty Sum
    

}

function Solve-Part2 {  

    cat $InputFile | % {
        $game = sls -InputObject $_ "Game\s(\d+):\s(.+)"
        $num = $game.Matches.Groups[1].Value

        $pieces = @{
            red = 0;
            green = 0;
            blue = 0;
        }

        $turns = $game.Matches.Groups[2].Value -split '; '
        $turns | % {
            $turn = $_ | sls "(\d+)\s(red|green|blue)" -AllMatches
            $turn.Matches | % {
                $cubes = [int]$_.Groups[1].Value
                $colour = $_.Groups[2].Value
                if ($cubes -gt $pieces[$colour]) {
                    $pieces[$colour] = $cubes
                }
            }
        }
        @{
            Game = $num;
            Red = $pieces["red"];
            Green = $pieces["green"];
            Blue = $pieces["blue"];
            Power = $pieces["red"] * $pieces["green"] * $pieces["blue"]
        }
    } | select -ExpandProperty Power | measure -Sum | select -ExpandProperty Sum
}

Solve-Part1
Solve-Part2