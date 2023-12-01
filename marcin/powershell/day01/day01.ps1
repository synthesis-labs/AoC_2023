param([string] $InputFile)

function Get-Number($pattern, $numbermap=@{}) {
    cat $InputFile | % {
        $matchlist = [regex]::Matches($_, $pattern);
        $key1 = ($matchlist | select -first 1).Groups[1].Value;
        $key2 = ($matchlist | select -last 1).Groups[1].Value;
        $first = $numbermap[$key1] ? $numbermap[$key1] : $key1;
        $last = $numbermap[$key2] ? $numbermap[$key2] : $key2;
        [long]"$first$last"
    } | measure -Sum | select -ExpandProperty Sum
}

function Solve-Part1 {
    Get-Number "(\d)"
}

function Solve-Part2 {
    $numbers = @{ one = 1; two = 2; three = 3; four = 4; five = 5; six = 6; seven = 7; eight = 8; nine = 9; }
    Get-Number "(?=(\d|one|two|three|four|five|six|seven|eight|nine))" $numbers
}

echo "Part 1: $(Solve-Part1)"
echo "Part 2: $(Solve-Part2)"