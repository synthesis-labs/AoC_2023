param([string] $InputFile)

function Solve-Day01-Part1 {
    cat $InputFile | % {
        $matchlist = [regex]::Matches($_, "(\d)");
        $first = ($matchlist | select -first 1).Groups[1].Value;
        $last = ($matchlist | select -last 1).Groups[1].Value;
        [long]"$first$last" 
    } | measure -Sum | select -ExpandProperty Sum
}

function Solve-Day01-Part2 {
    $numbers = @{ one = 1; two = 2; three = 3; four = 4; five = 5; six = 6; seven = 7; eight = 8; nine = 9; }
    cat $InputFile | % {
        $matchlist = [regex]::Matches($_, "(?=(\d|one|two|three|four|five|six|seven|eight|nine))");
        $key1 = ($matchlist | select -first 1).Groups[1].Value;
        $key2 = ($matchlist | select -last 1).Groups[1].Value;
        $first = $numbers[$key1] ? $numbers[$key1] : $key1;
        $last = $numbers[$key2] ? $numbers[$key2] : $key2;
        [long]"$first$last" 
    } | measure -Sum | select -ExpandProperty Sum
}

echo "Part 1: $(Solve-Day01-Part1)"
echo "Part 2: $(Solve-Day01-Part2)"