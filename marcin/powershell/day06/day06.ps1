param([string] $InputFile)

$content = cat $InputFile
$times = $content[0] | sls "(\d+)+" -AllMatches | % { $_.Matches | % { [int]$_.Value }}
$distances = $content[1] | sls "(\d+)+" -AllMatches | % { $_.Matches | % { [int]$_.Value }}
$races = $times | % {
    @{
        TimeLimit = $_
        Record = $distances[$times.IndexOf($_)] 
    }
}

function SplitArray($rangeStart, $rangeEnd, $numberOfBatches) {
    $rangeSize = 1 + $rangeEnd - $rangeStart
    $batchSize = [math]::Ceiling($rangeSize / $numberOfBatches)
    $(for ($i = 0; $i -lt $numberOfBatches; $i++) {
        $start = ($i * $batchSize) + $rangeStart
        $end = [math]::Min($start + $batchSize - 1, $rangeEnd)
        ,@($start..$end)
    })
}

function NumberOfWaysToWinThreaded($race, $threadCount) {
    $bag = [System.Collections.Concurrent.ConcurrentBag[psobject]]::new()
    $ranges = SplitArray 0 $race.TimeLimit $threadCount
    $ranges | ForEach-Object -Parallel {
        $localRace = $using:race
        $racesWithRecordBeaten = $_ | ? { (($localRace.TimeLimit - $_) * $_) -gt $localRace.Record }  
        $localBag = $using:bag
        $localBag.Add($racesWithRecordBeaten.Count)
    } 
    $possibilities = $bag | % { $_ }
    $possibilities -join "+" | iex
}

function NumberOfWaysToWin($race) { 
    # $(0..$race.TimeLimit | ? { (($race.TimeLimit - $_) * $_) -gt $race.Record }).Count
    $range = 0..$race.TimeLimit
    $wins = 0
    foreach ($_ in $range) {
        $speed = $race.TimeLimit - $_
        $distance = $speed * $_
        if ($distance -gt $race.Record) {
            $wins++
        }
    } 
    $wins
}

$bigRace = @{
    TimeLimit = [long]($races.TimeLimit -join "");
    Record = [long]($races.Record -join "");
}

echo "Part 1: $($races | % { NumberOfWaysToWin $_ } | join-string -Separator "*" | iex)"
echo "Part 2: $(NumberOfWaysToWin $bigRace)"