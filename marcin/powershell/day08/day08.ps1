param([string] $InputFile)

$content = cat $InputFile -Raw
$directions = $content | sls "[LR]+" | % { $_.Matches.Value.toCharArray() }
$nodes = @{}

$content | sls "(\w{3}) = \((\w{3}), (\w{3})\)" -AllMatches | % { $_.Matches | % { 
    $nodes[$_.Groups[1].Value] = @{
        Suffux = $_.Groups[1].Value[2];
        Location = $_.Groups[1].Value;
        L = $_.Groups[2].Value;
        R = $_.Groups[3].Value;
    }
} }

function Get-LowestCommonMultiple([long]$a, [long]$b) {
    return ($a * $b) / (Get-GreatestCommonDivisor $a $b)
}

function Get-GreatestCommonDivisor([long]$a, [long]$b) {
    $remainder = 0
    while ($b -gt 0) {
        $remainder = $a % $b
        $a = $b
        $b = $remainder
    }
    $a
}


function Direct-Maze($suffix) {
    $currentNodes = @()

    foreach ($entry in $nodes.GetEnumerator()){
        if ($entry.Key.EndsWith($suffix)) {
            $currentNodes += $entry.Value
        }
    }

    $allSteps = @()
    for ($i = 0; $i -lt $currentNodes.Length; $i++) {
        $steps = 0
        while ('Z' -ne $currentNodes[$i].Suffux) {
            foreach ($direction in $directions) {
                if ('Z' -ne $currentNodes[$i].Suffux) {
                    $nextNode = $currentNodes[$i]["$direction"]
                    $currentNodes[$i] = $nodes["$nextNode"]
                    $steps++
                }
            }
        }
        $allSteps += $steps
    }

    $result = $allSteps[0]
    1..($allSteps.Length-1) | % {
        $result = Get-LowestCommonMultiple $result $allSteps[$_]
    }
    
    $result

}

# Direct-Maze 'AAA'
Direct-Maze 'A'