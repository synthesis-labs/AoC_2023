param([string] $InputFile)

function Get-Diffs($list) {
    $diffTree = @()
    $diffs = 0..($list.Length - 2) | % { $list[$_+1] - $list[$_] }
    $diffTree += ,$diffs
    $allZero = ($diffs | ? { $_ -ne 0 }).Length -eq 0
    if (-not $allZero) {
       Get-Diffs $diffs | % { $diffTree += ,$_ }
    }
    $diffTree
}

function Get-ReportValue($sequence, $operator) {
    $val = 0
    $tree = ,$sequence + $(Get-Diffs $sequence)
    -1..-($tree.Length) | % { $val = @($tree[$_][$operator -eq "-" ? 0 : -1], $val) -join $operator -replace "--","+" | iex }
    $val
}

function Get-Sum($operator) {
    cat $InputFile | % {
        $sequence = sls "[\-\d]+" -InputObject $_ -AllMatches | % { $_.Matches.Value }
        Get-ReportValue $sequence $operator
    } | Join-String -separator '+' | iex
}

echo "Part 1: $(Get-Sum "+")"
echo "Part 2: $(Get-Sum "-")"