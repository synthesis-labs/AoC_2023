param([string] $InputFile)

$lines = cat $InputFile
$content = cat $InputFile -Raw

function Solve-Part1 {
    $content = cat $InputFile -Raw
    $seeds = $content | sls "seeds: ([\d\s]+\d)" | % { $_.Matches.Groups[1].Value -split ' ' | % { [double]$_ } }
    $pattern = '(\w+-to-\w+)[\s\S]+?(?=(\w+-to-\w+)|$)'
    $groups = $content | sls $pattern -AllMatches | % {
        $_.Matches | % { 
            $group = $_.Groups[1].Value
            $values = $_.Value | sls "(\d+)\s(\d+)\s(\d+)" -AllMatches
            $mappings = $values | % {
                $_.Matches | % {
                    $dst = [long]$_.Groups[1].Value
                    $src = [long]$_.Groups[2].Value
                    $len = [long]$_.Groups[3].Value
                    $offset = [long]($dst-$src)
                    @{
                        Source = $src;
                        Destination = $dst;
                        Offset = $offset;
                        Length = $len; 
                    }
                }
            }
            @{
                Group = $group;
                Mappings = $mappings;
            }
        }
    }


    # # $groups[6].Mappings | % { $m = $_; "${m.Destination} ${m.Source} ${m.Length}"}
    # $groups[4].Mappings | % { 
    #     $d = $_.Destination
    #     $s = $_.Source
    #     $l = $_.Length
    #      "$d $s $l"
    # }

    $locations = $seeds | % {
        $cur = $_
        $iter = 0
        # write-host "seed: $cur"
        $groups | % {
            $grp = $_.Group
            $destination = $_.Group | sls "\w+-to-(\w+)" | % { $_.Matches.Groups[1].Value }
            $mapped = $false
            $_.Mappings | % {
                $src = $_.Source
                $dst = $_.Destination
                $range = $_.Length
                # if ($destination -eq "water") {
                #     write-host "*iter: $iter,`t`tcur: $cur,`t`tsrc: $src`t`tdst: $dst`t`trange: $range,`t`tgroup: $destination"
                # }
                if ($cur -ge $_.Source -and $cur -le ($_.Source + $_.Length) -and -not $mapped) {
                    $cur += $_.Offset
                    $mapped = $true
                }
                # if ($destination -eq "water") {
                #     write-host "iter: $iter,`t`tcur: $cur,`t`tsrc: $src`t`tdst: $dst`t`trange: $range,`t`tgroup: $destination"
                # }
                $iter++
            }          
            # write-host "$destination`: $cur"      
        }
        [double]$cur
    }

    ($locations | sort)[0]
}

# Solve-Part1


function Solve-Part2 {
    $content = cat $InputFile -Raw
    $seedMap = $content | sls "seeds: ([\d\s]+\d)" | % { $_.Matches.Groups[1].Value | sls "(\d+)\s(\d+)" -AllMatches | % {
        $_.Matches | % {
            @{
                First = [long]$_.Groups[1].Value;
                Range = [long]$_.Groups[2].Value;
                Last  = [long]$_.Groups[1].Value + [long]$_.Groups[2].Value -1 
            }
        }
    } }

    $pattern = '(\w+-to-\w+)[\s\S]+?(?=(\w+-to-\w+)|$)'
    $groups = $content | sls $pattern -AllMatches | % {
        $_.Matches | % { 
            $group = $_.Groups[1].Value
            $values = $_.Value | sls "(\d+)\s(\d+)\s(\d+)" -AllMatches
            $mappings = $values | % {
                $_.Matches | % {
                    $dst = [long]$_.Groups[1].Value
                    $src = [long]$_.Groups[2].Value
                    $len = [long]$_.Groups[3].Value
                    $offset = [long]($dst-$src)
                    @{
                        Source = $src;
                        Destination = $dst;
                        Offset = $offset;
                        Length = $len; 
                    }
                }
            }
            @{
                Group = $group;
                Mappings = $mappings;
            }
        }
    }

    $parallelLocations = [System.Collections.Concurrent.ConcurrentBag[psobject]]::new()
    $Message = "Seed bag: "
    write-host $(Get-Date)
    write-host "Bags: $($seedMap.Count)"
    $seedMap | ForEach-Object -Parallel {
        "$using:Message $($_.First)"
        $seedLocations = $(
            for ($s = $_.First; $s -le $_.Last; $s++) {
                $cur = $s
                $using:groups | % {
                    $grp = $_.Group
                    $mapped = $false
                    $_.Mappings | % {
                        $src = $_.Source
                        $range = $_.Length
                        if ($cur -ge $_.Source -and $cur -le ($_.Source + $_.Length) -and -not $mapped) {
                            $cur += $_.Offset
                            $mapped = $true
                        }
                    }          
                }
                [double]$cur
            }
        )
        $minLocalLocation = ($seedLocations | sort)[0]
        $localLocations = $using:parallelLocations
        $localLocations.Add($minLocalLocation)
    } -ThrottleLimit $($seedMap.Count)

    $locations = $parallelLocations | % { $_ }

    # $locations
    ($locations | sort)[0]
    write-host $(Get-Date)


    # $locations = $seedMap | % {
    #     $(for ($s = $_.First; $s -le $_.Last; $s++) {
    #         $cur = $s
    #         $groups | % {
    #             $grp = $_.Group
    #             $mapped = $false
    #             $_.Mappings | % {
    #                 $src = $_.Source
    #                 $range = $_.Length
    #                 if ($cur -ge $_.Source -and $cur -le ($_.Source + $_.Length) -and -not $mapped) {
    #                     $cur += $_.Offset
    #                     $mapped = $true
    #                 }
    #             }          
    #         }
    #         [double]$cur
    #     })
    # }
    # write-host "DONE"
    # ($locations | sort)[0]


    # $locations = $seeds | % {
    #     $cur = $_
    #     $iter = 0
    #     # write-host "seed: $cur"
    #     $groups | % {
    #         $grp = $_.Group
    #         $destination = $_.Group | sls "\w+-to-(\w+)" | % { $_.Matches.Groups[1].Value }
    #         $mapped = $false
    #         $_.Mappings | % {
    #             $src = $_.Source
    #             $dst = $_.Destination
    #             $range = $_.Length
    #             # if ($destination -eq "water") {
    #             #     write-host "*iter: $iter,`t`tcur: $cur,`t`tsrc: $src`t`tdst: $dst`t`trange: $range,`t`tgroup: $destination"
    #             # }
    #             if ($cur -ge $_.Source -and $cur -le ($_.Source + $_.Length) -and -not $mapped) {
    #                 $cur += $_.Offset
    #                 $mapped = $true
    #             }
    #             # if ($destination -eq "water") {
    #             #     write-host "iter: $iter,`t`tcur: $cur,`t`tsrc: $src`t`tdst: $dst`t`trange: $range,`t`tgroup: $destination"
    #             # }
    #             $iter++
    #         }          
    #         # write-host "$destination`: $cur"      
    #     }
    #     [double]$cur
    # }

    # ($locations | sort)[0]
}

Solve-Part2

# echo "Part 1: $(Solve-Part1)"
# echo "Part 2: $(Solve-Part2)"