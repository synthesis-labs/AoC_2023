List<string> GetInput()
{
    List<string> totalInput = new List<string>();
    StreamReader reader = new StreamReader("input.txt");

    //read first line so no breaky
    var line = reader.ReadLine();

    while (line != null)
    {
        totalInput.Add(line);
        line = reader.ReadLine();
    }

    //close connection so no breaky
    reader.Close();
    return totalInput;
}

string GetPart1()
{
    var input = GetInput();
    var lengthOfInput = input.Count;
    var lengthOfLine = input[0].Length;
    var totalCount = 0;

    var charList = new List<List<char>>();

    //convert into chars
    for (int line = 0; line <= lengthOfInput-1; line++)
    {
        charList.Add(new List<char>());
        for (int character = 0; character <= lengthOfLine-1; character++)
        {
            charList[line].Add(input[line][character]);
        }
    }

    for (int line = 0; line < lengthOfInput; line++)
    {
        for (int character = 0; character < lengthOfLine;)
        {

            var currentChar = input[line][character];
            bool isIncreased = false; //used to track if the current number found has already been added to the total

            //check if current spot is a number
            int mmmmm;
            
            if (int.TryParse(currentChar.ToString(), out mmmmm))
            {
                //get the size of the current numner (eg. 9=1, 11=2, 121=3)
                var numberLength = GetNumberLength(line, character, charList, 1);
                var numberFound = int.Parse(GetNumberFound(line, character, charList, ""));
                //check if there are any special characters around the number below:

                //check if top row is special
                //a slice of the upper adjacent chars
                if (line != 0)
                {
                    List<char> upperChars = new List<char>();

                    //if not first char in line, get the upper left char
                    if(character != 0)
                    {
                        upperChars.Add(charList[line - 1][character - 1]);
                    }
                    //get all the chars above the number
                    for (int i = 0; i < numberLength; i++)
                    {
                        upperChars.Add(charList[line - 1][character+i]);
                    }
                    //if character + numberLength < lengthOfLine 
                    if(character + numberLength < lengthOfLine)
                    {
                        upperChars.Add(charList[line - 1][character + numberLength]);
                    }
                    if (upperChars.Any(x => x != '.'))
                    {
                        foreach (var charcheck in upperChars)
                        {
                            int tmep;
                            if (charcheck != '.' && !char.IsLetterOrDigit(charcheck) && !int.TryParse(charcheck.ToString(), out tmep) && !isIncreased)
                            {
                                totalCount += numberFound;
                                isIncreased = true;
                                break;
                            }
                        }
                    }
                }

                //check if middle left
                if (character != 0)
                {
                    int tmm;
                    if (charList[line][character - 1] != '.' && !char.IsLetterOrDigit(charList[line][character - 1]) && !int.TryParse(charList[line][character - 1].ToString(), out tmm) && !isIncreased)
                    {
                        totalCount += numberFound;
                        isIncreased = true;
                    }
                }

                //check if middle right
                if (character + numberLength < lengthOfLine && !isIncreased)
                {
                    int tmm;
                    if (charList[line][character + numberLength] != '.' && !char.IsLetterOrDigit(charList[line][character + numberLength]) && !int.TryParse(charList[line][character + numberLength].ToString(), out tmm) && !isIncreased)
                    {
                        totalCount += numberFound;
                        isIncreased = true;
                    }
                }
                
                //check if bottom row is special
                //a slice of the lower adjacent chars
                List<char> lowerChars = new List<char>();
                //get lower chars, dont read if last line in input
                if (line != charList.Count - 1 && !isIncreased)
                {
                    //get the lower left horizontal char, dont read if first char in line
                    if (character != 0)
                    {
                        lowerChars.Add(charList[line + 1][character - 1]);
                    }
                    //get all the chars below the number
                    for (int i = 0; i < numberLength; i++)
                    {
                        lowerChars.Add(charList[line + 1][character+i]);
                    }
                    //get the lower right horizontal char, dont read if num is last in line
                    if (character + numberLength < lengthOfLine)
                    {
                        lowerChars.Add(charList[line + 1][character + numberLength]);
                    }

                    if (lowerChars.Any(x => x != '.') && !isIncreased)
                    {
                        foreach (var charcheck in lowerChars)
                        {
                            int tmep;
                            if (charcheck != '.' && !char.IsLetterOrDigit(charcheck) && !int.TryParse(charcheck.ToString(), out tmep))
                            {
                                totalCount += numberFound;
                                isIncreased = true;
                                break;
                            }
                        }
                    }
                }

                //increase character by length of number found so that we dont read more of its numbers
                character += numberLength;
            }

            //if not already increased, increase by 1
            if (!isIncreased)
            {
                character++;
            }
        }
    }

    return totalCount.ToString();
}

string GetNumberFound(int y, int x, List<List<char>> input, string currentFoundDigits)
{
    currentFoundDigits = currentFoundDigits + input[y][x].ToString();
    if (x == 139)
    {
        //if they are here, we know it was a digit because it was checked in the prev iteration
        return currentFoundDigits;
    }
    //check if there are more numbers following
    if (char.IsDigit(input[y][x + 1]))
    {
        currentFoundDigits = GetNumberFound(y, x+1, input, currentFoundDigits);
    }

    return currentFoundDigits;
}

string GetNumberFoundLeft(int y, int x, List<List<char>> input, string currentFoundDigits)
{
    currentFoundDigits = input[y][x].ToString() + currentFoundDigits;
    if (x == 0)
    {
        //if they are here, we know it was a digit because it was checked in the prev iteration
        return currentFoundDigits;
    }
    //check if there are more numbers following
    if (char.IsDigit(input[y][x - 1]))
    {
        currentFoundDigits = GetNumberFoundLeft(y, x - 1, input, currentFoundDigits);
    }

    return currentFoundDigits;
}

int GetNumberLength(int y, int x, List<List<char>> input, int currentLength)
{
    int lengthOfNumber = currentLength;
    //check if there are more numbers following
    if(x == 139)
    {
        //if they are here, we know it was a digit because it was checked in the prev iteration
        return lengthOfNumber;
    }
    if (char.IsDigit(input[y][x + 1]))
    {
        lengthOfNumber = currentLength + 1;
        lengthOfNumber = GetNumberLength(y, x+1, input, lengthOfNumber);
    }

    return lengthOfNumber;
}

string GetPart2()
{
    var input = GetInput();
    var lengthOfInput = input.Count;
    var lengthOfLine = input[0].Length;
    var sumOfTotalGearRatios = 0;

    var charList = new List<List<char>>();

    //.....
    //..*..
    //.....

    //convert into chars
    for (int readline = 0; readline <= lengthOfInput - 1; readline++)
    {
        charList.Add(new List<char>());
        for (int readcharacter = 0; readcharacter <= lengthOfLine - 1; readcharacter++)
        {
            charList[readline].Add(input[readline][readcharacter]);
        }
    }

    for (int line = 0; line < lengthOfInput; line++)
    {
        for (int character = 0; character < lengthOfLine; character++)
        {
            var currentChar = input[line][character];

            if(currentChar == '*')
            {
                var listOfAdjNums = new List<int>();


                //check middle left
                if (character != 0)
                {
                    int tmm;
                    if (int.TryParse(charList[line][character - 1].ToString(), out tmm))
                    {
                        var adjNum = GetNumberFoundLeft(line, character-1, charList, "");//this works for numbers to the left
                        listOfAdjNums.Add(int.Parse(adjNum));
                    }
                }
                //check middle right
                if (character + 1 < lengthOfLine)
                {
                    int tmm;
                    if (int.TryParse(charList[line][character+1].ToString(), out tmm))
                    {
                        var adjNum = GetNumberFound(line, character+1, charList, "");//this works for numbers to the right
                        listOfAdjNums.Add(int.Parse(adjNum));
                    }
                }
                //UPPER:
                if (line != 0)
                {
                    //if top middle = .
                    if (charList[line - 1][character] == '.')
                    {
                        //dont check left
                        if(character != 0)
                        {
                            var topLeftChar = charList[line - 1][character - 1];
                            int mmm;
                            if(int.TryParse(topLeftChar.ToString(), out mmm))
                            {
                                //top left is num
                                var num = GetNumberFoundLeft(line-1, character-1, charList, "");
                                listOfAdjNums.Add(int.Parse(num));
                            }
                        }
                        //dont check right
                        if(character != lengthOfLine - 1)
                        {
                            var topRightChar = charList[line - 1][character +1];
                            int mmm;
                            if (int.TryParse(topRightChar.ToString(), out mmm))
                            {
                                //top right is num
                                var num = GetNumberFound(line-1, character+1, charList, "");
                                listOfAdjNums.Add(int.Parse(num));
                            }
                        }
                    }
                    //not .
                    else
                    {
                        //check if top left is num
                        int mmm;
                        if (character != 0 && int.TryParse(charList[line - 1][character - 1].ToString(), out mmm)){
                            //check if there is a num further left
                            string num = "";
                            if(character - 2 >= 0 && int.TryParse(charList[line - 1][character - 2].ToString(), out mmm))
                            {
                                num = GetNumberFound(line - 1, character - 2, charList, "");
                            }
                            else
                            {
                                num = GetNumberFound(line-1, character - 1, charList, "");
                            }
                            if(num != "")
                            {
                                listOfAdjNums.Add(int.Parse(num));
                            }
                        }
                        //else check if top middle is num
                        else if (int.TryParse(charList[line - 1][character].ToString(), out mmm))
                        {
                            var num = GetNumberFound(line-1, character, charList, "");
                            listOfAdjNums.Add(int.Parse(num));
                        }
                        //else check if top right is num
                        else if (character < lengthOfLine - 1 && int.TryParse(charList[line - 1][character + 1].ToString(), out mmm) && character != lengthOfLine-1)
                        {
                            var num = GetNumberFound(line - 1, character + 1, charList, "");
                            listOfAdjNums.Add(int.Parse(num));
                        }
                    }
                }

                //LOWER:
                if (line < lengthOfInput-1)
                {
                    //if bottom middel = .
                    if (charList[line + 1][character] == '.')
                    {
                        //dont check left
                        if (character != 0)
                        {
                            var bottomLeftChar = charList[line + 1][character - 1];
                            int mmm;
                            if (int.TryParse(bottomLeftChar.ToString(), out mmm))
                            {
                                //bottom left is num
                                var num = GetNumberFoundLeft(line+1, character -1, charList, "");
                                listOfAdjNums.Add(int.Parse(num));
                            }
                        }
                        //dont check right
                        if (character != lengthOfLine - 1)
                        {
                            var bottomLeftChar = charList[line + 1][character + 1];
                            int mmm;
                            if (int.TryParse(bottomLeftChar.ToString(), out mmm))
                            {
                                //bottom right is num
                                var num = GetNumberFound(line+1, character + 1, charList, "");
                                listOfAdjNums.Add(int.Parse(num));
                            }
                        }
                    }
                    //not .
                    else
                    {
                        //check if bottom left is num
                        int mmm;
                        if (character != 0 && int.TryParse(charList[line + 1][character - 1].ToString(), out mmm) && character != 0)
                        {
                            //check if there is a num further left
                            string num = "";
                            if (character - 2 >= 0 && int.TryParse(charList[line + 1][character - 2].ToString(), out mmm))
                            {
                                num = GetNumberFound(line + 1, character - 2, charList, "");
                            }
                            else
                            {
                                num = GetNumberFound(line + 1, character - 1, charList, "");
                            }
                            if (num != "")
                            {
                                listOfAdjNums.Add(int.Parse(num));
                            }
                        }
                        //else check if bottom middle is num
                        else if (int.TryParse(charList[line + 1][character].ToString(), out mmm))
                        {
                            if(charList[line - 1][character] == '5')
                            {
                                var num2 = GetNumberFound(line + 1, character, charList, "");
                            }
                            var num = GetNumberFound(line + 1, character, charList, "");
                            listOfAdjNums.Add(int.Parse(num));
                        }
                        //else check if bottom right is num
                        else if (character < lengthOfLine-1 && int.TryParse(charList[line + 1][character + 1].ToString(), out mmm) && character != lengthOfLine - 1)
                        {
                            var num = GetNumberFound(line + 1, character + 1, charList, "");
                            listOfAdjNums.Add(int.Parse(num));
                        }
                    }
                }
                //check if adj is EXACTLY 2
                if (listOfAdjNums.Count == 2)
                {
                    Console.WriteLine($"found the numbers {listOfAdjNums[0]} , {listOfAdjNums[1]} to be gear ratios on line: {line}");
                    //get the gear ratio that are adj to gear.
                    sumOfTotalGearRatios += listOfAdjNums[0] * listOfAdjNums[1];
                }
                Console.WriteLine();
            }
        }
    }

    return sumOfTotalGearRatios.ToString();
}
Console.WriteLine("Part 1: " + GetPart1());
Console.WriteLine("Part 2: " + GetPart2());
