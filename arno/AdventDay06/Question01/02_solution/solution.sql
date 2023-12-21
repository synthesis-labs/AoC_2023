-- Create a table for races with an auto-incrementing ID column
CREATE TABLE Races (
    [ID] INT IDENTITY(1,1) PRIMARY KEY,
    [RaceTime] INT,
    [RecordDistance_mm] INT
);

-- Create a table for toy boats with an auto-incrementing ID column
CREATE TABLE ToyBoats (
    [ID] INT IDENTITY(1,1) PRIMARY KEY,
    [BoatName] VARCHAR(30),
    [StartingSpeed_mm_per_ms] INT,
    [SpeedIncrease_mm_per_ms] INT
);

-- Create a table for race button hold strategies
CREATE TABLE RaceButtonHoldStrategies (
    [RaceID] INT FOREIGN KEY REFERENCES Races(ID),
    [ButtonHold_ms] INT,
    [BoatMove_ms] INT,
    [TraveledDistance_mm] INT,
    [WinAgainstCurrentRecord] BIT
);

-- Insert sample data into Races
INSERT INTO Races ([RaceTime], [RecordDistance_mm])
VALUES
(47, 282),
(70, 1079),
(75, 1147),
(66, 1062)

-- Insert sample data into ToyBoats
INSERT INTO ToyBoats (BoatName, StartingSpeed_mm_per_ms, SpeedIncrease_mm_per_ms)
VALUES ('ArnosToyBoat', 0, 1);

-- Insert entries into RaceButtonHoldStrategies based on RaceTime
DECLARE @RaceID INT;
DECLARE @RaceTime INT;
DECLARE @TraveledDistance_mm INT;
DECLARE @ButtonHold_ms INT = 0;
DECLARE @BoatMove_ms INT = 1;
DECLARE @WinAgainstCurrentRecord BIT = 0;
DECLARE @SpeedIncrease_mm_per_ms INT;
SET @SpeedIncrease_mm_per_ms = (SELECT SpeedIncrease_mm_per_ms FROM ToyBoats)

-- Cursor to iterate through Races and insert entries
DECLARE race_cursor CURSOR FOR
SELECT [ID], [RaceTime] FROM Races;
OPEN race_cursor;
FETCH NEXT FROM race_cursor INTO @RaceID, @RaceTime;

WHILE @@FETCH_STATUS = 0
BEGIN
    DECLARE @RecordDistance_mm INT;
    SET @RecordDistance_mm = (SELECT RecordDistance_mm FROM Races WHERE ID = @RaceID)

    -- Insert entries for ButtonHold_sec 0 to RaceTime
    WHILE @ButtonHold_ms <= @RaceTime
    BEGIN
        IF (@ButtonHold_ms = 0)
            SET @BoatMove_ms = 0
        ELSE
            SET @BoatMove_ms = @RaceTime - @ButtonHold_ms

        SET @TraveledDistance_mm = @ButtonHold_ms * @SpeedIncrease_mm_per_ms * @BoatMove_ms

        IF (@TraveledDistance_mm > @RecordDistance_mm)
            SET @WinAgainstCurrentRecord = 1
        ELSE
            SET @WinAgainstCurrentRecord = 0

        INSERT INTO RaceButtonHoldStrategies ([RaceID], [ButtonHold_ms], [BoatMove_ms], [TraveledDistance_mm], [WinAgainstCurrentRecord])
        VALUES (@RaceID, @ButtonHold_ms, @BoatMove_ms, @TraveledDistance_mm, @WinAgainstCurrentRecord);

        SET @ButtonHold_ms = @ButtonHold_ms + 1;
    END

    SET @ButtonHold_ms = 1; -- Reset ButtonHold_sec for the next race
    FETCH NEXT FROM race_cursor INTO @RaceID, @RaceTime;
END

CLOSE race_cursor;
DEALLOCATE race_cursor;


-- Show the content of Races table
SELECT 'Races' AS 'Table', * FROM Races;

-- Show the content of ToyBoats table
SELECT 'ToyBoats' AS 'Table', * FROM ToyBoats;

-- Show the content of RaceButtonHoldStrategies table
SELECT 'RaceButtonHoldStrategies' AS 'Table', * FROM RaceButtonHoldStrategies;

SELECT RaceID, COUNT(WinAgainstCurrentRecord) AS WaysToWin
INTO #WaysToWinPerRace
FROM Races r
JOIN RaceButtonHoldStrategies rs ON rs.RaceID = r.ID
WHERE rs.WinAgainstCurrentRecord = 1
GROUP BY RaceID

SELECT ROUND(EXP(SUM(LOG(WaysToWin))),0) AS MultipliedWaysToWin FROM #WaysToWinPerRace

-- Drop tables when done testing
DROP TABLE RaceButtonHoldStrategies;
DROP TABLE ToyBoats;
DROP TABLE Races;
DROP TABLE #WaysToWinPerRace