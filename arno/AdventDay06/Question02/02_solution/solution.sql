-- NOTE TO SELF: 
-- While the initial idea of crafting a solution in SQL Server seemed amusing, it quickly became apparent that this approach was ill-advised.
-- SQL, with its inherent database limitations and constraints, proved to be an inefficient method for tackling the problem at hand.
-- Although the SQL implementation technically functions, its execution time is exorbitantly long, prompting a shift to Python, which emerged as the victorious choice for its speed and flexibility. Python FTW (For The Win)!


SET NOCOUNT ON; -- Suppress "rows affected" messages

-- Create a table for races with an auto-incrementing ID column
CREATE TABLE #Races (
    [ID] BIGINT IDENTITY(1,1) PRIMARY KEY,
    [RaceTime] BIGINT,
    [RecordDistance_mm] BIGINT
);

-- Create a table for toy boats with an auto-incrementing ID column
CREATE TABLE #ToyBoats (
    [ID] BIGINT IDENTITY(1,1) PRIMARY KEY,
    [BoatName] VARCHAR(30),
    [StartingSpeed_mm_per_ms] BIGINT,
    [SpeedIncrease_mm_per_ms] BIGINT
);

-- Create a table for race button hold strategies
CREATE TABLE #RaceButtonHoldStrategies (
    [RaceID] BIGINT,
    [ButtonHold_ms] BIGINT,
    [BoatMove_ms] BIGINT,
    [TraveledDistance_mm] BIGINT,
    [WinAgainstCurrentRecord] BIT
);

-- Insert sample data into Races
INSERT INTO #Races ([RaceTime], [RecordDistance_mm])
VALUES
(47707566, 282107911471062)

-- Insert sample data into ToyBoats
INSERT INTO #ToyBoats (BoatName, StartingSpeed_mm_per_ms, SpeedIncrease_mm_per_ms)
VALUES ('ArnosToyBoat', 0, 1);

DECLARE @i BIGINT = 0;

WHILE @i <= (SELECT RaceTime FROM #Races)
BEGIN
    INSERT INTO #RaceButtonHoldStrategies ([RaceID], [ButtonHold_ms], [BoatMove_ms], [TraveledDistance_mm], [WinAgainstCurrentRecord])
    SELECT
        r.[ID] AS [RaceID],
        @i AS [ButtonHold_ms],
        CASE WHEN @i = 0 THEN 0 ELSE r.[RaceTime] - @i END AS [BoatMove_ms],
        @i * (CASE WHEN @i = 0 THEN 0 ELSE r.[RaceTime] - @i END) AS [TraveledDistance_mm],
        CASE WHEN @i * (CASE WHEN @i = 0 THEN 0 ELSE r.[RaceTime] - @i END) > r.[RecordDistance_mm] THEN 1 ELSE 0 END AS [WinAgainstCurrentRecord]
    FROM
        #Races r;

    SET @i = @i + 1;
END;

SELECT RaceID, COUNT(WinAgainstCurrentRecord) AS WaysToWin
INTO #WaysToWinPerRace
FROM #Races r
JOIN #RaceButtonHoldStrategies rs ON rs.RaceID = r.ID
WHERE rs.WinAgainstCurrentRecord = 1
GROUP BY RaceID

SELECT ROUND(EXP(SUM(LOG(WaysToWin))),0) AS MultipliedWaysToWin FROM #WaysToWinPerRace

-- Drop tables when done testing
DROP TABLE #RaceButtonHoldStrategies;
DROP TABLE #ToyBoats;
DROP TABLE #Races;
DROP TABLE #WaysToWinPerRace