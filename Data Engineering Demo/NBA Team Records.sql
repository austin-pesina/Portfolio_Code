CREATE TABLE nba.team_records(
    Season VARCHAR(20),
    full_team_name VARCHAR(255),
    Wins INT,
    Losses INT
);

INSERT INTO nba.team_records
WITH game_results AS (
	SELECT DISTINCT 
		`Date`, 
        TM, 
        place, 
        Opp, 
        LEFT(result, 1) AS Result,
        CASE
			WHEN Date BETWEEN '2021-10-19' AND '2022-06-16' THEN '2021-22'
            WHEN Date BETWEEN '2022-10-18' AND '2023-06-12' THEN '2022-23'
		END AS Season
	FROM nba.player_stats
)
SELECT 
	res.Season,
    teams.full_team_name, 
    SUM(CASE
		WHEN Result = 'W' THEN 1
		ELSE 0
		END) AS Wins,
	SUM(CASE
		WHEN Result = 'L' THEN 1
		ELSE 0
		END) AS Losses
FROM game_results res
INNER JOIN nba.teams on teams.abbr = res.TM
GROUP BY teams.full_team_name, res.Season