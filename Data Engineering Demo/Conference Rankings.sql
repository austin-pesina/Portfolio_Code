CREATE TABLE nba.conference_rankings(
    Season VARCHAR(20),
    Conference VARCHAR(10),
    full_team_name VARCHAR(255),
    Wins INT,
    Losses INT,
    Conference_Rank INT,
    Playoff_Status VARCHAR(20)
);

INSERT INTO nba.conference_rankings
SELECT 
	Season, 
    Conference, 
    r.full_team_name, 
    Wins, 
    Losses,
    # Uses RANK over DENSE_RANK to keep count going
	RANK() OVER (PARTITION BY Season, Conference ORDER BY Wins desc) AS Conference_Rank,
    CASE
		WHEN RANK() OVER (PARTITION BY Season, Conference ORDER BY Wins desc) <= 8 THEN 'Playoff Contender'
        ELSE 'Missed Playoffs'
	END AS Playoff_Status
FROM 
	nba.team_records r
INNER JOIN (
			SELECT 
				abbr, full_team_name
			FROM
				nba.teams) t ON t.full_team_name = r.full_team_name
INNER JOIN (
			SELECT *
            FROM nba.conferences
            ) c ON c.TM = t.abbr