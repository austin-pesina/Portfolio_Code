CREATE TABLE nba.player_season_stats(
    Season VARCHAR(20),
    Player VARCHAR(50),
    player_key VARCHAR(10),
    TM VARCHAR(3),
    Total_FG INT, Total_FGA INT, `FG%` float,
    Total_3P INT, Total_3PA INT, `3P%` float,
    Total_FT INT, Total_FTA INT, `FT%` float,
    Total_ORB INT, Total_DRB INT, Total_RB INT,
    Total_AST INT, Total_STL INT, Total_BLK INT,
    Total_TOV INT,
    Total_PF INT,
    Total_PTS INT
);

INSERT INTO nba.player_season_stats
SELECT 
	CASE
		WHEN Date BETWEEN '2021-10-19' AND '2022-06-16' THEN '2021-22'
		WHEN Date BETWEEN '2022-10-18' AND '2023-06-12' THEN '2022-23'
	END AS Season,
    Player, pl.player_key, TM,
    SUM(FG) AS Total_FG, SUM(FGA) AS Total_FGA, 
    CASE
		WHEN SUM(FGA) = 0 THEN NULL
        ELSE ROUND((SUM(FG)/SUM(FGA))*100,2)
	END AS `FG%`,
    SUM(`3P`) as Total_3P, SUM(`3PA`) AS Total_3PA, 
    CASE
		WHEN SUM(`3PA`) = 0 THEN NULL
        ELSE ROUND((SUM(`3P`)/SUM(`3PA`))*100,2) 
	END AS `3P%`,
    SUM(FT) AS Total_FT, SUM(FTA) AS Total_FTA, 
    CASE
		WHEN SUM(FTA) = 0 THEN NULL
        ELSE ROUND((SUM(FT)/SUM(FTA))*100,2) 
	END AS `FT%`,
    SUM(ORB) AS Total_ORB, SUM(DRB) AS Total_DRB, SUM(TRB) AS Total_RB,
    SUM(AST) AS Total_AST, SUM(STL) AS Total_STL,
    SUM(BLK) AS Total_BLK, SUM(TOV) AS Total_TOV,
    SUM(PF) AS Total_PF, SUM(PTS) AS Total_PTS
FROM 
	nba.player_stats ps
INNER JOIN (SELECT 
				name,
                player_key
            FROM 
				nba.player_list) pl ON pl.name = ps.player
GROUP BY
	Player, TM, Season, player_key
ORDER BY Season, TM, Player