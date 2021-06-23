SELECT * FROM portfolio_project..covid_deaths
WHERE continent is not null
ORDER BY 3,4

--SELECT * FROM portfolio_project..covid_vaccinations
--ORDER BY 3,4



-- Select data we are going to use
SELECT location, date, total_cases, new_cases, total_deaths, population 
FROM portfolio_project..covid_deaths
ORDER BY 1,2



-- Looking at total cases vs total deaths
-- Shows likelihood of dying if you contract covid in your country
SELECT location, date, total_cases, total_deaths,(total_deaths/total_cases)*100 as death_percentage
FROM portfolio_project..covid_deaths
WHERE location like '%states%'
ORDER BY 1,2



-- Lookin at total cases vs population
-- Shows what percentage of population got covid
SELECT location, date, population, total_cases,(total_cases/population)*100 as infection_percentage
FROM portfolio_project..covid_deaths
--WHERE location like '%states%'
ORDER BY 1,2


-- Looking at countries with highest infection rate compared to population
SELECT location, population, MAX(total_cases) as highest_infection_count, MAX((total_cases/population))*100 as infection_percentage
FROM portfolio_project..covid_deaths
--WHERE location like '%states%'
GROUP BY location, population
ORDER BY infection_percentage desc


-- Showing countries with highest death count per pop
SELECT location, MAX(cast(total_deaths as int)) as total_death_count
FROM portfolio_project..covid_deaths
--WHERE location like '%states%'
WHERE continent is not null
GROUP BY location
ORDER BY total_death_count desc



-- LET'S BREAK THINGS DOWN BY CONTINENT

-- Showing continents with the highest death count per population
SELECT continent, MAX(cast(total_deaths as int)) as total_death_count
FROM portfolio_project..covid_deaths
--WHERE location like '%states%'
WHERE continent is not null
GROUP BY continent
ORDER BY total_death_count desc


-- GLOBAL NUMBERS

SELECT date, SUM(new_cases) as total_cases, SUM(cast(new_deaths as int)) as total_deaths,
	SUM(cast(new_deaths as int))/SUM(new_cases)*100 as death_percentage
FROM portfolio_project..covid_deaths
--WHERE location like '%states%'
WHERE continent is not null
GROUP BY date
ORDER BY 1,2



--Looking at Total Population vs Vaccinations

SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
	SUM(CONVERT(int, vac.new_vaccinations)) OVER (PARTITION BY dea.location ORDER BY dea.location, dea.date)
	as rolling_people_vaccinated
FROM portfolio_project..covid_deaths dea
JOIN portfolio_project..covid_vaccinations vac
	ON dea.location = vac.location AND dea.date = vac.date
WHERE dea.continent IS NOT NULL
ORDER BY 2,3


-- USE CTE
WITH pop_vs_vac (Continent, location, date, population, new_vaccinations, rolling_people_vaccinated)
as
(
SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
	SUM(CONVERT(int, vac.new_vaccinations)) OVER (PARTITION BY dea.location ORDER BY dea.location, dea.date)
	as rolling_people_vaccinated
FROM portfolio_project..covid_deaths dea
JOIN portfolio_project..covid_vaccinations vac
	ON dea.location = vac.location AND dea.date = vac.date
WHERE dea.continent IS NOT NULL
)
SELECT *, (rolling_people_vaccinated/population)*100
FROM pop_vs_vac



--TEMP TABLE
DROP TABLE if exists #percent_population_vaccinated
CREATE TABLE #percent_population_vaccinated
(
continent nvarchar(255),
location nvarchar(255),
date datetime,
population numeric,
new_vaccinations numeric,
rolling_people_vaccinated numeric
)

Insert into #percent_population_vaccinated
SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
	SUM(CONVERT(int, vac.new_vaccinations)) OVER (PARTITION BY dea.location ORDER BY dea.location, dea.date)
	as rolling_people_vaccinated
FROM portfolio_project..covid_deaths dea
JOIN portfolio_project..covid_vaccinations vac
	ON dea.location = vac.location AND dea.date = vac.date
WHERE dea.continent IS NOT NULL

SELECT *, (rolling_people_vaccinated/population)*100
FROM #percent_population_vaccinated


-- Creating View to store data for later visualizations

CREATE VIEW percent_population_vaccinated as
SELECT dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
	SUM(CONVERT(int, vac.new_vaccinations)) OVER (PARTITION BY dea.location ORDER BY dea.location, dea.date)
	as rolling_people_vaccinated
FROM portfolio_project..covid_deaths dea
JOIN portfolio_project..covid_vaccinations vac
	ON dea.location = vac.location AND dea.date = vac.date
WHERE dea.continent IS NOT NULL


SELECT *
FROM percent_population_vaccinated