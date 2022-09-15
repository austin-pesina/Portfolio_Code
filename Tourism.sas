libname cr '/home/u49643936/ECRB94/data';

/* Clean Data; Pt 1 */

data tourism_cleaned (drop=_1995-_2013);
	set cr.tourism;
	length Country_Name $300 Tourism_Type $20;
	retain Country_Name "" Tourism_Type "";
	if A ne . then Country_Name=Country;
	if lowcase(Country)="inbound tourism" then Tourism_Type="Inbound tourism";
	else if lowcase(Country)='outbound tourism' then Tourism_Type="Outbound tourism";
	if Country_Name ne Country and Country ne Tourism_Type;
	
/* Series/Conversion Pt 2	 */

	Series=upcase(Series);
	if Series=".." then Series="";
	ConversionType=strip(scan(country,-1,' '));
	if _2014=".." then _2014=".";
	
/* Y2014 Pt 3 */

	if ConversionType = 'Mn' then do;
		if input(_2014,16.) ne . then Y2014=input(_2014,16.)*1000000;
		else Y2014=.;
	end;
	else if ConversionType = 'Thousands' then do;
		if input(_2014,16.) ne . then Y2014=input(_2014,16.)*1000;
		else Y2014=.;
	end;
	if ConversionType = 'Mn' then Category=cat(scan(country,1,'-','r')," -US$");
	else if ConversionType = 'Thousands' then Category=scan(country,1,'-','r');
	format Y2014 comma25.;
	drop A ConversionType Country _2014;
run;



proc format;
	value continents 1 = "North America"
					 2 = "South America"
					 3 = "Europe"
					 4 = "Africa"
					 5 = "Asia"
					 6 = "Oceania"
					 7 = "Antarctica";
run;



/* Final_Tourism Table */

proc sort data=cr.country_info out=Country_Sorted(rename=(Country=Country_Name));
	by Country;
run;


data final_tourism NoCountryFound(keep=Country_Name); 
	merge tourism_cleaned(in=t) Country_Sorted(in=c);
	by Country_Name;
	if t=1 and c=1 then output Final_Tourism;
	if (t=1 and c=0) and first.country_name then output NoCountryFound;
	format Continent continents.;
run;


/* Questions */

proc means data=final_tourism;
	class Category Continent;
	var Y2014;
run;


proc means data=final_tourism;
	class Category;
	var Y2014;
run;
