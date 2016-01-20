######################################
#Create the main data we will be using
######################################
USE meteo;

#Create the table to be used for analysis
create table visit (
 ruid VARCHAR(40) DEFAULT NULL,
 visitdate DATETIME DEFAULT NULL,
 url TEXT DEFAULT NULL,
 quotation INT UNSIGNED DEFAULT NULL,
 quotationid BIGINT UNSIGNED DEFAULT NULL,
 amount INT UNSIGNED DEFAULT NULL,
 quotationae INT UNSIGNED DEFAULT NULL,
 eulerianid BIGINT UNSIGNED DEFAULT NULL,
 productid VARCHAR(30) DEFAULT NULL,
 city VARCHAR(100) DEFAULT NULL,
 weathertype VARCHAR(30) DEFAULT NULL,
 temp INT DEFAULT NULL,
 pressure INT DEFAULT NULL,
 humidity INT DEFAULT NULL,
 KEY Idxruid (ruid),
 KEY Idxquotation (quotation),
 KEY Idxproductid (productid),
 KEY Idxcity (city),
 KEY Idxweathertype (weathertype))
ENGINE = MyISAM;

LOAD DATA LOCAL INFILE 'C:/Program Files/MySQL/MySQL Server 5.6/data/zsk_yob_visites_finales.csv' 
INTO TABLE visit 
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(@ruid, @visitdate, @url, @quotation, @quotationid, @amount, @quotationae, @eulerianid, @productid, @city, @weathertype, @temp, @pressure, @humidity)
SET ruid = if(@ruid='',NULL,@ruid),
	visitdate = if(@visitedate='',NULL,@visitdate),
 url = if(@url='',NULL,@url),
 quotation = if(@quotation='',NULL,@quotation),
 quotationid = if(@quotationid = '',NULL,@quotationid),
 amount = if(@amount='',NULL,@amount),
 quotationae = if(@quotationae='',NULL,@quotationae),
 eulerianid = if(@eulerianid='',NULL,@eulerianid),
 productid = if(@productid='',NULL,@productid),
 city = if(@city='',NULL,@city),
 weathertype = if(@weathertype='',NULL,@weathertype),
 temp = if(@temp='',NULL,@temp),
 pressure = if(@pressure='',NULL,@pressure),
 humidity = if(@humidity='',NULL,@humidity);


create table print (
 auctionid BIGINT UNSIGNED DEFAULT NULL,
 ruid VARCHAR(40) DEFAULT NULL,
 printdate DATETIME DEFAULT NULL,
 campaignid INT UNSIGNED DEFAULT NULL,
 lineitemid INT UNSIGNED DEFAULT NULL,
 paid FLOAT(11,8) UNSIGNED DEFAULT NULL,
 bid FLOAT(11,8) UNSIGNED DEFAULT NULL,
 average FLOAT(11,8) UNSIGNED DEFAULT NULL,
 appnexusid BIGINT UNSIGNED DEFAULT NULL,
 reserveprice FLOAT(11,8) UNSIGNED DEFAULT NULL,
 size VARCHAR(15) DEFAULT NULL,
 placementid INT UNSIGNED DEFAULT NULL,
 sellerid INT UNSIGNED DEFAULT NULL,
 publisherid INT UNSIGNED DEFAULT NULL,
 creaid INT UNSIGNED DEFAULT NULL,
 url TEXT DEFAULT NULL,
 click SMALLINT UNSIGNED DEFAULT NULL,
 city VARCHAR(100) DEFAULT NULL,
 weathertype VARCHAR(30) DEFAULT NULL,
 temp INT DEFAULT NULL,
 pressure INT DEFAULT NULL,
 humidity INT DEFAULT NULL,
 KEY Idxruid (ruid),
 KEY Idxclick (click),
 KEY Idxcity (city),
 KEY Idxauctionid (auctionid))
ENGINE = MyISAM;



LOAD DATA INFILE 'C:/Program Files/MySQL/MySQL Server 5.6/data/zsk_yob_imps_villesprincipales.csv' 
INTO TABLE print 
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS
(@auctionid, @ruid, @printdate, @campaignid, @lineitemid, @paid, @bid, @average, @appnexusid, @reserveprice,@size, @placementid, @sellerid, @publisherid, @creaid, @url, @click, @city, @weathertype, @temp, @pressure, @humidity)
SET auctionid = if(@auctionid='',NULL,@auctionid),
	ruid = if(@ruid='',NULL,@ruid),
	printdate = if(@printdate='',NULL,@printdate),
    campaignid = if(@campaignid='',NULL,@campaignid),
    lineitemid = if(@lineitemid='',NULL,@lineitemid),
    paid = if(@paid='',NULL,@paid),
    bid = if(@bid='',NULL,@bid),
    average = if(@average='',NULL,@average),
    appnexusid = if(@appnexusid='',NULL,@appnexusid),
    reserveprice = if(@reserveprice='',NULL,@reserveprice),
    size = if(@size='',NULL,@size),
	placementid = if(@placementid='',NULL,@placementid),
	sellerid = if(@sellerid='',NULL,@sellerid),
	publisherid = if(@publisherid='',NULL,@publisherid),
    creaid = if(@creaid='',NULL,@creaid),
    url = if(@url='',NULL,@url),
    click = if(@click='',NULL,@click),
	city = if(@city='',NULL,@city),
	weathertype = if(@weathertype='',NULL,@weathertype),
	temp = if(@temp='',NULL,@temp),
	pressure = if(@pressure='',NULL,@pressure),
	humidity = if(@humidity='',NULL,@humidity);

ALTER TABLE print ADD printid INT PRIMARY KEY AUTO_INCREMENT;
ALTER TABLE print ADD INDEX Idxprintid (printid);


 create table cityweather (
 cityid INT UNSIGNED DEFAULT NULL,
 city VARCHAR(100) DEFAULT NULL,
 lat DECIMAL(13,9) DEFAULT NULL,
 lon DECIMAL(13,9) DEFAULT NULL,
 countrycode VARCHAR(3) DEFAULT NULL,
 weatherdate INT UNSIGNED DEFAULT NULL,
 weatherid INT UNSIGNED DEFAULT NULL,
 temp INT DEFAULT NULL,
 pressure INT DEFAULT NULL,
 humidity INT DEFAULT NULL,
 tempmin INT DEFAULT NULL,
 tempmax INT DEFAULT NULL,
 KEY Idxcity (city),
 KEY Idxlat (lat),
 KEY Idxlon (lon),
 KEY Idxcountrycode (countrycode),
 KEY Idxweatherdate (weatherdate))
ENGINE = MyISAM;


LOAD DATA LOCAL INFILE 'C:/Program Files/MySQL/MySQL Server 5.6/data/meteo_yob_220150630.csv' 
INTO TABLE cityweather
FIELDS TERMINATED BY ';' 
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 0 ROWS
(@cityid, @city, @lat, @lon, @countrycode, @weatherdate, @weatherid, @temp, @pressure, @humidity, @tempmin, @tempmax)
SET cityid = if(@cityid='',NULL,@cityid),
	city = if(@city='',NULL,@city),
	lat = if(@lat='',NULL,@lat),
	lon = if(@lon='',NULL,@lon),
	countrycode = if(@countrycode = '',NULL,@countrycode),
	weatherdate = if(@weatherdate='',NULL,@weatherdate),
	weatherid = if(@weatherid='',NULL,@weatherid),
	temp = if(@temp='',NULL,@temp),
	pressure = if(@pressure='',NULL,@pressure),
	humidity = if(@humidity='',NULL,@humidity),
	tempmin = if(@tempmin='',NULL,@tempmin),
	tempmax = if(@tempmax='',NULL,@tempmax);

#Create a useful subtable with the data structured as needed and with only french cities
CREATE TABLE cityweatherfr
	SELECT 	city,
			lat, 
            lon, 
            FROM_UNIXTIME(weatherdate) as weatherdate, 
            temp, 
            pressure, 
            humidity
	FROM	cityweather
    WHERE	countrycode = 'FR' AND NOT(ISNULL(city))
    ORDER BY city, weatherdate;

#Limit the data to the needed period
CREATE TABLE cityweatherfrsub
	SELECT 	city,
			lat, 
            lon, 
            weatherdate, 
            temp, 
            pressure, 
            humidity
	FROM	cityweatherfr
    WHERE	weatherdate >= '2014-12-01 00:00:01' AND weatherdate <= '2015-03-01 00:00:01'
    ORDER BY weatherdate, city;

CREATE TABLE visitsub
	SELECT 	ruid,
			IF(productid LIKE '%PBAUTO%', 'PBAUTO', IF(productid LIKE '%PBMOTO%', 'PBMOTO', IF(productid LIKE '%PBPERSO%', 'PBPERSO', IF(productid LIKE '%PBTRAV%', 		'PBTRAV', IF(productid LIKE '%PBRACHAT%', 'PBRACHAT', IF(productid LIKE '%REV%', 'REV', IF(ISNULL(productid), NULL, 'OTHER' ))))))) as productid,
			visitdate,
			minute(visitdate) as minute,
			hour(visitdate) as hour,
			weekday(visitdate) as weekday,
			quotation,
			amount,
			city,
			weathertype,
			temp,
			pressure,
			humidity
		FROM visit
		WHERE NOT(ISNULL(city)) AND NOT(ISNULL(weathertype))
        ORDER BY city, visitdate;

#Adding dummy variables for all product types        
ALTER TABLE visitsub 	ADD COLUMN	pbauto 		tinyint,
						ADD COLUMN	pbmoto 		tinyint,
						ADD COLUMN  pbperso 	tinyint,
						ADD COLUMN	pbtrav 		tinyint,
						ADD COLUMN  rev 		tinyint,
						ADD COLUMN  pbrachat 	tinyint,
						ADD COLUMN	other 		tinyint,
						ADD COLUMN  clouds 		tinyint,
						ADD COLUMN  rain	 	tinyint,
						ADD COLUMN	drizzle		tinyint,
						ADD COLUMN  atmosphere	tinyint,
						ADD COLUMN  snow	 	tinyint,
						ADD COLUMN	thunderstorm tinyint;

UPDATE visitsub SET pbauto=IF(productid='PBAUTO','1','0'), 
					pbmoto=IF(productid='PBMOTO','1','0'), 
					pbperso=IF(productid='PBPERSO','1','0'), 
                    pbtrav=IF(productid='PBTRAV','1','0'), 
                    pbrachat=IF(productid='PBRACHAT','1','0'), 
                    rev=IF(productid='REV','1','0'), 
                    other=IF(productid='OTHER','1','0'),
                    clouds=IF(weathertype='Clouds','1','0'),
                    rain=IF(weathertype='Rain','1','0'),
                    drizzle=IF(weathertype='Drizzle','1','0'),
                    atmosphere=IF(weathertype='Atmosphere','1','0'),
                    snow=IF(weathertype='Snow','1','0'),
                    thunderstorm=IF(weathertype='Thunderstorm','1','0');

ALTER TABLE cityweatherfrsub ADD INDEX Idx_date (weatherdate);
ALTER TABLE cityweatherfrsub ADD INDEX Idx_city (city);
ALTER TABLE cityweatherfrsub ADD INDEX Idx_citydate (city, weatherdate);
ALTER TABLE visitsub ADD INDEX Idxcitydate (city, visitdate);
ALTER TABLE visitsub ADD INDEX Idxdate (visitdate);

CREATE TABLE intermedbefore
	SELECT t.ruid, t.city, t.visitdate, amount, pbauto, pbmoto, pbperso, pbtrav, pbrachat, rev, other, t.weathertype, clouds, rain, drizzle, atmosphere, snow, 			 thunderstorm, d.weatherdate
		FROM visitsub as t 
		JOIN cityweatherfrsub as d
		ON d.city=t.city AND d.weatherdate <= t.visitdate AND d.weatherdate + interval 1 day >= t.visitdate;


ALTER TABLE intermedbefore ADD INDEX Idxcity (city);
ALTER TABLE intermedbefore ADD INDEX IdxVdate (visitdate);
ALTER TABLE intermedbefore ADD INDEX IdxWdate (weatherdate);

CREATE TABLE beforedatetable
	SELECT ruid, city, visitdate, max(weatherdate) as beforedate, amount, pbauto, pbmoto, pbperso, pbtrav, pbrachat, rev, other, weathertype, clouds, rain, drizzle, atmosphere, snow, thunderstorm
		FROM intermedbefore
        GROUP BY ruid, city, visitdate;

CREATE TABLE intermedafter
	SELECT t.ruid, t.city, t.visitdate, d.weatherdate
		FROM visitsub as t 
		JOIN cityweatherfrsub as d
		ON d.city=t.city AND d.weatherdate >= t.visitdate AND t.visitdate + interval 1 day >= d.weatherdate;

ALTER TABLE intermedafter ADD INDEX Idxcity (city);
ALTER TABLE intermedafter ADD INDEX IdxVdate (visitdate);
ALTER TABLE intermedafter ADD INDEX IdxWdate (weatherdate);

CREATE TABLE afterdatetable
	SELECT ruid, city, visitdate, min(weatherdate) as afterdate 
		FROM intermedafter
        GROUP BY city, visitdate;


ALTER TABLE afterdatetable ADD INDEX Idxcity (city);
ALTER TABLE afterdatetable ADD INDEX IdxVdate (visitdate);
ALTER TABLE afterdatetable ADD INDEX Idxruid (ruid);
ALTER TABLE beforedatetable ADD INDEX Idxcity (city);
ALTER TABLE beforedatetable ADD INDEX IdxVdate (visitdate);
ALTER TABLE beforedatetable ADD INDEX Idxruid (ruid);


CREATE TABLE aftbefdatetable
	SELECT t.ruid, t.city, t.visitdate, t.beforedate, d.afterdate, amount, t.pbauto, t.pbmoto, t.pbperso, t.pbtrav, t.pbrachat, t.rev, t.other, t.weathertype, t.clouds, t.rain, t.drizzle, t.atmosphere, t.snow, t.thunderstorm
		FROM 	beforedatetable t
        JOIN 	afterdatetable d
		ON		t.ruid = d.ruid AND t.city = d.city AND t.visitdate = d.visitdate;

ALTER TABLE aftbefdatetable ADD INDEX Idxruid (ruid);
ALTER TABLE aftbefdatetable ADD INDEX Idxcity (city);
ALTER TABLE aftbefdatetable ADD INDEX Idxbefdate (beforedate);
ALTER TABLE aftbefdatetable ADD INDEX Idxaftdate (afterdate);

CREATE TABLE visitweather
	SELECT 		t.ruid,
				t.city, 
				t.visitdate,
                HOUR(t.visitdate) as hour,
                MINUTE(t.visitdate) as minute,
                WEEKDAY(t.visitdate) as weekday,
                t.beforedate, 
                t.afterdate,
                amount,
                timestampdiff(minute, t.visitdate, t.beforedate)/timestampdiff(minute, t.afterdate, t.beforedate)*(p.temp - d.temp) + d.temp as temp, 
                timestampdiff(minute, t.visitdate, t.beforedate)/timestampdiff(minute, t.afterdate, t.beforedate)*(p.pressure - d.pressure) + d.pressure as pressure, 
                timestampdiff(minute, t.visitdate, t.beforedate)/timestampdiff(minute, t.afterdate, t.beforedate)*(p.humidity - d.humidity) + d.humidity as humidity,
                t.pbauto, t.pbmoto, t.pbperso, t.pbtrav, t.pbrachat, t.rev, t.other, 
                t.weathertype, t.clouds, t.rain, t.drizzle, t.atmosphere, t.snow, t.thunderstorm
		FROM aftbefdatetable as t
        JOIN cityweatherfrsub as d
        ON t.city = d.city AND t.beforedate = d.weatherdate
        JOIN cityweatherfrsub as p
        ON t.city = p.city AND t.afterdate = p.weatherdate;

#Deleting 323 rows that have NULL values for Temperature, Humidity and Pressure. 
#I did not investigate why because it is only 323 rows. Mayble later :)
DELETE FROM visitweather WHERE temp IS NULL;

ALTER TABLE visitweather ADD COLUMN	quotation tinyint;
UPDATE visitweather SET quotation = pbauto + pbmoto + pbperso + pbtrav + pbrachat + rev + other;

#Create balanced printsub table from the print table
	#Select all the line from print table with click = 1
    CREATE TABLE printsub1
		SELECT ruid, printdate, paid, bid, average, click, city
			FROM print
            WHERE click = '1';
	
    select count(ruid) from print;
	#Select random lines from the print table
	CREATE TABLE printsub0	
		SELECT ruid, printdate, paid, bid, average, click, city
		  FROM print JOIN
			   (SELECT CEIL(RAND() *
							(SELECT MAX(printid)
							   FROM print
                               WHERE click ='0')) AS printid
				) AS r2
			   USING (printid)
		   LIMIT 10;

   CREATE PROCEDURE printfill_0 (n INT)
		BEGIN
			WHILE (n<80000)
				DO 
					SELECT ruid, printdate, paid, bid, average, click, city
					  FROM print JOIN
						   (SELECT CEIL(RAND() *
										(SELECT MAX(printid)
										   FROM print
										   WHERE click ='0')) AS printid
							) AS r2
					USING (printid)
			END WHILE;
		END;
					


#Analyse data per weathertype
CREATE TABLE weathertype
SELECT 	weathertype, 
		count(visitdate) as nbvisit, 
        sum(quotation)/count(visitdate) + 0.00000000 as quotationrate, 
        (sum(quotation)/count(visitdate))*(1-sum(quotation)/count(visitdate))/count(visitdate) as quotationvariance, 
        POW((sum(quotation)/count(visitdate))*(1-sum(quotation)/count(visitdate))/count(visitdate), 0.5) as quotatiostd, 
        sum(quotation)/count(visitdate) + POW((sum(quotation)/count(visitdate))*(1-sum(quotation)/count(visitdate))/count(visitdate), 0.5)*(1-0.95)/2 as quotationSupInterval, 
        sum(quotation)/count(visitdate) - POW((sum(quotation)/count(visitdate))*(1-sum(quotation)/count(visitdate))/count(visitdate), 0.5)*(1-0.95)/2 as quotationInfInterval, 
        sum(amount)/sum(quotation) + 0.00 as avgamount
FROM visitweather
GROUP BY weathertype;
     
SELECT *
FROM visitweather
WHERE (temp>-20) AND (temp<25) and (humidity<=100) AND (quotation=1) AND (NOT(amount IS NULL));
	
    
SELECT amount, count(amount)
FROM visitweather
WHERE quotation=1 and pbmoto=1
GROUP BY amount
ORDER BY amount;

SELECT * from cityweatherfr
	WHERE date(weatherdate)>'2014-12-21' AND date(weatherdate)<'2014-12-23';

SELECT weathertype
          FROM visitweather
          WHERE (temp>-20) AND (temp<25) AND (humidity<=100)
          GROUP BY weathertype;