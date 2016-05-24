require("RPostgreSQL")
require("data.table")

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

RDCLASS <- as.data.table(dbGetQuery(con,"
  SELECT
    uid, class_code AS rdclass
  FROM
	  gis_victoria.vic_gda9455_roads_state
  ")) #~20 second query
setkey(RDCLASS,uid)


RDDENS <- as.data.table(dbGetQuery(con,"
  SELECT
    p.uid AS uid, (Sum(ST_Length(ST_Intersection(p.geom,r.geom)))/1000) AS rddens
	FROM gis_victoria.vic_gda9455_roads_state AS r,
		(SELECT
      uid, ST_Buffer(ST_ClosestPoint(geom, ST_Centroid(geom)),564) AS geom
		FROM
      gis_victoria.vic_gda9455_roads_state
    LIMIT 100) AS p
	WHERE
    ST_DWithin(p.geom, r.geom, 564)
	GROUP BY
    p.uid
	LIMIT 100
  "))
setkey(RDDENS,uid)


KMTOHWY <- as.data.table(dbGetQuery(con,"
  SELECT
    r.uid AS uid, ST_Distance(r.geom,p.geom) as kmtohwy
	FROM
    (SELECT
      uid, ST_ClosestPoint(geom, ST_Centroid(geom)) AS geom
		FROM
      gis_victoria.vic_gda9455_roads_state
    WHERE
      class_code = 0 OR class_code = 1) AS p,
		(SELECT
      uid, ST_ClosestPoint(geom, ST_Centroid(geom)) AS geom
		FROM
      gis_victoria.vic_gda9455_roads_state) AS r
	WHERE ST_Intersects(p.geom,r.geom)
  ")) #~XXX second query
setkey(KMTOHWY,uid)


KMTODEV <- as.data.table(dbGetQuery(con,"
  SELECT
    r.uid AS uid, p.kmtodev as kmtodev
	FROM gis_victoria.vic_gda9455_admin_kmtodev AS p,
		(SELECT
      uid, ST_ClosestPoint(geom, ST_Centroid(geom)) AS geom
		FROM
      gis_victoria.vic_gda9455_roads_state) AS r
	WHERE ST_Intersects(p.geom,r.geom)
  ")) #~100 second query
setkey(KMTODEV,uid)


INCOMEPP <- as.data.table(dbGetQuery(con,"
  SELECT
    r.uid AS uid, p.incomepp as incomepp
	FROM gis_victoria.vic_gda9455_demo_sa2_income AS p,
		(SELECT
      uid, ST_ClosestPoint(geom, ST_Centroid(geom)) AS geom
		FROM
      gis_victoria.vic_gda9455_roads_state) AS r
	WHERE ST_Intersects(p.geom,r.geom)
  ")) #~200 second query
setkey(INCOMEPP,uid)


POPDENS <- as.data.table(dbGetQuery(con,"
  SELECT
    r.uid AS uid, p.popdens as popdens
	FROM gis_victoria.vic_gda9455_demo_sa1_popdens AS p,
		(SELECT
      uid, ST_ClosestPoint(geom, ST_Centroid(geom)) AS geom
		FROM
      gis_victoria.vic_gda9455_roads_state) AS r
	WHERE ST_Intersects(p.geom,r.geom)
  ")) #~90 second query
setkey(POPDENS,uid)
