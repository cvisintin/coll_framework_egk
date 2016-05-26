require("RPostgreSQL")
require("data.table")
require("doMC")

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

rds.count <- dbGetQuery(con,"
  SELECT
    Max(uid)
	FROM
      gis_victoria.vic_gda9455_roads_state
  ")$max

count.vec <- c(0:rds.count)

chunks <- split(count.vec, ceiling(seq_along(count.vec)/100000))


XY <- as.data.table(dbGetQuery(con,"
  SELECT
    r.uid as uid, ST_X(r.geom) AS x, ST_Y(r.geom) AS y
  FROM
	  (SELECT
      uid, ST_ClosestPoint(geom, ST_Centroid(geom)) AS geom
		FROM
      gis_victoria.vic_gda9455_roads_state) AS r
  ")) #~50 second query
setkey(XY,uid)


RDCLASS <- as.data.table(dbGetQuery(con,"
  SELECT
    uid, class_code AS rdclass
  FROM
	  gis_victoria.vic_gda9455_roads_state
  ")) #~20 second query
setkey(RDCLASS,uid)


registerDoMC(cores=detectCores()-1)
RDDENS <- as.data.table(foreach(i = 1:length(chunks), .packages="RPostgreSQL", .combine=rbind) %dopar% {
  drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
  con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab
  temp <- dbGetQuery(con,paste0("
  SELECT
    p.uid AS uid, (Sum(ST_Length(ST_Intersection(ST_Buffer(ST_ClosestPoint(p.geom, ST_Centroid(p.geom)),564),r.geom)))/1000) AS rddens
  FROM
	  gis_victoria.vic_gda9455_roads_state AS r,
	  (SELECT uid, geom FROM gis_victoria.vic_gda9455_roads_state WHERE r.uid BETWEEN ",range(chunks[i])[1]," AND ",range(chunks[i])[2],") AS p
  WHERE
    ST_DWithin(p.geom, r.geom, 564)
  GROUP BY
    p.uid
  "))
  temp
})#~XXX second query
setkey(RDDENS,uid)

# SELECT
# p.uid AS uid, (Sum(ST_Length(ST_Intersection(p.geom,r.geom)))/1000) AS rddens
# FROM gis_victoria.vic_gda9455_roads_state AS r,
# (SELECT
#  uid, ST_Buffer(ST_ClosestPoint(geom, ST_Centroid(geom)),564) AS geom
#  FROM
#  gis_victoria.vic_gda9455_roads_state) AS p
# WHERE
# ST_DWithin(p.geom, r.geom, 564)
# GROUP BY
# p.uid



registerDoMC(cores=detectCores()-1)
KMTOHWY <- as.data.table(foreach(i = 1:length(chunks), .packages="RPostgreSQL", .combine=rbind) %dopar% {
  drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
  con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab
  temp <- dbGetQuery(con,paste0("
    SELECT DISTINCT ON (r.uid)
      r.uid AS uid, ST_Distance(r.geom,p.geom)/1000 as kmtohwy
    FROM
      (SELECT
        ST_Union(geom) AS geom
      FROM
        gis_victoria.vic_gda9455_roads_state
      WHERE
        class_code = 0 OR class_code = 1) AS p,
      (SELECT
        uid, ST_ClosestPoint(geom, ST_Centroid(geom)) AS geom
      FROM
        gis_victoria.vic_gda9455_roads_state) AS r
      WHERE r.uid BETWEEN ",range(chunks[i])[1]," AND ",range(chunks[i])[2]
  ))
  temp
})#~900 second query
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


SPEEDLMT <- as.data.table(dbGetQuery(con,"
  SELECT
      r.uid as uid, mode() WITHIN GROUP (ORDER BY p.speeds) AS speedlmt
		FROM
      gis_victoria.vic_gda9455_roads_state as r, gis_victoria.vic_gda9455_roads_allspeeds AS p
    WHERE ST_DWithin(p.geom,r.geom,5)
    GROUP BY r.uid
  ")) #~10 second query
setkey(SPEEDLMT,uid)


AADT <- as.data.table(dbGetQuery(con,"
  SELECT
    r.uid AS uid, round(avg(p.aadt),0) as aadt
	FROM
    gis_victoria.vic_gda9455_roads_state as r, gis_victoria.vic_gda9455_roads_aadt AS p
  WHERE ST_DWithin(p.geom,r.geom,5)
  GROUP BY r.uid
  ")) #~1 second query
setkey(AADT,uid)


merged.data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(XY,AADT,SPEEDLMT,RDCLASS,KMTOHWY,KMTODEV,POPDENS,INCOMEPP))

write.csv(merged.data, "data/model_data_traffic.csv", row.names=FALSE)
