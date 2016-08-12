CREATE EXTENSION postgis;

CREATE TABLE geomap_place_polygon(
    ID INT PRIMARY KEY                              NOT NULL,
    PLACE           CHAR(256)                       NOT NULL,
    POLYGON         GEOGRAPHY(MULTIPOLYGON,4326)    NOT NULL,
    LAST_UPDATED    TIMESTAMP                       NOT NULL
);
