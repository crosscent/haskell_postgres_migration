CREATE TABLE geomap_place(
    ID INT PRIMARY  KEY                             NOT NULL,
    DISPLAY_NAME    CHAR(256)                       NOT NULL,
    OSM_ID          INT                             NOT NULL,
    PLACE INT REFERENCES geomap_place_polygon (id)  NOT NULL,
    OSM_TYPE        CHAR(256)                       NOT NULL,
    LAST_UPDATED    TIMESTAMP                       NOT NULL
);
