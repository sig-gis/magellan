(ns magellan.core
  (:require [schema.core :as s]
            [clojure.java.io :as io]
            [tech.v3.datatype :as d]
            [tech.v3.tensor :as t])
  (:import (java.net URL)
           (org.geotools.coverage.grid GridCoverageFactory
                                       GridGeometry2D
                                       RenderedSampleDimension
                                       GridCoverage2D)
           (org.geotools.coverage.grid.io GridFormatFinder AbstractGridFormat)
           (org.geotools.coverage GridSampleDimension)
           (org.geotools.referencing CRS ReferencingFactoryFinder)
           (org.geotools.referencing.factory PropertyAuthorityFactory
                                             ReferencingFactoryContainer)
           (org.geotools.referencing.operation.projection MapProjection)
           (org.geotools.metadata.iso.citation Citations)
           (org.geotools.util.factory Hints)
           (org.geotools.geometry GeneralEnvelope Envelope2D)
           (org.geotools.coverage.processing Operations)
           (org.geotools.gce.geotiff GeoTiffWriter GeoTiffWriteParams)
           (org.opengis.referencing.crs CoordinateReferenceSystem)
           javax.media.jai.RenderedOp
           (org.opengis.parameter GeneralParameterValue)))

(s/defrecord RasterInfo
    [coverage   :- GridCoverage2D
     image      :- RenderedOp
     crs        :- CoordinateReferenceSystem
     projection :- (s/maybe MapProjection)
     envelope   :- GeneralEnvelope
     grid       :- GridGeometry2D
     width      :- s/Int
     height     :- s/Int
     bands      :- [RenderedSampleDimension]])

;; TODO Add :envelope-2d (.getEnvelope2D coverage)
;; TODO Add a function that converts a simple Clojure map of literals into a RasterInfo record
(s/defn to-raster :- RasterInfo
  [coverage :- GridCoverage2D]
  (let [image (.getRenderedImage coverage)
        crs   (.getCoordinateReferenceSystem coverage)
        grid  (.getGridGeometry coverage)]
    (map->RasterInfo
     {:coverage   coverage
      :image      image
      :crs        crs
      :projection (CRS/getMapProjection crs)
      :envelope   (.getEnvelope coverage)
      :grid       grid
      :width      (.getWidth image)
      :height     (.getHeight image)
      :bands      (vec (.getSampleDimensions coverage))})))

(s/defn read-raster :- RasterInfo
  [filename :- s/Str]
  (let [file (io/file filename)]
    (if (.exists file)
      (try (-> file
               (GridFormatFinder/findFormat)
               (.getReader file)
               (.read nil)
               (to-raster))
           (catch Exception e
             (println "Cannot read raster. Exception:" (class e))))
      (println "Cannot read raster. No such file:" filename))))

(s/defn srid-to-crs :- CoordinateReferenceSystem
  [srid-code :- s/Str]
  (CRS/decode srid-code))

(s/defn wkt-to-crs :- CoordinateReferenceSystem
  [wkt :- s/Str]
  (CRS/parseWKT wkt))

(s/defn crs-to-srid :- s/Str
  [crs :- CoordinateReferenceSystem]
  (CRS/lookupIdentifier crs true))

(s/defn crs-to-wkt :- s/Str
  [crs :- CoordinateReferenceSystem]
  (.toWKT crs))

(s/defn register-new-crs-definitions-from-properties-file! :- s/Any
  [authority-name :- s/Str
   filename       :- s/Str]
  (let [^URL url (if (instance? URL filename)
                   filename
                   (io/as-url (io/file filename)))]
    (ReferencingFactoryFinder/addAuthorityFactory
     (PropertyAuthorityFactory.
      (ReferencingFactoryContainer.
       (Hints. Hints/CRS_AUTHORITY_FACTORY PropertyAuthorityFactory))
      (Citations/fromName authority-name)
      url)))
  (ReferencingFactoryFinder/scanForPlugins))

(s/defn make-envelope :- Envelope2D
  [srid       :- s/Str
   upperleftx :- Double
   upperlefty :- Double
   width      :- Double
   height     :- Double]
  (Envelope2D. (srid-to-crs srid) upperleftx upperlefty width height))

(defn- matrix-to-float-array-2d
  "Converts the supplied matrix data to a primitive Java float[][] (array of arrays)."
  ^"[[F" [matrix]
  ;; NOTE why use (cond ...), rather than polymorphic dispatch, (Val, 26 Jul 2022)
  ;; such as an IToFloat2DArray protocol? Several reasons:
  ;; 1. Expediency
  ;; 2. Maintenance: before committing to expose
  ;; an open-to-extension protocol for Magellan users to implement,
  ;; let's conservatively wait until they ask for it,
  ;; rather than adding to the maintenance burden. YAGNI.
  ;; 3. Robustness to deps upgrades:
  ;; libraries like tech.v3.datatype are not always
  ;; super stable wrt the Java types they expose. Predicates such as
  ;; t/tensor? are probably more reliable.
  (cond
    (t/tensor? matrix)
    (->> (t/rows matrix)
         (mapv d/->float-array)
         (into-array))

    :else
    ;; WARNING: this code path is much less efficient than the above,
    ;; due to the overhead of primitive number boxing and lazy seqs.
    ;; Experiments showed a >4x increase in execution time.
    (into-array (map float-array matrix))))

(s/defn float-2d-array-to-raster :- GridCoverage2D
  "Like matrix-to-raster, but lets callers supply the data
  directly as a Java float[][], in case matrix-to-raster
  does not know how to do that."
  [name         :- s/Str
   float-matrix :- (Class/forName "[[F")
   envelope     :- Envelope2D]
  (to-raster (.create (GridCoverageFactory.)
               ^String name
               ;; Yes, this type hint seems redundant,
               ;; but it's required against reflection warnings.
               ^"[[F" float-matrix
               envelope)))

(s/defn matrix-to-raster :- GridCoverage2D
  [name     :- s/Str
   matrix   :- [[double]]
   envelope :- Envelope2D]
  (-> matrix
      (matrix-to-float-array-2d)
      (as-> float-matrix (float-2d-array-to-raster name float-matrix envelope))))

;; FIXME: Throws a NoninvertibleTransformException when reprojecting to EPSG:4326.
(s/defn reproject-raster :- RasterInfo
  [raster :- RasterInfo
   crs    :- CoordinateReferenceSystem]
  (to-raster (.resample Operations/DEFAULT (:coverage raster) crs)))

(s/defn resample-raster :- RasterInfo
  [raster :- RasterInfo
   grid   :- GridGeometry2D]
  (to-raster (.resample Operations/DEFAULT (:coverage raster) nil grid nil)))

(s/defn crop-raster :- RasterInfo
  [raster   :- RasterInfo
   envelope :- GeneralEnvelope]
  (to-raster (.crop Operations/DEFAULT ^GridCoverage2D (:coverage raster) envelope)))

;; FIXME: Throws a NullPointerException when writing a resampled coverage.
;; FIXME: Parameterize the compression and tiling operations.
;; REFERENCE: http://svn.osgeo.org/geotools/trunk/modules/plugin/geotiff/src/test/java/org/geotools/gce/geotiff/GeoTiffWriterTest.java
(s/defn write-raster :- s/Any
  [raster   :- RasterInfo
   filename :- s/Str]
  (let [writer (GeoTiffWriter. (io/file filename))
        params (-> writer
                   (.getFormat)
                   (.getWriteParameters))]
    (-> params
        (.parameter (str (.getName AbstractGridFormat/GEOTOOLS_WRITE_PARAMS)))
        (.setValue (doto (GeoTiffWriteParams.)
                     (.setCompressionMode GeoTiffWriteParams/MODE_EXPLICIT)
                     (.setCompressionType "LZW")
                     (.setCompressionQuality 0.5)
                     (.setTilingMode GeoTiffWriteParams/MODE_EXPLICIT)
                     (.setTiling 256 16))))
    ;; Write the GeoTIFF to disk
    (try (.write writer
                 (:coverage raster)
                 (into-array GeneralParameterValue (.values params)))
         (catch Exception e
           (println "Cannot write raster. Exception:" (class e))))))

(s/defn raster-band-stats :- {:min s/Num :max s/Num :nodata (s/maybe s/Num)}
  [raster   :- RasterInfo
   band-num :- s/Int]
  (let [^GridSampleDimension band (nth (:bands raster) band-num)]
    {:min    (.getMinimumValue band)
     :max    (.getMaximumValue band)
     :nodata (.getNoDataValues band)}))
