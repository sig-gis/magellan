(ns magellan.raster.inspect
  (:require [magellan.core :refer [crs-to-srid]]
            [tech.v3.datatype :as d]
            [tech.v3.tensor :as t])
  (:import (java.awt.image DataBuffer DataBuffer Raster)
           org.geotools.coverage.GridSampleDimension
           org.geotools.coverage.grid.GridCoverage2D
           org.geotools.geometry.GeneralEnvelope
           javax.media.jai.RenderedOp
           magellan.core.RasterInfo))

;; (set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

;;=====================================================================
;; RasterInfo inspection functions (ALPHA)
;;=====================================================================

(defn describe-image [^RenderedOp image]
  {:height (.getHeight image)
   :width  (.getWidth image)
   :bands  (.getNumBands (.getSampleModel image))
   :origin {:x (.getMinX image)
            :y (.getMinY image)}
   :tile   {:height (.getTileHeight image)
            :width  (.getTileWidth image)
            :min    {:x (.getMinTileX image)
                     :y (.getMinTileY image)}
            :max    {:x (.getMaxTileX image)
                     :y (.getMaxTileY image)}
            :total  {:x (.getNumXTiles image)
                     :y (.getNumYTiles image)}
            :offset {:x (.getTileGridXOffset image)
                     :y (.getTileGridYOffset image)}}})

(defn describe-envelope [^GeneralEnvelope envelope]
  (let [dimensions [:x :y :z]]
    (reduce (fn [acc ordinate]
              (assoc acc
                     (dimensions ordinate)
                     {:min  (.getMinimum envelope ordinate)
                      :max  (.getMaximum envelope ordinate)
                      :span (.getSpan    envelope ordinate)}))
            {}
            (range (.getDimension envelope)))))

(defn describe-band [^GridSampleDimension band]
  {:description (str (.getDescription band))
   :type        (str (.getSampleDimensionType band))
   ;; FIXME: missing band info when writing raster to disk via matrix-to-raster
   ;; :min         (.getMinimum (.getRange band))
   ;; :max         (.getMaximum (.getRange band))
   :no-data     (.getNoDataValues band)
   :offset      (.getOffset band)
   :scale       (.getScale band)
   :units       (.getUnits band)
   ;; FIXME: missing band info when writing raster to disk via matrix-to-raster
   ;; :categories  (reduce (fn [acc cat]
   ;;                        (let [range (.getRange cat)]
   ;;                          (assoc acc
   ;;                                 (str (.getName cat))
   ;;                                 {:min (.getMinimum range)
   ;;                                  :max (.getMaximum range)})))
   ;;                      {}
   ;;                      (.getCategories band))
   })

(defn describe-raster [^RasterInfo raster]
  (let [image    (describe-image (:image raster))
        envelope (describe-envelope (:envelope raster))
        bands    (mapv describe-band (:bands raster))
        srid     (crs-to-srid (:crs raster))]
    {:image    image
     :envelope envelope
     :bands    bands
     :srid     srid}))

(def datatypes #{:int32 :float32 :float64})

;; NOTE: .getSamples isn't supported for byte-array or short-array, so
;; we substitute int-array instead. If the type cannot be determined,
;; we fall back to using a double array.
(defn- get-typed-array-fn
  ([^Raster data ^Integer x ^Integer w]
   (let [data-type (.getDataType (.getDataBuffer data))
         int-fn    (fn [^Integer b ^Integer y] (.getSamples data x y w (int 1) b (int-array w)))
         float-fn  (fn [^Integer b ^Integer y] (.getSamples data x y w (int 1) b (float-array w)))
         double-fn (fn [^Integer b ^Integer y] (.getSamples data x y w (int 1) b (double-array w)))]
     (condp = data-type
       DataBuffer/TYPE_BYTE      int-fn
       DataBuffer/TYPE_USHORT    int-fn
       DataBuffer/TYPE_SHORT     int-fn
       DataBuffer/TYPE_INT       int-fn
       DataBuffer/TYPE_FLOAT     float-fn
       DataBuffer/TYPE_DOUBLE    double-fn
       DataBuffer/TYPE_UNDEFINED double-fn)))

  ([^Raster data ^Integer x ^Integer y ^Integer w ^Integer h datatype]
   (let [total-area (* w h)
         int-fn     (fn [^Integer b] (.getSamples data x y w h b (int-array total-area)))
         float-fn   (fn [^Integer b] (.getSamples data x y w h b (float-array total-area)))
         double-fn  (fn [^Integer b] (.getSamples data x y w h b (double-array total-area)))]
     (condp = (or datatype (.getDataType (.getDataBuffer data)))
       :int32                    int-fn
       :float32                  float-fn
       :float64                  double-fn
       DataBuffer/TYPE_BYTE      int-fn
       DataBuffer/TYPE_USHORT    int-fn
       DataBuffer/TYPE_SHORT     int-fn
       DataBuffer/TYPE_INT       int-fn
       DataBuffer/TYPE_FLOAT     float-fn
       DataBuffer/TYPE_DOUBLE    double-fn
       DataBuffer/TYPE_UNDEFINED double-fn))))

(defn extract-matrix [^RasterInfo raster]
  (let [image            ^RenderedOp (:image raster)
        {:keys [height
                width
                bands
                origin]} (describe-image image)
        {min-x :x
         min-y :y}       origin
        data             (.getData image)
        row->typed-array (get-typed-array-fn data min-x width)]
    (if (> bands 1)
      (into-array (for [b (range bands)]
                    (into-array (for [y (range min-y (+ min-y height))]
                                  (row->typed-array b y)))))
      (into-array (for [y (range min-y (+ min-y height))]
                    (row->typed-array 0 y))))))

(defn extract-tensor [^RasterInfo raster & options]
  (let [{:keys [convert-fn datatype]}       options
        datatype                            (get datatypes datatype)
        image                               ^RenderedOp (:image raster)
        {:keys [height width bands origin]} (describe-image image)
        {min-x :x min-y :y}                 origin
        data                                (.getData image)
        row->typed-array                    (get-typed-array-fn data min-x min-y width height datatype)
        tensor                              (if (= bands 1)
                                              (-> (row->typed-array 0)
                                                  (t/ensure-tensor)
                                                  (t/reshape [height width]))
                                              (as-> (mapv (fn [b] (d/->buffer (row->typed-array b))) (range bands)) $
                                                (d/concat-buffers $)
                                                (t/ensure-tensor $)
                                                (t/reshape $ [bands height width])))]
    (if convert-fn
      (d/copy! (d/emap convert-fn datatype tensor) tensor)
      tensor)))

(defn show-raster [raster]
  (let [^GridCoverage2D coverage (:coverage raster)]
    (.show coverage nil)))
