(ns magellan.raster.inspect
  (:require [magellan.core    :refer [crs-to-srid]]
            [tech.v3.datatype :as d]
            [tech.v3.tensor   :as t])
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
(def ^:private raster-datatype->default-tensor-dtype
  {DataBuffer/TYPE_BYTE      :int32
   DataBuffer/TYPE_USHORT    :int32
   DataBuffer/TYPE_SHORT     :int32
   DataBuffer/TYPE_INT       :int32
   DataBuffer/TYPE_FLOAT     :float32
   DataBuffer/TYPE_DOUBLE    :float64
   DataBuffer/TYPE_UNDEFINED :float64})

(defn- raster-datatype?
  [raster-datatype]
  (contains? raster-datatype->default-tensor-dtype raster-datatype))

(defn- default-target-dtype
  [raster-datatype]
  {:pre [(raster-datatype? raster-datatype)]}
  (get raster-datatype->default-tensor-dtype raster-datatype))

(defn- resolve-target-dtype
  "Chooses the datatype for the target dtype-next tensor, given:
  - `requested-dtype`: a requested dtype-next tensor datatype, either nil or a keyword in `#'datatypes`,
  - `raster-datatype`: an integer, the datatype of the Raster from which to read.

  Returns a dtype-next datatype keyword."
  [requested-dtype raster-datatype]
  {:pre  [(or (nil? requested-dtype) (contains? datatypes requested-dtype))
          (raster-datatype? raster-datatype)]
   :post [(contains? datatypes %)]}
  (or requested-dtype
      (default-target-dtype raster-datatype)))

(defn- resolve-target-dtype-for-raster
  [requested-dtype ^Raster raster]
  (resolve-target-dtype requested-dtype
                        (.getDataType (.getDataBuffer raster))))

(defn- tensor-backing-array
  [target-dtype arr-length]
  (let [arr-length (int arr-length)]
    (case target-dtype
      :int32   (int-array arr-length)
      :float32 (float-array arr-length)
      :float64 (double-array arr-length))))

(defn- get-samples
  [target-dtype ^Raster raster x y w h b]
  (let [w            (int w)
        h            (int h)
        chunk-length (* w h)
        x            (int x)
        y            (int y)
        b            (int b)]
    (case target-dtype
      :int32   (.getSamples raster x y w h b (int-array chunk-length))
      :float32 (.getSamples raster x y w h b (float-array chunk-length))
      :float64 (.getSamples raster x y w h b (double-array chunk-length)))))

(defn- get-typed-array-fn
  ([^Raster data x w]
   (let [x         (int x)
         w         (int w)
         int-fn    (fn [b y] (.getSamples data x (int y) w (int 1) (int b) (int-array w)))
         float-fn  (fn [b y] (.getSamples data x (int y) w (int 1) (int b) (float-array w)))
         double-fn (fn [b y] (.getSamples data x (int y) w (int 1) (int b) (double-array w)))]
     (case (resolve-target-dtype-for-raster nil data)
       :int32   int-fn
       :float32 float-fn
       :float64 double-fn)))

  ([^Raster data x y w h datatype]
   (let [x          (int x)
         y          (int y)
         w          (int w)
         h          (int h)
         total-area (* w h)
         int-fn     (fn [b] (.getSamples data x y w h (int b) (int-array total-area)))
         float-fn   (fn [b] (.getSamples data x y w h (int b) (float-array total-area)))
         double-fn  (fn [b] (.getSamples data x y w h (int b) (double-array total-area)))]
     (case (resolve-target-dtype-for-raster datatype data)
       :int32   int-fn
       :float32 float-fn
       :float64 double-fn))))

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
    ;; IMPROVEMENT use raster-interop/get-samples for this function too, and remove get-typed-array-fn. (Val, 29 Nov 2022)
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
        target-dtype                        (resolve-target-dtype-for-raster datatype data)
        chunk-length                        (* (long width) (long height))
        tensor-arr-length                   (* (long bands) chunk-length)
        ;; NOTE for performance, we eagerly initialize a flat array, fill it using fast JVM interop, and use it to back the returned tensor. (Val, 28 Nov 2022)
        tensor-arr                          (tensor-backing-array target-dtype tensor-arr-length)
        _copied                             (->> (range bands)
                                                 (run! (fn copy-band! [band-index]
                                                         (let [chunk-array (get-samples target-dtype data min-x min-y width height band-index)]
                                                           (System/arraycopy chunk-array (int 0) tensor-arr (* (int chunk-length) (int band-index)) chunk-length)))))
        tensor                              (-> (t/ensure-tensor tensor-arr)
                                                (t/reshape (if (= bands 1)
                                                             [height width]
                                                             [bands height width])))]
    (if convert-fn
      (d/copy! (d/emap convert-fn datatype tensor) tensor)
      tensor)))

(defn show-raster [raster]
  (let [^GridCoverage2D coverage (:coverage raster)]
    (.show coverage nil)))
