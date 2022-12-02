(ns magellan.raster.impl.interop
  (:import (java.awt.image Raster)))

(defn tensor-backing-array
  [target-dtype arr-length]
  (let [arr-length (int arr-length)]
    (case target-dtype
      :int32   (int-array arr-length)
      :float32 (float-array arr-length)
      :float64 (double-array arr-length))))

(defn get-samples
  [target-dtype ^Raster raster x y w h b]
  (let [w            (int w)
        h            (int h)
        chunk-length (* w h)]
    (case target-dtype
      :int-32  (.getSamples raster (int x) (int y) w h (int b) (int-array chunk-length))
      :float32 (.getSamples raster (int x) (int y) w h (int b) (float-array chunk-length))
      :float64 (.getSamples raster (int x) (int y) w h (int b) (double-array chunk-length)))))
