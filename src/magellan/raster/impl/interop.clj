(ns magellan.raster.impl.interop
  (:import (java.awt.image Raster)))

;; NOTE: the overhead of multimethod invocation is typically insignificant, given that we call these once per band.
(defmulti tensor-backing-array (fn [target-dtype _arr-length] target-dtype))

(defmulti get-samples (fn [target-dtype _raster _x _y _w _h _b] target-dtype))

;; NOTE we could use macros to eliminate more repetition in this code, (Val, 29 Nov 2022)
;; but it doesn't seem worth it: that would make the code more magical,
;; less tangible, and less editor-friendly.

(defmethod tensor-backing-array :int32
  [_target-dtype arr-length]
  (int-array (int arr-length)))

(defmethod get-samples :int32
  [_target-dtype ^Raster raster x y w h b]
  (let [w            (int w)
        h            (int h)
        chunk-length (* w h)]
    (.getSamples raster (int x) (int y) w h (int b) (int-array chunk-length))))

(defmethod tensor-backing-array :float32
  [_target-dtype arr-length]
  (float-array (int arr-length)))

(defmethod get-samples :float32
  [_target-dtype ^Raster raster x y w h b]
  (let [w            (int w)
        h            (int h)
        chunk-length (* w h)]
    (.getSamples raster (int x) (int y) w h (int b) (float-array chunk-length))))

(defmethod tensor-backing-array :float64
  [_target-dtype arr-length]
  (double-array (int arr-length)))

(defmethod get-samples :float64
  [_target-dtype ^Raster raster x y w h b]
  (let [w            (int w)
        h            (int h)
        chunk-length (* w h)]
    (.getSamples raster (int x) (int y) w h (int b) (double-array chunk-length))))
