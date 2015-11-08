(ns chibs.core
  (:require [overtone.live :refer :all]))

(definst trem [freq 440 depth 10 rate 6 length 3]
(* 0.3
   (line:kr 0 1 length FREE)
   (saw (+ freq (* depth (sin-osc:kr rate))))))

(def mr-note
  (map #(identity) (trem 220 9 3 1)
      (trem 440 3 2 1)))

(defsynth osc-organ [freq 200 dur 0.5]
    (let [src (saw [freq (* freq 1.01) (* 0.99 freq)])
                  low (sin-osc (/ freq 2))
                          filt (lpf src (line:kr (* 10 freq) freq 10))
                                  env (env-gen (perc 0.1 dur) :action FREE)]
          (out 0 (pan2 (* 0.8 low env filt)))))

(do
  (osc-organ 440 16)
  (trem 220 1 3 8)
  (trem 659.26 1 3 8)
  (trem 554.37 1 3 8)
  (trem 660 1 6 8)
  (osc-organ 110 16))
