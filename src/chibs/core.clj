(ns chibs.core
  (:require [overtone.live :refer :all]))

(definst trem [freq 440 depth 10 rate 6 length 3]
(* 0.3
   (line:kr 0 1 length FREE)
   (saw (+ freq (* depth (sin-osc:kr rate))))))

(defsynth osc-organ [freq 200 dur 0.5]
  (let [src (saw [freq (* freq 1.01) (* 0.99 freq)])
        low (sin-osc (/ freq 2))
        filt (lpf src (line:kr (* 10 freq) freq 10))
        env (env-gen (perc 0.1 dur) :action FREE)]
    (out 0 (pan2 (* 0.8 low env filt)))))

(def equal-tempered
  {"A" 440.00
   "A#/Bb" 466.16
   "B" 493.88
   "C" 523.25
   "C#/Db" 554.37
   "D" 587.33
   "D#/Eb" 622.25
   "E" 659.26
   "F" 698.46
   "F#/Gb" 739.99
   "G" 783.99
   "G#/Ab" 830.61
   "A5" 880.00})


(defn chordy [rt third fifth]
  (do
    (trem (get equal-tempered fifth) 1 3 8)
    (trem (get equal-tempered third) 1 3 8)
    (trem (get equal-tempered fifth) 1 6 8)
    (osc-organ (get equal-tempered rt) 16)))

(chordy "A" "C#/Db" "E")
(chordy "E" "G#/Ab" "B")
(chordy "B" "D#/Eb" "F#/Gb")

;; A simple pad sound using definst rather than defsynth, which will
;; automatically take the enclosing synth and send it to a bus.
;; (Note how in comparison to foo above it doesn't use the out and pan ugens.)
(definst overpad [note 60 amp 0.7 attack 0.001 release 2]
  (let [freq  (midicps note)
        env   (env-gen (perc attack release) :action FREE)
        f-env (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)))))
        bfreq (/ freq 2)
        sig   (apply +
                     (concat (* 0.7 (sin-osc [bfreq (* 0.99 bfreq)]))
                             (lpf (saw [freq (* freq 1.01)]) f-env)))]
    (* amp env sig)))

(defn play-notes [t beat-dur notes attacks]
  (when notes
    (let [note      (+ 12 (first notes))
          attack    (first attacks)
          amp       0.5
          release   0.1
          next-beat (+ t beat-dur)]
      (at t (overpad note amp attack release))
      (apply-by next-beat #'play-notes next-beat beat-dur (next notes) (next attacks) []))))

(play-notes (now) 425 (cycle [40 42 44 45 47 49 51 52]) (repeat 0.4))
