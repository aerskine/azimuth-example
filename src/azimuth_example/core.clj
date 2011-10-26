(ns azimuth-example.core)

(def solar-constant-watts-per-sq-meter 1360.0)

(def watts-per-sq-meter-A 218.0)
(def watts-per-sq-meter-per-celcius-B 1.9)

(def co-albedo-ice-planet 0.38)
(def co-albedo-ice-free 0.71)

(defn pade-approx-tanh
  [x]
  (cond (< x -3.0) -1.0
        (> x 3.0) 1.0
        :else (/ (* x (+ 27.0 (* x x))) (+ 27.0 (* 9.0 x x)))))

(defn tanh [x] (Math/tanh x))
;; (defn tanh [x] (pade-approx-tanh x))

(defn co-albedo-p
  [t gamma]
  (let [ai co-albedo-ice-planet
        af co-albedo-ice-free
        gt (* gamma t)]
    (+ ai (* 0.5 (- af ai) (+ 1.0 (tanh gt))))))

(defn temperature-balance
  [teq gamma q]
  (let [A watts-per-sq-meter-A
        B watts-per-sq-meter-per-celcius-B
        ap (fn [t] co-albedo-p t gamma)]
    (- (/ (+ A (* B teq)) (ap teq)) q)))

;; ------
;; begin copy/paste, with gratitude, from:
;;   http://www.learningclojure.com/2010/02/clojure-dojo-3-from-heron-to-newton.html
;;
;; a simple Newton's method solver
(defn deriv [f dx]
  (fn [x] (/  (- (f (+ x dx)) (f x) )  dx)))

(defn make-improver [f dx]
  (fn [guess] (- guess (/ (f guess) ((deriv f dx) guess)))))

(defn make-good-enough [f tolerance]
  (fn [guess] (< (abs (f guess)) tolerance)))

(defn iterative-improve [x improve good?]
  (if (good? x) x
      (iterative-improve (improve x) improve good?)))

(defn solvee [f guess dx tolerance]
  (iterative-improve guess (make-improver f dx) (make-good-enough f tolerance)))

(defn solve-it [f] 
  (solvee f 1.0 0.000001 0.000001))
;; end of copy/paste
;; (could also have used Incanter's solver)
;; --------


(defn teq
  [q]
  (solve-it (fn [t] temperature-balance t 0.1 q)))

;; (use '(incanter core charts))

;; example interactive tanh
;; (let [x (range -3 3 0.1)]
;;   (view (dynamic-xy-plot [gg (range 0.1 10 0.1)]
;;           [x (map #(tanh (* gg %)) x)])))

;; interactive temperature balance
;; (let [q (range 1000 2000 1)]
;;            (view (dynamic-xy-plot [teq (range -100 100 0.5)
;;                                    gamma (range 0.1 10 0.1)]
;;                                   [q (map #(temperature-balance teq gamma %) q)])))
