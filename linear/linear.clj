;Vector
(defn vOperation [f]
  (fn [& vectors]
    (apply mapv f vectors)))

(def v+ (vOperation +))

(def v- (vOperation -))

(def v* (vOperation *))

(defn scalar [& vectors]
  (apply + (apply mapv * vectors)))

(defn v*s [vector & s]
  (mapv (fn [x] (* x (apply * s))) vector))

(defn coord [x y v1 v2]
  (- (* (nth x v1) (nth y v2))
     (* (nth y v1) (nth x v2))))

(defn vect [& vectors]
  (reduce (fn [x y]
            (vector (coord x y 1 2)
                    (coord x y 2 0)
                    (coord x y 0 1)))
          vectors))

;Matrix
(defn mOperation [f matrixs]
  (apply mapv f matrixs))

(defn m+ [& matrixs]
  (mOperation v+ matrixs))

(defn m- [& matrixs]
  (mOperation v- matrixs))

(defn m* [& matrixs]
  (mOperation v* matrixs))

(defn m*s*v [matrix f & args]
  (mapv (fn [x] (apply f x args)) matrix))

(defn m*s [matrix & s]
  (apply m*s*v matrix v*s s))

(defn m*v [matrix & v]
  (apply m*s*v matrix scalar v))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn m*m [& matrixs]
  (reduce (fn [x y]
            (mapv (fn [z] (m*v (transpose y) z)) x))
          matrixs))

;Shapeless
(defn sOperation [f] (fn [a b] (if (vector? a) (mapv (sOperation f) a b) (f a b))))

(def s+ (sOperation +))

(def s- (sOperation -))

(def s* (sOperation *))
