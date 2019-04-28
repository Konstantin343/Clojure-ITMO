(defn constant [value] 
  (fn [variables] value)
  )

(defn variable [name] 
  (fn [variables] (get variables name))
  )

(defn operation [f] 
 (fn [& arguments] 
   (fn [args] 
     (apply f (mapv (fn [g] (g args)) arguments))
     )
   )
 )

(def add (operation +))
(def subtract (operation -))
(def negate (operation -))
(def multiply (operation *))
(def divide (operation (fn [x y] (/ (double x) (double y)))))

(def sqrt (operation (fn [x] (Math/sqrt (Math/abs x)))))
(def square (operation (fn [x] (* x x))))
(def min (operation (fn [& x] (reduce (fn [f s] (min f s)) (first x) (rest x)))))
(def max (operation (fn [& x] (reduce (fn [f s] (max f s)) (first x) (rest x)))))
(def avg (operation (fn [& x] (/ (apply + x) (count x)))))
(def med (operation (fn [& x] (nth (sort x) (int (/ (count x) 2))))))

(def ops
  {'+ add, '- subtract, '* multiply, '/ divide, 'negate negate, 
   'sin sin, 'cos cos, 'sinh sinh, 'cosh cosh, 'square square, 'sqrt sqrt,
   'min min, 'max max, 'med med, 'avg avg }
  )

(defn parseFunction [expression] 
  (cond
    (number? expression) (constant expression)
    (symbol? expression) (variable (str expression))
    (seq? expression) (apply (get ops (first expression)) (map parseFunction (rest expression)))
    (string? expression) (parseFunction (read-string expression))
    )
  )