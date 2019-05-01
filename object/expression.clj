;Interface
(definterface Operation
  (evaluate [])
  (toString [])
  (diff []))

;Operation
(defn evaluate [expr vars] ((.evaluate expr) vars))
(defn toString [expr] (.toString expr))
(defn diff [expr var] ((.diff expr) var))

(deftype CommonPrototype [operation symbol diffRule])

(deftype CommonOperation [prototype args]
  Operation
  (evaluate [this] #(apply (.operation prototype) (map (fn [x] (evaluate x %)) args)))
  (toString [this] (str "(" (.symbol prototype) " " (clojure.string/join " " (map toString args)) ")"))
  (diff [this] #(apply (.diffRule prototype) (concat args (map (fn [x] (diff x %)) args)))))

;Constant
(declare Constant)
(declare ONE)
(declare ZERO)
(declare TWO)

(deftype ConstantPrototype [value]
  Operation
  (evaluate [this] (fn [vars] value))
  (toString [this] (format "%.1f" (double value)))
  (diff [this] (fn [var] ZERO)))

(defn Constant [value] (ConstantPrototype. value))
(def ZERO (Constant 0))
(def ONE (Constant 1))
(def TWO (Constant 2))

;Variable
(declare Variable)

(deftype VariablePrototype [name]
  Operation
  (evaluate [this] (fn [vars] (get vars name)))
  (toString [this] name)
  (diff [this] (fn [var] (if (= var name) ONE ZERO))))

(defn Variable [name] (VariablePrototype. name))

;Add
(declare Add)

(def AddPrototype (CommonPrototype.
                    +
                    "+"
                    (fn [a b da db] (Add da db))))

(defn Add [& args] 
  (CommonOperation. AddPrototype args))

;Subtract
(declare Subtract)

(def SubtractPrototype (CommonPrototype.
                    -
                    "-"
                    (fn [a b da db] (Subtract da db))))

(defn Subtract [& args] 
  (CommonOperation. SubtractPrototype args))

;Multiply
(declare Multiply)

(def MultiplyPrototype (CommonPrototype.
                    *
                    "*"
                    (fn [a b da db] (Add (Multiply da b) (Multiply db a)))))

(defn Multiply [& args] 
  (CommonOperation. MultiplyPrototype args))

;Divide
(declare Divide)

(def DividePrototype (CommonPrototype.
                    #(/ (double %1) (double %2))
                    "/"
                    (fn [a b da db] (Divide (Subtract (Multiply da b) (Multiply db a)) (Multiply b b)))))

(defn Divide [& args] 
  (CommonOperation. DividePrototype args))

;Negate
(declare Negate)

(def NegatePrototype (CommonPrototype.
                    -
                    "negate"
                    (fn [a da] (Negate da))))
(defn Negate [& args] 
  (CommonOperation. NegatePrototype args))

;Square
(declare Square)

(def SquarePrototype (CommonPrototype.
                    #(* % %)
                    "square"
                    (fn [a da] (Multiply TWO da a))))

(defn Square [& args]
  (CommonOperation. SquarePrototype args))

;Sqrt
(declare Sqrt)

(def SqrtPrototype (CommonPrototype.
                    #(Math/sqrt (Math/abs %))
                    "sqrt"
                    (fn [a da] (Multiply (Sqrt (Square a)) da (Divide ONE (Multiply a TWO (Sqrt a)))))))

(defn Sqrt [& args]
  (CommonOperation. SqrtPrototype args))

;SinCos
(declare Sin)
(declare Cos)

;Sin
(def SinPrototype (CommonPrototype.
                    #(Math/sin %)
                    "sin"
                    (fn [a da] (Multiply da (Cos a)))))

(defn Sin [& args]
  (CommonOperation. SinPrototype args))

;Cos
(def CosPrototype (CommonPrototype.
                    #(Math/cos %)
                    "cos"
                    (fn [a da] (Multiply da (Negate (Sin a))))))

(defn Cos [& args]
  (CommonOperation. CosPrototype args))

;SinhCosh
(declare Sinh)
(declare Cosh)

;Sinh
(def SinhPrototype (CommonPrototype.
                    #(Math/sinh %)
                    "sinh"
                    (fn [a da] (Multiply da (Cosh a)))))

(defn Sinh [& args]
  (CommonOperation. SinhPrototype args))

;Cosh
(def CoshPrototype (CommonPrototype.
                    #(Math/cosh %)
                    "cosh"
                    (fn [a da] (Multiply da (Sinh a)))))

(defn Cosh [& args]
  (CommonOperation. CoshPrototype args))

;Parser
(def ops {'+ Add,
          '- Subtract,
          '* Multiply,
          '/ Divide,
          'negate Negate,
          'square Square,
          'sqrt Sqrt,
          'sin Sin,
          'cos Cos})

(def vars {'x (Variable "x"),
          'y (Variable "y"),
          'z (Variable "z")})

(defn parseObject [expression]
  (cond   
    (symbol? expression) (get vars expression)
    (number? expression) (Constant expression)
    (seq? expression) (apply (get ops (first expression)) (map parseObject (rest expression)))
    (string? expression) (parseObject (read-string expression))))