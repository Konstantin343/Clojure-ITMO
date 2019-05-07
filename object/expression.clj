;Interface
(definterface Operation
  (evaluate [])
  (toString [])
  (toStringSuffix [])
  (toStringInfix [])
  (diff []))

;Operation
(defn evaluate [expr vars] ((.evaluate expr) vars))
(defn toString [expr] (.toString expr))
(defn toStringSuffix [expr] (.toStringSuffix expr))
(defn toStringInfix [expr] (.toStringInfix expr))
(defn diff [expr var] ((.diff expr) var))

(deftype CommonPrototype [operation symbol diffRule])

(declare unary)

(deftype CommonOperation [prototype args]
  Operation
  (evaluate [this] #(apply (.operation prototype) (map (fn [x] (evaluate x %)) args)))
  (toString [this] (str "(" (.symbol prototype) " " (clojure.string/join " " (map toString args)) ")"))
  (toStringSuffix [this] (str "(" (clojure.string/join " " (map toStringSuffix args)) " " (.symbol prototype) ")"))
  (toStringInfix [this] (str (if (contains? unary (.symbol prototype)) (.symbol prototype)) 
                             (str "(" (clojure.string/join (str " " (.symbol prototype) " ") (map toStringInfix args))) ")"))
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
  (toStringSuffix [this] (toString this))
  (toStringInfix [this] (toString this))
  (diff [this] (fn [var] ZERO)))

(defn Constant [value] (ConstantPrototype. value))
(def ZERO (Constant 0))
(def ONE (Constant 1))
(def TWO (Constant 2))

;Variable
(declare Variable)

(defn getVar [name] (str (first (clojure.string/lower-case name))))

(deftype VariablePrototype [name]
  Operation
  (evaluate [this] (fn [vars] (get vars (getVar name))))
  (toString [this] name)
  (toStringSuffix [this] name)
  (toStringInfix [this] name)
  (diff [this] (fn [var] (if (= var (getVar name)) ONE ZERO))))

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

;Pow
(declare Pow)

(def PowPrototype (CommonPrototype.
                    #(Math/pow %1 %2)
                    "**"
                    (fn [] ()))) ;without diff
(defn Pow [& args]
  (CommonOperation. PowPrototype args))

;Log
(declare Log)

(def LogPrototype (CommonPrototype.
                    #(/ (Math/log (Math/abs %2)) (Math/log (Math/abs %1)))
                    "//"
                    (fn [] ()))) ;without diff
(defn Log [& args]
  (CommonOperation. LogPrototype args))

;Parsers
(def ops { '+ Add,
          '- Subtract,
          '* Multiply,
          '/ Divide,
          'negate Negate,
          'square Square,
          'sqrt Sqrt,
          'sin Sin,
          'cos Cos
          'sinh Sinh,
          'cosh Cosh,
          (symbol "//") Log,
          '** Pow})

(def unary #{"negate"
            "sin"
            "cos"
            "sinh"
            "cosh"
            "square"
            "sqrt"})

(declare parseObject)
(declare parseObjectSuffix)

(defn parse [expression mode]
  (cond   
    (number? expression) (Constant expression)
    (symbol? expression) (Variable (str expression))
    (seq? expression) (if (zero? mode)
                       (apply (get ops (first expression)) (map parseObject (rest expression)))
                       (apply (get ops (last expression)) (map parseObjectSuffix (take (- (count expression) 1) expression))))
    (string? expression) (parse (read-string expression) mode)))

;PrefixSuffix
(defn parseObject [expression] (parse expression 0))
(defn parseObjectSuffix [expression] (parse expression 1))

;Combinators
(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)
(defn _show [result]
  (if (-valid? result) (str "-> " (pr-str (-value result)) " | " (pr-str (apply str (-tail result))))
    "!"))
(defn tabulate [parser inputs]
  (run! (fn [input] (printf "    %-10s %s\n" input (_show (parser input)))) inputs))
(defn _empty [value] (partial -return value))
(defn _char [p]
  (fn [[c & cs]]
    (if (and c (p c)) (-return c cs))))
(defn _map [f]
  (fn [result]
    (if (-valid? result)
      (-return (f (-value result)) (-tail result)))))
(defn _combine [f a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar)
        ((_map (partial f (-value ar)))
          ((force b) (-tail ar)))))))
(defn _either [a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar) ar ((force b) str)))))
(defn _parser [p]
  (fn [input]
    (-value ((_combine (fn [v _] v) p (_char #{\u0000})) (str input \u0000)))))

(defn +char [chars] (_char (set chars)))
(defn +char-not [chars] (_char (comp not (set chars))))
(defn +map [f parser] (comp (_map f) parser))
(def +parser _parser)
(def +ignore (partial +map (constantly 'ignore)))
(defn iconj [coll value]
  (if (= value 'ignore) coll (conj coll value)))
(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))
(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))
(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))
(defn +or [p & ps]
  (reduce (partial _either) p ps))
(defn +opt [p]
  (+or p (_empty nil)))
(defn +star [p]
  (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))
(defn +plus [p] (+seqf cons p (+star p)))
(defn +str [p] (+map (partial apply str) p))

;Infix
(def *all-chars (mapv char (range 0 128)))
(def *digits (apply str (filter #(Character/isDigit %) *all-chars)))
(def *digit (+char *digits))
(def *spaceSymbols(apply str (filter #(Character/isWhitespace %) *all-chars)))
(def *space (+char *spaceSymbols))
(def *ws (+ignore (+star *space)))

(declare *infixPart)
(declare *infixSeq)

(def *constant (+map read-string (+str (+seqf #(into (vec (cons %1 %2)) (vec (cons %3 %4))) 
                                              (+opt (+char "+-")) (+plus *digit) (+opt (+char ".")) (+opt (+plus *digit))))))
(def *variable (+map symbol (+str (+plus (+char "xXyYzZ")))))
(def *addSubSymbol (+map symbol (+str (+map list (+char "+-")))))
(def *divMulSymbol (+map symbol (+str (+plus (+char "/*")))))
(def *opsSymbol (+map symbol (+str (+plus (+char-not (str *spaceSymbols *digits \u0000 "xyzXYZ+-/*()."))))))

(defn *infArgsOp [operand sign] (+map (partial reduce #(list (first %2) %1 (second %2))) (+seqf cons *ws operand (+star (+seq *ws sign *ws operand)) *ws)))
(defn *ops [] (+map #(list (first %) (second %)) (+seq *ws *opsSymbol *ws (+or (delay (*infixSeq)) (delay (*ops)) *constant *variable))))
(defn *divMul [] (*infArgsOp (+or (delay (*infixSeq)) (delay (*ops)) *constant *variable) *divMulSymbol))
(defn *addSub [] (*infArgsOp (+or (delay (*divMul)) (delay (*infixSeq)) (delay (*ops)) *constant *variable) *addSubSymbol))
(defn *infixSeq [] (+seqn 1 (+char "(") *ws (delay (*infixPart)) *ws (+char ")")))
(defn *infixPart [] (+or (*addSub) (*divMul) (*infixSeq) (*ops) *constant *variable))

(defn parseObjectInfix [expression] (parseObject ((+parser (+seqn 0 *ws (*infixPart) *ws)) expression)))