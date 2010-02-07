;;; Conway's Game of Life, a simple 2D cellular automaton
;(ns net.pataprogramming.life)

; default world dimensions
(def *dimensions* {:rows 20 :columns 20})

; Cells know their own locations so they can find their neighbors
(defstruct cell :row :column :state)

(defn make-world
  ([dimensions] (make-world dimensions #{}))
  ([dimensions seeds]
     (vec (for [row (range (:rows dimensions))]
            (vec (for [column (range (:columns dimensions))]
                   (struct cell row column
                           (if (contains? seeds [row column])
                             :alive :dead))))))))

(defstruct coordinate :row :column)
(defn make-coordinate [pair]
  (let [row (first pair)
        column (second pair)]
    (struct coordinate row column)))

(defn add-coordinates [a b]
  (struct coordinate (+ (:row a) (:row b)) (+ (:column a) (:column b))))
                 
; Retrieve cell (take modulus so world wraps around)
(defn find-cell [world coord]
  ((world (mod (:row coord) (:rows *dimensions*)))
   (mod (:column coord) (:columns *dimensions*))))

(defn alive? [cel]
  (= (:state cel) :alive))

(defn neighbor [world coord offset]
  (find-cell world (add-coordinates coord offset)))

(def *adjacencies* (map make-coordinate
                        (disj (set (for [r [-1 0 1] c [-1 0 1]] [r c])) [0 0])))
                     
;(def *adjacencies* (map make-coordinate
;                        (disj (set (cartesian-product '(-1 0 1) '(-1 0 1)))
;                              '(0 0))))

(defn neighbors [world coord]
    (map #(neighbor world coord %) *adjacencies*))

(defn live-neighbor-count [world coord]
  (count (filter alive? (neighbors world coord))))

(defn resurrect-cell [cel]
  (struct cell (:row cel) (:column cel) :alive))

(defn kill-cell [cel]
  (struct cell (:row cel) (:column cel) :dead))

(defn static-cell [cel]
  cel)

(defn update-cell [world cel]
  (let [neighbor-count (live-neighbor-count world cel)]
    (if (alive? cel) 
       (if (or (= neighbor-count 2) (= neighbor-count 3))
         cel
         (kill-cell cel))
       (if (= neighbor-count 2)
         (resurrect-cell cel)
         cel))))

(defn alive-rule [neighbor-count] 
  ({2 static-cell 3 static-cell} neighbor-count kill-cell))
 
(defn dead-rule [neighbor-count]
  ({3 resurrect-cell} neighbor-count static-cell))

(def *rules* {:alive alive-rule :dead dead-rule})

(defn update-cell-rules [world cel]
  (let [neighbor-count (live-neighbor-count world cel)]
    ((:state cel) *rules*)))

(defn update-world [world]
  (vec (for [row world]
         (vec (for [cel row]
                (update-cell world cel))))))

(defn step-world []
  (dosync (ref-set *world* (update-world @*world*))))

(defn print-world [world]
  (let [disp {:alive \+ :dead \.}]
    (doseq [rows world]
      (doseq [cell rows] (print ((:state cell) disp)))
      (println))
    (println)))  

(defn run [steps]
  (dotimes [i steps]
    (step-world)
    (print-world @*world*)))

(defn initialize
  ([] (initialize *dimensions*))
  ([dimensions] (initialize dimensions #{}))
  ([dimensions seeds]
     (def *world* (ref (make-world dimensions seeds)))))

; Now, make it go  
(def *seeds* #{[0 3] [1 3] [0 4] [1 4] [1 2] [1 3] [1 4] [1 5] [1 6] [2 1]
               [2 2] [2 3] [3 4] [3 3]})
(initialize *dimensions* *seeds*)
;(run 10)