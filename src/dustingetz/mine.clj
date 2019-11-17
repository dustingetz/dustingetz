(ns dustingetz.mine)


;Example Input:
;rows = 6
;columns = 4
;bombs = 2
;
;Example Output:
; 1  1  0  0
; *  1  0  0
; 1  2  1  1
; 0  1  *  1
; 0  1  1  1
; 0  0  0  0


(def rows 6)
(def columns 4)
(def bombs 2)

(defn grid []
  (for [r (range rows)
        c (range columns)]
    [r c]
    ))

(def needles (set (take bombs (shuffle (grid)))))

(comment
  (->> [[(inc r) (dec c)]
        [(inc r) c]
        [(inc r) (inc c)]

        [r (dec c)]
        [r (inc c)]

        [(dec r) (dec c)]
        [(dec r) c]
        [(dec r) (inc c)]]
       (map needles))

  (needles [2 4])
  )

(->>
  (for [r (range rows)
        c (range columns)]
    (if (needles [r c])
      '*
      (->> [[(inc r) (dec c)]
            [(inc r) c]
            [(inc r) (inc c)]

            [r (dec c)]
            [r (inc c)]

            [(dec r) (dec c)]
            [(dec r) c]
            [(dec r) (inc c)]]
           (map needles)
           (remove nil?)
           count)
      )
    )
  (partition columns)
  )

[(0 0 0 0)
 (0 0 0 0)
 (0 1 1 1)
 (0 2 * 2)
 (0 2 * 2)
 (0 1 1 1)]



