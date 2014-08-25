(mac blk a
  `((fn () ,@a)))

(mac let (a x . bd)
  `((fn (,a) ,@bd) ,x))

(mac with (vs . bd)
  (let g (grp vs 2)
    `((fn ,(map car g) ,@bd) ,@(map cadr g))))

(mac wgs (nm . bd)
  (if (lis? nm)
        `(with ,(afta nm '(gs)) ,@bd)
      `(let ,nm (gs) ,@bd)))

(mac afn (ag . bd)
  `(rfn self ,ag ,@bd))

(mac for (i n m . bd)
  `(do (var #m ,m)
       (loop (var ,i ,n) (<= ,i #m) (++ ,i) ,@bd)))

(mac to (i n . bd)
  `(do (var #m ,n)
       (loop (var ,i 0) (< ,i #m) (++ ,i) ,@bd)))

(mac fr (i n . bd)
  `(loop (var ,i ,n) (>= ,i 0) (-- ,i) ,@bd))

(mac rep (n . bd)
  `(loop (var #i ,n) (>= #i 1) (-- #i) ,@bd))

(mac no (a) `(nil? ,a))

(mac when (ts . bd)
  `(if ,ts (do ,@bd)))

(mac stk (a x . bd)
  `(do (psh ,x ,a)
       (var #ret (do ,@bd))
       (pop ,a)
       #ret))

(mac dyn (a x . bd)
  `(do (var #ori ,a)
       (= ,a ,x)
       (var #ret (do ,@bd))
       (= ,a #ori)
       #ret))

; (in x 1 2 3) -> (or (is x 1) (is x 2) (is x 3)
(mac in (x . a)
  `(or ,@(map #[qq (is ,x ,%)] a)))

(mac dfm (nm ag bd)
  `(do (def ,nm ,ag ,bd)
       (mac ,nm ,ag
         ,(lis 'qq
            (rdc #[drpl %2 (lis 'uq %2) %1] bd ag)))))

#|(mac dfm (nm (ag) bd)
  `(do (def ,nm (,ag) ,bd)
       (mac ,nm (,ag) ,(lis 'qq (drpl ag (lis 'uq ag) bd)))))|#

(mac dfn (nm ag . bd)
  `(exe (def ,nm ,ag ,@bd)))

(dfn mkimp (x a)
  `(var ,a (. ,x ,a)))

(mac imp (x . a)
  `(do ,@(map #[mkimp x %] a)))