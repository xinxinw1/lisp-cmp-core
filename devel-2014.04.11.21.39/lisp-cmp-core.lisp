(mac let (a x . bd)
  (if (blk?) `(do (var ,a ,x) ,@bd)
      `(letb ,a ,x ,@bd)))

(mac wit (vs . bd)
  (if (blk?) `(do ,@(map #[hea % 'var] (grp vs 2)) ,@bd)
      `(witb ,vs ,@bd)))

(mac letb (a x . bd)
  `((fn (,a) ,@bd) ,x))

(mac witb (vs . bd)
  (let g (grp vs 2)
    `((fn ,(map car g) ,@bd) ,@(map cadr g))))

(mac blk a
  `((fn () ,@a)))

(mac wgs (nm . bd)
  (if (lis? nm)
        `(wit ,(afta nm '(gs)) ,@bd)
      `(let ,nm (gs) ,@bd)))

(mac afn (ag . bd)
  `(rfn self ,ag ,@bd))

(mac lop (beg tst end . bd)
  (if (blk?) `(loop ,beg ,tst ,end ,@bd)
      `(blk (loop ,beg ,tst ,end ,@bd))))

(mac for (i n m . bd)
  `(let #m ,m
     (lop (var ,i ,n) (<= ,i #m) (++ ,i) ,@bd)))

(mac to (i n . bd)
  `(let #m ,n
     (lop (var ,i 0) (< ,i #m) (++ ,i) ,@bd)))

(mac fr (i n . bd)
  `(lop (var ,i ,n) (>= ,i 0) (-- ,i) ,@bd))

(mac ind (i a . bd)
  `(to ,i (len ,a) ,@bd))

(mac inr (i a . bd)
  `(fr ,i (- (len ,a) 1) ,@bd))

(mac each (x a . bd)
  `(wit (,x nil #a ,a)
     (ind #i #a
       (= ,x #(#a #i))
       ,@bd)))

(mac eacr (x a . bd)
  `(wit (,x nil #a ,a)
     (inr #i #a
       (= ,x #(#a #i))
       ,@bd)))

(mac rep (n . bd)
  `(lop (var #i ,n) (>= #i 1) (-- #i) ,@bd))

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

(mac dfn (nm ag . bd)
  `(exe (def ,nm ,ag ,@bd)))

(mac inl (nm ag . bd)
  `(mac ,nm ,ag
     `(do ,,@(let p (pnms ag)
               (dmap #[if (has % p) (aq %) %] bd)))))

(mac dfm (nm ag . bd)
  `(do (def ,nm ,ag ,@bd)
       (inl ,nm ,ag ,@bd)))

(mac imp (x . a)
  `(do ,@(map #[mkimp x %] a)))

(dfn mkimp (x a)
  `(var ,a (. ,x ,a)))

(mac jn (ag . bd)
  `(fn ,ag (nrt (do ,@bd))))

(mac dej (nm ag . bd)
  `(def ,nm ,ag (nrt (do ,@bd))))

(mac &= (x a)
  `(= ,a (+ ,x ,a)))

(mac zap (f a . rst)
  `(= ,a (,f ,a ,@rst)))

(mac nfn bd
  (let g (gs)
    `(fn (,g) ,@(dmap
       #[if (is % '%) g
            (and (sym? %) (beg % "%")) `#(arguments ,(rst %))
            %] bd))))