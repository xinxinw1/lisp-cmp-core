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

(mac wits (vs . bd)
  (if (blk?) `(wit ,vs ,@bd)
      (no vs) `(do ,@bd)
      `(let ,(car vs) ,(cadr vs) 
         (wits ,(cddr vs) ,@bd))))

(mac blk a
  `((fn () ,@a)))

(mac dec a
  `(var ,@(afta a nil)))

(mac wgs (nm . bd)
  (if (lis? nm) `(wit ,(afta nm '(gs)) ,@bd)
                `(let ,nm (gs) ,@bd)))

(mac afn (ag . bd)
  `(rfn self ,ag ,@bd))

(mac lop (beg tst end . bd)
  (if (blk?) `(loop ,beg ,tst ,end ,@bd)
             `(blk (loop ,beg ,tst ,end ,@bd))))

(mac for (i n m . bd)
  (if (sym? m) `(lop (var ,i ,n) (<= ,i ,m) (++ ,i) ,@bd)
               `(let #m ,m (for ,i ,n #m ,@bd))))

(mac dwn (i n m . bd)
  (if (sym? m) `(lop (var ,i ,n) (>= ,i ,m) (++ ,i) ,@bd)
               `(let #m ,m (dwn ,i ,n #m ,@bd))))

(mac to (i n . bd)
  (if (sym? n) `(lop (var ,i 0) (< ,i ,n) (++ ,i) ,@bd)
               `(let #n ,n (to ,i #n ,@bd))))

(mac fr (i n . bd)
  `(dwn ,i ,n 0 ,@bd))

(mac ind (i a . bd)
  `(to ,i (len ,a) ,@bd))

(mac inr (i a . bd)
  `(fr ,i (- (len ,a) 1) ,@bd))

(mac each (x a . bd)
  (if (sym? a) `(let ,x nil (ind #i ,a (= ,x #(,a #i)) ,@bd))
               `(let #a ,a (each ,x #a ,@bd))))

(mac eacr (x a . bd)
  (if (sym? a) `(let ,x nil (inr #i ,a (= ,x #(,a #i)) ,@bd))
               `(let #a ,a (eacr ,x #a ,@bd))))

(mac rep (n . bd)
  `(dwn #i ,n 1 ,@bd))

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

(mac swap (a b)
  `(let #c ,a
     (= ,a ,b)
     (= ,b #c)))

(mac nfn bd
  (let g (gs)
    `(fn (,g) ,@(dmap
       #[if (is % '%) g
            (and (sym? %) (beg % "%")) `#(arguments ,(- (rst %) 1))
            %] bd))))

(mac psh (x a)
  `(= ,a (app ,a ,x)))

; accumulate
(mac accum (f . bd)
  `(wits (#var [] ,f #[psh % #var])
     ,@bd
     #var))

; Repeatedly evaluates its body till it returns nil, then returns vals.

(mac drai (ex (o eof nil))
  `(wit (#acc [] #done false)
     (whi !#done
       (let #res ,ex
         (if (is #res ,eof)
             (= #done true)
             (psh #res #acc))))
     #acc))

(mac whir (var ex end . bd)
  `(wit (,var nil #f (tfn ,end))
     (whi !(#f (= ,var ,ex)) ,@bd)))

(mac chk (x tst (o alt))
  (if (sym? x) `(if (,tst ,x) ,x ,alt)
               `(let #x ,x (chk #x ,tst ,alt))))

(mac nof (n a)
  `(let #r (emp ,a)
     (rep ,n (psh ,a #r))
     #r))

