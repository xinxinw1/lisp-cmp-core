(js-mac mac a `(js-mac ,@a))
(mac exe a `(js-exe ,@a))

(mac dfn a `(exe (def ,@a)))
(mac dmc a `(exe (mac ,@a)))

(mac dot () `|.|)

(mac byone (nm a)
  ``(do ,@(map [qq (,,nm ,_)] ,a)))

(mac bytwo (nm a)
  ``(do ,@(map [qq (,,nm ,(car _) ,(cadr _))] (grp ,a 2))))

(mac alias a (bytwo alias1 a))

(mac alias1 (new old)
  `(mac ,new #args `(,,old ,@#args)))

(mac jsali a
  `(alias ,@(fla (map jsali1 a))))

(dfn jsali1 (a)
  (lis a (app 'js- a)))

(jsali + - * / % ++ --
       and or not del
       = += -= *= /= %=
       < > >= <=
       inst is isn
       arr obj
       |.| # cal
       var fn rfn def new
       if do while foi swit case brk cont
       ret thr nret
       rmac
       mlet exe qt)

(mac dmac a (byone js-dmac a))
(mac rmac a (bytwo js-rmac a))

(mac let (a x . bd)
  `((fn (,a) ,@bd) ,x))

(mac with (vs . bd)
  (let g (grp vs 2)
    `((fn ,(map car g) ,@bd) ,@(map cadr g))))

(mac withs (vs . bd)
  (if (no vs) `(do ,@bd)
      `(let ,(car vs) ,(cadr vs)
         (with ,(cddr vs) ,@bd))))

(mac blk a
  `((fn () ,@a)))

(mac dec a
  `(var ,@(afta a nil)))

(mac wgs (nm . bd)
  (symlis nm
    `(with ,(afta nm '(gs)) ,@bd)))

(mac afn (ag . bd)
  `(rfn self ,ag ,@bd))

(mac ngs (n v . bd)
  `(let ,v (mkngs ,n) ,@bd))

(mac once (vs . bd)
  (symlis vs
    (ngs (len vs) gens
      `(with ,(fla (par gens (map [qq (if (sym? ,_) ,_ (gs))] vs)))
         (gswith (lis ,@gens) (lis ,@vs)
           (with ,(fla (par vs gens)) ,@bd))))))

(dmc surblk (a)
  `(if (blk?) ,a
       `(blk ,,(auq a))))

(mac loop (st p up . bd)
  (surblk `(js-loop ,st ,p ,up ,@bd)))

(mac for (i n m . bd)
  (once (n m)
    `(loop (var ,i ,n) (<= ,i ,m) (++ ,i) ,@bd)))

(mac down (i n m . bd)
  (once (n m)
    `(loop (var ,i ,n) (>= ,i ,m) (-- ,i) ,@bd)))

(mac to (i n . bd)
  (once n
    `(loop (var ,i 0) (< ,i ,n) (++ ,i) ,@bd)))

(mac fr (i n . bd)
  `(down ,i ,n 0 ,@bd))

(mac ind (i a . bd)
  `(to ,i (len ,a) ,@bd))

(mac inr (i a . bd)
  `(fr ,i (- (len ,a) 1) ,@bd))

(mac each (x a . bd)
  (once a
    `(let ,x nil
       (ind #i ,a
         (= ,x #(,a #i))
         ,@bd))))

(mac eacr (x a . bd)
  (once a
    `(let ,x nil
       (inr #i ,a
         (= ,x #(,a #i))
         ,@bd))))

(mac rep (n . bd)
  `(down #i ,n 1 ,@bd))

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
  (once x
    `(or ,@(map [qq (is ,x ,_)] a))))

(mac inl (nm ag . bd)
  `(mac ,nm ,ag
     `(do ,,@(let p (pnms ag)
               (dmap [if (has _ p) (auq _) _] bd)))))

(mac dfm (nm ag . bd)
  `(do (def ,nm ,ag ,@bd)
       (inl ,nm ,ag ,@bd)))

(mac imp (x . a)
  `(do ,@(map [mkimp x _] a)))

(dfn mkimp (x a)
  `(var ,a (. ,x ,a)))

(mac jn (ag . bd)
  `(fn ,ag (nret (do ,@bd))))

(mac dej (nm ag . bd)
  `(def ,nm ,ag (nret (do ,@bd))))

(mac &= (x a)
  `(= ,a (+ ,x ,a)))

(mac zap (f a . rst)
  `(= ,a (,f ,a ,@rst)))

(mac swap (a b)
  `(let #c ,a
     (= ,a ,b)
     (= ,b #c)))

(mac nfn (a) `(fn (_) ,a))

(mac psh (x a)
  `(= ,a (app ,a ,x)))

; accumulate
(mac accum (f . bd)
  `(withs (#var [] ,f [psh _ #var])
     ,@bd
     #var))

; Repeatedly evaluates its body till it returns nil, then returns vals.

(mac drai (ex (o eof nil))
  `(with (#acc #[] #done false)
     (while !#done
       (let #res ,ex
         (if (is #res ,eof)
             (= #done true)
             (psh #res #acc))))
     #acc))

(mac whir (var ex end . bd)
  `(with (,var nil #f (tfn ,end))
     (while !(#f (= ,var ,ex)) ,@bd)))

(mac chk (x tst (o alt))
  (once x `(if (,tst ,x) ,x ,alt)))

(mac nof (n a)
  `(let #r (emp ,a)
     (rep ,n (psh ,a #r))
     #r))

#|(mac cons (a b) `(obj a ,a b ,b))
(mac car (a) `(. ,a a))
(mac cdr (a) `(. ,a b))|#

(mac cons (a b) `#[,a ,b])
(mac car (a) `#(,a 0))
(mac cdr (a) `#(,a 1))

(mac lis a
  (if (no a) nil
      `(cons ,(car a) (lis ,@(cdr a)))))

(mac lisd a
  (if (no a) nil
      (no (cdr a)) (car a)
      `(cons ,(car a) (lisd ,@(cdr a)))))

(mac cmcx (a)
  `(exe (al (cmcx ,a))))