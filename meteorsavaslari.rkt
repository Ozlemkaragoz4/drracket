#lang racket/base

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require racket/list)
(require racket/math)

(define-values
  (scn_w scn_h spd rot_spd target_rot smooth rot move r roff meteor_count bullet_count draw-bullets pos-bullets bullet_spd score death o)
  (values 600 600 15 7 0 15 0 0 60 0 10 0 empty empty 25 0 0 2))
#|
scn_w ---------------- Ekran genişliği
scn_h ---------------- Ekran yüksekliği
target_rot ----------- Hedef dönüş açısı (Düzgün amaçlar için)
smooth --------------- Sabitleme pürüzsüz
rot ------------------ Akım dönme açısı
move ----------------- 1 ise: İleri hareket, -1: Geri hareket.
r -------------------- Yarıçapı
roff ----------------- Radius ofseti (“Random Meteor Generator'üm için”)
meteor_count --------- Mevcut olan meteorların sayısı.
bullet_count --------- Mevcut olan madde işaretlerinin sayısı.
draw-bullets ---------- Madde işareti görüntülerinin listesi
pos-bullets ---------- Madde işaretleri konumlarının listesi (posn x y)
bullet_spd ----------- Mermilerin hızı
|#

(define-values (x y) (values (+ (/ scn_w 2) r) (+ r (/ scn_h 2)))) ; ---------- Mevcut pozisyon koordinatları
(define target_pos (list (+ (/ scn_w 2) r) (+ r (/ scn_h 2)))) ; --------------Hedef pozisyon koordinatlarının listesi (x y)



(define (ran x y)
  (cond
    ((<= x 0 y) (- (random (max (round (inexact->exact (+ x (abs x) 1))) 2) (max (round (inexact->exact (+ y (abs x)))) 2)) (round (inexact->exact (abs x)))))
    ((>= x 0 y) (- (random (max (round (inexact->exact (+ x (abs y) 1))) 2) (max (round (inexact->exact (+ y (abs y)))) 2)) (round (inexact->exact (abs y)))))
    ((or (<= y x 0) (<= x y 0)) (* -1 (random (round (inexact->exact x)) (round (inexact->exact y)))))
    (else (random (round (inexact->exact x)) (round (inexact->exact y))))
    )
  )



(define (P n)
  (list-ref player n)
  )
(define (t-pos n)
  (list-ref target_pos n)
  )


(define player (list (triangle r "outline" "black") x y 0));-------------------- Oynatıcı görüntüsü (Varsayılan olarak yukarı dönük üçgen)
(define keys (list 0 0 0 0));--------------------------------------------------- Sırasına göre tuşların listesi (yukarıdan aşağıya, sola doğru)
(define meteors empty);--------------------------------------------------------- Göktaşlarının listesi (görüntü x y hız dönüş yarıçapı)
(define bullets empty);--------------------------------------------------------- Madde işaretleri listesi (x y döndürme)

(define scn (rectangle scn_w scn_h "solid" "pink"));--------------------------- Arka planı pembe olarak ayarlama
(define p (circle 35 0 "red"));----------------------------------------------- Oynatıcı görüntüsü için üst daire. 



(define (degtorad x)
  (degrees->radians x)
  )

#|
4 parametresi var:
n: Poligonun sahip olacağı köşe miktarı
x: Poligonun x koordinatı
y: Çokgenin y koordinatı
r: Çokgenin yarıçapı
İşlev, tüm köşelerden dolaşıyor ve aralarına bir çizgi çiziyor.
Ancak bunu yaparken her köşedeki r değerleri rastgele değişiyor.
Ve ana sahneye koymak için tek bir görüntü elde etmek için tüm meteorları üst üste yerleştirir.

|#

(define (draw-meteor n x y r)
  (let ((meteors empty))
    (let ((rads empty))
      
      (let loop ((i 0))
        (if (< i n) (begin
                      (set! rads (append rads (list (+ r (ran (* r -0.3) (* r 0.5))))))
                      (loop (add1 i))
                      ) void)
        )
      
      (let loop ((i 0))
        (if (< i n) (begin
                    
                    
                      (set! meteors (append meteors (list (list (round (+ x (* (list-ref rads i) (cos (* i (/ (* 2 pi) n)))))) (round (+ y (* (list-ref rads i) (sin (* i (/ (* 2 pi) n)))))) (round (+ x (* (list-ref rads (modulo (add1 i) n)) (cos (* (+ 1 i) (/ (* 2 pi) n)))))) (round (+ y (* (list-ref rads (modulo (add1 i) n)) (sin (* (+ 1 i) (/ (* 2 pi) n)))))) ))))
                      (loop (add1 i))
                      
                      )
            void)
        )
      )
    
    (let loop ((i 0))
      (if (< i n) (add-line (add-line (loop (add1 i))
                                      (list-ref (list-ref meteors i) 0)
                                      (list-ref (list-ref meteors i) 1)
                                      (list-ref (list-ref meteors i) 2)
                                      (list-ref (list-ref meteors i) 3) "red")
                            (list-ref (list-ref meteors i) 2)
                            (list-ref (list-ref meteors i) 3)
                            (list-ref (list-ref meteors (modulo (add1 i) n)) 0)
                            (list-ref (list-ref meteors (modulo (add1 i) n)) 1) "red")
          (rectangle (* r 3) (* r 3) 0 "pink"))
      )
    )
  )



#|
“L” listesinin “n” öğesinin ekrandan çıkıp çıkmadığını kontrol eder. Öyleyse; true, aksi takdirde false döndürür.
|#

(define (outofscreen l n)
  (if (or (< (posn-x (list-ref l n)) 0) (> (posn-x (list-ref l n)) scn_w) (< (posn-y (list-ref l n)) 0) (> (posn-y (list-ref l n)) scn_h)) #t #f)
  )


(define (destroy l n)
  (append (take l n) (rest (drop l n)))
  )

(define (dis x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2))))
  )

(define (check-hit x1 y1 x2 y2 r)
  (if (< (dis x1 y1 x2 y2) r) #t #f)
  )

(define (split meteors i)
  (let ((rot (random 360))) (append meteors
                                    (list (list (draw-meteor (random 7 13) (* o (/ (list-ref (list-ref meteors i) 5) 2)) (* o (/ (list-ref (list-ref meteors i) 5) 2)) (/ (list-ref (list-ref meteors i) 5) 2)) (list-ref (list-ref meteors i) 1) (list-ref (list-ref meteors i) 2) (inexact->exact (round (* 1.2 (list-ref (list-ref meteors i) 3)))) rot (inexact->exact (round (/ (list-ref (list-ref meteors i) 5) 1.5)))))
                                    (list (list (draw-meteor (random 7 13) (* o (/ (list-ref (list-ref meteors i) 5) 2)) (* o (/ (list-ref (list-ref meteors i) 5) 2)) (/ (list-ref (list-ref meteors i) 5) 2)) (list-ref (list-ref meteors i) 1) (list-ref (list-ref meteors i) 2) (inexact->exact (round (* 1.2 (list-ref (list-ref meteors i) 3)))) (+ 180 rot) (inexact->exact (round (/ (list-ref (list-ref meteors i) 5) 1.5)))))))
  )


(define (draw ws)
  (place-images
   (append
    (list (rotate (P 3) (place-image/align (P 0) 35 27 "center" "center" p)))
    (list (text (number->string score) 50 "yellow"))
    draw-bullets
    )
   (append
    (list
     (make-posn (P 1) (P 2))
     (make-posn (/ scn_w 2) 100))
    pos-bullets
    )
   (let loop ((i 0))
     (if (< i (length meteors)) (place-image (list-ref (list-ref meteors i) 0) (- (list-ref (list-ref meteors i) 1) (list-ref (list-ref meteors i) 5)) (- (list-ref (list-ref meteors i) 2) (list-ref (list-ref meteors i) 5)) (loop (add1 i))) scn)
     )
   )
  )

(define (key-handler ws ke)
  (cond
    ((key=? ke "up") (set! keys (list-set keys 0 1)))
    ((key=? ke "right") (set! keys (list-set keys 1 1)))
    ((key=? ke "down") (set! keys (list-set keys 2 1)))
    ((key=? ke "left") (set! keys (list-set keys 3 1)))
    ((key=? ke " ") (set! bullet_count (add1 bullet_count)))
    )
  )

(define (release-handler ws ke)
  (cond
    ((key=? ke "up") (set! keys (list-set keys 0 0)))
    ((key=? ke "right") (set! keys (list-set keys 1 0)))
    ((key=? ke "down") (set! keys (list-set keys 2 0)))
    ((key=? ke "left") (set! keys (list-set keys 3 0)))
    )
  )


(define (mouse-handler ws x y me)
  ws
  )

(define (step ws)
  (begin
    
    (let loop ((i 0))
      (if (and (< i bullet_count) (not (= bullet_count (length bullets)))) (begin
                                                                             (set! bullets (append bullets (list (list (- (modulo (round (+ x r)) (+ scn_w r r)) r) (- (modulo (round (+ y r)) (+ scn_h r r)) r) (modulo (round rot) 360)))))
                                                                             (loop (add1 i))
                                                                             )void)
      )
    
    (if (not (= bullet_count (length draw-bullets)))
        (set! draw-bullets
              (let ((b empty))
                (let loop ((i 0))
                  (if (< i bullet_count) (begin (set! b (append b (list (rotate (list-ref (list-ref bullets i) 2) (rectangle 2 10 "solid" "black") )))) (loop (add1 i))) b)
                  )
                )
              )
        void)

    (if (not (= bullet_count (length pos-bullets))) (set! pos-bullets
                                                          (let ((b empty))
                                                            (let loop ((i 0))
                                                              (if (< i bullet_count) (begin (set! b (append b (list (make-posn (- (list-ref (list-ref bullets i) 0) r) (- (list-ref (list-ref bullets i) 1) r))))) (loop (add1 i))) b)
                                                              )
                                                            )
                                                          )
        void)

    (let loop ((i 0))
      (if (and (< i meteor_count) (not (= meteor_count (length meteors)))) (begin
                                                                             (let ((r (random 20 50))) (set! meteors (append meteors (list (list (draw-meteor (random 7 13) (* r o) (* r o) r) (random scn_w) (random scn_h) (random 1 5) (random 360) r)))))
                                                                             (loop (add1 i))
                                                                             )void)
      )



    (let loop ((i 0))
      (if (< i meteor_count) (begin
                               (set! meteors (list-set meteors i (list-set (list-ref meteors i) 1 (modulo (round (+ (list-ref (list-ref meteors i) 1) (* (list-ref (list-ref meteors i) 3) (cos (degtorad (list-ref (list-ref meteors i) 4)))))) (+ scn_w (* 2 (list-ref (list-ref meteors i) 5)))))))
                               (set! meteors (list-set meteors i (list-set (list-ref meteors i) 2 (modulo (round (+ (list-ref (list-ref meteors i) 2) (* (list-ref (list-ref meteors i) 3) (sin (degtorad (list-ref (list-ref meteors i) 4)))))) (+ scn_h (* 2 (list-ref (list-ref meteors i) 5)))))))
                               (loop (add1 i))
                               )
          void)
      )

    (let loop ((i 0))
          
      (if (< i bullet_count) (begin
                               (set! bullets (list-set bullets i (list-set (list-ref bullets i) 0 (round (+ (list-ref (list-ref bullets i) 0) (* bullet_spd (cos (degtorad (+ (list-ref (list-ref bullets i) 2) 90)))))))))
                               (set! bullets (list-set bullets i (list-set (list-ref bullets i) 1 (round (+ (list-ref (list-ref bullets i) 1) (* bullet_spd (sin (degtorad (- (list-ref (list-ref bullets i) 2) 90)))))))))
                               (loop (add1 i))
                               )
          void)
      )

                                                       


    (let loop ((i 0))
      (if (< i bullet_count) (begin
                               (set! pos-bullets (list-set pos-bullets i (make-posn (round (+ (posn-x (list-ref pos-bullets i)) (* bullet_spd (cos (degtorad (+ (list-ref (list-ref bullets i) 2) 90))))))
                                                                                    (round (+ (posn-y (list-ref pos-bullets i)) (* bullet_spd (sin (degtorad (- (list-ref (list-ref bullets i) 2) 90)))))))))
                               (loop (add1 i))
                               )
          void)
      )

    (if (> bullet_count 0) (begin
                             (if (outofscreen pos-bullets 0)
                                 (begin
                                   (set! bullet_count (sub1 bullet_count))
                                   (set! draw-bullets (destroy draw-bullets 0))
                                   (set! pos-bullets (destroy pos-bullets 0))
                                   (set! bullets (destroy bullets 0)))
                                 void)
                             )
        void)

    (let loop_m ((i 0))
      (if (< i meteor_count) (begin
                               (if (check-hit
                                   x
                                   y
                                   (list-ref (list-ref meteors i) 1)
                                   (list-ref (list-ref meteors i) 2)
                                   (+ (+ (list-ref (list-ref meteors i) 5) (* (list-ref (list-ref meteors i) 5) -0.3)) r)
                                   )
                                   (set! death 1)
                                   void
                                   )
                               
                               (let loop_b ((j 0))
                               (if (< j bullet_count) (if (check-hit
                                                           (list-ref (list-ref bullets j) 0)
                                                           (list-ref (list-ref bullets j) 1)
                                                           (list-ref (list-ref meteors i) 1)
                                                           (list-ref (list-ref meteors i) 2)
                                                           (+ (list-ref (list-ref meteors i) 5) (* (list-ref (list-ref meteors i) 5) -0.3))
                                                           )
                                                          (begin
                                                            (set! score (add1 score))
                                                            (set! bullet_count (sub1 bullet_count))
                                                            (set! draw-bullets (destroy draw-bullets j))
                                                            (set! pos-bullets (destroy pos-bullets j))
                                                            (set! bullets (destroy bullets j))
                                                            
                                                            (if (>= (list-ref (list-ref meteors i) 5) 15)
                                                                (begin
                                                                  (set! meteors (split meteors i))
                                                                  (set! meteor_count (+ meteor_count 2)))
                                                                void)
                                                            
                                                            (set! meteor_count (sub1 meteor_count))
                                                            (set! meteors (destroy meteors i))
                                                            )
                                                          (loop_b (add1 j)))
                                   (loop_m (add1 i)))
                               )
                               
                               )void)
      )
    
    
    (if (= (list-ref keys 1) 1) (set! target_rot (- target_rot rot_spd)) (set! target_rot target_rot))
    (if (= (list-ref keys 3) 1) (set! target_rot (+ target_rot rot_spd)) (set! target_rot target_rot))
    (set! move (- (list-ref keys 0) (list-ref keys 2)))
    (set! x (+ x (/ (- (t-pos 0) x) smooth)))
    (set! y (+ y (/ (- (t-pos 1) y) smooth)))
    (set! rot (+ rot (/ (- target_rot rot) (/ smooth 2))))
    
    (set! player (list (P 0) (- (modulo (round x) (+ scn_w r r)) r)  (- (modulo (round y) (+ scn_h r r)) r) (modulo (round rot) 360)))

    (if (= move 0) (set! target_pos target_pos) (if (= move 1) (set! target_pos (list (+ (t-pos 0) (* spd (cos (degtorad (+ rot 90))))) (+ (t-pos 1) (* spd (sin (degtorad (- rot 90))))))) (set! target_pos (list (- (t-pos 0) (* spd (cos (degtorad (+ rot 90))))) (- (t-pos 1) (* spd (sin (degtorad (- rot 90)))))))))


    )
 

  )


(big-bang 0
          (on-tick step)
          (on-mouse mouse-handler)
          (on-draw draw)
          (on-release release-handler)
          (on-key key-handler)
          )
