(DEFUN C:AGG_DIST ()
;  (command"_tilemode" 1)
;  (setq *error* nil)
;          (setq cabin (getvar "dwgname"))
;          (setq assieme (substr cabin 1 (- (strlen cabin) 4)))
;	  (SETQ ASSIEME (SUBSTR ASSIEME 1 10))
;  (setq righe (ssget "x" (list '(0 . "INSERT") '(2 . "rigatab"))))
;  (setq nr -1)
;   (setq lrighe nil)
;  (while (setq riga (ssname righe (setq nr (+ 1 nr))))
;    (setq pos (cdr (assoc '1 (entget (entnext riga)))))
;    (setq pan (cdr (assoc '1 (entget (entnext (entnext riga))))))
;    (setq pt (cdr (assoc '10 (entget riga))))
;    (setq desc (cdr (assoc '1 (entget (entnext (entnext (entnext riga)))))))
;    (setq lrighe (cons (list (atoi pos) pan pt riga desc) lrighe))
;    (setq layer (cdr (assoc '8 (entget riga))))
;    )
;    (setq lispan (carica "dibpas" modalita))
;  (setq righen nil)
;    (foreach n (cdr lispan)
;      (if (and (= "LR-" (substr (cadr n) 1 3)) (= (car n) assieme))
;	(setq righen (cons n righen)))
;      )
;    (setq pos -1)
;  (command "_layer" "_m" layer "")
;    (foreach n lrighe
;      (if (= "LR-" (substr (cadr n) 1 3))
;	(progn
;	  (entdel (nth 3 n))
;	  (command"_insert" "rigatab" "_non" (nth 2 n) "0.3" "0.3" "0"
;		  (rtos (nth 0 n) 2 0)
;		  (cadr (nth (setq pos (+ 1 pos)) righen))
;		  (setq desclana  (nth 4 n))
;		  "Nr"
;		  (nth 5 (nth (setq pos (+ 0 pos)) righen))
;
;		  )
;	  )
;	)
;      )
;  (setq inc 0)
;(while (> (- (length righen) 1) pos)
;  (command"_insert" "rigatab" "_non" (list (car (nth 2 (last lrighe))) (- (cadr (nth 2 (last lrighe))) (* (setq inc (+ 1 inc)) 30))) "0.3" "0.3" "0" 
;		  (rtos (+ inc (car (last lrighe)))  2 0)
;		  (cadr (nth (setq pos (+ 1 pos)) righen))
;		  desclana
;
;		  "Nr"
;		  (nth 5 (nth (setq pos (+ 0 pos)) righen))
;		  )
;		  )
;
    (command"_tilemode" 0)
  (command"_plot"
"_y"
"A4"
"dwg to pdf.pc3"
"loris"
""
"p"
"_N"
"_l"
"1:1"
"3.77,3.35"
	  ""
"KIT.ctb"
"_y"
""
""
""
""
	  ""
	  "_y"
)
  )