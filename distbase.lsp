;Rev 2020
(defun distbase (testa assieme pt scala)
  (setq modalita (cadr (assoc 'percregtmp parametri)))

  (command"_undo" "_g")
  (setq listad nil)
  (setq pos 0)
  (setq rigedist (carica "anag" ""))
;;;  (setq file (open (findfile (strcat modalita "corridoi" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 2 r) (nth 3 r)) rigedist))
;;;  )
;;;  (close file)
;---
;;;  (setq file (open (findfile (strcat modalita "carrelli" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 1 r) (nth 2 r)) rigedist))
;;;  )
;;;  (close file)
;---    )

  (if (findfile (strcat modalita "$$pannelli" ".txt"))
    (setq file (open (findfile (strcat modalita "$$pannelli" ".txt")) "r"))
    (setq file (open (findfile (strcat modalita "pannelli" ".txt")) "r"))
    )
  (while (setq r (read-line file))
    (setq r (read (strcat "(" r ")")))
    (setq rigedist (cons (list (nth 0 r) (nth 4 r) (nth 2 r)) rigedist))
  )
  (close file)
;-----  
;---
  (if (findfile (strcat modalita "$$pannellil" ".txt"))
    (setq file (open (findfile (strcat modalita "$$pannellil" ".txt")) "r"))
    (setq file (open (findfile (strcat modalita "pannellil" ".txt")) "r"))
    )
  (while (setq r (read-line file))
    (setq r (read (strcat "(" r ")")))
    (setq rigedist (cons (list (nth 0 r) (nth 12 r) (nth 1 r)) rigedist))
  )
  (close file)
;-----  
;---
;-----  
;---
  (setq file (open (findfile (strcat modalita "cabine" ".txt")) "r"))
  (while (setq r (read-line file))
    (setq r (read (strcat "(" r ")")))
    (setq rigedist (cons (list (nth 0 r) (nth 2 r) (nth 1 r)) rigedist))
  )
  (close file)
;-----  
;---
  (if (findfile (strcat modalita "$$sviluppi" ".txt"))
    (setq file (open (findfile (strcat modalita "$$sviluppi" ".txt")) "r"))
    (setq file (open (findfile (strcat modalita "sviluppi" ".txt")) "r"))
    )
  (while (setq r (read-line file))
    (setq r (read (strcat "(" r ")")))
    (setq rigedist (cons (list (nth 0 r) (nth 10 r) (nth 1 r)) rigedist))
  )
  (close file)
;-----  
;---
  (setq file (open (findfile (strcat modalita "cesoiati" ".txt")) "r"))
  (while (setq r (read-line file))
    (setq r (read (strcat "(" r ")")))
    (setq rigedist (cons (list (nth 0 r) (nth 7 r) (nth 1 r)) rigedist))
  )
  (close file)
;-----  
;---
  (setq file (open (findfile (strcat modalita "mater" ".txt")) "r"))
  (while (setq r (read-line file))
    (setq r (read (strcat "(" r ")")))
    (setq rigedist (cons (list (nth 0 r) (nth 9 r) (nth 1 r)) rigedist))
  )
  (close file)
;-----  
;---
  (setq file (open (findfile (strcat modalita "tabrinf" ".txt")) "r"))
  (while (setq r (read-line file))
    (setq r (read (strcat "(" r ")")))
    (setq rigedist (cons (list (nth 0 r) (nth 4 r) (nth 1 r)) rigedist))
  )
  (close file)
;-----  
;---
;;;  (setq file (open (findfile (strcat modalita "travinf" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 4 r) (nth 1 r)) rigedist))
;;;  )
;;;  (close file)
;;;;-----  
;;;;---
;;;  (setq file (open (findfile (strcat modalita "travsup" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 4 r) (nth 1 r)) rigedist))
;;;  )
;;;  (close file)
;-----  
;---
;;;  (setq file (open (findfile (strcat modalita "rinfpiede" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 4 r) (nth 1 r)) rigedist))
;;;  )
;;;  (close file)
;-----  
;---
;;;  (setq file (open (findfile (strcat modalita "lanacer" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 7 r) (nth 1 r)) rigedist))
;;;  )
;;;  (close file)
     

;-----  
  
;-----  
  (if (null distsvi) (setq distsvi (carica_f "dibsvi" modalita)))
  (foreach n distsvi
   (if (and (/= "000000000" (cadr n)) (= assieme (car n)))
    
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
       (setq listad
         (cons
            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 4 n) 1) 2 2))
            listad
         )
      )
     )
    )
  )
;-----  
  (if (null distces) (setq distces (carica_f "dibces" modalita)))
  (foreach n distces
    (if (and (/= "" (cadr n)) (= assieme (car n)))
     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
       (setq listad
         (cons
            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 5 n) 1) 2 2))
            listad
         )
      )
     )
    )
  )
;-----  
  (if (null distrin) (setq distrin (carica "dibrin" modalita)))
  (foreach n distrin
    (if (and (/= "" (cadr n)) (= assieme (car n)))
     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
       (setq listad
         (cons
            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 2 n) 1) 2 2))
            listad
         )
      )
     )
    )
  )
;-----  
  (if (null distrin) (setq distrin (carica "dibrin" modalita)))
  (foreach n distrin
    (if (and (/= "" (cadr n)) (= assieme (car n)))
     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
       (setq listad
         (cons
            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 2 n) 1) 2 2))
            listad
         )
      )
     )
    )
  )
;-----  
  ;-----  
;;;  (if (null distcor) (setq distcor (carica "dibcor" modalita)))
;;;  (foreach n distcor
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 2 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
  ;-----  
;;;  (if (null distcar) (setq distcar (carica "dibcar" modalita)))
;;;  (foreach n distcar
;;;    (if (and (/= "" (caddr n)) (= assieme (car n)))
;;;     (if (null (assoc (caddr n) rigedist)) (alert (strcat "Codice " (caddr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (caddr n) (nth 1 (assoc (caddr n) rigedist)) (nth 2 (assoc (caddr n) rigedist)) (rtos (/ (nth 3 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
  ;-----  
;;;  (if (null disttrs) (setq disttrs (carica "dibtrs" modalita)))
;;;  (foreach n disttrs
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 2 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
  ;-----  
;;;  (if (null disttri) (setq disttri (carica "dibtri" modalita)))
;;;  (foreach n disttri
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 2 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
;;;
;-----  9
  (if (null distldr) (setq distldr (carica "dibldr" modalita)))
  (foreach n distldr
    (if (and (/= "" (cadr n)) (= assieme (car n)))
     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
       (setq listad
         (cons
            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 5 n) 1) 2 2))
            listad
         )
      )
     )
    )
  )
;-----  
;;;  (if (null distlcr) (setq distlcr (carica "diblcr" modalita)))
;;;  (foreach n distlcr
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 5 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
;-----  
  (if (null distpan) (setq distpan (carica_f "dibpan" modalita)))
  (foreach n distpan
   (if (and (/= "000000000" (cadr n)) (= assieme (car n)))
    (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
      (setq listad
         (cons
            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (nth 4 n) 2 0))
            listad
         )
      )
     )
   )
  )
  (if (null distpas) (setq distpas (carica_f "dibpas" modalita)))
  (foreach n distpas
   (if (and (/= "" (cadr n)) (= assieme (car n)))
    (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
      (setq listad
         (cons
            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (nth 2 n) 2 2))
            listad
         )
      )
     )
   )
  )
  (if (null distcab) (setq distcab (carica "cabpan" modalita)))
  (foreach n distcab
    (if (and (/= "" (cadr n)) (= assieme (car n)))
      (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
        (setq listad
         (cons
            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (nth 2 n) 2 0))
            listad
         )
        )
      )
    )
  )
  (setvar"expert" 5)
  
  (if (= testa "si")
    (progn
      (COMMAND"_INSERT" "testatab" "_non" pt scala scala "0" assieme (nth 2 (assoc assieme rigedist)))
      (command"_-group" "_c" (substr assieme 2 2) "" (entlast) "")
    )
    (command"_-group" "_c" (substr assieme 2 2) "" "")
  )
  (setvar"expert" 1)
  (setq listam listad)
  (setq listad nil)
  (foreach n listam
    (if (assoc (car n) listad)
      (setq listad
	     (subst (list (nth 0 n) (nth 1 n) (nth 2 n) (rtos (+ (atof (nth 3 n)) (atof (nth 3 (assoc (car n) listad)))) 2 2))
		    (assoc (car n) listad)
		    listad
	     )
      )
      (setq listad (cons n listad))
    )
  )
  (foreach n listad
      (COMMAND"_INSERT" "rigatab" "_non" pt scala scala "0")
      (cond ((= "MQ" (strcase (nth 2 n)))
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (rtos (/ (atof (nth 3 n)) 1000000)2 2))
	    )
	    ((= "ML" (strcase (nth 2 n)))
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (rtos (/ (atof (nth 3 n)) 1000)2 2))
	    )
	    ((= "KG" (strcase (nth 2 n)))
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (rtos (/ (atof (nth 3 n)) 1000)2 2))
	    )
	    ("t"
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (rtos (/ (atof (nth 3 n)) 1)2 0))
	    )
      )
      (if (= testa "si") (command"_-group" "_add" (substr assieme 2 2) (entlast) ""))
    (setq pt (list (car pt) (- (cadr pt) (* scala 100) 0)))
  )
  (command"_undo" "_e")
  (setq $pt$ pt)
)