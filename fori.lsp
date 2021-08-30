;Rev 2020

(defun c:foro ()
  (setq percorso_fori (cadr (assoc 'perc_fori parametri)))
  (setq nforo (getfiled "Selezionare Foro" percorso_fori "dwg" 2)) 
  (command"_insert" nforo pause "1" "1")
)
(DEFUN DIBFORO ()
  (setq listk_svi nil)
  (setq nrcalate 0)
  (setq elcalate nil)
  (command"_layer" "_off" "Hforo" "_off" "testo" "")
  (setq grf (ssget "_f" LISPTFO (list '(0 . "INSERT") '(2 . "FORO*"))))
    (command"_layer" "_on" "Hforo" "_on" "testo" "")

  (SETQ NRF 0)
  (SETQ LISFORP NIL)
  
  (if grf (setq lisforp (cons (list (list (setq foro (ssname grf 0)) (ATOI (CDR (ASSOC '1 (ENTGET (ENTNEXT (ssname grf 0)))))))) lisforp)))
  
;;;  (WHILE (AND grf (SSNAME GRF (SETQ NRF (+ 1 NRF))) (SETQ FOROd (substr (SSNAME GRF (SETQ NRF (+ 0 NRF)))1 8)))
  (WHILE (AND grf (SSNAME GRF (SETQ NRF (+ 1 NRF))) (SETQ FOROd (SSNAME GRF (SETQ NRF (+ 0 NRF)))))
    (if (< (distance (cdr (assoc '10 (entget foro))) (cdr (assoc '10 (entget forod)))) 25)
      
      (progn
	(setq lisforp (subst (cons (list forod (ATOI (CDR (ASSOC '1 (ENTGET (ENTNEXT forod)))))) (car lisforp))
			    (car lisforp)
			    lisforp
		      )
	)
      )
      (setq lisforp (cons (list (list forod (ATOI (CDR (ASSOC '1 (ENTGET (ENTNEXT FOROd))))))) lisforp))
    )
    (setq foro forod)
  )
  (setq lisf nil)
  (foreach n lisforp
    (setq n (vl-sort n (function (lambda (e1 e2) (> (CADR e1) (CADR e2))))))
    (setq lisf (cons n lisf))
  )
  (setq lisforp nil)
  (foreach gfori lisf
    (setq pfor (CDR (ASSOC '10 (ENTGET (car (car gFORi))))))
    (if (and (/= ldpos0_f 0) (> (distance pt pfor) ldpos0_f))
      (setq pxdf (+ (distance pt0x_f pfor) ldpos0_f))
      (setq pxdf (distance pt pfor))
    )
    (SETQ LISFORP (CONS (LIST ;(CDR (ASSOC '2 (ENTGET (car (car gfori)))))
			      (substr (CDR (ASSOC '2 (ENTGET (car (car gfori))))) 1 8)
 				   (cadr (car gfori))
				  'NX (atof (rtos pxdf 2 1))) LISFORP)
    )
    (setq nrxp 0)
    (foreach foro (cdr gfori)
       (setq pfor (CDR (ASSOC '10 (ENTGET (car foro)))))
       (if (and (/= ldpos0_f 0) (> (distance pt pfor) ldpos0_f))
           (setq pxdf (+ (distance pt0x_f pfor) ldpos0_f))
           (setq pxdf (distance pt pfor))
       )
       (setq nonx nil)
       (if (member (list (SETQ N1 (CDR (ASSOC '2 (ENTGET (car foro))))) (SETQ N2 (CDR (ASSOC '2 (ENTGET (car (nth nrxp gfori))))))) POSpon)
           (SETQ LISFORP (CONS (LIST ;(CDR (ASSOC '2 (ENTGET (car foro))))
				     (substr (CDR (ASSOC '2 (ENTGET (car foro)))) 1 8)
    				          (cadr foro)
				          'SP (atof (rtos pxdf 2 1))
				          (- (CADR (nth nrxp gfori)) (CADR FORO)
					     (/ (NTH 3 (ASSOC N1 PRESE)) 2)
					     (/ (NTH 3 (ASSOC N2 PRESE)) 2)
			                  )
			       ) LISFORP)

           )
           (if (and (= nonx t) (/= n2 "DFOR0020"))
               (SETQ LISFORP (CONS (LIST (substr (CDR (ASSOC '2 (ENTGET (car foro)))) 1 8)
    				          (cadr foro)
				          'SF (atof (rtos pxdf 2 1))) LISFORP) nonx t

               )
               (SETQ LISFORP (CONS (LIST (substr (CDR (ASSOC '2 (ENTGET (car foro)))) 1 8)
    				          (cadr foro)
				          'NX (atof (rtos pxdf 2 1))) LISFORP)

               )
	   )
       )
       (setq nrxp (+ 1 nrxp))
    )
 )
 (FOREACH FORO LISFORP
;;;   (setq *error* nil)
   (setq posponti (carica "posponti" ""))
    (setq scatola (nth 1 (assoc (car foro) prese)))
    (if (< 3 (strlen scatola))
      (progn
         (SETQ LISTK (CONS (LIST scatola (nth 3 foro) 1  (nth 1 foro)
				    (nth 2 (assoc (car foro) prese)) (nth 3 (assoc (car foro) prese))) LISTK)
		 )
	)
      )
   (if (= profelet "DEM239") (setq -vinnest -70) (setq -vinnest 0))
    (FOREACH RIGA DISTFORI
      (IF (and (or (= "Aperto" (nth 1 riga)) (= "Variante" (nth 1 riga))) (= (CAR RIGA) (CAR FORO)))
	(progn
	  (setq var nil)
        (IF (= 'SF (nth 2 foro))
          (progn
	     (setq llh nil)
		     (foreach l lisforp (if (and (> (nth 1 l) (nth 1 foro)) (= (nth 3 foro) (nth 3 l)) ) (setq llh (cons (list (nth 1 l) (nth 0 l))llh))))
		     (setq llh (vl-sort llh (function (lambda (e1 e2) (< (car e1) (car e2))))))
		     (setq lhp (car (car llh)))
		     (if llh (setq lhp (- lhp (/ (nth 3 (assoc (cadr (car llh)) PRESE)) 2))))
		     (if (member (list (cadr (car llh)) (nth 0 riga) ) POSPONTI)
		       (setq Var nil) (setq var t))
	   ))
          (COND ((and (null var) (= "A" (substr (nth 2 riga)1 1)) (= "Tubo" (NTH 4 RIGA)) (/= -1 (nth 6 riga)) (/= -2 (nth 6 riga)))
		 (IF (/= 'SF (nth 2 foro))
		   (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga))
					   (atoi (rtos (- (- altkit (- 0 -vinnest) (+ (nth 6 riga) (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro))) 50.0) 2 0))
					   (+ (nth 6 riga) (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro))
				    (+ (NTH 3 foro) (nth 3 riga))
					   (+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro) (ATOI (RTOS (- ALTKIT (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0)))) LISTK))
			       (progn		     
		    (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga))
				(ATOI (RTOS (- lhp (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0)) (+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro))
				    (+ (NTH 3 foro) (nth 3 riga))
					    (+ (ATOI (RTOS (- lhp (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0)) (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro))) LISTK))
		    )
		   ))
		((and (null var) (= "A" (substr (nth 2 riga)1 1)) (or (= "Tubo" (NTH 4 RIGA)) (= "Curva" (NTH 4 RIGA))) (= -2 (nth 6 riga)))
		 (IF (/= 'SF (nth 2 foro))
		   (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ -64.5 (NTH 3 foro) (nth 3 riga))
					   1
					   altkit
				    65.5
					   0
					   ) LISTK))
			       
		   ))
		
                ((EQUAL (NTH 2 FORO) (NTH 2 RIGA))(SETQ LISTK (CONS (LIST (NTH 1 RIGA) (NTH 3 foro) 1 (nth 1 foro)
                                       (nth 2 (assoc (car foro) PRESE)) (nth 3 (assoc (car foro) PRESE) )) LISTK)))
		((and (null var) (= "A" (substr (nth 2 riga)1 1)) (= "Innesto" (NTH 4 RIGA)))
		 (IF (/= 'SF (nth 2 foro))
		   (if (/= 0 (nth 6 riga))
		     (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga)) 1 (+ (nth 1 foro) (nth 6 riga))
				    1 0) LISTK))
		     (progn
		       
		   (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga)) 1 (+ hpan -vinnest)
				    1 0) LISTK)
		 LISTK_SVI (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga)) 1 (+ hpan -vinnest)
				    1 0) LISTK_SVI))))
		   )
		 )
		((and (null var) (= "A" (substr (nth 2 riga)1 1)) (= "Passacavi" (NTH 4 RIGA)))
		 (if (= -2 (nth 6 riga)) (setq v6 -2) (setq v6 1))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga)) 1 (+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro))
				    v6 0) LISTK)))
		((and (null var) (= "A" (substr (nth 2 riga)1 1)) (= "Tubo" (NTH 4 RIGA)) (= -1 (nth 6 riga)))
		 (if (= -1.0 (nth 6 riga)) (setq v6 -1) (setq v6 1))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga)) 1 (+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro))
				    v6 0) LISTK)))
		
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "S" (substr (nth 2 riga)1 1)) (= "Tubo" (NTH 4 RIGA))(/= -2 (nth 6 riga)))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
		       (+  (nth 7 riga) (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
				(ATOI (RTOS (- ALTKIT 45 30 (- 0 -vinnest) (NTH 1 FORO)) 2 0))
;;;				(ATOI (RTOS (- ALTKIT 50.0 (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0))
				    (+ 30 (nth 1 foro));1		 
;;;				    (+ (nth 6 riga) (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro));1
					 (+ (nth 7 riga) (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
					(+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro)(ATOI (RTOS (- ALTKIT (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0)))
					 ) LISTK)))
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "S" (substr (nth 2 riga)1 1)) (or (= "Tubo" (NTH 4 RIGA)) (= "CurvaS" (NTH 4 RIGA)))(= -2 (nth 6 riga)))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
		(+ -64.5 (nth 7 riga) (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
				1
				    ALTKIT
				    65.5
					0
					 ) LISTK)))
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "S" (substr (nth 2 riga)1 1)) (= "Innesto" (NTH 4 RIGA)))
		 (if (/= 0 (nth 6 riga))
		   (progn 
		   (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
					   (+ (nth 7 riga) (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
					   1 (+ (nth 1 foro) (nth 6 riga)) 1 180) LISTK))
		   )
		  

		   (progn
		  (SETQ LISTK (CONS (LIST (NTH 5 RIGA)    (+ (nth 7 riga) (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2)) 1 (+ hpan -vinnest) 1 0) LISTK))
		  (SETQ LISTK_SVI (CONS (LIST (NTH 5 RIGA)(+ (nth 7 riga) (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2)) 1 (+ hpan -vinnest) 1 0) LISTK_SVI))
		  )
		   ))
		 
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "S" (substr (nth 2 riga)1 1)) (= "Passacavi" (NTH 4 RIGA)))
		 (if (= -1.0 (nth 6 riga)) (setq v6 -1) (setq v6 1))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
													     (+ (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
													     1
													     (- (+ 50 (nth 1 foro)) (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga)))
				    v6 90) LISTK)))
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "S" (substr (nth 2 riga)1 1)) (= "CurvaS" (NTH 4 RIGA)) (/= -2 (nth 6 riga)))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
													  (+ (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
													  1
													  (- (+ 50 (nth 1 foro)) 0)
				    (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga)) 90) LISTK)))
;;;		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "Tubo" (NTH 4 RIGA))(/= -2 (nth 6 riga)))
;;;		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
;;;		       (- (NTH 3 foro)(+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga))  (/ (nth 2 (assoc (car foro) PRESE)) 2))
;;;				(ATOI (RTOS (- ALTKIT (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0))
;;;				    (+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro));1
;;;				    (- (NTH 3 foro)(+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga))  (/ (nth 2 (assoc (car foro) PRESE)) 2))
;;;					(+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro)(ATOI (RTOS (- ALTKIT (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0)))) LISTK)))
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "Tubo" (NTH 4 RIGA))(/= -2 (nth 6 riga)))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
		       (-  (NTH 3 foro) (nth 7 riga)  (/ (nth 2 (assoc (car foro) PRESE)) 2))
					 
				(ATOI (RTOS (- ALTKIT 45 30 (- 0 -vinnest) (NTH 1 FORO)) 2 0))
;;;				(ATOI (RTOS (- ALTKIT 50.0 (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0))
				    (+ 30 (nth 1 foro));1
					 (- (NTH 3 foro)(nth 7 riga)  (/ (nth 2 (assoc (car foro) PRESE)) 2))
					(+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro)(ATOI (RTOS (- ALTKIT (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0)))
					 ) LISTK)))

;;;		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "Tubo" (NTH 4 RIGA))(= -2 (nth 6 riga)))
;;;		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
;;;		       (-  (NTH 3 foro)(+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga)) 65.0 (/ (nth 2 (assoc (car foro) PRESE)) 2))
;;;				1
;;;				    ALTKIT
;;;				    65.0
;;;					0
;;;					 ) LISTK)))
                 ((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (or (= "Tubo" (NTH 4 RIGA)) (= "CurvaD" (NTH 4 RIGA)))(= -2 (nth 6 riga)))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
		(- (NTH 3 foro) +64.5 (nth 7 riga)  (/ (nth 2 (assoc (car foro) PRESE)) 2))
				1
				    ALTKIT
				    65.5
					0
					 ) LISTK)))
;;;		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "Innesto" (NTH 4 RIGA)))
;;;		 (if (/= 0 (nth 6 riga))
;;;		   (progn
;;;		     (SETQ LISTK (CONS (LIST (NTH 5 RIGA)(- (NTH 3 foro) (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga))  (/ (nth 2 (assoc (car foro) PRESE)) 2)) 1 (+ (nth 1 foro) (nth 6 riga)) 1 180) LISTK))
;;;		     )
;;;		   (progn
;;;		  (SETQ LISTK (CONS (LIST (NTH 5 RIGA)(- (NTH 3 foro) (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga))  (/ (nth 2 (assoc (car foro) PRESE)) 2)) 1 (+ hpan 0) 1 0) LISTK))
;;;		  (SETQ LISTK_SVI (CONS (LIST (NTH 5 RIGA)(- (NTH 3 foro)(+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga))  (/ (nth 2 (assoc (car foro) PRESE)) 2)) 1 (+ hpan 0) 1 0) LISTK_SVI))
;;;		  )))
                 ((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "Innesto" (NTH 4 RIGA)))
		 (if (/= 0 (nth 6 riga))
		   (progn 
		   (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
					   (- (NTH 3 foro)(nth 7 riga)  (/ (nth 2 (assoc (car foro) PRESE)) 2))
					   1 (+ (nth 1 foro) (nth 6 riga)) 1 180) LISTK))
		   )
		  

		   (progn
		  (SETQ LISTK (CONS (LIST (NTH 5 RIGA)    (- (NTH 3 foro)(nth 7 riga)  (/ (nth 2 (assoc (car foro) PRESE)) 2)) 1 (+ hpan -vinnest) 1 0) LISTK))
		  (SETQ LISTK_SVI (CONS (LIST (NTH 5 RIGA)(- (NTH 3 foro)(nth 7 riga)  (/ (nth 2 (assoc (car foro) PRESE)) 2)) 1 (+ hpan -vinnest) 1 0) LISTK_SVI))
		  )
		   ))
;;;		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "Passacavi" (NTH 4 RIGA)))
;;;		 (if (= -1.0 (nth 6 riga)) (setq v6 -1) (setq v6 1))
;;;		                                                                       (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
;;;													     (- (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
;;;													     1
;;;													     (- (+ 50 (nth 1 foro)) (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga)))
;;;				    v6 -90) LISTK)))
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "Passacavi" (NTH 4 RIGA)))
		 (if (= -1.0 (nth 6 riga)) (setq v6 -1) (setq v6 1))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
					 (- (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
					 1
					 (- (+ 50 (nth 1 foro)) (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga)))
				    -1 -90) LISTK)));21.00
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "CurvaD" (NTH 4 RIGA))(/= -2 (nth 6 riga))) (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
													  (- (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
													  1
													  (- (+ 50 (nth 1 foro)) 0)
				    (- 0 (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga))) 90) LISTK)))
		((and (= "B" (substr (nth 2 riga)1 1)) (= "Passacavi" (NTH 4 RIGA)))
		 (if (= -1.0 (nth 6 riga)) (setq v6 -1) (setq v6 1))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga));(+ (NTH 3 foro) (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga)))
					 1
													     (- (nth 1 foro) (/ (nth 3 (assoc (car foro) PRESE)) 2) )
				    v6 180) LISTK)))
          ))
	)
      )
    

  )
  (if listk_svi
    (progn
      (setq listk_svi (vl-sort listk_svi (function (lambda (e1 e2) (< (nth 1 e1) (nth 1 e2))))))
      (setq posk_svi (atof (rtos (/ (+ (nth 1 (car listk_svi)) (nth 1 (last listk_svi))) 2) 2 1)))
      
    (if (<= (length listk_svi) 8)
      (progn
	 (setq listk (cons (list profelet posk_svi 210 hpan 1 0 ) listk))
;;;         (alert "2 calate")
      )
      
      (progn
	 (setq listk (cons (list profelet (- posk_svi 105) 210 hpan 1 0 ) listk))
	(setq listk (cons (list profelet (+ posk_svi 105) 210 hpan 1 0 ) listk))
;;;         (alert "4 calate")
      )))
    )
  (setq lisfor_ (vl-sort lisforp (function (lambda (e1 e2) (< (nth 1 e1) (nth 1 e2))))))
  (setq lisfor nil)
  (foreach fr lisfor_
;;;    (if (/= "FORO0002" (nth 0 fr)) (setq lisfor (cons fr lisfor)))
    (if (= -1 (nth 7 (assoc (car fr) prese))) (setq lisfor (cons fr lisfor)))
    )
  (setq lisfor (reverse lisfor))
  (setq lfeltro nil)(setq feltro nil)
  (foreach for lisfor
    (if (null feltro) (setq feltro (list (nth 1 for) (nth 1 for) (nth 3 for) "325"))
      (if (< (abs (- (nth 1 for) (car feltro))) 100)
	(setq feltro (list (car feltro) (nth 1 for) (nth 3 for) "590"))
	(setq lfeltro (cons feltro lfeltro) feltro (setq feltro (list (nth 1 for) (nth 1 for) (nth 3 for) "325")))
	)
      )
    
    )
  (if feltro (setq lfeltro (cons feltro lfeltro) feltro nil))
    
    (print)
    (foreach fel lfeltro
      (if (= "325" (nth 3 fel))
        (setq listk (cons (list (cadr (assoc 'feltro parametri)) (nth 2 fel) (/ (* (atoi (nth 3 fel)) 220) 1) (/ (+ (nth 0 fel) (nth 1 fel))2) (atoi (nth 3 fel)) 220) listk))
	(setq listk (cons (list (cadr (assoc 'feltro parametri)) (/ (distance (nth 0 LISPTFO) (nth 1 LISPTFO)) 2)
				(/ (* (atoi (rtos (- (distance (nth 0 LISPTFO) (nth 1 LISPTFO)) 10) 2 1)) 220) 1)
				(/ (+ (nth 0 fel) (nth 1 fel))2) (atoi (rtos (- (distance (nth 0 LISPTFO) (nth 1 LISPTFO)) 10) 2 1)) 220) listk))
	)
      )
;;;(print listk_svi)
;;;  (getpoint)
)