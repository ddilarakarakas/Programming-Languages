;; Trimmed the list ...
(defun trim-list (list)
	(setq newList '())
	(dolist (line list)
		(setq resultString "")
		(loop for ch across line
			do 
				(cond
					((equal (string ch) "(") (setq resultString (concatenate 'string resultString (string "( "))))
					((equal (string ch) ")") (setq resultString (concatenate 'string resultString (string " )"))))
					(t (setq resultString (concatenate 'string resultString (string ch))))
				)
		)
		(setq newList (append newList (list resultString)))
	)
	newList
)

(defun string-to-list (str)
        (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil)))         



(setq line2 "")
(setq full_ '())
(let ((in (open "input.txt" :if-does-not-exist nil)))
    (when in
        (loop for line = (read-line in nil)
            while line do 
            (progn 
                (setq line2 line)
                (setq list_ '())
                (setq list_ (append list_ (list line)))
                (setq read_string (nth 0 (trim-list list_)))
                (cond
                    ((> (length read_string) 2)
                        (progn 
                            (setq list_1 (string-to-list read_string))
                            (setf full_ (append full_ list_1))
                        )
                    )
                )
                
            )
        )
        ;;(print full_)
        (close in)
    )
)


(setq horn_clause_list '())
(setq fact_list '())
(setq query_list '())
(setq control 0)
(setq new_full '())
(setq new_full full_)

(print "READ FILE : ")
(print new_full)
(terpri)
(terpri)
(dolist (i new_full)
    (if (>= (length i) 2)
        (progn 
            (if (equal (nth 1 i) nil)
                (progn 
                    (setq fact_list (append fact_list (list (nth 0 i))))
                    (setq control 1)
                )    
            )
            (if (equal (nth 0 i) nil)
                (progn
                    (setq query_list (append query_list (list (nth 1 i))))
                    (setq control 1)
                )      
            )
            (if (equal control 0)
                (progn 
                    (setq horn_clause_list (append horn_clause_list (list i)))   
                )
            )
            (setq control 0)
        )
    )
)

(terpri)
(terpri)
(print "ALL LISTS")
(print horn_clause_list)
(print "------------------------")
(print fact_list)
(print "------------------------")
(print query_list)
(print "------------------------")
(terpri)
(terpri)


(defun go_horn_clause(query)
    (setq return_list '())
    (setq first_list '())
    (setq second_list '())
    (dolist (element horn_clause_list)
       (if (equal (nth 0 (nth 0 element)) (nth 0 query))
            (progn 
                (setq new_list (nth 1 (nth 0 element)))
                (if (equal (length new_list) (length (nth 1 query)))
                    (progn 
                        (dotimes (i (length new_list))
                            (if (equal (type-of (nth i new_list)) (type-of (nth i (nth 1 query))))
                                (progn
                                    (if (not (equal (type-of (nth i new_list)) (type-of 'ABC)))
                                        (progn
                                            (if (equal (nth i new_list) (nth i (nth 1 query)))
                                                (progn     
                                                    (setq return_list (append return_list (list element)))
                                                )
                                            )
                                        )
                                        (progn
                                            (setq first_list (append first_list (list (nth i new_list))))
                                            (setq second_list (append second_list (list (nth i (nth 1 query)))))
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
       )
    )

    (setf first_list (remove-duplicates first_list :test #'equal))
    (setf second_list (delete-duplicates second_list :test #'equal))
    ;;(print first_list)
    ;;(print second_list)

    (dolist (list_x return_list)
        (setf predic_list (nth 1 list_x))
        (dolist (small_list predic_list)
            (setf new_small_list (nth 1 small_list))
            (dotimes (i (length new_small_list))
                (if (not (equal (position (nth i new_small_list) first_list :test #'equal) nil))
                    (progn
                        (setq index (position (nth i new_small_list) first_list :test #'equal))
                        (if (not (equal (nth index second_list) nil))
                            (setf (nth i new_small_list) (nth index second_list))  
                        )            
                    )
                )
            )
        )
    )
    return_list
)

(with-open-file (stream "output.txt" :direction :output )
    (dolist (querys query_list)
        (setq resultList (go_horn_clause querys))
        (setq ifTheNewQueryList (nth 1 (nth 0 resultList)))
        ;;(print "ILK SONUC")
        ;;(print ifTheNewQueryList)

        (setq controlIFTrue 0)
        (dolist (first_element ifTheNewQueryList)
            (dolist (second_element fact_list)
                (if (equal first_element second_element)
                    (setq controlIFTrue (+ controlIFTrue 1))
                )
            )
        )

        (setq result_query "nil")
        (if (equal controlIFTrue (length ifTheNewQueryList))
            (setq result_query "true")
        )

        (format stream result_query)
        (terpri stream)
    )
    (close stream)
)