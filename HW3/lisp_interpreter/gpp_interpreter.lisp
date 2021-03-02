(load "gpp_lexer.lisp")

(setq fileList nil) 
(setq outputList nil)
(setq outStack nil)
(setq charList nil)
(setq tempUpdateStack nil)
(setq errorList nil)
(setq outFile nil)

(defun outFileUpdate ()
    (cond
        ((equal nil outStack) )
        (T (setf outFile (append outFile outStack))) 
    )  
)

(defun LineToCharList (line)
    (dotimes (i (length line))
        (setf charList (append charList (list (char line i))))
    )
)

(defun addStack (ch)
    (setf outStack (append  outStack (list (string ch))))
)

(defun is-OPCP (ch)
    (cond
        ((char-equal ch (char ")" 0)) T)
        (T nil)
    )
)

(defun removeLastElement ()
    (setf outStack (reverse (cdr (reverse outStack))))
)

(defun is-num (nb)
    (and (<= (char-code (char nb 0)) 57) (>= (char-code (char nb 0)) 48))
)

(defun addOperator ()
    (cond
        ((= 3 (list-length tempUpdateStack))
            (progn
                (cond
                    ((is-num (nth 0 tempUpdateStack))
                        (progn
                            (if (is-num (nth 1 tempUpdateStack))
                                (progn
                                    (setq num1 (parse-integer (nth 1 tempUpdateStack)))
                                    (setq num2 (parse-integer (nth 0 tempUpdateStack)))
                                    (setf num1 (+ num1 num2))
                                    (setf outStack (append outStack (list (write-to-string num1))))
                                )
                                (setf errorList "SYNTAX_ERROR")
                            )
                        )
                    )
                    (T (setf errorList "SYNTAX_ERROR"))
                )
            )
        )
        ((> (list-length tempUpdateStack) 3) (setf errorList "SYNTAX_ERROR"))
    )
)

(defun subOperator ()
    (cond
        ((= 3 (list-length tempUpdateStack))
            (progn
                (cond
                    ((is-num (nth 0 tempUpdateStack))
                        (progn
                            (if (is-num (nth 1 tempUpdateStack))
                                (progn
                                    (setq num1 (parse-integer (nth 1 tempUpdateStack)))
                                    (setq num2 (parse-integer (nth 0 tempUpdateStack)))
                                    (setf num1 (- num1 num2))
                                    (setf outStack (append outStack (list (write-to-string num1))))
                                )
                                (setf errorList "SYNTAX_ERROR")
                            )
                        )
                    )
                    (T (setf errorList "SYNTAX_ERROR"))
                )
            )
        )
        ((> (list-length tempUpdateStack) 3) (setf errorList "SYNTAX_ERROR"))
    )
)

(defun divOperator ()
    (cond
        ((= 3 (list-length tempUpdateStack))
            (progn
                (cond
                    ((is-num (nth 0 tempUpdateStack))
                        (progn
                            (if (is-num (nth 1 tempUpdateStack))
                                (progn
                                    (setq num1 (parse-integer (nth 1 tempUpdateStack)))
                                    (setq num2 (parse-integer (nth 0 tempUpdateStack)))
                                    (setf num1 (/ num1 num2))
                                    (setf outStack (append outStack (list (write-to-string num1))))
                                )
                                (setf errorList "SYNTAX_ERROR")
                            )
                        )
                    )
                    (T (setf errorList "SYNTAX_ERROR"))
                )
            )
        )
        ((> (list-length tempUpdateStack) 3) (setf errorList "SYNTAX_ERROR"))
    )
)

(defun multOperator ()
    (cond
        ((= 3 (list-length tempUpdateStack))
            (progn
                (cond
                    ((is-num (nth 0 tempUpdateStack))
                        (progn
                            (if (is-num (nth 1 tempUpdateStack))
                                (progn
                                    (setq num1 (parse-integer (nth 1 tempUpdateStack)))
                                    (setq num2 (parse-integer (nth 0 tempUpdateStack)))
                                    (setf num1 (* num1 num2))
                                    (setf outStack (append outStack (list (write-to-string num1))))
                                )
                                (setf errorList "SYNTAX_ERROR")
                            )
                        )
                    )
                    (T (setf errorList "SYNTAX_ERROR"))
                )
            )
        )
        ((> (list-length tempUpdateStack) 3) (setf errorList "SYNTAX_ERROR"))
    )
)

(defun is-True-False (strtemp)
    (cond
        ((string-equal strtemp "true") T)
        ((string-equal strtemp "false") T)
        ((string-equal strtemp "1") T)
        ((string-equal strtemp "0") T)
        (T nil)
    )
)

(defun True-False (strtemp)
    (cond
        ((string-equal strtemp "true") 1)
        ((string-equal strtemp "false") 0)
        ((string-equal strtemp "1") 1)
        ((string-equal strtemp "0") 0)
    )
)

(defun andOperator ()
    (cond
        ((is-True-False (nth 0 tempUpdateStack))
            (progn
                (if (is-True-False (nth 1 tempUpdateStack))
                    (progn
                        (setq num1 (True-False (nth 1 tempUpdateStack)))
                        (setq num2 (True-False (nth 0 tempUpdateStack)))
                        (setq num3 (and num1 num2))
                        (setf outStack (append outStack (list (write-to-string num3))))
                    )
                    (setf errorList "SYNTAX_ERROR")
                )
            )
        )
        (T (setf errorList "SYNTAX_ERROR"))
    )
)

(defun orOperator ()
    (cond
        ((is-True-False (nth 0 tempUpdateStack))
            (progn
                (if (is-True-False (nth 1 tempUpdateStack))
                    (progn
                        (setq num1 (True-False (nth 1 tempUpdateStack)))
                        (setq num2 (True-False (nth 0 tempUpdateStack)))
                        (setf num1 (or num1 num2))
                        (setf outStack (append outStack (list (write-to-string num1))))
                    )
                    (setf errorList "SYNTAX_ERROR")
                )
            )
        )
        (T (setf errorList "SYNTAX_ERROR"))
    )
)

(defun listFunc (templist index)
    (cond
        ((= -1 index) (setf outStack (append outStack (list templist))))
        (T
            (progn
                (cond
                    ((equal nil templist) 
                        (progn
                            (setf templist "(")
                            (setf templist (concatenate 'string templist (nth index tempUpdateStack)))
                            (if (= 3 (list-length tempUpdateStack))
                                (setf templist (concatenate 'string templist " "))
                            )
                        )
                    )
                    ((= 0 index)
                        (progn
                            (setf templist (concatenate 'string templist (nth index tempUpdateStack)))
                            (setf templist (concatenate 'string templist ")"))
                        )
                    )
                    (T 
                        (progn
                            (setf templist (concatenate 'string templist " "))
                            (setf templist (concatenate 'string templist (nth index tempUpdateStack)))
                            (setf templist (concatenate 'string templist " "))
                        )
                    )
                )
                (setf index (- index 1))
                (listFunc templist index)
            )
        )
    )
)

(defun setFunc ()
    (setf outStack (append outStack (list (nth 0 tempUpdateStack))))
)

(defun appendFunc ()
    (setq tempAppend "(")
    (setq tempAppendstr (nth 1 tempUpdateStack))
    (dotimes (i (length tempAppendstr))
        (cond
            ((not(or(= 0 i) (= i (- (length tempAppendstr)1))))
                (progn
                    (setf tempAppend (concatenate 'string tempAppend (string (char tempAppendstr i))))
                )
            )
        )  
    )
    (setf tempAppendstr (nth 0 tempUpdateStack))
    (setf tempAppend (concatenate 'string tempAppend " "))
    (dotimes (i (length tempAppendstr))
        (cond
            ((not(or(= 0 i) (= i (- (length tempAppendstr)1))))
                (progn
                    (setf tempAppend (concatenate 'string tempAppend (string (char tempAppendstr i))))
                )
            )
        )  
    )
    (setf tempAppend (concatenate 'string tempAppend ")"))
    (setf outStack (append outStack (list tempAppend)))
)

(defun ifFunc ()
    (setf tempUpdateStack (reverse tempUpdateStack))
    (cond
        ((= (list-length tempUpdateStack) 4)
            (progn
                (cond
                    ((string-equal "1" (nth 1 tempUpdateStack)) (setf outStack (append outStack (list (nth 2 tempUpdateStack)))))
                    ((string-equal "0" (nth 1 tempUpdateStack)) (setf outStack (append outStack (list (nth 3 tempUpdateStack)))))
                    (T (setf errorList "SYNTAX_ERROR"))
                )
            )
        )
        (T (setf errorList "SYNTAX_ERROR"))
    )
)

(defun notFunc ()
    (cond
        ((= 2 (list-length tempUpdateStack)) 
            (progn
                (cond
                    ((string-equal "1" (nth 0 tempUpdateStack)) (setf outStack (append outStack (list "0"))))
                    ((string-equal "0" (nth 0 tempUpdateStack)) (setf outStack (append outStack (list "1"))))
                )
            )
        )
        (T (setf errorList "SYNTAX_ERROR"))
    )
)

(defun update_ ()
    (setq templist nil)
    (cond
        ((string-equal "+" (nth (- (list-length tempUpdateStack) 1) tempUpdateStack)) (addOperator))
        ((string-equal "-" (nth (- (list-length tempUpdateStack) 1) tempUpdateStack)) (subOperator))
        ((string-equal "/" (nth (- (list-length tempUpdateStack) 1) tempUpdateStack)) (divOperator))
        ((string-equal "*" (nth (- (list-length tempUpdateStack) 1) tempUpdateStack)) (multOperator))
        ((string-equal "and" (nth (- (list-length tempUpdateStack) 1) tempUpdateStack)) (andOperator))
        ((string-equal "or" (nth (- (list-length tempUpdateStack) 1) tempUpdateStack)) (orOperator))
        ((string-equal "list" (nth (- (list-length tempUpdateStack) 1) tempUpdateStack)) (listFunc templist (- (list-length tempUpdateStack) 2)))
        ((string-equal "set" (nth (- (list-length tempUpdateStack) 1) tempUpdateStack)) (setFunc))
        ((string-equal "append" (nth (- (list-length tempUpdateStack) 1) tempUpdateStack)) (appendFunc))
        ((string-equal "if" (nth (- (list-length tempUpdateStack) 1) tempUpdateStack)) (ifFunc))
        ((string-equal "not" (nth (- (list-length tempUpdateStack) 1) tempUpdateStack)) (notFunc))
    )
    (setf tempUpdateStack nil)
)

(defun updateStack (control_update_stack)
    (cond
        ((string-equal "(" (nth control_update_stack outStack))
            (progn
                (removeLastElement)
                (update_)
            )
        )
        (T  (progn
                (setf tempUpdateStack (append tempUpdateStack (list (nth control_update_stack outStack))))
                (removeLastElement)
                (setf control_update_stack (- control_update_stack 1))
                (updateStack control_update_stack)
            )
        )
    )
)

(defun fillOutStack (line)
    (setq control_OP 0)
    (setf charList nil)
    (LineToCharList line)
    (setq temp_string nil)
    (dotimes (i (list-length charList))
        (setq temp_char (nth i charList))
        (cond
            ((char-equal #\+ temp_char) (addStack temp_char))
            ((char-equal #\- temp_char) (addStack temp_char))
            ((char-equal #\/ temp_char) (addStack temp_char))
            ((char-equal #\( temp_char) (addStack temp_char))
            ((char-equal #\" temp_char) 
                (progn
                    (cond
                        ((= control_OP 0)
                            (progn
                                (setf outStack (append outStack (list "OP_OC")))
                                (setf control_OP 1)
                            )
                        )
                        (T (progn
                            (setf outStack (append outStack (list "OP_CC")))
                            (setf control_OP 0)
                        ))
                    )
                )
            )
            ((char-equal #\* temp_char)
                (progn
                    (cond
                        ((char-equal #\* (nth (+ 1 i) charList)) 
                            (progn
                                (setf outStack (append outStack (list "**")))
                                (setf i (+ 1 i))
                            )
                        )
                        (T (addStack temp_char))
                    )
                )
            )
            ((equal T (is-OPCP temp_char)) 
                (progn
                    (cond
                        ((/= 0 (length temp_string))  
                            (progn
                                (setf outStack (append outStack (list temp_string)))
                                (setf temp_string nil)
                            )
                        )
                    )
                    (updateStack (- (list-length outStack) 1))
                )
            )
            ((char-equal temp_char #\Space ) 
                (progn
                    (cond
                        ((/= 0 (length temp_string))
                            (progn
                                (setf outStack (append outStack (list temp_string)))
                                (setf temp_string nil)
                            )
                        )
                    )  
                )
            )
            (T (setf temp_string (concatenate 'string temp_string (string temp_char))))
        )
    )
)

(defun writeFile ()
    (cond
        ((equal nil errorList)
            (progn
                (with-open-file (stream "output.txt" :direction :output)
                    (dotimes (m (list-length outFile))
                        (format stream (nth m outFile))
                        (terpri stream)
                    )
                )
            )
        )
        (T 
            (progn
                (with-open-file (stream "output.txt" :direction :output)
                    (format stream "SYNTAX_ERROR")
                )
            )
        )
    )   
)

(defun seperateList (i line)
    (if (/= i (list-length fileList)) 
        (progn
            (cond
                ((eq nil errorList)
                    (progn
                        (fillOutStack line)
                        ;(print outStack)
                        (outFileUpdate)
                        ;(print outFile)
                        (setf outStack nil)
                        (setf i (+ 1 i))
                        (seperateList i (nth i fileList))
                    )
                )
                (T (setf i (list-length fileList))) 
            )
        )
    )
)

(defun mylistAppend (line my_list)
    (setf my_list (append my_list(list line)))
    my_list
)

(defun controlError (control_index)
    (cond
        ((string= "SYNTAX_ERROR" (nth control_index outFileToken)) nil)
        ((= control_index (length outFileToken)) T)
        (T (controlError (+ 1 control_index)))
    )
)

(defun gppinterpreter ()
    (cond
        ((equal T (controlError 0)) 
            (progn
                (setf fileList listFile)
                (seperateList 0 (nth 0 listFile))
                 (writeFile)
            )
        )
        (T (print "SYNTAX_ERROR Expression not recognized"))
    ) 
)

(defun gppinterpreter_file (name) 
    (cond
        ((equal T (controlError 0)) 
            (progn
                (setf fileList listFile)
                (seperateList 0 (nth 0 listFile))
                (writeFile)
            )
        )
        (T (print "SYNTAX_ERROR Expression not recognized"))
    )  
)

(if (null *args*)
    (gppinterpreter)
    (gppinterpreter_file (car *args*)) 
)