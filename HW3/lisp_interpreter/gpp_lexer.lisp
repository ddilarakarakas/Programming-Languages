(setq listFile nil)
(setq outFileToken nil)
(setq wordCharList nil)
(setq KEYWORDS '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(setq KEYWORDS_TOKEN '("KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT"
                       "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(setq OPERATORS '("+" "-" "/" "(" ")" ","))
(setq OPERATORS_TOKEN '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_OP" "OP_CP" "OP_COMMA"))

(defun fillOutFileToken (line)
    (setq controlMULT 0)
    (dotimes (j (length line))
        (setq ch (char line j))
        (setf wordCharList (append wordCharList (list ch)))
    )
    (cond 
        ((equal (not (null wordCharList)) nil))
        (t (progn  
                (charWordListtoStringWordList wordCharList)
                (setf wordCharList nil)
            )
        )
    )
)

(defun searchOperators (searchWord)
    (setq index -1)
    (dotimes (i (list-length OPERATORS))
        (setq temp (nth i OPERATORS))
        (cond
            ((string-equal temp searchWord) (setf index i ))
        )
    )
    index
)

; Return T if the ch is between the 'A' and 'z' according the ASCII code ...
(defun is-character (ch)
    (and (<= (char-code ch) 122) (>= (char-code ch) 65))
)

; Return T if the nb is between the '0' and '9' according the ASCII code ...
(defun is-number (nb)
    (and (<= (char-code nb) 57) (>= (char-code nb) 48)) 
)

(defun is-operator (ch)
    (setq isChar (is-character ch))
    (setq isNum (is-number ch))
    (if (or (eq T isChar) (eq T isNum))
        -1
        T
    )
)

(defun searchKeyword (searchWord)
    (setq index -1)
    (dotimes (i (list-length KEYWORDS))
        (setq temp (nth i KEYWORDS))
        (cond
            ((string-equal temp searchWord) (setf index i ))
        )
    )
    index
)

(defun isValue (str)
    (setq control T)
    (cond
        ((= (length str) 1) 
            (progn
                (setq ch2 (char str 0))
                (if (eq T (not (eq T (is-number ch2))))
                        (setf control nil)
                )
            )
        )
        ((char= #\0 (char str 0)) (setf control nil))
        (t  (progn
                (dotimes (i (length str))
                    (setq ch2 (char str i))
                    (if (eq T (not (eq T (is-number ch2))))
                        (setf control nil)
                    )
                )
            )
        )
    )
    control
)

(defun isIdentifier (str)
    (setq controlIde T)
    (cond
        ((eq T (is-number (char str 0))) (setf controlIde nil))
        (t  (progn
                (dotimes (i (length str))
                    (setq ch1 (char str i))
                    (if (and (eq T (not (eq T (is-character ch1)))) (eq T (not (eq T (is-number ch1)))))
                        (setf controlIde nil)
                    )
                )
            )
        )
    )
    controlIde
)

(defun addIdentifierValue (str)
    (cond
        ((eq T (isIdentifier str)) (setf outFileToken (cons "IDENTIFIER" outFileToken)))
        ((eq T (isValue str)) (setf outFileToken (cons "VALUE" outFileToken)))
        (t (setf outFileToken (cons "SYNTAX_ERROR" outFileToken)))
    )
)

(defun updateToken (str)
    (if (not (eq nil str))
        (progn
            (setq indexKeyword (searchKeyword str))
            (cond
                ((= -1 indexKeyword) (addIdentifierValue str))
                (T (setf outFileToken (cons (nth indexKeyword KEYWORDS_TOKEN) outFileToken)))
            )
        )
    )
)

(defun charWordListtoStringWordList (word_)
    (setq control_OP_OC 0)
    (setq str nil)
    (dotimes (i (list-length word_))
        (setq ch (nth i word_))
        (cond
            ((char-equal #\Space ch) 
                (progn
                    (updateToken str)
                    (setf str nil)
                )
            )
            ((char-equal #\; ch) 
                (progn
                    (cond
                        ((char-equal #\; (nth (+ 1 i) word_)) (setf outFileToken (cons "COMMENT" outFileToken)))
                        (t (setf outFileToken (cons "SYNTAX_ERROR" outFileToken)))
                    )
                    (setf i (+ 3 (list-length word_)))
                )
            )
            ((char-equal #\" ch) 
                (progn
                    (updateToken str)
                    (setf str nil)
                    (if (= control_OP_OC 1)
                        (progn
                            (setf outFileToken (cons "OP_CC" outFileToken))
                            (setf control_OP_OC 0)
                        )
                        (progn
                            (setf outFileToken (cons "OP_OC" outFileToken))
                            (setf control_OP_OC 1) 
                        )
                    )
                )
            )
            ((eq T (is-operator ch))
                (progn
                    (if (eq nil str)
                        (progn
                            (cond
                                ((char-equal #\* ch) 
                                    (progn
                                        (if (char-equal #\* (nth (+ i 1) word_))
                                            (progn
                                                (setf outFileToken (cons "OP_DBLMULT" outFileToken))
                                                (setf i (+ 1 i))
                                            )
                                            (setf outFileToken (cons "OP_MULT" outFileToken))
                                        )
                                    )
                                )
                                (t  (progn
                                        (setq in (searchOperators (string ch)))
                                        (if (/= -1 in)
                                            (setf outFileToken (cons (nth in OPERATORS_TOKEN) outFileToken))
                                        )
                                    )
                                )
                            )
                            
                        )
                        (progn
                            (cond 
                                ((= (+ i 1) (list-length word_))
                                    (progn
                                        (updateToken str)
                                        (setf str nil)
                                        (setq in (searchOperators (string ch)))
                                        (if (/= -1 in)
                                            (setf outFileToken (cons (nth in OPERATORS_TOKEN) outFileToken))
                                        )
                                    )
                                )
                                ((char= #\Space (nth (+ 1 i) word_))
                                    (progn
                                        (updateToken str)
                                        (setf str nil)
                                        (setq in (searchOperators (string ch)))
                                        (if (/= -1 in)
                                            (setf outFileToken (cons (nth in OPERATORS_TOKEN) outFileToken))
                                        )
                                    )
                                )
                                ((eq T (is-operator (nth (+ 1 i) word_)))
                                    (progn
                                        (updateToken str)
                                        (setf str nil)
                                        (setq in (searchOperators (string ch)))
                                        (if (/= -1 in)
                                            (setf outFileToken (cons (nth in OPERATORS_TOKEN) outFileToken))
                                        )
                                    )
                                )
                                (t  (setf str (concatenate 'string str (string ch))))
                            )
                        )                
                    )
                    
                )
            )
            (t  (progn
                    (cond
                        ((= 1 (list-length word_)) (updateToken (string ch)))
                        ((= (+ i 1) (list-length word_))
                            (progn
                                (setf str (concatenate 'string str (string ch)))
                                (updateToken str)
                                (setf str nil)
                            )
                        )
                        (t  (setf str (concatenate 'string str (string ch))))
                    )
                )
            )
        )
    )
)

(defun splitList (i line)
    (if (/= i (list-length listFile)) 
        (progn
            (fillOutFileToken line)
            (setf i (+ 1 i))
            (splitList i (nth i listFile))
        )
    )
)

(defun fillList (line my_list)
    (setf my_list (append my_list(list line)))
    my_list
)



(defun gppinterpreter_file_lexer (name) 
    (let ((in (open name :if-does-not-exist nil)))
        (when in
            (loop for line = (read-line in nil)
                while line do (setf listFile (fillList line listFile))
            )
            (close in)
        )
    )
    (setq i 0)
    (splitList i (nth 0 listFile))
    (setf outFileToken (reverse outFileToken))
    ;(writeFile)
)

(defun gppinterpreter_lexer ()
    ;(gppinterpreter_file "grammar.txt")
    (gppinterpreterRecursive_Lexer (setq controlLine nil))
    (splitList 0 (nth 0 listFile))
    (setf outFileToken (reverse outFileToken))
    ;(writeFile)
)

(defun gppinterpreterRecursive_Lexer (controlLine)
    (setq controlLine (read-line))
    (if (string-not-equal "" controlLine)
        (progn
            (setf listFile (fillList controlLine listFile))
            (gppinterpreterRecursive_Lexer controlLine)
        )
    )
)

(if (null *args*)
    (gppinterpreter_lexer)
    (gppinterpreter_file_lexer (car *args*)) 
)