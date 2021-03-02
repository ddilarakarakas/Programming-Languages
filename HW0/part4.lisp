(defstruct letter 
   charecter 
   frekans 
   leftChild 
   rightChild
   path
)
(defstruct huffmanCode
    str
    size
)

(defun fillList (line my_list)
    (dotimes (i (length line))
        (setq ch (char line i))
        (setq index (searchList my_list ch))
        (cond
            ((= -1 index)
                (progn
                    (setq letter
                            (make-letter    
                                :charecter ch
                                :frekans 1
                                :leftChild nil
                                :rightChild nil
                                :path nil
                            )
                    )
                    (setf my_list (append my_list (list letter)))
                )
            )
            (t (setf (letter-frekans (nth index my_list)) (+ 1 (letter-frekans (nth index my_list)))))
        )
    )
    (setf my_list (sortList my_list))
    my_list
)

(defun sortList (my_list)
    (setq size (list-length my_list))
    (dotimes (i (- size 1))
        (dotimes (j (- (- size i) 1))
            (setq frenkans1 (letter-frekans (nth j my_list)))
            (setq frenkans2 (letter-frekans (nth (+ 1 j) my_list)))
            (cond
                ((> frenkans1 frenkans2)
                    (progn
                        (setq temp (nth j my_list))
                        (setf (nth j my_list) (nth (+ 1 j) my_list))
                        (setf (nth (+ 1 j) my_list) temp)
                    )
                )
            )
        )
    )
    my_list
)

(defun sortHuffmanList (listHuffman)
    (setq size (list-length listHuffman))
    (dotimes (i (- size 1))
        (dotimes (j (- (- size i) 1))
            (setq size1 (huffmanCode-size (nth j listHuffman)))
            (setq size2 (huffmanCode-size (nth (+ 1 j) listHuffman)))
            (cond
                ((> size1 size2)
                    (progn
                        (setq temp (nth j listHuffman))
                        (setf (nth j listHuffman) (nth (+ 1 j) listHuffman))
                        (setf (nth (+ 1 j) listHuffman) temp)
                    )
                )
            )
        )
    )
    listHuffman
)

(defun searchList (my_list ch)
    (setq index -1)
    (dotimes (i (list-length my_list))
        (setq temp (nth i my_list))
        (cond
            ((char= ch (letter-charecter temp)) 
                (progn
                    (setf index i)
                )
            )
        )  
    )
    index
)

(defun huffmanTree (my_list)
    (cond
        ((= (list-length my_list) 1) my_list)
        (t  (progn
                (setq temp1 (nth 0 my_list))
                (setq temp2 (nth 1 my_list))
                (setf my_list (cdr my_list))
                (setf my_list (cdr my_list))
                (cond
                    ((> (letter-frekans temp1) (letter-frekans temp2)) 
                        (progn
                            (setf (letter-path temp1) "1")
                            (setf (letter-path temp2) "0")
                            (setq letter
                                (make-letter    
                                    :charecter nil
                                    :frekans (+ (letter-frekans temp1) (letter-frekans temp2))
                                    :leftChild temp2
                                    :rightChild temp1
                                    :path nil
                                )
                            )
                            (setf my_list (append my_list (list letter)))
                        )
                    )
                    (t  (progn
                            (setf (letter-path temp1) "0")
                            (setf (letter-path temp2) "1")
                            (setq letter
                                (make-letter    
                                    :charecter nil
                                    :frekans (+ (letter-frekans temp1) (letter-frekans temp2))
                                    :leftChild temp1
                                    :rightChild temp2
                                    :path nil
                                )
                            )
                            (setf my_list (append my_list (list letter)))
                        )
                    )
                )
                (sortList my_list)
                (huffmanTree my_list)
            )
        )
    )
)

(defun updatePath (node str)
    (if (not (eq node nil))
        (progn
            (setf (letter-path node) (concatenate 'string str (letter-path node)))
            (updatePath (letter-rightChild node) (letter-path node))
            (updatePath (letter-leftChild node) (letter-path node))
        )
        node
    )
)

(defun createHuffmanCodeList (huffmanCodeList root)
    (if (not (eq root nil))
        (progn
            (setq ch (letter-charecter root))
            (cond
                ((not (eq ch nil))
                    (progn
                        (cond
                            ((char= #\newline ch) (setf temp (concatenate 'string "Newline" ": ")))
                            ((char= #\space ch) (setf temp (concatenate 'string "Space" ": ")))
                            (t (setf temp (concatenate 'string (string ch) ": ")))
                        )
                        (setf temp (concatenate 'string temp (letter-path root)))
                        (setq huffmanCode
                            (make-huffmanCode    
                                :str temp
                                :size (length (letter-path root))
                            )
                        )
                        (setf huffmanCodeList (append huffmanCodeList (list huffmanCode)))
                    )
                )
            )
            (setf huffmanCodeList (createHuffmanCodeList huffmanCodeList (letter-rightChild root)))
            (setf huffmanCodeList (createHuffmanCodeList huffmanCodeList (letter-leftChild root)))
        )
        huffmanCodeList
    )
)

(defun writeFile (ls)
    (setq i 1)
    (with-open-file (stream "huffman_codes.txt" :direction :output)
        (dotimes (i (length ls))
            (format stream (huffmanCode-str (nth i ls)))
            (terpri stream)
        )
    )
)

(defun readFile (name)
    (setq my_list nil)
    (setq huffmanCodeList nil)
    (setq controlLine 0)
    (let ((in (open name :if-does-not-exist nil)))
        (when in
            (loop for line = (read-line in nil)
                while line do (setf my_list (fillList line my_list))
                (setf controlLine (+ 1 controlLine))
            )
            (close in)
        )
    )
    (setq letter
        (make-letter    
            :charecter #\newline
            :frekans controlLine
            :leftChild nil
            :rightChild nil
            :path nil
        )
    )
    (setf my_list (append my_list (list letter)))
    (setf my_list (sortList my_list))
    ;(print my_list)
    (setf my_list (huffmanTree my_list))
    (setf my_list (nth 0 my_list))
    (updatePath my_list (letter-path my_list))
    (setf huffmanCodeList (createHuffmanCodeList huffmanCodeList my_list))
    (setf huffmanCodeList (sortHuffmanList huffmanCodeList))
    (writeFile huffmanCodeList)
)

(readFile "paragraph.txt")