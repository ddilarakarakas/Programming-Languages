(defun primeNumberRecursive (number control flag)
    (cond
        ((= control 1)
            (progn
                (cond
                    ((= 0 flag) 1)
                    (t 0)
                )
            )
        )
        ((= 0 (mod number control)) (primeNumberRecursive number (- control 1) (+ flag 1)))
        (t (primeNumberRecursive number (- control 1) flag))
    )
)

(defun primeNumber (number)
    (primeNumberRecursive number (- number 1) 0)
)

(defun isSemiPrime (num)
    (cond
        ((= 2 (semiPrimeRecursive num 0 2)) 1)
        (t 0)
    )
)

(defun semiPrimeRecursive (num control flag)
    (cond
        ((= flag num) control)
        ((= 0 (mod num flag)) 
            (progn
                (cond
                    ((= 1 (primeNumber flag)) 
                        (progn 
                            (cond
                                ((= flag (sqrt num)) (semiPrimeRecursive num (+ 2 control) (+ flag 1)))
                                (t (semiPrimeRecursive num (+ control 1) (+ flag 1)))
                            )
                        )
                    )
                    (t 0)
                )
            )
        )
        (t (semiPrimeRecursive num control (+ flag 1)))
    ) 
)

(defun writeFile (number1 number2)
    (setq i 1)
    (with-open-file (stream "primedistribution.txt" :direction :output)
        (cond
            ((= 1 number1 ) (setf number1 (+ number1 1)))
        )
        (loop for a from number1 to number2
            do (progn
                (cond
                    ((= 1 (primeNumber number1)) 
                        (progn
                            (format stream "~d is Prime" number1)
                            (terpri stream)
                        )
                    )
                    ((= 1 (isSemiPrime number1)) 
                        (progn
                            (format stream "~d is Semi-prime" number1)
                            (terpri stream)
                        )
                    )
                )
                (setf number1 (+ 1 number1))
            )
        )
    )
)

(defun convertToInt (line)
    (setq number1 nil)
    (setq number2 nil)
    (setq temp 0)
    (dotimes (i (length line))
        (setq ch (char line i))
        (cond
            ((char= ch #\space ) (setf temp (+ temp 1)))
            (t (progn
                    (cond
                        ((= temp 0) (setf number1 (append number1 (list ch))))
                        ((= temp 1) (setf number2 (append number2 (list ch))))
                    )
                )
            ) 
        )
    )
    (setq num1 (parse-integer (concatenate 'string number1)))
    (setq num2 (parse-integer (concatenate 'string number2)))
    (writeFile num1 num2)
)

(defun primecrawler (name)
    (let ((in (open name :if-does-not-exist nil)))
        (when in
            (loop for line = (read-line in nil)
                while line do (convertToInt line)
            )
            (close in)
        )
    )
)

(primecrawler "boundries.txt")