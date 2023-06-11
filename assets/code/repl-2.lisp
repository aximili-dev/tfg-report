> (defun fact (n)
    (if (eq n 0)
	1
        (* n (fact (1- n)))))
FACT
