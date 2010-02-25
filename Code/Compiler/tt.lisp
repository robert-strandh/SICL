;;; An if that generates a value

(if test then else)

(let (val)
  (if test
      (setq val then)
      (setq val else))
  val)

(let (val)
  (tagbody
   (if test (go then))
   else
     (setq val else)
     (go out)
   then
     (setq val then)
   out)
  val)

(if test then else)

(tagbody
   (if test (go then))
 else
   (progn else)
   (go out)
 then
   (progn then)
 out)


(if (if x x thing) then else)

(tagbody
   (tagbody
      (if x (go then1))
    else1
      (progn x)
 else
   (progn else)
   (go out)
 then
   (progn then)
 out)

(if (if a b c) d e)

(flet ((d () d)
       (e () e))
  (if a
      (if b
	  (d)
	  (e))
      (if c
	  (d)
	  (e))))

(if (if a a c) d e)

(flet ((d () d)
       (e () e))
  (if a
      (d)
      (if c
	  (d)
	  (e))))



(if (if a b c) d e)

(let (v)
  (if a
      (setq v b)
      (setq v c))
  (if v
      d
      e))