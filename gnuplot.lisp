(in-package :bld-solarsailwebapp)

(defun write-traj (traj)
  (with-open-file (s "www/sail.gp" :direction :output :if-exists :supersede)
    (format s "set grid polar xtics ytics~%")
    (format s "set size ratio -1~%")
    (format s "set terminal png~%")
    (format s "set output 'sail.png'~%")
    (format s "set polar~%")
    (format s "plot '-' using 3:2 with lines notitle~%")
    (loop for (tm (r th vr vt)) in traj
       do (write-csv-row (list tm r th vr vt) :stream s :separator #\space :newline '(#\Newline)))))
