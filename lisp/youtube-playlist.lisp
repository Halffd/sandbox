(defun playlist-items (url)
  (let* ((tree
          (with-current-buffer (url-retrieve-synchronously url)
            (goto-char (point-min))
            (search-forward "\n\n")
            (xml-parse-region (point) (point-max)))))
    (mapcar
     (lambda (entry)
       (cons (dom-by-tag entry 'title) (dom-by-tag entry 'link)))
     (dom-by-tag tree 'entry))))