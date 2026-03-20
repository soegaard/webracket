(include-lib sxml)

(define (check-equal got want label)
  (unless (equal? got want)
    (error 'check-equal label)))

(check-equal (sxml->html '(div (@ (class "x") (data-id "7")) "Hello <&>" (span "world")))
             "<div class=\"x\" data-id=\"7\">Hello &lt;&amp;&gt;<span>world</span></div>"
             "element + attrs + escaping")
(check-equal (sxml->html '((p "A") (p "B")))
             "<p>A</p><p>B</p>"
             "root list")
(check-equal (sxml->html "plain text") "plain text" "text root")
(check-equal (sxml->html 42) "42" "number root")
(check-equal (sxml->html '(a (@ (href "https://example.com?q=1&x=2") (title "say \"hello\"")) "link"))
             "<a href=\"https://example.com?q=1&amp;x=2\" title=\"say &quot;hello&quot;\">link</a>"
             "attribute escaping")

12345
