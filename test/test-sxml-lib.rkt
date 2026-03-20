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
(check-equal (sxml->html '(a (@ (title "it's ok")) "x"))
             "<a title=\"it&#39;s ok\">x</a>"
             "attribute single quote escaping")
(check-equal (sxml->html '(p "\"'"))
             "<p>\"'</p>"
             "text keeps quotes")
(check-equal (sxml->html '(div))
             "<div></div>"
             "empty element")
(check-equal (sxml->html '(div ""))
             "<div></div>"
             "empty text child")
(check-equal (sxml->html '())
             ""
             "empty root list")
(check-equal (sxml->html '(p "a" (b "x") "c"))
             "<p>a<b>x</b>c</p>"
             "mixed content ordering")
(check-equal (sxml->html '(div (@ (class "x")) @))
             "<div class=\"x\">@</div>"
             "at-symbol child")
(check-equal (sxml->html '(div (@ (data-n 7) (data-s ok)) "x"))
             "<div data-n=\"7\" data-s=\"ok\">x</div>"
             "number and symbol attribute values")
(check-equal (sxml->html '(section (article (p "x & y"))))
             "<section><article><p>x &amp; y</p></article></section>"
             "nested escaping")

12345
