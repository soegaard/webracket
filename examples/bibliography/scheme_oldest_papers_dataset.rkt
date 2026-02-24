#lang racket

;; Auto-generated dataset of early Scheme / Scheme-adjacent papers.
;; Generated: 2026-01-27T18:53:29Z

(define scheme-paper-dataset
  (hash
    (generated_utc "2026-01-27T18:53:29Z")
    (notes (list
      "This file contains a best-effort, source-verified subset of the requested dataset."
      "Many older entries on readscheme.org point at repository.readscheme.org PDFs, which were returning HTTP 502 from this environment; where possible, I substituted stable PDFs (MIT DSpace, standards.scheme.org, bitsavers, etc.)."
      "Dates are as stated on the archival landing page or on the PDF title page; if only year/month is available, day is set to 0."
    ))
    (papers (list
      (hash
        (category "strict-scheme")
        (date (hash
          (year 1975)
          (month 12)
          (day 1)
        ))
        (title "SCHEME: An Interpreter for Extended Lambda Calculus")
        (authors (list
          "Gerald Jay Sussman"
          "Guy Lewis Steele Jr."
        ))
        (venue "MIT AI Memo AIM-349")
        (landing "https://dspace.mit.edu/handle/1721.1/5794")
        (pdf "https://dspace.mit.edu/bitstream/handle/1721.1/5794/AIM-349.pdf")
      )
      (hash
        (category "strict-scheme")
        (date (hash
          (year 1978)
          (month 1)
          (day 1)
        ))
        (title "The Revised Report on SCHEME: A Dialect of LISP")
        (authors (list
          "Guy Lewis Steele Jr."
          "Gerald Jay Sussman"
        ))
        (venue "MIT AI Memo AIM-452 (R1RS)")
        (landing "https://dspace.mit.edu/handle/1721.1/6283")
        (pdf "https://dspace.mit.edu/bitstream/handle/1721.1/6283/AIM-452.pdf")
      )
      (hash
        (category "strict-scheme")
        (date (hash
          (year 1985)
          (month 8)
          (day 0)
        ))
        (title "The Revised Revised Report on Scheme, or, An UnCommon Lisp")
        (authors (list
          "William Clinger"
          "Jonathan Rees"
          "et al."
        ))
        (venue "MIT AI Memo AIM-848 (R2RS)")
        (landing "https://dspace.mit.edu/bitstream/handle/1721.1/5600/AIM-848.pdf")
        (pdf "https://dspace.mit.edu/bitstream/handle/1721.1/5600/AIM-848.pdf")
      )
      (hash
        (category "strict-scheme")
        (date (hash
          (year 1986)
          (month 12)
          (day 0)
        ))
        (title "Revised^3 Report on the Algorithmic Language Scheme (R3RS)")
        (authors (list
          "Harold Abelson"
          "Richard K. Dybvig"
          "Christopher T. Haynes"
          "Guillermo J. Rozas"
          "Norman I. Adams IV"
          "Daniel P. Friedman"
          "Eugene Kohlbecker"
          "Guy L. Steele Jr."
          "David H. Bartley"
          "Robert Halstead"
          "David Oxley"
          "Gerald J. Sussman"
          "Gerald Brooks"
          "Chris Hanson"
          "Kent M. Pitman"
          "Mitchell Wand"
          "William Clinger"
          "Jonathan Rees (editors)"
        ))
        (venue "R3RS")
        (landing "https://standards.scheme.org/official/r3rs.pdf")
        (pdf "https://standards.scheme.org/official/r3rs.pdf")
      )
      (hash
        (category "strict-scheme")
        (date (hash
          (year 1991)
          (month 11)
          (day 2)
        ))
        (title "Revised^4 Report on the Algorithmic Language Scheme (R4RS)")
        (authors (list
          "William Clinger"
          "Jonathan Rees (editors)"
          "and others"
        ))
        (venue "R4RS")
        (landing "https://groups.csail.mit.edu/mac/classes/6.001/FT98/manuals/r4rs/r4rs.pdf")
        (pdf "https://groups.csail.mit.edu/mac/classes/6.001/FT98/manuals/r4rs/r4rs.pdf")
      )
      (hash
        (category "strict-scheme")
        (date (hash
          (year 1989)
          (month 1)
          (day 0)
        ))
        (title "SCHEME->C: A Portable Scheme-to-C Compiler")
        (authors (list
          "Joel F. Bartlett"
        ))
        (venue "DEC Western Research Laboratory, Research Report 89/1")
        (landing "https://bitsavers.org/pdf/dec/tech_reports/WRL-89-1.pdf")
        (pdf "https://bitsavers.org/pdf/dec/tech_reports/WRL-89-1.pdf")
      )
      (hash
        (category "strict-scheme")
        (date (hash
          (year 1986)
          (month 6)
          (day 0)
        ))
        (title "ORBIT: an optimizing compiler for Scheme")
        (authors (list
          "David A. Kranz"
          "Richard A. Kelsey"
          "Jonathan A. Rees"
          "Paul Hudak"
          "James Philbin"
        ))
        (venue "SIGPLAN Symposium on Compiler Construction (1986)")
        (landing "https://dl.acm.org/doi/10.1145/13310.13333")
        (pdf "https://www.ccs.neu.edu/home/shivers/cs6983/papers/orbit-pldi86.pdf")
      )
      (hash
        (category "scheme-adjacent")
        (date (hash
          (year 1976)
          (month 3)
          (day 1)
        ))
        (title "LAMBDA: The Ultimate Imperative")
        (authors (list
          "Guy Lewis Steele Jr."
          "Gerald Jay Sussman"
        ))
        (venue "MIT AI Memo AIM-353")
        (landing "https://dspace.mit.edu/handle/1721.1/5790")
        (pdf "https://dspace.mit.edu/bitstream/handle/1721.1/5790/AIM-353.pdf")
      )
      (hash
        (category "scheme-adjacent")
        (date (hash
          (year 1976)
          (month 11)
          (day 1)
        ))
        (title "LAMBDA: The Ultimate Declarative")
        (authors (list
          "Guy Lewis Steele Jr."
          "Gerald Jay Sussman"
        ))
        (venue "MIT AI Memo AIM-379")
        (landing "https://dspace.mit.edu/handle/1721.1/6091")
        (pdf "https://dspace.mit.edu/bitstream/handle/1721.1/6091/AIM-379.pdf")
      )
      (hash
        (category "scheme-adjacent")
        (date (hash
          (year 1977)
          (month 10)
          (day 0)
        ))
        (title "Debunking the \"Expensive Procedure Call\" Myth, or, Procedure Call Implementations Considered Harmful, or, LAMBDA: The Ultimate GOTO")
        (authors (list
          "Guy Lewis Steele Jr."
          "Gerald Jay Sussman"
        ))
        (venue "MIT AI Memo AIM-443")
        (landing "https://dspace.mit.edu/bitstream/handle/1721.1/5753/AIM-443.pdf")
        (pdf "https://dspace.mit.edu/bitstream/handle/1721.1/5753/AIM-443.pdf")
      )
      (hash
        (category "scheme-adjacent")
        (date (hash
          (year 1978)
          (month 5)
          (day 0)
        ))
        (title "The Art of the Interpreter, or, The Modularity Complex (Parts Zero, One, and Two)")
        (authors (list
          "Guy Lewis Steele Jr."
          "Gerald Jay Sussman"
        ))
        (venue "MIT AI Memo AIM-453")
        (landing "https://dspace.mit.edu/bitstream/handle/1721.1/6094/AIM-453.pdf")
        (pdf "https://dspace.mit.edu/bitstream/handle/1721.1/6094/AIM-453.pdf")
      )
      (hash
        (category "scheme-adjacent")
        (date (hash
          (year 1988)
          (month 0)
          (day 0)
        ))
        (title "λv-CS: An extended λ-calculus for Scheme")
        (authors (list
          "Matthias Felleisen"
        ))
        (venue "LFP 1988 (ACM)")
        (landing "https://dl.acm.org/doi/10.1145/62678.62686")
        (pdf "https://dl.acm.org/doi/pdf/10.1145/62678.62686")
      )
    ))
  ))

(provide scheme-paper-dataset)
