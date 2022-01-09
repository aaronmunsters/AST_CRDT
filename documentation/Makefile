main.pdf: main.tex bibliography.bib images/out/* test-report/test-report.tex sections/*
	pdflatex -shell-escape main
	bibtex   main               1>/dev/null
	pdflatex -shell-escape main 1>/dev/null
	pdflatex -shell-escape main 1>/dev/null
	rm -rf main.bbl main.blg main.log main.out main.aux _minted-main

test-report/test-report.tex: ../src/test/**/*
	# src: https://superuser.com/a/271992
	# src: https://www.scalatest.org/user_guide/using_scalatest_with_sbt
	(cd .. ; sbt "test" | tee documentation/test-report/test-report.txt)
	test-report/reportExtractor.py
	rm -rf target project

images/out/*: images/source/*
	./images/compile.sh