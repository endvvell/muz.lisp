build-and-run:
	sbcl --load muz.asd \
	     --eval '(ql:quickload :muz)' \
		 --eval '(asdf:make :muz)' \
		 --eval '(quit)'
