build:
	sbcl --load muz.asd \
	     --eval '(ql:quickload :muz)' \
		 --eval '(asdf:make :muz)' \
		 --eval '(quit)'
