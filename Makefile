
.PHONY: compile
compile:
	sbt fullOptJS

.PHONY: deploy
deploy:
	mkdir -p dist/target/scala-2.11
	cp favicon.ico dist/favicon.ico
	cp lambda.html dist/index.html
	cp target/scala-2.11/parser-opt.js dist/target/scala-2.11/parser-opt.js
