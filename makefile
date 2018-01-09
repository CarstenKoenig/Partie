Partie: Partie-exe bundle.js


start: Partie
	stack exec Partie


install: Partie
	stack install


clean:
	rm -rf .stack-work/
	rm -f ./static/bundle.js
	rm -rf ./client/.pulp-cache
	rm -rf ./client/bower_components
	rm -rf ./client/output
	rm ./client/src/Test.purs


bundle.js: test.purs
	cd ./client && \
	bower install && \
	pulp build --optimise --to ../static/bundle.js


test.purs: Partie-exe
	stack exec PS-bridge


Partie-exe: setup
	stack build


setup:
	stack setup
