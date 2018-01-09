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


bundle.js:
	cd ./client && \
  bower install && \
	pulp build --optimise --to ../static/bundle.js


Partie-exe: setup
	stack build


setup:
	stack setup
