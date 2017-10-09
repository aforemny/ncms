build:
	mkdir -p ../site
	cp page.html ../site/index.html
	elm-make --yes frontend/Main.elm --output ../site/elm.js
