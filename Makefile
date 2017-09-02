cms:
	mkdir -p build
	cp page.html build/index.html
	rsync -r user blog build
	elm-make --yes src/Main.elm --output build/elm.js

pages: cms
	(cd build && git add . && git commit -m "Update gh-pages")

clean:
	rm -rf elm-stuff/build-artifacts
	rm -rf build
