cms:
	mkdir -p build
	cp page.html build/index.html
	mkdir -p build/data
	rsync -r _user user _blog blog build/data
	elm-make --yes src/Main.elm --output build/elm.js

clean:
	rm -rf elm-stuff/build-artifacts
	rm -rf build
