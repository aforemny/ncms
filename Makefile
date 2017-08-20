cms:
	mkdir -p build
	#cp page.html build/index.html
	elm-make --yes src/Main.elm --output build/index.html

clean:
	rm -rf elm-stuff/build-artifacts
	rm -rf build
