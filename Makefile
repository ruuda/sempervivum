BIN_PATH = .stack-work/dist/x86_64-linux-nix/Cabal-2.4.0.1/build/sempervivum/sempervivum

out: app $(BIN_PATH) assets/*.svg app/*.html app/*.css app/*.json
	mkdir -p out
	mkdir -p out/assets
	$(BIN_PATH) export-species > out/species.json
	# In the source we use the non-minified versions for faster development, but
	# in release we want to use the minified versions.
	sed -e 's/app\.js/app.min.js/' -e 's/sw\.js/sw.min.js/' app/index.html > out/index.html
	sed -e 's/app\.js/app.min.js/' -e 's/sw\.js/sw.min.js/' app/output/app.min.js > out/app.min.js
	sed -e 's/app\.js/app.min.js/' -e 's/sw\.js/sw.min.js/' app/output/sw.min.js > out/sw.min.js
	cp app/manifest.json out/manifest.json
	cp app/style.css out/style.css
	cp assets/check.svg out/assets/check.svg
	cp assets/droplet.svg out/assets/droplet.svg
	cp assets/plant.svg out/assets/plant.svg

$(BIN_PATH): app/*.hs src/*.hs
	stack build

app:
	$(MAKE) -C app

.PHONY: app out
