UTIL_BIN_PATH = util/.stack-work/dist/x86_64-linux-nix/Cabal-2.4.0.1/build/sempervivum/sempervivum

out: app/output/app.min.js app/output/sw.min.js assets app/*.html
	mkdir -p out
	# In the source we use the non-minified versions for faster development, but
	# in release we want to use the minified versions.
	sed -e 's/app\.js/app.min.js/' -e 's/sw\.js/sw.min.js/' app/index.html > out/index.html
	sed -e 's/app\.js/app.min.js/' -e 's/sw\.js/sw.min.js/' app/output/app.min.js > out/app.min.js
	sed -e 's/app\.js/app.min.js/' -e 's/sw\.js/sw.min.js/' app/output/sw.min.js > out/sw.min.js

debug: app/output/app.js app/output/sw.js assets app/*.html
	mkdir -p out
	cp app/index.html out/index.html
	cp app/output/app.js out/app.js
	cp app/output/sw.js out/sw.js

assets: assets/*.svg app/*.css app/*.json out/species.json
	mkdir -p out/assets
	cp app/manifest.json out/manifest.json
	cp app/style.css out/style.css
	cp assets/check.svg out/assets/check.svg
	cp assets/droplet.svg out/assets/droplet.svg
	cp assets/plant.svg out/assets/plant.svg

out/species.json: $(UTIL_BIN_PATH)
	# Convert species toml files into a single json file using the utility.
	mkdir -p out
	$(UTIL_BIN_PATH) > out/species.json

$(UTIL_BIN_PATH): util/src/*.hs
	cd util && stack build

app/output/app.js: app/src/*.purs app/src/*.js
	$(MAKE) -C app output/app.js output/sw.js

app/output/app.min.js: app/src/*.purs app/src/*.js
	$(MAKE) -C app output/app.min.js output/sw.min.js

.PHONY: assets debug out
