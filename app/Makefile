output/app.min.js: output/app.js
	# TODO: This also removes the PS entry point. How can we keep it?
	closure-compiler --js_output_file=$@ $^

output/app.js: output/Main/index.js
	purs bundle output/*/*.js -m Main > output/app.js

output/Main/index.js: src/*.purs src/*.js psc-package.json
	psc-package build
