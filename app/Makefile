all: output/app.min.js output/sw.min.js

clean:
	rm output/app.js output/app.min.js
	rm output/sw.js output/sw.min.js

output/app.min.js: output/app.js
	closure-compiler --isolation_mode=IIFE --js_output_file=$@ $^

output/sw.min.js: output/sw.js
	closure-compiler --isolation_mode=IIFE --js_output_file=$@ $^

# Apart from bundling the compiled PureScript sources, we also add the js
# entry point that registers PureScript functions as event handlers. A nice side
# effect of adding that, is that Closure in the minimization step does not
# eliminate everything as dead code.

output/app.js: output/Main/index.js src/Main.entry.js
	spago bundle --minify --bundle-type module --module Main --outfile output/app.bundle.js
	esbuild --bundle --minify src/Main.entry.js > $@

output/sw.js: output/ServiceWorker/index.js src/ServiceWorker.entry.js
	spago bundle --minify --bundle-type module --module ServiceWorker --outfile output/sw.bundle.js
	esbuild --bundle --minify src/ServiceWorker.entry.js > $@

output/Main/index.js output/ServiceWorker/index.js &: src/*.purs src/*.js spago.yaml spago.lock
	spago build
