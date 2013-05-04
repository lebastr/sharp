test: tests/test2.hs
	cd tests; fay --package fay-ref,fay-jquery  --include ..  test2.hs

test1.js: *.hs
	hastec test1.hs --with-js=time.js,console.js
clean:
	rm -f *.hi *.o *.jsmod tests/*.js
