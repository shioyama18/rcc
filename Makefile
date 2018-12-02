rcc: src/main.rs
	cargo build

test: rcc
	./test.sh

clean:
	rm -f tmp tmp.s
