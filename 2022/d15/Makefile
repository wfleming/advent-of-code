run: target/release/d15
	$< input.txt 2000000

target/release/d15: **/*.rs
	cargo build --release

sample:
	cargo run sample.txt 10
