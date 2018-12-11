.PHONY: test run

IMAGE_NAME=wfleming/aoc-d10

image:
	docker build -t $(IMAGE_NAME) .

run:
	ruby -I. d10.rb input.txt

sample:
	ruby -I. d10.rb input-sample.txt

shell: image
	docker run --rm -it \
		--workdir /work \
		--volume $(PWD):/work \
		$(IMAGE_NAME) \
		bash

clean:
	docker rmi $(IMAGE_NAME) || true
	docker rmi ruby:2.5-slim || true
