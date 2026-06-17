HUGO_IMAGE = hugomods/hugo:exts
RUN = docker run --rm -v $(PWD):/src -w /src $(HUGO_IMAGE)

.PHONY: serve build check stop new clean

## serve: live dev server at http://localhost:1313 (drafts + future posts visible)
serve:
	docker compose up

## build: production build into ./docs (no drafts, no future-dated posts)
build:
	$(RUN) hugo --minify --cleanDestinationDir

## check: build to memory, fail on errors — used by CI and pre-commit
check:
	$(RUN) hugo --renderToMemory --logLevel warn

## stop: stop the dev server
stop:
	docker compose down

## new: scaffold a new post — make new SLUG=2026/07/my-post-title
new:
	$(RUN) hugo new content/post/$(SLUG).md
