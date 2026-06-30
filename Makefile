.PHONY: serve build check new

PORT ?= 1313

## serve: live dev server at http://localhost:$(PORT) (drafts + future posts visible)
##        override port: make serve PORT=1314
serve:
	hugo server --buildDrafts --buildFuture --disableFastRender --port $(PORT)

## serve-pub: dev server as production renders it (no drafts, no future posts)
serve-pub:
	hugo server --disableFastRender --port $(PORT)

## build: production build into ./docs (no drafts, no future-dated posts)
build:
	hugo --minify --cleanDestinationDir

## check: build to memory, fail on errors — used by CI and pre-commit
check:
	hugo --renderToMemory --logLevel warn

## new: scaffold a new post — make new SLUG=2026/07/my-post-title
new:
	hugo new content/post/$(SLUG).md
