.PHONY: serve build check new

## serve: live dev server at http://localhost:1313 (drafts + future posts visible)
serve:
	hugo server --buildDrafts --buildFuture --disableFastRender

## build: production build into ./docs (no drafts, no future-dated posts)
build:
	hugo --minify --cleanDestinationDir

## check: build to memory, fail on errors — used by CI and pre-commit
check:
	hugo --renderToMemory --logLevel warn

## new: scaffold a new post — make new SLUG=2026/07/my-post-title
new:
	hugo new content/post/$(SLUG).md
