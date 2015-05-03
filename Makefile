# ql-to-deb build tool
APP_NAME   = tapoueh

SRC       = lisp/tapoueh.asd			\
            $(wildcard lisp/*lisp)		\
            $(wildcard lisp/lib/*lisp)		\
            $(wildcard lisp/src/*lisp)		\
            $(wildcard lisp/muse/*lisp)		\
            $(wildcard lisp/utils/*lisp)

BUILDDIR   = build
LIBS       = $(BUILDDIR)/libs.stamp
QLDIR      = $(BUILDDIR)/quicklisp
QUICKLISP  = $(BUILDDIR)/quicklisp.lisp
MANIFEST   = $(BUILDDIR)/manifest.ql
TAPOUEH    = $(BUILDDIR)/bin/$(APP_NAME)
BUILDAPP   = $(BUILDDIR)/bin/buildapp.sbcl

CL         = sbcl
CL_OPTS    = --no-sysinit --no-userinit

all: $(TAPOUEH)

clean:
	rm -rf $(LIBS) $(QUICKLISP) $(QLDIR) $(MANIFEST) $(BUILDAPP) $(TAPOUEH)

$(QLDIR)/setup.lisp:
	mkdir -p $(BUILDDIR)
	curl -o $(QUICKLISP) http://beta.quicklisp.org/quicklisp.lisp
	$(CL) $(CL_OPTS) --load $(QUICKLISP) \
             --eval '(quicklisp-quickstart:install :path "$(BUILDDIR)/quicklisp")' \
             --eval '(quit)'

quicklisp: $(QLDIR)/setup.lisp ;

$(LIBS): $(QLDIR)/setup.lisp
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp                 \
             --eval '(push "$(CURDIR)/" asdf:*central-registry*)'   \
             --eval '(ql:quickload "tapoueh")'                     \
             --eval '(quit)'
	touch $@

libs: $(LIBS) ;

$(MANIFEST): $(LIBS)
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp                \
             --eval '(ql:write-asdf-manifest-file "$(MANIFEST)")'  \
             --eval '(quit)'

manifest: $(MANIFEST) ;

$(BUILDAPP): $(QLDIR)/setup.lisp
	mkdir -p $(BUILDDIR)/bin
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp               \
             --eval '(ql:quickload "buildapp")'                   \
             --eval '(buildapp:build-buildapp "$@")'              \
             --eval '(quit)'

buildapp: $(BUILDAPP) ;

$(TAPOUEH): $(MANIFEST) $(BUILDAPP) $(SRC)
	mkdir -p $(BUILDDIR)/bin
	$(BUILDAPP)      --logfile /tmp/build.log                \
                         --compress-core                         \
                         --require asdf                          \
                         --load-system asdf                      \
                         --asdf-tree $(QLDIR)/dists              \
                         --asdf-path lisp                        \
                         --load-system $(APP_NAME)               \
                         --entry tapoueh:main                    \
                         --dynamic-space-size 1024               \
                         --output $@

tapoueh: $(TAPOUEH) ;
