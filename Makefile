all: animation

.PHONY:animation test
animation:
	jbuilder build ./_build/default/src/animation/animation_top.exe
	./_build/default/src/animation/animation_top.exe

test:
	jbuilder build @run_test

.PHONY:clean
clean:
	jbuilder clean

install:
	jbuilder build @install
	jbuilder install
