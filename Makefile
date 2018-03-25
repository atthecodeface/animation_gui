all: animation

.PHONY:animation test client
animation:
	jbuilder build ./_build/default/src/animation/animation_top.exe
	./_build/default/src/animation/animation_top.exe

client:
	jbuilder build @run_example

test:
	jbuilder build @run_test

.PHONY:clean
clean:
	jbuilder clean

install:
	jbuilder build @install
	jbuilder install
