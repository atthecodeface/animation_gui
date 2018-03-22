all: animation

.PHONY:animation
animation:
	jbuilder build ./_build/default/src/animation/animation_top.exe
	./_build/default/src/animation/animation_top.exe

.PHONY:clean
clean:
	jbuilder clean

install:
	jbuilder build @install
	jbuilder install
