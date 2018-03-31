all: animation

.PHONY:animation test client
animation:
	jbuilder build ./_build/default/src/animation/animation_top.exe
	./_build/default/src/animation/animation_top.exe

client:
	jbuilder build ./_build/default/src/example_client/example_client.exe
	jbuilder build ./_build/default/src/animation/animation_top.exe
	(./_build/default/src/animation/animation_top.exe & (sleep 1;./_build/default/src/example_client/example_client.exe))

test:
	jbuilder build @run_test

.PHONY:clean
clean:
	jbuilder clean

install:
	jbuilder build @install
	jbuilder install
