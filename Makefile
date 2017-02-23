CC=erl
CFLAGS=-compile
RFLAGS=-noshell
clean:
	rm *.beam

all: 
	$(CC) $(CFLAGS) *.erl

run: all
	$(CC) $(RFLAGS) -s $(ARG)

run_interactive: all
	$(CC) -s $(ARG)

