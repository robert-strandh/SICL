all: libcount.so

libcount.so: recursive-count.o
	gcc -shared -Wl,-soname,libcount.so -o libcount.so recursive-count.o

recursive-count.o: recursive-count.c
	gcc -c -O2 -fPIC recursive-count.c

clean:
	rm -f *.o *.so *.s *~
