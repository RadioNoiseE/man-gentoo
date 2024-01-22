# Do not edit; Internal use only.

all : simplify htmlify

simplify : main.md
	opencc -i main.md -o main-sf.md -c t2s.json

htmlify : main.md
	pandoc main.md -o main.html

obsolete : main.md c.c
	clang c.c -o c -lopencc
	./c >> main-sf.md
	rm c
