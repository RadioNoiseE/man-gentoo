# Do not edit; Internal use only.

browser := firefox

all : simplify htmlify preview

simplify : main.md
	opencc -i main.md -o main-sf.md -c t2s.json

htmlify : main.md
	pandoc main.md -o main.html
	pandoc main-sf.md -o main-sf.html

preview : main.html main-sf.html
	${browser} main.html
	${browser} main-sf.html

obsolete : main.md c.c
	clang c.c -o c -lopencc
	./c >> main-sf.md
	rm c
