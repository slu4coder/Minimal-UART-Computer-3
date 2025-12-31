VPATH				:= include/glad
CFLAGS			:= -O2 -std=c++20 -Iinclude
ifeq ($(OS),Windows_NT)
	LDFLAGS		:= -s -static -Llib
	LIBS			:= -lglfw3win -lopengl32 -lgdi32
else
	LDFLAGS		:= -s -pthread -Llib
	LIBS			:= -lrt -lm -ldl -lX11 -lXrandr -lXinerama -lXxf86vm -lXcursor -lGL -lglfw3linux
endif

MinimalUART.exe: main.o glad.o resources.o
	g++ $(LDFLAGS) -L$(3RDPARTY)/lib -o$@ $^ $(LIBS)

main.o: main.cpp
	g++ $(CFLAGS) -c $<

glad.o: glad.c glad.h
	g++ $(CFLAGS) -c $<

resources.o: shader.glsl charset.bin
	ld -r -b binary -o $@ $^
ifneq ($(OS),Windows_NT)
	objcopy --add-section .note.GNU-stack=/dev/null --set-section-flags .note.GNU-stack=contents,readonly $@
endif

# clean-up
.PHONY: clean
clean:
	rm -f *.o *.exe
