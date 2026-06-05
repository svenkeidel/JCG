
.PHONY: all clean

all: DynamicCG.so

DynamicCG.so: DynamicCG.cpp
	g++ -O3 -std=c++20 -shared -fPIC \
		-I $(JAVA_HOME)/include/  \
		-I $(JAVA_HOME)/include/linux/ \
		DynamicCG.cpp \
		-lboost_iostreams -lz \
		-o DynamicCG.so

clean:
	rm -f DynamicCG.so