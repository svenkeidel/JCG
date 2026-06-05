
.PHONY: all clean

all: DynamicCG.so

DynamicCG.so: DynamicCG.cpp
	g++ -O3 -std=c++20 -shared -fPIC \
		-I /usr/lib/jvm/java-25-openjdk-amd64/include/  \
		-I /usr/lib/jvm/java-25-openjdk-amd64/include/linux/ \
		DynamicCG.cpp \
		-lboost_iostreams -lz \
		-o DynamicCG.so

clean:
	rm DynamicCG.so