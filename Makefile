# Note: To compile with my Linux-based iPhone cross-compiler,
# I had to remove "assert" - because it uses 'abort',
# which is apparently missing from my cross compiler's RTL.

CXXFLAGS=-Wall -g

# Better error reporting
#CXX=clang++
CXX=g++

# You can uncomment this one, to use the C++11 version
# TARGET= Unblock-solve-c++11
TARGETCPP=Unblock-solve
TARGETCPP11=Unblock-solve-c++11
TARGETOCAML=Unblock

all:	$(TARGETCPP)

world:	$(TARGETCPP) $(TARGETCPP11) $(TARGETOCAML)

$(TARGETCPP): $(TARGETCPP).cc
	$(CXX) -O3 -o $@ $(CXXFLAGS) $<

$(TARGETCPP11):	$(TARGETCPP11).cc
	$(CXX) -O3 -std=c++0x -o $@ $(CXXFLAGS) $<

$(TARGETOCAML):	$(TARGETOCAML).ml
	ocamlopt -annot -o ./$@ bigarray.cmxa $<
	#ocamlopt -unsafe -rectypes -inline 1000 -o ./$@ common.ml $<

test: data.rgb | $(TARGETCPP)
	display -size 320x480 -depth 8 data.rgb &
	./$(TARGETCPP)

data.rgb:	IMG_0354.PNG
	convert $< $@

cross:
	arm-apple-darwin-g++ -DNDEBUG Unblock-solve.cc -o Unblock-iOS

benchmark:	world
	@./bench.sh

clean:
	rm -f $(TARGETCPP) $(TARGETCPP11) $(TARGETOCAML) data.rgb  Unblock.cm? Unblock.o
