# To compile with my Linux-based iPhone cross-compiler,
# I had to remove "assert" - because it uses 'abort',
# which is apparently missing from my cross compiler's RTL.

CXXFLAGS=-Wall -g

# Better error reporting
#CXX=clang++
CXX=g++

# You can uncomment this one, to use the C++11 version
# TARGET= Unblock-solve-c++11
TARGET= Unblock-solve

all:	$(TARGET)

$(TARGET): $(TARGET).o
	$(CXX) -o $@ $(CXXFLAGS) $<

Unblock-solve-c++11:	Unblock-solve-c++11.cc
	$(CXX) -std=c++0x -o $@ $(CXXFLAGS) $<

test: data.rgb | $(TARGET)
	display -size 320x480 -depth 8 data.rgb &
	./$(TARGET)

data.rgb:	IMG_0354.PNG
	convert $< $@

clean:
	rm -f $(TARGET) $(TARGET).o data.rgb Unblock-solve-c++11
