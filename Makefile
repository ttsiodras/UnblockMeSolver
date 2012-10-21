CXXFLAGS=-Wall -g
#CXX=clang++
CXX=g++

TARGET= Unblock-solve

all:	$(TARGET)

$(TARGET): $(TARGET).o
	$(CXX) -o $@ $(CXXFLAGS) $<

test: data.rgb | $(TARGET)
	display -size 320x480 -depth 8 data.rgb &
	./$(TARGET)

data.rgb:	IMG_0354.PNG
	convert $< $@

clean:
	rm -f $(TARGET) $(TARGET).o data.rgb
