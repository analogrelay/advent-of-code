CONFIGURATION?=Debug
TARGET=adventofcode

.PHONY: all
all: build/$(CONFIGURATION)/$(TARGET)

build/$(CONFIGURATION)/$(TARGET):
	@xcodebuild -project adventofcode.xcodeproj -configuration $(CONFIGURATION) -target $(TARGET)