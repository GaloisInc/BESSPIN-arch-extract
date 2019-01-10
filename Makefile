CXX = g++
OBJECTS = main.o

VERIFIC_ROOT ?= ../verific

CXXFLAGS = -I$(VERIFIC_ROOT)/verilog -I$(VERIFIC_ROOT)/util \
    -I$(VERIFIC_ROOT)/containers -O3 -std=c++11 -g

LDFLAGS = $(VERIFIC_ROOT)/verilog/verilog-linux.a   \
    $(VERIFIC_ROOT)/util/util-linux.a               \
    $(VERIFIC_ROOT)/containers/containers-linux.a   \
    $(VERIFIC_ROOT)/database/database-linux.a -lz -ltinycbor

all:    arch-extract exporter

arch-extract:    main.o
	$(CXX) $^ $(LDFLAGS) -o $@ -O3

exporter:    exporter.o
	$(CXX) $^ $(LDFLAGS) -o $@ -O3

clean:
	$(RM) $(OBJECTS) arch-extract exporter

.PHONY: all clean
