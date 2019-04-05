CXX = g++

CXXFLAGS = -O3 -std=c++11 -g

LDFLAGS = -lz -ltinycbor \
	-lverific_verilog -lverific_util -lverific_containers -lverific_database

all:    export-verilog importer ppfeatures driver

EXPORT_VERILOG_OBJS = export-verilog.o

%.o: %.cpp
	$(CXX) -c $< -o $@ $(CXXFLAGS)

export-verilog:		$(EXPORT_VERILOG_OBJS)
	# Note - order of -l flags matters for linking
	$(CXX) $^ -o $@ $(CXXFLAGS) $(LDFLAGS)

clean:
	rm -fv $(EXPORT_VERILOG_OBJS) exporter

importer:
	ghc -j1 --make -O3 importer.hs

ppfeatures:
	ghc -j1 --make -O3 ppfeatures.hs

driver:
	ghc -j1 --make -O3 -Wincomplete-patterns driver.hs

.PHONY: all clean importer ppfeatures driver
