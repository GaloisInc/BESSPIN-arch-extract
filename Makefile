CXX = g++

CXXFLAGS = -O3 -std=c++11 -g

LDFLAGS = -lz -ltinycbor \
	-lverific_verilog -lverific_util -lverific_containers -lverific_database

all:    exporter importer ppfeatures

EXPORTER_OBJS = exporter.o

%.o: %.cpp
	$(CXX) -c $< -o $@ $(CXXFLAGS)

exporter:    $(EXPORTER_OBJS)
	# Note - order of -l flags matters for linking
	$(CXX) $^ -o $@ $(CXXFLAGS) $(LDFLAGS)

clean:
	rm -fv $(EXPORTER_OBJS) exporter

importer:
	ghc -j1 --make -O3 importer.hs

ppfeatures:
	ghc -j1 --make -O3 ppfeatures.hs

.PHONY: all clean importer ppfeatures
