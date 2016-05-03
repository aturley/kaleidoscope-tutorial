clang++-3.8 -g src/kc.cpp $(/usr/local/Cellar/llvm38/3.8.0/bin/llvm-config-3.8 --cxxflags --ldflags --system-libs --libs core mcjit native) -o kc
