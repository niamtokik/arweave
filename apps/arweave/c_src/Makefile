# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

ifeq ($(MODE), debug)
	CFLAGS ?= -O0 -g
	CXXFLAGS ?= -O0 -g
else
	CFLAGS ?= -O3
	CXXFLAGS ?= -O3
endif

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval 'io:format("~ts/erts-~ts/include/", [code:root_dir(), erlang:system_info(version)]).' -s init stop)
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -eval 'io:format("~ts", [code:lib_dir(erl_interface, include)]).' -s init stop)
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -eval 'io:format("~ts", [code:lib_dir(erl_interface, lib)]).' -s init stop)

C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT ?= $(CURDIR)/../priv/arweave.so
TESTS_DIR = $(CURDIR)/tests
TESTS_OUTPUT = $(TESTS_DIR)/tests

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	OSX_CPU_ARCH ?= x86_64
	# nix systems may not have sysctl where uname -m will return the correct arch
	SYSCTL_EXISTS := $(shell which sysctl 2>/dev/null)
	ifdef SYSCTL_EXISTS
		ifneq ($(shell sysctl -n machdep.cpu.brand_string | egrep "M(1|2)"),)
	  	OSX_CPU_ARCH = arm64
		endif
  else
		ifneq ($(shell uname -m | egrep "arm64"),)
	  	OSX_CPU_ARCH = arm64
	 	endif
  endif
	CC ?= cc
	CFLAGS += -std=c99 -arch $(OSX_CPU_ARCH) -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS += -arch $(OSX_CPU_ARCH) -finline-functions -Wall
	LDFLAGS ?= -arch $(OSX_CPU_ARCH)
	LDFLAGS += -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS += -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS += -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS += -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS += -finline-functions -Wall
endif

ifneq (, $(shell which pkg-config))
	CFLAGS += `pkg-config --cflags gmp openssl`
	CXXFLAGS += `pkg-config --cflags gmp openssl`
	LDFLAGS += `pkg-config --libs gmp openssl`
endif

CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I /usr/local/include -I ../lib/RandomX/src
CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I ../lib/RandomX/src -std=c++11

LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -L /usr/local/lib -lei -lgmp -lssl -lcrypto ../lib/RandomX/build/librandomx.a

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

ALL_LIB_SOURCES := $(wildcard $(C_SRC_DIR)/*.c $(C_SRC_DIR)/*.cpp)
ENIF_SOURCES := $(wildcard $(C_SRC_DIR)/ar_*.c $(C_SRC_DIR)/ar_*.cpp)
C_SOURCES := $(filter-out $(ENIF_SOURCES), $(ALL_LIB_SOURCES))
TESTS_SOURCES = $(wildcard $(TESTS_DIR)/*.cpp)

ALL_LIB_OBJECTS = $(addsuffix .o, $(basename $(ALL_LIB_SOURCES)))
ENIF_OBJECTS = $(addsuffix .o, $(basename $(ENIF_SOURCES)))
C_OBJECTS = $(addsuffix .o, $(basename $(C_SOURCES)))
TESTS_OBJECTS = $(addsuffix .o, $(basename $(TESTS_SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

$(C_SRC_OUTPUT): $(ALL_LIB_OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CXX) $(ALL_LIB_OBJECTS) $(LDFLAGS) $(LDLIBS) -shared -o $(C_SRC_OUTPUT)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

$(TESTS_OUTPUT): $(C_OBJECTS) $(TESTS_OBJECTS)
	$(link_verbose) $(CXX) $(C_OBJECTS) $(TESTS_OBJECTS) $(LDFLAGS) $(LDLIBS) -o $(TESTS_OUTPUT)

tests: $(TESTS_OUTPUT)

clean:
	@rm -f $(C_SRC_OUTPUT) $(TESTS_OUTPUT) $(ALL_LIB_OBJECTS) $(TESTS_OBJECTS)





