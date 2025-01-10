# CSMC330 Project 8 Makefile

# Mostly this wraps cargo functionality but eases the task of running
# simple tests as well as allowing tests of the whole programs rather
# via testy.

AN    = p8
CLASS = 330
SHELL = /bin/bash
CWD   = $(shell pwd | sed 's/.*\///g')

all:
	cargo build

clean-tests:
	rm -rf test-results

clean:
	cargo clean

test: all test-funcs test-bin

test-funcs: all			#functions
	cargo test --test test_p8funcs

test-bin:			#command line tests
	@chmod u+x ./test_bin.sh
	./test_bin.sh

# these tests will run on linux systems and Gradescope, they are known
# to fail on MacOS due to incompatible shells

ltest: all ltest-funcs ltest-bin

test-setup:
	@chmod u+x testy test_post_filter	#ensure testy is executable
	cargo test --no-run			#build test programs

ltest-funcs: all test-setup
	./testy test_funcs.org $(testnum)

ltest-bin: all test-setup
	./testy test_bin.org $(testnum)



############################################################
# 'make zip' to create complete.zip for submission
ZIPNAME = $(AN)-complete.zip
zip : clean clean-tests
	rm -f $(ZIPNAME)
	cd .. && zip "$(CWD)/$(ZIPNAME)" -r "$(CWD)"
	@echo Zip created in $(ZIPNAME)
	@if (( $$(stat -c '%s' $(ZIPNAME)) > 10*(2**20) )); then echo "WARNING: $(ZIPNAME) seems REALLY big, check there are no abnormally large test files"; du -h $(ZIPNAME); fi
	@if (( $$(unzip -t $(ZIPNAME) | wc -l) > 256 )); then echo "WARNING: $(ZIPNAME) has 256 or more files in it which may cause submission problems"; fi

############################################################
# help message to show build targets
help :
	@echo 'Typical usage is:'
	@echo '  > make                          # build all programs'
	@echo '  > make test                     # run all tests'
	@echo '  > make test-funcs               # run tests for functions'
	@echo '  > make test-bin                 # run test for binaries'
	@echo '  > make clean-tests              # remove test-results/ directory'
	@echo '  > make clean                    # remove build files'
	@echo
	@echo 'LINUX ONLY (makes use of testy script which fails on MacOS'
	@echo '  > make ltest                    # run all tests and show scores, linux only'
	@echo '  > make ltest-funcs              # run function tests and show score, linux only'
	@echo '  > make ltest-bin                # run binary tests and show score, linux only'
	@echo '  > make ltest-funcs testnum=5    # run function tests test #5 only, linux only'
