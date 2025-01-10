# CSMC330 Project 7 Makefile

# Mostly this wraps cargo functionality but eases the task of running
# simple tests as well as allowing tests of the whole programs rather
# via testy.

AN    = p7
CLASS = 330
SHELL = /bin/bash
CWD   = $(shell pwd | sed 's/.*\///g')

all:
	cargo build

clean-tests:
	rm -rf test-results

clean:
	cargo clean

test: all test-prob1 test-prob2 

test-prob1: all			#all probl in unit tests
	cargo test --test test_prob1

test-prob2:			#command line tests
	@chmod u+x ./test_prob2_wc.sh
	./test_prob2_wc.sh

# these tests will run on linux systems and Gradescope, they are known
# to fail on MacOS due to incompatible shells

ltest: all ltest-prob1 ltest-prob2

test-setup:
	@chmod u+x testy test_post_filter	#ensure testy is executable
	cargo test --no-run			#build test programs

ltest-prob1: all test-setup
	./testy test_prob1.org $(testnum)

ltest-prob2: all test-setup
	./testy test_prob2.org $(testnum)



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
	@echo '  > make test-prob1               # run test for problem 1'
	@echo '  > make test-prob2               # run test for problem 2'
	@echo '  > make clean-tests              # remove test-results/ directory'
	@echo '  > make clean                    # remove build files'
	@echo
	@echo 'LINUX ONLY (makes use of testy script which fails on MacOS'
	@echo '  > make ltest                    # run all tests and show scores, linux only'
	@echo '  > make ltest-prob1              # run problem 1 tests and show score, linux only'
	@echo '  > make ltest-prob2              # run problem 2 tests and show score, linux only'
	@echo '  > make ltest-prob2 testnum=5    # run problem 2 test #5 only, linux only'
