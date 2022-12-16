.PHONY: data clean plot all

all: plot

plot: test_process.pdf test_process_idx.pdf

test_process.pdf: benchmark_process
	python3 plot.py process test_process $(SIZES)

test_process_idx.pdf: benchmark_process_idx
	python3 plot.py process_idx test_process_idx $(SIZES)

benchmark_process: process-opencl.json process-c.json 
benchmark_process_idx: process_idx-opencl.json process_idx-c.json

SIZES = 100 1000 10000 100000 1000000 10000000

process-opencl.json: process.fut
	futhark bench --backend=opencl \
	              --json process-opencl.json \
	              process.fut

process-c.json: process.fut
	futhark bench --backend=c \
	              --json process-c.json \
	              process.fut

process_idx-opencl.json: process_idx.fut
	futhark bench --backend=opencl \
	              --json process-opencl.json \
	              process_idx.fut

process_idx-c.json: process_idx.fut
	futhark bench --backend=c \
	              --json process-c.json \
	              process_idx.fut

clean:
	rm -fr data exercise1 *.pdf *.bin *.c *.pyc *.json
