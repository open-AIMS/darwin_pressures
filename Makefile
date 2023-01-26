# help:
# 	@echo "Usage: make -i SRC=<path/file> -> to make a specific file"
# 	@echo "       make -i                 -> to make all altered files"

SHELL:=/bin/bash

.PHONY: build build_singularity run code_container docs_container R_container code_local docs_local code_singularity docs_singularity code_slurm docs_slurm

build:
	docker build . --tag darwin_pressures

build_singularity:
	docker save darwin_pressures -o darwin_pressures.tar 
	singularity build darwin_pressures.sif docker-archive://darwin_pressures.tar

# Run interactive R session in docker container
R_container:
	docker run --rm -it -v "$(shell pwd)":/home/Project darwin_pressures R

code_container:
	docker run --rm -v "$(shell pwd)":/home/Project darwin_pressures $(MAKE) -f scripts/Makefile

docs_container:
	docker run --rm -v "$(shell pwd)":/home/Project darwin_pressures $(MAKE) -f docs/Makefile

code_local:
	$(MAKE) -f scripts/Makefile

docs_local:
	$(MAKE) -f docs/Makefile

code_singularity:
	@echo "Transfer to scripts/Makefile"
	module load singularity; \
	singularity exec -B .:/home/Project darwin_pressures.sif $(MAKE) -f scripts/Makefile

docs_singularity:
	@echo "Transfer to docs/Makefile"
	module load singularity
	singularity exec -B .:/home/Project darwin_pressures.sif $(MAKE) -f docs/Makefile

code_slurm:
	@echo "Submit slurm job to run code"
	sbatch code.slurm

docs_slurm:
	@echo "Submit slurm job to compile docs"
	sbatch docs.slurm

clean:
	rm -f *.log *.aux *.out texput.log *.stderr
