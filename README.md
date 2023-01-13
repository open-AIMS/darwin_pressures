# Darwin Harbour - Statistical analyses for relating pressures to stressors
by Murray Logan

# About

# Building the docker container image

```{build docker, engine='bash', results='markdown', eval=FALSE}
make build
```

Alternatively,

```{build docker alt, engine='bash', results='markdown', eval=FALSE}
docker build . --tag darwin_pressures
```

You can then confirm that the docker image has been built by looking
at a list of all docker images on your local machine.  There should be
a "REPOSITORY" called `darwin_pressures` with a "TAG" of `latest`.

# Running R in the docker container (interactively)

```{run docker, engine='bash', results='markdown', eval=FALSE}
make R_container
```

Alternatively,

```{run docker alt, engine='bash', results='markdown', eval=FALSE}
docker run --rm -v -it "$(pwd):/home/Project" darwin_pressures R
```

# Running the code in the docker container

```{run docker code container, engine='bash', results='markdown', eval=FALSE}
make code_container
```

Alternatively,

```{run docker code container alt, engine='bash', results='markdown', eval=FALSE}
docker run --rm -v -it "$(pwd):/home/Project" darwin_pressures Rscript scripts/00_main.R
```

# Compiling documents in the docker container

```{run docker docs container, engine='bash', results='markdown', eval=FALSE}
make docs_container
```

Alternatively,

```{run docker docs container alt, engine='bash', results='markdown', eval=FALSE}
docker run --rm -v -it "$(pwd):/home/Project" darwin_pressures Rscript docs/00_main.Rmd
```

# Running the code locally 

```{run docker code local, engine='bash', results='markdown', eval=FALSE}
make code_local
```

# Compiling the documents locally 

```{run docker docs local, engine='bash', results='markdown', eval=FALSE}
make docs_local
```

# Building singularity

Alternatively,

```{build singularity alt, engine='bash', results='markdown', eval=FALSE}
docker save darwin_pressures -o darwin_pressures.tar 
singularity build darwin_pressures.sif docker-archive://darwin_pressures.tar
```

# Running (executing) singularity

Alternatively,

```{run singularity alt, engine='bash', results='markdown', eval=FALSE}
singularity exec -B .:/home/Project darwin_pressures.sif Rscript 00_main.R
```
