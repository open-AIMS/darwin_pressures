DOCS_SRC_DIR  = $(addprefix , docs)
ifdef FILE
DOCS_FILE = $(DOCS_SRC_DIR)/$(FILE)
else
DOCS_FILE ?= $(foreach sdir, $(DOCS_SRC_DIR), $(wildcard $(sdir)/*.qmd))
RMD_FILE ?= $(foreach sdir, $(DOCS_SRC_DIR), $(wildcard $(sdir)/*.Rmd))
endif

HTML_FILE := $(patsubst %.qmd, %.html, $(DOCS_FILE))
DOCX_FILE := $(patsubst %.Rmd, %.docx, $(RMD_FILE))

$(info ************************************)
$(info DOCS Source directory:     $(DOCS_SRC_DIR))
$(info DOCS Source files:         $(DOCS_FILE))
$(info HTML files:                $(HTML_FILE))
$(info DOCX files:                $(DOCX_FILE))
$(info RMD files:                 $(RMD_FILE))
$(info ************************************)

all : $(HTML_FILE) $(DOCX_FILE)

.PHONY : all

$(HTML_FILE) : %.html : %.qmd
	@echo "Compiling documntation for analyses"
	$(info Source = $<; Destination = $@)
	echo "library(quarto); quarto_render(\"$<\")" | R --no-save --no-restore;
	# echo "library(rmarkdown); render(\"$<\", output_format = \"html_document\")" | R --no-save --no-restore;

$(DOCX_FILE) : %.docx : %.Rmd docs/ReportCompilation.R
	@echo "Compiling documntation for analyses"
	$(info Source = $<; Destination = $@)
	Rscript -e "library(rmarkdown); render(\"$<\", output_format=\"officedown::rdocx_document\", clean=FALSE)"
	cd docs; Rscript ReportCompilation.R
	#echo "library(quarto); quarto_render(\"$<\", output_format = \"docx\")" | R --no-save --no-restore;
