## ---- Libraries
library(tidyverse)
library(officer)
library(rmarkdown)
library(knitr)
## ----end

PREP_STAGE <- 1  # produce the coverpage (CoverPage.docx) from the coverpage template (CoverPage_template.docx) and the 'metadata.txt'
## PREP_STAGE <- 2  # read in coverpage (CoverPage.docx) and then append on the report document

## ---- STAGE 1
{
    ## ---- ReadMetaData
    metadata <- readLines('metadata.txt')

    metadata_parse <- function(metadata) {
        key <- gsub('([^=]*) =.*','\\1',metadata)
        value <- gsub('.* = (.*)', '\\1', metadata)
        L <- lapply(1:length(key), function(x) value[x])
        names(L) <- key
        L
    }
    metadata <- metadata_parse(metadata)
    ## ----end
    ## ---- ReadCoverSheetTemplate
    if (PREP_STAGE == 1) {
        doc <- read_docx("resources/CoverPage_template.docx")

        doc <- cursor_reach(doc, 'TITLE')

        for (i in 1:length(metadata)) {
            doc <- doc %>%
                body_replace_all_text(old_value = names(metadata)[i],
                                      new_value = metadata[[i]],
                                      only_at_cursor = FALSE, fixed = TRUE)
            }
        
        doc %>% print(target = 'resources/CoverPage.docx')
    }
    ## ----end
}
## ----end
## ---- STAGE 2
{
    ## ---- ReadCoverSheetComplete
    doc <- read_docx("resources/CoverPage.docx")
    ## ----end
    ## ---- ReadReport
    ## doc1 <- read_docx('sediment_monitoring_word.docx')
    ## styles <- doc1 %>% styles_info()
    
    ## ----end
    ## ---- Combine
    doc <- doc %>%
        body_add_break() %>%
        ## body_add_docx('AAA.docx') %>%
        body_add_docx('00_main.docx') %>%
        ## change_styles(mapstyles = styles) %>%
        set_doc_properties(creator = "Murray Logan", title = metadata$TITLE, created = Sys.time()) #%>%
        ## footers_replace_all_text(old_value = 'Marie Roman',
        ##                          new_value = metadata$TITLE,
        ##                          only_at_cursor = FALSE, fixed = FALSE, warn=TRUE) %>% 
    doc %>% print(target = '00_main_Optimisation.docx')
    ## ----end
}
## ----end

