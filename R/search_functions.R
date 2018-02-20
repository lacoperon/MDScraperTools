library(XML)
library(readr)
library(httr)
library(dplyr)
library(purrr)

#' RCSB Search Result Scraper Function
#'
#' This function allows you to parse (to df) the basic results of a search on
#' RCSB, based on attribute XPaths derived by Elliot Williams in February 2018.
#'
#' You want to save the HTML associated with a given search (with all search
#' results displayed) to disk before running this command.
#'
#' @param path The path to the HTML search file
#' @keywords PDB RSCB
#' @export
#' @examples
#' getPDBResults(path="data/searchResults.html")
getPDBResults <- function(path="data/searchResults.html") {
  # Opens pre-saved version of the search results
  searchResults <- read_file(path)
  d <- htmlParse(searchResults)

  # Gets the number of structures returned
  node_num_struct <- getNodeSet(doc=d, path="//ul/li[@class='active']//a")[[1]]
  num_structure <- trimws(xmlToList(node_num_struct)$text)

  # Scrapes all search result HTML nodes (and their PDB accession numbers)
  node_set_path <- "//ul[@id='SearchResultsDetails-MainContent']/li"
  result_nodes <- getNodeSet(doc=d, path=node_set_path)
  # Returns the class element of each search result item corresponding to PDB code
  cand_structs  <- data.frame(sapply(result_nodes,
                                     function(node) {
                                       substr(xmlAttrs(node)[[1]], 11, 1000)
                                     }))
  colnames(cand_structs) <- c("PDB_Number")

  # Scrapes brief description of structure
  desc_path <- "//ul[@id='SearchResultsDetails-MainContent']/li/div[2]/h4/a"
  desc_nodes <- getNodeSet(doc=d, path=desc_path)
  cand_structs$Description  <- sapply(desc_nodes,
                                      xmlValue)

  # Scrapes all search result links (to the PDB structures themselves)
  file_link_path <- "//ul[@id='SearchResultsDetails-MainContent']/li/div[2]/div[1]/div/a[1]"
  cand_structs$PDBFileLink   <- xpathSApply(doc=d, path=file_link_path, fun=xmlGetAttr, name="href")


  # Scrapes Citation of Paper
  cit_path <- "//ul[@id='SearchResultsDetails-MainContent']/li/div[2]/p[2]"
  cit_nodes <- getNodeSet(doc=d, path=cit_path)
  cand_structs$PaperDate  <- sapply(cit_nodes, xmlValue)

  # Scrapes Link to Individual Structure Page
  link_path <- "//ul[@id='SearchResultsDetails-MainContent']/li/div[2]/h3/a"
  cand_structs$PageLink <- xpathSApply(doc=d, path=link_path, fun=xmlGetAttr, name="href")

  return(cand_structs)
}


#' RCSB Link Scraper Function
#'
#' This function allows you to scrape a unique link specified by an XPath,
#' applied on an individual RCSB structure page. Concretely, this lets you
#' obtain links to assets (like the FASTA file, PDB file, mmCIF file, etc.)
#' if you have the corresponding XPath for that <a> element.
#'
#' To apply this to a vector, see `getLinksFromPages``
#'
#' @param url The url corresponding to the RCSB Structure Page in question
#' @param path The path of the <a> element you're wanting to scrape a link from
#' @param prepend Any string you want to prepend to the results
#'
#' @keywords PDB RSCB structure
#' @export
#' @examples
#' # This gets the FASTA file link associated with the 6B4V structure on RCSB
#' url <- "http://www.rcsb.org/structure/6B4V"
#' fasta_path <- '//*[@id="DownloadFilesButton"]/ul/li[1]/a'
#' getLinkFromPage(url, fasta_path)
getLinkFromPage <- function(url, path, prepend="http://www.rcsb.org") {
  # Gets HTML associated with particular site
  print(url)
  s <- GET(url)
  w <- content(s, as='text') # converts s to plaintext of HTML
  d <- htmlParse(file=content(s, as="text", asText=T))

  # Gets link from page
  link <- xpathSApply(doc=d, path=path, fun=xmlGetAttr, name="href")[1]
  # Sees that it's valid (ie is not empty)
  if (nchar(link) == 0) {
    # Tries again (one more time), if necessary
    s <- GET(url)
    w <- content(s, as='text') # converts s to plaintext of HTML
    d <- htmlParse(file=content(s, as="text", asText=T))
    link <- xpathSApply(doc=d, path=path, fun=xmlGetAttr, name="href")[1]
  }
  link <- paste(prepend, link, sep="")
  return(link)
}
