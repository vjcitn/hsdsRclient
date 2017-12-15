#' class for managing information about an HSDS 'host'
#' @import httr rjson S4Vectors rhdf5client
#' @rdname HSDS_source
#' @aliases HSDS_source-class
#' @exportClass HSDS_source
#' @examples
#' hostPath = "/home/john/tenx_full.h5"
#' serverPort = "http://149.165.156.174:5101/"
#' HSDS_source(serverPort=serverPort, hostPath=hostPath)
setClass("HSDS_source", representation(serverPort="character",
     hostPath="character"))
#' display HSDS_source instance
#' @rdname HSDS_source
#' @aliases show,HSDS_source-method
#' @exportMethod show
setMethod("show", "HSDS_source", function(object) {
cat("HDF Object Store instance at ", object@serverPort, "\n")
cat("hostPath: ", object@hostPath, "\n")
})
#' constructor for HSDS_source
#' @rdname HSDS_source
#' @param serverPort character(1) fully qualified URL and port number for HSDS instance
#' @param hostPath character(1) string with resource name
#' @export
HSDS_source = function(serverPort, hostPath)
 new("HSDS_source", serverPort=serverPort, hostPath=hostPath)

# internal methods: getDims, etc.

getDims = function(txsrc) {
  stopifnot(is(txsrc, "HSDS_source"))
  getDatasetAttrs(txsrc@serverPort, txsrc@hostPath)$shape$dims
}

getDatasetUUIDs = function(serverPort, hostPath, ...) {
 query = sprintf("%sdatasets?host=%s", serverPort, hostPath)
 ans = try(GET(query))
 if (inherits(ans, "try-error")) stop("could not resolve datasets query")
 cont = fromJSON(readBin( ans$content, what="character"))
 cont$datasets
}

getDatasetAttrs = function(serverPort, hostPath, ...) {
 uu = getDatasetUUIDs(serverPort, hostPath)
 query = sprintf("%sdatasets/%s?host=%s", serverPort, uu, hostPath)
 ans = try(GET(query))
 if (inherits(ans, "try-error")) stop("could not resolve datasets query")
 cont = fromJSON(readBin( ans$content, what="character"))
 cont
}

getDims = function(txsrc) {
  stopifnot(is(txsrc, "HSDS_source"))
  getDatasetAttrs(txsrc@serverPort, txsrc@hostPath)$shape$dims
}

getHRDF = function(txsrc) {
 stopifnot(is(txsrc, "HSDS_source"))
 atts = getDatasetAttrs(txsrc@serverPort, txsrc@hostPath)
 nms = sapply(atts$hrefs, "[[", "rel")
 vals = sapply(atts$hrefs, "[[", "href")
 DataFrame(hrefName=nms, hrefValue=vals)
}

# This function generates the object that will
# answer [,], transposed relative to h5serv expectations
#
# WE ARE REUSING rhdf5client H5S_dataset class, but not in a
# genuine way
#
# FIXME: JSON transfermode??
#

#' @importClassesFrom rhdf5client H5S_source H5S_dataset
#' @exportClass HSDS_dataset
setClass("HSDS_dataset", contains="H5S_dataset")

#' create HSDS dataset reference amenable to bracket subsetting
#' @param instance of \code{\link{HSDS_source-class}}
#' @export
HSDS_dataset = function(txsrc) {
 src = new("H5S_source", serverURL=txsrc@serverPort, dsmeta=DataFrame())
 atts = getDatasetAttrs(txsrc@serverPort, txsrc@hostPath)
 ans = getHRDF(txsrc)
 rownames(ans) = ans[,1]
 self = ans["self", "hrefValue"]
 prep = sub("\\?host=", "/value?host=", self)
 prep = paste0(prep, "&select=[%%SEL1%%,%%SEL2%%]")
 new("H5S_dataset", source=src, simpleName=txsrc@hostPath, shapes=atts$shape,
       hrefs = ans, allatts=atts, presel=prep, transfermode="JSON")
}

getDatasetSlice = function(serverPort, hostPath, dsindex=1, selectionString, ...) {
 requireNamespace("httr")
 requireNamespace("rjson")
 uuid = getDatasetUUIDs(serverPort, hostPath)[dsindex]
 query = sprintf("%sdatasets/%s/value?host=%s&select=%s", serverPort, uuid, hostPath, selectionString)
 ans = try(GET(query, add_headers(Accept="application/json")))
 if (inherits(ans, "try-error")) stop("could not resolve select query")
 fromJSON(readBin( ans$content, what="character"))
}

