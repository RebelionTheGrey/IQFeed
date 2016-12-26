InitializeLookup <- function(lookup = 9100, protocol = '5.2') 
{
	lookupConnection <- socketConnection(host = "localhost", ports$lookup, server = FALSE, blocking = TRUE, open = "r+");

	protocolString <- paste('S,SET PROTOCOL,',protocol,'\r\n', sep = '')
	writeLines(protocolString, lookupConnection);
}

