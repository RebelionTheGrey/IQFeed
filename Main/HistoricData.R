HistoricalTickData <- function(type, symbol, maxDataPoints = '0', dataDirection = '1', requestID = 'Base Request', dataPointsPerSend = '', 
							   beginFilterTime = '', endFilterTime = '', days = '', beginDate = '', endDate = '', beginTime = '',
							   endTime = '')
{
	requestType <- paste('HT', type, sep = '');

	baseRequestString <- paste(requestType, symbol, maxDataPoints, dataDirection, requestID, dataPointsPerSend, sep = ',');

	requestString <- switch(type,
							'X' = paste(requestType, symbol, maxDataPoints, dataDirection, requestID, dataPointsPerSend, sep = ','),
							'D' = paste(requestType, symbol, days, maxDataPoints, beginFilterTime, endFilterTime, dataDirection, 
										requestID, dataPointsPerSend, sep = ','),
							'T' = paste(requestType, symbol, paste(beginDate, beginTime, sep = ' '), paste(endDate, endTime, sep = ' '),
										beginFilterTime, endFilterTime, dataDirection, requestID, dataPointsPerSend, sep = ','));

	requestString <- paste(requestString, '\r\n', sep = '');

	quotes <- writeLines(requestString, get('lookupConnection', envir = .GlobalEnv));
	result <- rbindlist(lapply(res[c(-1, - length(res))], function(elem) { tstrsplit(elem, split = ',', fixed = TRUE); }));

	result
}