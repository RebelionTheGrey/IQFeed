GetData <- function(requestString, quotesColumns)
{
	quotes <- writeLines(requestString, get('lookupConnection', envir = .GlobalEnv));
	result <- rbindlist(lapply(res[c(-1, - length(res))], function(elem) { tstrsplit(elem, split = ',', fixed = TRUE); }));

	setnames(result, paste("V", 1:ncol(result), sep = ""), quotesColumns);

	result
}

HistoricTickData <- function(type, symbol, maxDataPoints = '0', dataDirection = '1', requestID = 'Base Request', dataPointsPerSend = '',
							   beginFilterTime = '', endFilterTime = '', days = '', beginDate = '', endDate = '', beginTime = '',
							   endTime = '')
{
	requestType <- paste('HT', type, sep = '');

	requestString <- switch(type,
							'X' = paste(requestType, symbol, maxDataPoints, dataDirection, requestID, dataPointsPerSend, sep = ','),
							'D' = paste(requestType, symbol, days, maxDataPoints, beginFilterTime, endFilterTime, dataDirection, 
										requestID, dataPointsPerSend, sep = ','),
							'T' = paste(requestType, symbol, paste(beginDate, beginTime, sep = ' '), paste(endDate, endTime, sep = ' '),
										beginFilterTime, endFilterTime, dataDirection, requestID, dataPointsPerSend, sep = ','));

	requestString <- paste(requestString, '\r\n', sep = '');
	quotesColumns <- c('Request ID', 'TimeStamp', 'Last', 'Last Size', 'Total Volume', 'Bid', 'Ask', 'TickID', 'Basis For Last',
					   'Trade Market Center', 'Trade Conditions');

	result <- GetData(requestString, quotesColumns);
	result
}

HistoricIntervalData <- function(type, symbol, interval = '60', maxDataPoints = '0', dataDirection = '1', requestID = 'Base Request', 
								 dataPointsPerSend = '', beginFilterTime = '', endFilterTime = '', days = '', beginDate = '', 
								 endDate = '', beginTime = '', endTime = '') 
{
	requestType <- paste('HI', type, sep = '');

	baseRequestString <- paste(requestType, symbol, maxDataPoints, dataDirection, requestID, dataPointsPerSend, sep = ',');

	requestString <- switch(type,
							'X' = paste(requestType, symbol, interval, maxDataPoints, dataDirection, requestID, dataPointsPerSend, sep = ','),
							'D' = paste(requestType, symbol, interval, days, maxDataPoints, beginFilterTime, endFilterTime, dataDirection,
										requestID, dataPointsPerSend, sep = ','),
							'T' = paste(requestType, symbol, interval, paste(beginDate, beginTime, sep = ' '), paste(endDate, endTime, sep = ' '),
										beginFilterTime, endFilterTime, dataDirection, requestID, dataPointsPerSend, sep = ','));

	requestString <- paste(requestString, '\r\n', sep = '');
	quotesColumns <- c('Request ID', 'TimeStamp', 'High', 'Low', 'Open', 'Close', 'Total Volume', 'Period Volume', 'Number of Trades');

	result <- GetData(requestString, quotesColumns);
	result
}

HistoricEODData <- function(type, symbol, maxDataPoints = '0', dataDirection = '1', requestID = 'Base Request',
								 dataPointsPerSend = '', beginDate = '', endDate = '') 
{
	requestType <- paste('HD', type, sep = '');

	requestString <- switch(type,
							'X' = paste(requestType, symbol, maxDataPoints, dataDirection, requestID, dataPointsPerSend, sep = ','),
							'T' = paste(requestType, symbol, beginDate, endDate, maxDataPoints, dataDirection, requestID, 
										dataPointsPerSend, sep = ','));

	requestString <- paste(requestString, '\r\n', sep = '');
	quotesColumns <- c('Request ID', 'DateStamp', 'High', 'Low', 'Open', 'Close', 'Period Volume', 'Open Interest');

	result <- GetData(requestString, quotesColumns);
	result
}

HistoricEODData <- function(type, symbol, maxDataPoints = '0', dataDirection = '1', requestID = 'Base Request',
								 dataPointsPerSend = '', beginDate = '', endDate = '') 
{
	requestType <- paste('HD', type, sep = '');

	requestString <- switch(type,
							'X' = paste(requestType, symbol, maxDataPoints, dataDirection, requestID, dataPointsPerSend, sep = ','),
							'T' = paste(requestType, symbol, beginDate, endDate, maxDataPoints, dataDirection, requestID,
										dataPointsPerSend, sep = ','));

	requestString <- paste(requestString, '\r\n', sep = '');
	quotesColumns <- c('Request ID', 'DateStamp', 'High', 'Low', 'Open', 'Close', 'Period Volume', 'Open Interest');

	result <- GetData(requestString, quotesColumns);
	result
}

HistoricEOWData <- function(symbol, maxDataPoints = '0', dataDirection = '1', requestID = 'Base Request',
								 dataPointsPerSend = '', beginDate = '', endDate = '') 
{
	requestString <- paste('HWX', symbol, maxDataPoints, dataDirection, requestID, dataPointsPerSend, sep = ',');
	requestString <- paste(requestString, '\r\n', sep = '');

	quotesColumns <- c('Request ID', 'DateStamp', 'High', 'Low', 'Open', 'Close', 'Period Volume', 'Open Interest');

	result <- GetData(requestString, quotesColumns);
	result
}

HistoricEOMData <- function(symbol, maxDataPoints = '0', dataDirection = '1', requestID = 'Base Request',
								 dataPointsPerSend = '', beginDate = '', endDate = '') 
{
	requestString <- paste('HMX', symbol, maxDataPoints, dataDirection, requestID, dataPointsPerSend, sep = ',');
	requestString <- paste(requestString, '\r\n', sep = '');

	quotesColumns <- c('Request ID', 'DateStamp', 'High', 'Low', 'Open', 'Close', 'Period Volume', 'Open Interest');

	result <- GetData(requestString, quotesColumns);
	result
}
