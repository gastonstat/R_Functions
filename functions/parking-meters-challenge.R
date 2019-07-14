%------------------------------------------------
  
  \begin{frame}[fragile]
\begin{center}
\ig[width=8cm]{images/sfopendata.png}
\end{center}
{\tiny \url{"https://data.sfgov.org/api/views/28my-4796/rows.csv?accessType=DOWNLOAD"}}
\end{frame}

%------------------------------------------------
  
  \begin{frame}[fragile]
\frametitle{SF OpenData Parking Meters}
Read in Parking Meters data from SF OpenData 
<<size='tiny'>>=
  #load 'readr'
  library(readr)

# SF parking meters
pms <- read_csv("https://data.sfgov.org/api/views/28my-4796/rows.csv?accessType=DOWNLOAD")
@
  
  \end{frame}

%------------------------------------------------
  
  \begin{frame}[fragile]
\frametitle{SF OpenData Parking Meters}

<<size='scriptsize'>>=
  str(pms, vec.len = 1)
@
  \code{LOCATION} contains the coordinates (i.e. latitude, longitude)
\end{frame}

%------------------------------------------------
  
  \begin{frame}[fragile]
\frametitle{SF OpenData Parking Meters}
Processing \code{LOCATION} data
<<>>=
  # get location
  loc <- pms$LOCATION

# remove parentheses and comma
loc <- gsub("[(,)]", "", loc)

# split latitude and longitude
loc <- strsplit(loc, " ")

lat <- sapply(loc, function(x) x[1])
lon <- sapply(loc, function(x) x[2])
@
  \end{frame}

%------------------------------------------------
  
  \begin{frame}[fragile]
\frametitle{SF OpenData Parking Meters}
Processing \code{LOCATION} data
<<>>=
  # get location
  plot(lat, lon, pch=".")
@
  \end{frame}


types <- c("roadmap", "satellite", 
          "terrain", "hybrid")
for (i in seq_along(types)) {
  BerkeleyMap <- GetMap(
    center = c(37.87162, -122.2780), 
    zoom = 14, 
    destfile = sprintf("berkeley_%s.png", types[i]),
    maptype = types[i])  
}


