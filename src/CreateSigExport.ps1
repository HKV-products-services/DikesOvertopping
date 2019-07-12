Remove-Item ..\*.zip
if ( Test-Path '..\DikesOvertopping_source' -PathType Container ) { Remove-Item -recurse -path ..\DikesOvertopping_source}
svn export .\ ..\DikesOvertopping_source
Compress-Archive ..\DikesOvertopping_source\ -DestinationPath ('..\' + 'DikesOvertopping_source_' + (get-date -Format yyyyMMdd) + '.zip')