# USAGE:
#    sed -f script.sed  albums.csv | pbcopy

# Remove Windows line ends
s/\r//
# Start each line with "  , entry"
s/^/  , entry "/
# Replace each first ;
s/;/" /
# Replace each next ;
s/;/ "/
# Replace each next ;
s/;/" ["/
# Replace each ; not followed by a newline with ,
s/;\([^\n]\)/","\1/
# Remove all ;
s/;//
# Add "] to end of each line
s/$/"]/
