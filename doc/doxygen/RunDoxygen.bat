doxygen DikesOvertoppingFortran.doxyfile > Doxygen.log

call ".\latex\make.bat" >> Doxygen.log

copy ".\latex\refman.pdf" "..\Dikes Overtopping Kernel - Technical Documentation.pdf" >> Doxygen.log
