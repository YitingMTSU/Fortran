Householder : Householder.o QRfraction.o solution.o
	gfortran -o Householder Householder.o QRfraction.o solution.o
QRfraction.o : QRfraction.f90
	gfortran -c QRfraction.f90
Householder.o : Householder.f90
	gfortran -c Householder.f90
solution.o : solution.f90
	gfortran -c solution.f90
