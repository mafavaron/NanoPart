nanopart : nanopart.o calendar.o wind.o particles.o concentration.o
	ifort -O3 -xAVX -vec-report1 -onanopart nanopart.o calendar.o wind.o particles.o concentration.o

calendar.o calendar.mod : calendar.f90
	ifort -O3 -xAVX -vec-report1 -c calendar.f90
	
concentration.o concentration.mod : concentration.f90
	ifort -O3 -xAVX -vec-report1 -c concentration.f90
	
wind.o wind.mod : wind.f90
	ifort -O3 -xAVX -vec-report1 -c wind.f90
	
particles.o particles.mod : particles.f90
	ifort -O3 -xAVX -vec-report1 -c particles.f90
	
nanopart.o : nanopart.f90 calendar.mod wind.mod particles.mod concentration.mod
	ifort -O3 -xAVX -vec-report1 -c nanopart.f90

clean :
	rm *.o
	rm nanopart

test :
	./nanopart ../data 2013-10-18 2 ../results DEBUG
	
testwind :
	./nanopart ../data 2013-10-18 2 ../results WIND_QA
