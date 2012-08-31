all: class client server 

class: PowerService.java 
	javac PowerService.java 

client: PowerServiceClient.java 
	javac PowerServiceClient.java 

server: PowerServiceServer.java 
	javac PowerServiceServer.java 
	rmic PowerServiceServer

clean: 
	rm -f PowerServiceClient.class 
	rm -f PowerService.class
	rm -f PowerServiceServer.class 
	rm -f PowerServiceServer_Stub.class 
