all : chat nplayer calculator

chat: ChatServer.hs ChatClient.hs 
	ghc --make -O2 -threaded ChatServer.hs
	ghc --make -O2 -threaded ChatClient.hs
nplayer: NPlayerServer.hs NPlayerClient.hs
	ghc --make -O2 -threaded NPlayerServer.hs
	ghc --make -O2 -threaded NPlayerClient.hs
calculator: Calculator.hs CalculatorServer.hs CalculatorClient.hs
	ghc --make -O2 Calculator.hs
	ghc --make -O2 -threaded CalculatorServer.hs
	ghc --make -O2 -threaded CalculatorClient.hs
clean:
	rm -f *.hi 
	rm -f *.o
	rm -f ChatServer
	rm -f ChatClient
	rm -f NPLayerServer 
	rm -f CalculatorServer
	rm -f NPlayerClient
	rm -f CalculatorClient


