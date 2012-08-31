all : chat nplayer calculator

chat: ChatServer.hs ChatClient.hs 
	ghc --make -O2 -threaded ./q3/ChatServer.hs
	ghc --make -O2 -threaded ./q3/ChatClient.hs
nplayer: NPlayerServer.hs NPlayerClient.hs
	ghc --make -O2 -threaded ./q1/NPlayerServer.hs
	ghc --make -O2 -threaded ./q1/NPlayerClient.hs
calculator: Calculator.hs CalculatorServer.hs CalculatorClient.hs
	ghc --make -O2 ./q2/Calculator.hs
	ghc --make -O2 -threaded ./q2/CalculatorServer.hs
	ghc --make -O2 -threaded ./q2/CalculatorClient.hs
clean:
	rm -f *.hi 
	rm -f *.o
	rm -f *_flymake.hs
	rm -f ChatServer
	rm -f ChatClient
	rm -f NPlayerServer 
	rm -f CalculatorServer
	rm -f NPlayerClient
	rm -f CalculatorClient
	rm -rf ./dist/


