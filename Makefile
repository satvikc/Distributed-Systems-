all : chat client calculator tcp
chat: Chat.hs 
	ghc --make -O2 -threaded Chat.hs
client: Client.hs
	ghc --make -O2 -threaded Client.hs
calculator: Calculator.hs
	ghc --make -O2 Calculator.hs
tcp: calculator TcpServer.hs
	ghc --make -O2 TcpServer.hs
clean:
	rm -f *.hi 
	rm -f *.o
	rm -f Chat 
	rm -f Client 
	rm -f TcpServer
	rm -f NPlayerClient
	rm -f NPlayer


