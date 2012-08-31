import java.rmi.*;
import java.rmi.Naming;
import java.io.*;

//
//
// PowerServiceClient
//
//
public class PowerServiceClient
{
    public static void main(String args[]) throws Exception
    {
        // Check for hostname argument
        if (args.length != 1)
        {
            System.out.println
            ("Syntax - PowerServiceClient host");
            System.exit(1);
        }

        // Assign security manager
        if (System.getSecurityManager() == null)
        {
            System.setSecurityManager
            (new RMISecurityManager());
        }

        // Call registry for PowerService
        PowerService service = (PowerService) Naming.lookup
            ("rmi://" + args[0] + "/PowerService");

        DataInputStream din = new
            DataInputStream (System.in);
        String line;
        for (;;)
        {
              line = din.readLine();
              try{
                  if(line.contains("=")){
                      service.assign(line);
                  }
                  else {
                      System.out.println(service.evaluate(line));}}
              catch(RemoteException e){
                  System.out.println("Wrong Expression. Try again.");
              }


        }
    }

}
