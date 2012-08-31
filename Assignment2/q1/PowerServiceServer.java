import java.math.*;
import java.rmi.*;
import java.rmi.server.*;
import javax.script.ScriptEngineManager;
import javax.script.ScriptEngine;

public class PowerServiceServer extends UnicastRemoteObject
implements PowerService
{
    public ScriptEngine engine;

    public PowerServiceServer () throws RemoteException
    {
        super();
        ScriptEngineManager mgr = new ScriptEngineManager();
        engine = mgr.getEngineByName("JavaScript");
    }

    // Assignment
    public void assign ( String ass )
    throws RemoteException
    {
        try{
        engine.eval(ass);
        } catch (Exception e) {
            System.out.println("Exception: "+e);
            throw new RemoteException();
        }
    }

    // Calculate the power of a number
    public Double evaluate ( String expr)
    throws RemoteException
    {
        try
            {
                Double output = (Double) engine.eval(expr);
                return(output);
            } catch (Exception e) {
            System.out.println("Exception: "+e);
            throw new RemoteException();
        }

    }

    public static void main ( String args[] ) throws Exception
    {
        // Assign a security manager, in the event that dynamic
    // classes are loaded
        if (System.getSecurityManager() == null)
            System.setSecurityManager ( new RMISecurityManager() );

        // Create an instance of our power service server ...
        PowerServiceServer svr = new PowerServiceServer();

        // ... and bind it with the RMI Registry
        Naming.bind ("PowerService", svr);

        System.out.println ("Service bound....");
    }
}
