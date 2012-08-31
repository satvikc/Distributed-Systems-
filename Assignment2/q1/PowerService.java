import java.math.BigInteger;
import java.rmi.*;

//
// PowerService Interface
//
// Interface for a RMI service that calculates powers
//
public interface PowerService extends java.rmi.Remote
{
    // Does the assignment
    public void assign ( String ass )
        throws RemoteException;

    // Does the evaluation
    public Double evaluate  ( String expr)
        throws RemoteException;

}
