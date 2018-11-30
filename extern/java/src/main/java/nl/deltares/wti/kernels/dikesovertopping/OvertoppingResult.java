package nl.deltares.wti.kernels.dikesovertopping;

import com.sun.jna.Structure;

import java.util.Arrays;
import java.util.List;

public class OvertoppingResult {

    public double z2;                       // 2% wave run-up (m)
    public double qo;                       // wave overtopping discharge (m3/m per s)
    public boolean success;
    public String message;
    public double dikeHeight;               // calculated dike height
}
