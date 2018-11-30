package nl.deltares.wti.kernels.dikesovertopping;

import com.sun.jna.Structure;

import java.util.Arrays;
import java.util.List;

public class OvertoppingLoadStruct extends Structure {

    public double waterLevel;
    public double height;
    public double period;
    public double direction;

    @Override
    protected List getFieldOrder() {
        return Arrays.asList("waterLevel", "height", "period", "direction");
    }
}
