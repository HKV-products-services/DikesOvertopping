package nl.deltares.wti.kernels.dikesovertopping;

import com.sun.jna.Structure;

import java.util.Arrays;
import java.util.List;

public class OvertoppingGeometryStruct extends Structure {

    public double[] xCoords;                  // pointer to vector with x-coordinates cross section (m)
    public double[] yCoords;                  // pointer to vector with y-coordinates cross section (m+NAP)
    public double[] roughness;                // pointer to vector with roughness factors cross section
    public double normal;                   // dike normal (degrees)
    public double dikeHeight;

    @Override
    protected List getFieldOrder() {
        return Arrays.asList("xCoords", "yCoords", "roughness","normal", "nPoints", "dikeHeight");
    }
}
