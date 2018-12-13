package nl.deltares.wti.kernels.dikesovertopping;

import com.sun.jna.Structure;

import java.util.Arrays;
import java.util.List;

public class OvertoppingModelFactors extends Structure {
        public double factorDeterminationQnFn;  // model factor for non-breaking waves
        public double factorDeterminationQbFb;  // model factor for breaking waves
        public double mz2;                      // model factor describing the uncertainty of 2% runup height
        public double fShallow;                 // model factor for shallow waves
        public double computedOvertopping;      // model factor computed overtopping
        public double criticalOvertopping;      // model factor critical overtopping
        public double relaxationFactor;         // relaxation factor iteration procedure wave runup
        public double reductionFactorForeshore; // reduction factor foreshore

    @Override
    protected List getFieldOrder() {
        return Arrays.asList("factorDeterminationQnFn", "factorDeterminationQbFb", "mz2", "fShallow", "computedOvertopping", "criticalOvertopping", "relaxationFactor", "reductionFactorForeshore");
    }
}
