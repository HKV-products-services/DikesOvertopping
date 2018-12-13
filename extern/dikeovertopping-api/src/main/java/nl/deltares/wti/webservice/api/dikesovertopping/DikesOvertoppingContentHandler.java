package nl.deltares.wti.webservice.api.dikesovertopping;

import nl.deltares.wti.kernels.dikesovertopping.OvertoppingGeometryStruct;
import nl.deltares.wti.kernels.dikesovertopping.OvertoppingModelFactors;
import nl.deltares.wti.kernels.dikesovertopping.OvertoppingLoadStruct;

public class DikesOvertoppingContentHandler {

    private OvertoppingModelFactors modelFactors;
    private OvertoppingLoadStruct load;
    private OvertoppingGeometryStruct geometry;
    private double discharge;

    public DikesOvertoppingContentHandler() {
        this.modelFactors = new OvertoppingModelFactors();

        //set defaults
        modelFactors.computedOvertopping = 1;
        modelFactors.criticalOvertopping = 1;
        modelFactors.factorDeterminationQbFb = 4.3;
        modelFactors.factorDeterminationQnFn = 2.3;
        modelFactors.fShallow = 0.6778;
        modelFactors.mz2 = 1.07;
        modelFactors.relaxationFactor = 1;
        modelFactors.reductionFactorForeshore = 0.5;
    }

    public OvertoppingModelFactors getModelFactors() {
        return modelFactors;
    }

    public void setModelFactors(OvertoppingModelFactors input) {
        this.modelFactors = input;
    }

    public OvertoppingLoadStruct getLoad() {
        return load;
    }

    public void setLoad(OvertoppingLoadStruct load) {
        this.load = load;
    }

    public OvertoppingGeometryStruct getGeometry() {
        return geometry;
    }

    public void setGeometry(OvertoppingGeometryStruct geometry) {
        this.geometry = geometry;
    }

    public void setDischarge(double discharge) {
        this.discharge = discharge;
    }

    public double getDischarge() {
        return discharge;
    }
}
