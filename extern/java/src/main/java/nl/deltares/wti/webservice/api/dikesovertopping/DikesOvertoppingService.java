package nl.deltares.wti.webservice.api.dikesovertopping;

import nl.deltares.wti.kernels.dikesovertopping.*;
import org.springframework.stereotype.Component;

@Component
public class DikesOvertoppingService {


    public OvertoppingResult calculateDischarge(OvertoppingLoadStruct load, OvertoppingGeometryStruct geometry, OvertoppingModelFactors modelFactor) {
        return LibDikesOvertopping.calculateQo(load, geometry, modelFactor);
    }

    public OvertoppingResult calculateHeight(OvertoppingLoadStruct load, OvertoppingGeometryStruct geometry, OvertoppingModelFactors modelFactor, double discharge) {
        return LibDikesOvertopping.omkeerVariant(load, geometry, modelFactor, discharge);
    }

    public OvertoppingResult validate(OvertoppingGeometryStruct geometry, OvertoppingModelFactors modelFactors) {
        return LibDikesOvertopping.validateInput(geometry, modelFactors);
    }
}
