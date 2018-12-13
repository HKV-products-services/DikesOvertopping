package nl.deltares.wti.webservice.api.dikesovertopping;

import nl.deltares.wti.kernels.dikesovertopping.OvertoppingGeometryStruct;
import nl.deltares.wti.kernels.dikesovertopping.OvertoppingLoadStruct;
import nl.deltares.wti.kernels.dikesovertopping.OvertoppingModelFactors;
import nl.wldelft.util.Arguments;
import nl.wldelft.util.IOUtils;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.io.InputStream;

public class JsonCalculateParser {

    public void parse(InputStream inputStream, DikesOvertoppingContentHandler contentHandler) throws IOException {

        JSONObject jsonObject = new JSONObject(new String(IOUtils.readBytes(inputStream)));
        if (!jsonObject.isNull("load")) {
            parseLoad(contentHandler, jsonObject.getJSONObject("load"));
        }
        parseGeometry(contentHandler, jsonObject.getJSONObject("geometry"));
        if (!jsonObject.isNull("modelFactors")) {
            //modelFactors is optional
            parseModelFactors(contentHandler, jsonObject.getJSONObject("modelFactors"));
        }
        if (!jsonObject.isNull("discharge")) {
            //discharge is optional
            contentHandler.setDischarge(jsonObject.getDouble("discharge"));
        }
    }

    private void parseModelFactors(DikesOvertoppingContentHandler contentHandler, JSONObject jsonObject) {
        Arguments.require.notNull(jsonObject);
        OvertoppingModelFactors factors = new OvertoppingModelFactors();
        factors.computedOvertopping = jsonObject.getDouble("computedOvertopping");
        factors.criticalOvertopping = jsonObject.getDouble("criticalOvertopping");
        factors.factorDeterminationQbFb = jsonObject.getDouble("factorDeterminationQbFb");
        factors.factorDeterminationQnFn = jsonObject.getDouble("factorDeterminationQnFn");
        factors.fShallow = jsonObject.getDouble("fShallow");
        factors.mz2 = jsonObject.getDouble("mz2");
        factors.reductionFactorForeshore = jsonObject.getDouble("reductionFactorForeshore");
        factors.relaxationFactor = jsonObject.getDouble("relaxationFactor");

        contentHandler.setModelFactors(factors);
    }

    private void parseGeometry(DikesOvertoppingContentHandler contentHandler, JSONObject jsonObject) {
        Arguments.require.notNull(jsonObject);

        JSONArray xCoords = jsonObject.getJSONArray("xCoords");
        OvertoppingGeometryStruct geometry = new OvertoppingGeometryStruct();
        geometry.dikeHeight = jsonObject.getDouble("dikeHeight");
        geometry.normal = jsonObject.getDouble("normal");

        geometry.xCoords = toDoubleArray(xCoords);
        geometry.yCoords = toDoubleArray(jsonObject.getJSONArray("yCoords"));
        geometry.roughness = toDoubleArray(jsonObject.getJSONArray("roughness"));

        contentHandler.setGeometry(geometry);
    }

    private double[] toDoubleArray(JSONArray jsonArray) {
        double[] values = new double[jsonArray.length()];
        for (int i = 0; i < values.length; i++) {
            values[i] = jsonArray.getDouble(i);
        }
        return values;
    }

    private void parseLoad(DikesOvertoppingContentHandler contentHandler, JSONObject jsonObject) {
        Arguments.require.notNull(jsonObject);
        OvertoppingLoadStruct load = new OvertoppingLoadStruct();
        load.direction = jsonObject.getDouble("direction");
        load.height = jsonObject.getDouble("height");
        load.period = jsonObject.getDouble("period");
        load.waterLevel = jsonObject.getDouble("waterLevel");
        contentHandler.setLoad(load);
    }
}
