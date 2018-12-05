package nl.deltares.wti.kernels.dikesovertopping;

import nl.wldelft.util.TextUtils;
import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.*;

public class LibDikesOvertoppingTest {


    @Test
    public void TestOverTopping(){

        OvertoppingLoadStruct load = new OvertoppingLoadStruct();
        load.direction = 50;
        load.height = 1;
        load.period = 4;
        load.waterLevel = 5.5;

        int npoints = 3;
        OvertoppingGeometryStruct geometry = new OvertoppingGeometryStruct();
        geometry.normal = 60;
        double[] xcoords = new double[npoints];
        double[] ycoords = new double[npoints];
        double[] roughness = new double[npoints];
        for (int i = 0; i < npoints; i++)
        {
            xcoords[i] = 5*(i + 1);
            ycoords[i] = 3 + 2*(i + 1);
            roughness[i] = 1;
        }
        geometry.xCoords = xcoords;
        geometry.yCoords = ycoords;
        geometry.roughness = roughness;
        geometry.dikeHeight = 9.1;


        OvertoppingModelFactors factors = new OvertoppingModelFactors();
        factors.computedOvertopping = 1;
        factors.criticalOvertopping = 1;
        factors.factorDeterminationQbFb = 4.3;
        factors.factorDeterminationQnFn = 2.3;
        factors.fShallow = 0.92;
        factors.mz2 = 1;
        factors.relaxationFactor = 1;
        factors.reductionFactorForeshore = 0.5;

        OvertoppingResult result= LibDikesOvertopping.calculateQo(load, geometry, factors);
        if (!result.success)
            throw new RuntimeException("Calculation of Q failed with message: " + result.message);

        Assert.assertEquals(8.089025E-09d, result.qo, 1.0E-15d);
        Assert.assertEquals(1.519737, result.z2, 0.000001);
    }

    @Test
    public void TestReverse(){

        OvertoppingLoadStruct load = new OvertoppingLoadStruct();
        load.direction = 50;
        load.height = 1;
        load.period = 4;
        load.waterLevel = 5.5;

        int npoints = 3;
        OvertoppingGeometryStruct geometry = new OvertoppingGeometryStruct();
        geometry.normal = 60;
        double[] xcoords = new double[npoints];
        double[] ycoords = new double[npoints];
        double[] roughness = new double[npoints];
        for (int i = 0; i < npoints; i++)
        {
            xcoords[i] = 5*(i + 1);
            ycoords[i] = 3 + 2*(i + 1);
            roughness[i] = 1;
        }
        geometry.xCoords = xcoords;
        geometry.yCoords = ycoords;
        geometry.roughness = roughness;

        OvertoppingModelFactors factors = new OvertoppingModelFactors();
        factors.computedOvertopping = 1;
        factors.criticalOvertopping = 1;
        factors.factorDeterminationQbFb = 4.75;
        factors.factorDeterminationQnFn = 2.6;
        factors.fShallow = 0.92;
        factors.mz2 = 1;
        factors.relaxationFactor = 1;
        factors.reductionFactorForeshore = 0.5;

        OvertoppingResult result= LibDikesOvertopping.omkeerVariant(load, geometry, factors, 1.519737);
        if (!result.success)
            throw new RuntimeException("Reverse Calculation failed with message: " + result.message);

        Assert.assertEquals(0.22143452756966334d, result.qo, 1.0E-15d);
        Assert.assertEquals(1.519737, result.z2, 0.000001);
        Assert.assertEquals(5.5, result.dikeHeight, 0.000001);
    }


    @Test
    public void TestValidate(){

        int npoints = 3;
        OvertoppingGeometryStruct geometry = new OvertoppingGeometryStruct();
        geometry.normal = 60;
        double[] xcoords = new double[npoints];
        double[] ycoords = new double[npoints];
        double[] roughness = new double[npoints];
        for (int i = 0; i < npoints; i++)
        {
            xcoords[i] = 5*(i + 1);
            ycoords[i] = 3 + 2*(i + 1);
            roughness[i] = 1;
        }
        geometry.xCoords = xcoords;
        geometry.yCoords = ycoords;
        geometry.roughness = roughness;

        OvertoppingModelFactors factors = new OvertoppingModelFactors();
        factors.computedOvertopping = 1;
        factors.criticalOvertopping = 1;
        factors.factorDeterminationQbFb = 4.75;
        factors.factorDeterminationQnFn = 2.6;
        factors.fShallow = 0.92;
        factors.mz2 = 1;
        factors.relaxationFactor = 1;
        factors.reductionFactorForeshore = 0.5;

        OvertoppingResult result= LibDikesOvertopping.validateInput(geometry, factors);
        if (!result.success)
            throw new RuntimeException("Validation failed with message: " + result.message);

    }

    @Test
    public void TestValidateFail(){

        OvertoppingGeometryStruct geometry = new OvertoppingGeometryStruct();
        geometry.normal = 60;
        geometry.xCoords = new double[]{0,10,20,30,40};
        geometry.yCoords = new double[]{-5,0,5,4,0};
        geometry.roughness = new double[]{1,1,1,1,1};
        geometry.dikeHeight = 9.1;

        OvertoppingModelFactors factors = new OvertoppingModelFactors();
        factors.computedOvertopping = 1;
        factors.criticalOvertopping = 1;
        factors.factorDeterminationQbFb = 4.75;
        factors.factorDeterminationQnFn = 2.6;
        factors.fShallow = 0.92;
        factors.mz2 = 1;
        factors.relaxationFactor = 1;
        factors.reductionFactorForeshore = 0.5;

        OvertoppingResult result= LibDikesOvertopping.validateInput(geometry, factors);
        assertFalse(result.success);
        String[] messages = TextUtils.split(result.message, '\t');
        assertEquals(2, messages.length);

        assertEquals("FOUT:Verticale coordinaten mogen niet afnemen.   5.00 en    4.00 doen dat wel.", messages[0]);
        assertEquals("FOUT:Verticale coordinaten mogen niet afnemen.   4.00 en    0.00 doen dat wel.", messages[1]);

    }
}

