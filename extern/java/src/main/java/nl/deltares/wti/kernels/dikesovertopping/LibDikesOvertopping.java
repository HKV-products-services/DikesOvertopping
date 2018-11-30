package nl.deltares.wti.kernels.dikesovertopping;

import com.sun.jna.Library;
import com.sun.jna.Native;
import nl.wldelft.util.SystemUtils;
import nl.wldelft.util.TextUtils;

/**
 * Created by pelgrim on 30-Jun-16.
 */
public class LibDikesOvertopping {

    public interface FortranLibrary extends Library {
        FortranLibrary INSTANCE = (FortranLibrary) Native.loadLibrary(SystemUtils.findLibraryWithDependencies(LibDikesOvertopping.class.getClassLoader(), "dllDikesOvertopping"), FortranLibrary.class);

        int GetLanguage(byte[] languageCode, int length);

        int SetLanguage(byte[] languageCode, int i);

        int versionNumber(byte[] version, int i);

        int ValidateInputJ(double[] x, double[] y, double[] roughness, double[] normal, int[] nPoints, double[] dikeHeight, OvertoppingModelFactors modelFactors, boolean[] success, byte[] message, int messageLength);

        int calculateQoJ(OvertoppingLoadStruct load, double[] x, double[] y, double[] roughness, double[] normal, int[] nPoints, double[] dikeHeights,  OvertoppingModelFactors modelFactors, double[] output,  boolean[] success, byte[] message, int messageLength);

        int omkeerVariantJ(OvertoppingLoadStruct load, double[] x, double[] y, double[] roughness, double[] normal, int[] nPoints, double[] givenDischarge, double[] dikeHeight, OvertoppingModelFactors modelFactors, double[] output, boolean[] success, byte[] errorMessage, int messageLength);
    }


    public static String getLanguage() {
        byte[] languageCode = new byte[2];
        FortranLibrary.INSTANCE.GetLanguage(languageCode, 2);
        return new String(languageCode);
    }

    public static void setLanguage(String languageCode) {
        byte[] bytes = languageCode.getBytes();
        FortranLibrary.INSTANCE.SetLanguage(bytes, 2);
    }

    public static String versionNumber() {
        byte[] versionNumber = new byte[20];
        FortranLibrary.INSTANCE.versionNumber(versionNumber, 20);
        return new String(versionNumber);
    }

    public static OvertoppingResult validateInput(OvertoppingGeometryStruct geometry, OvertoppingModelFactors modelFactors) {
        boolean[] success = new boolean[]{false};
        byte[] message = new byte[256];
        FortranLibrary.INSTANCE.ValidateInputJ(geometry.xCoords, geometry.yCoords, geometry.roughness, new double[]{geometry.normal}, new int[]{geometry.xCoords.length},  new double[]{geometry.dikeHeight}, modelFactors, success, message, message.length);
        OvertoppingResult result = new OvertoppingResult();
        result.message = TextUtils.trimToNull(new String(message));
        result.success = success[0];
        return result;
    }

    public static OvertoppingResult calculateQo(OvertoppingLoadStruct load, OvertoppingGeometryStruct geometry, OvertoppingModelFactors modelFactors) {
        boolean[] success = new boolean[]{false};
        byte[] message = new byte[256];
        double[] output = new double[2];

        FortranLibrary.INSTANCE.calculateQoJ(load, geometry.xCoords, geometry.yCoords, geometry.roughness, new double[]{geometry.normal}, new int[]{geometry.xCoords.length}, new double[]{geometry.dikeHeight}, modelFactors, output, success, message, message.length);

        OvertoppingResult result = new OvertoppingResult();
        result.message = TextUtils.trimToNull(new String(message));
        result.success = success[0];
        result.z2 = output[1];
        result.qo = output[0];
        return result;
    }

    public static OvertoppingResult omkeerVariant(OvertoppingLoadStruct load,
                                                  OvertoppingGeometryStruct geometry,
                                                  OvertoppingModelFactors modelFactors, double givenDischarge) {
        boolean[] success = new boolean[]{false};
        byte[] message = new byte[256];
        double[] output = new double[2];
        double[] dikeHeightOutput = new double[1];

        FortranLibrary.INSTANCE.omkeerVariantJ(load, geometry.xCoords, geometry.yCoords, geometry.roughness, new double[]{geometry.normal}, new int[]{geometry.xCoords.length}, new double[]{givenDischarge}, dikeHeightOutput, modelFactors, output, success, message, message.length);

        OvertoppingResult result = new OvertoppingResult();
        result.message = TextUtils.trimToNull(new String(message));
        result.success = success[0];
        result.z2 = output[0];
        result.qo = output[1];
        result.dikeHeight = dikeHeightOutput[0];
        return result;
    }

}

